package hibernate

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
)

// DefaultWakeTimeout is how long a relaunched agent has to get ready,
// when wake_timeout is not set.
const DefaultWakeTimeout = 60 * time.Second

// WakeModelKeep is the wake_model / wake --model value that preserves the
// agent's original model.
const WakeModelKeep = "keep"

// WakePanes are the pane calls a wake makes, by handle, on the agent's
// own machine.
type WakePanes interface {
	PaneProcessInfo(ctx context.Context, handle string) (herdr.ProcessInfo, error)
	PaneRun(ctx context.Context, handle, command string) error
	PaneAgent(ctx context.Context, handle string) (adapter.Agent, bool, error)
}

// Waker relaunches a hibernated agent in its pane from its record.
type Waker struct {
	Panes WakePanes
	Clock Clock
	// Timeout is how long the agent has to get ready. Zero is
	// DefaultWakeTimeout.
	Timeout time.Duration
}

// PaneReusedError is a wake refused because the pane's foreground is not
// its shell, or an agent is already running there.
type PaneReusedError struct {
	Handle string
	// Command is what the pane runs instead.
	Command string
	// Reason overrides the default error message when set.
	Reason string
}

func (e *PaneReusedError) Error() string {
	if e.Reason != "" {
		return e.Reason
	}
	return fmt.Sprintf("the pane of %s now runs %s, not its shell", e.Handle, e.Command)
}

// PaneGoneError is a wake into a pane herdr no longer has. It cannot
// succeed, so it is not retried.
type PaneGoneError struct {
	Handle string
	Err    error
}

func (e *PaneGoneError) Error() string {
	return fmt.Sprintf("the pane of %s no longer exists: %v", e.Handle, e.Err)
}

func (e *PaneGoneError) Unwrap() error { return e.Err }

// paneGone wraps a herdr pane_not_found error as a *PaneGoneError.
func paneGone(handle string, err error) error {
	if strings.Contains(err.Error(), "pane_not_found") {
		return &PaneGoneError{Handle: handle, Err: err}
	}
	return err
}

// Wake checks that rec's pane is at its shell, runs the agent's resume
// command there in its cwd, and waits until herdr reports the agent
// idle or done, with rec's session when herdr reports one. A pane that
// runs something else is a *PaneReusedError and nothing is launched.
func (w *Waker) Wake(ctx context.Context, rec Record) error {
	return w.WakeWithModel(ctx, rec, "")
}

// WakeWithModel is Wake with an optional model override (empty or "keep"
// preserves rec's original model).
func (w *Waker) WakeWithModel(ctx context.Context, rec Record, model string) error {
	command, err := WakeCommandWithModel(rec, model)
	if err != nil {
		return err
	}
	a, listed, err := w.Panes.PaneAgent(ctx, rec.Handle)
	if err != nil {
		return fmt.Errorf("read the pane's agent: %w", paneGone(rec.Handle, err))
	}
	if listed && a.Kind != "" {
		return &PaneReusedError{
			Handle: rec.Handle,
			Reason: fmt.Sprintf("pane runs another session; not waking %s", rec.Handle),
		}
	}
	info, err := w.Panes.PaneProcessInfo(ctx, rec.Handle)
	if err != nil {
		return fmt.Errorf("read the pane's processes: %w", paneGone(rec.Handle, err))
	}
	if !info.AtShell() {
		return &PaneReusedError{Handle: rec.Handle, Command: foreground(info)}
	}
	if err := w.Panes.PaneRun(ctx, rec.Handle, command); err != nil {
		return fmt.Errorf("relaunch: %w", err)
	}
	return w.waitReady(ctx, rec)
}

// waitReady polls the pane until the agent is ready, for up to the
// timeout.
func (w *Waker) waitReady(ctx context.Context, rec Record) error {
	timeout := w.Timeout
	if timeout <= 0 {
		timeout = DefaultWakeTimeout
	}
	deadline := w.Clock.Now().Add(timeout)
	last := "not listed by herdr"
	for {
		ready, state := w.ready(ctx, rec)
		if ready {
			return nil
		}
		if state != "" {
			last = state
		}
		if !w.Clock.Now().Before(deadline) {
			return fmt.Errorf("relaunched agent not ready within %s: %s", timeout, last)
		}
		if err := w.Clock.Sleep(ctx, defaultPoll); err != nil {
			return fmt.Errorf("wait for the relaunched agent: %w", err)
		}
	}
}

// ready reports whether the relaunched agent waits for input, or else
// what it saw.
func (w *Waker) ready(ctx context.Context, rec Record) (bool, string) {
	a, listed, err := w.Panes.PaneAgent(ctx, rec.Handle)
	switch {
	case err != nil:
		return false, err.Error()
	case !listed || a.Kind == "":
		return false, "no agent in the pane"
	case !resting(a.Status):
		return false, "agent is " + a.Status
	case a.SessionID != "" && rec.Session != "" && a.SessionID != rec.Session:
		return false, fmt.Sprintf("agent has session %q, not %q", a.SessionID, rec.Session)
	}
	// herdr may still list the agent from before the stop; the agent
	// process must own the pane again.
	info, err := w.Panes.PaneProcessInfo(ctx, rec.Handle)
	if err != nil {
		return false, err.Error()
	}
	if info.AtShell() {
		return false, "the pane's shell still has the foreground"
	}
	return true, ""
}

func foreground(info herdr.ProcessInfo) string {
	p, ok := agentProcess(info)
	switch {
	case !ok:
		return "no foreground process"
	case len(p.Argv) > 0:
		return strings.Join(p.Argv, " ")
	default:
		return p.Name
	}
}

// WakeCommand is the shell line that starts rec's agent again: cd to
// its cwd, then its argv in resume form.
func WakeCommand(rec Record) (string, error) {
	return WakeCommandWithModel(rec, "")
}

// WakeCommandWithModel is WakeCommand with an optional model override
// (empty or "keep" preserves rec's original model).
func WakeCommandWithModel(rec Record, model string) (string, error) {
	argv, err := ResumeArgv(rec.Kind, rec.Argv, rec.Session, model)
	if err != nil {
		return "", err
	}
	if rec.Cwd == "" {
		return "", errors.New("the record has no cwd")
	}
	quoted := make([]string, len(argv))
	for i, arg := range argv {
		quoted[i] = shellQuote(arg)
	}
	return "cd " + shellQuote(rec.Cwd) + " && " + strings.Join(quoted, " "), nil
}

// SupportsWakeModel reports whether kind's resume form supports a
// model flag.
func SupportsWakeModel(kind string) bool {
	return kind == "claude"
}

// ArgvModel returns the model flag value in argv for kind, or "" if
// none or unsupported.
func ArgvModel(kind string, argv []string) string {
	if !SupportsWakeModel(kind) || len(argv) < 2 {
		return ""
	}
	var model string
	rest := argv[1:]
	for i := 0; i < len(rest); i++ {
		arg := rest[i]
		switch {
		case strings.HasPrefix(arg, "--model="):
			model = strings.TrimPrefix(arg, "--model=")
		case arg == "--model" && i+1 < len(rest) && !strings.HasPrefix(rest[i+1], "-"):
			model = rest[i+1]
			i++
		}
	}
	return model
}

// ResumeArgv is argv in a kind's resume form. For Claude Code it is
// --resume <session> right after the program, with any --resume [id],
// -r [id], --continue, or -c dropped and every other argument kept.
// When model is non-empty and not "keep", any --model X or --model=X is
// replaced with --model <model> (or added when absent).
func ResumeArgv(kind string, argv []string, session, model string) ([]string, error) {
	switch {
	case kind != "claude":
		return nil, fmt.Errorf("no resume form for a %s agent", kind)
	case session == "":
		return nil, errors.New("the record has no session id")
	case len(argv) == 0:
		return nil, errors.New("the record has no argv")
	}
	out := []string{argv[0], "--resume", session}
	replaceModel := model != "" && model != WakeModelKeep
	if replaceModel {
		out = append(out, "--model", model)
	}
	return append(out, filterClaudeResumeArgs(argv[1:], replaceModel)...), nil
}

func filterClaudeResumeArgs(rest []string, dropModel bool) []string {
	var out []string
	for i := 0; i < len(rest); i++ {
		arg := rest[i]
		if arg == "--continue" || arg == "-c" || strings.HasPrefix(arg, "--resume=") || strings.HasPrefix(arg, "-r=") {
			continue
		}
		if arg == "--resume" || arg == "-r" || (dropModel && arg == "--model") {
			if i+1 < len(rest) && !strings.HasPrefix(rest[i+1], "-") {
				i++
			}
			continue
		}
		if dropModel && strings.HasPrefix(arg, "--model=") {
			continue
		}
		out = append(out, arg)
	}
	return out
}
