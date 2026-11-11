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
// its shell: someone runs something else there now.
type PaneReusedError struct {
	Handle string
	// Command is what the pane runs instead.
	Command string
}

func (e *PaneReusedError) Error() string {
	return fmt.Sprintf("the pane of %s now runs %s, not its shell", e.Handle, e.Command)
}

// Wake checks that rec's pane is at its shell, runs the agent's resume
// command there in its cwd, and waits until herdr reports the agent
// idle or done, with rec's session when herdr reports one. A pane that
// runs something else is a *PaneReusedError and nothing is launched.
func (w *Waker) Wake(ctx context.Context, rec Record) error {
	command, err := WakeCommand(rec)
	if err != nil {
		return err
	}
	info, err := w.Panes.PaneProcessInfo(ctx, rec.Handle)
	if err != nil {
		return fmt.Errorf("read the pane's processes: %w", err)
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
	argv, err := ResumeArgv(rec.Kind, rec.Argv, rec.Session)
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

// ResumeArgv is argv in a kind's resume form. For Claude Code it is
// --resume <session> right after the program, with any --resume [id],
// -r [id], --continue, or -c dropped and every other argument kept.
func ResumeArgv(kind string, argv []string, session string) ([]string, error) {
	switch {
	case kind != "claude":
		return nil, fmt.Errorf("no resume form for a %s agent", kind)
	case session == "":
		return nil, errors.New("the record has no session id")
	case len(argv) == 0:
		return nil, errors.New("the record has no argv")
	}
	out := []string{argv[0], "--resume", session}
	rest := argv[1:]
	for i := 0; i < len(rest); i++ {
		arg := rest[i]
		switch {
		case arg == "--continue" || arg == "-c":
		case strings.HasPrefix(arg, "--resume=") || strings.HasPrefix(arg, "-r="):
		case arg == "--resume" || arg == "-r":
			// The id is optional; a next argument that is a flag is not it.
			if i+1 < len(rest) && !strings.HasPrefix(rest[i+1], "-") {
				i++
			}
		default:
			out = append(out, arg)
		}
	}
	return out, nil
}
