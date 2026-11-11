package herdr

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

// The calls here act on a pane of the local herdr server: agentbus
// hibernate reads an agent's screen, types its exit input, and leaves a
// note in the shell left behind.

// Screen is the pane's visible screen, from `herdr agent read <pane>
// --source visible`.
func (a *Adapter) Screen(ctx context.Context, pane string) (string, error) {
	return a.paneCall(ctx, "agent read", []string{"agent", "read", pane, "--source", "visible"})
}

// SendText types text into the pane with `herdr pane send-text`,
// without Enter.
func (a *Adapter) SendText(ctx context.Context, pane, text string) error {
	_, err := a.paneCall(ctx, "pane send-text", []string{"pane", "send-text", pane, text})
	return err
}

// SendKeys presses keys in the pane with `herdr pane send-keys`, such as
// enter or esc.
func (a *Adapter) SendKeys(ctx context.Context, pane string, keys ...string) error {
	_, err := a.paneCall(ctx, "pane send-keys", append([]string{"pane", "send-keys", pane}, keys...))
	return err
}

// RunInPane types command and Enter into the pane's shell with `herdr
// pane run`.
func (a *Adapter) RunInPane(ctx context.Context, pane, command string) error {
	_, err := a.paneCall(ctx, "pane run", []string{"pane", "run", pane, command})
	return err
}

func (a *Adapter) paneCall(ctx context.Context, what string, args []string) (string, error) {
	return a.paneCallOn(ctx, localMachine, what, args)
}

func (a *Adapter) paneCallOn(ctx context.Context, machine, what string, args []string) (string, error) {
	stdout, stderr, code, err := a.exec(ctx, machine, args)
	if err != nil {
		return "", err
	}
	if code != 0 {
		return "", fmt.Errorf("herdr %s %s: exit %d: %s", what, args[2], code, firstText(stderr, stdout))
	}
	return stdout, nil
}

// The calls below act on the pane of a handle, on the handle's machine:
// the daemon wakes a hibernated agent through them, through `herdr
// --machine <m>` for an agent on another machine.

// PaneProcessInfo is ProcessInfo for the pane of the handle raw.
func (a *Adapter) PaneProcessInfo(ctx context.Context, raw string) (ProcessInfo, error) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return ProcessInfo{}, err
	}
	return a.processInfoOn(ctx, machine, pane)
}

// PaneRun is RunInPane for the pane of the handle raw.
func (a *Adapter) PaneRun(ctx context.Context, raw, command string) error {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return err
	}
	_, err = a.paneCallOn(ctx, machine, "pane run", []string{"pane", "run", pane, command})
	return err
}

// PaneAgent is the agent herdr lists in the pane of the handle raw, whatever
// its session. ok is false when herdr lists none there.
func (a *Adapter) PaneAgent(ctx context.Context, raw string) (adapter.Agent, bool, error) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return adapter.Agent{}, false, err
	}
	agents, err := a.agents(ctx, machine)
	if err != nil {
		return adapter.Agent{}, false, err
	}
	for _, item := range agents {
		if item.PaneID == pane {
			return adapter.Agent{
				Handle: handle(machine, item.Session.Value, pane), PaneID: pane, TabID: item.TabID,
				Kind: item.Agent, Status: item.Status, SessionID: item.Session.Value, Title: item.Title, StateSeq: item.StateSeq,
			}, true, nil
		}
	}
	return adapter.Agent{}, false, nil
}

// ComposerOf parses the input box of a kind of agent on screen. ok is
// false when the kind has no parser or the screen does not parse.
func ComposerOf(kind, screen string) (empty, ok bool) {
	match, known := composerMatchers[kind]
	if !known {
		return false, false
	}
	return match(screen)
}

// dialogMatchers find a permission or question dialog on a screen, per
// agent kind, and name it.
var dialogMatchers = map[string]func(screen string) (string, bool){
	"claude": claudeDialog,
}

// PendingDialog names the permission or question dialog a kind of agent
// shows on screen. A kind with no matcher shows none.
func PendingDialog(kind, screen string) (string, bool) {
	match, ok := dialogMatchers[kind]
	if !ok {
		return "", false
	}
	return match(screen)
}

// dialogTail is how many of the last screen lines a dialog is looked for
// in.
const dialogTail = 20

// claudeMenuOption is the selected option of a Claude Code menu.
var claudeMenuOption = regexp.MustCompile(`^\s*` + promptMarker + `\s*\d+\.\s`)

// claudeDialog finds Claude Code's permission prompt ("Do you want
// to ...?" over a numbered menu) or question prompt ("Esc to cancel"
// under the options) in the bottom lines. A dialog takes the input
// box's place, so a screen whose input box parses shows none, whatever
// the agent's last reply says.
func claudeDialog(screen string) (string, bool) {
	if _, ok := claudeComposerEmpty(screen); ok {
		return "", false
	}
	lines := strings.Split(strings.TrimRight(screen, "\n"), "\n")
	lines = lines[max(0, len(lines)-dialogTail):]
	for _, line := range lines {
		switch {
		case strings.Contains(line, "Esc to cancel"):
			return "question prompt", true
		case strings.Contains(line, "Do you want to"), claudeMenuOption.MatchString(line):
			return "permission prompt", true
		}
	}
	return "", false
}

// exitInputs are the text and keys that make a kind of agent exit
// cleanly.
var exitInputs = map[string]struct {
	text string
	keys []string
}{
	"claude": {text: "/exit", keys: []string{"enter"}},
}

// ExitInput is the text to type and the keys to press that make a kind
// of agent exit cleanly. ok is false for a kind with none.
func ExitInput(kind string) (text string, keys []string, ok bool) {
	in, ok := exitInputs[kind]
	if !ok {
		return "", nil, false
	}
	return in.text, append([]string(nil), in.keys...), true
}
