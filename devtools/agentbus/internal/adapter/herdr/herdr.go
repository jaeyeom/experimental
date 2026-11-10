// Package herdr is the v1 agentbus adapter for a local herdr server.
package herdr

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"unicode"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

// Exec runs one herdr invocation. args do not include the herdr binary.
// A non-nil error means the process did not start. A non-zero code is herdr's.
type Exec func(ctx context.Context, args []string) (stdout, stderr string, code int, err error)

// Adapter talks to herdr through recorded or live command output.
type Adapter struct {
	Run Exec
}

// Name is the adapter name used when a self-test fails.
func (a *Adapter) Name() string { return "herdr" }

// Inventory reads `herdr agent list` and tab labels from `herdr tab list`.
func (a *Adapter) Inventory(ctx context.Context) ([]adapter.Agent, error) {
	agents, err := a.agents(ctx)
	if err != nil {
		return nil, err
	}
	labels, err := a.tabLabels(ctx)
	if err != nil {
		return nil, err
	}
	out := make([]adapter.Agent, 0, len(agents))
	for _, item := range agents {
		session := item.Session.Value
		out = append(out, adapter.Agent{
			Handle:    handle(session, item.PaneID),
			PaneID:    item.PaneID,
			TabID:     item.TabID,
			TabLabel:  labels[item.TabID],
			Kind:      item.Agent,
			Status:    item.Status,
			SessionID: session,
			Title:     item.Title,
		})
	}
	return out, nil
}

// Identify builds herdr:local/<session>/<pane> from HERDR_PANE_ID.
// The pane's agent session id comes from herdr, not from the caller.
func (a *Adapter) Identify(ctx context.Context, env []string) (string, error) {
	pane := envValue(env, "HERDR_PANE_ID")
	if pane == "" {
		return "", errors.New("HERDR_PANE_ID is not set")
	}
	agents, err := a.agents(ctx)
	if err != nil {
		return "", err
	}
	for _, item := range agents {
		if item.PaneID != pane {
			continue
		}
		if item.Session.Value == "" {
			return "", fmt.Errorf("herdr pane %s has no agent session id", pane)
		}
		return handle(item.Session.Value, pane), nil
	}
	return "", fmt.Errorf("herdr pane %s is not in the agent list", pane)
}

// State maps herdr's agent status. v1 accepts the local machine only.
func (a *Adapter) State(ctx context.Context, raw string) (adapter.State, error) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return adapter.StateUnknown, err
	}
	if machine != "local" {
		return adapter.StateUnknown, fmt.Errorf("herdr handle %s is not local", raw)
	}
	agents, err := a.agents(ctx)
	if err != nil {
		return adapter.StateUnknown, err
	}
	for _, item := range agents {
		if item.PaneID != pane {
			continue
		}
		return mapStatus(item.Agent, item.Status), nil
	}
	return adapter.StateGone, nil
}

// composerMatchers reads the input box from a visible screen, per agent
// kind. ok is false when the screen does not parse.
var composerMatchers = map[string]func(screen string) (empty, ok bool){
	"claude": claudeComposerEmpty,
}

// ComposerEmpty reads the pane's visible screen and parses the composer.
// A kind with no matcher cannot be read, so it holds while the pane is
// focused, where the user may be typing.
func (a *Adapter) ComposerEmpty(ctx context.Context, raw string) (bool, string, error) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return false, adapter.HoldDraftUnknown, err
	}
	if machine != "local" {
		return false, adapter.HoldDraftUnknown, fmt.Errorf("herdr handle %s is not local", raw)
	}
	agents, err := a.agents(ctx)
	if err != nil {
		return false, adapter.HoldDraftUnknown, err
	}
	for _, item := range agents {
		if item.PaneID != pane {
			continue
		}
		match, ok := composerMatchers[item.Agent]
		if !ok {
			if item.Focused {
				return false, adapter.HoldFocused, nil
			}
			return true, "", nil
		}
		stdout, stderr, code, err := a.exec(ctx, []string{"agent", "read", pane, "--source", "visible"})
		if err != nil {
			return false, adapter.HoldDraftUnknown, err
		}
		if code != 0 {
			return false, adapter.HoldDraftUnknown, fmt.Errorf("herdr agent read: exit %d: %s", code, firstText(stderr, stdout))
		}
		empty, ok := match(stdout)
		switch {
		case !ok:
			return false, adapter.HoldDraftUnknown, nil
		case !empty:
			return false, adapter.HoldDraft, nil
		default:
			return true, "", nil
		}
	}
	return false, adapter.HoldDraftUnknown, nil
}

// promptMarker starts Claude Code's input line.
const promptMarker = "❯"

// claudeComposerEmpty finds Claude Code's input box: the lines between the
// last two horizontal rules, the first starting with the prompt marker.
// The shell-mode marker "!" always counts as a draft.
func claudeComposerEmpty(screen string) (empty, ok bool) {
	lines := strings.Split(screen, "\n")
	bottom := lastRule(lines, len(lines))
	if bottom < 0 {
		return false, false
	}
	top := lastRule(lines, bottom)
	if top < 0 || top+1 >= bottom {
		return false, false
	}
	box := lines[top+1 : bottom]
	switch {
	case strings.HasPrefix(box[0], "!"):
		return false, true
	case !strings.HasPrefix(box[0], promptMarker):
		return false, false
	}
	box[0] = strings.TrimPrefix(box[0], promptMarker)
	for _, line := range box {
		if !blank(line) {
			return false, true
		}
	}
	return true, true
}

func lastRule(lines []string, before int) int {
	for i := before - 1; i >= 0; i-- {
		line := strings.TrimSpace(lines[i])
		if line != "" && strings.Trim(line, "─") == "" {
			return i
		}
	}
	return -1
}

func blank(s string) bool {
	return strings.TrimFunc(s, unicode.IsSpace) == ""
}

// Deliver runs `herdr agent prompt` and does not wait for the agent to finish.
func (a *Adapter) Deliver(ctx context.Context, raw, text string) (adapter.Result, error) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return adapter.ResultFailed, err
	}
	if machine != "local" {
		return adapter.ResultFailed, fmt.Errorf("herdr handle %s is not local", raw)
	}
	stdout, stderr, code, err := a.exec(ctx, []string{"agent", "prompt", pane, text})
	if err != nil {
		return adapter.ResultFailed, err
	}
	return mapPrompt(code, stdout, stderr), nil
}

// SelfTest requires `herdr agent list` to return a parseable agent list.
func (a *Adapter) SelfTest(ctx context.Context) error {
	if _, err := a.agents(ctx); err != nil {
		return fmt.Errorf("herdr agent list: %w", err)
	}
	return nil
}

// RunLive executes the herdr binary on PATH.
func RunLive(ctx context.Context, args []string) (string, string, int, error) {
	cmd := exec.CommandContext(ctx, "herdr", args...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	if err == nil {
		return stdout.String(), stderr.String(), 0, nil
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return stdout.String(), stderr.String(), exitErr.ExitCode(), nil
	}
	return stdout.String(), stderr.String(), -1, fmt.Errorf("run herdr: %w", err)
}

type listedAgent struct {
	PaneID  string `json:"pane_id"` //nolint:tagliatelle // herdr wire format
	TabID   string `json:"tab_id"`  //nolint:tagliatelle // herdr wire format
	Agent   string `json:"agent"`
	Status  string `json:"agent_status"`   //nolint:tagliatelle // herdr wire format
	Title   string `json:"terminal_title"` //nolint:tagliatelle // herdr wire format
	Focused bool   `json:"focused"`
	Session struct {
		Value string `json:"value"`
	} `json:"agent_session"` //nolint:tagliatelle // herdr wire format
}

func (a *Adapter) agents(ctx context.Context) ([]listedAgent, error) {
	stdout, stderr, code, err := a.exec(ctx, []string{"agent", "list"})
	if err != nil {
		return nil, err
	}
	if code != 0 {
		return nil, fmt.Errorf("exit %d: %s", code, firstText(stderr, stdout))
	}
	var env struct {
		Result struct {
			Agents []listedAgent `json:"agents"`
		} `json:"result"`
	}
	if err := json.Unmarshal([]byte(stdout), &env); err != nil {
		return nil, fmt.Errorf("decode agent list: %w", err)
	}
	return env.Result.Agents, nil
}

func (a *Adapter) tabLabels(ctx context.Context) (map[string]string, error) {
	stdout, stderr, code, err := a.exec(ctx, []string{"tab", "list"})
	if err != nil {
		return nil, err
	}
	if code != 0 {
		return nil, fmt.Errorf("exit %d: %s", code, firstText(stderr, stdout))
	}
	var env struct {
		Result struct {
			Tabs []struct {
				TabID string `json:"tab_id"` //nolint:tagliatelle // herdr wire format
				Label string `json:"label"`
			} `json:"tabs"`
		} `json:"result"`
	}
	if err := json.Unmarshal([]byte(stdout), &env); err != nil {
		return nil, fmt.Errorf("decode tab list: %w", err)
	}
	labels := make(map[string]string, len(env.Result.Tabs))
	for _, tab := range env.Result.Tabs {
		labels[tab.TabID] = tab.Label
	}
	return labels, nil
}

func (a *Adapter) exec(ctx context.Context, args []string) (string, string, int, error) {
	run := a.Run
	if run == nil {
		run = RunLive
	}
	return run(ctx, args)
}

func mapStatus(kind, status string) adapter.State {
	if kind == "" && status == "" {
		return adapter.StateNoAgent
	}
	switch status {
	case "idle":
		return adapter.StateIdle
	case "working":
		return adapter.StateWorking
	case "blocked":
		return adapter.StateBlocked
	case "done":
		return adapter.StateDone
	case "unknown":
		return adapter.StateUnknown
	default:
		return adapter.StateUnknown
	}
}

func mapPrompt(code int, stdout, stderr string) adapter.Result {
	if code == 0 {
		return adapter.ResultAccepted
	}
	text := stderr + "\n" + stdout
	if strings.Contains(text, "agent_blocked") || strings.Contains(text, "agent_prompt_stalled") || strings.Contains(text, "timeout") {
		return adapter.ResultRefusedRetryable
	}
	return adapter.ResultFailed
}

func handle(session, pane string) string {
	return "herdr:local/" + session + "/" + pane
}

func parseHandle(raw string) (machine, session, pane string, err error) {
	rest, ok := strings.CutPrefix(raw, "herdr:")
	if !ok {
		return "", "", "", fmt.Errorf("handle %q is not a herdr handle", raw)
	}
	machine, rest, ok = strings.Cut(rest, "/")
	if !ok || machine == "" {
		return "", "", "", fmt.Errorf("handle %q is missing a machine", raw)
	}
	session, pane, ok = strings.Cut(rest, "/")
	if !ok || session == "" || pane == "" {
		return "", "", "", fmt.Errorf("handle %q is missing a session or pane", raw)
	}
	return machine, session, pane, nil
}

func envValue(env []string, key string) string {
	for _, entry := range env {
		name, value, ok := strings.Cut(entry, "=")
		if ok && name == key {
			return value
		}
	}
	return ""
}

func firstText(stderr, stdout string) string {
	if strings.TrimSpace(stderr) != "" {
		return strings.TrimSpace(stderr)
	}
	return strings.TrimSpace(stdout)
}
