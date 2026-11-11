// Package herdr is the agentbus adapter for herdr. It reaches the local
// server and saved herdr machines through `herdr --machine`.
package herdr

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"
	"unicode"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// localMachine is the machine part of a handle on the local herdr server.
const localMachine = hubcall.LocalMachine

// DefaultTimeout bounds one call to another machine when Timeout is zero.
const DefaultTimeout = 5 * time.Second

// Exec runs one herdr invocation. args do not include the herdr binary.
// A non-nil error means the process did not start. A non-zero code is herdr's.
type Exec func(ctx context.Context, args []string) (stdout, stderr string, code int, err error)

// Adapter talks to herdr through recorded or live command output.
// Machines are the saved herdr machines Inventory adds to the local one.
// Timeout bounds each call to another machine; zero uses DefaultTimeout.
type Adapter struct {
	Run      Exec
	Machines []string
	Timeout  time.Duration
}

// Name is the adapter name used when a self-test fails.
func (a *Adapter) Name() string { return "herdr" }

// Inventory reads `herdr agent list` and tab labels from `herdr tab list`,
// locally and then on each of Machines. A machine that cannot be listed
// adds a line to an *adapter.InventoryWarning returned with the rest.
func (a *Adapter) Inventory(ctx context.Context) ([]adapter.Agent, error) {
	out, err := a.inventoryOf(ctx, localMachine)
	if err != nil {
		return nil, err
	}
	var warnings []string
	for _, machine := range a.Machines {
		agents, err := a.inventoryOf(ctx, machine)
		if ue := remotecall.AsUnreachable(err); ue != nil {
			warnings = append(warnings, ue.Error())
			continue
		}
		if err != nil {
			warnings = append(warnings, fmt.Sprintf("machine %s: %v", machine, err))
			continue
		}
		out = append(out, agents...)
	}
	if len(warnings) > 0 {
		return out, &adapter.InventoryWarning{Lines: warnings}
	}
	return out, nil
}

func (a *Adapter) inventoryOf(ctx context.Context, machine string) ([]adapter.Agent, error) {
	agents, err := a.agents(ctx, machine)
	if err != nil {
		return nil, err
	}
	labels, err := a.tabLabels(ctx, machine)
	if err != nil {
		return nil, err
	}
	out := make([]adapter.Agent, 0, len(agents))
	for _, item := range agents {
		session := item.Session.Value
		out = append(out, adapter.Agent{
			Handle:    handle(machine, session, item.PaneID),
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
//
// A command that arrived in hub mode carries its caller in
// AGENTBUS_AS_HANDLE, a handle on another machine. It is trusted only
// when SSH_CONNECTION shows the command came over ssh.
//
// A request on the hub socket carries its caller in ctx, already checked
// by RemoteCaller. That caller wins over env.
func (a *Adapter) Identify(ctx context.Context, env []string) (string, error) {
	if caller, ok := adapter.ForwardedCaller(ctx); ok {
		return caller, nil
	}
	if envValue(env, AsHandleEnv) != "" {
		return hubcall.TrustedCaller(env, AsHandleEnv) //nolint:wrapcheck // the shared trust rule names the variable
	}
	pane := envValue(env, "HERDR_PANE_ID")
	if pane == "" {
		return "", errors.New("HERDR_PANE_ID is not set")
	}
	agents, err := a.agents(ctx, localMachine)
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
		return handle(localMachine, item.Session.Value, pane), nil
	}
	return "", fmt.Errorf("herdr pane %s is not in the agent list", pane)
}

// AsHandleEnv names the hub-mode caller handle in a command's environment.
const AsHandleEnv = "AGENTBUS_AS_HANDLE"

// RemoteCaller accepts raw only as herdr:<machine>/<session>/<pane> on
// a machine other than local: a caller that reached the hub from
// another machine.
func RemoteCaller(raw string) (string, error) {
	return hubcall.RemoteCaller(raw) //nolint:wrapcheck // the shared rule's message names the handle
}

// LocalHandle is the handle of the caller's pane on the local herdr
// server, herdr:local/<session>/<pane>.
func LocalHandle(ctx context.Context, env []string) (string, error) {
	return (&Adapter{}).Identify(ctx, without(env, AsHandleEnv))
}

// OnMachine rewrites a herdr:local/... handle to herdr:<machine>/....
func OnMachine(raw, machine string) (string, error) {
	return hubcall.OnMachine(raw, machine) //nolint:wrapcheck // the shared rule's message names the handle
}

func without(env []string, key string) []string {
	out := make([]string, 0, len(env))
	for _, entry := range env {
		if name, _, _ := strings.Cut(entry, "="); name != key {
			out = append(out, entry)
		}
	}
	return out
}

// State maps herdr's agent status on the handle's machine. A machine that
// cannot be reached is a *remotecall.UnreachableError.
func (a *Adapter) State(ctx context.Context, raw string) (adapter.State, error) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return adapter.StateUnknown, err
	}
	agents, err := a.agents(ctx, machine)
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
	agents, err := a.agents(ctx, machine)
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
		stdout, stderr, code, err := a.exec(ctx, machine, []string{"agent", "read", pane, "--source", "visible"})
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
	stdout, stderr, code, err := a.exec(ctx, machine, []string{"agent", "prompt", pane, text})
	if err != nil {
		return adapter.ResultFailed, err
	}
	return mapPrompt(code, stdout, stderr), nil
}

// SelfTest requires `herdr agent list` to return a parseable agent list.
func (a *Adapter) SelfTest(ctx context.Context) error {
	if _, err := a.agents(ctx, localMachine); err != nil {
		return fmt.Errorf("herdr agent list: %w", err)
	}
	return nil
}

// RunLive executes the herdr binary on PATH. Cancelling ctx kills herdr
// and the ssh it may have started.
func RunLive(ctx context.Context, args []string) (string, string, int, error) {
	res, err := remotecall.Run(ctx, "herdr", args...)
	if err != nil {
		return "", "", -1, fmt.Errorf("run herdr: %w", err)
	}
	return res.Stdout, res.Stderr, res.Code, nil
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

func (a *Adapter) agents(ctx context.Context, machine string) ([]listedAgent, error) {
	stdout, stderr, code, err := a.exec(ctx, machine, []string{"agent", "list"})
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

// ProcessInfo is a pane's process group from `herdr pane process-info`.
type ProcessInfo struct {
	ShellPID                 int                 `json:"shell_pid"`                   //nolint:tagliatelle // herdr wire format
	ForegroundProcessGroupID int                 `json:"foreground_process_group_id"` //nolint:tagliatelle // herdr wire format
	ForegroundProcesses      []ForegroundProcess `json:"foreground_processes"`        //nolint:tagliatelle // herdr wire format
}

// ForegroundProcess is one process in a pane's foreground process group.
type ForegroundProcess struct {
	PID  int      `json:"pid"`
	Name string   `json:"name"`
	Argv []string `json:"argv"`
	Cwd  string   `json:"cwd"`
}

// ForegroundPIDs are the pids of the foreground processes, in herdr's
// order.
func (p ProcessInfo) ForegroundPIDs() []int {
	out := make([]int, 0, len(p.ForegroundProcesses))
	for _, fp := range p.ForegroundProcesses {
		if fp.PID > 0 {
			out = append(out, fp.PID)
		}
	}
	return out
}

// ProcessInfo runs `herdr pane process-info --pane <pane>` on the local
// server.
func (a *Adapter) ProcessInfo(ctx context.Context, pane string) (ProcessInfo, error) {
	stdout, stderr, code, err := a.exec(ctx, localMachine, []string{"pane", "process-info", "--pane", pane})
	if err != nil {
		return ProcessInfo{}, err
	}
	if code != 0 {
		return ProcessInfo{}, fmt.Errorf("herdr pane process-info %s: exit %d: %s", pane, code, firstText(stderr, stdout))
	}
	var env struct {
		Result struct {
			ProcessInfo *ProcessInfo `json:"process_info"` //nolint:tagliatelle // herdr wire format
		} `json:"result"`
	}
	if err := json.Unmarshal([]byte(stdout), &env); err != nil {
		return ProcessInfo{}, fmt.Errorf("decode pane process-info: %w", err)
	}
	if env.Result.ProcessInfo == nil {
		return ProcessInfo{}, fmt.Errorf("herdr pane process-info %s: no process_info in the reply", pane)
	}
	return *env.Result.ProcessInfo, nil
}

func (a *Adapter) tabLabels(ctx context.Context, machine string) (map[string]string, error) {
	stdout, stderr, code, err := a.exec(ctx, machine, []string{"tab", "list"})
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

// exec runs one herdr call on machine. Every call path goes through it.
// A call to another machine runs as `herdr --machine <machine> <args>`
// under Timeout, and a failure that means the machine cannot be reached
// is a *remotecall.UnreachableError. Other failures are herdr's, as for
// a local call.
func (a *Adapter) exec(ctx context.Context, machine string, args []string) (string, string, int, error) {
	if machine == localMachine {
		return a.run(ctx, args)
	}
	stdout, stderr, code, err := remotecall.OnHerdrMachine(ctx, a.run, remotecall.HerdrProbe(a.run, machine), machine, a.timeout(), args)
	if ue := remotecall.AsUnreachable(err); ue != nil {
		return stdout, stderr, code, fmt.Errorf("herdr %s: %w", strings.Join(args[:min(2, len(args))], " "), ue)
	}
	return stdout, stderr, code, err //nolint:wrapcheck // a start failure is a's own Exec error, as for a local call
}

func (a *Adapter) run(ctx context.Context, args []string) (string, string, int, error) {
	if a.Run == nil {
		return RunLive(ctx, args)
	}
	return a.Run(ctx, args)
}

func (a *Adapter) timeout() time.Duration {
	if a.Timeout > 0 {
		return a.Timeout
	}
	return DefaultTimeout
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

func handle(machine, session, pane string) string {
	return hubcall.Handle(machine, session, pane)
}

func parseHandle(raw string) (machine, session, pane string, err error) {
	return hubcall.ParseHandle(raw) //nolint:wrapcheck // the shared parser's message names the handle
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
