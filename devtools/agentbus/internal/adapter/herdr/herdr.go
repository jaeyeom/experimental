// Package herdr is the agentbus adapter for herdr. It reaches the local
// server and saved herdr machines through `herdr --machine`.
package herdr

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"
	"time"
	"unicode"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
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
	Run Exec
	// RunEnv, when non-nil, overrides Run and receives the child
	// environment (with SSH_AUTH_SOCK set when resolved).
	RunEnv   func(ctx context.Context, env []string, args []string) (stdout, stderr string, code int, err error)
	Machines []string
	Timeout  time.Duration
	// HerdrPath is the herdr binary from herdr_path. It wins over
	// AGENTBUS_HERDR. Empty uses that variable, then "herdr" on PATH.
	HerdrPath string
	// Placeholders are whole-line strings treated as an empty composer
	// when style information is not available. Nil uses
	// DefaultComposerPlaceholders.
	Placeholders []string
	// SSHAgentSocket is the configured ssh_agent_socket (or
	// --ssh-agent-socket flag).
	SSHAgentSocket string
	// Env is the base environment for herdr calls. Nil uses os.Environ().
	Env []string
	// AgentSockets, when non-nil, overrides SSH agent socket discovery.
	AgentSockets func(ctx context.Context) []string
	// Log, when non-nil, receives operational messages such as which
	// fallback SSH agent socket worked.
	Log func(format string, args ...any)
}

// DefaultComposerPlaceholders are the known post-error and idle
// placeholder suggestions treated as empty when style info is absent.
var DefaultComposerPlaceholders = []string{
	"Once fixed, resend your message",
}

func (a *Adapter) placeholders() []string {
	if a.Placeholders != nil {
		return a.Placeholders
	}
	return DefaultComposerPlaceholders
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
			StateSeq:  item.StateSeq,
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

// HubCaller accepts a remote herdr handle or an anon:<host>-<pid> handle.
// A hub-mode outbox replays the sender it stored, including an anon pid
// that is not this flusher's.
func HubCaller(raw string) (string, error) {
	if _, err := hubcall.AnonCaller(raw); err == nil {
		return raw, nil
	}
	return RemoteCaller(raw)
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

// composerText is the unsubmitted box. ok is false when the kind has no
// parser or the screen does not parse.
func composerText(kind, screen string) (string, bool) {
	if kind != "claude" {
		return "", false
	}
	return claudeComposerText(screen)
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
		stdout, stderr, code, err := a.exec(ctx, machine, []string{"agent", "read", pane, "--source", "visible", "--format", "ansi"})
		if err != nil {
			return false, adapter.HoldDraftUnknown, err
		}
		if code != 0 {
			return false, adapter.HoldDraftUnknown, fmt.Errorf("herdr agent read: exit %d: %s", code, firstText(stderr, stdout))
		}
		var empty, parsed bool
		if item.Agent == "claude" {
			empty, parsed = claudeComposerEmptyWithPlaceholders(stdout, a.placeholders())
		} else {
			empty, parsed = match(stdout)
		}
		switch {
		case !parsed:
			return a.scrolledOrUnknown(ctx, machine, pane, match)
		case !empty:
			return false, adapter.HoldDraft, nil
		default:
			return true, "", nil
		}
	}
	return false, adapter.HoldDraftUnknown, nil
}

// ComposerInfo reads the unsubmitted composer text (with dim placeholder
// text removed) and tab label for handle.
func (a *Adapter) ComposerInfo(ctx context.Context, raw string) (string, string, bool, error) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return "", "", false, err
	}
	agents, err := a.agents(ctx, machine)
	if err != nil {
		return "", "", false, err
	}
	for _, item := range agents {
		if item.PaneID != pane {
			continue
		}
		if item.Agent != "claude" {
			return "", "", false, nil
		}
		tabLabel := item.TabID
		if labels, lerr := a.tabLabels(ctx, machine); lerr == nil && labels[item.TabID] != "" {
			tabLabel = labels[item.TabID]
		}
		stdout, stderr, code, err := a.exec(ctx, machine, []string{"agent", "read", pane, "--source", "visible", "--format", "ansi"})
		if err != nil {
			return "", tabLabel, false, err
		}
		if code != 0 {
			return "", tabLabel, false, fmt.Errorf("herdr agent read: exit %d: %s", code, firstText(stderr, stdout))
		}
		text, ok := claudeComposerDraftText(stdout, a.placeholders())
		if !ok {
			return "", tabLabel, false, nil
		}
		return strings.TrimSpace(text), tabLabel, true, nil
	}
	return "", "", false, nil
}

// scrolledOrUnknown reads the bottom of the buffer when the visible
// screen has no input box. A box there, empty or not, means the pane is
// scrolled up, so the message stays held. A missing box or a failed read
// stays draft-unknown. The visible read already succeeded, so a failed
// recent read is not an error.
func (a *Adapter) scrolledOrUnknown(ctx context.Context, machine, pane string, match func(string) (bool, bool)) (bool, string, error) {
	stdout, err := a.readPane(ctx, machine, pane, "recent")
	if err != nil {
		return false, adapter.HoldDraftUnknown, nil
	}
	if _, ok := match(stdout); ok {
		return false, adapter.HoldScrolled, nil
	}
	return false, adapter.HoldDraftUnknown, nil
}

// Probe reads the recent scrollback and the visible composer. A kind
// with no parser, or a read that fails, is unreadable. recent is still
// returned when only the composer cannot be read, so a scrollback hit
// can confirm the paste.
func (a *Adapter) Probe(ctx context.Context, raw, _ string) (string, string, bool, error) {
	machine, pane, kind, ok := a.probeTarget(ctx, raw)
	if !ok {
		return "", "", true, nil
	}
	recent, err := a.readPane(ctx, machine, pane, "recent")
	if err != nil {
		return "", "", true, nil
	}
	visible, err := a.readPane(ctx, machine, pane, "visible")
	if err != nil {
		return recent, "", true, nil
	}
	text, parsed := composerText(kind, visible)
	if !parsed {
		return recent, "", true, nil
	}
	return recent, text, false, nil
}

// Submit presses Enter in the pane. It does not send the text again.
func (a *Adapter) Submit(ctx context.Context, raw string) error {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return fmt.Errorf("submit enter: %w", err)
	}
	_, err = a.paneCallOn(ctx, machine, "pane send-keys", []string{"pane", "send-keys", pane, "enter"})
	if err != nil {
		return fmt.Errorf("submit enter: %w", err)
	}
	return nil
}

func (a *Adapter) probeTarget(ctx context.Context, raw string) (machine, pane, kind string, ok bool) {
	machine, _, pane, err := parseHandle(raw)
	if err != nil {
		return "", "", "", false
	}
	agents, err := a.agents(ctx, machine)
	if err != nil {
		return "", "", "", false
	}
	for _, item := range agents {
		if item.PaneID == pane {
			return machine, pane, item.Agent, true
		}
	}
	return "", "", "", false
}

func (a *Adapter) readPane(ctx context.Context, machine, pane, source string) (string, error) {
	return a.paneCallOn(ctx, machine, "agent read", []string{"agent", "read", pane, "--source", source})
}

// promptMarker starts Claude Code's input line.
const promptMarker = "❯"

// claudeComposerEmpty finds Claude Code's input box: the lines between the
// last two horizontal rules, the first starting with the prompt marker.
// The shell-mode marker "!" always counts as a draft.
func claudeComposerEmpty(screen string) (empty, ok bool) {
	return claudeComposerEmptyWithPlaceholders(screen, DefaultComposerPlaceholders)
}

func claudeComposerEmptyWithPlaceholders(screen string, placeholders []string) (empty, ok bool) {
	text, ok := claudeComposerDraftText(screen, placeholders)
	if !ok {
		return false, false
	}
	if strings.HasPrefix(text, "!") {
		return false, true
	}
	for _, line := range strings.Split(text, "\n") {
		if !blank(line) {
			return false, true
		}
	}
	return true, true
}

// claudeComposerText is the input box with the prompt marker removed.
// Shell mode keeps the "!" line. ok is false when the screen does not parse.
func claudeComposerText(screen string) (string, bool) {
	lines := strings.Split(stripANSI(screen), "\n")
	bottom := lastRule(lines, len(lines))
	if bottom < 0 {
		return "", false
	}
	top := lastRule(lines, bottom)
	if top < 0 || top+1 >= bottom {
		return "", false
	}
	box := append([]string(nil), lines[top+1:bottom]...)
	switch {
	case strings.HasPrefix(box[0], "!"):
		return strings.Join(box, "\n"), true
	case !strings.HasPrefix(box[0], promptMarker):
		return "", false
	}
	box[0] = strings.TrimPrefix(box[0], promptMarker)
	return strings.Join(box, "\n"), true
}

// claudeComposerDraftText extracts Claude Code's input box, removes ANSI
// styling, and treats a known placeholder as empty. Dim styling alone does
// not make composer text empty: Claude also dims collapsed pasted text.
func claudeComposerDraftText(screen string, placeholders []string) (string, bool) {
	rawLines := strings.Split(screen, "\n")
	plainLines := make([]string, len(rawLines))
	for i, l := range rawLines {
		plainLines[i] = stripANSI(l)
	}
	bottom := lastRule(plainLines, len(plainLines))
	if bottom < 0 {
		return "", false
	}
	top := lastRule(plainLines, bottom)
	if top < 0 || top+1 >= bottom {
		return "", false
	}
	firstPlain := plainLines[top+1]
	switch {
	case strings.HasPrefix(firstPlain, "!"):
		return strings.Join(plainLines[top+1:bottom], "\n"), true
	case !strings.HasPrefix(firstPlain, promptMarker):
		return "", false
	}
	out := make([]string, 0, bottom-top-1)
	for i := top + 1; i < bottom; i++ {
		plain := stripANSI(rawLines[i])
		if i == top+1 {
			plain = strings.TrimPrefix(plain, promptMarker)
		}
		out = append(out, plain)
	}
	joined := strings.Join(out, "\n")
	if isPlaceholderOnly(out, placeholders) {
		return "", true
	}
	return joined, true
}

func isPlaceholderOnly(lines, placeholders []string) bool {
	var nonBlank []string
	for _, l := range lines {
		if !blank(l) {
			nonBlank = append(nonBlank, strings.TrimFunc(l, unicode.IsSpace))
		}
	}
	if len(nonBlank) != 1 {
		return false
	}
	for _, p := range placeholders {
		if p != "" && nonBlank[0] == p {
			return true
		}
	}
	return false
}

// stripANSI removes CSI and OSC escape sequences from s.
func stripANSI(s string) string {
	var b strings.Builder
	b.Grow(len(s))
	for i := 0; i < len(s); {
		if s[i] == '\x1b' {
			i = skipEscape(s, i)
			continue
		}
		b.WriteByte(s[i])
		i++
	}
	return b.String()
}

func skipEscape(s string, i int) int {
	if i+1 >= len(s) {
		return i + 1
	}
	switch s[i+1] {
	case '[':
		return skipCSI(s, i)
	case ']':
		for j := i + 2; j < len(s); j++ {
			if s[j] == '\x07' {
				return j + 1
			}
			if s[j] == '\x1b' && j+1 < len(s) && s[j+1] == '\\' {
				return j + 2
			}
		}
		return len(s)
	default:
		return i + 2
	}
}

func skipCSI(s string, i int) int {
	for j := i + 2; j < len(s); j++ {
		if s[j] >= 0x40 && s[j] <= 0x7e {
			return j + 1
		}
	}
	return len(s)
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

// RunLive executes herdr. AGENTBUS_HERDR, when set, is the binary.
// Cancelling ctx kills herdr and the ssh it may have started.
func RunLive(ctx context.Context, args []string) (string, string, int, error) {
	return runHerdr(ctx, herdrBinary(""), args)
}

// herdrBinary is configured, else AGENTBUS_HERDR, else the name on PATH.
func herdrBinary(configured string) string {
	if configured != "" {
		return configured
	}
	if bin := os.Getenv("AGENTBUS_HERDR"); bin != "" {
		return bin
	}
	return "herdr"
}

func runHerdr(ctx context.Context, bin string, args []string) (string, string, int, error) {
	return runHerdrEnv(ctx, nil, bin, args)
}

func runHerdrEnv(ctx context.Context, env []string, bin string, args []string) (string, string, int, error) {
	res, err := remotecall.RunEnv(ctx, env, bin, args...)
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
	// StateSeq is herdr's state_change_seq.
	StateSeq int64 `json:"state_change_seq"` //nolint:tagliatelle // herdr wire format
	Session  struct {
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
	return a.processInfoOn(ctx, localMachine, pane)
}

// AtShell reports whether the pane's shell has the foreground: the
// foreground process group is the shell's. When herdr does not name the
// shell, every foreground process must be a shell by name.
func (p ProcessInfo) AtShell() bool {
	if p.ShellPID > 0 {
		return p.ForegroundProcessGroupID == p.ShellPID
	}
	if len(p.ForegroundProcesses) == 0 {
		return false
	}
	for _, fp := range p.ForegroundProcesses {
		if !procinfo.IsShell(procinfo.Process{PID: fp.PID, Name: fp.Name, Argv: fp.Argv}) {
			return false
		}
	}
	return true
}

func (a *Adapter) processInfoOn(ctx context.Context, machine, pane string) (ProcessInfo, error) {
	stdout, stderr, code, err := a.exec(ctx, machine, []string{"pane", "process-info", "--pane", pane})
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
	sockets := a.resolveSockets(ctx)
	firstSock := ""
	if len(sockets) > 0 {
		firstSock = sockets[0]
	}
	stdout, stderr, code, err := a.execWithSocket(ctx, machine, firstSock, args)
	if ue := remotecall.AsUnreachable(err); ue != nil {
		if remotecall.IsAuthLocked(ue) && len(sockets) > 1 {
			nextSock := sockets[1]
			stdout2, stderr2, code2, err2 := a.execWithSocket(ctx, machine, nextSock, args)
			if ue2 := remotecall.AsUnreachable(err2); ue2 != nil {
				if remotecall.IsAuthLocked(ue2) {
					ue2.Cause = remotecall.AuthLockedCause(nextSock)
				}
				return stdout2, stderr2, code2, fmt.Errorf("herdr %s: %w", strings.Join(args[:min(2, len(args))], " "), ue2)
			}
			if err2 == nil {
				a.logf("ssh to %s succeeded with fallback agent socket %s (after %s failed)", machine, nextSock, firstSock)
			}
			return stdout2, stderr2, code2, err2 //nolint:wrapcheck // a start failure is a's own Exec error, as for a local call
		}
		if remotecall.IsAuthLocked(ue) && firstSock != "" {
			ue.Cause = remotecall.AuthLockedCause(firstSock)
		}
		return stdout, stderr, code, fmt.Errorf("herdr %s: %w", strings.Join(args[:min(2, len(args))], " "), ue)
	}
	return stdout, stderr, code, err //nolint:wrapcheck // a start failure is a's own Exec error, as for a local call
}

func (a *Adapter) execWithSocket(ctx context.Context, machine, sock string, args []string) (string, string, int, error) {
	run := func(c context.Context, callArgs []string) (string, string, int, error) {
		return a.runWithSocket(c, sock, callArgs)
	}
	return remotecall.OnHerdrMachine(ctx, run, remotecall.HerdrProbe(run, machine), machine, a.timeout(), args) //nolint:wrapcheck // caller wraps UnreachableError; start failures stay unwrapped
}

func (a *Adapter) runWithSocket(ctx context.Context, sock string, args []string) (string, string, int, error) {
	env := a.envForSocket(sock)
	if a.RunEnv != nil {
		return a.RunEnv(ctx, env, args)
	}
	if a.Run != nil {
		return a.Run(ctx, args)
	}
	return runHerdrEnv(ctx, env, herdrBinary(a.HerdrPath), args)
}

func (a *Adapter) resolveSockets(ctx context.Context) []string {
	if a.AgentSockets != nil {
		return a.AgentSockets(ctx)
	}
	return remotecall.ResolveAgentSockets(ctx, remotecall.AgentSocketOptions{
		Configured: a.SSHAgentSocket,
		Env:        a.Env,
	})
}

func (a *Adapter) envForSocket(sock string) []string {
	if sock == "" && a.Env == nil {
		return nil
	}
	base := a.Env
	if base == nil {
		base = os.Environ()
	}
	if sock == "" {
		return base
	}
	out := make([]string, 0, len(base)+1)
	replaced := false
	for _, entry := range base {
		if k, _, ok := strings.Cut(entry, "="); ok && k == "SSH_AUTH_SOCK" {
			out = append(out, "SSH_AUTH_SOCK="+sock)
			replaced = true
			continue
		}
		out = append(out, entry)
	}
	if !replaced {
		out = append(out, "SSH_AUTH_SOCK="+sock)
	}
	return out
}

func (a *Adapter) logf(format string, args ...any) {
	if a.Log != nil {
		a.Log(format, args...)
	}
}

func (a *Adapter) run(ctx context.Context, args []string) (string, string, int, error) {
	if a.Run != nil {
		return a.Run(ctx, args)
	}
	return runHerdr(ctx, herdrBinary(a.HerdrPath), args)
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
