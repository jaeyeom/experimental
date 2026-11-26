// Package herdr is a thin client for the herdr CLI.
// Commands run through an injected go-cmdexec Executor.
package herdr

import (
	"context"
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	defaultCallTimeout = 60 * time.Second
	// RemoteReadTimeout bounds a list call on a herdr saved machine.
	RemoteReadTimeout = 15 * time.Second
	// machineStatusTimeout bounds the reachability probe.
	machineStatusTimeout = 10 * time.Second
	// promptTimeoutGrace lets herdr finish and print its own timeout error
	// before the executor kills the process.
	promptTimeoutGrace = 5 * time.Second
)

// herdrVersionRe matches `herdr <dotted version>`, the same shape prsync accepts.
var herdrVersionRe = regexp.MustCompile(`herdr ([0-9.]+)`)

// Client runs herdr through an injected executor.
type Client struct {
	exec executor.Executor
	bin  string
	// machine is the herdr saved machine every call names with --machine.
	// Empty is the local herdr server.
	machine string
	// readTimeout bounds the list calls. A remote machine whose SSH agent
	// is locked can hang instead of failing, so remote reads get a short deadline.
	readTimeout time.Duration
}

// NewClient returns a client that runs bin through exec.
func NewClient(exec executor.Executor, bin string) *Client {
	return &Client{exec: exec, bin: bin, readTimeout: defaultCallTimeout}
}

// OnMachine returns a client that runs every call as
// `herdr --machine MACHINE ...`. An empty machine is the local server and
// returns a copy of c. A remote client's list calls time out after
// RemoteReadTimeout. Every other call keeps its own timeout.
func (c *Client) OnMachine(machine string) *Client {
	out := *c
	out.machine = machine
	out.readTimeout = defaultCallTimeout
	if machine != "" {
		out.readTimeout = RemoteReadTimeout
	}
	return &out
}

// Machine is the herdr saved machine this client names. Empty is local.
func (c *Client) Machine() string {
	return c.machine
}

// RequireMin runs `herdr --version` and checks that it is at least minimum.
// minimum is a dotted version such as "0.9.0".
// A missing binary is ErrNotInstalled. An older or unreadable version is ErrUnsupported.
func (c *Client) RequireMin(ctx context.Context, minimum string) error {
	result, err := c.execute(ctx, defaultCallTimeout, "--version")
	if err != nil {
		if errors.Is(err, ErrNotInstalled) {
			return err
		}
		return fmt.Errorf("%w: %w", ErrUnsupported, err)
	}
	text := result.Output
	if !herdrVersionRe.MatchString(text) {
		text = result.Stderr
	}
	got, ok := parseHerdrVersion(text)
	if !ok {
		raw := strings.TrimSpace(result.Output + "\n" + result.Stderr)
		return fmt.Errorf("%w: expected herdr <version>, found %q", ErrUnsupported, raw)
	}
	cmp, ok := compareVersions(got, minimum)
	if !ok {
		return fmt.Errorf("%w: expected a dotted version, found %q", ErrUnsupported, got)
	}
	if cmp < 0 {
		return fmt.Errorf("%w: expected herdr >= %s, found %s", ErrUnsupported, minimum, got)
	}
	return nil
}

// Workspaces runs `herdr workspace list` and returns .result.workspaces.
func (c *Client) Workspaces(ctx context.Context) ([]Workspace, error) {
	raw, err := c.output(ctx, c.readTimeout, "workspace", "list")
	if err != nil {
		return nil, err
	}
	result, err := decodeEnvelope(raw, "workspace_list")
	if err != nil {
		return nil, err
	}
	return decodeList(result, "workspaces", decodeWorkspace)
}

// Tabs runs `herdr tab list` and returns .result.tabs.
func (c *Client) Tabs(ctx context.Context) ([]Tab, error) {
	raw, err := c.output(ctx, c.readTimeout, "tab", "list")
	if err != nil {
		return nil, err
	}
	result, err := decodeEnvelope(raw, "tab_list")
	if err != nil {
		return nil, err
	}
	return decodeList(result, "tabs", decodeTab)
}

// Panes runs `herdr pane list` and returns .result.panes.
func (c *Client) Panes(ctx context.Context) ([]Pane, error) {
	raw, err := c.output(ctx, c.readTimeout, "pane", "list")
	if err != nil {
		return nil, err
	}
	result, err := decodeEnvelope(raw, "pane_list")
	if err != nil {
		return nil, err
	}
	return decodeList(result, "panes", decodeListedPane)
}

// CreateWorkspace runs `herdr workspace create --label L --cwd C --no-focus`.
// It returns the created workspace and its root pane.
// env entries are KEY=VALUE, each passed as --env for the launched shell.
func (c *Client) CreateWorkspace(ctx context.Context, label, cwd string, env []string) (Workspace, Pane, error) {
	args, err := withEnv([]string{"workspace", "create", "--label", label, "--cwd", cwd}, env)
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	raw, err := c.output(ctx, defaultCallTimeout, append(args, "--no-focus")...)
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	return decodeWorkspaceCreated(raw)
}

// CloseTab runs `herdr tab close TAB`. A missing tab is ErrTabNotFound.
// The string is result.type. ok and any other success result are a closed tab.
func (c *Client) CloseTab(ctx context.Context, tabID string) (string, error) {
	if tabID == "" {
		return "", errors.New("herdr tab close: expected a tab id, found empty")
	}
	raw, err := c.output(ctx, defaultCallTimeout, "tab", "close", tabID)
	if err != nil {
		var proc *ProcError
		if errors.As(err, &proc) && commandErrorCode(proc.Stdout, proc.Stderr) == "tab_not_found" {
			return "", fmt.Errorf("%w: %s", ErrTabNotFound, proc.Error())
		}
		return "", err
	}
	return decodeCloseResult(raw, tabID)
}

// CreateTab runs `herdr tab create --workspace W --label L --cwd C --no-focus`.
// It returns .result.tab and .result.root_pane.
// env entries are KEY=VALUE, each passed as --env for the launched shell.
func (c *Client) CreateTab(ctx context.Context, wsID, label, cwd string, env []string) (Tab, Pane, error) {
	args, err := withEnv([]string{"tab", "create", "--workspace", wsID, "--label", label, "--cwd", cwd}, env)
	if err != nil {
		return Tab{}, Pane{}, err
	}
	raw, err := c.output(ctx, defaultCallTimeout, append(args, "--no-focus")...)
	if err != nil {
		return Tab{}, Pane{}, err
	}
	return decodeTabCreated(raw)
}

// withEnv appends --env KEY=VALUE for each entry. An entry without a key
// and an = is an error.
func withEnv(args, env []string) ([]string, error) {
	for _, kv := range env {
		key, _, ok := strings.Cut(kv, "=")
		if !ok || key == "" {
			return nil, fmt.Errorf("herdr --env: expected KEY=VALUE, found %q", kv)
		}
		args = append(args, "--env", kv)
	}
	return args, nil
}

// StartAgent runs `herdr agent start NAME --kind K --pane P --timeout MS`.
// timeout is sent as milliseconds. Non-empty agentArgs are appended after --.
func (c *Client) StartAgent(ctx context.Context, name, kind, paneID string, timeout time.Duration, agentArgs []string) error {
	args := []string{"agent", "start", name, "--kind", kind, "--pane", paneID, "--timeout", formatMS(timeout)}
	if len(agentArgs) > 0 {
		args = append(args, "--")
		args = append(args, agentArgs...)
	}
	raw, err := c.output(ctx, timeout+promptTimeoutGrace, args...)
	if err != nil {
		return err
	}
	result, err := decodeEnvelope(raw, "agent_started")
	if err != nil {
		return err
	}
	if err := requireAgent(result); err != nil {
		return err
	}
	return requireStringArray(result, "argv")
}

// Prompt runs `herdr agent prompt TARGET TEXT --wait --until S... --timeout MS`.
// timeout is sent as milliseconds. Each until value is one --until flag.
func (c *Client) Prompt(ctx context.Context, target, text string, until []string, timeout time.Duration) error {
	args := []string{"agent", "prompt", target, text, "--wait"}
	for _, status := range until {
		args = append(args, "--until", status)
	}
	args = append(args, "--timeout", formatMS(timeout))
	raw, err := c.output(ctx, timeout+promptTimeoutGrace, args...)
	if err != nil {
		return err
	}
	result, err := decodeEnvelope(raw, "agent_prompted")
	if err != nil {
		return err
	}
	return requireAgent(result)
}

func decodeWorkspaceCreated(raw []byte) (Workspace, Pane, error) {
	result, err := decodeEnvelope(raw, "workspace_created")
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	// workspace_created requires tab as well as the workspace and root pane
	// this method returns.
	wsRaw, err := objectField(result, "workspace")
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	tabRaw, err := objectField(result, "tab")
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	paneRaw, err := objectField(result, "root_pane")
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	if _, err := decodeTab(tabRaw); err != nil {
		return Workspace{}, Pane{}, fmt.Errorf("workspace created tab: %w", err)
	}
	ws, err := decodeWorkspace(wsRaw)
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	pane, err := decodePane(paneRaw)
	if err != nil {
		return Workspace{}, Pane{}, err
	}
	return ws, pane, nil
}

func decodeTabCreated(raw []byte) (Tab, Pane, error) {
	result, err := decodeEnvelope(raw, "tab_created")
	if err != nil {
		return Tab{}, Pane{}, err
	}
	tabRaw, err := objectField(result, "tab")
	if err != nil {
		return Tab{}, Pane{}, err
	}
	paneRaw, err := objectField(result, "root_pane")
	if err != nil {
		return Tab{}, Pane{}, err
	}
	tab, err := decodeTab(tabRaw)
	if err != nil {
		return Tab{}, Pane{}, err
	}
	pane, err := decodePane(paneRaw)
	if err != nil {
		return Tab{}, Pane{}, err
	}
	return tab, pane, nil
}

func (c *Client) output(ctx context.Context, timeout time.Duration, args ...string) ([]byte, error) {
	result, err := c.execute(ctx, timeout, args...)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
	}
	return []byte(result.Output), nil
}

func (c *Client) execute(ctx context.Context, timeout time.Duration, args ...string) (*executor.ExecutionResult, error) {
	if c.machine != "" {
		args = append([]string{"--machine", c.machine}, args...)
	}
	result, err := c.exec.Execute(ctx, executor.ToolConfig{
		Command: c.bin,
		Args:    args,
		Timeout: timeout,
	})
	if err != nil {
		var notFound *executor.ExecutableNotFoundError
		if errors.As(err, &notFound) {
			return nil, fmt.Errorf("%w: %w", ErrNotInstalled, err)
		}
		return nil, &RunError{Bin: c.bin, Err: err}
	}
	if result == nil {
		return nil, fmt.Errorf("run %s: expected an execution result, found nil", c.bin)
	}
	return result, nil
}

func formatMS(timeout time.Duration) string {
	return strconv.FormatInt(timeout.Milliseconds(), 10)
}

func parseHerdrVersion(text string) (string, bool) {
	match := herdrVersionRe.FindStringSubmatch(text)
	if match == nil {
		return "", false
	}
	if _, ok := parseDotVersion(match[1]); !ok {
		return "", false
	}
	return match[1], true
}

func compareVersions(a, b string) (int, bool) {
	as, ok := parseDotVersion(a)
	if !ok {
		return 0, false
	}
	bs, ok := parseDotVersion(b)
	if !ok {
		return 0, false
	}
	n := max(len(as), len(bs))
	for i := 0; i < n; i++ {
		av, bv := 0, 0
		if i < len(as) {
			av = as[i]
		}
		if i < len(bs) {
			bv = bs[i]
		}
		if av < bv {
			return -1, true
		}
		if av > bv {
			return 1, true
		}
	}
	return 0, true
}

func parseDotVersion(s string) ([]int, bool) {
	if s == "" {
		return nil, false
	}
	parts := strings.Split(s, ".")
	out := make([]int, len(parts))
	for i, part := range parts {
		if part == "" {
			return nil, false
		}
		n, err := strconv.Atoi(part)
		if err != nil {
			return nil, false
		}
		out[i] = n
	}
	return out, true
}
