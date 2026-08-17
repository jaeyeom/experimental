package herdr

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	defaultCallTimeout = 60 * time.Second
	promptTimeoutGrace = 5 * time.Second
)

// Same regex as devtools/setup-dev/ansible/packages_data.go for herdr --version.
var herdrVersionRe = regexp.MustCompile(`herdr ([0-9.]+)`)

// Client shells out to herdr_bin through an injected executor.
type Client struct {
	exec executor.Executor
	bin  string
}

// NewClient returns a herdr adapter that invokes bin via exec.
func NewClient(exec executor.Executor, bin string) *Client {
	return &Client{exec: exec, bin: bin}
}

// RequireMin runs `--version` and checks it is at least minimum (e.g. "0.8.0").
func (c *Client) RequireMin(ctx context.Context, minimum string) error {
	result, err := c.execute(ctx, defaultCallTimeout, "--version")
	if err != nil {
		var notFound *executor.ExecutableNotFoundError
		if errors.As(err, &notFound) {
			return fmt.Errorf("%w: %w", ErrNotInstalled, err)
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
		return fmt.Errorf("%w: cannot parse herdr version from %q", ErrUnsupported, raw)
	}
	cmp, ok := compareVersions(got, minimum)
	if !ok {
		return fmt.Errorf("%w: cannot parse herdr version from %q", ErrUnsupported, got)
	}
	if cmp < 0 {
		return fmt.Errorf("%w: herdr %s is older than %s", ErrUnsupported, got, minimum)
	}
	return nil
}

// TabList runs `herdr tab list` and returns .result.tabs.
func (c *Client) TabList(ctx context.Context) ([]Tab, error) {
	raw, err := c.output(ctx, defaultCallTimeout, "tab", "list")
	if err != nil {
		return nil, err
	}
	var env struct {
		Result struct {
			Tabs *[]Tab `json:"tabs"`
		} `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return nil, fmt.Errorf("decode tab list: %w", err)
	}
	if env.Result.Tabs == nil {
		return nil, fmt.Errorf("%w: tab list missing tabs", ErrDegraded)
	}
	return *env.Result.Tabs, nil
}

// AgentList runs `herdr agent list` and returns .result.agents.
func (c *Client) AgentList(ctx context.Context) ([]Agent, error) {
	raw, err := c.output(ctx, defaultCallTimeout, "agent", "list")
	if err != nil {
		return nil, err
	}
	var env struct {
		Result struct {
			Agents *[]Agent `json:"agents"`
		} `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return nil, fmt.Errorf("decode agent list: %w", err)
	}
	if env.Result.Agents == nil {
		return nil, fmt.Errorf("%w: agent list missing agents", ErrDegraded)
	}
	return *env.Result.Agents, nil
}

// PaneCurrent runs `herdr pane current --pane <paneID>`.
func (c *Client) PaneCurrent(ctx context.Context, paneID string) (Pane, error) {
	raw, err := c.output(ctx, defaultCallTimeout, "pane", "current", "--pane", paneID)
	if err != nil {
		return Pane{}, err
	}
	var env struct {
		Result struct {
			PaneID      string `json:"pane_id"`      //nolint:tagliatelle // herdr/brief wire format
			TabID       string `json:"tab_id"`       //nolint:tagliatelle // herdr/brief wire format
			WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // herdr/brief wire format
			Pane        *Pane  `json:"pane"`
		} `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return Pane{}, fmt.Errorf("decode pane current: %w", err)
	}
	if env.Result.PaneID != "" {
		return Pane{
			PaneID:      env.Result.PaneID,
			TabID:       env.Result.TabID,
			WorkspaceID: env.Result.WorkspaceID,
		}, nil
	}
	if env.Result.Pane != nil && env.Result.Pane.PaneID != "" {
		return *env.Result.Pane, nil
	}
	return Pane{}, fmt.Errorf("%w: pane current missing pane", ErrDegraded)
}

// RunnerPane returns HERDR_PANE_ID. An unset value must not call pane current.
func (c *Client) RunnerPane(ctx context.Context) string {
	paneID := os.Getenv("HERDR_PANE_ID")
	if paneID == "" {
		return ""
	}
	_, _ = c.PaneCurrent(ctx, paneID)
	return paneID
}

// Prompt runs `herdr agent prompt` and maps the process result to a PromptOutcome.
func (c *Client) Prompt(ctx context.Context, paneID, text string, until []string, timeout time.Duration) PromptOutcome {
	args := []string{"agent", "prompt", paneID, text, "--wait"}
	for _, tok := range until {
		args = append(args, "--until", tok)
	}
	args = append(args, "--timeout", strconv.FormatInt(timeout.Milliseconds(), 10))

	result, err := c.execute(ctx, timeout+promptTimeoutGrace, args...)
	if err != nil {
		return PromptOutcome{Status: PromptError, Err: err}
	}
	if result.ExitCode == 0 {
		if agent, ok := parsePromptAgent(result.Output); ok {
			return PromptOutcome{Status: PromptMatched, Agent: agent}
		}
	}
	if code := promptErrorCode(result.Stderr, result.Output); code != "" {
		switch code {
		case "agent_prompt_stalled":
			return PromptOutcome{Status: PromptStalled}
		case "timeout":
			return PromptOutcome{Status: PromptTimeout}
		default:
			return PromptOutcome{Status: PromptError, Err: fmt.Errorf("herdr prompt: %s", code)}
		}
	}
	if ctx.Err() != nil {
		return PromptOutcome{Status: PromptError, Err: ctx.Err()}
	}
	return PromptOutcome{Status: PromptError, Err: fmt.Errorf("herdr prompt: unparseable result")}
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
	result, err := c.exec.Execute(ctx, executor.ToolConfig{
		Command: c.bin,
		Args:    args,
		Timeout: timeout,
	})
	if err != nil {
		return nil, fmt.Errorf("run %s: %w", c.bin, err)
	}
	return result, nil
}

func parseHerdrVersion(text string) (string, bool) {
	m := herdrVersionRe.FindStringSubmatch(text)
	if m == nil {
		return "", false
	}
	if _, ok := parseDotVersion(m[1]); !ok {
		return "", false
	}
	return m[1], true
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
	n := len(as)
	if len(bs) > n {
		n = len(bs)
	}
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
	for i, p := range parts {
		if p == "" {
			return nil, false
		}
		n, err := strconv.Atoi(p)
		if err != nil {
			return nil, false
		}
		out[i] = n
	}
	return out, true
}

func parsePromptAgent(raw string) (Agent, bool) {
	var env struct {
		Result *struct {
			Agent *Agent `json:"agent"`
		} `json:"result"`
	}
	if json.Unmarshal([]byte(raw), &env) != nil || env.Result == nil || env.Result.Agent == nil {
		return Agent{}, false
	}
	if env.Result.Agent.PaneID == "" {
		return Agent{}, false
	}
	return *env.Result.Agent, true
}

func promptErrorCode(blobs ...string) string {
	for _, raw := range blobs {
		if code, ok := jsonErrorCode(raw); ok {
			return code
		}
	}
	return ""
}

func jsonErrorCode(raw string) (string, bool) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return "", false
	}
	var env struct {
		Error *struct {
			Code string `json:"code"`
			Type string `json:"type"`
		} `json:"error"`
		Code string `json:"code"`
	}
	if json.Unmarshal([]byte(raw), &env) != nil {
		return "", false
	}
	if env.Error != nil {
		if env.Error.Code != "" {
			return env.Error.Code, true
		}
		if env.Error.Type != "" {
			return env.Error.Type, true
		}
	}
	if env.Code != "" {
		return env.Code, true
	}
	return "", false
}
