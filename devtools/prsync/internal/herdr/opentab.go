package herdr

import (
	"context"
	"encoding/json"
	"fmt"
	"strconv"
	"time"
)

// Workspace is one herdr workspace from `herdr workspace list`.
type Workspace struct {
	WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // herdr/brief wire format
	Label       string `json:"label"`
}

// NewTab is the tab `herdr tab create` made, with its root pane.
type NewTab struct {
	TabID       string
	WorkspaceID string
	PaneID      string
}

// WorkspaceList runs `herdr workspace list` and returns .result.workspaces.
func (c *Client) WorkspaceList(ctx context.Context) ([]Workspace, error) {
	raw, err := c.output(ctx, c.callTimeout(), "workspace", "list")
	if err != nil {
		return nil, err
	}
	var env struct {
		Result struct {
			Workspaces *[]Workspace `json:"workspaces"`
		} `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return nil, fmt.Errorf("decode workspace list: %w", err)
	}
	if env.Result.Workspaces == nil {
		return nil, fmt.Errorf("%w: workspace list missing workspaces", ErrDegraded)
	}
	return *env.Result.Workspaces, nil
}

// TabCreate runs `herdr tab create [--workspace W] [--cwd C] --label L
// --no-focus`. An empty workspace or cwd is left to herdr.
func (c *Client) TabCreate(ctx context.Context, workspaceID, cwd, label string) (NewTab, error) {
	args := []string{"tab", "create"}
	if workspaceID != "" {
		args = append(args, "--workspace", workspaceID)
	}
	if cwd != "" {
		args = append(args, "--cwd", cwd)
	}
	args = append(args, "--label", label, "--no-focus")
	raw, err := c.output(ctx, c.callTimeout(), args...)
	if err != nil {
		return NewTab{}, err
	}
	var env struct {
		Result struct {
			Tab struct {
				TabID       string `json:"tab_id"`       //nolint:tagliatelle // herdr/brief wire format
				WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // herdr/brief wire format
			} `json:"tab"`
			RootPane struct {
				PaneID string `json:"pane_id"` //nolint:tagliatelle // herdr/brief wire format
			} `json:"root_pane"` //nolint:tagliatelle // herdr/brief wire format
		} `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return NewTab{}, fmt.Errorf("decode tab create: %w", err)
	}
	r := env.Result
	if r.Tab.TabID == "" || r.RootPane.PaneID == "" {
		return NewTab{}, fmt.Errorf("%w: tab create missing tab or root pane", ErrDegraded)
	}
	return NewTab{TabID: r.Tab.TabID, WorkspaceID: r.Tab.WorkspaceID, PaneID: r.RootPane.PaneID}, nil
}

// AgentStart runs `herdr agent start <name> --kind <kind> --pane <pane>
// --timeout <ms> [-- args...]`. agent_not_ready (an agent blocked at
// startup) is not an error: the caller's ready wait decides.
func (c *Client) AgentStart(ctx context.Context, name, kind, pane string, args []string, timeout time.Duration) error {
	argv := []string{"agent", "start", name, "--kind", kind, "--pane", pane, "--timeout", strconv.FormatInt(timeout.Milliseconds(), 10)}
	if len(args) > 0 {
		argv = append(argv, "--")
		argv = append(argv, args...)
	}
	result, err := c.execute(ctx, timeout+promptTimeoutGrace, argv...)
	if err != nil {
		return err
	}
	if result.ExitCode == 0 {
		return nil
	}
	if promptErrorCode(result.Stderr, result.Output) == "agent_not_ready" {
		return nil
	}
	return &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
}
