package herdr

import (
	"context"
	"encoding/json"
	"fmt"
)

// ProcessInfo is the result of `herdr pane process-info`.
type ProcessInfo struct {
	PaneID                   string              `json:"pane_id"`                     //nolint:tagliatelle // herdr wire format
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

// PaneProcessInfo runs `herdr pane process-info --pane <paneID>`.
func (c *Client) PaneProcessInfo(ctx context.Context, paneID string) (ProcessInfo, error) {
	raw, err := c.output(ctx, c.callTimeout(), "pane", "process-info", "--pane", paneID)
	if err != nil {
		return ProcessInfo{}, err
	}
	var env struct {
		Result struct {
			ProcessInfo *ProcessInfo `json:"process_info"` //nolint:tagliatelle // herdr wire format
		} `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return ProcessInfo{}, fmt.Errorf("decode pane process-info: %w", err)
	}
	if env.Result.ProcessInfo == nil {
		return ProcessInfo{}, fmt.Errorf("%w: pane process-info missing process_info", ErrDegraded)
	}
	return *env.Result.ProcessInfo, nil
}
