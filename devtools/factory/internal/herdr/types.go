package herdr

import (
	"errors"
	"fmt"
)

const (
	// AgentStatusIdle means the agent is waiting for input.
	AgentStatusIdle = "idle"
	// AgentStatusWorking means the agent is working.
	AgentStatusWorking = "working"
	// AgentStatusBlocked means the agent is blocked on a person.
	AgentStatusBlocked = "blocked"
	// AgentStatusDone means the agent finished its turn.
	AgentStatusDone = "done"
	// AgentStatusUnknown means herdr could not classify the agent.
	AgentStatusUnknown = "unknown"
)

var (
	// ErrNotInstalled means the herdr binary is missing.
	ErrNotInstalled = errors.New("herdr not installed")
	// ErrUnsupported means the herdr version is missing or older than the minimum.
	ErrUnsupported = errors.New("herdr version unsupported")
	// ErrDegraded means a success envelope is missing a required field or has the wrong shape.
	ErrDegraded = errors.New("herdr response missing expected fields")
	// ErrAmbiguous means a label matched more than one workspace or tab.
	ErrAmbiguous = errors.New("herdr match ambiguous")
	// ErrTabNotFound means herdr tab close reported that the tab is gone.
	ErrTabNotFound = errors.New("herdr tab not found")
)

// ProcError is a non-zero exit from herdr.
type ProcError struct {
	ExitCode int    // Process exit code.
	Stdout   string // Standard output.
	Stderr   string // Standard error.
}

func (e *ProcError) Error() string {
	if e == nil {
		return ""
	}
	if e.Stderr != "" {
		return e.Stderr
	}
	if e.Stdout != "" {
		return e.Stdout
	}
	return fmt.Sprintf("exit %d", e.ExitCode)
}

// RunError means herdr could not be run or did not finish, for example a timeout.
type RunError struct {
	Bin string // Binary that was run.
	Err error  // Executor error.
}

func (e *RunError) Error() string {
	if e == nil {
		return ""
	}
	return fmt.Sprintf("run %s: %v", e.Bin, e.Err)
}

func (e *RunError) Unwrap() error {
	if e == nil {
		return nil
	}
	return e.Err
}

// Workspace is one entry from `herdr workspace list`.
type Workspace struct {
	// Workspace id.
	ID    string `json:"workspace_id"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	Label string `json:"label"`        // Workspace label.
	// One of the AgentStatus constants.
	AgentStatus string `json:"agent_status"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
}

// Tab is one entry from `herdr tab list`.
type Tab struct {
	// Tab id.
	ID string `json:"tab_id"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	// Workspace that owns the tab.
	WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	Label       string `json:"label"`        // Tab label.
	// One of the AgentStatus constants.
	AgentStatus string `json:"agent_status"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	// Number of panes in the tab.
	PaneCount int `json:"pane_count"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
}

// Pane is a herdr pane. ID is PaneInfo.pane_id.
// AgentStatus is empty when the response omits agent_status.
// Cwd and ForegroundCwd are empty when herdr omits them or sends null.
type Pane struct {
	// Pane id.
	ID string `json:"pane_id"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	// Tab that owns the pane.
	TabID string `json:"tab_id"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	// Workspace that owns the pane.
	WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	// One of the AgentStatus constants when herdr sent it.
	AgentStatus string `json:"agent_status"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	Cwd         string `json:"cwd"`          // Pane directory.
	// Foreground process directory.
	ForegroundCwd string `json:"foreground_cwd"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
}
