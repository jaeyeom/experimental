// Package herdr talks to herdr through its CLI.
package herdr

import (
	"errors"
	"fmt"
)

// ErrNotInstalled is returned when herdr_bin is missing from PATH.
var ErrNotInstalled = errors.New("herdr not installed")

// ErrUnsupported is returned when --version is unparseable or older than the minimum.
var ErrUnsupported = errors.New("herdr version unsupported")

// ErrDegraded is returned when a herdr success envelope is missing an expected field.
var ErrDegraded = errors.New("herdr response missing expected fields")

// ErrTabNotFound is returned when `herdr tab close` reports the tab is gone.
var ErrTabNotFound = errors.New("herdr tab not found")

// ProcError is a non-zero process exit from herdr.
type ProcError struct {
	ExitCode int
	Stdout   string
	Stderr   string
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

// Tab is one herdr tab from `herdr tab list`.
type Tab struct {
	TabID       string `json:"tab_id"`       //nolint:tagliatelle // herdr/brief wire format
	WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // herdr/brief wire format
	Label       string `json:"label"`
	AgentStatus string `json:"agent_status"` //nolint:tagliatelle // herdr/brief wire format
	PaneCount   int    `json:"pane_count"`   //nolint:tagliatelle // herdr/brief wire format
}

// Agent is one herdr agent from `herdr agent list`.
type Agent struct {
	PaneID      string `json:"pane_id"` //nolint:tagliatelle // herdr/brief wire format
	TabID       string `json:"tab_id"`  //nolint:tagliatelle // herdr/brief wire format
	Agent       string `json:"agent"`
	AgentStatus string `json:"agent_status"` //nolint:tagliatelle // herdr/brief wire format
}

// Pane is the focused or addressed pane from `herdr pane current`.
type Pane struct {
	PaneID      string `json:"pane_id"`      //nolint:tagliatelle // herdr/brief wire format
	TabID       string `json:"tab_id"`       //nolint:tagliatelle // herdr/brief wire format
	WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // herdr/brief wire format
}

// PromptStatus is the classified outcome of `herdr agent prompt`.
type PromptStatus string

// Prompt outcome values.
const (
	PromptMatched PromptStatus = "matched"
	PromptStalled PromptStatus = "stalled"
	PromptTimeout PromptStatus = "timeout"
	PromptError   PromptStatus = "error"
)

// PromptOutcome is the mapped result of AgentPrompt.
type PromptOutcome struct {
	Status PromptStatus
	Agent  Agent
	Err    error
}
