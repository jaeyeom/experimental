// Package herdr reads a normalized snapshot from the herdr CLI.
// It runs only workspace list, tab list --workspace, agent list, and pane list.
package herdr

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"strings"
)

// Runner runs herdr with args and returns stdout. args does not include
// the herdr binary.
type Runner func(ctx context.Context, args []string) ([]byte, error)

// Snapshot is the live herdr state orgctl compares with a spec.
type Snapshot struct {
	Workspaces []Workspace `json:"workspaces"`
	Tabs       []Tab       `json:"tabs"`
	Agents     []Agent     `json:"agents"`
	Panes      []Pane      `json:"panes"`
}

// Workspace is one herdr workspace.
type Workspace struct {
	ID    string `json:"id"`
	Label string `json:"label"`
}

// Tab is one herdr tab.
type Tab struct {
	ID        string `json:"id"`
	Workspace string `json:"workspace"`
	Label     string `json:"label"`
}

// Agent is one recognized agent.
// Kind is the product herdr reports in agent_session.source, with a
// herdr: prefix removed. agent_session.kind says how the session id is
// stored, and SessionID is agent_session.value.
type Agent struct {
	Name      string `json:"name"`
	Pane      string `json:"pane"`
	Tab       string `json:"tab"`
	Cwd       string `json:"cwd"`
	Status    string `json:"status"`
	Kind      string `json:"kind"`
	SessionID string `json:"agentSessionId"`
}

// Pane is one terminal pane.
type Pane struct {
	ID  string `json:"id"`
	Tab string `json:"tab"`
	Cwd string `json:"cwd"`
}

// NotReadError means the herdr snapshot could not be read.
// Plan prints the error and continues.
type NotReadError struct {
	Err error
}

func (e *NotReadError) Error() string {
	return fmt.Sprintf("herdr: not read (%v)", e.Err)
}

func (e *NotReadError) Unwrap() error { return e.Err }

// Read loads a snapshot. A nil run uses Exec.
func Read(ctx context.Context, run Runner) (Snapshot, error) {
	if run == nil {
		run = Exec
	}
	snap := Snapshot{
		Workspaces: []Workspace{},
		Tabs:       []Tab{},
		Agents:     []Agent{},
		Panes:      []Pane{},
	}
	var workspaces struct {
		Workspaces []struct {
			ID    string `json:"workspace_id"` //nolint:tagliatelle // herdr wire format
			Label string `json:"label"`
		} `json:"workspaces"`
	}
	if err := call(ctx, run, []string{"workspace", "list"}, &workspaces); err != nil {
		return Snapshot{}, err
	}
	for _, ws := range workspaces.Workspaces {
		snap.Workspaces = append(snap.Workspaces, Workspace{ID: ws.ID, Label: ws.Label})
		if ws.ID == "" {
			continue
		}
		tabs, err := readTabs(ctx, run, ws.ID)
		if err != nil {
			return Snapshot{}, err
		}
		snap.Tabs = append(snap.Tabs, tabs...)
	}
	agents, err := readAgents(ctx, run)
	if err != nil {
		return Snapshot{}, err
	}
	snap.Agents = agents
	panes, err := readPanes(ctx, run)
	if err != nil {
		return Snapshot{}, err
	}
	snap.Panes = panes
	return snap, nil
}

// Exec runs the installed herdr binary. args must be an allowlisted read.
func Exec(ctx context.Context, args []string) ([]byte, error) {
	if !allowlisted(args) {
		return nil, fmt.Errorf("herdr: command not allowed: %s", strings.Join(args, " "))
	}
	cmd := exec.CommandContext(ctx, "herdr", args...) //nolint:gosec // G204: argv is an allowlisted herdr read
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("herdr %s: %w", strings.Join(args, " "), err)
	}
	return out, nil
}

func readTabs(ctx context.Context, run Runner, workspace string) ([]Tab, error) {
	var raw struct {
		Tabs []struct {
			ID        string `json:"tab_id"`       //nolint:tagliatelle // herdr wire format
			Workspace string `json:"workspace_id"` //nolint:tagliatelle // herdr wire format
			Label     string `json:"label"`
		} `json:"tabs"`
	}
	if err := call(ctx, run, []string{"tab", "list", "--workspace", workspace}, &raw); err != nil {
		return nil, err
	}
	tabs := make([]Tab, 0, len(raw.Tabs))
	for _, tab := range raw.Tabs {
		ws := tab.Workspace
		if ws == "" {
			ws = workspace
		}
		tabs = append(tabs, Tab{ID: tab.ID, Workspace: ws, Label: tab.Label})
	}
	return tabs, nil
}

func readAgents(ctx context.Context, run Runner) ([]Agent, error) {
	var raw struct {
		Agents []struct {
			Name    string `json:"agent"`
			PaneID  string `json:"pane_id"` //nolint:tagliatelle // herdr wire format
			TabID   string `json:"tab_id"`  //nolint:tagliatelle // herdr wire format
			Cwd     string `json:"cwd"`
			Status  string `json:"agent_status"` //nolint:tagliatelle // herdr wire format
			Session *struct {
				Source string `json:"source"`
				Value  string `json:"value"`
			} `json:"agent_session"` //nolint:tagliatelle // herdr wire format
		} `json:"agents"`
	}
	if err := call(ctx, run, []string{"agent", "list"}, &raw); err != nil {
		return nil, err
	}
	agents := make([]Agent, 0, len(raw.Agents))
	for _, agent := range raw.Agents {
		item := Agent{
			Name: agent.Name, Pane: agent.PaneID, Tab: agent.TabID,
			Cwd: agent.Cwd, Status: agent.Status,
		}
		if agent.Session != nil {
			item.Kind = strings.TrimPrefix(agent.Session.Source, "herdr:")
			item.SessionID = agent.Session.Value
		}
		agents = append(agents, item)
	}
	return agents, nil
}

func readPanes(ctx context.Context, run Runner) ([]Pane, error) {
	var raw struct {
		Panes []struct {
			ID    string `json:"pane_id"` //nolint:tagliatelle // herdr wire format
			TabID string `json:"tab_id"`  //nolint:tagliatelle // herdr wire format
			Cwd   string `json:"cwd"`
		} `json:"panes"`
	}
	if err := call(ctx, run, []string{"pane", "list"}, &raw); err != nil {
		return nil, err
	}
	panes := make([]Pane, 0, len(raw.Panes))
	for _, pane := range raw.Panes {
		panes = append(panes, Pane{ID: pane.ID, Tab: pane.TabID, Cwd: pane.Cwd})
	}
	return panes, nil
}

func call(ctx context.Context, run Runner, args []string, dest any) error {
	if !allowlisted(args) {
		return fmt.Errorf("herdr: command not allowed: %s", strings.Join(args, " "))
	}
	out, err := run(ctx, args)
	if err != nil {
		return &NotReadError{Err: err}
	}
	var env struct {
		Result json.RawMessage `json:"result"`
	}
	if err := json.Unmarshal(out, &env); err != nil {
		return &NotReadError{Err: err}
	}
	if len(env.Result) == 0 {
		return &NotReadError{Err: errors.New("herdr returned no result")}
	}
	if err := json.Unmarshal(env.Result, dest); err != nil {
		return &NotReadError{Err: err}
	}
	return nil
}

func allowlisted(args []string) bool {
	switch strings.Join(args, " ") {
	case "workspace list", "agent list", "pane list":
		return true
	}
	return len(args) == 4 && args[0] == "tab" && args[1] == "list" && args[2] == "--workspace" && safeID(args[3])
}

func safeID(id string) bool {
	return id != "" && !strings.HasPrefix(id, "-") && !strings.ContainsAny(id, " \t\n")
}
