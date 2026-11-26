package herdr

import (
	"fmt"
	"strings"
)

// FindWorkspace returns the workspace with label.
// More than one match is ErrAmbiguous and names every matching id.
// No match returns ok false and a nil error.
func FindWorkspace(ws []Workspace, label string) (Workspace, bool, error) {
	var ids []string
	var match Workspace
	for _, item := range ws {
		if item.Label != label {
			continue
		}
		ids = append(ids, item.ID)
		match = item
	}
	switch len(ids) {
	case 0:
		return Workspace{}, false, nil
	case 1:
		return match, true, nil
	default:
		return Workspace{}, false, fmt.Errorf("%w: workspace label %q matched %s", ErrAmbiguous, label, strings.Join(ids, ", "))
	}
}

// FindTab returns the tab with label.
// An empty workspaceID searches every workspace.
// More than one match is ErrAmbiguous and names every matching id.
// No match returns ok false and a nil error.
func FindTab(tabs []Tab, workspaceID, label string) (Tab, bool, error) {
	var ids []string
	var match Tab
	for _, tab := range tabs {
		if tab.Label != label {
			continue
		}
		if workspaceID != "" && tab.WorkspaceID != workspaceID {
			continue
		}
		ids = append(ids, tab.ID)
		match = tab
	}
	switch len(ids) {
	case 0:
		return Tab{}, false, nil
	case 1:
		return match, true, nil
	default:
		where := "any workspace"
		if workspaceID != "" {
			where = fmt.Sprintf("workspace %q", workspaceID)
		}
		return Tab{}, false, fmt.Errorf("%w: tab label %q in %s matched %s", ErrAmbiguous, label, where, strings.Join(ids, ", "))
	}
}

// AnyWorking returns the first tab whose agent is working.
func AnyWorking(tabs []Tab) (Tab, bool) {
	for _, tab := range tabs {
		if tab.AgentStatus == AgentStatusWorking {
			return tab, true
		}
	}
	return Tab{}, false
}
