package scan

import (
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

// TabKey names a tab uniquely across herdr machines: the tab id on the
// local server, and <tab id>@<machine> on another machine.
func TabKey(machine, tabID string) string {
	if machine == "" {
		return tabID
	}
	return tabID + "@" + machine
}

// Match finds a unique herdr tab for the identifier. Never guesses on 0 or N.
// Tabs and agents may come from several machines; a tab's agent must be on
// the same machine.
func Match(id *string, repo string, number int, template string, tabs []herdr.Tab, agents []herdr.Agent) (*Tab, string) {
	tab, _, warning := match(id, repo, number, template, tabs, agents)
	return tab, warning
}

// match is Match that also returns how many tabs carry the label.
func match(id *string, repo string, number int, template string, tabs []herdr.Tab, agents []herdr.Agent) (*Tab, int, string) {
	if id == nil {
		return nil, 0, fmt.Sprintf("%s#%d: no identifier matched title_id_pattern", repo, number)
	}
	label := strings.ReplaceAll(template, "{id}", *id)
	var hits []herdr.Tab
	for _, tab := range tabs {
		if tab.Label == label {
			hits = append(hits, tab)
		}
	}
	switch len(hits) {
	case 0:
		return nil, 0, fmt.Sprintf("%s: identifier matched but no herdr tab labeled '%s'", *id, label)
	case 1:
		tab, warning := attachAgent(*id, hits[0], agents)
		return tab, 1, warning
	default:
		ids := make([]string, len(hits))
		for i, tab := range hits {
			ids[i] = TabKey(tab.Machine, tab.TabID)
		}
		return nil, len(hits), fmt.Sprintf("%s: ambiguous herdr tab label '%s' matches %s", *id, label, strings.Join(ids, ", "))
	}
}

func attachAgent(id string, hit herdr.Tab, agents []herdr.Agent) (*Tab, string) {
	var matched []herdr.Agent
	for _, agent := range agents {
		if agent.TabID == hit.TabID && agent.Machine == hit.Machine {
			matched = append(matched, agent)
		}
	}
	out := &Tab{
		TabID:       hit.TabID,
		WorkspaceID: hit.WorkspaceID,
		Label:       hit.Label,
		AgentStatus: "none",
		Machine:     hit.Machine,
	}
	switch len(matched) {
	case 1:
		pane := matched[0].PaneID
		out.PaneID = &pane
		out.AgentStatus = matched[0].AgentStatus
		return out, ""
	case 0:
		return out, ""
	default:
		return out, fmt.Sprintf("%s: tab %s has %d agents; refusing to guess", id, TabKey(hit.Machine, hit.TabID), len(matched))
	}
}
