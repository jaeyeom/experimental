package scan

import (
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

// Match finds a unique herdr tab for the identifier. Never guesses on 0 or N.
func Match(id *string, repo string, number int, template string, tabs []herdr.Tab, agents []herdr.Agent) (*Tab, string) {
	if id == nil {
		return nil, fmt.Sprintf("%s#%d: no identifier matched title_id_pattern", repo, number)
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
		return nil, fmt.Sprintf("%s: identifier matched but no herdr tab labeled '%s'", *id, label)
	case 1:
		return attachAgent(*id, hits[0], agents)
	default:
		ids := make([]string, len(hits))
		for i, tab := range hits {
			ids[i] = tab.TabID
		}
		return nil, fmt.Sprintf("%s: ambiguous herdr tab label '%s' matches %s", *id, label, strings.Join(ids, ", "))
	}
}

func attachAgent(id string, hit herdr.Tab, agents []herdr.Agent) (*Tab, string) {
	var matched []herdr.Agent
	for _, agent := range agents {
		if agent.TabID == hit.TabID {
			matched = append(matched, agent)
		}
	}
	out := &Tab{
		TabID:       hit.TabID,
		WorkspaceID: hit.WorkspaceID,
		Label:       hit.Label,
		AgentStatus: "none",
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
		return out, fmt.Sprintf("%s: tab %s has %d agents; refusing to guess", id, hit.TabID, len(matched))
	}
}
