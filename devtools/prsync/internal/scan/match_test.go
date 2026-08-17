package scan

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

func TestMatch(t *testing.T) {
	t.Parallel()

	id := "PROJ-123"
	tab := herdr.Tab{TabID: "w2:tC", WorkspaceID: "w2", Label: "PROJ-123", AgentStatus: "idle"}
	wipTab := herdr.Tab{TabID: "w2:tW", WorkspaceID: "w2", Label: "wip/PROJ-123"}
	agent := herdr.Agent{PaneID: "w2:pC", TabID: "w2:tC", Agent: "codex", AgentStatus: "idle"}

	tests := []struct {
		name       string
		id         *string
		repo       string
		number     int
		template   string
		tabs       []herdr.Tab
		agents     []herdr.Agent
		wantNil    bool
		wantTabID  string
		wantPane   *string
		wantStatus string
		wantWarn   string
	}{
		{
			name:     "zero tabs",
			id:       &id,
			template: "{id}",
			wantNil:  true,
			wantWarn: "PROJ-123: identifier matched but no herdr tab labeled 'PROJ-123'",
		},
		{
			name:       "one tab one agent",
			id:         &id,
			template:   "{id}",
			tabs:       []herdr.Tab{tab},
			agents:     []herdr.Agent{agent},
			wantTabID:  "w2:tC",
			wantPane:   strPtr("w2:pC"),
			wantStatus: "idle",
		},
		{
			name:       "one tab zero agents",
			id:         &id,
			template:   "{id}",
			tabs:       []herdr.Tab{tab},
			wantTabID:  "w2:tC",
			wantPane:   nil,
			wantStatus: "none",
		},
		{
			name:       "one tab two agents",
			id:         &id,
			template:   "{id}",
			tabs:       []herdr.Tab{tab},
			agents:     []herdr.Agent{agent, {PaneID: "w2:pD", TabID: "w2:tC", AgentStatus: "idle"}},
			wantTabID:  "w2:tC",
			wantPane:   nil,
			wantStatus: "none",
			wantWarn:   "PROJ-123: tab w2:tC has 2 agents; refusing to guess",
		},
		{
			name:     "two tabs never guess",
			id:       &id,
			template: "{id}",
			tabs:     []herdr.Tab{tab, {TabID: "w3:tC", WorkspaceID: "w3", Label: "PROJ-123"}},
			wantNil:  true,
			wantWarn: "PROJ-123: ambiguous herdr tab label 'PROJ-123' matches w2:tC, w3:tC",
		},
		{
			name:       "wip template",
			id:         &id,
			template:   "wip/{id}",
			tabs:       []herdr.Tab{wipTab},
			agents:     []herdr.Agent{{PaneID: "w2:pW", TabID: "w2:tW", AgentStatus: "idle"}},
			wantTabID:  "w2:tW",
			wantPane:   strPtr("w2:pW"),
			wantStatus: "idle",
		},
		{
			name:     "case-sensitive miss",
			id:       &id,
			template: "{id}",
			tabs:     []herdr.Tab{{TabID: "w2:tC", Label: "proj-123"}},
			wantNil:  true,
			wantWarn: "PROJ-123: identifier matched but no herdr tab labeled 'PROJ-123'",
		},
		{
			name:     "no identifier",
			id:       nil,
			repo:     "acme/widgets",
			number:   9,
			template: "{id}",
			tabs:     []herdr.Tab{tab},
			wantNil:  true,
			wantWarn: "acme/widgets#9: no identifier matched title_id_pattern",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, warn := Match(tc.id, tc.repo, tc.number, tc.template, tc.tabs, tc.agents)
			if warn != tc.wantWarn {
				t.Fatalf("warning = %q, want %q", warn, tc.wantWarn)
			}
			if got == nil {
				if !tc.wantNil {
					t.Fatal("tab is nil, want object")
				}
				return
			}
			if tc.wantNil {
				t.Fatalf("tab = %+v, want nil", got)
			}
			if got.TabID != tc.wantTabID {
				t.Errorf("tab_id = %q, want %q", got.TabID, tc.wantTabID)
			}
			if !sameStringPtr(got.PaneID, tc.wantPane) {
				t.Errorf("pane_id = %s, want %s", formatPtr(got.PaneID), formatPtr(tc.wantPane))
			}
			if got.AgentStatus != tc.wantStatus {
				t.Errorf("agent_status = %q, want %q", got.AgentStatus, tc.wantStatus)
			}
		})
	}
}
