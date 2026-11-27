package decide

import (
	"fmt"
	"slices"
	"sort"
)

// Built-in Status dashboard panel IDs.
const (
	BuiltinPanelAgents    = "agents"
	BuiltinPanelResources = "resources"
	BuiltinPanelBusHealth = "bus-health"
	BuiltinPanelOutbox    = "outbox"
	BuiltinPanelAsks      = "asks"
)

// BuiltinPanelIDs returns the built-in Status panel IDs in default order.
func BuiltinPanelIDs() []string {
	return []string{
		BuiltinPanelAgents,
		BuiltinPanelResources,
		BuiltinPanelBusHealth,
		BuiltinPanelOutbox,
	}
}

// BuiltinPanelTitle returns the human-readable title for a built-in panel ID.
func BuiltinPanelTitle(id string) string {
	switch id {
	case BuiltinPanelAgents:
		return "Agents"
	case BuiltinPanelResources:
		return "Resources"
	case BuiltinPanelBusHealth:
		return "Bus Health"
	case BuiltinPanelOutbox:
		return "Outbox"
	case BuiltinPanelAsks:
		return "Asks"
	default:
		return id
	}
}

// DashboardUI configures dashboard panel visibility and order under ui.dashboard.
type DashboardUI struct {
	Panels []DashboardPanelConfig `json:"panels,omitempty"`
}

// DashboardPanelConfig configures one panel's enabled state and order.
type DashboardPanelConfig struct {
	ID      string `json:"id"`
	Enabled *bool  `json:"enabled"`
	Order   int    `json:"order"`
}

// IsEnabled reports whether the configured panel is enabled (nil defaults to true).
func (p DashboardPanelConfig) IsEnabled() bool {
	return p.Enabled == nil || *p.Enabled
}

// NewDashboardPanelConfig builds a DashboardPanelConfig with an explicit enabled bool.
func NewDashboardPanelConfig(id string, enabled bool, order int) DashboardPanelConfig {
	return DashboardPanelConfig{
		ID:      id,
		Enabled: &enabled,
		Order:   order,
	}
}

// DashboardPanelState is the resolved state of one dashboard panel.
type DashboardPanelState struct {
	ID      string
	Enabled bool
	Order   int
}

func checkDashboardConfig(d DashboardUI) error {
	seen := make(map[string]bool, len(d.Panels))
	for i, p := range d.Panels {
		if err := ValidPanelID(p.ID); err != nil {
			return fmt.Errorf("decide config ui.dashboard.panels[%d]: %w", i, err)
		}
		if seen[p.ID] {
			return fmt.Errorf("decide config ui.dashboard.panels[%d]: duplicate panel id %q", i, p.ID)
		}
		seen[p.ID] = true
	}
	return nil
}

// ResolveDashboardPanels merges configured panel entries with availableIDs.
// Listed panels appear first sorted stably by Order; panels not listed are
// appended after the listed ones, enabled.
func ResolveDashboardPanels(configured []DashboardPanelConfig, availableIDs []string) []DashboardPanelState {
	type indexed struct {
		cfg DashboardPanelConfig
		idx int
	}
	listed := make([]indexed, len(configured))
	seen := make(map[string]bool, len(configured)+len(availableIDs))
	for i, c := range configured {
		listed[i] = indexed{cfg: c, idx: i}
		seen[c.ID] = true
	}
	sort.SliceStable(listed, func(i, j int) bool {
		if listed[i].cfg.Order != listed[j].cfg.Order {
			return listed[i].cfg.Order < listed[j].cfg.Order
		}
		return listed[i].idx < listed[j].idx
	})
	out := make([]DashboardPanelState, 0, len(listed)+len(availableIDs))
	for _, item := range listed {
		out = append(out, DashboardPanelState{
			ID:      item.cfg.ID,
			Enabled: item.cfg.IsEnabled(),
			Order:   len(out),
		})
	}
	for _, id := range availableIDs {
		if seen[id] {
			continue
		}
		seen[id] = true
		out = append(out, DashboardPanelState{
			ID:      id,
			Enabled: true,
			Order:   len(out),
		})
	}
	return out
}

// ToggleDashboardPanel toggles the enabled state of id and returns the updated
// normalized config slice.
func ToggleDashboardPanel(configured []DashboardPanelConfig, availableIDs []string, id string) []DashboardPanelConfig {
	resolved := ResolveDashboardPanels(configured, availableIDs)
	found := false
	for i := range resolved {
		if resolved[i].ID == id {
			resolved[i].Enabled = !resolved[i].Enabled
			found = true
			break
		}
	}
	if !found && id != "" {
		resolved = append(resolved, DashboardPanelState{
			ID:      id,
			Enabled: false,
			Order:   len(resolved),
		})
	}
	return statesToConfigs(resolved)
}

// MoveDashboardPanel moves id among visibleIDs by delta (-1 up, +1 down) while
// preserving all configured panels and returning the normalized config slice.
func MoveDashboardPanel(configured []DashboardPanelConfig, availableIDs, visibleIDs []string, id string, delta int) ([]DashboardPanelConfig, bool) {
	if delta == 0 {
		return nil, false
	}
	resolved := ResolveDashboardPanels(configured, availableIDs)
	visPositions := make([]int, 0, len(visibleIDs))
	visTarget := -1
	for i, st := range resolved {
		if slices.Contains(visibleIDs, st.ID) {
			if st.ID == id {
				visTarget = len(visPositions)
			}
			visPositions = append(visPositions, i)
		}
	}
	if visTarget < 0 {
		return nil, false
	}
	nextVis := visTarget + delta
	if nextVis < 0 || nextVis >= len(visPositions) {
		return nil, false
	}
	fromIdx := visPositions[visTarget]
	toIdx := visPositions[nextVis]
	moving := resolved[fromIdx]
	if fromIdx < toIdx {
		copy(resolved[fromIdx:toIdx], resolved[fromIdx+1:toIdx+1])
		resolved[toIdx] = moving
	} else {
		copy(resolved[toIdx+1:fromIdx+1], resolved[toIdx:fromIdx])
		resolved[toIdx] = moving
	}
	return statesToConfigs(resolved), true
}

func statesToConfigs(states []DashboardPanelState) []DashboardPanelConfig {
	out := make([]DashboardPanelConfig, len(states))
	for i, st := range states {
		out[i] = NewDashboardPanelConfig(st.ID, st.Enabled, i)
	}
	return out
}
