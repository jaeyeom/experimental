package tui

import (
	"context"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// locationMsg is the herdr lookup result for item id's owner.
type locationMsg struct {
	id  string
	loc decide.OwnerLocation
}

// ownerLocation is the cached owner location of it, or its stored pane
// until the lookup returns.
func (m *Model) ownerLocation(it *decide.Item) decide.OwnerLocation {
	if loc, ok := m.locations[it.ID]; ok {
		return loc
	}
	return decide.OwnerPane(it, m.Machine)
}

// locateCmd looks up the open item's owner in herdr off the update
// loop, once each time the item is opened.
func (m *Model) locateCmd() tea.Cmd {
	if !m.locateWant {
		return nil
	}
	m.locateWant = false
	if m.detail == nil {
		return nil
	}
	pending := decide.OwnerPane(m.detail, m.Machine)
	if pending.Pane == "" {
		return nil
	}
	id := m.detail.ID
	return func() tea.Msg {
		locs := decide.LocationResolver{}.Resolve(context.Background(), []decide.OwnerLocation{pending})
		return locationMsg{id: id, loc: locs[0]}
	}
}

func (m *Model) setLocation(msg locationMsg) {
	if m.locations == nil {
		m.locations = map[string]decide.OwnerLocation{}
	}
	m.locations[msg.id] = msg.loc
}

// focusMsg is the status line text after a jump to the owner's tab.
type focusMsg string

// jumpToOwner asks Update to focus the open item's owner tab from its
// cached location. It leaves the inbox and the item as they are.
func (m *Model) jumpToOwner() {
	if m.detail == nil {
		return
	}
	loc, ok := m.locations[m.detail.ID]
	if !ok {
		loc = decide.OwnerPane(m.detail, m.Machine)
		if loc.Pane != "" {
			m.status = "owner location still loading"
			return
		}
	}
	m.focusWant = &loc
}

// focusCmd runs the jump jumpToOwner asked for off the update loop.
func (m *Model) focusCmd() tea.Cmd {
	if m.focusWant == nil {
		return nil
	}
	loc := *m.focusWant
	m.focusWant = nil
	return func() tea.Msg {
		return focusMsg(decide.LocationResolver{}.Focus(context.Background(), loc))
	}
}
