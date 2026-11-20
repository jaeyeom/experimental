package tui

import (
	"fmt"
	"strings"
)

const (
	// contactPanelWidth is the detail panel beside the contact list.
	contactPanelWidth = 40
	// contactListMin is the least room the list keeps beside the panel.
	contactListMin = 50
	// contactLabelWidth is the label column of the panel.
	contactLabelWidth = 11
)

// contactPanelPlace says where the detail panel of the contact under the
// cursor goes. A region wide enough puts it beside the list. Otherwise
// it goes below the list when i has shown it, and is hidden by default.
func (m *Model) contactPanelPlace(width int) (side, below bool) {
	if width <= 0 || len(m.visibleContacts()) == 0 {
		return false, false
	}
	if !m.narrow() && width >= contactListMin+1+contactPanelWidth {
		return true, false
	}
	return false, m.contactDetail
}

// contactListLimit is the rows the list may use: the layout budget,
// less the panel and its gap when the panel sits below the list.
func (m *Model) contactListLimit(width int) int {
	_, limit := m.layoutBudget()
	if _, below := m.contactPanelPlace(width); below && limit > 0 {
		limit = max(limit-len(m.contactPanel(width))-1, 3)
	}
	return limit
}

// contactPanel is the selected contact in full: its avatar as the thread
// shows it, its state and liveness with when agentbus saw it, its full
// when text, item counts, last activity, and delivery problems.
func (m *Model) contactPanel(width int) []screenLine {
	c, ok := m.selectedContact()
	if !ok {
		return nil
	}
	useStyles()
	lines := []screenLine{{text: focusMark.Render(c.Name)}}
	if width >= avatarCols {
		for _, row := range m.avatarLines(c.Name) {
			lines = append(lines, screenLine{text: row})
		}
	}
	now, loc := m.now(), m.zone()
	// group wraps whole groups, such as "1 open,", so a count stays with
	// its word. The last column is left as a margin.
	group := func(label string, groups []string) {
		if len(groups) == 0 {
			groups = []string{"-"}
		}
		prefix := fmt.Sprintf("%-*s", contactLabelWidth, label)
		for _, text := range wrapWords(groups, max(width-1, contactLabelWidth+1), prefix) {
			lines = append(lines, screenLine{text: text})
		}
	}
	field := func(label, value string) { group(label, strings.Fields(value)) }
	state := c.stateLabel()
	if state == "" {
		state = "not known yet"
	}
	field("state", state)
	live := c.liveLabel()
	if c.LiveAge != nil && !m.busAt.IsZero() && live != "" {
		live += ", seen " + stamp(m.busAt.Add(-*c.LiveAge), now, loc)
	}
	field("liveness", live)
	field("when", c.When)
	group("items", []string{
		fmt.Sprintf("%d open,", c.Open), fmt.Sprintf("%d answered,", c.Answered),
		fmt.Sprintf("%d done,", c.Done), fmt.Sprintf("%d withdrawn", c.Withdrawn),
	})
	field("activity", stamp(c.Last, now, loc))
	group("deliveries", []string{fmt.Sprintf("%d waiting,", c.Waiting), fmt.Sprintf("%d failed", c.Failed)})
	if len(c.Aka) > 0 {
		field("aka", strings.Join(c.Aka, ", "))
	}
	return lines
}
