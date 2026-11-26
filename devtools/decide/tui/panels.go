package tui

import (
	"fmt"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func (m *Model) loadPanelsOnce() {
	if m.panelsLoaded {
		return
	}
	pb, ok := m.store().(PanelsBackend)
	if !ok {
		return
	}
	panels, err := pb.Panels()
	if err != nil {
		return
	}
	m.panels = panels
	m.panelsLoaded = true
}

// panelLines renders the dashboard panels section for the Status view.
// Unreadable panels render "panel unreadable: <file>" and never break the page.
// Part 2 plugs per-kind component renderers in here.
func (m *Model) panelLines(width int, now time.Time) []screenLine {
	if len(m.panels) == 0 {
		return nil
	}
	var rows [][]string
	var rawRows []decide.PanelEntry
	var bad []string
	for _, p := range m.panels {
		if m.filter != "" && !panelMatchesFilter(&p, m.filter) {
			continue
		}
		if p.Unreadable {
			name := p.File
			if name == "" {
				name = p.ID
			}
			bad = append(bad, fmt.Sprintf("panel unreadable: %s", name))
			continue
		}
		age := ""
		if p.UpdatedAt != nil {
			age = stamp(*p.UpdatedAt, now, m.zone())
		}
		state := ""
		if p.Stale {
			state = "stale"
		}
		rows = append(rows, []string{p.Title, p.Kind, age, state})
		rawRows = append(rawRows, p)
	}
	var lines []screenLine
	if len(rows) > 0 {
		lines = append(lines, styledStatusRows([]string{"panel", "kind", "updated", "state"}, rows, width, func(i int) lipgloss.Style {
			if rawRows[i].Stale {
				return staleLine
			}
			return plainLine
		})...)
	}
	for _, msg := range bad {
		lines = append(lines, screenLine{text: staleLine.Render(msg)})
	}
	return lines
}

func panelMatchesFilter(p *decide.PanelEntry, filter string) bool {
	q := strings.ToLower(filter)
	return strings.Contains(strings.ToLower(p.ID), q) ||
		strings.Contains(strings.ToLower(p.File), q) ||
		strings.Contains(strings.ToLower(p.Title), q) ||
		strings.Contains(strings.ToLower(p.Kind), q)
}
