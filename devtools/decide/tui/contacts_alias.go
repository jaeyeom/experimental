package tui

import (
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// aliasHub stores contact aliases on the hub, as SetHidden does for the
// hidden list. A local backend does not implement it.
type aliasHub interface {
	SetAliases(aliases map[string]string) (decide.DisplaySettings, error)
}

// beginAliasPick asks which bus name the selected contact should join.
func (m *Model) beginAliasPick() {
	c, ok := m.selectedContact()
	if !ok {
		return
	}
	if len(m.aliasChoices()) == 0 {
		m.status = "no bus names"
		return
	}
	m.aliasFrom = c.Name
	m.aliasCursor = 0
	m.aliasPick = true
	m.status = "alias " + c.Name + " to a bus name"
}

func (m *Model) aliasChoices() []string {
	var names []string
	seen := map[string]bool{}
	for _, tg := range m.busNames {
		if tg.Name == "" || seen[tg.Name] {
			continue
		}
		seen[tg.Name] = true
		names = append(names, tg.Name)
	}
	return names
}

// aliasPickKey moves the bus-name list. Enter stores the alias and
// Esc cancels.
func (m *Model) aliasPickKey(r rune) (bool, error) {
	names := m.aliasChoices()
	switch r {
	case '\x1b':
		m.aliasPick = false
		m.status = ""
	case 'j':
		if m.aliasCursor < len(names)-1 {
			m.aliasCursor++
		}
	case 'k':
		if m.aliasCursor > 0 {
			m.aliasCursor--
		}
	case '\r', '\n':
		m.aliasPick = false
		if m.aliasCursor >= 0 && m.aliasCursor < len(names) {
			m.saveAlias(m.aliasFrom, names[m.aliasCursor])
		}
	}
	return false, nil
}

func (m *Model) aliasPickScreen(width int) []screenLine {
	lines := []screenLine{{text: "alias " + m.aliasFrom + " to"}}
	useStyles()
	for i, name := range m.aliasChoices() {
		text := "  " + name
		if i == m.aliasCursor {
			text = plainLine.Reverse(true).Render(text)
		}
		lines = append(lines, screenLine{text: text})
	}
	_, limit := m.layoutBudget()
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

func (m *Model) saveAlias(from, to string) {
	from = strings.TrimSpace(from)
	to = strings.TrimSpace(to)
	if from == "" || to == "" || from == to {
		m.status = "alias needs two names"
		return
	}
	next := cloneAliases(m.aliases)
	if next == nil {
		next = map[string]string{}
	}
	next[from] = to
	if err := m.persistAliases(next); err != nil {
		m.status = err.Error()
		return
	}
	m.status = fmt.Sprintf("aliased %s to %s", from, to)
	m.rebuildContacts()
}

// persistAliases stores aliases on the hub in hub mode, otherwise in
// this machine's config.
func (m *Model) persistAliases(aliases map[string]string) error {
	if hub, ok := m.backend.(aliasHub); ok {
		settings, err := hub.SetAliases(aliases)
		if err != nil {
			return fmt.Errorf("save contact aliases: %w", err)
		}
		eff, err := decide.MergeDisplay(m.localConfig, settings)
		if err != nil {
			return fmt.Errorf("merge contact aliases: %w", err)
		}
		m.aliases = cloneAliases(eff.Config.ContactAliases)
		return nil
	}
	stored, err := decide.SetContactAliases(aliases)
	if err != nil {
		return fmt.Errorf("save contact aliases: %w", err)
	}
	m.aliases = cloneAliases(stored)
	return nil
}

func cloneAliases(in map[string]string) map[string]string {
	if len(in) == 0 {
		return nil
	}
	out := make(map[string]string, len(in))
	for k, v := range in {
		out[k] = v
	}
	return out
}

func sameAliases(a, b map[string]string) bool {
	if len(a) != len(b) {
		return false
	}
	for k, v := range a {
		if b[k] != v {
			return false
		}
	}
	return true
}
