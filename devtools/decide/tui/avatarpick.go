package tui

import (
	"fmt"
	"slices"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
)

const (
	// avatarDefaultEntry removes the owner's exact rule.
	avatarDefaultEntry = "default"
	// avatarNoneEntry hides the owner's avatar.
	avatarNoneEntry = "none"
	// avatarCellGap separates picker cells in a row.
	avatarCellGap = " "
	// avatarCellRows is a figure and its label.
	avatarCellRows = avatarRows + 1
)

const avatarPickHint = "hjkl/arrows move  enter saves  esc cancels"

// SaveAvatar sets name's avatar in this machine's config to a built-in
// id, "default" to drop name's exact rule, or "none" to hide it. It
// returns the config's avatar rules after the write. The inbox picker
// and `decide avatars set` both save through it.
func SaveAvatar(name, choice string) ([]decide.AvatarRule, error) {
	var rule decide.AvatarRule
	switch choice {
	case avatarDefaultEntry:
	case avatarNoneEntry:
		rule.Hide = true
	default:
		if !slices.Contains(avatar.BuiltinIDs(), choice) {
			return nil, fmt.Errorf("unknown built-in avatar %q", choice)
		}
		rule.Builtin = choice
	}
	rules, err := decide.SetAvatarRule(name, rule)
	if err != nil {
		return nil, fmt.Errorf("save avatar: %w", err)
	}
	return rules, nil
}

// avatarPickEntries is default and none, then every built-in id.
func avatarPickEntries() []string {
	return append([]string{avatarDefaultEntry, avatarNoneEntry}, avatar.BuiltinIDs()...)
}

// beginAvatarPick opens the picker for the open item's owner, with the
// cursor on the current entry.
func (m *Model) beginAvatarPick() {
	if m.detail == nil || m.detail.Owner == "" {
		m.status = "no owner"
		return
	}
	m.avatarOwner = m.detail.Owner
	m.avatarPick = true
	m.avatarTop = 0
	m.avatarCursor = max(slices.Index(avatarPickEntries(), m.avatarCurrent()), 0)
}

// avatarCurrent is the entry the owner shows now: none for an exact
// hide rule, the built-in shown, or default.
func (m *Model) avatarCurrent() string {
	name := m.avatarOwner
	for _, r := range m.Avatars {
		if r.Match == name && r.Hide {
			return avatarNoneEntry
		}
	}
	rule, ok := decide.MatchAvatar(m.Avatars, name)
	switch {
	case ok && rule.Builtin != "" && !rule.Hide:
		return rule.Builtin
	case !ok && m.AvatarDefault:
		return avatar.PickBuiltin(name)
	}
	return avatarDefaultEntry
}

// pickerOpen reports whether the link or avatar picker is open.
func (m *Model) pickerOpen() bool {
	return m.linkPick || m.avatarPick
}

// popupKey sends a key to the open link or avatar picker.
func (m *Model) popupKey(r rune) (bool, error) {
	if m.avatarPick {
		return m.avatarPickKey(r)
	}
	return m.linkPickKey(r)
}

// avatarPickKey drives the picker: h/l and j/k move, Enter saves, and
// Esc cancels.
func (m *Model) avatarPickKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.avatarPick = false
		m.status = ""
	case 'h':
		m.moveAvatarPick(-1)
	case 'l':
		m.moveAvatarPick(1)
	case 'j':
		m.moveAvatarPick(m.avatarPerRow())
	case 'k':
		m.moveAvatarPick(-m.avatarPerRow())
	case '\r':
		m.avatarPick = false
		m.saveAvatar(m.avatarOwner, avatarPickEntries()[m.avatarCursor])
	}
	return false, nil
}

func (m *Model) moveAvatarPick(delta int) {
	n := len(avatarPickEntries())
	m.avatarCursor = max(min(m.avatarCursor+delta, n-1), 0)
}

// saveAvatar saves choice for name and shows it at once. An error leaves
// the avatars as they were.
func (m *Model) saveAvatar(name, choice string) {
	rules, err := SaveAvatar(name, choice)
	if err != nil {
		m.status = fmt.Sprintf("avatar for %s: %v", name, err)
		return
	}
	m.Avatars = rules
	if rule, ok := decide.MatchAvatar(rules, name); ok {
		delete(m.avatarFailed, rule.File)
		delete(m.avatarFailed, "builtin:"+rule.Builtin)
	}
	m.status = fmt.Sprintf("avatar for %s: %s (saved to config)", name, choice)
}

// avatarCellWidth is a two-column marker and the longest entry.
func avatarCellWidth() int {
	w := avatarCols
	for _, e := range avatarPickEntries() {
		w = max(w, 2+len(e))
	}
	return w
}

// avatarPickInner is the width inside the picker box.
func (m *Model) avatarPickInner() int {
	width := m.contentWidth()
	if width <= 0 {
		width = 80
	}
	return width - 2
}

// avatarPerRow is how many cells fit in one picker row.
func (m *Model) avatarPerRow() int {
	stride := avatarCellWidth() + len(avatarCellGap)
	return max(1, (m.avatarPickInner()+len(avatarCellGap))/stride)
}

// avatarVisibleRows is how many cell rows fit: the height less the
// frame, the status line, the box, and its title. Zero height fits all.
func (m *Model) avatarVisibleRows(total int) int {
	if m.height <= 0 {
		return total
	}
	used := 1 + 2 + 1
	if m.framed() {
		used += 2
	}
	return max(1, (m.height-used)/avatarCellRows)
}

// avatarPickLines is the picker: a title, then a grid of figures with
// each entry under its figure. ">" marks the cursor and "*" the current
// entry. Rows scroll to keep the cursor in view.
func (m *Model) avatarPickLines() []screenLine {
	entries := avatarPickEntries()
	perRow := m.avatarPerRow()
	total := (len(entries) + perRow - 1) / perRow
	visible := m.avatarVisibleRows(total)
	row := m.avatarCursor / perRow
	m.avatarTop = min(m.avatarTop, row)
	m.avatarTop = max(m.avatarTop, row-visible+1)
	m.avatarTop = max(min(m.avatarTop, total-visible), 0)

	current := m.avatarCurrent()
	title := "avatar for " + m.avatarOwner
	if total > visible {
		title += fmt.Sprintf("  (rows %d-%d of %d)", m.avatarTop+1, min(m.avatarTop+visible, total), total)
	}
	lines := []screenLine{{text: plainText(title)}}
	cellW := avatarCellWidth()
	for r := m.avatarTop; r < min(m.avatarTop+visible, total); r++ {
		cells := entries[r*perRow : min((r+1)*perRow, len(entries))]
		rows := make([]string, avatarCellRows)
		for c, e := range cells {
			i := r*perRow + c
			fig := m.pickFigure(e)
			for k := range avatarCellRows {
				var cell string
				switch {
				case k < avatarRows && k < len(fig):
					cell = fig[k]
				case k == avatarRows:
					cell = pickMarker(i == m.avatarCursor, e == current) + plainText(e)
				}
				if c > 0 {
					rows[k] += avatarCellGap
				}
				rows[k] += cell + strings.Repeat(" ", max(cellW-lipgloss.Width(cell), 0))
			}
		}
		for _, text := range rows {
			lines = append(lines, screenLine{text: text})
		}
	}
	return boxLines(lines)
}

// pickFigure is a built-in id's figure, or nil for default and none.
func (m *Model) pickFigure(entry string) []string {
	if entry == avatarDefaultEntry || entry == avatarNoneEntry {
		return nil
	}
	lines, err := avatar.Builtin(entry, avatarCols, avatarRows, m.avatarProfile)
	if err != nil {
		return nil
	}
	return lines
}

func pickMarker(cursor, current bool) string {
	b := []byte("  ")
	if cursor {
		b[0] = '>'
	}
	if current {
		b[1] = '*'
	}
	return string(b)
}

// avatarArrow moves the avatar picker with an arrow key.
func (m *Model) avatarArrow(msg tea.KeyMsg) bool {
	switch msg.Type {
	case tea.KeyLeft:
		m.moveAvatarPick(-1)
	case tea.KeyRight:
		m.moveAvatarPick(1)
	case tea.KeyUp:
		m.moveAvatarPick(-m.avatarPerRow())
	case tea.KeyDown:
		m.moveAvatarPick(m.avatarPerRow())
	default:
		return false
	}
	return true
}
