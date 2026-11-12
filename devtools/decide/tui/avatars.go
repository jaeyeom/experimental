package tui

import (
	"cmp"
	"fmt"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
	"github.com/muesli/termenv"
)

const (
	avatarCols = 10
	avatarRows = 6
	// avatarColumn is the avatar and its one-space gutter.
	avatarColumn = avatarCols + 1
	// avatarMinScreen is the narrowest terminal that shows avatars.
	avatarMinScreen = 60
	// avatarMinFields is the least room an avatar leaves for the text
	// beside it.
	avatarMinFields = 30
)

// avatarFits reports whether a region width columns wide has room for an
// avatar column.
func (m *Model) avatarFits(width int) bool {
	return m.width >= avatarMinScreen && width-avatarColumn >= avatarMinFields
}

// hasAvatar reports whether name shows an avatar, without rendering it.
// Under NO_COLOR or a monochrome terminal, avatarMono "hide" hides all.
// A matching hide rule hides name's.
func (m *Model) hasAvatar(name string) bool {
	if name == "" {
		return false
	}
	mono := os.Getenv("NO_COLOR") != "" || m.avatarProfile == termenv.Ascii
	if mono && m.AvatarMono == decide.AvatarMonoHide {
		return false
	}
	if rule, ok := decide.MatchAvatar(m.Avatars, name); ok {
		return !rule.Hide
	}
	return m.AvatarDefault
}

// avatarLines is name's rendered avatar, or nil when it has none. With no
// matching rule, avatarDefault picks a built-in by name. A file or
// built-in id that fails to render is hidden for the session, and the
// status line shows the error once.
func (m *Model) avatarLines(name string) []string {
	if !m.hasAvatar(name) {
		return nil
	}
	rule, ok := decide.MatchAvatar(m.Avatars, name)
	if !ok {
		return avatar.ForName(name, avatarCols, avatarRows, m.avatarProfile)
	}
	key := rule.File
	render := avatar.Render
	if m.avatarRender != nil {
		render = m.avatarRender.Render
	}
	if rule.Builtin != "" {
		key = "builtin:" + rule.Builtin
		render = avatar.Builtin
	}
	if m.avatarFailed[key] {
		return nil
	}
	lines, err := render(cmp.Or(rule.Builtin, rule.File), avatarCols, avatarRows, m.avatarProfile)
	if err != nil {
		if m.avatarFailed == nil {
			m.avatarFailed = map[string]bool{}
		}
		m.avatarFailed[key] = true
		m.status = fmt.Sprintf("avatar for %s: %v", name, err)
		return nil
	}
	return lines
}

// pickerAvatar is the selected recipient's avatar for a picker inner
// columns wide. reserve keeps the avatar column whenever any recipient has
// an avatar, so the picker stays put as the selection moves.
func (m *Model) pickerAvatar(inner int) (av []string, reserve bool) {
	if !m.avatarFits(inner) {
		return nil, false
	}
	for _, tg := range m.candidates() {
		if m.hasAvatar(tg.Name) {
			reserve = true
			break
		}
	}
	if !reserve {
		return nil, false
	}
	if tg, ok := m.selectedTarget(); ok {
		av = m.avatarLines(tg.Name)
	}
	return av, true
}

// besideAvatar puts av left of lines with a one-space gutter. The result
// is at least avatarRows tall. Rows past the avatar keep a blank column.
// Hits move right past the column, and a whole-row hit covers only the
// fieldW columns of text.
func besideAvatar(av []string, lines []screenLine, fieldW int) []screenLine {
	n := max(avatarRows, len(lines))
	blank := strings.Repeat(" ", avatarColumn)
	out := make([]screenLine, n)
	for i := range out {
		var line screenLine
		if i < len(lines) {
			line = lines[i]
		}
		prefix := blank
		if i < len(av) {
			prefix = av[i] + " "
		}
		line.text = prefix + line.text
		if len(line.hits) > 0 {
			hits := make([]hit, len(line.hits))
			for j, h := range line.hits {
				hits[j] = shiftHit(h, avatarColumn+1, fieldW)
			}
			line.hits = hits
		}
		out[i] = line
	}
	return out
}
