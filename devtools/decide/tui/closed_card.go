package tui

import (
	"fmt"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"

	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

type closedCardSpec struct {
	icon     string
	asciiTag string
	verb     string
	owner    string
	when     string
	body     string
}

func (m *Model) useASCIICard() bool {
	return m.useASCIIKeys()
}

func asciiCardBorder() lipgloss.Border {
	return lipgloss.Border{
		Top:         "-",
		Bottom:      "-",
		Left:        "|",
		Right:       "|",
		TopLeft:     "+",
		TopRight:    "+",
		BottomLeft:  "+",
		BottomRight: "+",
	}
}

func buildClosedCardSpec(it *decide.Item, now time.Time, loc *time.Location) (closedCardSpec, bool) {
	if !closedWithoutAnswer(it) {
		return closedCardSpec{}, false
	}
	owner := sanitizeCardText(it.Owner, false)
	if owner == "" {
		owner = "owner"
	}
	if it.State == decide.StateWithdrawn {
		at := it.WithdrawnAt
		if at.IsZero() {
			at = it.DoneAt
		}
		raw := strings.TrimSpace(it.Reason)
		if raw == "" {
			raw = strings.TrimSpace(it.Outcome)
		}
		return closedCardSpec{
			icon:     "↩️",
			asciiTag: "[withdrawn]",
			verb:     "Withdrawn",
			owner:    owner,
			when:     stamp(at, now, loc),
			body:     sanitizeCardText(raw, true),
		}, true
	}
	raw := strings.TrimSpace(it.Outcome)
	spec := closedCardSpec{
		icon:     "✅",
		asciiTag: "[closed]",
		verb:     "Closed",
		owner:    owner,
		when:     stamp(it.DoneAt, now, loc),
		body:     sanitizeCardText(raw, true),
	}
	if raw == "archived" {
		spec.icon = "📦"
		spec.asciiTag = "[archived]"
	}
	return spec, true
}

func (s closedCardSpec) header(ascii bool) string {
	tag := s.icon
	if ascii {
		tag = s.asciiTag
	}
	head := tag + " " + s.verb + " by " + s.owner
	if s.when != "" {
		head += " · " + s.when
	}
	return head
}

// sanitizeCardText neutralizes control characters and escape sequences so
// card text renders literally without altering terminal state or alignment.
func sanitizeCardText(s string, keepNewlines bool) string {
	s = strings.ReplaceAll(s, "\r\n", "\n")
	var b strings.Builder
	b.Grow(len(s))
	for _, r := range s {
		switch {
		case keepNewlines && r == '\n':
			b.WriteByte('\n')
		case r == '\t':
			if keepNewlines {
				b.WriteString("    ")
			} else {
				b.WriteByte(' ')
			}
		case unicode.IsControl(r):
			if r <= 0xff {
				fmt.Fprintf(&b, `\x%02x`, r)
			} else {
				fmt.Fprintf(&b, `\u%04x`, r)
			}
		default:
			b.WriteRune(r)
		}
	}
	return b.String()
}

func (m *Model) closedCardLines(it *decide.Item, width int) ([]screenLine, bool) {
	spec, ok := buildClosedCardSpec(it, m.now(), m.zone())
	if !ok {
		return nil, false
	}
	ascii := m.useASCIICard()
	header := spec.header(ascii)

	if width > 0 && width < 6 {
		var out []screenLine
		for _, l := range wrapCardText(header, width) {
			out = append(out, screenLine{text: plainText(l)})
		}
		for _, l := range wrapCardText(spec.body, width) {
			out = append(out, screenLine{text: plainText(l)})
		}
		return out, true
	}

	b := lipgloss.RoundedBorder()
	if ascii {
		b = asciiCardBorder()
	}
	borderStyle := paletteStyle(loadColors().Muted, paletteMuted)

	var bodyLines []string
	if width <= 0 {
		bodyLines = wrapCardText(spec.body, 0)
		maxBodyW := 0
		for _, l := range bodyLines {
			if w := lipgloss.Width(l); w > maxBodyW {
				maxBodyW = w
			}
		}
		width = max(lipgloss.Width(header)+6, maxBodyW+4)
	} else {
		bodyLines = wrapCardText(spec.body, max(width-4, 1))
	}

	innerW := width - 2
	bodyWrap := innerW - 2
	maxHeaderW := innerW - 3
	ellipsis := "…"
	if ascii {
		ellipsis = "..."
		if maxHeaderW < 3 {
			ellipsis = ""
		}
	}
	shownHeader := truncateCardText(header, maxHeaderW, ellipsis)
	trailing := innerW - lipgloss.Width(shownHeader) - 3

	lines := make([]screenLine, 0, len(bodyLines)+2)
	top := borderStyle.Render(b.TopLeft+b.Top) + " " + plainText(shownHeader) + " " +
		borderStyle.Render(strings.Repeat(b.Top, trailing)+b.TopRight)
	lines = append(lines, screenLine{text: top})

	for _, line := range bodyLines {
		if lipgloss.Width(line) > bodyWrap {
			line = truncateCardText(line, bodyWrap, "")
		}
		pad := max(bodyWrap-lipgloss.Width(line), 0)
		row := borderStyle.Render(b.Left) + " " + plainText(line) + strings.Repeat(" ", pad) + " " + borderStyle.Render(b.Right)
		lines = append(lines, screenLine{text: row})
	}

	bot := borderStyle.Render(b.BottomLeft + strings.Repeat(b.Bottom, innerW) + b.BottomRight)
	lines = append(lines, screenLine{text: bot})
	return lines, true
}

func isGraphemeExtend(r rune) bool {
	return r == '\uFE0F' || r == '\uFE0E' || r == '\u200D' || unicode.In(r, unicode.Mn, unicode.Mc, unicode.Me)
}

func nextClusterEnd(s string, i int) int {
	if i >= len(s) {
		return len(s)
	}
	r, size := utf8.DecodeRuneInString(s[i:])
	if size <= 0 {
		return len(s)
	}
	end := i + size
	prevZWJ := r == '\u200D'
	for end < len(s) {
		nr, nsize := utf8.DecodeRuneInString(s[end:])
		if nsize <= 0 {
			break
		}
		if prevZWJ || isGraphemeExtend(nr) {
			end += nsize
			prevZWJ = nr == '\u200D'
			continue
		}
		break
	}
	return end
}

func wrapCardText(text string, width int) []string {
	if text == "" {
		return nil
	}
	parts := strings.Split(text, "\n")
	var out []string
	for _, part := range parts {
		if part == "" {
			out = append(out, "")
			continue
		}
		if width <= 0 || lipgloss.Width(part) <= width {
			out = append(out, part)
			continue
		}
		out = append(out, wrapCardLine(part, width)...)
	}
	return out
}

func wrapCardLine(s string, width int) []string {
	var out []string
	for i := 0; i < len(s); {
		end := i
		lastBreak := -1
		for end < len(s) {
			cEnd := nextClusterEnd(s, end)
			if lipgloss.Width(s[i:cEnd]) > width && end > i {
				break
			}
			if s[end:cEnd] == " " {
				lastBreak = end
			}
			end = cEnd
		}
		if end < len(s) && lastBreak > i {
			out = append(out, s[i:lastBreak])
			i = lastBreak + 1
			continue
		}
		out = append(out, s[i:end])
		i = end
	}
	return out
}

func truncateCardText(s string, maxWidth int, ellipsis string) string {
	if maxWidth <= 0 {
		return ""
	}
	if lipgloss.Width(s) <= maxWidth {
		return s
	}
	elW := lipgloss.Width(ellipsis)
	target := maxWidth - elW
	if target < 0 {
		ellipsis = ""
		target = maxWidth
	}
	best := 0
	for end := 0; end < len(s); {
		cEnd := nextClusterEnd(s, end)
		if lipgloss.Width(s[:cEnd]) > target {
			break
		}
		best = cEnd
		end = cEnd
	}
	return s[:best] + ellipsis
}
