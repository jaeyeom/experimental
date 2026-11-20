package tui

import (
	"strings"
	"unicode/utf8"

	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/lipgloss"
)

// newComposerField is the multi-line field behind every compose box.
func newComposerField() textarea.Model {
	field := textarea.New()
	field.KeyMap = (&Model{}).composerKeys()
	field.Prompt = ""
	field.ShowLineNumbers = false
	field.Placeholder = ""
	field.CharLimit = 0
	// MaxWidth 0 removes the 500-column clamp before SetWidth. The view
	// wraps the logical line in wrapIndexed.
	field.MaxWidth = 0
	field.SetWidth(4096)
	return field
}

// clickCursor maps a click on a composer's text to a logical line and a
// rune column. row is the visual row after the text is wrapped to width,
// counted from the first row, so a scrolled box adds its offset. col is
// the display column from the text's left edge. A click before the text
// clamps to its start, and one past a row or below the text clamps to
// the end of that row or of the text.
func clickCursor(text string, width, row, col int) (int, int) {
	row = max(row, 0)
	col = max(col, 0)
	parts := strings.Split(text, "\n")
	for i, part := range parts {
		chunks := wrapIndexed(part, width)
		if row >= len(chunks) {
			row -= len(chunks)
			continue
		}
		ch := chunks[row]
		return i, utf8.RuneCountInString(part[:ch.from]) + runesWithin(ch.text, col)
	}
	last := len(parts) - 1
	return last, utf8.RuneCountInString(parts[last])
}

// runesWithin counts the runes of s that end at or before display column
// col. A click on the right half of a wide rune puts the cursor before it.
func runesWithin(s string, col int) int {
	n, w := 0, 0
	for _, r := range s {
		w += max(lipgloss.Width(string(r)), 1)
		if w > col {
			break
		}
		n++
	}
	return n
}

// setFieldCursor puts field's cursor at rune col of logical line.
func setFieldCursor(field *textarea.Model, line, col int) {
	line = min(max(line, 0), field.LineCount()-1)
	for field.Line() > line {
		field.CursorUp()
	}
	for field.Line() < line {
		field.CursorDown()
	}
	field.SetCursor(col)
}

// syncField loads text into field when they differ. The cursor goes to
// the end, the same place typing would have left it.
func syncField(field *textarea.Model, text string) {
	if field.Value() != text {
		field.SetValue(text)
	}
}
