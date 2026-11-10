package tui

import (
	"strings"
	"unicode/utf8"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textarea"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// composerKeys is the one keymap every compose box uses. Bindings left
// out of it, such as word motion, are disabled. Enter is not here: each
// composer decides whether Enter sends or inserts a newline. C-o is
// handled in editField because textarea has no open-line binding.
func composerKeys() textarea.KeyMap {
	bind := func(keys ...string) key.Binding { return key.NewBinding(key.WithKeys(keys...)) }
	return textarea.KeyMap{
		CharacterForward:        bind("right", "ctrl+f"),
		CharacterBackward:       bind("left", "ctrl+b"),
		LineNext:                bind("down", "ctrl+n"),
		LinePrevious:            bind("up", "ctrl+p"),
		LineStart:               bind("home", "ctrl+a"),
		LineEnd:                 bind("end", "ctrl+e"),
		DeleteCharacterForward:  bind("delete", "ctrl+d"),
		DeleteCharacterBackward: bind("backspace", "ctrl+h"),
		DeleteAfterCursor:       bind("ctrl+k"),
		InsertNewline:           bind("ctrl+j"),
	}
}

// composerHint is the footer while a composer is focused. Enter inserts a
// newline in the note composer, so it sends from [Send].
func composerHint(kind composerKind) string {
	send := "Enter send"
	if kind == composerNote {
		send = "Tab [Send]"
	}
	return "C-a/C-e line  C-k kill  C-j newline  C-o open  C-x C-e/C-g editor  " + send + "  Esc cancel"
}

// composerHelp is the composer part of the ? overlay.
var composerHelp = []string{
	"C-f/C-b/C-n/C-p or arrows move  C-a/C-e line",
	"C-d delete  C-k kill  C-j newline  C-o open",
	"C-x C-e or C-g edits the draft",
}

// newComposerField is the multi-line field behind every compose box.
func newComposerField() textarea.Model {
	field := textarea.New()
	field.KeyMap = composerKeys()
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

// isEditKey reports whether k is a composer binding that is not a
// printable key, so Update can send it to the focused field.
func isEditKey(k tea.KeyMsg) bool {
	if k.Type == tea.KeyCtrlO {
		return true
	}
	km := composerKeys()
	return key.Matches(k,
		km.CharacterForward, km.CharacterBackward, km.LineNext, km.LinePrevious,
		km.LineStart, km.LineEnd, km.DeleteCharacterForward,
		km.DeleteCharacterBackward, km.DeleteAfterCursor, km.InsertNewline)
}

// editField applies one key to a focused field. C-o inserts a newline
// and leaves the cursor before it.
func editField(field *textarea.Model, k tea.KeyMsg) {
	if k.Type == tea.KeyCtrlO {
		field.InsertRune('\n')
		*field, _ = field.Update(tea.KeyMsg{Type: tea.KeyLeft})
		return
	}
	*field, _ = field.Update(k)
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
