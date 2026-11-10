package tui

import (
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textarea"
	tea "github.com/charmbracelet/bubbletea"
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

// syncField loads text into field when they differ. The cursor goes to
// the end, the same place typing would have left it.
func syncField(field *textarea.Model, text string) {
	if field.Value() != text {
		field.SetValue(text)
	}
}
