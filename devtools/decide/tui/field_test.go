package tui

import (
	"fmt"
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// composerModel opens one relay item and focuses the composer of kind.
func composerModel(t *testing.T, kind composerKind) (*Model, *decide.Ledger, decide.Item) {
	t.Helper()
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		FreeText: true,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
		Messages: []decide.Message{{Author: "ada", Text: "ship it?"}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "ada", Pane: "pane-1"}}
	m.runSend = func([]string) error { return nil }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	switch kind {
	case composerReply:
		press(t, m, "0")
	case composerAnswer:
		m.focusAnswer()
	case composerOutcome:
		press(t, m, "e")
	case composerNote:
		press(t, m, "c\t")
	}
	if got, ok := m.composerKind(); !ok || got != kind {
		t.Fatalf("focused composer %v %v, want %v", got, ok, kind)
	}
	return m, led, item
}

// setComposer loads "abc\ndef" and puts the cursor at row, col.
func setComposer(t *testing.T, m *Model, row, col int) {
	t.Helper()
	field, text := m.composerField()
	*text = "abc\ndef"
	field.SetValue(*text)
	if row == 0 {
		field.CursorUp()
	}
	field.SetCursor(col)
}

func composerCursor(m *Model) (string, int, int) {
	field, text := m.composerField()
	info := field.LineInfo()
	return *text, field.Line(), info.StartColumn + info.ColumnOffset
}

func sendKey(t *testing.T, m *Model, k tea.KeyMsg) {
	t.Helper()
	next, _ := m.Update(k)
	if next != m {
		t.Fatalf("update returned %T", next)
	}
}

func TestComposerEditingKeys(t *testing.T) {
	kinds := map[composerKind]string{
		composerReply:   "reply",
		composerAnswer:  "answer",
		composerOutcome: "outcome",
		composerNote:    "note",
	}
	ctrl := func(k tea.KeyType) tea.KeyMsg { return tea.KeyMsg{Type: k} }
	tests := []struct {
		name             string
		key              tea.KeyMsg
		row, col         int
		want             string
		wantRow, wantCol int
	}{
		{"right", ctrl(tea.KeyRight), 0, 1, "abc\ndef", 0, 2},
		{"C-f", ctrl(tea.KeyCtrlF), 0, 1, "abc\ndef", 0, 2},
		{"left", ctrl(tea.KeyLeft), 0, 1, "abc\ndef", 0, 0},
		{"C-b", ctrl(tea.KeyCtrlB), 0, 1, "abc\ndef", 0, 0},
		{"down", ctrl(tea.KeyDown), 0, 1, "abc\ndef", 1, 1},
		{"C-n", ctrl(tea.KeyCtrlN), 0, 1, "abc\ndef", 1, 1},
		{"up", ctrl(tea.KeyUp), 1, 1, "abc\ndef", 0, 1},
		{"C-p", ctrl(tea.KeyCtrlP), 1, 1, "abc\ndef", 0, 1},
		{"C-a", ctrl(tea.KeyCtrlA), 0, 1, "abc\ndef", 0, 0},
		{"C-e", ctrl(tea.KeyCtrlE), 0, 1, "abc\ndef", 0, 3},
		{"C-d", ctrl(tea.KeyCtrlD), 0, 1, "ac\ndef", 0, 1},
		{"C-k mid-line", ctrl(tea.KeyCtrlK), 0, 1, "a\ndef", 0, 1},
		{"C-k at end of line", ctrl(tea.KeyCtrlK), 0, 3, "abcdef", 0, 3},
		{"C-j", ctrl(tea.KeyCtrlJ), 0, 1, "a\nbc\ndef", 1, 0},
		{"C-o", ctrl(tea.KeyCtrlO), 0, 1, "a\nbc\ndef", 0, 1},
		{"backspace", ctrl(tea.KeyBackspace), 0, 1, "bc\ndef", 0, 0},
		{"rune", tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'x'}}, 0, 1, "axbc\ndef", 0, 2},
	}
	for kind, kindName := range kinds {
		for _, tt := range tests {
			t.Run(kindName+"/"+tt.name, func(t *testing.T) {
				m, led, item := composerModel(t, kind)
				setComposer(t, m, tt.row, tt.col)
				sendKey(t, m, tt.key)
				text, row, col := composerCursor(m)
				if text != tt.want || row != tt.wantRow || col != tt.wantCol {
					t.Fatalf("got %q at %d,%d, want %q at %d,%d", text, row, col, tt.want, tt.wantRow, tt.wantCol)
				}
				if got, ok := m.composerKind(); !ok || got != kind {
					t.Fatalf("composer left focus: %v %v", got, ok)
				}
				if m.editor != nil || m.editPrefix {
					t.Fatalf("editor %v prefix %v", m.editor != nil, m.editPrefix)
				}
				got, err := led.Get(item.ID)
				if err != nil {
					t.Fatal(err)
				}
				if got.Answer != nil || len(got.Messages) != 1 || got.State != decide.StateOpen {
					t.Fatalf("key submitted the composer: %+v", got)
				}
			})
		}
	}
}

func TestReplyCtrlJInsertsNewlineAndEnterSends(t *testing.T) {
	m, led, item := composerModel(t, composerReply)
	press(t, m, "abc")
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlJ})
	press(t, m, "def")
	if !m.replyFocus || m.reply != "abc\ndef" {
		t.Fatalf("reply %q focus %v", m.reply, m.replyFocus)
	}
	view := m.View()
	if !strings.Contains(view, "reply: abc") || !strings.Contains(visibleLine(view), "def") {
		t.Fatalf("reply lines not shown\n%s", view)
	}
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	last := got.Messages[len(got.Messages)-1]
	if m.replyFocus || m.reply != "" || last.Text != "abc\ndef" {
		t.Fatalf("enter focus %v reply %q last %+v", m.replyFocus, m.reply, last)
	}
}

func TestComposerCtrlXCtrlEStillOpensEditor(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	t.Setenv("VISUAL", "true")
	setComposer(t, m, 0, 1)
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlX})
	_, cmd := m.Update(tea.KeyMsg{Type: tea.KeyCtrlE})
	if cmd == nil || m.editor == nil {
		t.Fatalf("C-x C-e did not start the editor")
	}
	if text, row, col := composerCursor(m); text != "abc\ndef" || row != 0 || col != 1 {
		t.Fatalf("C-x C-e moved the cursor: %q %d,%d", text, row, col)
	}
}

func TestComposerHintsListEditingKeys(t *testing.T) {
	const keys = "C-a/C-e line  C-k kill  C-j newline  C-o open  C-x C-e/C-g editor"
	for kind, send := range map[composerKind]string{
		composerReply:   "Enter send  Esc cancel",
		composerOutcome: "Enter send  Esc cancel",
		composerNote:    "Tab [Send]  Esc cancel",
	} {
		m, _, _ := composerModel(t, kind)
		m.width = 160
		m.height = 30
		m.status = ""
		footer := visibleLine(lineContaining(m.View(), "C-k kill"))
		if !strings.Contains(footer, keys+"  "+send) {
			t.Fatalf("composer %v footer %q", kind, footer)
		}
	}

	m, _, _ := composerModel(t, composerReply)
	m.height = 60
	press(t, m, "\x1b?")
	help := visibleLine(m.View())
	for _, needle := range []string{
		"C-f/C-b/C-n/C-p or arrows move  C-a/C-e line",
		"C-d delete  C-k kill  C-j newline  C-o open",
		"C-x C-e or C-g edits the draft",
	} {
		if !strings.Contains(help, needle) {
			t.Fatalf("help missing %q\n%s", needle, help)
		}
	}
}

func TestClickCursor(t *testing.T) {
	long := "abcdefghijklmnopqrstuvwxy"
	ten := "l0\nl1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9"
	tests := []struct {
		name              string
		text              string
		width, row, col   int
		wantLine, wantCol int
	}{
		// A 4-row box scrolled by 5 shows visual row 5 on its first row.
		{"scrolled", ten, 10, 5 + 1, 1, 6, 1},
		{"wrapped row 0", long, 10, 0, 3, 0, 3},
		{"wrapped row 1", long, 10, 1, 3, 0, 13},
		{"wrapped row 2", long, 10, 2, 3, 0, 23},
		{"scrolled and wrapped", "aa\n" + long + "\nbb", 10, 2 + 1, 3, 1, 23},
		{"word wrap skips the break", "hello world foo", 8, 1, 2, 0, 8},
		{"past end of line", "abc\ndef", 10, 0, 9, 0, 3},
		{"past end of wrapped row", long, 10, 2, 9, 0, 25},
		{"below last line", "abc\ndef", 10, 5, 0, 1, 3},
		{"empty", "", 10, 0, 4, 0, 0},
		{"wide rune first cell", "a漢b", 10, 0, 1, 0, 1},
		{"wide rune second cell", "a漢b", 10, 0, 2, 0, 1},
		{"after wide rune", "a漢b", 10, 0, 3, 0, 2},
		{"left border", "abc\ndef", 10, 1, -1, 1, 0},
		{"above the text", "abc\ndef", 10, -1, 2, 0, 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			line, col := clickCursor(tt.text, tt.width, tt.row, tt.col)
			if line != tt.wantLine || col != tt.wantCol {
				t.Fatalf("clickCursor = %d,%d, want %d,%d", line, col, tt.wantLine, tt.wantCol)
			}
		})
	}
}

// clickOn clicks off columns right of where needle starts on screen.
func clickOn(t *testing.T, m *Model, needle string, off int) {
	t.Helper()
	view := m.View()
	row := screenRow(view, needle)
	if row == 0 {
		t.Fatalf("no %q on screen\n%s", needle, visibleLine(view))
	}
	line := strings.Split(view, "\n")[row-1]
	if err := m.Click(visibleCol(line, needle)+off, row); err != nil {
		t.Fatal(err)
	}
}

func TestClickMovesUnfocusedReplyCursor(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	m.width, m.height = 80, 30
	press(t, m, "abc")
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlJ})
	press(t, m, "def\x1b")
	if m.replyFocus {
		t.Fatal("esc kept the reply focused")
	}
	clickOn(t, m, "def", 1)
	if !m.replyFocus {
		t.Fatal("click did not focus the reply")
	}
	press(t, m, "X")
	if m.reply != "abc\ndXef" {
		t.Fatalf("reply %q, want the X after d", m.reply)
	}
}

func TestClickReplyPrefixClampsToLineStart(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	m.width, m.height = 80, 30
	press(t, m, "abc")
	if err := m.Click(1, screenRow(m.View(), "reply: abc")); err != nil {
		t.Fatal(err)
	}
	press(t, m, "X")
	if m.reply != "Xabc" {
		t.Fatalf("reply %q, want X at the start", m.reply)
	}
}

func TestClickMovesOutcomeCursor(t *testing.T) {
	m, _, _ := composerModel(t, composerOutcome)
	m.width, m.height = 80, 30
	press(t, m, "shipped it")
	clickOn(t, m, "shipped it", 4)
	press(t, m, "X")
	if m.draft != "shipXped it" {
		t.Fatalf("outcome %q", m.draft)
	}
}

func TestClickMovesScrolledNoteCursor(t *testing.T) {
	m, _, _ := composerModel(t, composerNote)
	m.width, m.height = 80, 16
	for i := range 20 {
		if i > 0 {
			sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlJ})
		}
		press(t, m, fmt.Sprintf("n%02d", i))
	}
	if screenRow(m.View(), "n00") != 0 {
		t.Fatalf("note box did not scroll\n%s", visibleLine(m.View()))
	}
	clickOn(t, m, "n17", 1)
	press(t, m, "X")
	if !strings.Contains(m.note, "\nnX17\n") {
		t.Fatalf("note %q, want X inside n17", m.note)
	}

	// The unfocused box shows its top rows.
	press(t, m, "\t")
	if _, ok := m.composerKind(); ok {
		t.Fatal("tab kept the note focused")
	}
	clickOn(t, m, "n01", 2)
	if got, ok := m.composerKind(); !ok || got != composerNote {
		t.Fatalf("click focused %v %v, want the note", got, ok)
	}
	press(t, m, "Y")
	if !strings.Contains(m.note, "\nn0Y1\n") {
		t.Fatalf("note %q, want Y inside n01", m.note)
	}
}
