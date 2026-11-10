package tui

import (
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
