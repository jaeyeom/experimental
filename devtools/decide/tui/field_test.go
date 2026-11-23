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
		press(t, m, "d")
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

func TestReplyEnterInsertsNewline(t *testing.T) {
	m, led, item := composerModel(t, composerReply)
	press(t, m, "abc")
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
	press(t, m, "def")
	if !m.replyFocus || m.reply != "abc\ndef" {
		t.Fatalf("reply %q focus %v", m.reply, m.replyFocus)
	}
	view := m.View()
	if !strings.Contains(view, "reply:") || !strings.Contains(visibleLine(view), "abc") || !strings.Contains(visibleLine(view), "def") {
		t.Fatalf("reply lines not shown\n%s", view)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Messages) != 1 {
		t.Fatalf("enter sent the reply: %+v", got.Messages)
	}
}

func TestReplyTabReachesSend(t *testing.T) {
	m, led, item := composerModel(t, composerReply)
	m.width, m.height = 80, 30
	press(t, m, "abc\t")
	if m.replyFocus || !m.replySend {
		t.Fatalf("tab focus %v send %v", m.replyFocus, m.replySend)
	}
	if !strings.Contains(visibleLine(m.View()), "> [Send (^⏎)]") {
		t.Fatalf("[Send (^⏎)] not focused\n%s", visibleLine(m.View()))
	}
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyShiftTab})
	if !m.replyFocus || m.replySend {
		t.Fatalf("shift+tab focus %v send %v", m.replyFocus, m.replySend)
	}
	press(t, m, "d")
	clickOn(t, m, "[Send (^⏎)]", 1)
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if last := got.Messages[len(got.Messages)-1]; last.Text != "abcd" || m.reply != "" {
		t.Fatalf("click [Send]: last %q reply %q", last.Text, m.reply)
	}
	if m.replyField.Value() != "" || m.replyScroll != 0 {
		t.Fatalf("sent reply left field %q at scroll %d", m.replyField.Value(), m.replyScroll)
	}
}

func TestReplySendResetsFieldAndScroll(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	m.reply = "first\nsecond"
	m.replyField.SetValue(m.reply)
	m.replyScroll = 1
	if err := m.submitReply(); err != nil {
		t.Fatal(err)
	}
	if m.reply != "" || m.replyField.Value() != "" || m.replyScroll != 0 {
		t.Fatalf("sent reply left draft %q, field %q, scroll %d", m.reply, m.replyField.Value(), m.replyScroll)
	}
}

// replyRows counts the screen rows of the reply box.
func replyRows(m *Model) int {
	m.View()
	n := 0
	for _, row := range m.rows {
		for _, h := range row {
			if h.kind == hitReply {
				n++
				break
			}
		}
	}
	return n
}

func TestReplyBoxSizesToTerminalAndScrolls(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	m.width, m.height = 80, 30
	limit := max(m.height/2, 1)
	if _, detailN := m.layoutBudget(); detailN > 0 {
		limit = min(limit, max(detailN-4, 1))
	}
	if got := replyRows(m); got != 1 {
		t.Fatalf("empty reply box has %d rows, want 1", got)
	}
	for i := range 20 {
		if i > 0 {
			sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
		}
		press(t, m, fmt.Sprintf("r%02d", i))
		want := min(i+1, limit)
		if got := replyRows(m); got != want {
			t.Fatalf("after %d lines the box has %d rows, want %d", i+1, got, want)
		}
	}
	view := m.View()
	if screenRow(view, "r19") == 0 || screenRow(view, "r00") != 0 {
		t.Fatalf("box did not scroll to the cursor\n%s", visibleLine(view))
	}
	clickOn(t, m, "r15", 1)
	press(t, m, "X")
	if !strings.Contains(m.reply, "\nrX15\n") {
		t.Fatalf("reply %q, want X inside r15", m.reply)
	}
}

func TestReplyBoxGrowsWrapsAndShrinks(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	m.width, m.height = 12, 40
	if got := replyRows(m); got != 1 {
		t.Fatalf("empty reply box has %d rows, want 1", got)
	}
	press(t, m, "abc")
	if got := replyRows(m); got != 1 {
		t.Fatalf("one line has %d rows, want 1", got)
	}
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
	press(t, m, "def")
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
	press(t, m, "ghi")
	if got := replyRows(m); got != 3 {
		t.Fatalf("three lines have %d rows, want 3", got)
	}
	press(t, m, "12345678901234567890")
	if got := replyRows(m); got <= 3 {
		t.Fatalf("wrapped line did not grow the box: %d rows", got)
	}
	for range 3 {
		sendKey(t, m, tea.KeyMsg{Type: tea.KeyBackspace})
	}
	if got := replyRows(m); got <= 1 {
		t.Fatalf("deleting text should shrink wrapped rows, got %d", got)
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
		composerReply:   "Ctrl+Enter / Alt+Enter send  Tab [Send]  Esc cancel",
		composerOutcome: "Enter send  Esc cancel",
		composerNote:    "Ctrl+Enter / Alt+Enter send  Tab [Send]  Esc cancel",
	} {
		m, _, _ := composerModel(t, kind)
		m.width = 160
		m.height = 30
		m.status = ""
		footer := visibleLine(lineContaining(m.View(), "C-k kill"))
		if !strings.Contains(footer, keys+"  Alt+q fill  "+send) {
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
		"Alt+q fill",
		"enter in the reply or note text inserts a newline, tab reaches [Send]",
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
	if err := m.Click(1, screenRow(m.View(), "abc")); err != nil {
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

func TestDetailQQuits(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	press(t, m, "\x1b")
	if m.mode != modeDetail {
		t.Fatalf("mode %v, want detail", m.mode)
	}
	_, cmd := m.Update(runeKey('q'))
	if cmd == nil {
		t.Fatal("q in the item view returned no command")
	}
	if _, ok := cmd().(tea.QuitMsg); !ok {
		t.Fatal("q in the item view did not quit")
	}
}

func TestDetailQTypesInFocusedBox(t *testing.T) {
	for _, kind := range []composerKind{composerReply, composerAnswer} {
		t.Run(fmt.Sprint(kind), func(t *testing.T) {
			m, _, _ := composerModel(t, kind)
			quit, err := m.Key('q')
			if err != nil || quit {
				t.Fatalf("q in a focused box quit=%v err=%v", quit, err)
			}
			if _, text := m.composerField(); *text != "q" {
				t.Fatalf("draft %q, want q", *text)
			}
		})
	}
}

func TestDetailEscAndUReturnToList(t *testing.T) {
	for _, key := range []string{"\x1b", "u"} {
		m, _, _ := composerModel(t, composerReply)
		press(t, m, "\x1b"+key)
		if m.mode != modeList {
			t.Fatalf("%q left mode %v, want list", key, m.mode)
		}
	}
}

func TestDetailHintNamesQuit(t *testing.T) {
	m, _, _ := composerModel(t, composerReply)
	press(t, m, "\x1b")
	if hint := m.statusLine().text; !strings.Contains(hint, "q quit") {
		t.Fatalf("item hint %q lacks q quit", hint)
	}
	m.help = true
	if !strings.Contains(m.View(), "q quit") {
		t.Fatalf("item help lacks q quit\n%s", m.View())
	}
}

func TestBoxedEditorScrollsOnTypePasteArrowAndPage(t *testing.T) {
	for _, kind := range []composerKind{composerReply, composerNote} {
		t.Run(fmt.Sprint(kind), func(t *testing.T) {
			m, _, _ := composerModel(t, kind)
			m.width, m.height = 80, 18

			// Paste 20 lines in one KeyRunes message; the cursor stays visible at the bottom.
			var lines []string
			for i := range 20 {
				lines = append(lines, fmt.Sprintf("line-%02d", i))
			}
			sendKey(t, m, tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(strings.Join(lines, "\n")), Paste: true})

			view := m.View()
			if !strings.Contains(view, cursorOn) {
				t.Fatalf("cursor not visible after paste\n%s", visibleLine(view))
			}
			if screenRow(view, "line-19") == 0 || screenRow(view, "line-00") != 0 {
				t.Fatalf("did not scroll to bottom after paste\n%s", visibleLine(view))
			}

			// Move back up with PageUp and Up until line-00 is visible.
			for range 5 {
				sendKey(t, m, tea.KeyMsg{Type: tea.KeyPgUp})
			}
			for range 5 {
				sendKey(t, m, tea.KeyMsg{Type: tea.KeyUp})
			}
			view = m.View()
			if !strings.Contains(view, cursorOn) {
				t.Fatalf("cursor not visible after moving up\n%s", visibleLine(view))
			}
			if screenRow(view, "line-00") == 0 || screenRow(view, "line-19") != 0 {
				t.Fatalf("did not scroll back up\n%s", visibleLine(view))
			}

			// Move back down with PageDown and Down until line-19 is visible.
			for range 5 {
				sendKey(t, m, tea.KeyMsg{Type: tea.KeyPgDown})
			}
			for range 5 {
				sendKey(t, m, tea.KeyMsg{Type: tea.KeyDown})
			}
			view = m.View()
			if !strings.Contains(view, cursorOn) {
				t.Fatalf("cursor not visible after moving down\n%s", visibleLine(view))
			}
			if screenRow(view, "line-19") == 0 || screenRow(view, "line-00") != 0 {
				t.Fatalf("did not scroll back down\n%s", visibleLine(view))
			}
		})
	}
}

func TestBoxedEditorSmallTerminal(t *testing.T) {
	for _, kind := range []composerKind{composerReply, composerNote} {
		t.Run(fmt.Sprint(kind)+"/cursor_fits", func(t *testing.T) {
			m, _, _ := composerModel(t, kind)
			m.width, m.height = 80, 8
			for i := range 6 {
				if i > 0 {
					sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlJ})
				}
				press(t, m, fmt.Sprintf("s%02d", i))
			}
			view := m.View()
			if !strings.Contains(view, cursorOn) {
				t.Fatalf("cursor not visible at height 8\n%s", visibleLine(view))
			}
		})

		t.Run(fmt.Sprint(kind)+"/cursor_marker_when_too_small", func(t *testing.T) {
			m, _, _ := composerModel(t, kind)
			m.width, m.height = 80, 2
			if kind == composerNote {
				m.height = 5
			}
			press(t, m, "hello")
			view := m.View()
			if !strings.Contains(view, cursorOn) && !strings.Contains(view, "↓ cursor below") {
				t.Fatalf("neither cursor nor offscreen marker visible at height %d\n%s", m.height, visibleLine(view))
			}
		})
	}
}

func longThreadReplyModel(t *testing.T, lines int) (*Model, *decide.Ledger, decide.Item) {
	t.Helper()
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	var body []string
	for i := range lines {
		body = append(body, fmt.Sprintf("thread-line-%02d", i))
	}
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		FreeText: true,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
		Messages: []decide.Message{{Author: "ada", Text: strings.Join(body, "\n")}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "ada", Pane: "pane-1"}}
	m.runSend = func([]string) error { return nil }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	m.width, m.height = 80, 20
	return m, led, item
}

func TestPinnedReplyBoxScrollsThreadIndependently(t *testing.T) {
	m, _, _ := longThreadReplyModel(t, 40)
	view := m.View()
	// With an empty unfocused draft, the box is one hint line at the bottom
	// of the detail pane (above detail bottom border, status line, outer frame).
	wantReplyRow := m.height - 3
	if got := screenRow(view, "reply: ___"); got != wantReplyRow {
		t.Fatalf("reply hint row = %d, want %d\n%s", got, wantReplyRow, visibleLine(view))
	}
	if got := replyRows(m); got != 1 {
		t.Fatalf("empty unfocused reply rows = %d, want 1", got)
	}

	// Clicking in the thread does not focus the reply box.
	clickOn(t, m, "thread-line-00", 1)
	if m.replyFocus {
		t.Fatal("click in thread focused reply box")
	}

	// Scrolling the thread moves only the thread; the hint stays at the bottom.
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyPgDown})
	view = m.View()
	if m.scroll == 0 {
		t.Fatal("pgdn did not scroll the thread")
	}
	if got := screenRow(view, "reply: ___"); got != wantReplyRow {
		t.Fatalf("after pgdn reply hint row = %d, want %d\n%s", got, wantReplyRow, visibleLine(view))
	}
	m.Wheel(true)
	view = m.View()
	if got := screenRow(view, "reply: ___"); got != wantReplyRow {
		t.Fatalf("after wheel reply hint row = %d, want %d\n%s", got, wantReplyRow, visibleLine(view))
	}
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnd})
	view = m.View()
	if !strings.Contains(view, "thread-line-39") {
		t.Fatalf("end did not reach bottom of thread\n%s", visibleLine(view))
	}
	if got := screenRow(view, "reply: ___"); got != wantReplyRow {
		t.Fatalf("after end reply hint row = %d, want %d\n%s", got, wantReplyRow, visibleLine(view))
	}

	// Clicking the hint focuses the reply box without changing thread scroll.
	scrollBefore := m.scroll
	clickOn(t, m, "reply: ___", 1)
	if !m.replyFocus {
		t.Fatal("click on reply hint did not focus reply box")
	}
	m.View()
	if m.scroll != scrollBefore {
		t.Fatalf("focusing reply changed thread scroll %d -> %d", scrollBefore, m.scroll)
	}
}

func TestPinnedReplyBoxShrinksThreadAndResetsAfterSend(t *testing.T) {
	m, led, item := longThreadReplyModel(t, 40)
	_, detailN := m.layoutBudget()
	threadBefore, replyBefore := m.detailLayout(m.contentWidth())
	if threadBefore != detailN-1-len(replyBefore) || len(replyBefore) != 1 {
		t.Fatalf("initial threadN=%d len(reply)=%d detailN=%d", threadBefore, len(replyBefore), detailN)
	}

	// Focus reply and type past the cap; cursor stays visible inside the box
	// and thread height equals pane height minus box height.
	press(t, m, "0")
	for i := range 15 {
		if i > 0 {
			sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
		}
		press(t, m, fmt.Sprintf("r%02d", i))
		threadN, reply := m.detailLayout(m.contentWidth())
		if threadN != detailN-1-len(reply) {
			t.Fatalf("after %d lines: threadN=%d, want %d (detailN=%d len(reply)=%d)", i+1, threadN, detailN-1-len(reply), detailN, len(reply))
		}
	}
	view := m.View()
	if !strings.Contains(view, cursorOn) || screenRow(view, "r14") == 0 || screenRow(view, "r00") != 0 {
		t.Fatalf("cursor not visible inside scrolled reply box\n%s", visibleLine(view))
	}
	grownHeight := len(m.replyLines(m.detailInnerWidth(m.contentWidth())))

	// Thread scrolling does not move the reply box or its cursor.
	m.Wheel(true)
	view = m.View()
	if !strings.Contains(view, cursorOn) || screenRow(view, "r14") == 0 {
		t.Fatalf("thread scroll moved reply cursor\n%s", visibleLine(view))
	}

	// Unfocusing keeps the draft visible at its grown height.
	press(t, m, "\x1b")
	if m.replyFocus {
		t.Fatal("esc kept reply focused")
	}
	if got := len(m.replyLines(m.detailInnerWidth(m.contentWidth()))); got != grownHeight {
		t.Fatalf("unfocused draft height=%d, want %d", got, grownHeight)
	}

	// Shrinking the draft grows the thread area back.
	clickOn(t, m, "r00", 1)
	m.setComposerText(composerReply, "one line")
	m.reply = "one line"
	threadShrunk, replyShrunk := m.detailLayout(m.contentWidth())
	if threadShrunk <= detailN-1-grownHeight || threadShrunk != detailN-1-len(replyShrunk) {
		t.Fatalf("shrunk threadN=%d len(reply)=%d grown=%d detailN=%d", threadShrunk, len(replyShrunk), grownHeight, detailN)
	}

	// Clicking [Send (^⏎)] sends the reply and resets the box to one hint line.
	clickOn(t, m, "[Send (^⏎)]", 1)
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if last := got.Messages[len(got.Messages)-1]; last.Text != "one line" {
		t.Fatalf("sent reply = %q, want %q", last.Text, "one line")
	}
	if m.reply != "" || m.replyFocus || m.replySend {
		t.Fatalf("after send: reply=%q focus=%v send=%v", m.reply, m.replyFocus, m.replySend)
	}
	view = m.View()
	if gotRows := replyRows(m); gotRows != 1 || screenRow(view, "reply: ___") != m.height-3 {
		t.Fatalf("after send box did not reset to 1 line at bottom (rows=%d)\n%s", gotRows, visibleLine(view))
	}
}
