package tui

import (
	"fmt"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// openLiveThread opens a thread too long for its viewport. The view starts
// at the top with the first message selected.
func openLiveThread(t *testing.T, height int) (*Model, *decide.Ledger, decide.Item) {
	t.Helper()
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	var msgs []decide.Message
	for i := range 6 {
		msgs = append(msgs, decide.Message{
			Author: "ada",
			Time:   time.Date(2026, 11, 1, i, 0, 0, 0, time.UTC),
			Text:   fmt.Sprintf("message %d\nsecond line\nthird line", i),
		})
	}
	item := mustCreate(t, led, &decide.Item{
		Question:   "message 0",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		State:      decide.StateOpen,
		NotifyPane: "pane-9",
		FreeText:   true,
		Messages:   msgs,
		CreatedAt:  time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.Now = func() time.Time { return time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC) }
	m.runSend = func([]string) error { return nil }
	m.SendTemplate = "herdr agent prompt {pane}"
	m.width = 100
	m.height = height
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	if m.detail == nil || m.detail.ID != item.ID {
		t.Fatalf("open failed: %v", m.detail)
	}
	m.View()
	return m, led, item
}

func reloadTick(t *testing.T, m *Model) {
	t.Helper()
	sendKeyMsg(t, m, reloadMsg{})
}

func sendKeyMsg(t *testing.T, m *Model, msg tea.Msg) {
	t.Helper()
	next, _ := m.Update(msg)
	if next != m {
		t.Fatalf("update returned %T", next)
	}
	if m.runErr != nil {
		t.Fatal(m.runErr)
	}
}

func agentReply(t *testing.T, led *decide.Ledger, id, text string) {
	t.Helper()
	if _, err := led.Reply(id, text, nil, false); err != nil {
		t.Fatal(err)
	}
}

func TestLiveRefreshFollowsAtBottom(t *testing.T) {
	m, led, item := openLiveThread(t, 14)
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyEnd})
	m.View()

	agentReply(t, led, item.ID, "fresh news")
	reloadTick(t, m)
	view := m.View()
	if m.message != 6 || len(m.detail.Messages) != 7 {
		t.Fatalf("message %d of %d", m.message, len(m.detail.Messages))
	}
	if !strings.Contains(view, "fresh news") {
		t.Fatalf("new message not shown\n%s", view)
	}
	if strings.Contains(view, "new message") {
		t.Fatalf("marker shown while following\n%s", view)
	}
}

func TestLiveRefreshFollowsFromLastMessage(t *testing.T) {
	m, led, item := openLiveThread(t, 14)
	press(t, m, "nnnnn")
	press(t, m, "kkk")
	m.View()

	agentReply(t, led, item.ID, "fresh news")
	reloadTick(t, m)
	if m.message != 6 || !strings.Contains(m.View(), "fresh news") {
		t.Fatalf("message %d\n%s", m.message, m.View())
	}
}

func TestLiveRefreshKeepsPlaceWhenScrolledUp(t *testing.T) {
	m, led, item := openLiveThread(t, 14)
	press(t, m, "nj")
	before := m.View()
	scroll, message := m.scroll, m.message

	agentReply(t, led, item.ID, "fresh news")
	reloadTick(t, m)
	view := m.View()
	if m.scroll != scroll || m.message != message {
		t.Fatalf("scroll %d->%d message %d->%d", scroll, m.scroll, message, m.message)
	}
	if !strings.Contains(view, "↓ 1 new message") || strings.Contains(view, "↓ 1 new messages") {
		t.Fatalf("missing one-message marker\n%s", view)
	}
	if beforeBody, viewBody := dropFooter(before), dropFooter(view); beforeBody != viewBody {
		t.Fatalf("thread moved\nbefore\n%s\nafter\n%s", beforeBody, viewBody)
	}

	agentReply(t, led, item.ID, "more news")
	reloadTick(t, m)
	if view := m.View(); !strings.Contains(view, "↓ 2 new messages") || m.scroll != scroll || m.message != message {
		t.Fatalf("scroll %d message %d\n%s", m.scroll, m.message, view)
	}

	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyEnd})
	if view := m.View(); strings.Contains(view, "new message") {
		t.Fatalf("End kept the marker\n%s", view)
	}
}

func TestLiveRefreshMarkerClearsOnG(t *testing.T) {
	m, led, item := openLiveThread(t, 14)
	agentReply(t, led, item.ID, "fresh news")
	reloadTick(t, m)
	if !strings.Contains(m.View(), "↓ 1 new message") {
		t.Fatalf("missing marker\n%s", m.View())
	}
	press(t, m, "G")
	if view := m.View(); strings.Contains(view, "new message") || !strings.Contains(view, "fresh news") {
		t.Fatalf("G did not reach the new message\n%s", view)
	}
}

func TestLiveRefreshMarkerClearsWhenScrolledToNewMessage(t *testing.T) {
	m, led, item := openLiveThread(t, 14)
	agentReply(t, led, item.ID, "fresh news")
	reloadTick(t, m)
	for range 40 {
		press(t, m, "j")
	}
	if view := m.View(); strings.Contains(view, "new message") {
		t.Fatalf("scrolling down kept the marker\n%s", view)
	}
}

// dropFooter drops the footer row and the frame line under it.
func dropFooter(view string) string {
	lines := strings.Split(view, "\n")
	return strings.Join(lines[:len(lines)-2], "\n")
}

func TestLiveRefreshKeepsReplyDraft(t *testing.T) {
	m, led, item := openLiveThread(t, 14)
	press(t, m, "0hello")
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyCtrlB})
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyCtrlB})
	m.View()
	text, line, col := composerCursor(m)

	agentReply(t, led, item.ID, "fresh news")
	reloadTick(t, m)
	m.View()
	if got, gotLine, gotCol := composerCursor(m); got != text || gotLine != line || gotCol != col || !m.replyFocus {
		t.Fatalf("draft %q %d:%d -> %q %d:%d focus %v", text, line, col, got, gotLine, gotCol, m.replyFocus)
	}
	press(t, m, "X")
	if m.reply != "helXlo" {
		t.Fatalf("reply = %q", m.reply)
	}
}

func TestLiveRefreshKeepsComposeDraft(t *testing.T) {
	m, led, item := openLiveThread(t, 14)
	m.Targets = []Target{{Name: "ada", Pane: "pane-9"}}
	press(t, m, "c\thello")
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyCtrlB})
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyCtrlB})
	m.View()
	text, line, col := composerCursor(m)
	focus := m.composeFocus

	agentReply(t, led, item.ID, "fresh news")
	reloadTick(t, m)
	m.View()
	if got, gotLine, gotCol := composerCursor(m); got != text || gotLine != line || gotCol != col || !m.composing || m.composeFocus != focus {
		t.Fatalf("draft %q %d:%d -> %q %d:%d composing %v", text, line, col, got, gotLine, gotCol, m.composing)
	}
	press(t, m, "X")
	if m.noteField.Value() != "helXlo" {
		t.Fatalf("note = %q", m.noteField.Value())
	}
}

func TestLiveRefreshKeepsHighlight(t *testing.T) {
	m, led, item := openLiveThread(t, 40)
	press(t, m, "/second\r")
	press(t, m, "/thi")
	agentReply(t, led, item.ID, "second wind")
	reloadTick(t, m)
	if m.highlight != "thi" || !m.searching || m.search != "thi" {
		t.Fatalf("highlight %q searching %v search %q", m.highlight, m.searching, m.search)
	}
	press(t, m, "\x1b")
	// Enter on /second scrolled to its first match, so read the title line.
	if title := m.detailLines(80)[0].text; !strings.Contains(title, "/second (7)") {
		t.Fatalf("highlight count did not update: %q", title)
	}
}

func TestLiveRefreshShowsStateChange(t *testing.T) {
	m, led, item := openLiveThread(t, 40)
	if _, err := led.Answer(item.ID, "elsewhere", decide.ViaCLI); err != nil {
		t.Fatal(err)
	}
	reloadTick(t, m)
	view := m.View()
	if m.mode != modeDetail || m.detail == nil || m.detail.State != decide.StateAnswered {
		t.Fatalf("mode %d detail %+v", m.mode, m.detail)
	}
	if !strings.Contains(visibleLine(view), "State: answered") || !strings.Contains(view, "item answered elsewhere") {
		t.Fatalf("missing state change\n%s", view)
	}
}

func TestLiveRefreshKeepsDeletedItem(t *testing.T) {
	m, _, item := openLiveThread(t, 40)
	m.Dir = t.TempDir()
	reloadTick(t, m)
	view := m.View()
	if m.mode != modeDetail || m.detail == nil || m.detail.ID != item.ID {
		t.Fatalf("mode %d detail %+v", m.mode, m.detail)
	}
	if !strings.Contains(view, "item no longer in ledger") || !strings.Contains(view, "message 5") {
		t.Fatalf("missing deleted status\n%s", view)
	}
}

func TestLiveRefreshUnchangedLedgerKeepsView(t *testing.T) {
	m, _, _ := openLiveThread(t, 14)
	press(t, m, "nj")
	before := m.View()
	scroll, message, status := m.scroll, m.message, m.status
	reloadTick(t, m)
	if m.View() != before || m.scroll != scroll || m.message != message || m.status != status {
		t.Fatalf("view changed\nbefore\n%s\nafter\n%s", before, m.View())
	}
}
