package tui

import (
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// snoozeModel is an inbox on one open item with a clock the test moves.
func snoozeModel(t *testing.T) (*Model, *decide.Ledger, *time.Time, decide.Item) {
	t.Helper()
	dir := t.TempDir()
	clock := time.Date(2026, 11, 10, 14, 0, 0, 0, time.UTC)
	led := openLedger(t, dir)
	led.SetClock(func() time.Time { return clock })
	item := mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		CreatedAt: clock.Add(-time.Hour),
	})
	m := New(dir)
	m.Now = func() time.Time { return clock }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	return m, led, &clock, item
}

func folderIndex(t *testing.T, name string) int {
	t.Helper()
	for i, f := range Folders {
		if f.Name == name {
			return i
		}
	}
	t.Fatalf("no folder %s", name)
	return -1
}

func TestSnoozeFromTheListHidesUntilTheTime(t *testing.T) {
	m, _, clock, item := snoozeModel(t)
	if Folders[1].Name != "Snoozed" {
		t.Fatalf("folders = %+v", Folders)
	}
	press(t, m, "b")
	if !m.snoozing || !strings.Contains(m.View(), "tomorrow 9:00") || strings.Contains(m.View(), "unsnooze") {
		t.Fatalf("popup\n%s", m.View())
	}
	press(t, m, "\r")
	if m.snoozing || m.status != "snoozed "+item.ID+" until Tue 15:00" {
		t.Fatalf("status %q snoozing %v", m.status, m.snoozing)
	}
	if strings.Contains(m.View(), "ship it?") {
		t.Fatalf("inbox still shows the item\n%s", m.View())
	}
	if got := m.navCount(folderIndex(t, "Snoozed")); got != 1 {
		t.Fatalf("snoozed count = %d", got)
	}
	if got := m.navCount(0); got != 0 {
		t.Fatalf("inbox count = %d", got)
	}
	press(t, m, "gz")
	view := m.View()
	if !strings.Contains(view, "[Snoozed]") || !strings.Contains(view, "ship it?") || !strings.Contains(view, "until Tue 15:00") {
		t.Fatalf("snoozed folder\n%s", view)
	}

	*clock = clock.Add(time.Hour)
	sendKeyMsg(t, m, reloadMsg{})
	if strings.Contains(m.View(), "ship it?") {
		t.Fatalf("woken item still in Snoozed\n%s", m.View())
	}
	press(t, m, "gi")
	if len(m.items) != 1 || m.items[0].ID != item.ID || !m.items[0].Unread {
		t.Fatalf("inbox after wake = %+v", m.items)
	}
}

func TestAgentReplyWakesASnoozedItemInTheTUI(t *testing.T) {
	m, led, clock, item := snoozeModel(t)
	press(t, m, "b\r")
	*clock = clock.Add(10 * time.Minute)
	if _, err := led.Reply(item.ID, "", "news", nil, false); err != nil {
		t.Fatal(err)
	}
	sendKeyMsg(t, m, reloadMsg{})
	if len(m.items) != 1 || m.items[0].ID != item.ID || m.items[0].SnoozedUntil != nil || m.items[0].SnoozedAt != nil {
		t.Fatalf("inbox after reply = %+v", m.items)
	}
}

func TestUnsnoozeFromTheSnoozedFolder(t *testing.T) {
	m, _, _, item := snoozeModel(t)
	press(t, m, "b\r")
	press(t, m, "gz")
	press(t, m, "b")
	view := m.View()
	if !strings.Contains(view, "unsnooze") || m.snoozeCursor != 0 {
		t.Fatalf("popup\n%s", view)
	}
	press(t, m, "\r")
	if len(m.items) != 0 {
		t.Fatalf("snoozed folder after unsnooze = %+v", m.items)
	}
	press(t, m, "gi")
	if len(m.items) != 1 || m.items[0].ID != item.ID {
		t.Fatalf("inbox after unsnooze = %+v", m.items)
	}
}

func TestSnoozeFromTheItemViewReturnsToTheList(t *testing.T) {
	m, _, _, item := snoozeModel(t)
	press(t, m, "o")
	if m.mode != modeDetail {
		t.Fatal("not opened")
	}
	press(t, m, "bjj\r")
	if m.mode != modeList || m.status != "snoozed "+item.ID+" until Wed 09:00" {
		t.Fatalf("mode %d status %q", m.mode, m.status)
	}
}

func TestSnoozeEscChangesNothing(t *testing.T) {
	m, led, _, item := snoozeModel(t)
	before := m.status
	press(t, m, "bj")
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyDown})
	press(t, m, "\x1b")
	if m.snoozing || m.status != before {
		t.Fatalf("snoozing %v status %q", m.snoozing, m.status)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Snoozed() || len(m.items) != 1 {
		t.Fatalf("esc changed the item %+v", got)
	}
}

func TestSnoozeCustomEntry(t *testing.T) {
	m, led, _, item := snoozeModel(t)
	press(t, m, "b")
	for range 4 {
		sendKey(t, m, tea.KeyMsg{Type: tea.KeyDown})
	}
	if m.snoozeCursor != 4 {
		t.Fatalf("cursor %d", m.snoozeCursor)
	}
	press(t, m, "soon\r")
	if !m.snoozing || !strings.Contains(m.View(), "is not a duration") {
		t.Fatalf("no inline error\n%s", m.View())
	}
	press(t, m, "\x7f\x7f\x7f\x7f2h30m\r")
	if m.snoozing || m.status != "snoozed "+item.ID+" until Tue 16:30" {
		t.Fatalf("snoozing %v status %q", m.snoozing, m.status)
	}
	got, _ := led.Get(item.ID)
	if got.SnoozedUntil == nil {
		t.Fatalf("not snoozed %+v", got)
	}
}

func TestSnoozeHintsAndHelp(t *testing.T) {
	m, _, _, _ := snoozeModel(t)
	m.width, m.height = 120, 24
	if !strings.Contains(m.View(), "b snooze") {
		t.Fatalf("list hint\n%s", m.View())
	}
	m.help = true
	if !strings.Contains(m.View(), "b snooze") || !strings.Contains(m.View(), "z snoozed") {
		t.Fatalf("help\n%s", m.View())
	}
}
