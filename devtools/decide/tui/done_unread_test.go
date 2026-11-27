package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestDoneUnreadStaysInTheInboxButIsNotOpenWork(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{Question: "deployed", Owner: "ada", Kind: decide.KindRelay})
	got, err := led.Done(item.ID, "shipped")
	if err != nil || got.State != decide.StateDoneUnread {
		t.Fatalf("agent done = %s %v", got.State, err)
	}
	m := New(dir)
	m.busOn = false
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if m.counts[0] != 1 || len(m.items) != 1 || m.items[0].ID != item.ID {
		t.Fatalf("inbox count %d items %+v", m.counts[0], m.items)
	}
	for i, f := range Folders {
		if f.State == decide.StateDone && m.counts[i] != 0 {
			t.Fatalf("done folder count %d, want 0", m.counts[i])
		}
	}
	seen := false
	for _, c := range m.contacts {
		if c.Name == "ada" {
			seen = true
			if c.Open != 0 {
				t.Fatalf("ada open = %d, want 0", c.Open)
			}
		}
	}
	if !seen {
		t.Fatalf("ada missing from contacts %+v", m.contacts)
	}
}

func TestLocalBackendDoneIsTheUsersDone(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{Question: "deployed", Owner: "ada", Kind: decide.KindRelay})
	got, err := (&LocalBackend{Dir: dir}).Done(item.ID, "closed")
	if err != nil || got.State != decide.StateDone || got.Outcome != "closed" {
		t.Fatalf("backend done = %s %q %v", got.State, got.Outcome, err)
	}
}

func TestDoneUnreadWithOptionsShowsCloseAndRefusesAnswer(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("LC_ALL", "en_US.UTF-8")
	t.Setenv("NO_COLOR", "")
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "post this message?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		Class:    decide.ClassApproval,
		Verbatim: "hello world",
		Options: []decide.Option{
			{Key: "yes", Label: "Yes"},
			{Key: "no", Label: "No"},
			{Key: "edit", Label: "Edit"},
		},
	})
	if _, err := led.Done(item.ID, "Not posted: cancelled."); err != nil {
		t.Fatal(err)
	}
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir},
	}
	m := New(dir)
	m.backend = ob
	m.width, m.height = 100, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r")
	view := m.View()
	for _, want := range []string{"✅", "Closed by ada", "Not posted: cancelled.", "╭─", "╰"} {
		if !strings.Contains(view, want) {
			t.Fatalf("thread view missing closed card %q:\n%s", want, view)
		}
	}
	if strings.Contains(view, "1  Yes") || strings.Contains(view, "2  No") {
		t.Fatalf("options still shown on closed item:\n%s", view)
	}
	press(t, m, "1")
	if m.status != "already closed by ada" {
		t.Fatalf("digit status = %q, want %q", m.status, "already closed by ada")
	}
	if len(ob.entries) != 0 {
		t.Fatalf("outbox changed: %+v", ob.entries)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateDoneUnread || got.Answer != nil {
		t.Fatalf("item = state %s answer %+v", got.State, got.Answer)
	}
}

func TestArchiveDoneUnreadWithOptionsFromListThreadAndOtherFolder(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	opts := []decide.Option{{Key: "yes", Label: "Yes"}, {Key: "no", Label: "No"}}

	t.Run("from list", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question: "approve?", Owner: "ada", Kind: decide.KindRelay, Options: opts,
		})
		if _, err := led.Done(item.ID, "Not posted: cancelled."); err != nil {
			t.Fatal(err)
		}
		m := New(dir)
		m.width, m.height = 100, 30
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		if m.UnreadCount() != 1 || len(m.items) != 1 {
			t.Fatalf("before archive: unread=%d items=%d", m.UnreadCount(), len(m.items))
		}
		press(t, m, "e")
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateDone || got.Unread || got.Outcome != "Not posted: cancelled." {
			t.Fatalf("after list archive: state=%s unread=%v outcome=%q", got.State, got.Unread, got.Outcome)
		}
		if m.UnreadCount() != 0 || len(m.items) != 0 {
			t.Fatalf("after archive: unread=%d items=%d", m.UnreadCount(), len(m.items))
		}
	})

	t.Run("from thread view", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question: "approve?", Owner: "ada", Kind: decide.KindRelay, Options: opts,
		})
		if _, err := led.Done(item.ID, "Not posted: cancelled."); err != nil {
			t.Fatal(err)
		}
		m := New(dir)
		m.width, m.height = 100, 30
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		press(t, m, "\re")
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateDone || got.Unread || got.Outcome != "Not posted: cancelled." {
			t.Fatalf("after thread archive: state=%s unread=%v outcome=%q", got.State, got.Unread, got.Outcome)
		}
		if m.mode != modeList || m.UnreadCount() != 0 || len(m.items) != 0 {
			t.Fatalf("after thread archive: mode=%d unread=%d items=%d", m.mode, m.UnreadCount(), len(m.items))
		}
	})

	t.Run("from snoozed folder", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question: "approve?", Owner: "ada", Kind: decide.KindRelay, Options: opts,
		})
		if _, err := led.Done(item.ID, "Not posted: cancelled."); err != nil {
			t.Fatal(err)
		}
		if _, err := led.Snooze(item.ID, time.Now().Add(time.Hour)); err != nil {
			t.Fatal(err)
		}
		m := New(dir)
		m.width, m.height = 100, 30
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		// Switch to Snoozed folder (g z).
		press(t, m, "gz")
		if !Folders[m.folder].Snoozed || len(m.items) != 1 {
			t.Fatalf("snoozed folder = %+v items = %d", Folders[m.folder], len(m.items))
		}
		press(t, m, "e")
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateDone || got.Unread || got.Snoozed() {
			t.Fatalf("after snoozed archive: state=%s unread=%v snoozed=%v", got.State, got.Unread, got.Snoozed())
		}
	})

	t.Run("open item with options still needs an answer", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question: "approve?", Owner: "ada", Kind: decide.KindRelay, Options: opts,
		})
		m := New(dir)
		m.width, m.height = 100, 30
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		press(t, m, "e")
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateOpen || m.status != "needs an answer" {
			t.Fatalf("open item with options: state=%s status=%q", got.State, m.status)
		}
	})
}
