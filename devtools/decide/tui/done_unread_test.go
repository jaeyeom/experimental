package tui

import (
	"testing"

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
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{Question: "deployed", Owner: "ada", Kind: decide.KindRelay})
	got, err := (&LocalBackend{Dir: dir}).Done(item.ID, "closed")
	if err != nil || got.State != decide.StateDone || got.Outcome != "closed" {
		t.Fatalf("backend done = %s %q %v", got.State, got.Outcome, err)
	}
}
