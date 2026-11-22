package mailbox

import (
	"slices"
	"testing"
	"time"
)

func TestRecentDeliveriesListsOnlyTheWindow(t *testing.T) {
	w := newHoldWorld(t)
	old := w.send(t, "old")
	w.clock.add(90 * time.Second)
	fresh := w.send(t, "fresh")
	w.clock.add(30 * time.Second)
	if got := w.box.RecentDeliveries(holdTo); !slices.Equal(got, []string{fresh.ID}) {
		t.Fatalf("recent = %q, want only %s (not %s)", got, fresh.ID, old.ID)
	}
	w.clock.add(time.Minute)
	if got := w.box.RecentDeliveries(holdTo); len(got) != 0 {
		t.Fatalf("recent after the window = %q", got)
	}
}

func TestRecentDeliveriesWindowIsConfigurable(t *testing.T) {
	w := newHoldWorld(t)
	w.send(t, "x")
	w.clock.add(10 * time.Second)
	zero := time.Duration(0)
	w.box.SetHibernateWarnWithin(&zero)
	if got := w.box.RecentDeliveries(holdTo); len(got) != 0 {
		t.Fatalf("recent with the warning off = %q", got)
	}
	five := 5 * time.Second
	w.box.SetHibernateWarnWithin(&five)
	if got := w.box.RecentDeliveries(holdTo); len(got) != 0 {
		t.Fatalf("recent past a 5s window = %q", got)
	}
}
