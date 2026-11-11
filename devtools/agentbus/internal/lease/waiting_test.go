package lease

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func notifyEvents(t *testing.T, dir string) []eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Name: key, Event: eventlog.EventLeaseNotifyWaiting})
	if err != nil {
		t.Fatal(err)
	}
	var out []eventlog.Event
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev)
	}
	return out
}

func TestClaimQueuedBehindAHolderTellsTheHolderOnce(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})

	c.now = t0().Add(time.Minute)
	r := mustClaim(t, tbl, bob, Args{For: "30m", Why: "x"})
	if r.Waiting == nil || r.Waiting.To != alice {
		t.Fatalf("waiting = %+v", r.Waiting)
	}
	want := "[lease] 1 waiting for " + key + ": " + bob + " (x, for 30m). Release when you can: agentbus lease release"
	if r.Waiting.Text != want {
		t.Fatalf("text = %q, want %q", r.Waiting.Text, want)
	}
	evs := notifyEvents(t, dir)
	if len(evs) != 1 || evs[0].Actor != alice || !strings.Contains(evs[0].Detail, bob) || !strings.Contains(evs[0].Detail, "1 waiting") {
		t.Fatalf("events = %+v", evs)
	}

	// Claiming again while queued tells nobody.
	if r := mustClaim(t, tbl, bob, Args{For: "45m", Why: "x"}); r.Waiting != nil {
		t.Fatalf("repeat claim waiting = %+v", r.Waiting)
	}
	if evs := notifyEvents(t, dir); len(evs) != 1 {
		t.Fatalf("events after a repeat claim = %+v", evs)
	}

	r = mustClaim(t, tbl, carol, Args{For: "10m"})
	if r.Waiting == nil || r.Waiting.To != alice || !strings.Contains(r.Waiting.Text, "2 waiting") || !strings.Contains(r.Waiting.Text, carol+" (for 10m)") {
		t.Fatalf("second waiter = %+v", r.Waiting)
	}
}

func TestWaitingTextUsesTheWaiterName(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, bob, Args{For: "40m"})
	r := mustClaim(t, tbl, alice, Args{For: "20m"})
	if r.Waiting == nil || !strings.Contains(r.Waiting.Text, ": ops/alice (for 20m).") {
		t.Fatalf("waiting = %+v", r.Waiting)
	}
}

func TestClaimPastTheHolderDeadlineTellsNobody(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	c.now = t0().Add(40 * time.Minute)
	if r := mustClaim(t, tbl, bob, Args{For: "30m"}); r.Result != ResultQueued || r.Waiting != nil {
		t.Fatalf("claim = %+v", r)
	}
	if evs := notifyEvents(t, dir); len(evs) != 0 {
		t.Fatalf("events = %+v", evs)
	}
}

func TestClaimBehindTheUserTellsNobody(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustPause(t, tbl)
	if r := mustClaim(t, tbl, bob, Args{For: "30m"}); r.Result != ResultQueued || r.Waiting != nil {
		t.Fatalf("claim behind a pause = %+v", r)
	}
	if evs := notifyEvents(t, dir); len(evs) != 0 {
		t.Fatalf("events = %+v", evs)
	}
}

func TestQueuedPauseTellsNoWaitingNotice(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	if r := mustPause(t, tbl); r.Asked != alice || r.Waiting != nil {
		t.Fatalf("pause = %+v", r)
	}
	if evs := notifyEvents(t, dir); len(evs) != 0 {
		t.Fatalf("events = %+v", evs)
	}
}

func TestNewHolderIsNotToldAboutWaitersAlreadyQueued(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "30m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	r, err := tbl.Release(alice, Args{Checkout: key}, func(string) bool { return false })
	if err != nil || r.Granted != bob {
		t.Fatalf("release = %+v, %v", r, err)
	}
	if r.Waiting != nil {
		t.Fatalf("release waiting = %+v", r.Waiting)
	}
	// Carol claiming again does not tell bob either.
	if r := mustClaim(t, tbl, carol, Args{For: "15m"}); r.Waiting != nil {
		t.Fatalf("carol again = %+v", r.Waiting)
	}
	for _, ev := range notifyEvents(t, dir) {
		if ev.Actor == bob {
			t.Fatalf("bob was told: %+v", ev)
		}
	}
}
