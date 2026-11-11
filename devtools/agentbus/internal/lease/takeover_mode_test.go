package lease

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

var logTiming = Timing{Interval: 2 * time.Minute, Repeat: 15 * time.Minute, Grace: 5 * time.Minute, Takeover: TakeoverLog}

// observeLog runs one check of every watched holder at c.now with pane
// state p and takeover log-only.
func observeLog(t *testing.T, tbl *Table, p Pane) Decision {
	t.Helper()
	var d Decision
	for _, w := range tbl.Watched() {
		got, err := tbl.Observe(w, p, logTiming)
		if err != nil {
			t.Fatal(err)
		}
		if w.Checkout == key {
			d = got
		}
	}
	return d
}

func wouldTakeOvers(t *testing.T, dir string) []eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Name: key, Event: eventlog.EventLeaseWouldTakeOver})
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

func TestObserveLogOnlyIdleHolderLogsOnceAndKeepsTheLease(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	for _, m := range []int{0, 2, 4, 5, 7, 9, 11} {
		c.now = deadline.Add(time.Duration(m) * time.Minute)
		if d := observeLog(t, tbl, PaneIdle); d.TakeOver != "" {
			t.Fatalf("idle %dm with takeover log-only = %+v", m, d)
		}
	}
	evs := wouldTakeOvers(t, dir)
	if len(evs) != 1 {
		t.Fatalf("would-take-over events = %+v, want one", evs)
	}
	ev := evs[0]
	if ev.Actor != alice || !strings.Contains(ev.Reason, "idle and silent") || !strings.Contains(ev.Detail, bob) {
		t.Fatalf("would-take-over event = %+v", ev)
	}
	s := mustStatus(t, tbl)
	if s.State != StateHeld || s.Holder == nil || s.Holder.Handle != alice || len(s.Queue) != 1 || s.BackupRef != "" {
		t.Fatalf("status = %+v", s)
	}
	if events := leaseEvents(t, dir); strings.Contains(events, eventlog.EventLeaseTakeover+" ") {
		t.Fatalf("a takeover was logged:\n%s", events)
	}
}

func TestObserveLogOnlyGoneHolderLogsOnce(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	for m := range 4 {
		c.now = t0().Add(time.Duration(2*m) * time.Minute)
		if d := observeLog(t, tbl, PaneGone); d.TakeOver != "" {
			t.Fatalf("gone with takeover log-only = %+v", d)
		}
	}
	evs := wouldTakeOvers(t, dir)
	if len(evs) != 1 || evs[0].Reason != "pane gone" {
		t.Fatalf("would-take-over events = %+v", evs)
	}
	if s := mustStatus(t, tbl); s.State != StateHeld || s.Holder.Handle != alice {
		t.Fatalf("status = %+v", s)
	}
}

func TestObserveLogOnlyKeepsCheckInsAndAutoExtend(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	c.now = deadline
	if d := observeLog(t, tbl, PaneIdle); d.CheckIn == "" {
		t.Fatalf("no check-in at the deadline: %+v", d)
	}
	c.now = deadline.Add(5 * time.Minute)
	observeLog(t, tbl, PaneIdle)
	// alice works again after the would-be takeover: extended silently.
	c.now = deadline.Add(7 * time.Minute)
	observeLog(t, tbl, PaneBusy)
	if got, want := holderDeadline(t, tbl), stamp(deadline.Add(9*time.Minute)); got != want {
		t.Fatalf("deadline after busy = %s, want %s", got, want)
	}
}

func TestObserveLogOnlyLogsAgainAfterTheHolderAnswers(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	c.now = t0().Add(time.Minute)
	observeLog(t, tbl, PaneGone)
	if _, err := tbl.Extend(alice, Args{Checkout: key, For: "1m"}); err != nil {
		t.Fatal(err)
	}
	observeLog(t, tbl, PaneGone)
	observeLog(t, tbl, PaneGone)
	if n := len(wouldTakeOvers(t, dir)); n != 2 {
		t.Fatalf("would-take-over events after an extend = %d, want 2", n)
	}
}

func TestObserveLogOnlyDoesNotRetryAPendingTakeover(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	// A run with takeover on decided it, then stopped before carrying it
	// out; the next run has takeover log-only.
	if d := observe(t, tbl, PaneGone); d.TakeOver == "" {
		t.Fatalf("gone holder with takeover on = %+v", d)
	}
	for range 4 {
		if d := observeLog(t, tbl, PaneGone); d.TakeOver != "" {
			t.Fatalf("pending takeover with takeover log-only = %+v", d)
		}
	}
	if n := len(wouldTakeOvers(t, dir)); n != 1 {
		t.Fatalf("would-take-over events = %d, want 1", n)
	}
	if s := mustStatus(t, tbl); s.State != StateHeld || s.Holder.Handle != alice {
		t.Fatalf("status = %+v", s)
	}
}
