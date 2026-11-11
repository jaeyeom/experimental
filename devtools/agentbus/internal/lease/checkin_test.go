package lease

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

var timing = Timing{Interval: 2 * time.Minute, Repeat: 15 * time.Minute, Grace: 5 * time.Minute, Takeover: TakeoverOn}

// observe runs one check of the holder at c.now with pane state p.
func observe(t *testing.T, tbl *Table, p Pane) Decision {
	t.Helper()
	var d Decision
	for _, w := range tbl.Watched() {
		got, err := tbl.Observe(w, p, timing)
		if err != nil {
			t.Fatal(err)
		}
		if w.Checkout == key {
			d = got
		}
	}
	return d
}

func holderDeadline(t *testing.T, tbl *Table) string {
	t.Helper()
	r, err := tbl.Status(Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	return r.Leases[0].Holder.Deadline
}

func TestObserveBeforeTheDeadlineDoesNothing(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	c.now = t0().Add(39 * time.Minute)
	for _, p := range []Pane{PaneBusy, PaneIdle, PaneUnknown} {
		if d := observe(t, tbl, p); d != (Decision{}) {
			t.Fatalf("%s before the deadline = %+v", p, d)
		}
	}
}

func TestObserveSendsOneCheckInAtTheDeadline(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	c.now = t0().Add(40 * time.Minute)
	d := observe(t, tbl, PaneBusy)
	want := "[lease] your bid on " + key + " ended. Run 'agentbus lease extend --for <d>' or 'agentbus lease release'."
	if d.CheckIn != want || d.TakeOver != "" {
		t.Fatalf("decision at the deadline = %+v", d)
	}
	c.now = c.now.Add(2 * time.Minute)
	if d := observe(t, tbl, PaneBusy); d.CheckIn != "" {
		t.Fatalf("second check = %+v", d)
	}
}

func TestObserveExtendsAWorkingHolderAndRepeatsTheCheckInAfter15Minutes(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	checkIns := 0
	for i := 0; i <= 8; i++ {
		c.now = deadline.Add(time.Duration(i) * 2 * time.Minute)
		d := observe(t, tbl, PaneBusy)
		if d.TakeOver != "" {
			t.Fatalf("tick %d took over: %+v", i, d)
		}
		if d.CheckIn != "" {
			checkIns++
		}
		if got, want := holderDeadline(t, tbl), c.now.Add(2*time.Minute).Format(time.RFC3339); got != want {
			t.Fatalf("tick %d deadline = %s, want %s", i, got, want)
		}
		// Ticks 0..7 are within 15 minutes of the first check-in; tick 8
		// is 16 minutes after it.
		if want := 1 + i/8; checkIns != want {
			t.Fatalf("tick %d check-ins = %d, want %d", i, checkIns, want)
		}
	}
	events := leaseEvents(t, dir)
	if n := strings.Count(events, eventlog.EventLeaseAutoExtend+" "+alice); n != 9 {
		t.Fatalf("auto extends = %d:\n%s", n, events)
	}
	if n := strings.Count(events, eventlog.EventLeaseCheckIn+" "+alice); n != 2 {
		t.Fatalf("check-ins = %d:\n%s", n, events)
	}
}

func TestObserveTakesOverAfterFiveIdleMinutesWithAWaiter(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	for _, m := range []int{0, 2, 4} {
		c.now = deadline.Add(time.Duration(m) * time.Minute)
		if d := observe(t, tbl, PaneIdle); d.TakeOver != "" {
			t.Fatalf("idle %dm took over: %+v", m, d)
		}
	}
	c.now = deadline.Add(5 * time.Minute)
	d := observe(t, tbl, PaneIdle)
	if !strings.Contains(d.TakeOver, "idle") {
		t.Fatalf("idle 5m = %+v", d)
	}
	// The takeover is decided once; until the daemon carries it out,
	// each check asks for it again as pending.
	c.now = deadline.Add(7 * time.Minute)
	if d := observe(t, tbl, PaneIdle); !strings.HasPrefix(d.TakeOver, "takeover pending since ") || d.CheckIn != "" {
		t.Fatalf("second takeover = %+v", d)
	}
}

func TestObserveIdleThenExtendDoesNotTakeOver(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	for _, m := range []int{0, 2, 4} {
		c.now = deadline.Add(time.Duration(m) * time.Minute)
		observe(t, tbl, PaneIdle)
	}
	if _, err := tbl.Extend(alice, Args{Checkout: key, For: "1m"}); err != nil {
		t.Fatal(err)
	}
	// The new bid ends at 45m. Then the holder is checked in on again
	// and the silence starts over.
	for _, m := range []int{5, 7, 9} {
		c.now = deadline.Add(time.Duration(m) * time.Minute)
		d := observe(t, tbl, PaneIdle)
		if d.TakeOver != "" {
			t.Fatalf("idle %dm after extend took over: %+v", m, d)
		}
		if m == 5 && d.CheckIn == "" {
			t.Fatal("no check-in after the extended bid ended")
		}
	}
}

func TestObserveUnknownNeverTakesOver(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	for m := 0; m <= 60; m += 2 {
		c.now = deadline.Add(time.Duration(m) * time.Minute)
		if d := observe(t, tbl, PaneUnknown); d.TakeOver != "" {
			t.Fatalf("unknown %dm took over: %+v", m, d)
		}
	}
}

func TestObserveUnknownRestartsTheSilence(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	c.now = deadline
	observe(t, tbl, PaneIdle)
	c.now = deadline.Add(4 * time.Minute)
	observe(t, tbl, PaneUnknown)
	c.now = deadline.Add(6 * time.Minute)
	if d := observe(t, tbl, PaneIdle); d.TakeOver != "" {
		t.Fatalf("idle after unknown took over at once: %+v", d)
	}
}

func TestObserveWithNoWaiterChecksInOnceThenLeavesTheHolderAlone(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	deadline := t0().Add(40 * time.Minute)
	checkIns := 0
	for m := 0; m <= 60; m += 2 {
		c.now = deadline.Add(time.Duration(m) * time.Minute)
		d := observe(t, tbl, PaneIdle)
		if d.TakeOver != "" {
			t.Fatalf("%dm took over with nobody waiting: %+v", m, d)
		}
		if d.CheckIn != "" {
			checkIns++
		}
	}
	if checkIns != 1 {
		t.Fatalf("check-ins = %d", checkIns)
	}
	if len(tbl.Watched()) != 0 {
		t.Fatalf("watched = %+v, want none once checked in with nobody waiting", tbl.Watched())
	}
	// A waiter who arrives later starts the silence timer.
	mustClaim(t, tbl, bob, Args{For: "20m"})
	start := c.now
	observe(t, tbl, PaneIdle)
	c.now = start.Add(5 * time.Minute)
	if d := observe(t, tbl, PaneIdle); d.TakeOver == "" {
		t.Fatalf("idle 5m after a waiter came = %+v", d)
	}
}

func TestObserveGoneWithAWaiterTakesOverAtOnce(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	c.now = t0().Add(time.Minute)
	if d := observe(t, tbl, PaneGone); d != (Decision{}) {
		t.Fatalf("gone with nobody waiting = %+v", d)
	}
	mustClaim(t, tbl, bob, Args{For: "20m"})
	d := observe(t, tbl, PaneGone)
	if !strings.Contains(d.TakeOver, "gone") || d.CheckIn != "" {
		t.Fatalf("gone with a waiter = %+v", d)
	}
}

func TestObserveNeverChecksInOnTheUser(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	// The user's hold has no deadline. pause lands later; set it here.
	mustClaim(t, tbl, alice, Args{For: "40m"})
	tbl.mu.Lock()
	tbl.doc.Leases[key].Holder = &holder{Handle: UserHolder, Since: t0()}
	tbl.mu.Unlock()
	mustClaim(t, tbl, bob, Args{For: "20m"})
	c.now = t0().Add(24 * time.Hour)
	if w := tbl.Watched(); len(w) != 0 {
		t.Fatalf("watched = %+v", w)
	}
	d, err := tbl.Observe(Watch{Checkout: key, Holder: UserHolder}, PaneGone, timing)
	if err != nil || d != (Decision{}) {
		t.Fatalf("observe the user = %+v, %v", d, err)
	}
}

func TestObserveIgnoresAStaleWatch(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	c.now = t0().Add(time.Hour)
	w := tbl.Watched()
	if _, err := tbl.Release(alice, Args{Checkout: key}, noneGone); err != nil {
		t.Fatal(err)
	}
	// bob now holds; the watch named alice.
	d, err := tbl.Observe(w[0], PaneGone, timing)
	if err != nil || d != (Decision{}) {
		t.Fatalf("stale watch = %+v, %v", d, err)
	}
}

func TestObserveTimersSurviveARestart(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	deadline := t0().Add(40 * time.Minute)
	c.now = deadline
	if d := observe(t, tbl, PaneIdle); d.CheckIn == "" {
		t.Fatal("no check-in")
	}
	c.now = deadline.Add(3 * time.Minute)
	observe(t, tbl, PaneIdle)

	tbl = openTable(t, dir, c)
	c.now = deadline.Add(5 * time.Minute)
	d := observe(t, tbl, PaneIdle)
	if d.CheckIn != "" || d.TakeOver == "" {
		t.Fatalf("after restart = %+v, want takeover and no second check-in", d)
	}
}

func TestTimingDefaults(t *testing.T) {
	got := Timing{}.WithDefaults()
	want := Timing{Interval: 2 * time.Minute, Repeat: 15 * time.Minute, Grace: 5 * time.Minute, Takeover: TakeoverLog}
	if got != want {
		t.Fatalf("defaults = %+v", got)
	}
	custom := Timing{Interval: time.Minute, Repeat: time.Hour, Grace: time.Second, Takeover: TakeoverOn}
	if got := custom.WithDefaults(); got != custom {
		t.Fatalf("custom = %+v", got)
	}
	if got := (Timing{Takeover: "maybe"}).WithDefaults().Takeover; got != TakeoverLog {
		t.Fatalf("an unknown mode = %q, want log", got)
	}
}

func leaseEvents(t *testing.T, dir string) string {
	t.Helper()
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Name: key})
	if err != nil {
		t.Fatal(err)
	}
	var out []string
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev.Event+" "+ev.Actor)
	}
	return strings.Join(out, "\n")
}
