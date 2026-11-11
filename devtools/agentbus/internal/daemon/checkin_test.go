package daemon

import (
	"context"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

const (
	ciKey   = "local:/home/u/src/repo"
	ciAlice = "herdr:local/default/w1:p1"
	ciBob   = "herdr:local/default/w1:p2"
)

func ciT0() time.Time { return time.Date(2026, 11, 10, 9, 0, 0, 0, time.UTC) }

// clock is a lease clock tests move by hand. It is safe for a daemon
// goroutine to read.
type clock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *clock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *clock) Set(ts time.Time) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = ts
}

// checkinWorld is a server with a fake herdr where alice holds the
// checkout with a 40m bid and bob waits.
type checkinWorld struct {
	srv   *server
	ad    *fake.Adapter
	clock *clock
	dir   string
}

func newCheckinWorld(t *testing.T, waiter bool) *checkinWorld {
	t.Helper()
	dir := filepath.Join(t.TempDir(), "state")
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{ciAlice: adapter.StateWorking, ciBob: adapter.StateWorking},
		Result:      map[string]adapter.Result{ciAlice: adapter.ResultAccepted, ciBob: adapter.ResultAccepted},
	}
	c := &clock{now: ciT0()}
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	srv, err := newServer(ctx, dir, Config{Adapters: []adapter.Adapter{ad}, Now: c.Now}, eventlog.Open(dir))
	if err != nil {
		t.Fatal(err)
	}
	if _, err := srv.leases.Claim(ciAlice, lease.Args{Checkout: ciKey, For: "40m"}); err != nil {
		t.Fatal(err)
	}
	if waiter {
		if _, err := srv.leases.Claim(ciBob, lease.Args{Checkout: ciKey, For: "20m"}); err != nil {
			t.Fatal(err)
		}
	}
	return &checkinWorld{srv: srv, ad: ad, clock: c, dir: dir}
}

// at runs one check at deadline+m minutes with alice's pane in state.
func (w *checkinWorld) at(m int, state adapter.State) {
	w.clock.Set(ciT0().Add(40*time.Minute + time.Duration(m)*time.Minute))
	if state == adapter.StateGone {
		delete(w.ad.Reach, ciAlice)
	} else {
		w.ad.Reach[ciAlice] = state
	}
	w.srv.checkLeases()
}

func (w *checkinWorld) checkIns() int {
	n := 0
	for _, text := range w.ad.Texts {
		if strings.Contains(text, lease.CheckInText(ciKey)) {
			n++
		}
	}
	return n
}

func (w *checkinWorld) events(t *testing.T, name string) []eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(w.dir).Read(eventlog.Filter{Name: ciKey, Event: name})
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

func TestCheckLeasesSendsOneCheckInAtTheDeadline(t *testing.T) {
	w := newCheckinWorld(t, true)
	w.at(-1, adapter.StateWorking)
	if n := w.checkIns(); n != 0 {
		t.Fatalf("check-ins before the deadline = %d", n)
	}
	w.at(0, adapter.StateWorking)
	if n := w.checkIns(); n != 1 {
		t.Fatalf("check-ins at the deadline = %d, texts %q", n, w.ad.Texts)
	}
	if n := len(w.events(t, eventlog.EventLeaseCheckIn)); n != 1 {
		t.Fatalf("lease_checkin events = %d", n)
	}
}

func TestCheckLeasesExtendsAWorkingHolderAndRepeatsAfter15Minutes(t *testing.T) {
	w := newCheckinWorld(t, true)
	w.at(0, adapter.StateWorking)
	w.at(2, adapter.StateBlocked)
	w.at(4, adapter.StateWorking)
	w.at(6, adapter.StateWorking)
	if n := w.checkIns(); n != 1 {
		t.Fatalf("check-ins after three intervals = %d", n)
	}
	if n := len(w.events(t, eventlog.EventLeaseAutoExtend)); n != 4 {
		t.Fatalf("auto extends = %d", n)
	}
	for m := 8; m <= 14; m += 2 {
		w.at(m, adapter.StateWorking)
	}
	if n := w.checkIns(); n != 1 {
		t.Fatalf("check-ins within 15 minutes = %d", n)
	}
	w.at(16, adapter.StateWorking)
	if n := w.checkIns(); n != 2 {
		t.Fatalf("check-ins after 16 minutes = %d", n)
	}
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 0 {
		t.Fatalf("takeovers = %d", n)
	}
}

func TestCheckLeasesIdleFiveMinutesWithAWaiterWouldTakeOver(t *testing.T) {
	w := newCheckinWorld(t, true)
	for _, m := range []int{0, 2, 4} {
		w.at(m, adapter.StateIdle)
	}
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 0 {
		t.Fatalf("takeover within 4 minutes: %d", n)
	}
	w.at(5, adapter.StateDone)
	w.at(7, adapter.StateNoAgent)
	evs := w.events(t, eventlog.EventLeaseTakeover)
	if len(evs) != 1 || evs[0].Detail != "would take over" || evs[0].Actor != ciAlice || !strings.Contains(evs[0].Reason, "idle") {
		t.Fatalf("takeover events = %+v", evs)
	}
}

func TestCheckLeasesIdleThenExtendDoesNotTakeOver(t *testing.T) {
	w := newCheckinWorld(t, true)
	for _, m := range []int{0, 2, 4} {
		w.at(m, adapter.StateIdle)
	}
	if _, err := w.srv.leases.Extend(ciAlice, lease.Args{Checkout: ciKey, For: "20m"}); err != nil {
		t.Fatal(err)
	}
	for m := 6; m <= 22; m += 2 {
		w.at(m, adapter.StateIdle)
	}
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 0 {
		t.Fatalf("takeover after extend: %d", n)
	}
}

func TestCheckLeasesUnknownForAnHourNeverTakesOver(t *testing.T) {
	w := newCheckinWorld(t, true)
	for m := 0; m <= 60; m += 2 {
		w.at(m, adapter.StateUnknown)
	}
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 0 {
		t.Fatalf("takeover while unknown: %d", n)
	}
}

func TestCheckLeasesWithNoWaiterChecksInOnce(t *testing.T) {
	w := newCheckinWorld(t, false)
	for m := 0; m <= 60; m += 2 {
		w.at(m, adapter.StateIdle)
	}
	if n := w.checkIns(); n != 1 {
		t.Fatalf("check-ins = %d", n)
	}
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 0 {
		t.Fatalf("takeover with nobody waiting: %d", n)
	}
}

func TestCheckLeasesGoneWithAWaiterWouldTakeOverAtOnce(t *testing.T) {
	w := newCheckinWorld(t, true)
	w.at(-30, adapter.StateGone)
	evs := w.events(t, eventlog.EventLeaseTakeover)
	if len(evs) != 1 || !strings.Contains(evs[0].Reason, "gone") {
		t.Fatalf("takeover events = %+v", evs)
	}
	if n := w.checkIns(); n != 0 {
		t.Fatalf("check-ins to a gone pane = %d", n)
	}
}

func TestCheckLeasesNeverChecksInOnTheUser(t *testing.T) {
	w := newCheckinWorld(t, false)
	if _, err := w.srv.leases.Release(ciAlice, lease.Args{Checkout: ciKey}, func(string) bool { return false }); err != nil {
		t.Fatal(err)
	}
	// pause lands later; the user claims through the table here.
	if _, err := w.srv.leases.Claim(lease.UserHolder, lease.Args{Checkout: ciKey, For: "1m"}); err != nil {
		t.Fatal(err)
	}
	if _, err := w.srv.leases.Claim(ciBob, lease.Args{Checkout: ciKey, For: "20m"}); err != nil {
		t.Fatal(err)
	}
	w.clock.Set(ciT0().Add(24 * time.Hour))
	w.srv.checkLeases()
	if len(w.ad.Texts) != 0 || len(w.events(t, eventlog.EventLeaseCheckIn)) != 0 || len(w.events(t, eventlog.EventLeaseTakeover)) != 0 {
		t.Fatalf("the user was checked in on: texts %q", w.ad.Texts)
	}
}

func TestServeRunsLeaseChecksOnEachTick(t *testing.T) {
	dir := shortDir(t)
	stateDir := filepath.Join(t.TempDir(), "state")
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{ciAlice: adapter.StateWorking},
		Result:      map[string]adapter.Result{ciAlice: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": ciAlice},
	}
	c := &clock{now: ciT0()}
	ticks := make(chan time.Time)
	cfg := Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour, Now: c.Now, LeaseTicks: ticks}
	stop := startWith(t, pathsFor(dir, stateDir), cfg)
	resp := call(t, filepath.Join(dir, "daemon.sock"), Request{Op: "lease-claim", Env: []string{"HERDR_PANE_ID=p1"}, Lease: &lease.Args{Checkout: ciKey, For: "40m"}})
	if !resp.OK {
		stop()
		t.Fatalf("claim = %+v", resp)
	}
	c.Set(ciT0().Add(41 * time.Minute))
	ticks <- c.Now()
	deadline := time.Now().Add(2 * time.Second)
	for {
		lines, err := eventlog.Open(stateDir).Read(eventlog.Filter{Name: ciKey, Event: eventlog.EventLeaseCheckIn})
		if err != nil {
			t.Fatal(err)
		}
		if len(lines) == 1 {
			break
		}
		if time.Now().After(deadline) {
			stop()
			t.Fatal("no check-in after a tick")
		}
		time.Sleep(10 * time.Millisecond)
	}
	stop()
	if len(ad.Texts) != 1 || !strings.Contains(ad.Texts[0], lease.CheckInText(ciKey)) {
		t.Fatalf("deliveries = %q", ad.Texts)
	}
}

func pathsFor(socketDir, stateDir string) paths.Dirs {
	return paths.Dirs{SocketPath: filepath.Join(socketDir, "daemon.sock"), StateDir: stateDir}
}
