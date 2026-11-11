package lease

import (
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func mustPause(t *testing.T, tbl *Table, with ...string) Reply {
	t.Helper()
	r, err := tbl.Pause(Args{Checkout: key, With: with})
	if err != nil {
		t.Fatalf("pause: %v", err)
	}
	return r
}

func allowed(t *testing.T, tbl *Table, caller string) bool {
	t.Helper()
	r, err := tbl.Check(caller, Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	return r.Result == ResultAllowed
}

func TestPauseWhenFreeHoldsAsTheUserWithNoDeadline(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	r := mustPause(t, tbl)
	if r.Result != ResultPaused || r.Asked != "" {
		t.Fatalf("pause = %+v", r)
	}
	s := r.Lease
	if s.State != StatePaused || s.Holder == nil || s.Holder.Handle != UserHolder || s.Holder.Deadline != "" {
		t.Fatalf("status = %+v", s)
	}
	if allowed(t, tbl, alice) {
		t.Fatal("check passed for an agent under the user's pause")
	}
	if r := mustClaim(t, tbl, alice, Args{For: "40m"}); r.Result != ResultQueued {
		t.Fatalf("claim under a pause = %+v", r)
	}
	// Pausing again changes nothing.
	if r := mustPause(t, tbl); r.Result != ResultPaused || r.Lease.State != StatePaused {
		t.Fatalf("second pause = %+v", r)
	}
	if events := leaseEvents(t, dir); !strings.Contains(events, eventlog.EventLeasePause+" "+UserHolder) {
		t.Fatalf("events:\n%s", events)
	}
}

func TestPauseWhileAnAgentHoldsQueuesTheUserFirstAndAsksTheHolder(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "5m", Urgent: true})
	c.now = t0().Add(10 * time.Minute)
	r := mustPause(t, tbl)
	if r.Result != ResultQueued || r.Position != 1 || r.Asked != alice {
		t.Fatalf("pause = %+v", r)
	}
	if got := queueHandles(r.Lease); !slices.Equal(got, []string{UserHolder, bob}) {
		t.Fatalf("queue = %v", got)
	}
	if r.Lease.State != StateHeld || r.Lease.Holder.Handle != alice {
		t.Fatalf("status = %+v", r.Lease)
	}
	// The ask counts as the check-in: the bid ends now, and no second
	// check-in is sent for it.
	if d := r.Lease.Holder.Deadline; d != c.now.Format(time.RFC3339) {
		t.Fatalf("deadline = %s, want now", d)
	}
	if d := observe(t, tbl, PaneBusy); d.CheckIn != "" || d.TakeOver != "" {
		t.Fatalf("busy holder after the ask = %+v", d)
	}
	// A second pause asks nobody again.
	if r := mustPause(t, tbl); r.Asked != "" || r.Position != 1 {
		t.Fatalf("second pause = %+v", r)
	}
	events := leaseEvents(t, dir)
	for _, want := range []string{eventlog.EventLeasePause + " " + UserHolder, eventlog.EventLeaseCheckIn + " " + alice} {
		if !strings.Contains(events, want) {
			t.Errorf("events are missing %q:\n%s", want, events)
		}
	}
}

func TestPauseAskTextNamesTheCheckout(t *testing.T) {
	if got := PauseAskText(key); got != "[lease] the user asked for "+key+"; please finish up and release" {
		t.Fatalf("ask = %q", got)
	}
}

func TestPauseAskThenIdleIsTakenOverForTheUser(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustPause(t, tbl)
	c.now = t0().Add(time.Minute)
	if d := observe(t, tbl, PaneIdle); d.TakeOver != "" {
		t.Fatalf("idle 1m = %+v", d)
	}
	c.now = t0().Add(6 * time.Minute)
	d := observe(t, tbl, PaneIdle)
	if d.TakeOver == "" {
		t.Fatalf("idle and silent past the grace = %+v", d)
	}
	tk, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, d.TakeOver, noneGone)
	if err != nil || tk.To != UserHolder {
		t.Fatalf("takeover = %+v, %v", tk, err)
	}
}

func TestReleaseGrantsAQueuedPauseToTheUser(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, carol, Args{For: "40m"})
	mustPause(t, tbl, bob)
	r, err := tbl.Release(alice, Args{Checkout: key}, noneGone)
	if err != nil {
		t.Fatal(err)
	}
	s := r.Lease
	if r.Granted != UserHolder || s.State != StatePaused || s.Holder.Handle != UserHolder || s.Holder.Deadline != "" {
		t.Fatalf("release = %+v, lease %+v", r, s)
	}
	if !slices.Equal(s.With, []string{bob}) || !allowed(t, tbl, bob) {
		t.Fatalf("with = %v", s.With)
	}
	if got := queueHandles(s); !slices.Equal(got, []string{carol}) {
		t.Fatalf("queue = %v", got)
	}
}

func TestPauseWithSharesTheHoldUntilUnshare(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	r := mustPause(t, tbl, bob)
	if !slices.Equal(r.Lease.With, []string{bob}) {
		t.Fatalf("with = %v", r.Lease.With)
	}
	if !allowed(t, tbl, bob) || allowed(t, tbl, carol) {
		t.Fatal("check under --with bob")
	}
	if _, err := tbl.Release(bob, Args{Checkout: key}, noneGone); err == nil || !strings.Contains(err.Error(), "user") {
		t.Fatalf("release by a --with pane = %v", err)
	}
	if _, err := tbl.BeginRelease(bob, Args{Checkout: key}); err == nil {
		t.Fatal("begin release by a --with pane")
	}
	if _, err := tbl.Extend(bob, Args{Checkout: key, For: "10m"}); err == nil {
		t.Fatal("extend by a --with pane")
	}
	r = mustPause(t, tbl, carol, bob)
	if !slices.Equal(r.Lease.With, []string{bob, carol}) {
		t.Fatalf("with after adding carol = %v", r.Lease.With)
	}
	r, err := tbl.Unshare(Args{Checkout: key, With: []string{bob}})
	if err != nil || r.Result != ResultUnshared || !slices.Equal(r.Lease.With, []string{carol}) {
		t.Fatalf("unshare = %+v, %v", r, err)
	}
	if allowed(t, tbl, bob) || !allowed(t, tbl, carol) {
		t.Fatal("check after unshare bob")
	}
	if _, err := tbl.Unshare(Args{Checkout: key, With: []string{bob}}); err == nil {
		t.Fatal("unshare of a pane not shared with")
	}
	if _, err := tbl.Unshare(Args{Checkout: key}); err == nil {
		t.Fatal("unshare of nobody")
	}
	events := leaseEvents(t, dir)
	for _, want := range []string{eventlog.EventLeaseWith + " " + UserHolder, eventlog.EventLeaseUnshare + " " + UserHolder} {
		if !strings.Contains(events, want) {
			t.Errorf("events are missing %q:\n%s", want, events)
		}
	}
}

func TestWithAppliesAfterTheUserAdoptsATakenOverPause(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustPause(t, tbl, bob)
	if _, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "idle", noneGone); err != nil {
		t.Fatal(err)
	}
	if allowed(t, tbl, bob) {
		t.Fatal("check passed for a --with pane before adopt")
	}
	if _, err := tbl.BeginAdopt(UserHolder, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	if _, err := tbl.Adopt(UserHolder, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	if !allowed(t, tbl, bob) || allowed(t, tbl, carol) {
		t.Fatal("check after the user adopted")
	}
}

func TestWithSurvivesReopen(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	mustPause(t, openTable(t, dir, c), bob)
	tbl := openTable(t, dir, c)
	if !allowed(t, tbl, bob) {
		t.Fatal("with was not persisted")
	}
}

func TestResumeHandsTheCheckoutOnAndClearsWith(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustPause(t, tbl, bob)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	r, err := tbl.BeginResume(Args{Checkout: key})
	if err != nil || r.Result != ResultReleasing || r.Lease.State != StateReleasing {
		t.Fatalf("begin resume = %+v, %v", r, err)
	}
	if allowed(t, tbl, bob) {
		t.Fatal("check passed for a --with pane while resuming")
	}
	if r, err := tbl.AbortResume(Args{Checkout: key}); err != nil || r.Lease.State != StatePaused {
		t.Fatalf("abort resume = %+v, %v", r, err)
	}
	if _, err := tbl.BeginResume(Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	r, err = tbl.Resume(Args{Checkout: key, AsIs: true}, noneGone)
	if err != nil || r.Result != ResultResumed || r.Granted != alice {
		t.Fatalf("resume = %+v, %v", r, err)
	}
	if s := r.Lease; s.State != StateHeld || s.Holder.Handle != alice || len(s.With) != 0 {
		t.Fatalf("status after resume = %+v", s)
	}
	if allowed(t, tbl, bob) {
		t.Fatal("check passed for a --with pane after resume")
	}
	events := leaseEvents(t, dir)
	if !strings.Contains(events, eventlog.EventLeaseResume+" "+UserHolder+"\n"+eventlog.EventLeaseGrant+" "+alice) {
		t.Fatalf("events:\n%s", events)
	}
}

func TestResumeWhenNotHeldByTheUserIsRefused(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	if _, err := tbl.BeginResume(Args{Checkout: key}); err == nil {
		t.Fatal("resume of a free checkout")
	}
	mustClaim(t, tbl, alice, Args{For: "40m"})
	if _, err := tbl.BeginResume(Args{Checkout: key}); err == nil {
		t.Fatal("resume of an agent's hold")
	}
	if _, err := tbl.Resume(Args{Checkout: key}, noneGone); err == nil {
		t.Fatal("resume of an agent's hold")
	}
}

func TestResumeWhileQueuedWithdrawsThePause(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustPause(t, tbl, bob)
	r, err := tbl.BeginResume(Args{Checkout: key})
	if err != nil || r.Result != ResultLeft {
		t.Fatalf("resume while queued = %+v, %v", r, err)
	}
	if s := r.Lease; len(s.Queue) != 0 || len(s.With) != 0 || s.Holder.Handle != alice || s.State != StateHeld {
		t.Fatalf("status = %+v", s)
	}
}

func TestResumeAfterTheUserAdopted(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m", Tree: "dirty"})
	if _, err := tbl.BeginAdopt(UserHolder, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	if _, err := tbl.Adopt(UserHolder, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	if _, err := tbl.BeginResume(Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	r, err := tbl.Resume(Args{Checkout: key}, noneGone)
	if err != nil || r.Granted != alice {
		t.Fatalf("resume after adopt = %+v, %v", r, err)
	}
}

func TestPauseOfAnUnassignedNeedsAdoptIsRefused(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	if _, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "gone", noneGone); err != nil {
		t.Fatal(err)
	}
	if _, err := tbl.Pause(Args{Checkout: key}); err == nil || !strings.Contains(err.Error(), "adopt") {
		t.Fatalf("pause of a needs-adopt checkout = %v", err)
	}
}

func TestTheUsersHoldIsNeverCheckedInOnOrTakenOver(t *testing.T) {
	for _, mode := range []TakeoverMode{TakeoverLog, TakeoverOn} {
		t.Run(string(mode), func(t *testing.T) {
			dir := t.TempDir()
			c := &clock{now: t0()}
			tbl := openTable(t, dir, c)
			mustPause(t, tbl, carol)
			mustClaim(t, tbl, alice, Args{For: "5m"})
			tm := Timing{Interval: 2 * time.Minute, Repeat: 15 * time.Minute, Grace: 5 * time.Minute, Takeover: mode}
			for m := 0; m <= 60; m += 2 {
				c.now = t0().Add(time.Duration(m) * time.Minute)
				if w := tbl.Watched(); len(w) != 0 {
					t.Fatalf("watched at %dm = %+v", m, w)
				}
				for _, p := range []Pane{PaneIdle, PaneGone, PaneBusy} {
					d, err := tbl.Observe(Watch{Checkout: key, Holder: UserHolder}, p, tm)
					if err != nil || d != (Decision{}) {
						t.Fatalf("observe the user at %dm = %+v, %v", m, d, err)
					}
				}
				if tk, err := tbl.TakeOver(Watch{Checkout: key, Holder: UserHolder}, "test", noneGone); err != nil || tk.Done {
					t.Fatalf("takeover of the user = %+v, %v", tk, err)
				}
			}
			s := mustStatus(t, tbl)
			if s.State != StatePaused || s.Holder.Handle != UserHolder {
				t.Fatalf("status after an hour = %+v", s)
			}
			events := leaseEvents(t, dir)
			for _, bad := range []string{eventlog.EventLeaseCheckIn, eventlog.EventLeaseTakeover, eventlog.EventLeaseWouldTakeOver, eventlog.EventLeaseAutoExtend} {
				if strings.Contains(events, bad+" ") {
					t.Fatalf("event %s while paused:\n%s", bad, events)
				}
			}
		})
	}
}
