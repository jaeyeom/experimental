package lease

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func mustStatus(t *testing.T, tbl *Table) Status {
	t.Helper()
	r, err := tbl.Status(Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	return r.Leases[0]
}

func TestBackupRefNamesTheTimeAndTheOldHolder(t *testing.T) {
	got := BackupRef(time.Date(2026, 11, 10, 9, 5, 7, 0, time.FixedZone("x", 3600)), "herdr:local/default/w1:p1")
	if want := "refs/lease-backup/20261110T080507Z-herdr-local-default-w1-p1"; got != want {
		t.Fatalf("BackupRef = %q, want %q", got, want)
	}
	if got := BackupRef(t0(), "ops/alice"); got != "refs/lease-backup/20261110T090000Z-ops-alice" {
		t.Fatalf("named BackupRef = %q", got)
	}
	if got := BackupRef(t0(), " a..b~^:?*[\\c @{ "); strings.ContainsAny(got[len("refs/lease-backup/"):], " ~^:?*[\\/") || strings.Contains(got, "..") || strings.Contains(got, "@{") {
		t.Fatalf("unsafe BackupRef = %q", got)
	}
	if got := BackupRef(t0(), ""); got != "refs/lease-backup/20261110T090000Z-unknown" {
		t.Fatalf("empty BackupRef = %q", got)
	}
}

func TestTakeOverGrantsTheNextWaiterAsNeedsAdopt(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, dave, Args{For: "10m"})
	mustClaim(t, tbl, bob, Args{For: "20m", Why: "issue 626"})
	c.now = t0().Add(50 * time.Minute)

	tk, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "pane gone", func(h string) bool { return h == dave })
	if err != nil {
		t.Fatal(err)
	}
	wantRef := "refs/lease-backup/20261110T095000Z-ops-alice"
	if !tk.Done || tk.From != alice || tk.To != bob || tk.BackupRef != wantRef || tk.FromHeld != true {
		t.Fatalf("takeover = %+v", tk)
	}
	if len(tk.Skipped) != 1 || tk.Skipped[0] != dave {
		t.Fatalf("skipped = %v", tk.Skipped)
	}
	s := mustStatus(t, tbl)
	if s.State != StateNeedsAdopt || s.Holder == nil || s.Holder.Handle != bob || s.BackupRef != wantRef || len(s.Queue) != 0 {
		t.Fatalf("status after takeover = %+v", s)
	}
	if s.Holder.Deadline != c.now.Add(20*time.Minute).Format(time.RFC3339) || s.Holder.Why != "issue 626" {
		t.Fatalf("new holder = %+v", s.Holder)
	}
	events := leaseEventDetails(t, dir)
	for _, want := range []string{
		eventlog.EventLeaseTakeover + " " + alice,
		eventlog.EventLeaseSkip + " " + dave,
		eventlog.EventLeaseGrant + " " + bob,
	} {
		if !strings.Contains(events, want) {
			t.Errorf("events are missing %q:\n%s", want, events)
		}
	}
	// Takeover again by a stale watch does nothing.
	tk, err = tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "pane gone", noneGone)
	if err != nil || tk.Done {
		t.Fatalf("stale takeover = %+v, %v", tk, err)
	}
	if text := TakenOverText(key, wantRef); text != "[lease] taken over from you: "+key+"; your uncommitted work will be saved to "+wantRef {
		t.Fatalf("taken-over text = %q", text)
	}
	if text := AdoptWakeText(key, alice, wantRef); !strings.Contains(text, "needs-adopt") || !strings.Contains(text, "agentbus lease adopt") || !strings.Contains(text, wantRef) {
		t.Fatalf("adopt wake = %q", text)
	}
}

func TestTakeOverAcceptsAReleasingLease(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	if _, err := tbl.BeginRelease(alice, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	tk, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "pane gone", noneGone)
	if err != nil || !tk.Done || tk.To != bob {
		t.Fatalf("takeover of releasing = %+v, %v", tk, err)
	}
	if s := mustStatus(t, tbl); s.State != StateNeedsAdopt {
		t.Fatalf("state = %s", s.State)
	}
}

func TestTakeOverWithNoLiveWaiterLeavesItUnassigned(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, dave, Args{For: "20m"})
	tk, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "idle", func(string) bool { return true })
	if err != nil || !tk.Done || tk.To != "" {
		t.Fatalf("takeover = %+v, %v", tk, err)
	}
	s := mustStatus(t, tbl)
	if s.State != StateNeedsAdopt || s.Holder != nil || s.BackupRef == "" {
		t.Fatalf("status = %+v", s)
	}
	// The next claim gets it, still needs-adopt, even on a clean tree.
	r := mustClaim(t, tbl, carol, Args{For: "10m"})
	if r.Result != ResultGranted || r.Lease.State != StateNeedsAdopt || r.Lease.BackupRef != s.BackupRef {
		t.Fatalf("claim of unassigned needs-adopt = %+v", r)
	}
}

func TestNeedsAdoptPassesToTheNextWaiterWhenTheAdopterIsGone(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	first, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "pane gone", noneGone)
	if err != nil || first.To != bob {
		t.Fatalf("takeover = %+v, %v", first, err)
	}
	c.now = t0().Add(time.Hour)

	// The adopter is watched while someone waits; busy or idle does nothing.
	for _, p := range []Pane{PaneBusy, PaneIdle, PaneUnknown} {
		if d := observe(t, tbl, p); d != (Decision{}) {
			t.Fatalf("%s adopter = %+v", p, d)
		}
	}
	d := observe(t, tbl, PaneGone)
	if !strings.Contains(d.TakeOver, "gone") || d.CheckIn != "" {
		t.Fatalf("gone adopter = %+v", d)
	}
	second, err := tbl.TakeOver(Watch{Checkout: key, Holder: bob}, d.TakeOver, noneGone)
	if err != nil || !second.Done || second.To != carol || second.FromHeld || second.BackupRef != first.BackupRef {
		t.Fatalf("pass on = %+v, %v", second, err)
	}
	s := mustStatus(t, tbl)
	if s.State != StateNeedsAdopt || s.Holder.Handle != carol {
		t.Fatalf("status = %+v", s)
	}
	// With nobody waiting the gone adopter is not watched.
	if w := tbl.Watched(); len(w) != 0 {
		t.Fatalf("watched with no waiter = %+v", w)
	}
}

func TestWatchedRetriesAPendingTakeover(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	if d := observe(t, tbl, PaneGone); d.TakeOver == "" {
		t.Fatalf("gone holder = %+v", d)
	}
	// The daemon died before it took over: the next check asks again.
	d := observe(t, tbl, PaneBusy)
	if !strings.Contains(d.TakeOver, "pending") {
		t.Fatalf("pending takeover = %+v", d)
	}
}

func TestObserveTreatsReleasingLikeHeld(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	if _, err := tbl.BeginRelease(alice, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	c.now = t0().Add(40 * time.Minute)
	if d := observe(t, tbl, PaneIdle); d.CheckIn == "" {
		t.Fatalf("releasing past the deadline = %+v", d)
	}
	c.now = c.now.Add(5 * time.Minute)
	if d := observe(t, tbl, PaneIdle); d.TakeOver == "" {
		t.Fatalf("releasing idle past the grace = %+v", d)
	}
}

func TestAdoptByTheAssignedAdopter(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	tk, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "pane gone", noneGone)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := tbl.BeginAdopt(carol, Args{Checkout: key}); err == nil || !strings.Contains(err.Error(), "adopt") {
		t.Fatalf("begin adopt by a non-adopter = %v", err)
	}
	r, err := tbl.BeginAdopt(bob, Args{Checkout: key})
	if err != nil || r.Lease.BackupRef != tk.BackupRef || r.Lease.Holder.Handle != bob {
		t.Fatalf("begin adopt = %+v, %v", r, err)
	}
	r, err = tbl.Adopt(bob, Args{Checkout: key, Backups: []string{tk.BackupRef}})
	if err != nil || r.Result != ResultAdopted || r.Lease.State != StateHeld || r.Lease.BackupRef != "" || r.Lease.Holder.Handle != bob {
		t.Fatalf("adopt = %+v, %v", r, err)
	}
	if r, _ := tbl.Check(bob, Args{Checkout: key}); r.Result != ResultAllowed {
		t.Fatalf("check after adopt = %s", r.Result)
	}
	if _, err := tbl.Adopt(bob, Args{Checkout: key}); err == nil {
		t.Fatal("adopt of a held lease succeeded")
	}
	events := leaseEventDetails(t, dir)
	if !strings.Contains(events, eventlog.EventLeaseAdopt+" "+bob) || !strings.Contains(events, tk.BackupRef) {
		t.Fatalf("events are missing the adopt:\n%s", events)
	}
}

func TestAdoptByTheUserTakesTheCheckoutAndRequeuesTheAdopter(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	if _, err := tbl.TakeOver(Watch{Checkout: key, Holder: alice}, "pane gone", noneGone); err != nil {
		t.Fatal(err)
	}
	r, err := tbl.BeginAdopt(UserHolder, Args{Checkout: key})
	if err != nil || r.Lease.Holder.Handle != UserHolder || r.Lease.State != StateNeedsAdopt {
		t.Fatalf("user begin adopt = %+v, %v", r, err)
	}
	if got := queueHandles(r.Lease); len(got) != 2 || got[0] != bob || got[1] != carol {
		t.Fatalf("queue after the user stepped in = %v", got)
	}
	r, err = tbl.Adopt(UserHolder, Args{Checkout: key})
	if err != nil || r.Lease.State != StateHeld || r.Lease.Holder.Handle != UserHolder || r.Lease.Holder.Deadline != "" {
		t.Fatalf("user adopt = %+v, %v", r, err)
	}
}

func TestAdoptOfAnUnassignedLeftoverNamesABackupRef(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	// A free checkout with a dirty tree is granted as needs-adopt.
	mustClaim(t, tbl, alice, Args{For: "40m", Tree: "1 changed file"})
	r, err := tbl.BeginAdopt(alice, Args{Checkout: key})
	if err != nil || r.Lease.BackupRef != "refs/lease-backup/20261110T090000Z-unknown" {
		t.Fatalf("begin adopt = %+v, %v", r, err)
	}
	if _, err := tbl.BeginAdopt(bob, Args{Checkout: key}); err == nil {
		t.Fatal("begin adopt by a non-holder succeeded")
	}
}

func TestBeginAdoptOfAHeldLeaseIsRefused(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	if _, err := tbl.BeginAdopt(alice, Args{Checkout: key}); err == nil || !strings.Contains(err.Error(), "does not need adopt") {
		t.Fatalf("begin adopt of a held lease = %v", err)
	}
}

func leaseEventDetails(t *testing.T, dir string) string {
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
		out = append(out, ev.Event+" "+ev.Actor+" "+ev.Detail)
	}
	return strings.Join(out, "\n")
}

func TestObserveExtendsAWorkingReleasingHolder(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	if _, err := tbl.BeginRelease(alice, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	c.now = t0().Add(40 * time.Minute)
	for i := 0; i < 3; i++ {
		c.now = c.now.Add(2 * time.Minute)
		if d := observe(t, tbl, PaneBusy); d.TakeOver != "" {
			t.Fatalf("tick %d took over a working releasing holder: %+v", i, d)
		}
		if got, want := holderDeadline(t, tbl), c.now.Add(2*time.Minute).Format(time.RFC3339); got != want {
			t.Fatalf("tick %d deadline = %s, want %s", i, got, want)
		}
	}
}
