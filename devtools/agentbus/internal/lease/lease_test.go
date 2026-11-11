package lease

import (
	"encoding/json"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

const (
	key   = "local:/home/u/src/repo"
	alice = "herdr:local/default/w1:p1"
	bob   = "herdr:local/default/w1:p2"
	carol = "herdr:local/default/w1:p3"
	dave  = "herdr:local/default/w1:p4"
)

func t0() time.Time { return time.Date(2026, 11, 10, 9, 0, 0, 0, time.UTC) }

type clock struct{ now time.Time }

func (c *clock) Now() time.Time { return c.now }

func openTable(t *testing.T, dir string, c *clock) *Table {
	t.Helper()
	tbl, err := Open(dir, eventlog.Open(dir), c.Now, func(handle string) string {
		if handle == alice {
			return "ops/alice"
		}
		return ""
	})
	if err != nil {
		t.Fatal(err)
	}
	return tbl
}

func mustClaim(t *testing.T, tbl *Table, caller string, a Args) Reply {
	t.Helper()
	a.Checkout = key
	r, err := tbl.Claim(caller, a)
	if err != nil {
		t.Fatalf("claim by %s: %v", caller, err)
	}
	return r
}

func queueHandles(s *Status) []string {
	out := make([]string, len(s.Queue))
	for i, w := range s.Queue {
		out[i] = w.Handle
	}
	return out
}

func TestClaimGrantsAFreeCheckoutAndQueuesTheRest(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)

	r := mustClaim(t, tbl, alice, Args{For: "40m", Why: "issue 623"})
	if r.Result != ResultGranted || r.Lease.State != StateHeld || r.Lease.Holder == nil {
		t.Fatalf("first claim = %+v", r)
	}
	h := r.Lease.Holder
	if h.Handle != alice || h.Name != "ops/alice" || h.Why != "issue 623" || h.Deadline != t0().Add(40*time.Minute).Format(time.RFC3339) {
		t.Fatalf("holder = %+v", h)
	}

	c.now = t0().Add(time.Minute)
	r = mustClaim(t, tbl, bob, Args{For: "20m"})
	if r.Result != ResultQueued || r.Position != 1 {
		t.Fatalf("second claim = %+v", r)
	}
	r = mustClaim(t, tbl, carol, Args{For: "10m"})
	if r.Position != 2 {
		t.Fatalf("third claim = %+v", r)
	}
	r = mustClaim(t, tbl, dave, Args{For: "5m", Urgent: true})
	if r.Result != ResultQueued || r.Position != 1 {
		t.Fatalf("urgent claim = %+v", r)
	}
	if got := strings.Join(queueHandles(r.Lease), ","); got != dave+","+bob+","+carol {
		t.Fatalf("queue = %s", got)
	}
	if !r.Lease.Queue[0].Urgent || r.Lease.Queue[1].For != "20m" {
		t.Fatalf("queue = %+v", r.Lease.Queue)
	}
}

func TestUserQueuesAheadOfUrgent(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "5m", Urgent: true})
	r := mustClaim(t, tbl, UserHolder, Args{For: "1h"})
	if r.Position != 1 {
		t.Fatalf("user claim = %+v", r)
	}
	r = mustClaim(t, tbl, carol, Args{For: "5m", Urgent: true})
	if got := strings.Join(queueHandles(r.Lease), ","); got != UserHolder+","+bob+","+carol {
		t.Fatalf("queue = %s", got)
	}
}

func TestReclaimIsIdempotentAndUpdatesForAndWhy(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m", Why: "a"})
	mustClaim(t, tbl, bob, Args{For: "20m", Why: "b"})
	mustClaim(t, tbl, carol, Args{For: "10m"})

	c.now = t0().Add(10 * time.Minute)
	r := mustClaim(t, tbl, alice, Args{For: "30m", Why: "a2"})
	if r.Result != ResultGranted || r.Lease.Holder.Why != "a2" || r.Lease.Holder.Deadline != c.now.Add(30*time.Minute).Format(time.RFC3339) {
		t.Fatalf("holder reclaim = %+v %+v", r, r.Lease.Holder)
	}
	if r.Lease.Holder.Since != t0().Format(time.RFC3339) {
		t.Fatalf("holder since moved: %+v", r.Lease.Holder)
	}
	r = mustClaim(t, tbl, bob, Args{For: "25m", Why: "b2"})
	if r.Result != ResultQueued || r.Position != 1 || len(r.Lease.Queue) != 2 || r.Lease.Queue[0].For != "25m" {
		t.Fatalf("waiter reclaim = %+v", r)
	}
	if r.Lease.Queue[0].Since != t0().Format(time.RFC3339) {
		t.Fatalf("waiter since moved: %+v", r.Lease.Queue[0])
	}
}

func TestLeaveRemovesAWaiter(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	r, err := tbl.Leave(bob, Args{Checkout: key})
	if err != nil || r.Result != ResultLeft {
		t.Fatalf("leave = %+v, %v", r, err)
	}
	if got := strings.Join(queueHandles(r.Lease), ","); got != carol {
		t.Fatalf("queue = %s", got)
	}
	if _, err := tbl.Leave(bob, Args{Checkout: key}); err == nil {
		t.Fatal("second leave succeeded")
	}
	if _, err := tbl.Leave(alice, Args{Checkout: key}); err == nil || !strings.Contains(err.Error(), "hold") {
		t.Fatalf("holder leave = %v", err)
	}
}

func TestExtendIsHolderOnly(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	if _, err := tbl.Extend(bob, Args{Checkout: key, For: "1h"}); err == nil || !strings.Contains(err.Error(), "holder") {
		t.Fatalf("non-holder extend = %v", err)
	}
	c.now = t0().Add(50 * time.Minute)
	r, err := tbl.Extend(alice, Args{Checkout: key, For: "20m"})
	if err != nil || r.Result != ResultExtended || r.Lease.Holder.Deadline != c.now.Add(20*time.Minute).Format(time.RFC3339) {
		t.Fatalf("extend = %+v, %v", r, err)
	}
	if _, err := tbl.Extend(alice, Args{Checkout: key, For: "-1m"}); err == nil {
		t.Fatal("negative extend succeeded")
	}
}

func TestCheck(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	r, err := tbl.Check(alice, Args{Checkout: key})
	if err != nil || r.Result != ResultDenied || r.Lease.State != StateFree {
		t.Fatalf("free check = %+v, %v", r, err)
	}
	mustClaim(t, tbl, alice, Args{For: "40m"})
	if r, _ := tbl.Check(alice, Args{Checkout: key}); r.Result != ResultAllowed {
		t.Fatalf("holder check = %+v", r)
	}
	if r, _ := tbl.Check(bob, Args{Checkout: key}); r.Result != ResultDenied || r.Lease.Holder.Handle != alice {
		t.Fatalf("other check = %+v", r)
	}
}

func TestDirtyFreeCheckoutIsGrantedAsNeedsAdopt(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	r := mustClaim(t, tbl, alice, Args{For: "40m", Tree: "dirty: 1 modified file"})
	if r.Result != ResultGranted || r.Lease.State != StateNeedsAdopt || r.Lease.Holder.Handle != alice {
		t.Fatalf("dirty claim = %+v", r)
	}
	if r, _ := tbl.Check(alice, Args{Checkout: key}); r.Result != ResultDenied {
		t.Fatalf("needs-adopt holder check = %+v", r)
	}
	// A dirty tree under a holder does not change the state.
	r = mustClaim(t, tbl, bob, Args{For: "40m", Tree: "dirty"})
	if r.Result != ResultQueued || r.Lease.State != StateNeedsAdopt {
		t.Fatalf("queued dirty claim = %+v", r)
	}
}

func TestStateSurvivesReopen(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m", Why: "w"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	mustClaim(t, tbl, carol, Args{For: "5m", Urgent: true})
	before, err := tbl.Status(Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}

	c.now = t0().Add(time.Hour)
	again := openTable(t, dir, c)
	after, err := again.Status(Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	b, _ := json.Marshal(before.Leases)
	a, _ := json.Marshal(after.Leases)
	if string(a) != string(b) {
		t.Fatalf("after reopen\n%s\nwant\n%s", a, b)
	}
	// Arrival order continues after a reopen.
	r := mustClaim(t, again, dave, Args{For: "5m", Urgent: true})
	if got := strings.Join(queueHandles(r.Lease), ","); got != carol+","+dave+","+bob {
		t.Fatalf("queue after reopen = %s", got)
	}
}

func TestStatusAllListsEveryCheckoutSeen(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	if _, err := tbl.Status(Args{Checkout: "mac:/Users/u/src/repo"}); err != nil {
		t.Fatal(err)
	}
	mustClaim(t, tbl, alice, Args{For: "40m"})
	r, err := tbl.Status(Args{All: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(r.Leases) != 2 || r.Leases[0].Checkout != key || r.Leases[1].Checkout != "mac:/Users/u/src/repo" || r.Leases[1].State != StateFree {
		t.Fatalf("all = %+v", r.Leases)
	}
}

func TestEventsAreLogged(t *testing.T) {
	dir := t.TempDir()
	c := &clock{now: t0()}
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	if _, err := tbl.Extend(alice, Args{Checkout: key, For: "1h"}); err != nil {
		t.Fatal(err)
	}
	if _, err := tbl.Leave(bob, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Name: key})
	if err != nil {
		t.Fatal(err)
	}
	var got []string
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		got = append(got, ev.Event+" "+ev.Actor)
	}
	want := []string{
		eventlog.EventLeaseClaim + " " + alice,
		eventlog.EventLeaseGrant + " " + alice,
		eventlog.EventLeaseClaim + " " + bob,
		eventlog.EventLeaseExtend + " " + alice,
		eventlog.EventLeaseLeave + " " + bob,
	}
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Fatalf("events =\n%s\nwant\n%s", strings.Join(got, "\n"), strings.Join(want, "\n"))
	}
}

func TestBadArgsAreRefused(t *testing.T) {
	tbl := openTable(t, t.TempDir(), &clock{now: t0()})
	for _, a := range []Args{
		{Checkout: key},
		{Checkout: key, For: "soon"},
		{Checkout: key, For: "0s"},
		{Checkout: "/no/machine", For: "1m"},
		{Checkout: "local:relative", For: "1m"},
	} {
		if _, err := tbl.Claim(alice, a); err == nil {
			t.Errorf("claim %+v succeeded", a)
		}
	}
	if _, err := tbl.Claim("", Args{Checkout: key, For: "1m"}); err == nil {
		t.Error("claim without a caller succeeded")
	}
}

func TestConcurrentClaimsQueueEveryone(t *testing.T) {
	tbl := openTable(t, t.TempDir(), &clock{now: t0()})
	var wg sync.WaitGroup
	for i := range 20 {
		wg.Go(func() {
			if _, err := tbl.Claim(fmt.Sprintf("herdr:local/default/w1:q%d", i), Args{Checkout: key, For: "1m"}); err != nil {
				t.Error(err)
			}
		})
	}
	wg.Wait()
	r, err := tbl.Status(Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	if s := r.Leases[0]; s.Holder == nil || len(s.Queue) != 19 {
		t.Fatalf("status = %+v", s)
	}
}
