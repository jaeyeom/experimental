package lease

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func noneGone(string) bool { return false }

func TestReleaseIsHolderOnly(t *testing.T) {
	tbl := openTable(t, t.TempDir(), &clock{now: t0()})
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	for name, call := range map[string]func() (Reply, error){
		"begin":   func() (Reply, error) { return tbl.BeginRelease(bob, Args{Checkout: key}) },
		"abort":   func() (Reply, error) { return tbl.AbortRelease(bob, Args{Checkout: key}) },
		"release": func() (Reply, error) { return tbl.Release(bob, Args{Checkout: key}, noneGone) },
	} {
		if _, err := call(); err == nil || !strings.Contains(err.Error(), "only the holder") {
			t.Errorf("%s by a waiter: err = %v", name, err)
		}
	}
	r, err := tbl.Status(Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	if s := r.Leases[0]; s.State != StateHeld || s.Holder.Handle != alice || len(s.Queue) != 1 {
		t.Fatalf("status after refused release = %+v", s)
	}
}

func TestBeginReleaseMarksReleasingAndAbortRestoresHeld(t *testing.T) {
	tbl := openTable(t, t.TempDir(), &clock{now: t0()})
	mustClaim(t, tbl, alice, Args{For: "40m"})
	r, err := tbl.BeginRelease(alice, Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	if r.Result != ResultReleasing || r.Lease.State != StateReleasing {
		t.Fatalf("begin = %+v", r)
	}
	if r, _ := tbl.Check(alice, Args{Checkout: key}); r.Result != ResultDenied {
		t.Fatalf("check while releasing = %s", r.Result)
	}
	// Running release again after a crash finds it releasing already.
	if _, err := tbl.BeginRelease(alice, Args{Checkout: key}); err != nil {
		t.Fatalf("second begin: %v", err)
	}
	r, err = tbl.AbortRelease(alice, Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	if r.Lease.State != StateHeld || r.Lease.Holder.Handle != alice {
		t.Fatalf("abort = %+v", r.Lease)
	}
}

func TestReleaseGrantsTheNextWaiter(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m", Why: "issue 624"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	if _, err := tbl.BeginRelease(alice, Args{Checkout: key}); err != nil {
		t.Fatal(err)
	}
	c.now = t0().Add(30 * time.Minute)
	r, err := tbl.Release(alice, Args{Checkout: key}, noneGone)
	if err != nil {
		t.Fatal(err)
	}
	if r.Result != ResultReleased || r.Granted != bob || len(r.Skipped) != 0 {
		t.Fatalf("release = %+v", r)
	}
	s := r.Lease
	want := t0().Add(50 * time.Minute).Format(time.RFC3339)
	if s.State != StateHeld || s.Holder.Handle != bob || s.Holder.Why != "issue 624" || s.Holder.Deadline != want || s.Holder.Since != c.now.Format(time.RFC3339) {
		t.Fatalf("holder = %+v", s.Holder)
	}
	if got := queueHandles(s); len(got) != 1 || got[0] != carol {
		t.Fatalf("queue = %v", got)
	}
}

func TestReleaseSkipsAGoneWaiter(t *testing.T) {
	tbl := openTable(t, t.TempDir(), &clock{now: t0()})
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	r, err := tbl.Release(alice, Args{Checkout: key}, func(h string) bool { return h == bob })
	if err != nil {
		t.Fatal(err)
	}
	if r.Granted != carol || len(r.Skipped) != 1 || r.Skipped[0] != bob {
		t.Fatalf("release = %+v", r)
	}
	if r.Lease.Holder.Handle != carol || len(r.Lease.Queue) != 0 {
		t.Fatalf("lease = %+v", r.Lease)
	}
}

func TestReleaseWithNoWaiterFreesTheCheckout(t *testing.T) {
	tbl := openTable(t, t.TempDir(), &clock{now: t0()})
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	r, err := tbl.Release(alice, Args{Checkout: key}, func(string) bool { return true })
	if err != nil {
		t.Fatal(err)
	}
	if r.Granted != "" || r.Lease.State != StateFree || r.Lease.Holder != nil || len(r.Lease.Queue) != 0 {
		t.Fatalf("release = %+v %+v", r, r.Lease)
	}
}

func TestReleaseGrantAndSkipAreLogged(t *testing.T) {
	dir := t.TempDir()
	tbl := openTable(t, dir, &clock{now: t0()})
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "20m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	if _, err := tbl.Release(alice, Args{Checkout: key, PullError: "could not read from remote"}, func(h string) bool { return h == bob }); err != nil {
		t.Fatal(err)
	}
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Name: key})
	if err != nil {
		t.Fatal(err)
	}
	var got []string
	var release eventlog.Event
	for _, line := range lines[4:] {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		got = append(got, ev.Event+" "+ev.Actor)
		if ev.Event == eventlog.EventLeaseRelease {
			release = ev
		}
	}
	want := []string{
		eventlog.EventLeaseRelease + " " + alice,
		eventlog.EventLeaseSkip + " " + bob,
		eventlog.EventLeaseGrant + " " + carol,
	}
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Fatalf("events =\n%s\nwant\n%s", strings.Join(got, "\n"), strings.Join(want, "\n"))
	}
	if !strings.Contains(release.Reason, "could not read from remote") {
		t.Fatalf("release event = %+v", release)
	}
}

func TestWakeText(t *testing.T) {
	deadline := time.Date(2026, 11, 10, 9, 40, 0, 0, time.UTC)
	h := HolderView{Handle: bob, Deadline: deadline.Format(time.RFC3339), Why: "issue 624"}
	hhmm := deadline.Local().Format("15:04")
	want := "[lease] granted " + key + " until " + hhmm + " (for: issue 624). Release with: agentbus lease release"
	if got := WakeText(key, h); got != want {
		t.Fatalf("wake = %q, want %q", got, want)
	}
	h.Why = ""
	want = "[lease] granted " + key + " until " + hhmm + ". Release with: agentbus lease release"
	if got := WakeText(key, h); got != want {
		t.Fatalf("wake without why = %q", got)
	}
}
