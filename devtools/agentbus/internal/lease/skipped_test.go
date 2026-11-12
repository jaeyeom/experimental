package lease

import (
	"errors"
	"fmt"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func skippedEvents(t *testing.T, dir string) []eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Name: key, Event: eventlog.EventLeaseNotifySkipped})
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

func wantSkipped(t *testing.T, dir string, want ...eventlog.Event) {
	t.Helper()
	got := skippedEvents(t, dir)
	if len(got) != len(want) {
		t.Fatalf("skipped events = %+v, want %+v", got, want)
	}
	for i := range want {
		if got[i].Actor != want[i].Actor || got[i].Detail != want[i].Detail {
			t.Fatalf("skipped event %d = %+v, want %+v", i, got[i], want[i])
		}
	}
}

func skipped(waiter, holder, reason string, queue int) eventlog.Event {
	return eventlog.Event{Actor: waiter, Detail: fmt.Sprintf("holder %s; reason %s; queue %d", holder, reason, queue)}
}

func TestClaimBehindTheUserLogsASkippedNotice(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustPause(t, tbl)
	if r := mustClaim(t, tbl, bob, Args{For: "30m"}); r.Result != ResultQueued || r.Waiting != nil {
		t.Fatalf("claim = %+v", r)
	}
	wantSkipped(t, dir, skipped(bob, UserHolder, "user-holder", 1))
	if evs := notifyEvents(t, dir); len(evs) != 0 {
		t.Fatalf("notify events = %+v", evs)
	}
}

func TestClaimPastTheHolderDeadlineLogsASkippedNotice(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	c.now = t0().Add(41 * time.Minute)
	if r := mustClaim(t, tbl, bob, Args{For: "30m"}); r.Waiting != nil {
		t.Fatalf("claim = %+v", r)
	}
	wantSkipped(t, dir, skipped(bob, alice, "past-deadline", 1))
}

func TestRepeatClaimsLogOneSkippedNoticePerHolder(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "30m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	for range 3 {
		mustClaim(t, tbl, bob, Args{For: "30m"})
	}
	wantSkipped(t, dir, skipped(bob, alice, "repeat", 2))

	r, err := tbl.Release(alice, Args{Checkout: key}, func(string) bool { return false })
	if err != nil || r.Granted != bob {
		t.Fatalf("release = %+v, %v", r, err)
	}
	mustClaim(t, tbl, carol, Args{For: "10m"})
	mustClaim(t, tbl, carol, Args{For: "10m"})
	wantSkipped(t, dir, skipped(bob, alice, "repeat", 2), skipped(carol, bob, "repeat", 1))
}

func TestUserQueuedClaimLogsNoSkippedNotice(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl := openTable(t, dir, c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, UserHolder, Args{For: "30m"})
	mustClaim(t, tbl, UserHolder, Args{For: "30m"})
	mustPause(t, tbl)
	wantSkipped(t, dir)
}

// failSkipped is an event log that cannot write lease_notify_skipped.
type failSkipped struct{ *eventlog.Log }

func (f failSkipped) Append(e eventlog.Event) error {
	if e.Event == eventlog.EventLeaseNotifySkipped {
		return errors.New("disk full")
	}
	return f.Log.Append(e) //nolint:wrapcheck // test fake
}

func TestFailedSkippedLogLeavesTheReplyUnchanged(t *testing.T) {
	c := &clock{now: t0()}
	dir := t.TempDir()
	tbl, err := Open(dir, failSkipped{eventlog.Open(dir)}, c.Now, nil)
	if err != nil {
		t.Fatal(err)
	}
	mustClaim(t, tbl, alice, Args{For: "40m"})
	mustClaim(t, tbl, bob, Args{For: "30m"})
	r := mustClaim(t, tbl, bob, Args{For: "45m"})
	if r.Result != ResultQueued || r.Position != 1 || r.Waiting != nil || r.Lease == nil || len(r.Lease.Queue) != 1 || r.Lease.Queue[0].For != "45m" {
		t.Fatalf("claim = %+v", r)
	}
	wantSkipped(t, dir)
}
