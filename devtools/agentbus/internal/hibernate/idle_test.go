package hibernate

import (
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

func TestIdleTrackerKeepsSinceWhileTheStateStays(t *testing.T) {
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var tr IdleTracker
	tr.Observe(t0, []adapter.Agent{
		{Handle: alice, Status: "done", StateSeq: 1},
		{Handle: bob, Status: "working", StateSeq: 1},
		{Handle: carol, Status: ""},
	})
	tr.Observe(t0.Add(10*time.Minute), []adapter.Agent{
		// done to idle is the user looking at a finished turn.
		{Handle: alice, Status: "idle", StateSeq: 2},
		{Handle: bob, Status: "idle", StateSeq: 2},
	})
	got := tr.Seen(t0.Add(40 * time.Minute))
	if got[alice] != (Seen{Status: "idle", For: 40 * time.Minute}) {
		t.Fatalf("alice = %+v, want idle 40m", got[alice])
	}
	if got[bob] != (Seen{Status: "idle", For: 30 * time.Minute}) {
		t.Fatalf("bob = %+v, want idle 30m", got[bob])
	}
	if _, ok := got[carol]; ok || len(got) != 2 {
		t.Fatalf("seen = %+v, want no row for a pane without a status", got)
	}
	// An agent no longer listed is dropped, and starts over when it
	// comes back.
	tr.Observe(t0.Add(50*time.Minute), []adapter.Agent{{Handle: bob, Status: "idle", StateSeq: 2}})
	tr.Observe(t0.Add(60*time.Minute), []adapter.Agent{{Handle: alice, Status: "idle", StateSeq: 2}, {Handle: bob, Status: "idle", StateSeq: 2}})
	got = tr.Seen(t0.Add(60 * time.Minute))
	if got[alice].For != 0 || got[bob].For != 50*time.Minute {
		t.Fatalf("seen = %+v, want alice over at 0 and bob still 50m", got)
	}
}
