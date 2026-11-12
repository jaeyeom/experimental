package hibernate

import (
	"os"
	"path/filepath"
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
	// alice was already done when the tracker started: at least 40m.
	if got[alice] != (Seen{Status: "idle", For: 40 * time.Minute, LowerBound: true}) {
		t.Fatalf("alice = %+v, want idle at least 40m", got[alice])
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

// restartTracker saves tr to a file and loads it into a new tracker, as
// a restarted daemon does.
func restartTracker(t *testing.T, tr *IdleTracker, now time.Time) *IdleTracker {
	t.Helper()
	path := filepath.Join(t.TempDir(), "seen.json")
	if err := tr.Save(path, now); err != nil {
		t.Fatal(err)
	}
	var next IdleTracker
	if err := next.Load(path); err != nil {
		t.Fatal(err)
	}
	return &next
}

func TestIdleTrackerKeepsSinceAcrossARestartWhileTheSeqStays(t *testing.T) {
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var tr IdleTracker
	tr.Observe(t0, []adapter.Agent{
		{Handle: alice, Status: "idle", StateSeq: 1},
		{Handle: bob, Status: "working", StateSeq: 1},
		{Handle: carol, Status: "working", StateSeq: 1},
	})
	tr.Observe(t0.Add(time.Hour), []adapter.Agent{
		{Handle: alice, Status: "idle", StateSeq: 1},
		{Handle: bob, Status: "idle", StateSeq: 2},
		{Handle: carol, Status: "idle", StateSeq: 2},
	})
	next := restartTracker(t, &tr, t0.Add(2*time.Hour))
	if len(next.Seen(t0.Add(3*time.Hour))) != 0 {
		t.Fatal("a loaded tracker reports agents herdr has not confirmed")
	}
	next.Observe(t0.Add(3*time.Hour), []adapter.Agent{
		{Handle: alice, Status: "idle", StateSeq: 1},
		{Handle: bob, Status: "idle", StateSeq: 2},
		{Handle: carol, Status: "idle", StateSeq: 3},
	})
	got := next.Seen(t0.Add(3 * time.Hour))
	if got[bob] != (Seen{Status: "idle", For: 2 * time.Hour}) {
		t.Fatalf("bob = %+v, want the idle time kept across the restart", got[bob])
	}
	if got[alice] != (Seen{Status: "idle", For: 3 * time.Hour, LowerBound: true}) {
		t.Fatalf("alice = %+v, want the lower bound kept across the restart", got[alice])
	}
	if got[carol] != (Seen{Status: "idle", LowerBound: true}) {
		t.Fatalf("carol = %+v, want a changed seq to start over", got[carol])
	}
	// The loaded entries are used on the first snapshot only.
	next.Observe(t0.Add(4*time.Hour), []adapter.Agent{{Handle: bob, Status: "idle", StateSeq: 2}})
	next.Observe(t0.Add(5*time.Hour), []adapter.Agent{{Handle: bob, Status: "idle", StateSeq: 2}, {Handle: carol, Status: "idle", StateSeq: 2}})
	if got := next.Seen(t0.Add(5 * time.Hour))[carol]; got.For != 0 {
		t.Fatalf("carol = %+v, want a fresh start", got)
	}
}

func TestIdleTrackerLoadOfAMissingOrBadFile(t *testing.T) {
	dir := t.TempDir()
	var tr IdleTracker
	if err := tr.Load(filepath.Join(dir, "missing.json")); err != nil {
		t.Fatalf("Load(missing) = %v, want nil", err)
	}
	bad := filepath.Join(dir, "bad.json")
	if err := os.WriteFile(bad, []byte("{"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := tr.Load(bad); err == nil {
		t.Fatal("Load(bad) = nil, want an error")
	}
}

func TestIdleTrackerSavesAtMostOncePerMinute(t *testing.T) {
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	path := filepath.Join(t.TempDir(), "seen.json")
	var tr IdleTracker
	tr.Observe(t0, []adapter.Agent{{Handle: alice, Status: "idle", StateSeq: 1}})
	saved := func(now time.Time) bool {
		t.Helper()
		_ = os.Remove(path)
		if err := tr.SaveEvery(path, now); err != nil {
			t.Fatal(err)
		}
		_, err := os.Stat(path)
		return err == nil
	}
	if !saved(t0) {
		t.Fatal("the first save did not write")
	}
	if saved(t0.Add(59 * time.Second)) {
		t.Fatal("saved again within a minute")
	}
	if !saved(t0.Add(time.Minute)) {
		t.Fatal("did not save a minute later")
	}
}

func TestIdleTrackerBoundsAnUntrackedIdleAgentByItsLastTurn(t *testing.T) {
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var asked []string
	tr := IdleTracker{LastTurn: func(a adapter.Agent) (time.Time, bool) {
		asked = append(asked, a.Handle)
		return t0.Add(-3 * time.Hour), true
	}}
	tr.Observe(t0, []adapter.Agent{
		{Handle: alice, Status: "idle", StateSeq: 1},
		{Handle: bob, Status: "working", StateSeq: 1},
		{Handle: carol, Status: "done", StateSeq: 1},
	})
	got := tr.Seen(t0)
	if got[alice] != (Seen{Status: "idle", For: 3 * time.Hour}) || got[carol] != (Seen{Status: "done", For: 3 * time.Hour}) {
		t.Fatalf("seen = %+v, want alice and carol idle since their last turn 3h ago", got)
	}
	if got[bob] != (Seen{Status: "working", LowerBound: true}) {
		t.Fatalf("bob = %+v, want no last turn for a working agent", got[bob])
	}
	// A tracked agent keeps its own since; one that just went idle
	// starts now.
	asked = nil
	tr.Observe(t0.Add(time.Minute), []adapter.Agent{
		{Handle: alice, Status: "idle", StateSeq: 1},
		{Handle: bob, Status: "idle", StateSeq: 2},
	})
	if len(asked) != 0 {
		t.Fatalf("asked the last turn of %v, want no tracked agent asked", asked)
	}
	if got := tr.Seen(t0.Add(time.Minute))[bob]; got.For != 0 {
		t.Fatalf("bob = %+v, want idle from now", got)
	}
}

func TestIdleTrackerIgnoresALastTurnInTheFuture(t *testing.T) {
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	tr := IdleTracker{LastTurn: func(adapter.Agent) (time.Time, bool) { return t0.Add(time.Minute), true }}
	tr.Observe(t0, []adapter.Agent{{Handle: alice, Status: "idle", StateSeq: 1}})
	if got := tr.Seen(t0)[alice]; got != (Seen{Status: "idle", LowerBound: true}) {
		t.Fatalf("alice = %+v", got)
	}
}

func TestRestoredSeenMatchesOnlyUnchangedAgents(t *testing.T) {
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var tr IdleTracker
	tr.Observe(t0, []adapter.Agent{{Handle: alice, Status: "idle", StateSeq: 1}, {Handle: bob, Status: "idle", StateSeq: 1}})
	tr.Observe(t0.Add(time.Hour), []adapter.Agent{{Handle: alice, Status: "idle", StateSeq: 1}, {Handle: bob, Status: "idle", StateSeq: 1}, {Handle: carol, Status: "idle", StateSeq: 1}})
	path := filepath.Join(t.TempDir(), "seen.json")
	if err := tr.Save(path, t0.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	got, err := LoadSeen(path, t0.Add(2*time.Hour), []adapter.Agent{
		{Handle: alice, Status: "idle", StateSeq: 1},
		{Handle: bob, Status: "idle", StateSeq: 2},
		{Handle: carol, Status: "idle", StateSeq: 1},
	})
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]Seen{
		alice: {Status: "idle", For: 2 * time.Hour, LowerBound: true},
		carol: {Status: "idle", For: time.Hour},
	}
	if len(got) != len(want) || got[alice] != want[alice] || got[carol] != want[carol] {
		t.Fatalf("LoadSeen = %+v, want %+v", got, want)
	}
}
