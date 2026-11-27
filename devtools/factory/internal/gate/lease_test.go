package gate

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// fakeLease returns one reading and counts its calls.
type fakeLease struct {
	reading agentbus.Reading
	err     error
	dirs    []string
}

func (f *fakeLease) read(_ context.Context, dir string) (agentbus.Reading, error) {
	f.dirs = append(f.dirs, dir)
	return f.reading, f.err
}

func guarded(st agentbus.Status) agentbus.Reading {
	if st.Checkout == "" {
		st.Checkout = "local:/repo"
	}
	return agentbus.Reading{Guarded: true, Status: st}
}

const leaseDeadline = "2026-11-11T14:20:00Z"

func heldBy(name, why string, queued int) agentbus.Status {
	queue := make([]agentbus.Waiter, queued)
	for i := range queue {
		queue[i] = agentbus.Waiter{Handle: fmt.Sprintf("herdr:local/default/w9:p%d", i)}
	}
	return agentbus.Status{
		State:  agentbus.StateHeld,
		Holder: &agentbus.Holder{Handle: "herdr:local/default/w1:p2", Name: name, Deadline: leaseDeadline, Why: why},
		Queue:  queue,
	}
}

func localClock(value string) string {
	t, err := time.Parse(time.RFC3339, value)
	if err != nil {
		panic(err)
	}
	return t.Local().Format("15:04")
}

func leaseInputs(mode string, lease *fakeLease) Inputs {
	return Inputs{
		Checkouts: []string{"/repo"},
		GitStatus: func(context.Context, string) (string, error) { return "", nil },
		LeaseGate: mode,
		Lease:     lease.read,
		Now:       time.Date(2026, 11, 11, 13, 0, 0, 0, time.UTC),
	}
}

var leaseDefaults = config.Defaults{MaxInFlight: 3}

func TestCheckLease(t *testing.T) {
	missing := fmt.Errorf("%w: executable not found", agentbus.ErrMissing)
	tests := []struct {
		name    string
		mode    string
		reading agentbus.Reading
		err     error
		owner   agentbus.Owner
		reasons []string
		notes   []string
	}{
		{
			name:    "free is usable",
			mode:    config.LeaseGateAuto,
			reading: guarded(agentbus.Status{State: agentbus.StateFree}),
		},
		{
			name:    "held by another agent is busy with the detail",
			mode:    config.LeaseGateAuto,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 1)),
			reasons: []string{"/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (1 queued)"},
		},
		{
			name:    "held by the agent being started is usable",
			mode:    config.LeaseGateAuto,
			reading: guarded(heldBy("", "PRJ-1 Fix it", 0)),
			owner:   agentbus.Owner{ItemID: "PRJ-1", Agent: "prj-1"},
		},
		{
			name:    "held by the agent name is usable",
			mode:    config.LeaseGateRequired,
			reading: guarded(heldBy("prj-1", "", 0)),
			owner:   agentbus.Owner{ItemID: "PRJ-1", Agent: "prj-1"},
		},
		{
			name: "paused is busy",
			mode: config.LeaseGateAuto,
			reading: guarded(agentbus.Status{
				State:  agentbus.StatePaused,
				Holder: &agentbus.Holder{Handle: "user"},
			}),
			owner:   agentbus.Owner{ItemID: "PRJ-1"},
			reasons: []string{"/repo: lease paused by user (0 queued)"},
		},
		{
			name: "needs-adopt is busy",
			mode: config.LeaseGateAuto,
			reading: guarded(agentbus.Status{
				State: agentbus.StateNeedsAdopt,
				Queue: []agentbus.Waiter{{Handle: "h1"}, {Handle: "h2"}},
			}),
			reasons: []string{"/repo: lease needs-adopt (no holder, 2 queued)"},
		},
		{
			name: "missing agentbus under auto is the old behavior plus a note",
			mode: config.LeaseGateAuto,
			err:  missing,
			notes: []string{
				"/repo: lease not read (agentbus not installed: executable not found); used the herdr and git checks only",
			},
		},
		{
			name: "missing agentbus under required is busy",
			mode: config.LeaseGateRequired,
			err:  missing,
			reasons: []string{
				"/repo: lease not read (agentbus not installed: executable not found); lease gate is required",
			},
		},
		{
			name:  "unguarded checkout under auto is a note",
			mode:  config.LeaseGateAuto,
			notes: []string{"/repo: lease not read (not a guarded checkout in agentbus lease_checkouts); used the herdr and git checks only"},
		},
		{
			name:    "unguarded checkout under required is busy",
			mode:    config.LeaseGateRequired,
			reasons: []string{"/repo: lease not read (not a guarded checkout in agentbus lease_checkouts); lease gate is required"},
		},
		{
			name:    "free under queue proceeds",
			mode:    config.LeaseGateQueue,
			reading: guarded(agentbus.Status{State: agentbus.StateFree}),
		},
		{
			name:    "held by another agent under queue proceeds and queues",
			mode:    config.LeaseGateQueue,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 1)),
			notes: []string{
				"/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (1 queued); start will queue behind holder (lease gate: queue)",
				"/repo: checkout checks skipped: lease held by pm/foo",
			},
		},
		{
			name:    "held by owner under queue proceeds without queue note",
			mode:    config.LeaseGateQueue,
			reading: guarded(heldBy("prj-1", "PRJ-1 Fix it", 0)),
			owner:   agentbus.Owner{ItemID: "PRJ-1", Agent: "prj-1"},
		},
		{
			name: "releasing under queue proceeds and queues",
			mode: config.LeaseGateQueue,
			reading: guarded(agentbus.Status{
				State:  agentbus.StateReleasing,
				Holder: &agentbus.Holder{Handle: "herdr:local/default/w1:p2", Name: "pm/foo", Deadline: leaseDeadline},
				Queue:  []agentbus.Waiter{{Handle: "h1"}},
			}),
			notes: []string{
				"/repo: lease releasing by pm/foo until " + localClock(leaseDeadline) + " (1 queued); start will queue behind holder (lease gate: queue)",
				"/repo: checkout checks skipped: lease releasing by pm/foo",
			},
		},
		{
			name: "paused under queue still refuses",
			mode: config.LeaseGateQueue,
			reading: guarded(agentbus.Status{
				State:  agentbus.StatePaused,
				Holder: &agentbus.Holder{Handle: "user"},
			}),
			reasons: []string{"/repo: lease paused by user (0 queued)"},
		},
		{
			name: "needs-adopt under queue still refuses",
			mode: config.LeaseGateQueue,
			reading: guarded(agentbus.Status{
				State: agentbus.StateNeedsAdopt,
				Queue: []agentbus.Waiter{{Handle: "h1"}, {Handle: "h2"}},
			}),
			reasons: []string{"/repo: lease needs-adopt (no holder, 2 queued)"},
		},
		{
			name: "missing agentbus under queue falls back with a note",
			mode: config.LeaseGateQueue,
			err:  missing,
			notes: []string{
				"/repo: lease not read (agentbus not installed: executable not found); used the herdr and git checks only (lease gate: queue)",
			},
		},
		{
			name:    "held by another agent under queue-required proceeds and queues",
			mode:    config.LeaseGateQueueRequired,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 1)),
			notes: []string{
				"/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (1 queued); start will queue behind holder (lease gate: queue-required)",
				"/repo: checkout checks skipped: lease held by pm/foo",
			},
		},
		{
			name: "paused under queue-required still refuses",
			mode: config.LeaseGateQueueRequired,
			reading: guarded(agentbus.Status{
				State:  agentbus.StatePaused,
				Holder: &agentbus.Holder{Handle: "user"},
			}),
			reasons: []string{"/repo: lease paused by user (0 queued)"},
		},
		{
			name: "needs-adopt under queue-required still refuses",
			mode: config.LeaseGateQueueRequired,
			reading: guarded(agentbus.Status{
				State: agentbus.StateNeedsAdopt,
				Queue: []agentbus.Waiter{{Handle: "h1"}},
			}),
			reasons: []string{"/repo: lease needs-adopt (no holder, 1 queued)"},
		},
		{
			name: "missing agentbus under queue-required refuses",
			mode: config.LeaseGateQueueRequired,
			err:  missing,
			reasons: []string{
				"/repo: lease not read (agentbus not installed: executable not found); lease gate is queue-required",
			},
		},
		{
			name:    "unguarded checkout under queue-required refuses",
			mode:    config.LeaseGateQueueRequired,
			reasons: []string{"/repo: lease not read (not a guarded checkout in agentbus lease_checkouts); lease gate is queue-required"},
		},
		{
			name:    "unknown mode is a reason",
			mode:    "maybe",
			reasons: []string{`lease.gate: expected auto, off, required, queue, or queue-required, found "maybe"`},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lease := &fakeLease{reading: tt.reading, err: tt.err}
			in := leaseInputs(tt.mode, lease)
			in.LeaseOwner = tt.owner
			res := Check(context.Background(), in, "/repo", leaseDefaults)
			if !slices.Equal(res.Reasons, tt.reasons) {
				t.Fatalf("reasons = %q, want %q", res.Reasons, tt.reasons)
			}
			if !slices.Equal(res.Notes, tt.notes) {
				t.Fatalf("notes = %q, want %q", res.Notes, tt.notes)
			}
			if res.OK != (len(tt.reasons) == 0) {
				t.Fatalf("ok = %t with reasons %q", res.OK, res.Reasons)
			}
			if res.LeaseGate != tt.mode {
				t.Fatalf("leaseGate = %q, want %q", res.LeaseGate, tt.mode)
			}
			if tt.mode == "maybe" {
				return
			}
			if !slices.Equal(lease.dirs, []string{"/repo"}) {
				t.Fatalf("lease reads = %q, want [/repo]", lease.dirs)
			}
		})
	}
}

func TestCheckLeaseOffNeverReads(t *testing.T) {
	for _, mode := range []string{"", config.LeaseGateOff} {
		lease := &fakeLease{err: errors.New("must not be called")}
		res := Check(context.Background(), leaseInputs(mode, lease), "/repo", leaseDefaults)
		if !res.OK || len(res.Notes) != 0 || len(lease.dirs) != 0 {
			t.Fatalf("mode %q: result = %#v, reads = %q", mode, res, lease.dirs)
		}
	}
}

func TestCheckLeaseNilReader(t *testing.T) {
	in := leaseInputs(config.LeaseGateAuto, &fakeLease{})
	in.Lease = nil
	res := Check(context.Background(), in, "/repo", leaseDefaults)
	if !slices.Equal(res.Reasons, []string{"agentbus lease: expected a reader, found nil"}) {
		t.Fatalf("reasons = %q", res.Reasons)
	}
}

func TestCheckLaneLease(t *testing.T) {
	lease := &fakeLease{reading: guarded(heldBy("pm/foo", "", 0))}
	in := leaseInputs(config.LeaseGateAuto, lease)
	got := CheckLane(context.Background(), in, Lane{Name: "linux", Checkout: "/repo", MaxWorkers: 2})
	want := "linux: blocked workers 0/2 (running), active 0: /repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (0 queued)"
	if got.Line() != want {
		t.Fatalf("line = %q, want %q", got.Line(), want)
	}
	if len(lease.dirs) != 1 {
		t.Fatalf("lease reads = %q, want one: the worker count and the lease check share it", lease.dirs)
	}

	lease = &fakeLease{err: agentbus.ErrMissing}
	got = CheckLane(context.Background(), leaseInputs(config.LeaseGateAuto, lease), Lane{Name: "linux", Checkout: "/repo", MaxWorkers: 2})
	want = "linux: OK workers 0/2 (running), active 0 (/repo: lease not read (agentbus not installed); used the herdr and git checks only)"
	if got.Line() != want {
		t.Fatalf("line = %q, want %q", got.Line(), want)
	}

	lease = &fakeLease{reading: guarded(heldBy("pm/foo", "", 0))}
	got = CheckLane(context.Background(), leaseInputs(config.LeaseGateQueue, lease), Lane{Name: "linux", Checkout: "/repo", MaxWorkers: 2})
	want = "linux: OK workers 0/2 (running), active 0 (/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (0 queued); start will queue behind holder (lease gate: queue); /repo: checkout checks skipped: lease held by pm/foo)"
	if got.Line() != want {
		t.Fatalf("line = %q, want %q", got.Line(), want)
	}
}

func TestCheckQueueSkipsCheckoutChecksWhileHeldByAnotherAgent(t *testing.T) {
	dirty := func(context.Context, string) (string, error) { return " M dirty.go\n", nil }
	wrongBranch := func(context.Context, string) (string, error) { return "feature", nil }
	ahead := func(context.Context, string, string) (int, error) { return 3, nil }

	tests := []struct {
		name         string
		mode         string
		reading      agentbus.Reading
		err          error
		owner        agentbus.Owner
		dirty        bool
		wantOK       bool
		wantReasons  []string
		wantNotes    []string
		wantGitCalls bool
	}{
		{
			name:    "queue held by another agent skips dirty branch and ahead checks",
			mode:    config.LeaseGateQueue,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 1)),
			dirty:   true,
			wantOK:  true,
			wantNotes: []string{
				"/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (1 queued); start will queue behind holder (lease gate: queue)",
				"/repo: checkout checks skipped: lease held by pm/foo",
			},
			wantGitCalls: false,
		},
		{
			name:    "queue-required held by another agent skips dirty branch and ahead checks",
			mode:    config.LeaseGateQueueRequired,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 0)),
			owner:   agentbus.Owner{ItemID: "PRJ-1", Agent: "prj-1"},
			dirty:   true,
			wantOK:  true,
			wantNotes: []string{
				"/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (0 queued); start will queue behind holder (lease gate: queue-required)",
				"/repo: checkout checks skipped: lease held by pm/foo",
			},
			wantGitCalls: false,
		},
		{
			name:    "queue held by another agent with empty registered name uses handle",
			mode:    config.LeaseGateQueue,
			reading: guarded(heldBy("", "PRJ-9 other work", 0)),
			owner:   agentbus.Owner{ItemID: "PRJ-1", Agent: "prj-1"},
			dirty:   true,
			wantOK:  true,
			wantNotes: []string{
				"/repo: lease held by herdr:local/default/w1:p2 until " + localClock(leaseDeadline) + " (0 queued); start will queue behind holder (lease gate: queue)",
				"/repo: checkout checks skipped: lease held by herdr:local/default/w1:p2",
			},
			wantGitCalls: false,
		},
		{
			name: "queue held without holder omits by clause",
			mode: config.LeaseGateQueue,
			reading: guarded(agentbus.Status{
				State: agentbus.StateHeld,
			}),
			dirty:  true,
			wantOK: true,
			wantNotes: []string{
				"/repo: lease held (no holder, 0 queued); start will queue behind holder (lease gate: queue)",
				"/repo: checkout checks skipped: lease held",
			},
			wantGitCalls: false,
		},
		{
			name:    "queue held by same agent runs checkout checks and refuses dirty tree",
			mode:    config.LeaseGateQueue,
			reading: guarded(heldBy("prj-1", "PRJ-1 Fix it", 0)),
			owner:   agentbus.Owner{ItemID: "PRJ-1", Agent: "prj-1"},
			dirty:   true,
			wantOK:  false,
			wantReasons: []string{
				"/repo has uncommitted changes (1 files)",
				"/repo: expected no commits beyond origin/main, found 3 on feature",
			},
			wantGitCalls: true,
		},
		{
			name:    "queue free runs checkout checks and refuses dirty tree",
			mode:    config.LeaseGateQueue,
			reading: guarded(agentbus.Status{State: agentbus.StateFree}),
			dirty:   true,
			wantOK:  false,
			wantReasons: []string{
				"/repo has uncommitted changes (1 files)",
				"/repo: expected no commits beyond origin/main, found 3 on feature",
			},
			wantGitCalls: true,
		},
		{
			name: "queue paused with clean tree still refuses",
			mode: config.LeaseGateQueue,
			reading: guarded(agentbus.Status{
				State:  agentbus.StatePaused,
				Holder: &agentbus.Holder{Handle: "user"},
			}),
			dirty:        false,
			wantOK:       false,
			wantReasons:  []string{"/repo: lease paused by user (0 queued)"},
			wantGitCalls: true,
		},
		{
			name: "queue needs-adopt with clean tree still refuses",
			mode: config.LeaseGateQueue,
			reading: guarded(agentbus.Status{
				State: agentbus.StateNeedsAdopt,
				Queue: []agentbus.Waiter{{Handle: "h1"}},
			}),
			dirty:        false,
			wantOK:       false,
			wantReasons:  []string{"/repo: lease needs-adopt (no holder, 1 queued)"},
			wantGitCalls: true,
		},
		{
			name:   "queue-required unreadable lease behaves as today",
			mode:   config.LeaseGateQueueRequired,
			err:    errors.New("agentbus down"),
			dirty:  true,
			wantOK: false,
			wantReasons: []string{
				"/repo has uncommitted changes (1 files)",
				"/repo: expected no commits beyond origin/main, found 3 on feature",
				"/repo: lease not read (agentbus down); lease gate is queue-required",
			},
			wantGitCalls: true,
		},
		{
			name:    "auto held by another agent with dirty tree still refuses",
			mode:    config.LeaseGateAuto,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 1)),
			dirty:   true,
			wantOK:  false,
			wantReasons: []string{
				"/repo has uncommitted changes (1 files)",
				"/repo: expected no commits beyond origin/main, found 3 on feature",
				"/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (1 queued)",
			},
			wantGitCalls: true,
		},
		{
			name:    "required held by another agent with dirty tree still refuses",
			mode:    config.LeaseGateRequired,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 1)),
			dirty:   true,
			wantOK:  false,
			wantReasons: []string{
				"/repo has uncommitted changes (1 files)",
				"/repo: expected no commits beyond origin/main, found 3 on feature",
				"/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (1 queued)",
			},
			wantGitCalls: true,
		},
		{
			name:    "off with dirty tree still refuses",
			mode:    config.LeaseGateOff,
			reading: guarded(heldBy("pm/foo", "PRJ-9 other work", 1)),
			dirty:   true,
			wantOK:  false,
			wantReasons: []string{
				"/repo has uncommitted changes (1 files)",
				"/repo: expected no commits beyond origin/main, found 3 on feature",
			},
			wantGitCalls: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			gitCalls := 0
			lease := &fakeLease{reading: tc.reading, err: tc.err}
			in := leaseInputs(tc.mode, lease)
			in.LeaseOwner = tc.owner
			in.BaseBranch = "main"
			in.Require = config.GateCleanAgainstBase
			in.GitStatus = func(ctx context.Context, dir string) (string, error) {
				gitCalls++
				if tc.dirty {
					return dirty(ctx, dir)
				}
				return "", nil
			}
			in.GitBranch = func(ctx context.Context, dir string) (string, error) {
				gitCalls++
				if tc.dirty {
					return wrongBranch(ctx, dir)
				}
				return "main", nil
			}
			in.GitAhead = func(ctx context.Context, dir, base string) (int, error) {
				gitCalls++
				if tc.dirty {
					return ahead(ctx, dir, base)
				}
				return 0, nil
			}

			res := Check(context.Background(), in, "/repo", leaseDefaults)
			if res.OK != tc.wantOK {
				t.Fatalf("ok = %t, want %t (reasons=%q, notes=%q)", res.OK, tc.wantOK, res.Reasons, res.Notes)
			}
			if !slices.Equal(res.Reasons, tc.wantReasons) {
				t.Fatalf("reasons = %q, want %q", res.Reasons, tc.wantReasons)
			}
			if !slices.Equal(res.Notes, tc.wantNotes) {
				t.Fatalf("notes = %q, want %q", res.Notes, tc.wantNotes)
			}
			if tc.wantGitCalls && gitCalls == 0 {
				t.Fatal("expected git checks to run, got 0 calls")
			}
			if !tc.wantGitCalls && gitCalls != 0 {
				t.Fatalf("expected git checks to be skipped, got %d calls", gitCalls)
			}
		})
	}
}

func TestCheckLaneQueueSkipsCheckoutChecksWhileHeld(t *testing.T) {
	gitCalls := 0
	lease := &fakeLease{reading: guarded(heldBy("pm/foo", "PRJ-9", 1))}
	in := leaseInputs(config.LeaseGateQueue, lease)
	in.BaseBranch = "main"
	in.Require = config.GateCleanAgainstBase
	in.GitStatus = func(context.Context, string) (string, error) {
		gitCalls++
		return " M dirty.go\n", nil
	}
	in.GitBranch = func(context.Context, string) (string, error) {
		gitCalls++
		return "feature", nil
	}
	in.GitAhead = func(context.Context, string, string) (int, error) {
		gitCalls++
		return 2, nil
	}

	got := CheckLane(context.Background(), in, Lane{Name: "linux", Checkout: "/repo", MaxWorkers: 2})
	want := "linux: OK workers 0/2 (running), active 0 (/repo: lease held by pm/foo until " + localClock(leaseDeadline) + " (1 queued); start will queue behind holder (lease gate: queue); /repo: checkout checks skipped: lease held by pm/foo)"
	if !got.OK || got.Line() != want {
		t.Fatalf("lane = %#v\nline = %q\nwant = %q", got, got.Line(), want)
	}
	if gitCalls != 0 {
		t.Fatalf("gitCalls = %d, want 0", gitCalls)
	}
}

func TestCheckLaneLeaseRemote(t *testing.T) {
	for _, mode := range []string{config.LeaseGateAuto, config.LeaseGateRequired, config.LeaseGateQueue, config.LeaseGateQueueRequired} {
		lease := &fakeLease{err: errors.New("must not be called")}
		got := CheckLane(context.Background(), leaseInputs(mode, lease), Lane{Name: "mac", Machine: "mac1", Checkout: "/on/mac", MaxWorkers: 2})
		want := "mac: OK workers 0/2 (running), active 0 (checkout and lease not checked on remote machine mac1)"
		if got.Line() != want || len(lease.dirs) != 0 {
			t.Fatalf("mode %s: line = %q, reads = %q", mode, got.Line(), lease.dirs)
		}
	}
}
