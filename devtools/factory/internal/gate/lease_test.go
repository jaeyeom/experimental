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
			name:    "unknown mode is a reason",
			mode:    "maybe",
			reasons: []string{`lease.gate: expected auto, off, or required, found "maybe"`},
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
}

func TestCheckLaneLeaseRemote(t *testing.T) {
	for _, mode := range []string{config.LeaseGateAuto, config.LeaseGateRequired} {
		lease := &fakeLease{err: errors.New("must not be called")}
		got := CheckLane(context.Background(), leaseInputs(mode, lease), Lane{Name: "mac", Machine: "mac1", Checkout: "/on/mac", MaxWorkers: 2})
		want := "mac: OK workers 0/2 (running), active 0 (checkout and lease not checked on remote machine mac1)"
		if got.Line() != want || len(lease.dirs) != 0 {
			t.Fatalf("mode %s: line = %q, reads = %q", mode, got.Line(), lease.dirs)
		}
	}
}
