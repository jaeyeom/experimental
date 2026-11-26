package gate

import (
	"context"
	"errors"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestCheckLaneCountsRunningWorkers(t *testing.T) {
	clean := func(context.Context, string) (string, error) { return "", nil }
	handedOff := store.Claim{ID: "PRJ-1", Lane: "linux", State: store.HandedOff, TabID: "w1:t1"}
	startedWorking := store.Claim{ID: "PRJ-2", Lane: "linux", State: store.Started, TabID: "w1:t2"}
	tests := []struct {
		name string
		in   Inputs
		lane Lane
		want string
	}{
		{
			name: "handed-off claim with no open tab leaves room",
			in:   Inputs{GitStatus: clean, ActiveClaims: []store.Claim{handedOff}},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 1},
			want: "linux: OK workers 0/1 (running), active 1",
		},
		{
			name: "started claim whose tab is working fills the lane",
			in: Inputs{
				GitStatus:    clean,
				ActiveClaims: []store.Claim{handedOff, startedWorking},
				Tabs:         []herdr.Tab{{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking}},
			},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 1},
			want: "linux: blocked workers 1/1 (running), active 2: 1 running workers on lane linux (max_workers 1)",
		},
		{
			name: "handed-off claim whose tab is still open counts",
			in: Inputs{
				GitStatus:    clean,
				ActiveClaims: []store.Claim{handedOff},
				Tabs:         []herdr.Tab{{ID: "w1:t1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusIdle}},
			},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 1},
			want: "linux: blocked workers 1/1 (running), active 1: 1 running workers on lane linux (max_workers 1)",
		},
		{
			name: "claims on another lane do not count",
			in:   Inputs{GitStatus: clean, ActiveClaims: []store.Claim{{ID: "PRJ-3", Lane: "mac", State: store.Started}}},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 1},
			want: "linux: OK workers 0/1 (running), active 0",
		},
		{
			name: "max_active counts handed-off claims",
			in: Inputs{GitStatus: clean, ActiveClaims: []store.Claim{
				handedOff, {ID: "PRJ-4", Lane: "linux", State: store.HandedOff},
			}},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 1, MaxActive: 2},
			want: "linux: blocked workers 0/1 (running), active 2/2: 2 active claims on lane linux (max_active 2)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := CheckLane(context.Background(), tc.in, tc.lane).Line(); got != tc.want {
				t.Fatalf("Line() = %q\nwant      %q", got, tc.want)
			}
		})
	}
}

func TestCheckLaneRemoteLeaseHolderCounts(t *testing.T) {
	claim := store.Claim{ID: "PRJ-5", Lane: "mac", State: store.HandedOff, TabID: "w1:t5"}
	held := agentbus.Status{
		Checkout: "mac1:/on/mac", State: agentbus.StateHeld,
		Holder: &agentbus.Holder{Name: "pm/foo", Why: "PRJ-5 fix review"},
	}
	other := agentbus.Status{Checkout: "local:/on/mac", State: agentbus.StateFree}
	calls := 0
	in := Inputs{
		LeaseGate:    config.LeaseGateAuto,
		ActiveClaims: []store.Claim{claim},
		Leases: func(context.Context) ([]agentbus.Status, error) {
			calls++
			return []agentbus.Status{other, held}, nil
		},
	}
	lane := Lane{Name: "mac", Machine: "mac1", Checkout: "/on/mac", MaxWorkers: 1}
	got := CheckLane(context.Background(), in, lane)
	if got.OK || got.Workers != 1 || calls != 1 {
		t.Fatalf("result = %+v, Leases calls = %d", got, calls)
	}

	in.LeaseGate = config.LeaseGateOff
	calls = 0
	if got := CheckLane(context.Background(), in, lane); !got.OK || got.Workers != 0 || calls != 0 {
		t.Fatalf("lease gate off: result = %+v, Leases calls = %d", got, calls)
	}

	in.LeaseGate = config.LeaseGateAuto
	in.Leases = func(context.Context) ([]agentbus.Status, error) { return nil, errors.New("hub down") }
	got = CheckLane(context.Background(), in, lane)
	want := "mac: OK workers 0/1 (running), active 1 (checkout and lease not checked on remote machine mac1; lease list not read (hub down))"
	if got.Line() != want {
		t.Fatalf("Line() = %q, want %q", got.Line(), want)
	}
}

func TestSkippedLaneCountsStartedConservatively(t *testing.T) {
	claims := []store.Claim{
		{ID: "PRJ-1", Lane: "mac", State: store.Started, TabID: "w1:t1"},
		{ID: "PRJ-2", Lane: "mac", State: store.HandedOff, TabID: "w1:t2"},
	}
	got := SkippedLane(Lane{Name: "mac", Machine: "mac1", MaxWorkers: 2}, claims, "host unreachable")
	if got.Workers != 1 || got.Active != 2 {
		t.Fatalf("result = %+v, want 1 running and 2 active", got)
	}
}
