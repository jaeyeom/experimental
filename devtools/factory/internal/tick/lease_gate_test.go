package tick

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestTickLeaseQueueHeldProceedsAndReportsMode(t *testing.T) {
	deadline := "2026-10-31T14:20:00Z"
	parsed, err := time.Parse(time.RFC3339, deadline)
	if err != nil {
		t.Fatal(err)
	}
	wantClock := parsed.Local().Format("15:04")

	for _, mode := range []string{config.LeaseGateQueue, config.LeaseGateQueueRequired} {
		t.Run(mode, func(t *testing.T) {
			env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
			env.eng.Config.Lease.Gate = mode
			env.eng.Config.Sources[0].Lease = config.LeasePrompt{Mode: config.LeaseModePrompt, ClaimFor: "45m"}
			env.eng.Lease = func(context.Context, string) (agentbus.Reading, error) {
				return agentbus.Reading{
					Guarded: true,
					Status: agentbus.Status{
						Checkout: "local:/repo",
						State:    agentbus.StateHeld,
						Holder:   &agentbus.Holder{Handle: "herdr:local/default/w1:p2", Name: "pm/foo", Deadline: deadline, Why: "PRJ-9 other"},
						Queue:    []agentbus.Waiter{{Handle: "herdr:local/default/w1:p3"}},
					},
				}, nil
			}
			var logs bytes.Buffer
			restore := captureLog(t, &logs)
			t.Cleanup(restore)

			if err := env.eng.Tick(context.Background()); err != nil {
				t.Fatal(err)
			}

			cwd := env.eng.Config.Sources[0].Cwd
			wantNote := cwd + ": lease held by pm/foo until " + wantClock + " (1 queued); start will queue behind holder (lease gate: " + mode + ")"
			if !strings.Contains(env.out.String(), "note     "+wantNote+"\n") {
				t.Fatalf("stdout missing note %q:\n%s", wantNote, env.out.String())
			}
			if !strings.Contains(env.out.String(), "started PRJ-123 in factory/PRJ-123\n") {
				t.Fatalf("stdout missing started line:\n%s", env.out.String())
			}
			if !strings.Contains(logs.String(), "lease gate: "+mode) {
				t.Fatalf("logs missing mode %q:\n%s", mode, logs.String())
			}
			if got := env.src.claimIDs(); !slices.Equal(got, []string{"PRJ-123"}) {
				t.Fatalf("claims = %v, want [PRJ-123]", got)
			}
			run := tickRun(t, env.st)
			if run.Outcome != outcomeStarted {
				t.Fatalf("outcome = %s, want %s", run.Outcome, outcomeStarted)
			}
			var detail struct {
				Summary   string `json:"summary"`
				LeaseGate string `json:"lease_gate"` //nolint:tagliatelle // matches tick run detail JSON field
			}
			if err := json.Unmarshal([]byte(run.DetailJSON), &detail); err != nil {
				t.Fatalf("detail = %s: %v", run.DetailJSON, err)
			}
			if detail.LeaseGate != mode {
				t.Fatalf("detail.LeaseGate = %q, want %q (raw %s)", detail.LeaseGate, mode, run.DetailJSON)
			}
		})
	}
}

func TestTickLeaseQueueFreeProceeds(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.eng.Config.Lease.Gate = config.LeaseGateQueue
	env.eng.Lease = func(context.Context, string) (agentbus.Reading, error) {
		return agentbus.Reading{
			Guarded: true,
			Status:  agentbus.Status{Checkout: "local:/repo", State: agentbus.StateFree},
		}, nil
	}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(env.out.String(), "started PRJ-123 in factory/PRJ-123\n") {
		t.Fatalf("stdout = %q", env.out.String())
	}
	run := tickRun(t, env.st)
	if run.Outcome != outcomeStarted || !strings.Contains(run.DetailJSON, `"lease_gate":"queue"`) {
		t.Fatalf("run = %+v", run)
	}
}

func TestTickLeaseQueueRefusesPausedNeedsAdoptAndUnreadRequired(t *testing.T) {
	tests := []struct {
		name    string
		mode    string
		reading agentbus.Reading
		err     error
		want    string
	}{
		{
			name: "queue paused refuses",
			mode: config.LeaseGateQueue,
			reading: agentbus.Reading{
				Guarded: true,
				Status:  agentbus.Status{State: agentbus.StatePaused, Holder: &agentbus.Holder{Handle: "user"}},
			},
			want: ": lease paused by user (0 queued)",
		},
		{
			name: "queue needs-adopt refuses",
			mode: config.LeaseGateQueue,
			reading: agentbus.Reading{
				Guarded: true,
				Status:  agentbus.Status{State: agentbus.StateNeedsAdopt, Queue: []agentbus.Waiter{{Handle: "h1"}}},
			},
			want: ": lease needs-adopt (no holder, 1 queued)",
		},
		{
			name: "queue-required unreadable refuses",
			mode: config.LeaseGateQueueRequired,
			err:  errors.New("agentbus down"),
			want: ": lease not read (agentbus down); lease gate is queue-required",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
			env.eng.Config.Lease.Gate = tc.mode
			env.eng.Lease = func(context.Context, string) (agentbus.Reading, error) {
				return tc.reading, tc.err
			}
			if err := env.eng.Tick(context.Background()); err != nil {
				t.Fatal(err)
			}
			cwd := env.eng.Config.Sources[0].Cwd
			wantOut := "gate: " + cwd + tc.want + "\n"
			if env.out.String() != wantOut {
				t.Fatalf("stdout = %q, want %q", env.out.String(), wantOut)
			}
			if got := env.src.claimIDs(); len(got) != 0 {
				t.Fatalf("claims = %v", got)
			}
			assertNoClaim(t, env.st, "PRJ-123")
			run := tickRun(t, env.st)
			if run.Outcome != outcomeGated {
				t.Fatalf("outcome = %s", run.Outcome)
			}
			if !strings.Contains(run.DetailJSON, `"lease_gate":"`+tc.mode+`"`) {
				t.Fatalf("detail = %s, want lease_gate %q", run.DetailJSON, tc.mode)
			}
		})
	}
}

func TestTickLanesLeaseQueueHeldProceeds(t *testing.T) {
	deadline := "2026-10-31T14:20:00Z"
	env := newLaneEnv(t, []source.Item{labeled("PRJ-1", "needs-linux")}, true, 2)
	env.eng.Config.Lease.Gate = config.LeaseGateQueue
	env.eng.Lease = func(context.Context, string) (agentbus.Reading, error) {
		return agentbus.Reading{
			Guarded: true,
			Status: agentbus.Status{
				Checkout: "local:/repo",
				State:    agentbus.StateHeld,
				Holder:   &agentbus.Holder{Handle: "herdr:local/default/w1:p2", Name: "pm/foo", Deadline: deadline},
			},
		}, nil
	}
	var logs bytes.Buffer
	restore := captureLog(t, &logs)
	t.Cleanup(restore)

	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(env.out.String(), "started PRJ-1 in factory/PRJ-1 (lane linux)\n") {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if !strings.Contains(env.out.String(), "note     ") || !strings.Contains(env.out.String(), "lease gate: queue") {
		t.Fatalf("stdout missing queue note:\n%s", env.out.String())
	}
	if !strings.Contains(logs.String(), "lease gate: queue") {
		t.Fatalf("logs missing mode:\n%s", logs.String())
	}
}
