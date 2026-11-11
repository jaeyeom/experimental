package dispatch

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	executor "github.com/jaeyeom/go-cmdexec"
)

// fakeLease is a scripted agentbus lease. It never runs agentbus.
type fakeLease struct {
	mu         sync.Mutex
	guarded    bool
	guardedErr error
	status     LeaseStatus
	statusErr  error
	guardedN   int
	statusN    int
}

func (f *fakeLease) Guarded(context.Context, string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.guardedN++
	return f.guarded, f.guardedErr
}

func (f *fakeLease) Status(context.Context, string) (LeaseStatus, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.statusN++
	return f.status, f.statusErr
}

const leaseCheckout = "/src/repo"

func leaseGate(t *testing.T, lease *fakeLease, mode string, agents ...herdr.Agent) Result {
	t.Helper()
	cfg := signalCfg(leaseCheckout)
	cfg.GateLease = mode
	return gateWith(t, Probes{Lease: lease}, cfg, agents...)
}

func TestLeaseFreeDoesNotHoldTheGate(t *testing.T) {
	t.Parallel()

	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}
	// The lease is authoritative: a working tab does not hold a guarded checkout.
	got := leaseGate(t, lease, "auto", workingAgent("w3:p2", "w3:t2"))
	if !got.Safe || len(got.Busy) != 0 || len(got.SignalUnavailable) != 0 {
		t.Fatalf("result = %+v, want safe", got)
	}
	if lease.statusN != 1 {
		t.Fatalf("status calls = %d, want 1", lease.statusN)
	}
}

func TestLeaseNotFreeHoldsTheGate(t *testing.T) {
	t.Parallel()

	deadline := "2026-11-10T14:20:00Z"
	clock := time.Date(2026, 11, 10, 14, 20, 0, 0, time.UTC).Local().Format("15:04")
	for _, tc := range []struct {
		name   string
		status LeaseStatus
		detail string
	}{
		{"held", LeaseStatus{State: "held", Holder: &LeaseHolder{Handle: "herdr:local/default/w1:p1", Name: "pm/foo", Deadline: deadline}}, "held by pm/foo until " + clock},
		{"held by a handle", LeaseStatus{State: "held", Holder: &LeaseHolder{Handle: "herdr:local/default/w1:p1"}}, "held by herdr:local/default/w1:p1"},
		{"paused", LeaseStatus{State: "paused", Holder: &LeaseHolder{Handle: "user"}}, "paused by user"},
		{"needs-adopt", LeaseStatus{State: "needs-adopt", Holder: &LeaseHolder{Handle: "h", Name: "pm/bar"}}, "needs-adopt by pm/bar"},
		{"needs-adopt without holder", LeaseStatus{State: "needs-adopt"}, "needs-adopt"},
		{"releasing", LeaseStatus{State: "releasing", Holder: &LeaseHolder{Handle: "h", Name: "pm/foo"}}, "releasing by pm/foo"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := leaseGate(t, &fakeLease{guarded: true, status: tc.status}, "auto", idleAgent("w3:p2", "w3:t2"))
			if got.Safe || len(got.Busy) != 1 {
				t.Fatalf("result = %+v, want one lease hold", got)
			}
			want := Busy{Checkout: leaseCheckout, Signal: SignalLease, Detail: tc.detail}
			if got.Busy[0] != want {
				t.Fatalf("busy = %+v, want %+v", got.Busy[0], want)
			}
		})
	}
}

func TestLeaseHoldLine(t *testing.T) {
	t.Parallel()

	b := Busy{Checkout: leaseCheckout, Signal: SignalLease, Detail: "paused by user"}
	if got, want := holdLine(b), "gate: busy checkout /src/repo: lease paused by user"; got != want {
		t.Fatalf("holdLine = %q, want %q", got, want)
	}
	if got, want := gateWaitLine([]Busy{b}), "waiting on checkout /src/repo — lease paused by user"; got != want {
		t.Fatalf("gateWaitLine = %q, want %q", got, want)
	}
}

func TestLeaseUnreadableUnderAutoFallsBack(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name  string
		lease *fakeLease
	}{
		{"agentbus missing", &fakeLease{guardedErr: ErrAgentbusMissing}},
		{"hub unreachable", &fakeLease{guarded: true, statusErr: errors.New("host unreachable")}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := leaseGate(t, tc.lease, "auto", workingAgent("w3:p2", "w3:t2"))
			if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalWorking {
				t.Fatalf("result = %+v, want the working tab to hold", got)
			}
			if len(got.SignalUnavailable) != 1 || got.SignalUnavailable[0].Signal != SignalLease || got.SignalUnavailable[0].Checkout != leaseCheckout {
				t.Fatalf("unavailable = %+v, want lease", got.SignalUnavailable)
			}

			got = leaseGate(t, tc.lease, "auto", idleAgent("w3:p2", "w3:t2"))
			if !got.Safe || len(got.SignalUnavailable) != 1 {
				t.Fatalf("result = %+v, want safe with lease unavailable", got)
			}
		})
	}
}

func TestLeaseUnreadableUnderRequiredIsUnsafe(t *testing.T) {
	t.Parallel()

	for _, lease := range []*fakeLease{
		{guardedErr: ErrAgentbusMissing},
		{guarded: true, statusErr: errors.New("host unreachable")},
	} {
		got := leaseGate(t, lease, "required", idleAgent("w3:p2", "w3:t2"))
		if got.Safe || len(got.Busy) != 1 {
			t.Fatalf("result = %+v, want one lease hold", got)
		}
		b := got.Busy[0]
		if b.Signal != SignalLease || b.Checkout != leaseCheckout || !strings.HasPrefix(b.Detail, "cannot read the lease: ") {
			t.Fatalf("busy = %+v, want a lease hold naming the error", b)
		}
	}
}

func TestLeaseUnguardedCheckoutMakesNoLeaseCall(t *testing.T) {
	t.Parallel()

	for _, mode := range []string{"auto", "required"} {
		lease := &fakeLease{status: LeaseStatus{State: "held"}}
		got := leaseGate(t, lease, mode, workingAgent("w3:p2", "w3:t2"))
		if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalWorking || len(got.SignalUnavailable) != 0 {
			t.Fatalf("%s: result = %+v, want the old signals only", mode, got)
		}
		if lease.guardedN != 1 || lease.statusN != 0 {
			t.Fatalf("%s: guarded calls = %d status calls = %d, want 1 and 0", mode, lease.guardedN, lease.statusN)
		}
	}
}

func TestLeaseOffAndNoCheckoutAskNothing(t *testing.T) {
	t.Parallel()

	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "held"}}
	if got := leaseGate(t, lease, "off", idleAgent("w3:p2", "w3:t2")); !got.Safe {
		t.Fatalf("off: result = %+v, want safe", got)
	}
	cfg := signalCfg("")
	if got := gateWith(t, Probes{Lease: lease}, cfg, idleAgent("w3:p2", "w3:t2")); !got.Safe {
		t.Fatalf("no checkout: result = %+v, want safe", got)
	}
	if lease.guardedN != 0 || lease.statusN != 0 {
		t.Fatalf("calls = %d/%d, want none", lease.guardedN, lease.statusN)
	}
}

func TestWaitAsksWhetherGuardedOnce(t *testing.T) {
	t.Parallel()

	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}
	ctx := WithProbes(context.Background(), Probes{Lease: lease})
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	cfg := signalCfg(leaseCheckout)
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = time.Second
	got, err := Wait(ctx, h, cfg, "", nil, clock, &fakeSleeper{clock: clock})
	if err != nil || !got.Safe {
		t.Fatalf("Wait() = %+v, %v; want safe", got, err)
	}
	if lease.guardedN != 1 || lease.statusN != settleDebouncePolls {
		t.Fatalf("guarded calls = %d status calls = %d, want 1 and %d", lease.guardedN, lease.statusN, settleDebouncePolls)
	}
}

func TestAgentbusLease(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--guarded", "--json", "--path", "/src/repo").
		WillSucceed(`{"guarded": true, "checkout": "local:/src/repo"}`, 0).Build()
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--json", "--path", "/src/repo").
		WillSucceed(`{"checkout": "local:/src/repo", "state": "held", "holder": {"handle": "h", "name": "pm/foo", "since": "2026-11-10T09:00:00Z", "deadline": "2026-11-10T09:40:00Z"}, "queue": [], "takeover": "log"}`, 0).Build()
	a := AgentbusLease{Exec: mock, Bin: "agentbus"}
	guarded, err := a.Guarded(context.Background(), "/src/repo")
	if err != nil || !guarded {
		t.Fatalf("Guarded() = %v, %v; want true", guarded, err)
	}
	st, err := a.Status(context.Background(), "/src/repo")
	if err != nil || st.State != "held" || st.Holder == nil || st.Holder.Name != "pm/foo" || st.Holder.Deadline != "2026-11-10T09:40:00Z" {
		t.Fatalf("Status() = %+v, %v", st, err)
	}
}

func TestAgentbusLeaseUnguarded(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--guarded", "--json", "--path", "/src/repo").
		WillSucceed(`{"guarded": false}`, 0).Build()
	guarded, err := AgentbusLease{Exec: mock, Bin: "agentbus"}.Guarded(context.Background(), "/src/repo")
	if err != nil || guarded {
		t.Fatalf("Guarded() = %v, %v; want false", guarded, err)
	}
}

func TestAgentbusLeaseMissingAndFailing(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--guarded", "--json", "--path", "/src/repo").
		WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Build()
	if _, err := (AgentbusLease{Exec: mock, Bin: "agentbus"}).Guarded(context.Background(), "/src/repo"); !errors.Is(err, ErrAgentbusMissing) {
		t.Fatalf("Guarded() error = %v, want ErrAgentbusMissing", err)
	}

	mock = executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--json", "--path", "/src/repo").
		WillFail("agentbus lease status: host unreachable", 1).Build()
	_, err := AgentbusLease{Exec: mock, Bin: "agentbus"}.Status(context.Background(), "/src/repo")
	if err == nil || errors.Is(err, ErrAgentbusMissing) || !strings.Contains(err.Error(), "host unreachable") {
		t.Fatalf("Status() error = %v, want the agentbus failure", err)
	}
}
