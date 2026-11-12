package dispatch

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	macCheckout = "/Users/me/src/repo"
	macLeaseKey = "mac:" + macCheckout
)

// macCfg is a gate config scoped to tab_machine = mac, whose checkout
// is macCheckout.
func macCfg() config.Config {
	cfg := config.Defaults()
	cfg.Machines = []string{"mac"}
	cfg.MachineTimeout = 2 * time.Second
	cfg.TabMachine = "mac"
	cfg.MachineCheckouts = map[string]string{"mac": macCheckout}
	return cfg
}

func agentIn(a herdr.Agent, cwd string) herdr.Agent {
	a.Machine = "mac"
	a.Cwd = cwd
	return a
}

// macGate runs one gate check for macCfg with mac's herdr and lease.
// The local herdr lists a working agent, which a remote gate never reads.
func macGate(t *testing.T, cfg config.Config, mac RemoteHerdr, lease LeaseSource) (Result, error) {
	t.Helper()
	p := Probes{Remote: remotes(map[string]RemoteHerdr{"mac": mac})}
	if lease != nil {
		p.Lease = lease
	}
	ctx := WithProbes(context.Background(), p)
	return GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w1:p1", "w1:t1")}}}, cfg, "", nil)
}

func TestRemoteGateLeaseHeldIsBusyAndNamesTheHolder(t *testing.T) {
	t.Parallel()

	lease := &fakeLease{all: []LeaseStatus{
		{Checkout: "local:" + macCheckout, State: "free"},
		{Checkout: macLeaseKey, State: "held", Holder: &LeaseHolder{Handle: "h", Name: "pm/foo"}},
	}}
	got, err := macGate(t, macCfg(), fakeRemote{}, lease)
	if err != nil {
		t.Fatal(err)
	}
	if got.Safe || len(got.Busy) != 1 {
		t.Fatalf("result = %+v, want the lease to hold", got)
	}
	b := got.Busy[0]
	if b.Signal != SignalLease || b.Detail != "held by pm/foo" || b.Checkout != macCheckout || b.Machine != "mac" {
		t.Fatalf("busy = %+v, want mac's lease held by pm/foo", b)
	}
	if got.Machine != "mac" || got.LeaseKey != macLeaseKey {
		t.Fatalf("machine = %q lease_key = %q, want mac and %s", got.Machine, got.LeaseKey, macLeaseKey)
	}
	if lease.allN != 1 || lease.guardedN != 0 || lease.statusN != 0 {
		t.Fatalf("lease calls all=%d guarded=%d status=%d, want only --all", lease.allN, lease.guardedN, lease.statusN)
	}
}

func TestRemoteGateLeaseFreeIsSafe(t *testing.T) {
	t.Parallel()

	lease := &fakeLease{all: []LeaseStatus{{Checkout: macLeaseKey, State: "free"}}}
	// The lease decides alone: a working mac agent does not hold a free guarded checkout.
	mac := fakeRemote{agents: []herdr.Agent{agentIn(workingAgent("w3:p2", "w3:t2"), macCheckout)}}
	got, err := macGate(t, macCfg(), mac, lease)
	if err != nil {
		t.Fatal(err)
	}
	if !got.Safe || len(got.Busy) != 0 || got.Reason != "" {
		t.Fatalf("result = %+v, want safe", got)
	}
}

func TestRemoteGateMissingLeaseEntryUsesTabSignals(t *testing.T) {
	t.Parallel()

	lease := &fakeLease{all: []LeaseStatus{{Checkout: "local:" + macCheckout, State: "held"}}}
	mac := fakeRemote{agents: []herdr.Agent{agentIn(workingAgent("w3:p2", "w3:t2"), macCheckout)}}
	got, err := macGate(t, macCfg(), mac, lease)
	if err != nil {
		t.Fatal(err)
	}
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalWorking || got.Busy[0].Machine != "mac" {
		t.Fatalf("result = %+v, want mac's working tab to hold an unguarded checkout", got)
	}
}

func TestRemoteGateLeaseUnreadable(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		mode string
		safe bool
	}{{"auto", true}, {"required", false}} {
		cfg := macCfg()
		cfg.GateLease = tc.mode
		lease := &fakeLease{allErr: errors.New("hub unreachable")}
		got, err := macGate(t, cfg, fakeRemote{}, lease)
		if err != nil {
			t.Fatal(err)
		}
		if got.Safe != tc.safe || len(got.SignalUnavailable) != 1 || got.SignalUnavailable[0].Signal != SignalLease {
			t.Fatalf("gate_lease=%s: result = %+v, want safe=%v and lease unavailable", tc.mode, got, tc.safe)
		}
	}
}

func TestRemoteGateLeaseOffNeverAsks(t *testing.T) {
	t.Parallel()

	cfg := macCfg()
	cfg.GateLease = "off"
	lease := &fakeLease{allErr: errors.New("must not be called")}
	if _, err := macGate(t, cfg, fakeRemote{}, lease); err != nil {
		t.Fatal(err)
	}
	if lease.allN != 0 {
		t.Fatalf("all calls = %d, want none", lease.allN)
	}
}

func TestRemoteGateWithoutMachineCheckoutFails(t *testing.T) {
	t.Parallel()

	cfg := macCfg()
	cfg.MachineCheckouts = nil
	cfg.Checkout = "/local/repo"
	_, err := macGate(t, cfg, fakeRemote{}, nil)
	var keyErr *config.KeyError
	if !errors.As(err, &keyErr) || keyErr.Key != "machine_checkouts" || !strings.Contains(err.Error(), "mac") {
		t.Fatalf("GateCheck() error = %v, want a machine_checkouts error naming mac", err)
	}
}

func TestRemoteGateCheckoutScope(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name string
		cwd  string
		safe bool
	}{
		{"inside", macCheckout + "/pkg", false},
		{"outside", "/Users/me/src/other", true},
	} {
		cfg := macCfg()
		cfg.ConcurrencyCwd = "checkout"
		mac := fakeRemote{agents: []herdr.Agent{agentIn(workingAgent("w3:p2", "w3:t2"), tc.cwd)}}
		got, err := macGate(t, cfg, mac, nil)
		if err != nil {
			t.Fatal(err)
		}
		if got.Safe != tc.safe {
			t.Fatalf("%s: result = %+v, want safe=%v", tc.name, got, tc.safe)
		}
		if !tc.safe && (got.Busy[0].Machine != "mac" || got.Busy[0].Signal != SignalWorking) {
			t.Fatalf("%s: busy = %+v, want mac's working tab", tc.name, got.Busy)
		}
	}
}

func TestRemoteGateUnreachableIsUnsafe(t *testing.T) {
	t.Parallel()

	down := fakeRemote{err: &remotecall.UnreachableError{Machine: "mac", Cause: remotecall.CauseHostUnreachable}}
	for _, lease := range []*fakeLease{nil, {all: []LeaseStatus{{Checkout: macLeaseKey, State: "free"}}}} {
		var src LeaseSource
		if lease != nil {
			src = lease
		}
		got, err := macGate(t, macCfg(), down, src)
		if err != nil {
			t.Fatal(err)
		}
		if got.Safe || got.Reason != "machine mac unreachable" {
			t.Fatalf("lease %v: result = %+v, want unsafe with machine mac unreachable", lease != nil, got)
		}
	}
}

func TestRemoteGateSkipsLocalChecksWithANote(t *testing.T) {
	t.Parallel()

	got, err := macGate(t, macCfg(), fakeRemote{}, nil)
	if err != nil {
		t.Fatal(err)
	}
	if !got.Safe || len(got.Busy) != 0 {
		t.Fatalf("result = %+v, want safe: the local working agent is not read", got)
	}
	if len(got.Notes) != 1 || got.Notes[0] != "checkout not inspected on mac" {
		t.Fatalf("notes = %q, want checkout not inspected on mac", got.Notes)
	}
	if got.Machine != "mac" || got.LeaseKey != macLeaseKey {
		t.Fatalf("machine = %q lease_key = %q", got.Machine, got.LeaseKey)
	}
}

func TestLocalGateUnchanged(t *testing.T) {
	t.Parallel()

	cfg := macCfg()
	cfg.TabMachine = config.LocalMachine
	cfg.Checkout = leaseCheckout
	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "held", Holder: &LeaseHolder{Handle: "h"}}}
	got, err := macGate(t, cfg, fakeRemote{}, lease)
	if err != nil {
		t.Fatal(err)
	}
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Checkout != leaseCheckout || got.Busy[0].Machine != "" {
		t.Fatalf("result = %+v, want the local lease to hold", got)
	}
	if got.Machine != "" || got.LeaseKey != "" || len(got.Notes) != 0 || lease.allN != 0 || lease.statusN != 1 {
		t.Fatalf("result = %+v (all=%d status=%d), want the local gate unchanged", got, lease.allN, lease.statusN)
	}
}

func TestRunWithTabMachineGatesOnItsCheckout(t *testing.T) {
	f := herdrtest.Install(t)
	f.Write("local.agents", `{"result":{"agents":[]}}`)
	f.Write("mac.agents", `{"result":{"agents":[{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"idle"}]}}`)
	local := herdr.NewClient(executor.NewBasicExecutor(), "herdr")
	cfg, store := liveCfg(t)
	cfg.Machines = []string{"mac"}
	cfg.MachineTimeout = 2 * time.Second
	cfg.TabMachine = "mac"
	cfg.MachineCheckouts = map[string]string{"mac": macCheckout}
	lease := &fakeLease{all: []LeaseStatus{{Checkout: macLeaseKey, State: "paused", Holder: &LeaseHolder{Handle: "user"}}}}
	ctx := WithProbes(context.Background(), Probes{Remote: fakeHerdrRemotes, Lease: lease})
	pr := remotePR()
	pr.Tab.Machine = "mac"

	got, err := Run(ctx, local, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Remote: func(m string) Herdr { return local.OnMachine(m, 2*time.Second) },
	}, fixtureNow)
	if !errors.Is(err, ErrTimeout) {
		t.Fatalf("Run() error = %v, want gate timeout", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionGateTimeout || !strings.Contains(got.Results[0].Detail, "paused by user") {
		t.Fatalf("results = %+v, want gate_timeout on mac's paused lease", got.Results)
	}
	if lease.allN == 0 || lease.statusN != 0 {
		t.Fatalf("lease calls all=%d status=%d, want mac's hub entry read", lease.allN, lease.statusN)
	}
	if argv := strings.Join(f.Argv(), "\n"); !strings.Contains(argv, "--machine mac agent list") {
		t.Fatalf("argv = %s, want mac's agents read", argv)
	}

	f.Write("mac.down", "")
	got, err = Run(ctx, local, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Remote: func(m string) Herdr { return local.OnMachine(m, 2*time.Second) },
	}, fixtureNow)
	if !errors.Is(err, ErrDirtyTree) || len(got.Results) != 1 || got.Results[0].Action != ActionGateUnsafe || got.Results[0].Detail != "machine mac unreachable" {
		t.Fatalf("Run() = %+v, %v; want gate_unsafe machine mac unreachable", got.Results, err)
	}
}
