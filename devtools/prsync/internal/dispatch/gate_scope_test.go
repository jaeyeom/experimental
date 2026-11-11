package dispatch

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

const scopeCheckout = "/src/repo"

func inDir(cwd string, a herdr.Agent) herdr.Agent {
	a.Cwd = cwd
	return a
}

// jobProcs is a process table where the idle agent has an old
// background child, pid 4242, whose cwd is childCwd.
func jobProcs(childCwd string) *fakeProcs {
	return &fakeProcs{
		lists: [][]Process{append(baseProcs(), proc(4242, agentPID, "sh", "-c", "sleep 600"))},
		cwds:  map[int]string{4242: childCwd},
	}
}

func TestScopeBackgroundJobOutsideCheckoutIsFree(t *testing.T) {
	t.Parallel()

	buf, ctx := withLog(t)
	ctx = WithProbes(ctx, Probes{Panes: agentPane("w3:p2"), Procs: jobProcs("/src/other")})
	agent := inDir("/src/other", idleAgent("w3:p2", "w3:t2"))
	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{agent}}}, signalCfg(scopeCheckout), "", nil)
	if err != nil {
		t.Fatalf("GateCheck() unexpected error: %v", err)
	}
	if !got.Safe || len(got.Busy) != 0 {
		t.Fatalf("result = %+v, want a job outside the checkout not to hold", got)
	}
	if !strings.Contains(buf.String(), `"msg":"gate_signal_out_of_checkout"`) || !strings.Contains(buf.String(), SignalBackgroundJob) {
		t.Fatalf("log = %s, want gate_signal_out_of_checkout for background-job", buf.String())
	}
}

func TestScopeBackgroundJobTabInsideCheckoutHolds(t *testing.T) {
	t.Parallel()

	for _, agent := range []herdr.Agent{
		inDir(scopeCheckout+"/sub", idleAgent("w3:p2", "w3:t2")),
		func() herdr.Agent {
			a := idleAgent("w3:p2", "w3:t2")
			a.Cwd, a.ForegroundCwd = "/src/other", scopeCheckout
			return a
		}(),
	} {
		got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: jobProcs("/tmp")}, signalCfg(scopeCheckout), agent)
		if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalBackgroundJob {
			t.Fatalf("agent %+v: result = %+v, want background-job to hold", agent, got)
		}
	}
}

func TestScopeBackgroundJobChildInsideCheckoutHolds(t *testing.T) {
	t.Parallel()

	agent := inDir("/src/other", idleAgent("w3:p2", "w3:t2"))
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: jobProcs(scopeCheckout + "/pkg")}, signalCfg(scopeCheckout), agent)
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalBackgroundJob {
		t.Fatalf("result = %+v, want a child in the checkout to hold", got)
	}
}

func TestScopeWorkingOutsideCheckoutStillHolds(t *testing.T) {
	t.Parallel()

	agent := inDir("/src/other", workingAgent("w3:p2", "w3:t2"))
	got := gateWith(t, Probes{}, signalCfg(scopeCheckout), agent)
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalWorking {
		t.Fatalf("result = %+v, want working outside the checkout to hold", got)
	}
}

func TestScopePendingQuestionOutsideCheckoutIsFree(t *testing.T) {
	t.Parallel()

	item := DecideItem{ID: "D-17", State: "answered", NotifyPane: "w3:p2"}
	p := Probes{Decide: &fakeDecide{lists: [][]DecideItem{{item}}}}
	outside := gateWith(t, p, signalCfg(scopeCheckout), inDir("/src/other", idleAgent("w3:p2", "w3:t2")))
	if !outside.Safe {
		t.Fatalf("result = %+v, want a question outside the checkout not to hold", outside)
	}
	inside := gateWith(t, p, signalCfg(scopeCheckout), inDir(scopeCheckout, idleAgent("w3:p2", "w3:t2")))
	if inside.Safe || inside.Busy[0].Signal != SignalPendingQuestion {
		t.Fatalf("result = %+v, want a question inside the checkout to hold", inside)
	}
}

func TestScopeRemoteOutsideTabIgnoresDecideFailure(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{agents: []herdr.Agent{onMachine("m2", inDir("/src/other", idleAgent("w3:p2", "w3:t2")))}}
	cfg := remoteCfg()
	cfg.Checkout = scopeCheckout
	got := gateWith(t, Probes{Decide: &fakeDecide{err: errors.New("decide down")}, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}, cfg)
	if !got.Safe || len(got.SignalUnavailable) != 0 {
		t.Fatalf("result = %+v, want a remote tab outside the checkout not held by pending-question", got)
	}
}

func TestScopeNoKnownCheckoutKeepsSignals(t *testing.T) {
	t.Parallel()

	agent := inDir("/src/other", idleAgent("w3:p2", "w3:t2"))
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: jobProcs("/src/other")}, signalCfg(""), agent)
	if got.Safe || got.Busy[0].Signal != SignalBackgroundJob {
		t.Fatalf("result = %+v, want background-job without a known checkout", got)
	}
	item := DecideItem{ID: "D-17", State: "answered", NotifyPane: "w3:p2"}
	got = gateWith(t, Probes{Decide: &fakeDecide{lists: [][]DecideItem{{item}}}}, signalCfg(""), agent)
	if got.Safe || got.Busy[0].Signal != SignalPendingQuestion {
		t.Fatalf("result = %+v, want pending-question without a known checkout", got)
	}
}

func TestScopeRemoteBackgroundJobUsesRemoteCwds(t *testing.T) {
	t.Parallel()

	info := func(childCwd string) herdr.ProcessInfo {
		return herdr.ProcessInfo{PaneID: "w3:p2", ForegroundProcessGroupID: agentPID, ForegroundProcesses: []herdr.ForegroundProcess{
			{PID: agentPID, Name: "claude", Argv: []string{"claude"}, Cwd: "/src/other"},
			{PID: 4242, Name: "sh", Argv: []string{"sh", "-c", "sleep 600"}, Cwd: childCwd},
		}}
	}
	cfg := remoteCfg()
	cfg.Checkout = scopeCheckout
	cfg.GateJobMinAge = time.Second
	for _, tc := range []struct {
		name     string
		tabCwd   string
		childCwd string
		wantSafe bool
	}{
		{"outside", "/src/other", "/src/other", true},
		{"tab inside", scopeCheckout, "/tmp", false},
		{"child inside", "/src/other", scopeCheckout + "/pkg", false},
	} {
		m2 := fakeRemote{
			agents: []herdr.Agent{onMachine("m2", inDir(tc.tabCwd, idleAgent("w3:p2", "w3:t2")))},
			info:   map[string]herdr.ProcessInfo{"w3:p2": info(tc.childCwd)},
		}
		p := Probes{Panes: fakePanes{}, Procs: &fakeProcs{lists: [][]Process{baseProcs()}}, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}
		ctx := WithProbes(context.Background(), p)
		got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{nil}}, cfg, "", nil)
		if err != nil {
			t.Fatalf("%s: GateCheck() unexpected error: %v", tc.name, err)
		}
		if got.Safe != tc.wantSafe {
			t.Fatalf("%s: result = %+v, want safe=%v", tc.name, got, tc.wantSafe)
		}
		if !tc.wantSafe && (got.Busy[0].Machine != "m2" || got.Busy[0].Signal != SignalBackgroundJob) {
			t.Fatalf("%s: busy = %+v, want m2 background-job", tc.name, got.Busy)
		}
	}
}
