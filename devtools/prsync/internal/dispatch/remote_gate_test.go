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

// fakeRemote is one herdr machine for the gate.
type fakeRemote struct {
	agents  []herdr.Agent
	err     error
	info    map[string]herdr.ProcessInfo
	infoErr error
	tabs    []herdr.Tab
	calls   *[]string
}

func (f fakeRemote) note(call string) {
	if f.calls != nil {
		*f.calls = append(*f.calls, call)
	}
}

func (f fakeRemote) AgentList(context.Context) ([]herdr.Agent, error) {
	f.note("agent list")
	if f.err != nil {
		return nil, f.err
	}
	return f.agents, nil
}

func (f fakeRemote) PaneProcessInfo(_ context.Context, pane string) (herdr.ProcessInfo, error) {
	f.note("pane process-info " + pane)
	if f.infoErr != nil {
		return herdr.ProcessInfo{}, f.infoErr
	}
	info, ok := f.info[pane]
	if !ok {
		return herdr.ProcessInfo{}, errors.New("pane_not_found")
	}
	return info, nil
}

func (f fakeRemote) TabList(context.Context) ([]herdr.Tab, error) {
	f.note("tab list")
	return f.tabs, nil
}

func onMachine(m string, a herdr.Agent) herdr.Agent {
	a.Machine = m
	return a
}

// remoteCfg is a gate config with one other machine, m2.
func remoteCfg() config.Config {
	cfg := config.Defaults()
	cfg.Machines = []string{"m2"}
	cfg.MachineTimeout = 2 * time.Second
	return cfg
}

func remotes(m map[string]RemoteHerdr) func(string) RemoteHerdr {
	return func(machine string) RemoteHerdr { return m[machine] }
}

// fakeHerdrRemotes reaches each machine through the fake herdr on PATH.
var fakeHerdrRemotes = func(machine string) RemoteHerdr {
	return herdr.NewClient(executor.NewBasicExecutor(), "herdr").OnMachine(machine, 2*time.Second)
}

// remoteJobInfo is m2's pane process-info for w3:p2: claude (pid 100)
// leads the foreground group, and pid 4242 runs a build under it.
const remoteJobInfo = `{"result":{"process_info":{"pane_id":"w3:p2","shell_pid":50,"foreground_process_group_id":100,"foreground_processes":[` +
	`{"pid":100,"name":"claude","argv":["claude"]},` +
	`{"pid":4242,"name":"sh","argv":["sh","-c","bazel build //..."]}]}}}`

func TestGateRemoteWorkingAgentHoldsDispatchToItsMachine(t *testing.T) {
	f := herdrtest.Install(t)
	f.Write("local.agents", `{"result":{"agents":[]}}`)
	f.Write("m2.agents", `{"result":{"agents":[`+
		`{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"idle"},`+
		`{"pane_id":"w3:p9","tab_id":"w3:t9","agent":"claude","agent_status":"working"}]}}`)
	local := herdr.NewClient(executor.NewBasicExecutor(), "herdr")
	cfg, store := liveCfg(t)
	cfg.GateTimeout = 50 * time.Millisecond // this test waits for the gate to time out
	cfg.Machines = []string{"m2"}
	cfg.MachineTimeout = 2 * time.Second
	ctx := WithProbes(context.Background(), Probes{Remote: fakeHerdrRemotes})

	got, err := Run(ctx, local, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{remotePR()}},
		Remote: func(m string) Herdr { return local.OnMachine(m, 2*time.Second) },
	}, fixtureNow)
	if !errors.Is(err, ErrTimeout) {
		t.Fatalf("Run() error = %v, want gate timeout", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionGateTimeout {
		t.Fatalf("results = %+v, want gate_timeout", got.Results)
	}
	if d := got.Results[0].Detail; !strings.Contains(d, "waiting on w3:t9@m2 — working") {
		t.Fatalf("detail = %q, want the working m2 tab", d)
	}
	argv := strings.Join(f.Argv(), "\n")
	if !strings.Contains(argv, "--machine m2 agent list") || strings.Contains(argv, "agent prompt") {
		t.Fatalf("argv = %s, want m2's agent list read and no prompt", argv)
	}
}

func TestGateRemoteBackgroundJobUsesItsProcessInfo(t *testing.T) {
	f := herdrtest.Install(t)
	f.Write("m2.agents", `{"result":{"agents":[{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"claude","agent_status":"idle"}]}}`)
	f.Write("m2.process-info", remoteJobInfo)
	p := Probes{Panes: fakePanes{}, Procs: &fakeProcs{lists: [][]Process{baseProcs()}}, Remote: fakeHerdrRemotes}

	got := gateWith(t, p, remoteCfg())
	if got.Safe || len(got.Busy) != 1 {
		t.Fatalf("result = %+v, want one busy m2 tab", got)
	}
	b := got.Busy[0]
	if b.Machine != "m2" || b.TabID != "w3:t2" || b.Signal != SignalBackgroundJob || b.Detail != `pid 4242 sh -c "bazel build //..."` {
		t.Fatalf("busy = %+v, want m2 w3:t2 background-job pid 4242", b)
	}
	if argv := strings.Join(f.Argv(), "\n"); !strings.Contains(argv, "--machine m2 pane process-info --pane w3:p2") {
		t.Fatalf("argv = %s, want process-info on m2", argv)
	}
	if want := `gate: busy tab w3:t2@m2 (pane w3:p2): background-job pid 4242 sh -c "bazel build //..."`; holdLine(b) != want {
		t.Fatalf("holdLine = %q, want %q", holdLine(b), want)
	}
}

func TestGateRemoteUnreachableHoldsAndNamesTheCause(t *testing.T) {
	f := herdrtest.Install(t)
	f.Write("m2.down", "")
	buf, ctx := withLog(t)
	ctx = WithProbes(ctx, Probes{Remote: fakeHerdrRemotes})

	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{}}}, remoteCfg(), "", nil)
	if err != nil {
		t.Fatalf("GateCheck() unexpected error: %v", err)
	}
	if got.Safe || len(got.Busy) != 1 {
		t.Fatalf("result = %+v, want m2 to hold the gate", got)
	}
	b := got.Busy[0]
	if b.Machine != "m2" || b.Signal != SignalMachineUnreachable || b.Detail != remotecall.CauseHostUnreachable || b.TabID != "" {
		t.Fatalf("busy = %+v, want machine m2 unreachable (host unreachable)", b)
	}
	if want := "gate: busy machine m2: machine-unreachable host unreachable"; holdLine(b) != want {
		t.Fatalf("holdLine = %q, want %q", holdLine(b), want)
	}
	if line := gateWaitLine(got.Busy); line != "waiting on machine m2 — machine-unreachable host unreachable" {
		t.Fatalf("wait line = %q", line)
	}
	warned := false
	for _, rec := range logRecords(t, buf) {
		if rec["msg"] == "gate_machine_unreachable" && rec["machine"] == "m2" && rec["cause"] == remotecall.CauseHostUnreachable {
			warned = true
		}
	}
	if !warned {
		t.Fatalf("log = %s, want gate_machine_unreachable for m2", buf.String())
	}
}

func TestGateRemoteProcessInfoFailureHoldsTheTab(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{agents: []herdr.Agent{onMachine("m2", idleAgent("w3:p2", "w3:t2"))}, infoErr: errors.New("pane_not_found")}
	p := Probes{Panes: fakePanes{}, Procs: &fakeProcs{lists: [][]Process{baseProcs()}}, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}
	got := gateWith(t, p, remoteCfg())
	if got.Safe || len(got.Busy) != 1 {
		t.Fatalf("result = %+v, want the m2 tab held", got)
	}
	b := got.Busy[0]
	if b.Machine != "m2" || b.TabID != "w3:t2" || b.Signal != SignalUnavailableHold || !strings.Contains(b.Detail, "background-job") {
		t.Fatalf("busy = %+v, want signal-unavailable for background-job", b)
	}
	if len(got.SignalUnavailable) != 1 || got.SignalUnavailable[0].Machine != "m2" || got.SignalUnavailable[0].Signal != SignalBackgroundJob {
		t.Fatalf("signal_unavailable = %+v, want background-job on m2", got.SignalUnavailable)
	}
}

func TestGateRemoteEmptyProcessInfoIsUnavailable(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{
		agents: []herdr.Agent{onMachine("m2", idleAgent("w3:p2", "w3:t2"))},
		info:   map[string]herdr.ProcessInfo{"w3:p2": {PaneID: "w3:p2"}},
	}
	p := Probes{Panes: fakePanes{}, Procs: &fakeProcs{lists: [][]Process{baseProcs()}}, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}
	got := gateWith(t, p, remoteCfg())
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalUnavailableHold {
		t.Fatalf("result = %+v, want signal-unavailable", got)
	}
}

func TestGateRemoteIdleTabWithoutJobsIsFree(t *testing.T) {
	t.Parallel()

	info := herdr.ProcessInfo{PaneID: "w3:p2", ForegroundProcessGroupID: 100, ForegroundProcesses: []herdr.ForegroundProcess{{PID: 100, Name: "claude", Argv: []string{"claude"}}}}
	m2 := fakeRemote{agents: []herdr.Agent{onMachine("m2", idleAgent("w3:p2", "w3:t2"))}, info: map[string]herdr.ProcessInfo{"w3:p2": info}}
	p := Probes{Panes: fakePanes{}, Procs: &fakeProcs{lists: [][]Process{baseProcs()}}, Decide: &fakeDecide{}, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}
	got := gateWith(t, p, remoteCfg())
	if !got.Safe || len(got.SignalUnavailable) != 0 {
		t.Fatalf("result = %+v, want safe", got)
	}
}

func TestGateRemoteBlockedHolds(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{agents: []herdr.Agent{onMachine("m2", seqAgent("w3:p2", "w3:t2", "blocked", 0))}}
	got := gateWith(t, Probes{Remote: remotes(map[string]RemoteHerdr{"m2": m2})}, remoteCfg())
	if got.Safe || got.Busy[0].Signal != SignalBlocked || got.Busy[0].Machine != "m2" {
		t.Fatalf("result = %+v, want m2 blocked to hold", got)
	}
}

func TestGateRemotePendingQuestionMatchesRemoteHandle(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{agents: []herdr.Agent{onMachine("m2", idleAgent("w3:p2", "w3:t2"))}}
	decide := &fakeDecide{lists: [][]DecideItem{{
		{ID: "D-1", State: "answered", NotifyPane: "w3:p2"},
		{ID: "D-2", State: "answered", NotifyPane: "herdr:m3/s1/w3:p2"},
		{ID: "D-3", State: "answered", NotifyPane: "herdr:m2/s1/w3:p2"},
	}}}
	got := gateWith(t, Probes{Decide: decide, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}, remoteCfg())
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalPendingQuestion || got.Busy[0].Detail != "answered item D-3" {
		t.Fatalf("result = %+v, want pending-question D-3 on m2", got)
	}

	local := gateWith(t, Probes{Decide: &fakeDecide{lists: [][]DecideItem{{{ID: "D-3", State: "answered", NotifyPane: "herdr:m2/s1/w3:p2"}}}}}, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if !local.Safe {
		t.Fatalf("result = %+v, want a remote handle not to hold the local pane", local)
	}
}

func TestGateRemoteWithoutClientHolds(t *testing.T) {
	t.Parallel()

	got := gateWith(t, Probes{}, remoteCfg())
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Machine != "m2" || got.Busy[0].Signal != SignalMachineUnreachable {
		t.Fatalf("result = %+v, want m2 held without a client", got)
	}
}

func TestGateRemoteScope(t *testing.T) {
	t.Parallel()

	down := fakeRemote{err: &remotecall.UnreachableError{Machine: "m2", Cause: remotecall.CauseHostUnreachable}}
	tests := []struct {
		name    string
		mutate  func(*config.Config)
		matched map[string]struct{}
		safe    bool
	}{
		{"unscoped", func(*config.Config) {}, nil, false},
		{"tab machine local", func(c *config.Config) { c.TabMachine = config.LocalMachine }, nil, true},
		{"tab machine m2", func(c *config.Config) {
			c.TabMachine = "m2"
			c.MachineCheckouts = map[string]string{"m2": "/src/repo"}
		}, nil, false},
		{"managed without m2 tabs", func(c *config.Config) { c.ConcurrencyWaitOn = "managed" }, map[string]struct{}{"w2:tC": {}}, true},
		{"managed with an m2 tab", func(c *config.Config) { c.ConcurrencyWaitOn = "managed" }, map[string]struct{}{"w3:t2@m2": {}}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			var calls []string
			m2 := down
			m2.calls = &calls
			cfg := remoteCfg()
			tt.mutate(&cfg)
			ctx := WithProbes(context.Background(), Probes{Remote: remotes(map[string]RemoteHerdr{"m2": m2})})
			got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{}}}, cfg, "", tt.matched)
			if err != nil {
				t.Fatal(err)
			}
			if got.Safe != tt.safe {
				t.Fatalf("result = %+v, want safe=%v", got, tt.safe)
			}
			if tt.safe && len(calls) != 0 {
				t.Fatalf("calls = %v, want m2 not contacted", calls)
			}
		})
	}
}

func TestGateRemoteManagedWaitsOnlyOnMatchedRemoteTabs(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{agents: []herdr.Agent{
		onMachine("m2", workingAgent("w2:pC", "w2:tC")),
		onMachine("m2", workingAgent("w3:p2", "w3:t2")),
	}}
	cfg := remoteCfg()
	cfg.ConcurrencyWaitOn = "managed"
	ctx := WithProbes(context.Background(), Probes{Remote: remotes(map[string]RemoteHerdr{"m2": m2})})
	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w3:p2", "w3:t2")}}}, cfg, "", map[string]struct{}{"w3:t2@m2": {}})
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Busy) != 1 || got.Busy[0].Machine != "m2" || got.Busy[0].TabID != "w3:t2" {
		t.Fatalf("busy = %+v, want only w3:t2 on m2", got.Busy)
	}
}

func TestGateRemoteRunnerPaneIsLocalOnly(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{agents: []herdr.Agent{onMachine("m2", workingAgent("w3:p2", "w3:t2"))}}
	ctx := WithProbes(context.Background(), Probes{Remote: remotes(map[string]RemoteHerdr{"m2": m2})})
	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w3:p2", "w3:t2")}}}, remoteCfg(), "w3:p2", nil)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Busy) != 1 || got.Busy[0].Machine != "m2" {
		t.Fatalf("busy = %+v, want the m2 pane with the runner's id still waited on", got.Busy)
	}
}

func TestGateRemoteExemptTabUsesItsMachinesLabels(t *testing.T) {
	t.Parallel()

	m2 := fakeRemote{
		agents: []herdr.Agent{onMachine("m2", workingAgent("w3:p2", "w3:t2"))},
		tabs:   []herdr.Tab{{TabID: "w3:t2", Label: "pm", Machine: "m2"}},
	}
	cfg := remoteCfg()
	cfg.GateExemptTabs = []string{"pm*"}
	got := gateWith(t, Probes{Tabs: fakeTabs{}, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}, cfg)
	if !got.Safe || len(got.Exempt) != 1 || got.Exempt[0].Machine != "m2" || got.Exempt[0].Label != "pm" {
		t.Fatalf("result = %+v, want the m2 pm tab exempt", got)
	}
}

func TestWaitRemoteJobNeedsTwoSamples(t *testing.T) {
	t.Parallel()

	info := map[string]herdr.ProcessInfo{"w3:p2": {
		PaneID: "w3:p2", ForegroundProcessGroupID: agentPID,
		ForegroundProcesses: []herdr.ForegroundProcess{{PID: agentPID, Name: "claude"}, {PID: 4242, Name: "sh", Argv: []string{"sh"}}},
	}}
	m2 := fakeRemote{agents: []herdr.Agent{onMachine("m2", idleAgent("w3:p2", "w3:t2"))}, info: info}
	for _, tt := range []struct {
		timeout time.Duration
		safe    bool
	}{{0, true}, {3 * time.Millisecond, false}} {
		p := Probes{Panes: fakePanes{}, Procs: &fakeProcs{lists: [][]Process{baseProcs()}}, Remote: remotes(map[string]RemoteHerdr{"m2": m2})}
		ctx := WithProbes(context.Background(), p)
		clock := &fakeClock{now: time.Unix(0, 0).UTC()}
		cfg := remoteCfg()
		cfg.GatePoll = time.Millisecond
		cfg.GateTimeout = tt.timeout
		got, err := Wait(ctx, &scriptHerdr{lists: [][]herdr.Agent{{}}}, cfg, "", nil, clock, &fakeSleeper{clock: clock})
		if !errors.Is(err, ErrTimeout) {
			t.Fatalf("timeout %s: Wait() error = %v, want ErrTimeout", tt.timeout, err)
		}
		if got.Safe != tt.safe {
			t.Fatalf("timeout %s: result = %+v, want safe=%v", tt.timeout, got, tt.safe)
		}
	}
}
