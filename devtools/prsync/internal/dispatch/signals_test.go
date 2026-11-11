package dispatch

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

// fakePanes answers `herdr pane process-info` per pane.
type fakePanes struct {
	info map[string]herdr.ProcessInfo
	err  error
}

func (f fakePanes) PaneProcessInfo(_ context.Context, pane string) (herdr.ProcessInfo, error) {
	if f.err != nil {
		return herdr.ProcessInfo{}, f.err
	}
	return f.info[pane], nil
}

// fakeProcs is a scripted process table. Each List call returns the
// next listing; the last one repeats.
type fakeProcs struct {
	mu    sync.Mutex
	lists [][]Process
	cwds  map[int]string
	err   error
	n     int
}

func (f *fakeProcs) List(context.Context) ([]Process, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.err != nil {
		return nil, f.err
	}
	i := min(f.n, len(f.lists)-1)
	f.n++
	return f.lists[i], nil
}

func (f *fakeProcs) Cwd(_ context.Context, pid int) (string, error) {
	cwd, ok := f.cwds[pid]
	if !ok {
		return "", fmt.Errorf("pid %d: no cwd", pid)
	}
	return cwd, nil
}

// fakeDecide is a scripted `decide list --json`. The last listing repeats.
type fakeDecide struct {
	mu    sync.Mutex
	lists [][]DecideItem
	err   error
	n     int
}

func (f *fakeDecide) Items(context.Context) ([]DecideItem, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.n++
	if f.err != nil {
		return nil, f.err
	}
	if len(f.lists) == 0 {
		return nil, nil
	}
	return f.lists[min(f.n-1, len(f.lists)-1)], nil
}

const agentPID = 100

// agentPane is pane process-info whose foreground agent is agentPID.
func agentPane(pane string) fakePanes {
	return fakePanes{info: map[string]herdr.ProcessInfo{pane: {
		PaneID: pane, ShellPID: 50, ForegroundProcessGroupID: agentPID,
		ForegroundProcesses: []herdr.ForegroundProcess{{PID: agentPID, Name: "claude", Argv: []string{"claude"}}},
	}}}
}

func proc(pid, ppid int, argv ...string) Process {
	return Process{PID: pid, PPID: ppid, Name: filepath.Base(argv[0]), Argv: argv, Age: time.Hour}
}

func baseProcs() []Process {
	return []Process{proc(1, 0, "launchd"), proc(50, 1, "zsh"), proc(agentPID, 50, "claude")}
}

func signalCfg(checkout string) config.Config {
	cfg := config.Defaults()
	cfg.Checkout = checkout
	return cfg
}

func gateWith(t *testing.T, p Probes, cfg config.Config, agents ...herdr.Agent) Result {
	t.Helper()
	ctx := WithProbes(context.Background(), p)
	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{agents}}, cfg, "", nil)
	if err != nil {
		t.Fatalf("GateCheck() unexpected error: %v", err)
	}
	return got
}

func TestSignalBackgroundJobHoldsIdleTab(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(),
		proc(4242, agentPID, "sh", "-c", "bazel build //..."),
	)}}
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs}, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if got.Safe || len(got.Busy) != 1 {
		t.Fatalf("result = %+v, want one busy tab", got)
	}
	b := got.Busy[0]
	if b.Signal != SignalBackgroundJob || b.TabID != "w3:t2" || b.PaneID != "w3:p2" || b.Status != "idle" {
		t.Fatalf("busy = %+v, want idle w3:t2 background-job", b)
	}
	if want := `pid 4242 (1h0m0s) sh -c "bazel build //..."`; b.Detail != want {
		t.Fatalf("detail = %q, want %q", b.Detail, want)
	}
}

func TestSignalBackgroundJobIgnoresHelpers(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(),
		proc(300, agentPID, "node", "/opt/servers/github-mcp-server/index.js"),
		proc(301, 300, "node", "worker.js"),
		proc(302, agentPID, "caffeinate", "-i", "-t", "300"),
		proc(303, agentPID, "gopls", "serve"),
	)}}
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs}, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want safe with only ignored helpers", got)
	}
}

func TestSignalBuildToolHoldsCheckout(t *testing.T) {
	t.Parallel()

	checkout := "/src/repo"
	procs := &fakeProcs{
		lists: [][]Process{append(baseProcs(), proc(5000, 1, "bazel", "build", "//..."))},
		cwds:  map[int]string{5000: checkout + "/sub"},
	}
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs}, signalCfg(checkout), idleAgent("w3:p2", "w3:t2"))
	if got.Safe || len(got.Busy) != 1 {
		t.Fatalf("result = %+v, want the checkout busy", got)
	}
	b := got.Busy[0]
	if b.Signal != SignalBuildTool || b.Checkout != checkout || b.TabID != "" || b.PaneID != "" {
		t.Fatalf("busy = %+v, want checkout-level build-tool", b)
	}
	if want := "pid 5000 bazel build //... (cwd /src/repo/sub)"; b.Detail != want {
		t.Fatalf("detail = %q, want %q", b.Detail, want)
	}

	procs.cwds[5000] = "/elsewhere"
	procs.n = 0
	got = gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs}, signalCfg(checkout), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want safe for a build outside the checkout", got)
	}
}

func TestSignalBuildServerDaemonIsNotBusy(t *testing.T) {
	t.Parallel()

	checkout := "/src/repo"
	server := []Process{
		proc(6000, 1, "java", "-Xmx4g", "-jar", "/cache/install/A-server.jar", "--workspace_directory="+checkout),
		proc(6001, 1, "bazel", "--workspace_directory="+checkout, "--max_idle_secs=10800"),
	}
	procs := &fakeProcs{
		lists: [][]Process{append(baseProcs(), server...)},
		cwds:  map[int]string{6000: checkout, 6001: checkout, 7000: checkout},
	}
	p := Probes{Panes: agentPane("w3:p2"), Procs: procs}
	got := gateWith(t, p, signalCfg(checkout), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want safe with only an idle build server", got)
	}

	procs.lists = [][]Process{append(append(baseProcs(), server...), proc(7000, 1, "bazel", "build", "//..."))}
	got = gateWith(t, p, signalCfg(checkout), idleAgent("w3:p2", "w3:t2"))
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalBuildTool || !strings.HasPrefix(got.Busy[0].Detail, "pid 7000 ") {
		t.Fatalf("result = %+v, want build-tool from the client pid 7000", got)
	}
}

func TestSignalBuildToolSkipsSelfAndAncestors(t *testing.T) {
	t.Parallel()

	checkout := "/src/repo"
	procs := &fakeProcs{
		lists: [][]Process{append(baseProcs(), proc(500, 1, "make", "watch"), proc(600, 500, "prsync", "dispatch", "--go"))},
		cwds:  map[int]string{500: checkout, 600: checkout},
	}
	got := gateWith(t, Probes{Procs: procs, Self: 600}, signalCfg(checkout), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want the make that runs prsync to be ignored", got)
	}
}

func TestSignalPendingQuestion(t *testing.T) {
	t.Parallel()

	decide := &fakeDecide{lists: [][]DecideItem{
		{{ID: "D-17", State: "answered", NotifyPane: "w3:p2"}, {ID: "D-18", State: "answered", NotifyPane: "w9:p9"}},
		{{ID: "D-18", State: "answered", NotifyPane: "w9:p9"}},
	}}
	p := Probes{Decide: decide}
	got := gateWith(t, p, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if got.Safe || len(got.Busy) != 1 {
		t.Fatalf("result = %+v, want the tab busy", got)
	}
	if b := got.Busy[0]; b.Signal != SignalPendingQuestion || b.Detail != "answered item D-17" || b.PaneID != "w3:p2" {
		t.Fatalf("busy = %+v, want pending-question answered item D-17", b)
	}

	got = gateWith(t, p, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want safe once D-17 is done", got)
	}
}

func TestSignalPendingQuestionOff(t *testing.T) {
	t.Parallel()

	decide := &fakeDecide{lists: [][]DecideItem{{{ID: "D-17", State: "open", NotifyPane: "w3:p2"}}}}
	cfg := signalCfg("")
	cfg.GatePendingQuestion = "off"
	got := gateWith(t, Probes{Decide: decide}, cfg, idleAgent("w3:p2", "w3:t2"))
	if !got.Safe || decide.n != 0 {
		t.Fatalf("result = %+v decide calls = %d, want safe without asking decide", got, decide.n)
	}
}

func TestSignalDecideMissingWarnsOnce(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{
		append(baseProcs(), proc(4242, agentPID, "sh", "-c", "sleep 60")),
		baseProcs(),
	}}
	p := Probes{Panes: agentPane("w3:p2"), Procs: procs, Decide: &fakeDecide{err: ErrDecideMissing}}
	buf, ctx := withLog(t)
	ctx = WithProbes(ctx, p)
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	cfg := signalCfg("")
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond

	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}
	got, err := GateCheck(ctx, h, cfg, "", nil)
	if err != nil {
		t.Fatal(err)
	}
	if got.Safe || got.Busy[0].Signal != SignalBackgroundJob {
		t.Fatalf("result = %+v, want background-job while decide is missing", got)
	}
	if _, err := Wait(ctx, h, cfg, "", nil, clock, &fakeSleeper{clock: clock}); err != nil {
		t.Fatalf("Wait() unexpected error: %v", err)
	}
	n := 0
	for _, rec := range logRecords(t, buf) {
		if rec["msg"] == "gate_decide_missing" {
			n++
		}
	}
	// GateCheck stopped at background-job and never asked decide; the
	// wait asked on every poll but warns once.
	if n != 1 {
		t.Fatalf("gate_decide_missing logged %d times, want once across the wait's polls", n)
	}
}

func TestSignalUnavailableIsReported(t *testing.T) {
	t.Parallel()

	p := Probes{
		Panes:  fakePanes{err: errors.New("pane gone")},
		Procs:  &fakeProcs{lists: [][]Process{baseProcs()}},
		Decide: &fakeDecide{err: errors.New("ledger locked")},
	}
	got := gateWith(t, p, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want a local tab to stay safe", got)
	}
	signals := make([]string, 0, len(got.SignalUnavailable))
	for _, u := range got.SignalUnavailable {
		if u.TabID != "w3:t2" || u.PaneID != "w3:p2" || u.Error == "" {
			t.Fatalf("signal_unavailable = %+v, want tab w3:t2 with an error", u)
		}
		signals = append(signals, u.Signal)
	}
	if strings.Join(signals, ",") != "background-job,pending-question" {
		t.Fatalf("signal_unavailable signals = %v", signals)
	}
}

func TestSignalsSkipRunnerPane(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), proc(4242, agentPID, "sh", "-c", "sleep 60"))}}
	ctx := WithProbes(context.Background(), Probes{Panes: agentPane("w3:p2"), Procs: procs})
	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}, signalCfg(""), "w3:p2", nil)
	if err != nil {
		t.Fatal(err)
	}
	if !got.Safe {
		t.Fatalf("result = %+v, want the runner pane excluded", got)
	}
}

func TestSignalWorkingStillReportsWorking(t *testing.T) {
	t.Parallel()

	got := gateWith(t, Probes{}, signalCfg(""), workingAgent("w3:p2", "w3:t2"))
	if got.Safe || got.Busy[0].Signal != SignalWorking || got.Busy[0].Detail != "" {
		t.Fatalf("result = %+v, want signal working", got)
	}
}

func TestCheckNonWorkingStatusesAreSafeWithProbes(t *testing.T) {
	t.Parallel()

	for _, status := range []string{"idle", "done", "unknown", "none"} {
		p := Probes{
			Panes:  agentPane("w2:pC"),
			Procs:  &fakeProcs{lists: [][]Process{baseProcs()}},
			Decide: &fakeDecide{},
		}
		got := gateWith(t, p, signalCfg("/src/repo"), seqAgent("w2:pC", "w2:tC", status, 0))
		if !got.Safe || len(got.SignalUnavailable) != 0 {
			t.Fatalf("status %s: result = %+v, want safe", status, got)
		}
	}
}

func TestWaitBackgroundJobExitsThenPasses(t *testing.T) {
	t.Parallel()

	job := append(baseProcs(), proc(4242, agentPID, "sh", "-c", "bazel build //..."))
	procs := &fakeProcs{lists: [][]Process{job, job, baseProcs()}}
	buf, ctx := withLog(t)
	ctx = WithProbes(ctx, Probes{Panes: agentPane("w3:p2"), Procs: procs})
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	cfg := signalCfg("")
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond

	got, err := Wait(ctx, h, cfg, "", nil, clock, &fakeSleeper{clock: clock})
	if err != nil {
		t.Fatalf("Wait() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("result = %+v, want safe after the job exits", got)
	}
	if h.n != 2+settleDebouncePolls {
		t.Fatalf("polls = %d, want %d (two busy, then the debounce)", h.n, 2+settleDebouncePolls)
	}
	holds := 0
	for _, rec := range logRecords(t, buf) {
		if rec["msg"] == "gate_hold" {
			holds++
			if want := `gate: busy tab w3:t2 (pane w3:p2): background-job pid 4242 (1h0m0s) sh -c "bazel build //..."`; rec["line"] != want {
				t.Fatalf("gate_hold line = %v, want %q", rec["line"], want)
			}
		}
	}
	if holds != 1 {
		t.Fatalf("gate_hold logged %d times, want once for an unchanged hold", holds)
	}
}

func TestWaitTimeoutReportsLastSignal(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), proc(4242, agentPID, "sh", "-c", "sleep 600"))}}
	ctx := WithProbes(context.Background(), Probes{Panes: agentPane("w3:p2"), Procs: procs})
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	cfg := signalCfg("")
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 3 * time.Millisecond

	got, err := Wait(ctx, h, cfg, "", nil, clock, &fakeSleeper{clock: clock})
	if !errors.Is(err, ErrTimeout) {
		t.Fatalf("Wait() error = %v, want ErrTimeout", err)
	}
	if len(got.Busy) != 1 || got.Busy[0].Signal != SignalBackgroundJob {
		t.Fatalf("result = %+v, want background-job", got)
	}
	detail := gateTimeoutDetail(got.Busy, nil, Candidate{Repo: "acme/widgets", Number: 7})
	if want := `waiting on w3:t2 — background-job pid 4242 (1h0m0s) sh -c "sleep 600"; not dispatching acme/widgets#7 until it clears`; detail != want {
		t.Fatalf("timeout detail = %q, want %q", detail, want)
	}
}

func TestHoldLineGolden(t *testing.T) {
	t.Parallel()

	tests := []struct {
		busy Busy
		want string
	}{
		{
			busy: Busy{TabID: "w3:t2", PaneID: "w3:p2", Status: "idle", Signal: SignalBackgroundJob, Detail: `pid 4242 (2m13s) sh -c "bazel build ..."`},
			want: `gate: busy tab w3:t2 (pane w3:p2): background-job pid 4242 (2m13s) sh -c "bazel build ..."`,
		},
		{
			busy: Busy{Checkout: "/src/repo", Signal: SignalBuildTool, Detail: "pid 5000 bazel build //... (cwd /src/repo)"},
			want: "gate: busy checkout /src/repo: build-tool pid 5000 bazel build //... (cwd /src/repo)",
		},
		{
			busy: Busy{TabID: "w3:t2", PaneID: "w3:p2", Status: "idle", Signal: SignalPendingQuestion, Detail: "open item D-17"},
			want: "gate: busy tab w3:t2 (pane w3:p2): pending-question open item D-17",
		},
		{
			busy: Busy{TabID: "w3:t2", PaneID: "w3:p2", Status: "working", Signal: SignalWorking},
			want: "gate: busy tab w3:t2 (pane w3:p2): working",
		},
	}
	for _, tt := range tests {
		if got := holdLine(tt.busy); got != tt.want {
			t.Errorf("holdLine(%+v) =\n  %q\nwant\n  %q", tt.busy, got, tt.want)
		}
	}
}

func TestCommandLine(t *testing.T) {
	t.Parallel()

	if got := commandLine(proc(1, 0, "sh", "-c", "echo 'hi'", "")); got != `sh -c "echo 'hi'" ""` {
		t.Fatalf("commandLine() = %q", got)
	}
	if got := commandLine(proc(1, 0, "echo", "it's")); got != "echo it's" {
		t.Fatalf("commandLine() = %q, want quotes without whitespace left alone", got)
	}
	long := commandLine(proc(1, 0, "sh", strings.Repeat("x", 300)))
	if len([]rune(long)) != maxCommandLine+3 || !strings.HasSuffix(long, "...") {
		t.Fatalf("commandLine() = %q, want cut to %d runes", long, maxCommandLine)
	}
}

func TestMatchProcPattern(t *testing.T) {
	t.Parallel()

	p := proc(1, 0, "/usr/bin/node", "/opt/x/github-mcp-server/index.js")
	tests := []struct {
		pattern string
		want    bool
	}{
		{"node", true},
		{"*mcp-server*", true},
		{"*/opt/x/*", true},
		{"nod", true},
		{"nodx", false},
		{"python*", false},
	}
	for _, tt := range tests {
		if got := matchProc(tt.pattern, p); got != tt.want {
			t.Errorf("matchProc(%q) = %v, want %v", tt.pattern, got, tt.want)
		}
	}
}
