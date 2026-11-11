package dispatch

import (
	"context"
	"strconv"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

// aged is p with Age d.
func aged(p Process, d time.Duration) Process {
	p.Age = d
	return p
}

func TestBackgroundJobNeedsMinAge(t *testing.T) {
	t.Parallel()

	young := &fakeProcs{lists: [][]Process{append(baseProcs(), aged(proc(4242, agentPID, "sh", "-c", "sleep 60"), 500*time.Millisecond))}}
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: young}, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want a 0.5s child not to count", got)
	}

	old := &fakeProcs{lists: [][]Process{append(baseProcs(), aged(proc(4242, agentPID, "sh", "-c", "sleep 60"), 30*time.Second))}}
	got = gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: old}, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalBackgroundJob {
		t.Fatalf("result = %+v, want a 30s child to hold", got)
	}
	if want := `pid 4242 (30s) sh -c "sleep 60"`; got.Busy[0].Detail != want {
		t.Fatalf("detail = %q, want %q", got.Busy[0].Detail, want)
	}
}

func TestBackgroundJobMinAgeIsConfigurable(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), aged(proc(4242, agentPID, "sh", "-c", "sleep 60"), 30*time.Second))}}
	cfg := signalCfg("")
	cfg.GateJobMinAge = time.Minute
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs}, cfg, idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want a 30s child under a 1m minimum not to count", got)
	}
}

func TestBackgroundJobInWaitLoopNeedsTwoSamples(t *testing.T) {
	t.Parallel()

	job := aged(proc(4242, agentPID, "sh", "-c", "sleep 60"), 30*time.Second)
	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), job)}}
	ctx := WithProbes(context.Background(), Probes{Panes: agentPane("w3:p2"), Procs: procs})
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}
	policy := gatePolicyFrom(signalCfg(""), "", nil)
	run := &gateRun{waitLoop: true}

	first, err := snapshot(ctx, h, policy, run)
	if err != nil {
		t.Fatal(err)
	}
	if !first.Safe {
		t.Fatalf("first sample = %+v, want a job seen once not to count yet", first)
	}
	second, err := snapshot(ctx, h, policy, run)
	if err != nil {
		t.Fatal(err)
	}
	if second.Safe || second.Busy[0].Signal != SignalBackgroundJob {
		t.Fatalf("second sample = %+v, want the job seen twice to hold", second)
	}
}

func TestBackgroundJobRespawnedPidDoesNotCarryOver(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{
		append(baseProcs(), aged(proc(4242, agentPID, "sh", "-c", "x"), 30*time.Second)),
		append(baseProcs(), aged(proc(4243, agentPID, "sh", "-c", "x"), 30*time.Second)),
	}}
	ctx := WithProbes(context.Background(), Probes{Panes: agentPane("w3:p2"), Procs: procs})
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}
	policy := gatePolicyFrom(signalCfg(""), "", nil)
	run := &gateRun{waitLoop: true}
	for i := range 2 {
		got, err := snapshot(ctx, h, policy, run)
		if err != nil {
			t.Fatal(err)
		}
		if !got.Safe {
			t.Fatalf("sample %d = %+v, want a different pid each sample not to count", i, got)
		}
	}
}

const statusLineCmd = "bash -c 'cols=80; claude-hud --x'"

func statusLine(pid, ppid int) Process {
	return proc(pid, ppid, "/bin/sh", "-c", statusLineCmd)
}

func TestStatusLineIgnorePatternIsReported(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), aged(statusLine(4242, agentPID), time.Hour))}}
	cfg := signalCfg("")
	cfg.GateIgnoreProcs = []string{"claude-hud"}
	buf, ctx := withLog(t)
	ctx = WithProbes(ctx, Probes{Panes: agentPane("w3:p2"), Procs: procs})
	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}, cfg, "", nil)
	if err != nil {
		t.Fatal(err)
	}
	if !got.Safe {
		t.Fatalf("result = %+v, want the status-line child ignored", got)
	}
	want := Ignored{Pane: "w3:p2", PID: 4242, Command: `/bin/sh -c "` + statusLineCmd + `"`, Pattern: "claude-hud"}
	if len(got.Ignored) != 1 || got.Ignored[0] != want {
		t.Fatalf("ignored = %+v, want [%+v]", got.Ignored, want)
	}
	debug := 0
	for _, rec := range logRecords(t, buf) {
		if rec["msg"] == "gate_ignored" && rec["pattern"] == "claude-hud" {
			debug++
		}
	}
	if debug != 1 {
		t.Fatalf("gate_ignored logged %d times, want once", debug)
	}
}

func TestStatusLineDefaultIgnoreNeverCounts(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), aged(statusLine(4242, agentPID), time.Hour))}}
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs}, signalCfg(""), idleAgent("w3:p2", "w3:t2"))
	if !got.Safe || len(got.Ignored) != 1 {
		t.Fatalf("result = %+v, want the default patterns to ignore the status line", got)
	}
}

func TestIgnorePatternNoMatchLeavesJobCounted(t *testing.T) {
	t.Parallel()

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), aged(statusLine(4242, agentPID), time.Hour))}}
	cfg := signalCfg("")
	cfg.GateIgnoreProcs = []string{"nothing-like-it"}
	got := gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs}, cfg, idleAgent("w3:p2", "w3:t2"))
	if got.Safe || len(got.Ignored) != 0 {
		t.Fatalf("result = %+v, want the old child counted and nothing ignored", got)
	}
}

func TestIgnorePatternSubstringAndGlob(t *testing.T) {
	t.Parallel()

	p := statusLine(4242, agentPID)
	tests := []struct {
		pattern string
		want    bool
	}{
		{"claude-hud", true},
		{"*claude-hud*", true},
		{"claude-hu?", true},
		{"claude-hu[cd]", true},
		{"claude-hu[!d]", false},
		{"sh", true},
		{"claude-hux", false},
	}
	for _, tt := range tests {
		if got := matchProc(tt.pattern, p); got != tt.want {
			t.Errorf("matchProc(%q) = %v, want %v", tt.pattern, got, tt.want)
		}
	}
}

func TestPendingQuestionParkedItemIsFree(t *testing.T) {
	t.Parallel()

	start := time.Date(2026, 11, 10, 12, 0, 0, 0, time.UTC)
	item := DecideItem{ID: "D-17", State: "open", NotifyPane: "w3:p2", CreatedAt: start.Add(-48 * time.Hour)}
	got := gateAt(t, start, Probes{Decide: &fakeDecide{lists: [][]DecideItem{{item}}}}, idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want an item parked for two days not to hold", got)
	}

	answered := item
	answered.State = "answered"
	answered.AnsweredAt = start.Add(-time.Hour)
	got = gateAt(t, start, Probes{Decide: &fakeDecide{lists: [][]DecideItem{{answered}}}}, idleAgent("w3:p2", "w3:t2"))
	if got.Safe || got.Busy[0].Signal != SignalPendingQuestion || got.Busy[0].Detail != "answered item D-17" {
		t.Fatalf("result = %+v, want answered item D-17 to hold", got)
	}

	fresh := item
	fresh.CreatedAt = start.Add(time.Minute)
	got = gateAt(t, start, Probes{Decide: &fakeDecide{lists: [][]DecideItem{{fresh}}}}, idleAgent("w3:p2", "w3:t2"))
	if got.Safe || got.Busy[0].Detail != "open item D-17" {
		t.Fatalf("result = %+v, want an item opened this round to hold", got)
	}
}

func TestWaitRoundStartsWhenTheWaitStarts(t *testing.T) {
	t.Parallel()

	clock := &fakeClock{now: time.Date(2026, 11, 10, 12, 0, 0, 0, time.UTC)}
	old := DecideItem{ID: "D-17", State: "open", NotifyPane: "w3:p2", CreatedAt: clock.now.Add(-time.Hour)}
	ctx := WithProbes(context.Background(), Probes{Decide: &fakeDecide{lists: [][]DecideItem{{old}}}})
	cfg := signalCfg("")
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = time.Second
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w3:p2", "w3:t2")}}}
	got, err := Wait(ctx, h, cfg, "", nil, clock, &fakeSleeper{clock: clock})
	if err != nil || !got.Safe {
		t.Fatalf("Wait() = %+v, %v; want safe with an item opened before the wait", got, err)
	}
}

func gateAt(t *testing.T, start time.Time, p Probes, agents ...herdr.Agent) Result {
	t.Helper()
	ctx := WithRoundStart(WithProbes(context.Background(), p), start)
	got, err := GateCheck(ctx, &scriptHerdr{lists: [][]herdr.Agent{agents}}, signalCfg(""), "", nil)
	if err != nil {
		t.Fatalf("GateCheck() unexpected error: %v", err)
	}
	return got
}

type fakeTabs []herdr.Tab

func (f fakeTabs) TabList(context.Context) ([]herdr.Tab, error) { return f, nil }

func TestExemptTabByLabel(t *testing.T) {
	t.Parallel()

	cfg := signalCfg("")
	cfg.GateExemptTabs = []string{"pm*"}
	p := Probes{Tabs: fakeTabs{{TabID: "w3:t2", Label: "pm"}, {TabID: "w4:t1", Label: "ABC-1"}}}
	got := gateWith(t, p, cfg, workingAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want the pm tab exempt", got)
	}
	want := Exempt{PaneID: "w3:p2", TabID: "w3:t2", Label: "pm", Rule: "gate_exempt_tabs=pm*"}
	if len(got.Exempt) != 1 || got.Exempt[0] != want {
		t.Fatalf("exempt = %+v, want [%+v]", got.Exempt, want)
	}

	got = gateWith(t, p, cfg, workingAgent("w4:p1", "w4:t1"))
	if got.Safe || len(got.Exempt) != 0 {
		t.Fatalf("result = %+v, want a non-matching tab still busy", got)
	}
}

func TestExemptPaneByID(t *testing.T) {
	t.Parallel()

	cfg := signalCfg("")
	cfg.GateExemptPanes = []string{"w3:p2"}
	got := gateWith(t, Probes{}, cfg, workingAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want pane w3:p2 exempt while working", got)
	}
	want := Exempt{PaneID: "w3:p2", TabID: "w3:t2", Rule: "gate_exempt_panes=w3:p2"}
	if len(got.Exempt) != 1 || got.Exempt[0] != want {
		t.Fatalf("exempt = %+v, want [%+v]", got.Exempt, want)
	}

	procs := &fakeProcs{lists: [][]Process{append(baseProcs(), proc(4242, agentPID, "sh", "-c", "sleep 600"))}}
	decide := &fakeDecide{lists: [][]DecideItem{{{ID: "D-1", State: "answered", NotifyPane: "w3:p2"}}}}
	got = gateWith(t, Probes{Panes: agentPane("w3:p2"), Procs: procs, Decide: decide}, cfg, idleAgent("w3:p2", "w3:t2"))
	if !got.Safe {
		t.Fatalf("result = %+v, want pane w3:p2 exempt from background-job and pending-question", got)
	}
}

func TestExemptTabBuildToolStillHoldsCheckout(t *testing.T) {
	t.Parallel()

	checkout := "/src/repo"
	cfg := signalCfg(checkout)
	cfg.GateExemptTabs = []string{"pm*"}
	procs := &fakeProcs{
		lists: [][]Process{append(baseProcs(), proc(5000, agentPID, "bazel", "build", "//..."))},
		cwds:  map[int]string{5000: checkout},
	}
	p := Probes{Panes: agentPane("w3:p2"), Procs: procs, Tabs: fakeTabs{{TabID: "w3:t2", Label: "pm"}}}
	got := gateWith(t, p, cfg, idleAgent("w3:p2", "w3:t2"))
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].Signal != SignalBuildTool || len(got.Exempt) != 1 {
		t.Fatalf("result = %+v, want build-tool to hold the checkout for an exempt tab", got)
	}
}

// TestWaitTenTabsWithStatusLineChildrenAreFree is the #618 regression:
// each tab's agent reruns a status-line command on every UI refresh,
// so every sample catches a new short-lived child in each tab.
func TestWaitTenTabsWithStatusLineChildrenAreFree(t *testing.T) {
	t.Parallel()

	const tabs = 10
	panes := fakePanes{info: map[string]herdr.ProcessInfo{}}
	agents := make([]herdr.Agent, 0, tabs)
	base := []Process{proc(1, 0, "launchd")}
	for i := range tabs {
		pane := "w" + strconv.Itoa(i) + ":p1"
		agentPid := 1000 + i
		panes.info[pane] = herdr.ProcessInfo{PaneID: pane, ForegroundProcessGroupID: agentPid}
		agents = append(agents, idleAgent(pane, "w"+strconv.Itoa(i)+":t1"))
		base = append(base, proc(agentPid, 1, "claude"))
	}
	lists := make([][]Process, 0, 3*settleDebouncePolls)
	for sample := range 3 * settleDebouncePolls {
		list := append([]Process(nil), base...)
		for i := range tabs {
			list = append(list, aged(statusLine(10000+sample*tabs+i, 1000+i), 300*time.Millisecond))
		}
		lists = append(lists, list)
	}
	cfg := signalCfg("")
	cfg.GateIgnoreProcs = nil
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = time.Second
	ctx := WithProbes(context.Background(), Probes{Panes: panes, Procs: &fakeProcs{lists: lists}})
	h := &scriptHerdr{lists: [][]herdr.Agent{agents}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}

	got, err := Wait(ctx, h, cfg, "", nil, clock, &fakeSleeper{clock: clock})
	if err != nil {
		t.Fatalf("Wait() unexpected error: %v (result %+v)", err, got)
	}
	if !got.Safe || h.n != settleDebouncePolls {
		t.Fatalf("result = %+v after %d polls, want safe after the debounce", got, h.n)
	}
}
