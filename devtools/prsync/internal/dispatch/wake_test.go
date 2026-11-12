package dispatch

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

// fakeWaker is a scripted agentbus wake. It never runs agentbus. It
// records how many prompts h had sent when it was called.
type fakeWaker struct {
	mu            sync.Mutex
	h             *scriptHerdr
	woke          bool
	err           error
	calls         []string
	promptsBefore []int
}

func (f *fakeWaker) WakeIfHibernated(_ context.Context, machine, pane string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, machine+"|"+pane)
	f.h.mu.Lock()
	f.promptsBefore = append(f.promptsBefore, f.h.promptN)
	f.h.mu.Unlock()
	return f.woke, f.err
}

func wakeRun(t *testing.T, w *fakeWaker, h *scriptHerdr) (Document, error) {
	t.Helper()
	cfg, store := liveCfg(t)
	w.h = h
	return Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
		Waker: w,
	}, fixtureNow)
}

func TestRunLiveWakesAHibernatedPaneBeforePrompting(t *testing.T) {
	t.Parallel()

	h := idleScript()
	w := &fakeWaker{woke: true}
	got, err := wakeRun(t, w, h)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(w.calls) != 1 || w.calls[0] != "|w2:pC" || w.promptsBefore[0] != 0 {
		t.Fatalf("wake calls = %v with %v prompts before, want one before the prompt", w.calls, w.promptsBefore)
	}
	if h.promptN != 1 {
		t.Fatalf("prompts = %d, want 1 after the wake", h.promptN)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched || !got.Results[0].Woke {
		t.Fatalf("results = %+v, want dispatched and woke", got.Results)
	}
}

func TestRunLiveAwakePaneIsNotMarkedWoke(t *testing.T) {
	t.Parallel()

	h := idleScript()
	got, err := wakeRun(t, &fakeWaker{}, h)
	if err != nil || len(got.Results) != 1 || got.Results[0].Action != ActionDispatched || got.Results[0].Woke {
		t.Fatalf("results = %+v, %v; want dispatched, not woke", got.Results, err)
	}
}

func TestRunLiveFailedWakeSendsNoPrompt(t *testing.T) {
	t.Parallel()

	h := idleScript()
	got, err := wakeRun(t, &fakeWaker{err: errors.New("agentbus wake exited 1: relaunched agent not ready within 60s")}, h)
	if !errors.Is(err, ErrFailed) || !strings.Contains(err.Error(), "not ready within 60s") {
		t.Fatalf("Run() error = %v, want ErrFailed naming the wake failure", err)
	}
	if h.promptN != 0 {
		t.Fatalf("prompts = %d, want none into a hibernated pane", h.promptN)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionFailed || !strings.Contains(got.Results[0].Detail, "wake before prompt") {
		t.Fatalf("results = %+v, want failed naming the wake", got.Results)
	}
}

func TestRunLiveMissingAgentbusStillPrompts(t *testing.T) {
	t.Parallel()

	h := idleScript()
	got, err := wakeRun(t, &fakeWaker{err: ErrAgentbusMissing}, h)
	if err != nil || h.promptN != 1 || len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, %v, prompts %d; want dispatched as without agentbus", got.Results, err, h.promptN)
	}
}

func TestAgentbusWaker(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name, machine, wantMachine string
		build                      func(*executor.MockExpectationBuilder)
		woke                       bool
		wantErr                    string
		missing                    bool
	}{
		{name: "not hibernated", wantMachine: "local", build: func(b *executor.MockExpectationBuilder) { b.WillSucceed("", 0).Build() }},
		{name: "woke on a machine", machine: "mini", wantMachine: "mini", woke: true, build: func(b *executor.MockExpectationBuilder) {
			b.WillSucceed("woke w2:pC (herdr:mini/s1/w2:pC)\n", 0).Build()
		}},
		{name: "wake failed", wantMachine: "local", wantErr: "not ready within 60s", build: func(b *executor.MockExpectationBuilder) {
			b.WillFail("agentbus wake: relaunched agent not ready within 60s", 1).Build()
		}},
		{name: "missing", wantMachine: "local", missing: true, build: func(b *executor.MockExpectationBuilder) {
			b.WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Build()
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := executor.NewMockExecutor()
			tc.build(mock.ExpectCommandWithArgs("agentbus", "wake", "--if-hibernated", "--wait", "--machine", tc.wantMachine, "w2:pC"))
			woke, err := AgentbusWaker{Exec: mock, Bin: "agentbus"}.WakeIfHibernated(context.Background(), tc.machine, "w2:pC")
			switch {
			case tc.missing:
				if !errors.Is(err, ErrAgentbusMissing) {
					t.Fatalf("err = %v, want ErrAgentbusMissing", err)
				}
			case tc.wantErr != "":
				if err == nil || errors.Is(err, ErrAgentbusMissing) || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want %q", err, tc.wantErr)
				}
			default:
				if err != nil || woke != tc.woke {
					t.Fatalf("WakeIfHibernated() = %v, %v; want %v", woke, err, tc.woke)
				}
			}
		})
	}
}

// A hibernated PR tab's pane is back at its shell: herdr lists no agent
// status there, and nothing runs under the shell. It is idle, not busy.
func TestGateHibernatedTabIsNotBusy(t *testing.T) {
	t.Parallel()

	shell := fakePanes{info: map[string]herdr.ProcessInfo{"w3:p2": {
		PaneID: "w3:p2", ShellPID: 50, ForegroundProcessGroupID: 50,
		ForegroundProcesses: []herdr.ForegroundProcess{{PID: 50, Name: "zsh", Argv: []string{"zsh"}}},
	}}}
	procs := &fakeProcs{lists: [][]Process{{proc(1, 0, "launchd"), proc(50, 1, "zsh")}}}
	for _, agents := range [][]herdr.Agent{
		{{PaneID: "w3:p2", TabID: "w3:t2"}},
		nil,
	} {
		got := gateWith(t, Probes{Panes: shell, Procs: procs, Decide: &fakeDecide{}}, signalCfg(""), agents...)
		if !got.Safe || len(got.Busy) != 0 {
			t.Fatalf("agents %+v: result = %+v, want safe", agents, got)
		}
	}
}
