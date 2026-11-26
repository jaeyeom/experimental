package start

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
)

// useWake sets a Wake hook that logs "Wake <machine> <pane>" and returns woke, err.
func useWake(h *harness, woke bool, err error) {
	h.starter.Wake = func(ctx context.Context, machine, pane string) (bool, error) {
		if ctx == nil {
			return false, errors.New("wake: expected a context, found nil")
		}
		*h.log = append(*h.log, "Wake "+machine+" "+pane)
		return woke, err
	}
}

func withWake(calls []string, wake string) []string {
	out := make([]string, 0, len(calls)+1)
	for _, call := range calls {
		if call == "Prompt" {
			out = append(out, wake)
		}
		out = append(out, call)
	}
	return out
}

func runWoke(t *testing.T, h *harness) (string, bool) {
	t.Helper()
	runs, err := h.st.Runs(context.Background(), 1)
	if err != nil {
		t.Fatal(err)
	}
	if len(runs) != 1 {
		t.Fatalf("runs = %+v", runs)
	}
	var detail struct {
		Summary string `json:"summary"`
		Woke    bool   `json:"woke"`
	}
	if err := json.Unmarshal([]byte(runs[0].DetailJSON), &detail); err != nil {
		t.Fatal(err)
	}
	return detail.Summary, detail.Woke
}

func TestWakeHibernatedBeforePrompt(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	useWake(h, true, nil)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, withWake(happyCalls, "Wake  w1:p1"))
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	summary, woke := runWoke(t, h)
	if !woke || summary != "started PRJ-123 (woke hibernated agent)" {
		t.Fatalf("summary = %q woke = %v", summary, woke)
	}
}

func TestWakeNotHibernated(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	useWake(h, false, nil)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, withWake(happyCalls, "Wake  w1:p1"))
	assertRun(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	runs, err := h.st.Runs(context.Background(), 1)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(runs[0].DetailJSON, "woke") {
		t.Fatalf("detail = %s, want no woke key", runs[0].DetailJSON)
	}
}

func TestWakeLaneMachine(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Machine = "mac"
	h.starter.Lane = "mac"
	h.starter.Stash = nil
	useWake(h, false, nil)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, withWake(happyCalls, "Wake mac w1:p1"))
}

func TestWakeFailedSkipsPrompt(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	cause := errors.New("agentbus wake: exit 1: relaunch timed out")
	useWake(h, false, cause)
	err := h.start(h.snap, okGate, applyOpt)
	assertFailedStep(t, h, err, cause, 6)
	if !strings.Contains(err.Error(), "wake before prompt") || !strings.Contains(err.Error(), "prompt not sent") {
		t.Fatalf("error = %v", err)
	}
	assertCalls(t, h.log, []string{"Tabs", "Get", "Claim", "Get", "FindWorkspace", "CreateTab", "StartAgent", "WaitAgent", "Wake  w1:p1"})
	assertNoRun(t, h.st)
}

func TestWakeMissingAgentbusPrompts(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	useWake(h, false, agentbus.ErrMissing)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, withWake(happyCalls, "Wake  w1:p1"))
	assertRun(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
}
