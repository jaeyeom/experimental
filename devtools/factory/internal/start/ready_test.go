package start

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func herdrErr(code string) error {
	return &herdr.ProcError{ExitCode: 1, Stdout: `{"error":{"code":"` + code + `","message":"` + code + ` happened"}}`}
}

// runLog decodes the start run's ready and recovery fields.
func runLog(t *testing.T, h *harness) (readyMS int64, stalled bool, recovery string) {
	t.Helper()
	runs, err := h.st.Runs(context.Background(), 1)
	if err != nil || len(runs) != 1 {
		t.Fatalf("runs = %+v, err = %v", runs, err)
	}
	var d struct {
		ReadyWaitMS *int64 `json:"ready_wait_ms"` //nolint:tagliatelle // documented factory JSON output field
		Stalled     bool   `json:"stalled"`
		Recovery    string `json:"recovery"`
	}
	if err := json.Unmarshal([]byte(runs[0].DetailJSON), &d); err != nil || d.ReadyWaitMS == nil {
		t.Fatalf("detail = %s, err = %v", runs[0].DetailJSON, err)
	}
	return *d.ReadyWaitMS, d.Stalled, d.Recovery
}

func TestReadyWaitBeforePrompt(t *testing.T) {
	h := newHarness(t)
	var slept []time.Duration
	h.starter.Sleep = func(_ context.Context, d time.Duration) error { slept = append(slept, d); return nil }
	h.starter.Defaults.AgentReadySettle = time.Second
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, happyCalls) // Tabs, Get, Claim, Get, FindWorkspace, CreateTab, StartAgent, WaitAgent, Prompt.
	if len(slept) != 1 || slept[0] != time.Second {
		t.Fatalf("slept = %v, want the 1s settle", slept)
	}
	if _, stalled, recovery := runLog(t, h); stalled || recovery != "" {
		t.Fatalf("stalled = %v recovery = %q", stalled, recovery)
	}
}

func TestStallPromptInInputBoxSendsEnter(t *testing.T) {
	h := newHarness(t)
	h.herdr.promptErrs = []error{herdrErr(herdr.CodePromptStalled)}
	h.herdr.waits = []waitResult{{status: "idle"}, {status: "idle"}, {status: "working"}}
	h.herdr.screen = "> PRJ-123 Ship it https://example.test/PRJ-123 acme main PRJ-9\n  second line\n"
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if len(h.herdr.keys) != 1 || h.herdr.keys[0] != "enter" || h.herdr.prompts != 1 {
		t.Fatalf("keys = %v prompts = %d", h.herdr.keys, h.herdr.prompts)
	}
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	if _, stalled, recovery := runLog(t, h); !stalled || recovery != "enter" {
		t.Fatalf("stalled = %v recovery = %q", stalled, recovery)
	}
}

func TestStallEmptyInputBoxResendsOnce(t *testing.T) {
	h := newHarness(t)
	h.herdr.promptErrs = []error{herdrErr(herdr.CodeTimeout)}
	h.herdr.waits = []waitResult{{status: "idle"}, {status: "idle"}}
	h.herdr.screen = "> \n"
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h.herdr.prompts != 2 || len(h.herdr.keys) != 0 {
		t.Fatalf("prompts = %d keys = %v, want one resend", h.herdr.prompts, h.herdr.keys)
	}
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	if _, stalled, recovery := runLog(t, h); !stalled || recovery != "resent" {
		t.Fatalf("stalled = %v recovery = %q", stalled, recovery)
	}
}

func TestStallWhileWorkingNeedsNoRecovery(t *testing.T) {
	h := newHarness(t)
	h.herdr.promptErrs = []error{herdrErr(herdr.CodePromptStalled)}
	h.herdr.waits = []waitResult{{status: "idle"}, {status: "working"}}
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h.herdr.prompts != 1 || len(h.herdr.keys) != 0 {
		t.Fatalf("prompts = %d keys = %v", h.herdr.prompts, h.herdr.keys)
	}
	if _, stalled, recovery := runLog(t, h); !stalled || recovery != "none" {
		t.Fatalf("stalled = %v recovery = %q", stalled, recovery)
	}
}

func TestStallWithOtherActivityIsNotRetried(t *testing.T) {
	h := newHarness(t)
	cause := herdrErr(herdr.CodePromptStalled)
	h.herdr.promptErrs = []error{cause}
	h.herdr.waits = []waitResult{{status: "idle"}, {status: "done"}}
	h.herdr.screen = "Thinking about something else\nDone.\n"
	err := h.start(h.snap, okGate, applyOpt)
	assertFailedStep(t, h, err, cause, 6)
	if !strings.Contains(err.Error(), "agent is done, not retried; screen:\nThinking about something else\nDone.") {
		t.Fatalf("error = %v", err)
	}
	if h.herdr.prompts != 1 || len(h.herdr.keys) != 0 || len(h.herdr.closedTabs) != 0 {
		t.Fatalf("prompts = %d keys = %v closed = %v", h.herdr.prompts, h.herdr.keys, h.herdr.closedTabs)
	}
}

func TestSecondStallFailsWithoutAThirdPrompt(t *testing.T) {
	h := newHarness(t)
	h.herdr.promptErrs = []error{herdrErr(herdr.CodePromptStalled), herdrErr(herdr.CodePromptStalled)}
	h.herdr.waits = []waitResult{{status: "idle"}, {status: "idle"}}
	err := h.start(h.snap, okGate, applyOpt)
	assertFailedStep(t, h, err, nil, 6)
	if !strings.Contains(err.Error(), "resent once, then") || h.herdr.prompts != 2 {
		t.Fatalf("error = %v prompts = %d", err, h.herdr.prompts)
	}
	if len(h.herdr.closedTabs) != 0 {
		t.Fatalf("closed = %v, want the tab left for inspection", h.herdr.closedTabs)
	}
}

func TestAgentPaneBusyRetries(t *testing.T) {
	h := newHarness(t)
	var slept []time.Duration
	h.starter.Sleep = func(_ context.Context, d time.Duration) error { slept = append(slept, d); return nil }
	h.herdr.startErrs = []error{herdrErr(herdr.CodePaneBusy), herdrErr(herdr.CodePaneBusy)}
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if len(slept) != 2 || slept[0] != time.Second || slept[1] != 2*time.Second {
		t.Fatalf("slept = %v, want 1s then 2s", slept)
	}
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	if len(h.herdr.closedTabs) != 0 {
		t.Fatalf("closed = %v", h.herdr.closedTabs)
	}
}

func TestAgentPaneAlwaysBusyCleansUp(t *testing.T) {
	h := newHarness(t)
	h.starter.Defaults.AgentBusyTimeout = 3 * time.Second
	h.herdr.startErr = herdrErr(herdr.CodePaneBusy)
	err := h.start(h.snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "still busy after 3 attempts over 3s") ||
		!strings.Contains(err.Error(), "closed tab w1:t1 and released PRJ-123; re-run: factory start PRJ-123 --apply") {
		t.Fatalf("error = %v", err)
	}
	if len(h.herdr.closedTabs) != 1 || h.herdr.closedTabs[0] != "w1:t1" {
		t.Fatalf("closed = %v", h.herdr.closedTabs)
	}
	got, gerr := h.st.LatestClaim(context.Background(), "jira:acme", "PRJ-123")
	if gerr != nil || got.State != store.Released {
		t.Fatalf("claim = %+v, err = %v", got, gerr)
	}
	if h.herdr.prompts != 0 {
		t.Fatalf("prompts = %d", h.herdr.prompts)
	}
}

func TestPromptOnScreen(t *testing.T) {
	prompt := "Fix PRJ-1\n\n  then   report\n"
	if !promptOnScreen(prompt, "│ > Fix PRJ-1\n│   then report │\n") {
		t.Fatal("rewrapped prompt: want found")
	}
	if promptOnScreen(prompt, "> Fix PRJ-1\n") || promptOnScreen("", "anything") {
		t.Fatal("partial or empty prompt: want not found")
	}
}

func TestForceStartsDeferredItemAndNamesIt(t *testing.T) {
	h := newHarness(t)
	later := time.Date(2099, 1, 2, 0, 0, 0, 0, time.UTC)
	it := h.snap.Items[h.id]
	it.NotBefore = &later
	h.snap.Items[h.id] = it
	h.src.inner.Items[h.id] = it
	if err := h.start(h.snap, okGate, forceOpt); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(h.out.String(), "warning: --force skips the ready checks (deferred until 2099-01-02)") {
		t.Fatalf("output =\n%s", h.out.String())
	}
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
}
