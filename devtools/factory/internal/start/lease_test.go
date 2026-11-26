package start

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

const wantRule = "Before changing the shared checkout (~/src/monorepo), run:\n" +
	"  agentbus lease claim --for 45m --why 'PRJ-123 Ship it'\n" +
	"If it says queued, end your turn and wait for the \"[lease] granted\" message.\n" +
	"Run `agentbus lease release` when the tree is clean and pushed. Reading needs no claim."

const wantRenderedPrompt = "PRJ-123 Ship it https://example.test/PRJ-123 acme main PRJ-9\nsecond line"

func noLeaseCall(t *testing.T) func(context.Context, string) (agentbus.Reading, error) {
	t.Helper()
	return func(context.Context, string) (agentbus.Reading, error) {
		t.Error("agentbus was called")
		return agentbus.Reading{}, errors.New("must not be called")
	}
}

func TestLeaseOffPromptUnchanged(t *testing.T) {
	t.Parallel()
	for _, mode := range []string{"", config.LeaseModeOff} {
		h := newHarness(t)
		h.starter.Cfg.Lease = config.LeasePrompt{Mode: mode, ClaimFor: "45m"}
		h.starter.LeaseStatus = noLeaseCall(t)
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if h.herdr.promptText != wantRenderedPrompt {
			t.Fatalf("mode %q: prompt = %q", mode, h.herdr.promptText)
		}
		if got, want := h.out.String(), wantPlan+"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n"; got != want {
			t.Fatalf("mode %q: plan =\n%s\nwant =\n%s", mode, got, want)
		}
		assertRun(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
		runs, err := h.st.Runs(context.Background(), 1)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(runs[0].DetailJSON, "lease") {
			t.Fatalf("mode %q: detail = %s", mode, runs[0].DetailJSON)
		}
	}
}

func TestLeasePromptRuleFirst(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePrompt, ClaimFor: "45m"}
	h.starter.LeaseStatus = noLeaseCall(t)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if want := wantRule + "\n\n" + wantRenderedPrompt; h.herdr.promptText != want {
		t.Fatalf("prompt =\n%s\nwant =\n%s", h.herdr.promptText, want)
	}
	wantOut := "" +
		"claim    PRJ-123 → In Progress (assign to me)\n" +
		"tab      workspace \"factory\" / tab \"PRJ-123\" (cwd ~/src/monorepo)\n" +
		"agent    claude\n" +
		"args     (default)\n" +
		"prompt   Before changing the shared checkout (~/src/monorepo), run:…\n" +
		"lease    prompt, rule at the top of the prompt:\n" +
		"         Before changing the shared checkout (~/src/monorepo), run:\n" +
		"           agentbus lease claim --for 45m --why 'PRJ-123 Ship it'\n" +
		"         If it says queued, end your turn and wait for the \"[lease] granted\" message.\n" +
		"         Run `agentbus lease release` when the tree is clean and pushed. Reading needs no claim.\n"
	if !strings.HasSuffix(h.out.String(), wantOut+"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n") {
		t.Fatalf("plan =\n%s\nwant =\n%s", h.out.String(), wantOut)
	}
}

func TestLeaseRuleQuotesTitleAndUsesClaimFor(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	it := h.snap.Items[h.id]
	it.Title = `Don't break "it" $HOME`
	h.snap.Items[h.id] = it
	h.src.inner.Items[h.id] = it
	h.starter.Cfg.Cwd = "/lane/checkout"
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePrompt, ClaimFor: "1h30m"}
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	want := "Before changing the shared checkout (/lane/checkout), run:\n" +
		`  agentbus lease claim --for 1h30m --why 'PRJ-123 Don'\''t break "it" $HOME'` + "\n"
	if !strings.Contains(h.out.String(), "         "+strings.Split(want, "\n")[1]+"\n") ||
		!strings.Contains(h.out.String(), "(/lane/checkout)") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
}

func TestLeaseRulePlaceholder(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.StartPrompt = "Work on {id}.\n{{lease_rule}}\nThen open a PR."
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePrompt, ClaimFor: "45m"}
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	want := "Work on PRJ-123.\n" + wantRule + "\nThen open a PR."
	if h.herdr.promptText != want {
		t.Fatalf("prompt =\n%s\nwant =\n%s", h.herdr.promptText, want)
	}
	if !strings.Contains(h.out.String(), "lease    prompt, rule at {{lease_rule}}:\n") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
}

func TestLeaseRulePlaceholderOffIsRemoved(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.StartPrompt = "Work on {id}.{{lease_rule}}"
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModeOff, ClaimFor: "45m"}
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h.herdr.promptText != "Work on PRJ-123." {
		t.Fatalf("prompt = %q", h.herdr.promptText)
	}
}

func TestLeaseRuleTitleCannotInjectPlaceholder(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	it := h.snap.Items[h.id]
	it.Title = "{{lease_rule}}"
	h.snap.Items[h.id] = it
	h.src.inner.Items[h.id] = it
	h.starter.Cfg.StartPrompt = "{title}"
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePrompt, ClaimFor: "45m"}
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(h.out.String(), "rule at the top of the prompt") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
}

type runLease struct {
	State    string `json:"state"`
	Holder   string `json:"holder"`
	Deadline string `json:"deadline"`
	Queue    int    `json:"queue"`
	Error    string `json:"error"`
}

func lastRunLease(t *testing.T, h *harness) runLease {
	t.Helper()
	runs, err := h.st.Runs(context.Background(), 1)
	if err != nil {
		t.Fatal(err)
	}
	if len(runs) != 1 {
		t.Fatalf("runs = %d", len(runs))
	}
	var detail struct {
		Lease *runLease `json:"lease"`
	}
	if err := json.Unmarshal([]byte(runs[0].DetailJSON), &detail); err != nil {
		t.Fatal(err)
	}
	if detail.Lease == nil {
		t.Fatalf("detail has no lease: %s", runs[0].DetailJSON)
	}
	return *detail.Lease
}

func TestLeasePromptCheckHeld(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePromptCheck, ClaimFor: "45m"}
	deadline := "2026-11-01T14:20:00Z"
	var dirs []string
	h.starter.LeaseStatus = func(_ context.Context, dir string) (agentbus.Reading, error) {
		dirs = append(dirs, dir)
		return agentbus.Reading{Guarded: true, Status: agentbus.Status{
			Checkout: "local:/src/monorepo",
			State:    agentbus.StateHeld,
			Holder:   &agentbus.Holder{Handle: "herdr:local/default/w1:p9", Name: "pm/foo", Deadline: deadline},
			Queue:    []agentbus.Waiter{{Handle: "herdr:local/default/w1:p8"}},
		}}, nil
	}
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	until, err := time.Parse(time.RFC3339, deadline)
	if err != nil {
		t.Fatal(err)
	}
	line := "lease: held by pm/foo until " + until.Local().Format("15:04") + " (1 queued)\n" +
		"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n"
	if !strings.HasSuffix(h.out.String(), line) {
		t.Fatalf("output =\n%s\nwant suffix %q", h.out.String(), line)
	}
	if len(dirs) != 1 || dirs[0] != "~/src/monorepo" {
		t.Fatalf("lease reads = %q", dirs)
	}
	assertCalls(t, h.log, happyCalls)
	got := lastRunLease(t, h)
	want := runLease{State: "held", Holder: "pm/foo", Deadline: deadline, Queue: 1}
	if got != want {
		t.Fatalf("run lease = %+v, want %+v", got, want)
	}
}

func TestLeasePromptCheckMissingAgentbus(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePromptCheck, ClaimFor: "45m"}
	h.starter.LeaseStatus = func(context.Context, string) (agentbus.Reading, error) {
		return agentbus.Reading{}, agentbus.ErrMissing
	}
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	wantSuffix := "lease: unknown (agentbus not installed)\n" +
		"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n"
	if !strings.HasSuffix(h.out.String(), wantSuffix) {
		t.Fatalf("output =\n%s", h.out.String())
	}
	assertCalls(t, h.log, happyCalls)
	got := lastRunLease(t, h)
	if got != (runLease{State: "unknown", Error: "agentbus not installed"}) {
		t.Fatalf("run lease = %+v", got)
	}
}

func TestLeasePromptCheckRemoteLane(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePromptCheck, ClaimFor: "45m"}
	h.starter.Machine = "mac1"
	h.starter.Lane = "mac"
	h.starter.LeaseStatus = noLeaseCall(t)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(h.out.String(), "lease: unknown (checkout is on remote machine mac1; agentbus runs locally)\n") ||
		!strings.HasSuffix(h.out.String(), "factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n") {
		t.Fatalf("output =\n%s", h.out.String())
	}
}

func TestLeasePromptCheckDryRunDoesNotRead(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePromptCheck, ClaimFor: "45m"}
	h.starter.LeaseStatus = noLeaseCall(t)
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(h.out.String(), "lease: ") {
		t.Fatalf("output =\n%s", h.out.String())
	}
}

func TestLeaseUnknownModeIsAnError(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: "maybe", ClaimFor: "45m"}
	err := h.start(h.snap, okGate, Options{})
	if err == nil || !strings.Contains(err.Error(), `lease.mode: expected off, prompt, or prompt+check, found "maybe"`) {
		t.Fatalf("err = %v", err)
	}
}
