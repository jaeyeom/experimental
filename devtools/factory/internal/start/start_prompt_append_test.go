package start

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestStartPromptAppendEmptyUnchanged(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModeOff, ClaimFor: "45m"}
	h.starter.Cfg.StartPromptAppend = ""
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h.herdr.promptText != wantRenderedPrompt {
		t.Fatalf("prompt = %q, want %q", h.herdr.promptText, wantRenderedPrompt)
	}
	if got, want := h.out.String(), wantPlan+"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n"; got != want {
		t.Fatalf("plan =\n%s\nwant =\n%s", got, want)
	}
}

func TestStartPromptAppendAfterLeaseRule(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePrompt, ClaimFor: "45m"}
	h.starter.Cfg.StartPromptAppend = "Send questions to {caller_bus}.\nReport back: {report_back}"
	h.starter.Caller = "w1:p1"
	h.starter.CallerBus = "coord"
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	wantAppend := "Send questions to coord.\nReport back: agentbus send coord \"<message>\""
	wantPrompt := wantRule + "\n\n" + wantAppend + "\n\n" + wantRenderedPrompt
	if h.herdr.promptText != wantPrompt {
		t.Fatalf("prompt =\n%s\nwant =\n%s", h.herdr.promptText, wantPrompt)
	}
	wantPlanSuffix := "" +
		"append   after the lease rule:\n" +
		"         Send questions to coord.\n" +
		"         Report back: agentbus send coord \"<message>\"\n" +
		"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n"
	if !strings.HasSuffix(h.out.String(), wantPlanSuffix) {
		t.Fatalf("plan =\n%s\nwant suffix =\n%s", h.out.String(), wantPlanSuffix)
	}
}

func TestStartPromptAppendLeaseOffAtTop(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModeOff, ClaimFor: "45m"}
	h.starter.Cfg.StartPromptAppend = "Work on {id}.\nAsk coordinator."
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	wantOut := "" +
		"claim    PRJ-123 → In Progress (assign to me)\n" +
		"tab      workspace \"factory\" / tab \"PRJ-123\" (cwd ~/src/monorepo)\n" +
		"agent    claude\n" +
		"args     (default)\n" +
		"prompt   Work on PRJ-123.…\n" +
		"append   at the top of the prompt:\n" +
		"         Work on PRJ-123.\n" +
		"         Ask coordinator.\n"
	if h.out.String() != wantOut {
		t.Fatalf("plan =\n%s\nwant =\n%s", h.out.String(), wantOut)
	}

	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	wantPrompt := "Work on PRJ-123.\nAsk coordinator.\n\n" + wantRenderedPrompt
	if h.herdr.promptText != wantPrompt {
		t.Fatalf("prompt =\n%s\nwant =\n%s", h.herdr.promptText, wantPrompt)
	}
}

func TestStartPromptAppendPlaceholder(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.StartPrompt = "Work on {id}.\n{{start_prompt_append}}\nThen open a PR."
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModePrompt, ClaimFor: "45m"}
	h.starter.Cfg.StartPromptAppend = "Line A for {source}\nLine B"
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	wantPrompt := wantRule + "\n\nWork on PRJ-123.\nLine A for acme\nLine B\nThen open a PR."
	if h.herdr.promptText != wantPrompt {
		t.Fatalf("prompt =\n%s\nwant =\n%s", h.herdr.promptText, wantPrompt)
	}
	if !strings.Contains(h.out.String(), "append   at {{start_prompt_append}}:\n") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
}

func TestStartPromptAppendTitleCannotInjectPlaceholder(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	it := h.snap.Items[h.id]
	it.Title = "{{start_prompt_append}}"
	h.snap.Items[h.id] = it
	h.src.inner.Items[h.id] = it
	h.starter.Cfg.StartPrompt = "{title}"
	h.starter.Cfg.Lease = config.LeasePrompt{Mode: config.LeaseModeOff, ClaimFor: "45m"}
	h.starter.Cfg.StartPromptAppend = "Standing line"
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h.herdr.promptText != "Standing line\n\n{{start_prompt_append}}" {
		t.Fatalf("prompt = %q", h.herdr.promptText)
	}
	if !strings.Contains(h.out.String(), "append   at the top of the prompt:\n") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
}
