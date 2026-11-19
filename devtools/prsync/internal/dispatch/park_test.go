package dispatch

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
)

func parkConfig() config.Config {
	cfg := config.Defaults()
	cfg.DispatchParkMode = "on"
	cfg.DispatchAskTo = "review-manager"
	return cfg
}

func TestParkModeOffKeepsTodaysPrompt(t *testing.T) {
	pr := fixtureEligiblePR()
	cfg := config.Defaults()
	cfg.DispatchAskTo = "review-manager"
	want := renderPrompt(cfg, Request{}, pr)
	if got := renderPrompt(cfg, Request{Job: &Job{ID: "j-7", Runner: "tiger-runner"}}, pr); got != want {
		t.Fatalf("park mode off changed the job prompt:\n%s\nwant\n%s", got, want)
	}
	if got := renderPrompt(parkConfig(), Request{}, pr); got != want {
		t.Fatalf("park mode changed a prompt that runs no job:\n%s", got)
	}
}

func TestParkModeOnAddsTheParkAskAndNoWaitSteps(t *testing.T) {
	pr := fixtureEligiblePR()
	cfg := parkConfig()
	cfg.PromptSuffix = "SUFFIX"
	got := renderPrompt(cfg, Request{Job: &Job{ID: "j-7", Runner: "tiger-runner"}}, pr)
	for _, want := range []string{
		"finish, push, and reply on the mechanical threads first",
		`agentbus send review-manager "`,
		"recommended option",
		"never a stash",
		"agentbus jobs park j-7 --runner tiger-runner --ask-msg <message id> --saved <branch>",
		"--item <comment id>",
		"- PRRC_widget: https://github.com/acme/widgets/pull/123#discussion_r1",
		"Never wait in this tab",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("park prompt lacks %q:\n%s", want, got)
		}
	}
	if strings.Contains(got, "{ask_to}") || strings.Contains(got, "{job}") || strings.Contains(got, "{runner}") {
		t.Errorf("park prompt has an unexpanded placeholder:\n%s", got)
	}
	if !strings.HasSuffix(got, "\n\nSUFFIX") {
		t.Errorf("the suffix is not last:\n%s", got)
	}
	// The park steps apply to review threads only.
	if ci := renderPrompt(cfg, Request{CIFix: true, Job: &Job{ID: "j-7", Runner: "r"}}, pr); strings.Contains(ci, "agentbus jobs park") {
		t.Errorf("a CI-fix job got the park steps:\n%s", ci)
	}
}

func TestResumePromptCarriesTheAnswer(t *testing.T) {
	pr := fixtureEligiblePR()
	job := &Job{ID: "j-8", Runner: "tiger-runner", Items: []string{"PRRC_widget"}, Saved: "park/j-7", Answer: "Keep the old API; add a deprecation note."}
	got := renderPrompt(parkConfig(), Request{Job: job}, pr)
	for _, want := range []string{
		"Keep the old API; add a deprecation note.",
		"park/j-7",
		"PRRC_widget",
		"reply on each parked thread",
		"resolve it unless the reply asks the reviewer a follow-up",
		"push",
		"re-request review",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("resume prompt lacks %q:\n%s", want, got)
		}
	}
	if strings.Contains(got, "agentbus jobs park") {
		t.Errorf("a resumed job is told to park again:\n%s", got)
	}
}
