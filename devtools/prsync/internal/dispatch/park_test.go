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

func TestResumePromptCatchupAndRerequestMatrix(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	job := &Job{
		ID:     "j-8",
		Runner: "tiger-runner",
		Items:  []string{"PRRC_widget"},
		Saved:  "park/j-7",
		Answer: "Keep the old API; add a deprecation note.",
	}

	// Byte-for-byte default resume prompt (dispatch_catchup=rebase, dispatch_rerequest=on).
	wantDefaultResume := renderPrompt(config.Defaults(), Request{}, pr) + "\n\n" + strings.Join([]string{
		"Resume: agentbus job j-8 was parked on a design question and is ready",
		"again. Its work is on branch park/j-7. The parked threads:",
		"   - PRRC_widget: https://github.com/acme/widgets/pull/123#discussion_r1",
		"The answer:",
		"Keep the old API; add a deprecation note.",
		"",
		"Get onto park/j-7 and apply the answer to those threads. Then",
		"reply on each parked thread with the outcome, and",
		"resolve it unless the reply asks the reviewer a follow-up question.",
		"Then push, and re-request review as above.",
	}, "\n")
	if got := renderPrompt(parkConfig(), Request{Job: job}, pr); got != wantDefaultResume {
		t.Fatalf("default resume prompt mismatch:\ngot:\n%s\nwant:\n%s", got, wantDefaultResume)
	}

	for _, catchup := range []string{"rebase", "merge"} {
		for _, rerequest := range []string{"on", "off"} {
			t.Run("catchup="+catchup+"/rerequest="+rerequest, func(t *testing.T) {
				t.Parallel()
				cfg := parkConfig()
				cfg.DispatchCatchup = catchup
				cfg.DispatchRerequest = rerequest

				got := renderPrompt(cfg, Request{Job: job}, pr)
				if catchup == "merge" {
					if !strings.Contains(got, "merge origin/main into fix-widget") {
						t.Errorf("resume prompt preamble missing merge instruction:\n%s", got)
					}
					if !strings.Contains(got, "push normally") || !strings.Contains(strings.ToLower(got), "force-push") {
						t.Errorf("resume prompt missing push-normally / no-force-push:\n%s", got)
					}
					if strings.Contains(got, "bring fix-widget up to date with origin/main as needed") {
						t.Errorf("resume prompt kept generic catch-up preamble:\n%s", got)
					}
				} else if rerequest == "on" && got != wantDefaultResume {
					t.Errorf("rebase+on resume prompt differs from default:\n%s", got)
				}

				if rerequest == "off" {
					if strings.Contains(got, "add-reviewer") || strings.Contains(got, "re-request") || strings.Contains(got, "latestReviews") {
						t.Errorf("resume prompt still asks to re-request reviewers:\n%s", got)
					}
					if !strings.Contains(got, "another tool handles reviewer requests") {
						t.Errorf("resume prompt missing other-tool reviewer note:\n%s", got)
					}
				} else if !strings.Contains(got, "re-request review as above.") {
					t.Errorf("rerequest=on resume prompt missing re-request review:\n%s", got)
				}
			})
		}
	}

	// Park prompt (Answer == "") also respects dispatch_catchup=merge and dispatch_rerequest=off.
	parkCfg := parkConfig()
	parkCfg.DispatchCatchup = "merge"
	parkCfg.DispatchRerequest = "off"
	parkGot := renderPrompt(parkCfg, Request{Job: &Job{ID: "j-7", Runner: "tiger-runner"}}, pr)
	if !strings.Contains(parkGot, "merge origin/main into fix-widget") || !strings.Contains(parkGot, "push normally (do not rebase or force-push)") {
		t.Errorf("park prompt under merge missing merge/push-normally:\n%s", parkGot)
	}
	if strings.Contains(parkGot, "add-reviewer") || strings.Contains(parkGot, "re-request") {
		t.Errorf("park prompt under rerequest=off still mentions reviewer requests:\n%s", parkGot)
	}
	if !strings.Contains(parkGot, "another tool handles reviewer requests") {
		t.Errorf("park prompt under rerequest=off missing other-tool reviewer note:\n%s", parkGot)
	}
}
