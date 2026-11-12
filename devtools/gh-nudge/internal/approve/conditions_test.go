package approve

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func intPtr(n int) *int { return &n }

// basePR passes every condition in fullRule.
func basePR() github.ApprovalPullRequest {
	return github.ApprovalPullRequest{
		Ref:            github.PullRef{Owner: "acme", Repo: "widgets", Number: 1},
		URL:            "https://github.com/acme/widgets/pull/1",
		Title:          "Tidy dashboard",
		State:          "OPEN",
		Author:         "alice",
		HeadSHA:        "h1",
		Additions:      0,
		Deletions:      3,
		ChangedFiles:   1,
		Files:          []models.File{{Path: "dash/a.go", Deletions: 3}},
		Labels:         []string{"internal-only"},
		RequestedUsers: []string{"me"},
		Reviews:        []github.ApprovalReview{{Author: "review-bot", State: "APPROVED", CommitSHA: "h1"}},
		Checks:         []github.ApprovalCheck{{Name: "ci", Required: true, Outcome: models.ChecksPass}},
	}
}

func fullRule() config.ApproveRule {
	return config.ApproveRule{
		Name:                "full",
		Repos:               []string{"acme/*"},
		PathsAllMatch:       []string{"dash/**"},
		PathsNoneMatch:      []string{".github/**"},
		MaxChangedLines:     intPtr(10),
		MaxAddedLines:       intPtr(5),
		MaxFiles:            intPtr(2),
		PureDeletion:        true,
		AuthorsAllow:        []string{"alice"},
		AuthorsDeny:         []string{"mallory"},
		LabelsRequired:      []string{"internal-only"},
		LabelsForbidden:     []string{"wip"},
		RequiredApprovalsBy: []string{"review-bot"},
		Checks:              config.ChecksGreen,
		NotDraft:            true,
	}
}

func conditionResult(t *testing.T, result RuleResult, name string) ConditionResult {
	t.Helper()
	for _, cond := range result.Conditions {
		if cond.Condition == name {
			return cond
		}
	}
	t.Fatalf("condition %s not evaluated in %+v", name, result)
	return ConditionResult{}
}

// onlyCondition returns fullRule with every condition but name turned off.
// max_unresolved_threads is always on; basePR has no unresolved thread.
func onlyCondition(name string) config.ApproveRule {
	full := fullRule()
	rule := config.ApproveRule{Name: name}
	switch name {
	case "repos":
		rule.Repos = full.Repos
	case "paths_all_match":
		rule.PathsAllMatch = full.PathsAllMatch
	case "paths_none_match":
		rule.PathsNoneMatch = full.PathsNoneMatch
	case "max_changed_lines":
		rule.MaxChangedLines = full.MaxChangedLines
	case "max_added_lines":
		rule.MaxAddedLines = full.MaxAddedLines
	case "max_files":
		rule.MaxFiles = full.MaxFiles
	case "pure_deletion":
		rule.PureDeletion = true
	case "authors_allow":
		rule.AuthorsAllow = full.AuthorsAllow
	case "authors_deny":
		rule.AuthorsDeny = full.AuthorsDeny
	case "labels_required":
		rule.LabelsRequired = full.LabelsRequired
	case "labels_forbidden":
		rule.LabelsForbidden = full.LabelsForbidden
	case "required_approvals_by":
		rule.RequiredApprovalsBy = full.RequiredApprovalsBy
	case "checks":
		rule.Checks = full.Checks
	case "not_draft":
		rule.NotDraft = true
	}
	return rule
}

func TestEachConditionPassesAndFailsOnItsOwn(t *testing.T) {
	tests := []struct {
		condition string
		breakPR   func(pr *github.ApprovalPullRequest)
		wantSaw   string
	}{
		{"repos", func(pr *github.ApprovalPullRequest) { pr.Ref.Owner = "other" }, "other/widgets"},
		{"paths_all_match", func(pr *github.ApprovalPullRequest) {
			pr.Files = append(pr.Files, models.File{Path: "infra/main.tf"})
		}, "infra/main.tf"},
		{"paths_none_match", func(pr *github.ApprovalPullRequest) {
			pr.Files = append(pr.Files, models.File{Path: ".github/workflows/ci.yml"})
		}, ".github/workflows/ci.yml"},
		{"max_changed_lines", func(pr *github.ApprovalPullRequest) { pr.Deletions = 11 }, "11"},
		{"max_added_lines", func(pr *github.ApprovalPullRequest) { pr.Additions = 6 }, "6"},
		{"max_files", func(pr *github.ApprovalPullRequest) { pr.ChangedFiles = 3 }, "3"},
		{"pure_deletion", func(pr *github.ApprovalPullRequest) { pr.Additions = 1 }, "1"},
		{"authors_allow", func(pr *github.ApprovalPullRequest) { pr.Author = "bob" }, "bob"},
		{"authors_deny", func(pr *github.ApprovalPullRequest) { pr.Author = "Mallory" }, "Mallory"},
		{"labels_required", func(pr *github.ApprovalPullRequest) { pr.Labels = nil }, "internal-only"},
		{"labels_forbidden", func(pr *github.ApprovalPullRequest) { pr.Labels = append(pr.Labels, "wip") }, "wip"},
		{"required_approvals_by", func(pr *github.ApprovalPullRequest) { pr.Reviews = nil }, "review-bot"},
		{"checks", func(pr *github.ApprovalPullRequest) { pr.Checks[0].Outcome = models.ChecksFail }, "ci"},
		{"not_draft", func(pr *github.ApprovalPullRequest) { pr.Draft = true }, "draft"},
		{"max_unresolved_threads", func(pr *github.ApprovalPullRequest) { pr.UnresolvedThreads = 1 }, "1"},
	}
	full := EvaluateRule(fullRule(), basePR())
	if !full.Matched || len(full.Conditions) != len(tests) {
		t.Fatalf("base PR = %+v, want all %d conditions to pass", full, len(tests))
	}
	for _, tt := range tests {
		t.Run(tt.condition, func(t *testing.T) {
			rule := onlyCondition(tt.condition)
			passing := EvaluateRule(rule, basePR())
			if got := conditionResult(t, passing, tt.condition); !got.Pass || !passing.Matched {
				t.Fatalf("base %s failed: %+v", tt.condition, passing)
			}
			pr := basePR()
			pr.Checks = append([]github.ApprovalCheck(nil), pr.Checks...)
			tt.breakPR(&pr)
			result := EvaluateRule(rule, pr)
			got := conditionResult(t, result, tt.condition)
			if got.Pass || result.Matched {
				t.Fatalf("broken %s passed: %+v", tt.condition, result)
			}
			if !strings.Contains(got.Saw, tt.wantSaw) {
				t.Fatalf("saw = %q, want it to contain %q", got.Saw, tt.wantSaw)
			}
		})
	}
}

func TestUnsetConditionsAreNotEvaluated(t *testing.T) {
	result := EvaluateRule(config.ApproveRule{Name: "bare"}, basePR())
	if len(result.Conditions) != 1 || result.Conditions[0].Condition != "max_unresolved_threads" || !result.Matched {
		t.Fatalf("result = %+v", result)
	}
}

func TestRequiredApprovalsBy(t *testing.T) {
	rule := config.ApproveRule{Name: "bot", RequiredApprovalsBy: []string{"review-bot"}}
	tests := []struct {
		name    string
		reviews []github.ApprovalReview
		want    bool
	}{
		{"approved on older SHA", []github.ApprovalReview{{Author: "review-bot", State: "APPROVED", CommitSHA: "h0"}}, false},
		{"approved on head", []github.ApprovalReview{{Author: "review-bot", State: "APPROVED", CommitSHA: "h1"}}, true},
		{"login case differs", []github.ApprovalReview{{Author: "Review-Bot", State: "APPROVED", CommitSHA: "h1"}}, true},
		{"changes requested after approval on head", []github.ApprovalReview{
			{Author: "review-bot", State: "APPROVED", CommitSHA: "h1"},
			{Author: "review-bot", State: "CHANGES_REQUESTED", CommitSHA: "h1"},
		}, false},
		{"comment after approval on head", []github.ApprovalReview{
			{Author: "review-bot", State: "APPROVED", CommitSHA: "h1"},
			{Author: "review-bot", State: "COMMENTED", CommitSHA: "h1"},
		}, true},
		{"approval by someone else", []github.ApprovalReview{{Author: "carol", State: "APPROVED", CommitSHA: "h1"}}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pr := basePR()
			pr.Reviews = tt.reviews
			got := conditionResult(t, EvaluateRule(rule, pr), "required_approvals_by")
			if got.Pass != tt.want {
				t.Fatalf("pass = %v, want %v (saw %q)", got.Pass, tt.want, got.Saw)
			}
		})
	}
}

func TestChecksCondition(t *testing.T) {
	pending := []github.ApprovalCheck{
		{Name: "ci", Required: true, Outcome: models.ChecksPass},
		{Name: "e2e", Required: true, Outcome: models.ChecksPending},
	}
	failed := []github.ApprovalCheck{
		{Name: "ci", Required: true, Outcome: models.ChecksPass},
		{Name: "lint", Outcome: models.ChecksFail},
	}
	optionalPending := []github.ApprovalCheck{
		{Name: "ci", Required: true, Outcome: models.ChecksPass},
		{Name: "slow", Outcome: models.ChecksPending},
	}
	noneRequiredPending := []github.ApprovalCheck{{Name: "ci", Outcome: models.ChecksPending}}
	tests := []struct {
		name   string
		mode   string
		checks []github.ApprovalCheck
		want   bool
	}{
		{"green pending", config.ChecksGreen, pending, false},
		{"allow_pending pending", config.ChecksAllowPending, pending, true},
		{"green failed", config.ChecksGreen, failed, false},
		{"allow_pending failed", config.ChecksAllowPending, failed, false},
		{"green optional pending", config.ChecksGreen, optionalPending, true},
		{"green no required check pending", config.ChecksGreen, noneRequiredPending, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pr := basePR()
			pr.Checks = tt.checks
			got := conditionResult(t, EvaluateRule(config.ApproveRule{Name: "c", Checks: tt.mode}, pr), "checks")
			if got.Pass != tt.want {
				t.Fatalf("pass = %v, want %v (saw %q)", got.Pass, tt.want, got.Saw)
			}
		})
	}
}

func TestUnreadableDataFailsWithReason(t *testing.T) {
	tests := []struct {
		condition string
		rule      config.ApproveRule
		breakPR   func(pr *github.ApprovalPullRequest)
	}{
		{"max_unresolved_threads", config.ApproveRule{MaxUnresolvedThreads: 5}, func(pr *github.ApprovalPullRequest) {
			pr.ThreadsErr = "review thread data missing"
		}},
		{"checks", config.ApproveRule{Checks: config.ChecksAllowPending}, func(pr *github.ApprovalPullRequest) {
			pr.Checks = nil
			pr.ChecksErr = "check data missing on head commit"
		}},
		{"paths_all_match", config.ApproveRule{PathsAllMatch: []string{"**"}}, func(pr *github.ApprovalPullRequest) {
			pr.Files = nil
			pr.FilesErr = "file list incomplete"
		}},
		{"paths_none_match", config.ApproveRule{PathsNoneMatch: []string{"infra/**"}}, func(pr *github.ApprovalPullRequest) {
			pr.Files = nil
			pr.FilesErr = "file list incomplete"
		}},
		{"labels_forbidden", config.ApproveRule{LabelsForbidden: []string{"wip"}}, func(pr *github.ApprovalPullRequest) {
			pr.Labels = nil
			pr.LabelsErr = "label list incomplete"
		}},
	}
	for _, tt := range tests {
		t.Run(tt.condition, func(t *testing.T) {
			pr := basePR()
			tt.breakPR(&pr)
			tt.rule.Name = "r"
			result := EvaluateRule(tt.rule, pr)
			got := conditionResult(t, result, tt.condition)
			if got.Pass || result.Matched || !strings.Contains(got.Saw, "unreadable") {
				t.Fatalf("result = %+v", got)
			}
		})
	}
}

func TestEvaluateANDsWithinRuleAndORsAcrossRules(t *testing.T) {
	pr := basePR()
	small := config.ApproveRule{Name: "small", MaxChangedLines: intPtr(1), AuthorsAllow: []string{"alice"}}
	deletion := config.ApproveRule{Name: "deletion", PureDeletion: true, LabelsRequired: []string{"missing"}}
	dashboard := config.ApproveRule{Name: "dashboard", PathsAllMatch: []string{"dash/**"}}

	results, matched := Evaluate([]config.ApproveRule{small, deletion}, pr)
	if matched != "" || len(results) != 2 || results[0].Matched || results[1].Matched {
		t.Fatalf("AND: matched = %q results = %+v", matched, results)
	}
	_, matched = Evaluate([]config.ApproveRule{small, deletion, dashboard}, pr)
	if matched != "dashboard" {
		t.Fatalf("OR: matched = %q, want dashboard", matched)
	}
	_, matched = Evaluate([]config.ApproveRule{dashboard, {Name: "also"}}, pr)
	if matched != "dashboard" {
		t.Fatalf("first matching rule = %q, want dashboard", matched)
	}
}

func TestFirstFailingConditionPerRule(t *testing.T) {
	pr := basePR()
	pr.Author = "bob"
	pr.Draft = true
	rule := config.ApproveRule{Name: "r", AuthorsAllow: []string{"alice"}, NotDraft: true, MaxChangedLines: intPtr(1)}
	first, ok := EvaluateRule(rule, pr).FirstFailure()
	if !ok || first.Condition != "max_changed_lines" {
		t.Fatalf("first failure = %+v, want max_changed_lines", first)
	}
	if _, ok := EvaluateRule(config.ApproveRule{Name: "ok"}, pr).FirstFailure(); ok {
		t.Fatal("matching rule reported a failure")
	}
}
