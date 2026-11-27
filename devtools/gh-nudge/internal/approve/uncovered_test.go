package approve

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func uncoveredRule(mutate func(*config.ApproveRule)) config.ApproveRule {
	rule := config.ApproveRule{
		Name:           "cleanup",
		LabelsRequired: []string{"cleanup"},
		FileChecks: map[string]config.FileCheck{
			"docs/old.txt": {Builtin: config.FileCheckOnlyDeletions},
		},
	}
	if mutate != nil {
		mutate(&rule)
	}
	return rule
}

func uncoveredPR(paths ...string) []models.File {
	files := []models.File{fcFile("docs/old.txt", 0, 4)}
	for _, path := range paths {
		files = append(files, fcFile(path, 1, 1))
	}
	return files
}

func TestFileChecksFailOnUncoveredFile(t *testing.T) {
	pr := prNumber(1)
	pr.Labels = []string{"cleanup"}
	pr.Files = uncoveredPR("src/main.go")
	res := EvaluateRule(uncoveredRule(nil), pr)
	got := conditionResult(t, res, fileChecksCondition)
	if res.Matched || got.Pass {
		t.Fatalf("rule matched with an uncovered file: %+v", res.Conditions)
	}
	if !strings.Contains(got.Saw, "src/main.go (not covered by file_checks)") {
		t.Fatalf("saw = %q", got.Saw)
	}
	if !slicesContain(got.Details, "FAIL src/main.go: not covered by file_checks") {
		t.Fatalf("details = %v", got.Details)
	}
}

func slicesContain(items []string, want string) bool {
	for _, item := range items {
		if item == want {
			return true
		}
	}
	return false
}

func TestFileChecksAllowUncoveredFileWhenAnotherConditionLimitsIt(t *testing.T) {
	tests := map[string]struct {
		mutate func(*config.ApproveRule)
		want   string
	}{
		"paths_all_match": {
			mutate: func(r *config.ApproveRule) { r.PathsAllMatch = []string{"src/**"} },
			want:   "left to paths_all_match, which passed",
		},
		"codeowners_covered": {
			mutate: func(r *config.ApproveRule) { r.CodeownersCovered = true },
			want:   "left to codeowners_covered, which failed",
		},
		"codeowners_none_mine": {
			mutate: func(r *config.ApproveRule) { r.CodeownersNoneMine = true },
			want:   "left to codeowners_none_mine, which failed",
		},
		"mine_comments_only": {
			mutate: func(r *config.ApproveRule) { r.MineCommentsOnly = true },
			want:   "left to mine_comments_only, which failed",
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			pr := prNumber(1)
			pr.Labels = []string{"cleanup"}
			pr.Files = uncoveredPR("src/main.go")
			got := conditionResult(t, EvaluateRule(uncoveredRule(tt.mutate), pr), fileChecksCondition)
			if !got.Pass {
				t.Fatalf("file_checks failed on its own: %+v", got)
			}
			if !strings.Contains(strings.Join(got.Details, "\n"), "src/main.go: not covered ("+tt.want+")") {
				t.Fatalf("details = %v", got.Details)
			}
		})
	}
}

func TestFileChecksExplainUncoveredFileWhenPathsAllMatchFails(t *testing.T) {
	rule := uncoveredRule(func(r *config.ApproveRule) { r.PathsAllMatch = []string{"docs/**"} })
	pr := prNumber(1)
	pr.Labels = []string{"cleanup"}
	pr.Files = uncoveredPR("src/main.go")
	got := conditionResult(t, EvaluateRule(rule, pr), fileChecksCondition)
	if !strings.Contains(strings.Join(got.Details, "\n"), "src/main.go: not covered (left to paths_all_match, which failed)") {
		t.Fatalf("details = %v", got.Details)
	}
}

func TestFileChecksPathsAllMatchOverCoveredPathsPasses(t *testing.T) {
	rule := uncoveredRule(func(r *config.ApproveRule) { r.PathsAllMatch = []string{"docs/old.txt"} })
	pr := prNumber(1)
	pr.Labels = []string{"cleanup"}
	pr.Files = uncoveredPR()
	if res := EvaluateRule(rule, pr); !res.Matched {
		t.Fatalf("expected match: %+v", res.Conditions)
	}
	pr.Files = uncoveredPR("src/main.go")
	if res := EvaluateRule(rule, pr); res.Matched {
		t.Fatalf("an uncovered file outside paths_all_match matched: %+v", res.Conditions)
	}
}

func TestFileChecksNeverPassWithoutACoveredFile(t *testing.T) {
	rule := uncoveredRule(func(r *config.ApproveRule) { r.PathsAllMatch = []string{"src/**"} })
	pr := prNumber(1)
	pr.Labels = []string{"cleanup"}
	pr.Files = []models.File{fcFile("src/main.go", 1, 1)}
	got := conditionResult(t, EvaluateRule(rule, pr), fileChecksCondition)
	if got.Pass || !strings.Contains(got.Saw, "no changed file is covered by file_checks") {
		t.Fatalf("file_checks = %+v", got)
	}
}

// A covered file that passed is still marked viewed when the rule fails
// because of an uncovered file, as it was when the rule failed for any other
// reason.
func TestCoveredFilesStillMarkedWhenAnUncoveredFileFailsTheRule(t *testing.T) {
	rule := uncoveredRule(func(r *config.ApproveRule) { r.MarkViewedOnPass = true })
	pr := prNumber(1)
	pr.NodeID = "PR_1"
	pr.Labels = []string{"cleanup"}
	pr.Files = uncoveredPR("src/main.go")
	cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{rule}}
	viewed := config.ViewedConfig{MaxMarksPerRun: config.DefaultMaxMarksPerRun}
	res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true, Explain: true}, nil)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionMarkViewed || len(res.fake.approvals) != 0 {
		t.Fatalf("action = %q, approvals = %+v", res.log[0].Action, res.fake.approvals)
	}
	if len(res.fake.marks) != 1 || res.fake.marks[0].path != "docs/old.txt" {
		t.Fatalf("marks = %+v", res.fake.marks)
	}
}

func TestFileChecksWarnings(t *testing.T) {
	alone := uncoveredRule(nil)
	if got := config.FileChecksWarnings([]config.ApproveRule{alone}); len(got) != 1 || !strings.Contains(got[0], `"cleanup"`) {
		t.Fatalf("warnings = %v", got)
	}
	for name, mutate := range map[string]func(*config.ApproveRule){
		"paths_all_match":      func(r *config.ApproveRule) { r.PathsAllMatch = []string{"docs/**"} },
		"codeowners_covered":   func(r *config.ApproveRule) { r.CodeownersCovered = true },
		"codeowners_none_mine": func(r *config.ApproveRule) { r.CodeownersNoneMine = true },
		"mine_comments_only":   func(r *config.ApproveRule) { r.MineCommentsOnly = true },
	} {
		if got := config.FileChecksWarnings([]config.ApproveRule{uncoveredRule(mutate)}); len(got) != 0 {
			t.Fatalf("%s: warnings = %v", name, got)
		}
	}
	if got := config.FileChecksWarnings([]config.ApproveRule{{Name: "plain", NotDraft: true}}); len(got) != 0 {
		t.Fatalf("rule without file_checks: warnings = %v", got)
	}
}
