package approve

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

func fcFile(path string, adds, dels int) models.File {
	return models.File{
		Path:              path,
		Additions:         adds,
		Deletions:         dels,
		ViewerViewedState: models.ViewerUnviewed,
	}
}

func fcPatch(path, patch string, adds, dels int) github.PullFile {
	return github.PullFile{
		Path:      path,
		Additions: adds,
		Deletions: dels,
		Patch:     patch,
		PatchSet:  true,
	}
}

func TestFileChecksOnlyDeletions(t *testing.T) {
	rule := config.ApproveRule{
		Name: "baseline",
		FileChecks: map[string]config.FileCheck{
			"**/baseline.txt": {Builtin: config.FileCheckOnlyDeletions},
		},
	}
	tests := []struct {
		name string
		file models.File
		want bool
	}{
		{name: "only deletions passes", file: fcFile("a/baseline.txt", 0, 5), want: true},
		{name: "added line defers", file: fcFile("a/baseline.txt", 1, 5), want: false},
		{name: "zero deletions defers", file: fcFile("a/baseline.txt", 0, 0), want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			pr := prNumber(1)
			pr.Files = []models.File{tc.file}
			got := conditionResult(t, EvaluateRule(rule, pr), fileChecksCondition)
			if got.Pass != tc.want {
				t.Fatalf("Pass = %v, want %v (saw=%q details=%v)", got.Pass, tc.want, got.Saw, got.Details)
			}
		})
	}
}

func TestFileChecksDeletionsAndCommentsOnly(t *testing.T) {
	rule := config.ApproveRule{
		Name: "comments",
		FileChecks: map[string]config.FileCheck{
			"*.go": {Builtin: config.FileCheckDeletionsAndCommentsOnly},
		},
	}
	pr := prNumber(1)
	pr.Files = []models.File{fcFile("main.go", 1, 2)}
	pr.Patches = []github.PullFile{
		fcPatch("main.go", "@@ -1,3 +1,2 @@\n-old1\n-old2\n+// note\n", 1, 2),
	}
	got := conditionResult(t, EvaluateRule(rule, pr), fileChecksCondition)
	if !got.Pass {
		t.Fatalf("Pass = false, saw=%q details=%v", got.Saw, got.Details)
	}

	pr.Patches = []github.PullFile{
		fcPatch("main.go", "@@ -1,3 +1,2 @@\n-old1\n-old2\n+code()\n", 1, 2),
	}
	got = conditionResult(t, EvaluateRule(rule, pr), fileChecksCondition)
	if got.Pass {
		t.Fatalf("Pass = true on added code, saw=%q", got.Saw)
	}
}

func TestFileChecksSpecificityWins(t *testing.T) {
	rule := config.ApproveRule{
		Name: "spec",
		FileChecks: map[string]config.FileCheck{
			"**/baseline.txt":            {Builtin: config.FileCheckOnlyDeletions},
			"checkers/size/baseline.txt": {Argv: []string{"./check-shrink"}},
		},
	}
	pr := prNumber(1)
	pr.Files = []models.File{
		fcFile("checkers/size/baseline.txt", 1, 1),
		fcFile("other/baseline.txt", 0, 2),
	}
	pr.Patches = []github.PullFile{
		fcPatch("checkers/size/baseline.txt", "@@ -1 +1 @@\n-10\n+5\n", 1, 1),
		fcPatch("other/baseline.txt", "@@ -1,2 +0,0 @@\n-a\n-b\n", 0, 2),
	}
	var called []string
	fc := func(r config.ApproveRule, p github.ApprovalPullRequest) (bool, string, []string, []FileCheckPass) {
		return evaluateFileChecks(r, p, func(_ context.Context, _ []string, _ []byte, argv []string) (remotecall.Result, error) {
			called = append(called, argv[0])
			return remotecall.Result{Stdout: "approve\n"}, nil
		}, nil)
	}
	results, matched := evaluateWithFileChecks([]config.ApproveRule{rule}, pr, nil, fc)
	if matched != "spec" || len(called) != 1 || called[0] != "./check-shrink" {
		t.Fatalf("matched=%q called=%v results=%+v", matched, called, results)
	}
}

func TestFileChecksScriptContractAndEnvStdin(t *testing.T) {
	rule := config.ApproveRule{
		Name:              "script",
		FileChecksTimeout: 50 * time.Millisecond,
		FileChecks: map[string]config.FileCheck{
			"baseline.txt": {Argv: []string{"/bin/check-shrink", "--flag"}},
		},
	}
	pr := prNumber(42)
	pr.Ref = github.PullRef{Owner: "acme", Repo: "widgets", Number: 42}
	pr.HeadSHA = "deadbeef"
	pr.BaseRef = "main"
	pr.Files = []models.File{fcFile("baseline.txt", 1, 1)}
	diff := "@@ -1 +1 @@\n-10\n+5\n"
	pr.Patches = []github.PullFile{fcPatch("baseline.txt", diff, 1, 1)}

	t.Run("env and stdin reach script", func(t *testing.T) {
		var gotEnv []string
		var gotStdin string
		var gotArgv []string
		out := runScriptFileCheck(rule, pr, pr.Files[0], "baseline.txt", rule.FileChecks["baseline.txt"].Argv,
			map[string]github.PullFile{"baseline.txt": pr.Patches[0]},
			func(_ context.Context, env []string, stdin []byte, argv []string) (remotecall.Result, error) {
				gotEnv = env
				gotStdin = string(stdin)
				gotArgv = argv
				return remotecall.Result{Stdout: "approve ok\n", Stderr: "checked 1 line\n"}, nil
			}, nil)
		if !out.Pass || out.Stderr != "checked 1 line" {
			t.Fatalf("out = %+v", out)
		}
		if gotStdin != diff {
			t.Fatalf("stdin = %q, want %q", gotStdin, diff)
		}
		if !slices.Equal(gotArgv, []string{"/bin/check-shrink", "--flag"}) {
			t.Fatalf("argv = %v", gotArgv)
		}
		for _, want := range []string{
			"GH_APPROVE_REPO=acme/widgets",
			"GH_APPROVE_PR=42",
			"GH_APPROVE_PATH=baseline.txt",
			"GH_APPROVE_HEAD_SHA=deadbeef",
			"GH_APPROVE_BASE_REF=main",
		} {
			if !slices.Contains(gotEnv, want) {
				t.Fatalf("env %v missing %q", gotEnv, want)
			}
		}
	})

	cases := []struct {
		name     string
		res      remotecall.Result
		err      error
		wantPass bool
		wantWhy  string
	}{
		{name: "approve", res: remotecall.Result{Stdout: "approve\n"}, wantPass: true},
		{name: "defer", res: remotecall.Result{Stdout: "defer raised number\n"}, wantPass: false, wantWhy: "raised number"},
		{name: "other output", res: remotecall.Result{Stdout: "maybe\n"}, wantPass: false, wantWhy: "unexpected output"},
		{name: "empty output", res: remotecall.Result{Stdout: "  \n"}, wantPass: false, wantWhy: "empty stdout"},
		{name: "non-zero exit", res: remotecall.Result{Stdout: "approve\n", Code: 2}, wantPass: false, wantWhy: "exited 2"},
		{name: "timeout", res: remotecall.Result{TimedOut: true, Code: -1}, wantPass: false, wantWhy: "timed out"},
		{name: "start error", err: errors.New("not found"), wantPass: false, wantWhy: "not found"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			out := runScriptFileCheck(rule, pr, pr.Files[0], "baseline.txt", rule.FileChecks["baseline.txt"].Argv,
				map[string]github.PullFile{"baseline.txt": pr.Patches[0]},
				func(_ context.Context, _ []string, _ []byte, _ []string) (remotecall.Result, error) {
					return tc.res, tc.err
				}, nil)
			if out.Pass != tc.wantPass {
				t.Fatalf("Pass = %v, want %v (reason=%q)", out.Pass, tc.wantPass, out.Reason)
			}
			if tc.wantWhy != "" && !strings.Contains(out.Reason, tc.wantWhy) {
				t.Fatalf("Reason = %q, want %q", out.Reason, tc.wantWhy)
			}
		})
	}
}

func TestFileChecksUncoveredFallbackAndAllCovered(t *testing.T) {
	maxLines := 10
	rule := config.ApproveRule{
		Name:            "mixed",
		PathsAllMatch:   []string{"src/**"},
		MaxChangedLines: &maxLines,
		FileChecks: map[string]config.FileCheck{
			"**/baseline.txt": {Builtin: config.FileCheckOnlyDeletions},
		},
	}

	t.Run("uncovered passes other conditions while covered ignores max_changed_lines", func(t *testing.T) {
		pr := prNumber(1)
		pr.Files = []models.File{
			fcFile("gen/baseline.txt", 0, 500), // outside src/** and > 10 lines, but covered!
			fcFile("src/app.go", 3, 2),         // uncovered, inside src/**, 5 lines <= 10
		}
		res := EvaluateRule(rule, pr)
		if !res.Matched {
			t.Fatalf("expected match, got %+v", res.Conditions)
		}
	})

	t.Run("uncovered fails other conditions", func(t *testing.T) {
		pr := prNumber(1)
		pr.Files = []models.File{
			fcFile("gen/baseline.txt", 0, 50),
			fcFile("docs/readme.md", 1, 0), // uncovered, outside src/**
		}
		res := EvaluateRule(rule, pr)
		if res.Matched {
			t.Fatalf("expected failure on uncovered file")
		}
	})

	t.Run("all files covered needs no other file condition", func(t *testing.T) {
		pr := prNumber(1)
		pr.Files = []models.File{fcFile("gen/baseline.txt", 0, 50)}
		res := EvaluateRule(rule, pr)
		if !res.Matched {
			t.Fatalf("expected match when all files covered, got %+v", res.Conditions)
		}
	})

	t.Run("one covered file deferring defers PR", func(t *testing.T) {
		pr := prNumber(1)
		pr.Files = []models.File{
			fcFile("gen/baseline.txt", 1, 5), // adds 1 line -> defers
			fcFile("src/app.go", 1, 0),
		}
		res := EvaluateRule(rule, pr)
		if res.Matched {
			t.Fatalf("expected defer when covered file fails")
		}
	})
}

func TestFileChecksCacheReuse(t *testing.T) {
	now := time.Date(2026, 11, 18, 12, 0, 0, 0, time.UTC)
	cache := &AgentCache{
		Dir:      t.TempDir(),
		TTL:      time.Hour,
		MaxBytes: 1 << 20,
		Now:      func() time.Time { return now },
	}
	rule := config.ApproveRule{
		Name: "cached",
		FileChecks: map[string]config.FileCheck{
			"baseline.txt": {Argv: []string{"./check"}},
		},
	}
	pr := prNumber(1)
	pr.Files = []models.File{fcFile("baseline.txt", 1, 1)}
	pr.Patches = []github.PullFile{fcPatch("baseline.txt", "@@ -1 +1 @@\n-10\n+5\n", 1, 1)}

	calls := 0
	run := func(_ context.Context, _ []string, _ []byte, _ []string) (remotecall.Result, error) {
		calls++
		return remotecall.Result{Stdout: "approve\n", Stderr: "cached stderr\n"}, nil
	}

	first := evaluateFileChecksResult(rule, pr, run, cache)
	second := evaluateFileChecksResult(rule, pr, run, cache)
	if !first.Pass || !second.Pass || calls != 1 {
		t.Fatalf("first=%v second=%v calls=%d, want 1 call", first.Pass, second.Pass, calls)
	}
	if !strings.Contains(strings.Join(second.Details, "\n"), "cached stderr") {
		t.Fatalf("cached stderr missing from details: %v", second.Details)
	}
}

func evaluateFileChecksResult(rule config.ApproveRule, pr github.ApprovalPullRequest, run FileCheckRunner, cache *AgentCache) ConditionResult {
	pass, saw, details, _ := evaluateFileChecks(rule, pr, run, cache)
	return ConditionResult{Condition: fileChecksCondition, Pass: pass, Saw: saw, Details: details}
}

func TestFileChecksExplainAndMarkViewed(t *testing.T) {
	makeRule := func(markViewed bool) config.ApproveRule {
		return config.ApproveRule{
			Name:             "fc-rule",
			PathsAllMatch:    []string{"src/**"},
			MarkViewedOnPass: markViewed,
			FileChecks: map[string]config.FileCheck{
				"**/baseline.txt": {Builtin: config.FileCheckOnlyDeletions},
				"checkers/*.txt":  {Argv: []string{"./check-shrink"}},
			},
		}
	}

	t.Run("passing file marked on approved PR and explain lists all files", func(t *testing.T) {
		pr := prNumber(1)
		pr.NodeID = "PR_1"
		pr.Files = []models.File{
			fcFile("gen/baseline.txt", 0, 3),
			fcFile("src/main.go", 2, 1),
		}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{MaxMarksPerRun: config.DefaultMaxMarksPerRun}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true, Explain: true}, nil)
		if res.err != nil {
			t.Fatal(res.err)
		}
		if res.log[0].Action != ActionApproved {
			t.Fatalf("primary action = %q, want approved", res.log[0].Action)
		}
		if len(res.fake.marks) != 1 || res.fake.marks[0].path != "gen/baseline.txt" {
			t.Fatalf("marks = %+v", res.fake.marks)
		}
		if len(res.log) != 2 || res.log[1].Action != ActionMarkViewed ||
			res.log[1].FileCheckPatterns["gen/baseline.txt"] != "**/baseline.txt" {
			t.Fatalf("log = %+v", res.log)
		}
		for _, want := range []string{
			"pass gen/baseline.txt [**/baseline.txt -> only-deletions]: approve",
			"src/main.go: not covered",
		} {
			if !strings.Contains(res.out, want) {
				t.Fatalf("explain output missing %q:\n%s", want, res.out)
			}
		}
	})

	t.Run("passing file marked on PR left for human", func(t *testing.T) {
		pr := prNumber(2)
		pr.NodeID = "PR_2"
		pr.Files = []models.File{
			fcFile("gen/baseline.txt", 0, 3),
			fcFile("outside/app.go", 1, 0), // fails paths_all_match -> PR left for human
		}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{MaxMarksPerRun: config.DefaultMaxMarksPerRun}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true}, nil)
		if res.err != nil {
			t.Fatal(res.err)
		}
		if res.log[0].Action != ActionMarkViewed || strings.Join(res.log[0].Paths, ",") != "gen/baseline.txt" {
			t.Fatalf("entry = %+v", res.log[0])
		}
		if res.log[0].FileCheckPatterns["gen/baseline.txt"] != "**/baseline.txt" {
			t.Fatalf("patterns = %+v", res.log[0].FileCheckPatterns)
		}
		if len(res.fake.marks) != 1 || res.fake.marks[0].path != "gen/baseline.txt" {
			t.Fatalf("marks = %+v", res.fake.marks)
		}
	})

	t.Run("deferring or erroring file is not marked", func(t *testing.T) {
		pr := prNumber(3)
		pr.NodeID = "PR_3"
		pr.Files = []models.File{
			fcFile("gen/baseline.txt", 1, 3), // defers
			fcFile("checkers/a.txt", 1, 1),   // missing patch -> errors
		}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{MaxMarksPerRun: config.DefaultMaxMarksPerRun}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true}, nil)
		if len(res.fake.marks) != 0 {
			t.Fatalf("marks = %+v, want none", res.fake.marks)
		}
	})

	t.Run("never_mark wins", func(t *testing.T) {
		pr := prNumber(4)
		pr.NodeID = "PR_4"
		pr.Files = []models.File{fcFile("gen/baseline.txt", 0, 3)}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{
			MaxMarksPerRun: config.DefaultMaxMarksPerRun,
			NeverMark:      []string{"**/baseline.txt"},
		}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true, Explain: true}, nil)
		if len(res.fake.marks) != 0 {
			t.Fatalf("marks = %+v, want none", res.fake.marks)
		}
		if !strings.Contains(res.out, "excluded by viewed.never_mark") {
			t.Fatalf("explain output missing exclusion:\n%s", res.out)
		}
	})

	t.Run("dry run reports would_mark_viewed with no API call", func(t *testing.T) {
		pr := prNumber(5)
		pr.NodeID = "PR_5"
		pr.Files = []models.File{
			fcFile("gen/baseline.txt", 0, 3),
			fcFile("outside/app.go", 1, 0),
		}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{MaxMarksPerRun: config.DefaultMaxMarksPerRun}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{}, nil)
		if len(res.fake.marks) != 0 || res.fake.headReads != 0 {
			t.Fatalf("dry run mutated: marks=%+v headReads=%d", res.fake.marks, res.fake.headReads)
		}
		if res.log[0].Action != ActionWouldMarkViewed || !strings.Contains(res.out, "would_mark_viewed: 1 files") {
			t.Fatalf("entry=%+v out=%s", res.log[0], res.out)
		}
	})

	t.Run("shared cap stops marking", func(t *testing.T) {
		pr := prNumber(6)
		pr.NodeID = "PR_6"
		pr.Files = []models.File{
			fcFile("a/baseline.txt", 0, 1),
			fcFile("b/baseline.txt", 0, 1),
			fcFile("go.sum", 1, 1),
		}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{
			Enabled:        true,
			MaxMarksPerRun: 2,
			Rules:          []config.ViewedRule{{Name: "locks", PathsMatch: []string{"go.sum"}}},
		}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true}, nil)
		if len(res.fake.marks) != 2 {
			t.Fatalf("marks = %+v, want 2", res.fake.marks)
		}
		if res.log[0].Reason != "mark cap reached" {
			t.Fatalf("reason = %q, want mark cap reached", res.log[0].Reason)
		}
	})

	t.Run("mark_viewed_on_pass false marks nothing", func(t *testing.T) {
		pr := prNumber(7)
		pr.NodeID = "PR_7"
		pr.Files = []models.File{fcFile("gen/baseline.txt", 0, 3)}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(false)}}
		viewed := config.ViewedConfig{MaxMarksPerRun: config.DefaultMaxMarksPerRun}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true}, nil)
		if len(res.fake.marks) != 0 {
			t.Fatalf("marks = %+v, want none", res.fake.marks)
		}
	})

	t.Run("DISMISSED passing file is re-marked and logged with note", func(t *testing.T) {
		pr := prNumber(8)
		pr.NodeID = "PR_8"
		fDismissed := fcFile("gen/baseline.txt", 0, 3)
		fDismissed.ViewerViewedState = models.ViewerDismissed
		fViewed := fcFile("other/baseline.txt", 0, 2)
		fViewed.ViewerViewedState = models.ViewerViewed
		pr.Files = []models.File{
			fDismissed,
			fViewed,
			fcFile("outside/app.go", 1, 0),
		}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{MaxMarksPerRun: config.DefaultMaxMarksPerRun}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true, Explain: true}, nil)
		if res.err != nil {
			t.Fatal(res.err)
		}
		if len(res.fake.marks) != 1 || res.fake.marks[0].path != "gen/baseline.txt" {
			t.Fatalf("marks = %+v, want only gen/baseline.txt", res.fake.marks)
		}
		if res.log[0].Action != ActionMarkViewed || res.log[0].PathNotes["gen/baseline.txt"] != "re-mark after change" {
			t.Fatalf("entry = %+v", res.log[0])
		}
		if !strings.Contains(res.out, "gen/baseline.txt (re-mark after change)") {
			t.Fatalf("explain output missing re-mark note:\n%s", res.out)
		}
	})

	t.Run("DISMISSED file matching never_mark is not marked", func(t *testing.T) {
		pr := prNumber(9)
		pr.NodeID = "PR_9"
		fDismissed := fcFile("gen/baseline.txt", 0, 3)
		fDismissed.ViewerViewedState = models.ViewerDismissed
		pr.Files = []models.File{fDismissed}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{
			MaxMarksPerRun: config.DefaultMaxMarksPerRun,
			NeverMark:      []string{"**/baseline.txt"},
		}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true, Explain: true}, nil)
		if len(res.fake.marks) != 0 {
			t.Fatalf("marks = %+v, want none", res.fake.marks)
		}
		if !strings.Contains(res.out, "excluded by viewed.never_mark") {
			t.Fatalf("explain output missing exclusion:\n%s", res.out)
		}
	})

	t.Run("re-marks count toward mark cap", func(t *testing.T) {
		pr := prNumber(10)
		pr.NodeID = "PR_10"
		f1 := fcFile("a/baseline.txt", 0, 1)
		f1.ViewerViewedState = models.ViewerDismissed
		f2 := fcFile("b/baseline.txt", 0, 1)
		f2.ViewerViewedState = models.ViewerDismissed
		pr.Files = []models.File{f1, f2, fcFile("outside/app.go", 1, 0)}
		cfg := config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{makeRule(true)}}
		viewed := config.ViewedConfig{MaxMarksPerRun: 1}
		res := runConfig(t, newFake(pr), cfg, viewed, Options{Apply: true}, nil)
		if len(res.fake.marks) != 1 || res.fake.marks[0].path != "a/baseline.txt" {
			t.Fatalf("marks = %+v, want 1", res.fake.marks)
		}
		if res.log[0].Reason != "mark cap reached" {
			t.Fatalf("reason = %q, want mark cap reached", res.log[0].Reason)
		}
	})
}
