package approve

import (
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func viewedFile(path, state string) models.File {
	return models.File{Path: path, ViewerViewedState: state}
}

func viewedPR(n int, files ...models.File) github.ApprovalPullRequest {
	pr := prNumber(n)
	pr.Files = files
	pr.NodeID = "PR_node"
	return pr
}

func lockRules() config.ViewedConfig {
	return config.ViewedConfig{
		Enabled:        true,
		MaxMarksPerRun: config.DefaultMaxMarksPerRun,
		Rules: []config.ViewedRule{{
			Name:       "locks",
			PathsMatch: []string{"**/*.lock", "**/go.sum", "**/pnpm-lock.yaml"},
		}},
	}
}

func runViewed(t *testing.T, fake *fakeClient, viewed config.ViewedConfig, opts Options) runResult {
	t.Helper()
	return runConfig(t, fake, config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true}, viewed, opts, nil)
}

func TestViewedMarksOnlyUnviewed(t *testing.T) {
	pr := viewedPR(1,
		viewedFile("go.sum", models.ViewerUnviewed),
		viewedFile("pnpm-lock.yaml", models.ViewerViewed),
		viewedFile("yarn.lock", models.ViewerDismissed),
		viewedFile("other.lock", models.ViewerUnviewed),
	)
	res := runViewed(t, newFake(pr), lockRules(), Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionMarkViewed || strings.Join(res.log[0].Paths, ",") != "go.sum,other.lock" {
		t.Fatalf("entry = %+v", res.log[0])
	}
	if len(res.fake.marks) != 2 || res.fake.marks[0].path != "go.sum" || res.fake.marks[0].id != "PR_node" || res.fake.marks[1].path != "other.lock" {
		t.Fatalf("marks = %+v", res.fake.marks)
	}
	if !strings.Contains(res.out, "mark_viewed: 2 files") {
		t.Fatalf("out = %s", res.out)
	}
}

func TestViewedDryRunIssuesNoMutations(t *testing.T) {
	pr := viewedPR(1, viewedFile("go.sum", models.ViewerUnviewed), viewedFile("yarn.lock", models.ViewerUnviewed))
	res := runViewed(t, newFake(pr), lockRules(), Options{Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.marks) != 0 || res.fake.headReads != 0 {
		t.Fatalf("dry run marked %+v (head reads %d)", res.fake.marks, res.fake.headReads)
	}
	if res.log[0].Action != ActionWouldMarkViewed || strings.Join(res.log[0].Paths, ",") != "go.sum,yarn.lock" {
		t.Fatalf("entry = %+v", res.log[0])
	}
	if !strings.Contains(res.out, "would_mark_viewed: 2 files") || !strings.Contains(res.out, "go.sum") {
		t.Fatalf("out = %s", res.out)
	}
	plain := runViewed(t, newFake(pr), lockRules(), Options{})
	if strings.Contains(plain.out, "      go.sum") {
		t.Fatalf("paths printed without --explain:\n%s", plain.out)
	}
}

func TestViewedApplyHonoursCapAndMovedHead(t *testing.T) {
	first := viewedPR(1,
		viewedFile("a.lock", models.ViewerUnviewed),
		viewedFile("b.lock", models.ViewerUnviewed),
		viewedFile("c.lock", models.ViewerUnviewed),
	)
	second := viewedPR(2, viewedFile("d.lock", models.ViewerUnviewed))
	viewed := lockRules()
	viewed.MaxMarksPerRun = 2
	res := runViewed(t, newFake(first, second), viewed, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionMarkViewed || strings.Join(res.log[0].Paths, ",") != "a.lock,b.lock" || res.log[0].Reason != "mark cap reached" {
		t.Fatalf("first = %+v", res.log[0])
	}
	if res.log[1].Action != ActionManual || len(res.log[1].Paths) != 0 {
		t.Fatalf("second = %+v", res.log[1])
	}
	if len(res.fake.marks) != 2 {
		t.Fatalf("marks = %+v", res.fake.marks)
	}

	moved := newFake(first)
	moved.heads = map[int]string{1: "h2"}
	res = runViewed(t, moved, lockRules(), Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionSkipped || !strings.Contains(res.log[0].Reason, "head moved") || len(res.fake.marks) != 0 {
		t.Fatalf("moved = %+v marks %+v", res.log[0], res.fake.marks)
	}
}

func TestViewedReposFilter(t *testing.T) {
	mine := viewedPR(1, viewedFile("go.sum", models.ViewerUnviewed))
	other := viewedPR(2, viewedFile("go.sum", models.ViewerUnviewed))
	other.Ref.Owner, other.Ref.Repo = "other", "lib"
	viewed := lockRules()
	viewed.Rules[0].Repos = []string{"acme/widgets"}
	res := runViewed(t, newFake(mine, other), viewed, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionMarkViewed || res.log[1].Action != ActionManual || len(res.fake.marks) != 1 || res.fake.marks[0].ref.Repo != "widgets" {
		t.Fatalf("log = %+v marks %+v", res.log, res.fake.marks)
	}
}

func TestViewedUnreadableCodeownersMarksNothing(t *testing.T) {
	pr := viewedPR(1, viewedFile("go.sum", models.ViewerUnviewed), viewedFile("other/x.go", models.ViewerUnviewed))
	fake := newFake(pr)
	fake.codeownersErr = errors.New("CODEOWNERS missing")
	viewed := lockRules()
	viewed.Rules = append(viewed.Rules, config.ViewedRule{Name: "not-mine", CodeownersNotMine: true})
	res := runViewed(t, fake, viewed, Options{Apply: true})
	if res.err == nil || res.log[0].Action != ActionError || len(res.fake.marks) != 0 {
		t.Fatalf("err = %v entry %+v marks %+v", res.err, res.log[0], res.fake.marks)
	}
}

func TestViewedCodeownersNotMine(t *testing.T) {
	pr := viewedPR(1,
		viewedFile("dash/a.go", models.ViewerUnviewed),
		viewedFile("other/x.go", models.ViewerUnviewed),
	)
	fake := newFake(pr)
	fake.codeowners = testCodeowners
	viewed := config.ViewedConfig{
		Enabled: true, MaxMarksPerRun: 10,
		Rules: []config.ViewedRule{{Name: "not-mine", CodeownersNotMine: true}},
	}
	res := runViewed(t, fake, viewed, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if strings.Join(res.log[0].Paths, ",") != "other/x.go" {
		t.Fatalf("paths = %+v", res.log[0].Paths)
	}
}

func TestViewedPathsNoneMatchLetsAnotherRuleMark(t *testing.T) {
	pr := viewedPR(1,
		viewedFile("docs/guide.md", models.ViewerUnviewed),
		viewedFile("other/x.go", models.ViewerUnviewed),
		viewedFile("dash/a.go", models.ViewerUnviewed),
	)
	fake := newFake(pr)
	fake.codeowners = testCodeowners
	fake.teams = map[string][]string{"acme/everyone": {"bob", "carol"}}
	viewed := config.ViewedConfig{
		Enabled: true, MaxMarksPerRun: 10,
		Rules: []config.ViewedRule{
			{Name: "not-mine", CodeownersNotMine: true, PathsNoneMatch: []string{"docs/**"}},
			{Name: "docs", PathsMatch: []string{"docs/**"}},
		},
	}
	res := runViewed(t, fake, viewed, Options{Explain: true, Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if strings.Join(res.log[0].Paths, ",") != "docs/guide.md,other/x.go" {
		t.Fatalf("paths = %+v", res.log[0].Paths)
	}
	if strings.Contains(res.out, "excluded by") {
		t.Fatalf("a file another rule marked was explained as excluded:\n%s", res.out)
	}

	viewed.Rules = viewed.Rules[:1]
	again := newFake(pr)
	again.codeowners = testCodeowners
	again.teams = map[string][]string{"acme/everyone": {"bob", "carol"}}
	res = runViewed(t, again, viewed, Options{Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if strings.Join(res.log[0].Paths, ",") != "other/x.go" || len(res.fake.marks) != 0 {
		t.Fatalf("paths = %+v marks %+v", res.log[0].Paths, res.fake.marks)
	}
	if !strings.Contains(res.out, "docs/guide.md: excluded by rule not-mine paths_none_match docs/**") {
		t.Fatalf("explain = %s", res.out)
	}
	if len(res.log[0].Exclusions) != 0 {
		t.Fatalf("audit log recorded the exclusion: %+v", res.log[0].Exclusions)
	}
	plainFake := newFake(pr)
	plainFake.codeowners = testCodeowners
	plainFake.teams = again.teams
	plain := runViewed(t, plainFake, viewed, Options{})
	if strings.Contains(plain.out, "excluded by") {
		t.Fatalf("exclusion printed without --explain:\n%s", plain.out)
	}
}

func TestViewedNeverMarkBlocksEveryRule(t *testing.T) {
	pr := viewedPR(1,
		viewedFile("a.go", models.ViewerUnviewed),
		viewedFile("docs/b.go", models.ViewerUnviewed),
		viewedFile("README.md", models.ViewerUnviewed),
	)
	viewed := config.ViewedConfig{
		Enabled: true, MaxMarksPerRun: 10,
		NeverMark: []string{"**/*.go", "**/*.py", "**/*.proto"},
		Rules: []config.ViewedRule{{
			Name: "broad", PathsMatch: []string{"**/*"}, PathsNoneMatch: []string{"docs/**"},
		}},
	}
	res := runViewed(t, newFake(pr), viewed, Options{Explain: true, Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if strings.Join(res.log[0].Paths, ",") != "README.md" || len(res.fake.marks) != 1 || res.fake.marks[0].path != "README.md" {
		t.Fatalf("paths = %+v marks %+v", res.log[0].Paths, res.fake.marks)
	}
	if !strings.Contains(res.out, "a.go: excluded by viewed.never_mark **/*.go") {
		t.Fatalf("explain = %s", res.out)
	}
	if !strings.Contains(res.out, "docs/b.go: excluded by viewed.never_mark **/*.go") {
		t.Fatalf("never_mark lost to the rule exclusion:\n%s", res.out)
	}
	if strings.Contains(res.out, "paths_none_match") {
		t.Fatalf("rule exclusion won over never_mark:\n%s", res.out)
	}
	if len(res.log[0].Exclusions) != 0 {
		t.Fatalf("audit log recorded exclusions: %+v", res.log[0].Exclusions)
	}
}

func TestViewedSkipsMatchedDraftAndOwnPullRequests(t *testing.T) {
	matched := viewedPR(1, viewedFile("dash/a.go", models.ViewerUnviewed))
	res := runConfig(t, newFake(matched), config.ApproveConfig{
		Body: "LGTM", SkipAlreadyApproved: true, Rules: []config.ApproveRule{dashboardRule},
	}, lockRules(), Options{}, nil)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionWouldApprove || len(res.fake.marks) != 0 {
		t.Fatalf("matched = %+v marks %+v", res.log[0], res.fake.marks)
	}

	draft := viewedPR(1, viewedFile("go.sum", models.ViewerUnviewed))
	draft.Draft = true
	res = runViewed(t, newFake(draft), lockRules(), Options{Apply: true})
	if res.log[0].Action != ActionManual || len(res.fake.marks) != 0 {
		t.Fatalf("draft = %+v marks %+v", res.log[0], res.fake.marks)
	}

	own := viewedPR(1, viewedFile("go.sum", models.ViewerUnviewed))
	own.Author = "me"
	res = runViewed(t, newFake(own), lockRules(), Options{Apply: true})
	if res.log[0].Action != ActionSkipped || len(res.fake.marks) != 0 {
		t.Fatalf("own = %+v marks %+v", res.log[0], res.fake.marks)
	}
}
