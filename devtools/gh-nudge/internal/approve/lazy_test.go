package approve

import (
	"bytes"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

var coveredRule = config.ApproveRule{
	Name:              "covered",
	CodeownersCovered: true,
	PathsAnyMatch:     []string{"tools/dashboard/**"},
	NotDraft:          true,
}

// secondPR returns a copy of pr as pull request n on the same repository,
// registered with fake.
func secondPR(fake *fakeClient, pr github.ApprovalPullRequest, n int) github.ApprovalPullRequest {
	pr.Ref.Number = n
	fake.prs[n] = pr
	fake.search = append(fake.search, pr.Ref)
	return pr
}

func TestCheapFailureLoadsNothing(t *testing.T) {
	rules := []config.ApproveRule{
		{Name: "mine", MineCommentsOnly: true, AuthorsAllow: []string{"zed"}},
		{Name: "covered", CodeownersCovered: true, Repos: []string{"other/*"}},
		{Name: "dels", DeletionsAndCommentsOnly: true, NotDraft: true, LabelsRequired: []string{"missing"}},
	}
	_, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
	fake.viewerTeams = map[string][]string{"acme": {"core"}}
	res := run(t, fake, rules, Options{Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.codeownersReads != 0 || fake.viewerTeamReads != 0 || fake.teamCountReads != 0 ||
		fake.teamListReads != 0 || fake.patchReads != 0 {
		t.Fatalf("loads: codeowners %d, viewer teams %d, counts %d, lists %d, patches %d",
			fake.codeownersReads, fake.viewerTeamReads, fake.teamCountReads, fake.teamListReads, fake.patchReads)
	}
	if res.log[0].Action != ActionManual {
		t.Fatalf("action = %s", res.log[0].Action)
	}
	cond := conditionResult(t, res.log[0].Rules[0], "mine_comments_only")
	if cond.Pass || !strings.Contains(cond.Saw, "not evaluated") {
		t.Fatalf("mine_comments_only = %+v", cond)
	}
	first, _ := res.log[0].Rules[0].FirstFailure()
	if first.Condition != "authors_allow" {
		t.Fatalf("first failure = %+v, want the cheap authors_allow", first)
	}
}

func TestPassingCheapConditionsLoadOnceAcrossRules(t *testing.T) {
	rules := []config.ApproveRule{
		mineRule,
		{Name: "covered", CodeownersCovered: true, PathsAnyMatch: []string{"deploy/**"}},
		{Name: "dels", DeletionsAndCommentsOnly: true},
	}
	_, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
	res := run(t, fake, rules, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.codeownersReads != 1 || fake.patchReads != 1 {
		t.Fatalf("codeowners %d, patches %d, want one each", fake.codeownersReads, fake.patchReads)
	}
}

func TestCodeownersIsCachedPerRepositoryAndBase(t *testing.T) {
	pr, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
	second := secondPR(fake, pr, 2)
	fake.patches[2] = fake.patches[1]
	other := second
	other.Ref.Number = 3
	other.BaseRef = "release"
	fake.prs[3] = other
	fake.search = append(fake.search, other.Ref)
	fake.patches[3] = fake.patches[1]
	res := run(t, fake, []config.ApproveRule{mineRule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	// Pull requests 1 and 2 share a base, 3 targets another one.
	if fake.codeownersReads != 2 {
		t.Fatalf("codeowners reads = %d, want 2", fake.codeownersReads)
	}
	if len(res.log) != 3 {
		t.Fatalf("log = %+v", res.log)
	}
}

func viewerTeamsFake(pr github.ApprovalPullRequest) *fakeClient {
	fake := handoffFakeFor(pr)
	fake.viewerTeams = map[string][]string{"acme": {"core", "solo"}}
	return fake
}

func coveredPR(n int) github.ApprovalPullRequest {
	pr := handoffPR("tools/dashboard/a.go", "shared/x.go", "solo/y.go")
	pr.Ref.Number = n
	pr.RequestedTeams = []string{"core"}
	return pr
}

func TestTeamMembershipUsesOneViewerTeamsQueryPerOrg(t *testing.T) {
	first := coveredPR(1)
	fake := viewerTeamsFake(first)
	secondPR(fake, first, 2)
	res := run(t, fake, []config.ApproveRule{coveredRule}, Options{Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.viewerTeamReads != 1 {
		t.Fatalf("viewer team reads = %d, want 1", fake.viewerTeamReads)
	}
	if fake.teamListReads != 0 {
		t.Fatalf("member lists read %d times, want none", fake.teamListReads)
	}
	// One count per owning team of the viewer, shared by both pull requests.
	if fake.teamCountReads != 2 {
		t.Fatalf("team count reads = %d, want 2", fake.teamCountReads)
	}
	cond := coverage(t, runResult{log: res.log[:1]})
	if cond.Pass || !strings.Contains(strings.Join(cond.Details, "\n"), "FAIL solo/y.go: blocked by @acme/solo: you are its only member") {
		t.Fatalf("coverage = %+v", cond)
	}
	if !strings.Contains(strings.Join(cond.Details, "\n"), "pass shared/x.go: your owning teams are requested and have other members") {
		t.Fatalf("coverage = %+v", cond)
	}
}

// The client adds the ancestors of the viewer's direct teams to the set, so a
// parent of a child team the viewer is in is mine without a member listing.
func TestParentOfViewersChildTeamIsMine(t *testing.T) {
	pr := handoffPR("widgets/a.go")
	pr.RequestedTeams = []string{"widgets"}
	fake := handoffFakeFor(pr)
	fake.codeowners = "/widgets/** @acme/widgets\n"
	fake.teams["acme/widgets"] = []string{"me", "bob"}
	fake.viewerTeams = map[string][]string{"acme": {"widgets-core", "widgets"}}
	rule := config.ApproveRule{Name: "covered", CodeownersCovered: true}
	cond := coverage(t, run(t, fake, []config.ApproveRule{rule}, Options{Explain: true}))
	if !cond.Pass || !strings.Contains(strings.Join(cond.Details, "\n"), "your owning teams are requested and have other members") {
		t.Fatalf("coverage = %+v", cond)
	}
	if fake.teamListReads != 0 || fake.viewerTeamReads != 1 {
		t.Fatalf("lists %d, viewer teams %d", fake.teamListReads, fake.viewerTeamReads)
	}
}

func TestViewerTeamsFallbackReadsMemberLists(t *testing.T) {
	first := coveredPR(1)
	viaViewer := viewerTeamsFake(first)
	want := coverage(t, run(t, viaViewer, []config.ApproveRule{coveredRule}, Options{Explain: true}))

	fake := handoffFakeFor(first)
	fake.viewerTeamsErr = errors.New("read:org missing")
	res := run(t, fake, []config.ApproveRule{coveredRule}, Options{Explain: true})
	got := coverage(t, res)
	if fake.viewerTeamReads == 0 || fake.teamListReads == 0 || fake.teamCountReads != 0 {
		t.Fatalf("viewer teams %d, lists %d, counts %d", fake.viewerTeamReads, fake.teamListReads, fake.teamCountReads)
	}
	if got.Pass != want.Pass || got.Saw != want.Saw || strings.Join(got.Details, "\n") != strings.Join(want.Details, "\n") {
		t.Fatalf("fallback verdict differs:\n got %+v\nwant %+v", got, want)
	}
}

func TestViewerTeamsTeamCountFailureFailsClosed(t *testing.T) {
	first := coveredPR(1)
	fake := viewerTeamsFake(first)
	fake.teamErr = map[string]error{"acme/core": errors.New("boom")}
	cond := coverage(t, run(t, fake, []config.ApproveRule{coveredRule}, Options{}))
	if cond.Pass {
		t.Fatalf("coverage = %+v", cond)
	}
}

func TestPatchesAboveTheCapAreNotFetched(t *testing.T) {
	tests := map[string]struct {
		cfg     config.ApproveConfig
		files   int
		added   int
		fetched int
	}{
		"default file cap":       {files: 301, fetched: 0},
		"default added cap":      {files: 1, added: 5001, fetched: 0},
		"under the default caps": {files: 300, added: 5000, fetched: 1},
		"custom file cap":        {cfg: config.ApproveConfig{PatchMaxFiles: 2}, files: 3, fetched: 0},
		"custom added cap":       {cfg: config.ApproveConfig{PatchMaxAddedLines: 10}, files: 1, added: 11, fetched: 0},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			pr := prNumber(1)
			pr.ChangedFiles, pr.Additions = tt.files, tt.added
			fake := newFake(pr)
			fake.patches = map[int][]github.PullFile{1: {patchFile("dash/a.go", "@@ -1 +0,0 @@\n-x := 1\n")}}
			cfg := tt.cfg
			cfg.Body, cfg.SkipAlreadyApproved = "LGTM", true
			cfg.Rules = []config.ApproveRule{{Name: "dels", DeletionsAndCommentsOnly: true}}
			res := runConfig(t, fake, cfg, config.ViewedConfig{}, Options{}, nil)
			if res.err != nil {
				t.Fatal(res.err)
			}
			if fake.patchReads != tt.fetched {
				t.Fatalf("patch reads = %d, want %d", fake.patchReads, tt.fetched)
			}
			cond := conditionResult(t, res.log[0].Rules[0], "deletions_and_comments_only")
			if tt.fetched == 0 && (cond.Pass || !strings.HasPrefix(cond.Saw, "too large to check")) {
				t.Fatalf("condition = %+v", cond)
			}
		})
	}
}

func TestMineCommentsOnlyOverTheCapReportsTooLarge(t *testing.T) {
	pr, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
	pr.ChangedFiles = 400
	fake.prs[1] = pr
	res := run(t, fake, []config.ApproveRule{mineRule}, Options{})
	if fake.patchReads != 0 {
		t.Fatalf("patch reads = %d", fake.patchReads)
	}
	if cond := mineCondition(t, res); cond.Pass || !strings.HasPrefix(cond.Saw, "too large to check") {
		t.Fatalf("condition = %+v", cond)
	}
}

// stepClock moves only when a fake client call advances it.
type stepClock struct{ now time.Time }

func (c *stepClock) Now() time.Time { return c.now }

func (c *stepClock) advance(op string) {
	steps := map[string]time.Duration{
		"pull request": 100 * time.Millisecond,
		"codeowners":   200 * time.Millisecond,
		"teams":        10 * time.Millisecond,
		"patches":      300 * time.Millisecond,
	}
	c.now = c.now.Add(steps[op])
}

func runTimed(t *testing.T, fake *fakeClient, rules []config.ApproveRule, explain bool) string {
	t.Helper()
	clock := &stepClock{now: fixedNow()}
	fake.onCall = clock.advance
	fake.handoffFake.onCall = clock.advance
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: rules},
		Out:    &out,
		Log:    &logBuf,
		Now:    clock.Now,
	}
	if err := runner.Run(Options{Explain: explain}); err != nil {
		t.Fatal(err)
	}
	return out.String()
}

func timingLines(out string) []string {
	var lines []string
	for _, line := range strings.Split(out, "\n") {
		if strings.HasPrefix(strings.TrimSpace(line), "timing:") {
			lines = append(lines, strings.TrimSpace(line))
		}
	}
	return lines
}

func TestExplainPrintsOneTimingLinePerPullRequest(t *testing.T) {
	pr, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
	second := secondPR(fake, pr, 2)
	fake.patches[second.Ref.Number] = fake.patches[1]
	fake.viewerTeams = map[string][]string{"acme": {"core"}}
	out := runTimed(t, fake, []config.ApproveRule{mineRule}, true)
	got := timingLines(out)
	want := []string{
		"timing: total 620ms; pull request 100ms; CODEOWNERS 200ms; teams 20ms; patches 300ms; agent command skipped",
		"timing: total 400ms; pull request 100ms; CODEOWNERS cache hit; teams cache hit; patches 300ms; agent command skipped",
	}
	if len(got) != len(want) {
		t.Fatalf("timing lines = %q\nout:\n%s", got, out)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("timing line %d = %q, want %q", i, got[i], want[i])
		}
	}
}

func TestTimingLineShowsSkippedLoads(t *testing.T) {
	pr := prNumber(1)
	fake := newFake(pr)
	rules := []config.ApproveRule{{Name: "mine", MineCommentsOnly: true, AuthorsAllow: []string{"zed"}}}
	out := runTimed(t, fake, rules, true)
	want := "timing: total 0s; pull request 0s; CODEOWNERS skipped; teams skipped; patches skipped; agent command skipped"
	if got := timingLines(out); len(got) != 1 || got[0] != want {
		t.Fatalf("timing lines = %q, want %q", got, want)
	}
}

func TestTimingLineOnlyWithExplain(t *testing.T) {
	fake := newFake(prNumber(1))
	if got := timingLines(runTimed(t, fake, []config.ApproveRule{dashboardRule}, false)); len(got) != 0 {
		t.Fatalf("timing lines without --explain: %q", got)
	}
}
