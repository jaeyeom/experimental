package scan

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	executor "github.com/jaeyeom/go-cmdexec"
)

var update *bool

func TestMain(m *testing.M) {
	update = flag.Bool("update", false, "update golden files")
	os.Exit(m.Run())
}

var fixtureNow = time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

func TestRunGolden(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Repos = []string{"acme/widgets"}
	cfg.GHBin = "gh"
	cfg.HerdrBin = "herdr"

	deps := Deps{GH: fixtureGH{}, Herdr: fixtureHerdr{}}
	doc, err := Run(context.Background(), deps, cfg, nil, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}

	got, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	got = append(got, '\n')

	path := filepath.Join("testdata", "golden", "scan.json")
	if *update {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, got, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := os.ReadFile(path) //nolint:gosec // testdata path
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("golden mismatch\n got:\n%s\nwant:\n%s", got, want)
	}
}

func TestRunHerdrMissingDegrades(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Repos = []string{"acme/widgets"}

	deps := Deps{
		GH:    fixtureGH{},
		Herdr: stubHerdr{minErr: herdr.ErrNotInstalled},
	}
	doc, err := Run(context.Background(), deps, cfg, nil, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(doc.PRs) != 1 {
		t.Fatalf("len(prs) = %d, want 1", len(doc.PRs))
	}
	if doc.PRs[0].Tab != nil {
		t.Fatalf("tab = %+v, want nil", doc.PRs[0].Tab)
	}
	if len(doc.Warnings) != 1 || !bytes.Contains([]byte(doc.Warnings[0]), []byte("herdr unreachable")) {
		t.Fatalf("warnings = %v, want herdr unreachable", doc.Warnings)
	}
	want := Summary{Total: 1, OffMachine: 1}
	if doc.Summary != want {
		t.Fatalf("summary = %+v, want %+v", doc.Summary, want)
	}
}

func TestRunHerdrUnsupported(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Repos = []string{"acme/widgets"}

	deps := Deps{
		GH:    fixtureGH{},
		Herdr: stubHerdr{minErr: herdr.ErrUnsupported},
	}
	doc, err := Run(context.Background(), deps, cfg, nil, fixtureNow)
	if err == nil {
		t.Fatal("Run() error = nil, want unsupported")
	}
	if !errors.Is(err, herdr.ErrUnsupported) {
		t.Fatalf("Run() error = %v, want ErrUnsupported", err)
	}
	if len(doc.PRs) != 1 {
		t.Fatalf("len(prs) = %d, want 1 (partial document)", len(doc.PRs))
	}
	if doc.PRs[0].Tab != nil {
		t.Fatalf("tab = %+v, want nil", doc.PRs[0].Tab)
	}
}

func TestRunCICheckFilter(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		checks []gh.StatusCheck
		ignore []string
		only   []string
		want   string
	}{
		{
			name: "ignore review gates",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Context: "code owners approved", State: "FAILURE"},
				{Context: "review threads answered", State: "PENDING"},
			},
			ignore: []string{"*approved*", "*review*"},
			want:   "green",
		},
		{
			name: "allowlist wins over ignore",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "FAILURE"},
				{Context: "code owners approved", State: "FAILURE"},
			},
			ignore: []string{"build", "*approved*"},
			only:   []string{"build"},
			want:   "failing",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			pr := fixturePRs()[0]
			pr.StatusCheckRollup = tc.checks
			cfg := config.Defaults()
			cfg.Author = "alice"
			cfg.Repos = []string{"acme/widgets"}
			cfg.CIIgnoreChecks = tc.ignore
			cfg.CIOnlyChecks = tc.only

			g := &scriptGH{
				list:    map[string]listResult{"acme/widgets": {prs: []gh.PRListItem{pr}}},
				threads: map[int][]gh.Thread{},
			}
			doc, err := Run(context.Background(), Deps{GH: g, Herdr: fixtureHerdr{}}, cfg, nil, fixtureNow)
			if err != nil {
				t.Fatalf("Run() unexpected error: %v", err)
			}
			if len(doc.PRs) != 1 {
				t.Fatalf("len(prs) = %d, want 1", len(doc.PRs))
			}
			if doc.PRs[0].CIState != tc.want {
				t.Fatalf("CIState = %q, want %q", doc.PRs[0].CIState, tc.want)
			}
		})
	}
}

func TestRunRollupUnavailableWarnsAndContinues(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Repos = []string{"acme/widgets"}
	base := fixturePRs()[0]
	failed := base
	failed.Number = 7
	failed.ReviewRequests = nil
	failed.StatusCheckRollup = nil
	failed.StatusCheckError = "transient upstream error: HTTP 502"
	ok := base
	ok.Number = 9
	ok.ReviewRequests = nil
	ok.StatusCheckRollup = []gh.StatusCheck{{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"}}
	ok.StatusCheckError = ""
	g := &scriptGH{
		list:    map[string]listResult{"acme/widgets": {prs: []gh.PRListItem{failed, ok}}},
		threads: map[int][]gh.Thread{},
	}
	doc, err := Run(context.Background(), Deps{GH: g, Herdr: fixtureHerdr{}}, cfg, nil, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(doc.PRs) != 2 || doc.PRs[0].Number != 7 || doc.PRs[1].Number != 9 {
		t.Fatalf("prs = %+v, want #7 then #9", doc.PRs)
	}
	if doc.PRs[0].CIState != "unknown" || doc.PRs[0].Bucket != "waiting" {
		t.Fatalf("PR 7 = ci %q bucket %q, want unknown and waiting", doc.PRs[0].CIState, doc.PRs[0].Bucket)
	}
	if doc.PRs[1].CIState != "green" {
		t.Fatalf("PR 9 CIState = %q, want green", doc.PRs[1].CIState)
	}
	wantWarn := "ci rollup unavailable for acme/widgets#7: transient upstream error: HTTP 502"
	if len(doc.Warnings) != 1 || doc.Warnings[0] != wantWarn {
		t.Fatalf("warnings = %#v, want [%q]", doc.Warnings, wantWarn)
	}
}

func TestRunInaccessibleRepo(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"

	g := &scriptGH{
		list: map[string]listResult{
			"acme/widgets":  {prs: fixturePRs()},
			"acme/archived": {err: gh.ErrInaccessible},
		},
		threads: map[int][]gh.Thread{123: fixtureThreads()},
	}
	deps := Deps{GH: g, Herdr: fixtureHerdr{}}
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets", "acme/archived"}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(doc.PRs) != 1 {
		t.Fatalf("len(prs) = %d, want 1", len(doc.PRs))
	}
	if len(doc.InaccessibleRepos) != 1 || doc.InaccessibleRepos[0] != "acme/archived" {
		t.Fatalf("inaccessible = %v, want [acme/archived]", doc.InaccessibleRepos)
	}
}

func TestRunMidScanRateLimitEmitsPartial(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"

	g := &scriptGH{
		list: map[string]listResult{
			"acme/widgets": {prs: fixturePRs()},
			"acme/gizmos":  {err: &gh.ProcError{ExitCode: 1, Stderr: "API rate limit exceeded"}},
		},
		threads: map[int][]gh.Thread{123: fixtureThreads()},
	}
	deps := Deps{GH: g, Herdr: fixtureHerdr{}}
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets", "acme/gizmos"}, fixtureNow)
	if err == nil {
		t.Fatal("Run() error = nil, want rate-limit")
	}
	if len(doc.PRs) != 1 {
		t.Fatalf("len(prs) = %d, want 1 classified before failure", len(doc.PRs))
	}
	if doc.Author != "alice" {
		t.Fatalf("author = %q, want alice", doc.Author)
	}
}

func TestRunUnauthenticatedNoDocument(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	g := &scriptGH{authErr: gh.ErrUnauthenticated}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, nil, fixtureNow)
	if err == nil {
		t.Fatal("Run() error = nil, want unauthenticated")
	}
	if errors.Is(err, gh.ErrUnauthenticated) == false {
		t.Fatalf("Run() error = %v, want ErrUnauthenticated", err)
	}
	if doc.Author != "" || len(doc.PRs) != 0 {
		t.Fatalf("document started: author=%q prs=%d", doc.Author, len(doc.PRs))
	}
}

func TestRunGHMissing(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.GHBin = "/missing/gh"
	g := &scriptGH{authErr: &executor.ExecutableNotFoundError{Command: cfg.GHBin}}
	_, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, nil, fixtureNow)
	if err == nil {
		t.Fatal("Run() error = nil, want missing binary")
	}
	var notFound *executor.ExecutableNotFoundError
	if !errors.As(err, &notFound) {
		t.Fatalf("Run() error = %v, want ExecutableNotFoundError", err)
	}
}

func TestRunSearchCapWarning(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	g := &scriptGH{
		search:       []string{"acme/widgets"},
		searchCapped: true,
		list:         map[string]listResult{"acme/widgets": {prs: fixturePRs()}},
		threads:      map[int][]gh.Thread{123: fixtureThreads()},
	}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, nil, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	found := false
	for _, w := range doc.Warnings {
		if w == "search result hit --limit 1000; some repos may be missing" {
			found = true
		}
	}
	if !found {
		t.Fatalf("warnings = %v, missing search cap warning", doc.Warnings)
	}
}

func TestRunRepoOverrideReplacesConfig(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Repos = []string{"acme/ignored"}
	g := &scriptGH{
		list:    map[string]listResult{"acme/widgets": {prs: fixturePRs()}},
		threads: map[int][]gh.Thread{123: fixtureThreads()},
	}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(doc.Repos) != 1 || doc.Repos[0] != "acme/widgets" {
		t.Fatalf("repos = %v, want [acme/widgets]", doc.Repos)
	}
}

func TestStarted(t *testing.T) {
	t.Parallel()
	if Started(Document{}) {
		t.Fatal("empty document should not be started")
	}
	if !Started(Document{Author: "alice"}) {
		t.Fatal("author set should be started")
	}
}

func TestExtractIDNilRegexp(t *testing.T) {
	t.Parallel()
	if got := ExtractID("PROJ-1", nil); got != nil {
		t.Fatalf("ExtractID(nil re) = %v, want nil", got)
	}
}

func TestRunAuthorLastCommentNotBlocking(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	line := 1
	g := &scriptGH{
		list: map[string]listResult{"acme/widgets": {prs: fixturePRs()}},
		threads: map[int][]gh.Thread{123: {{
			ID: "PRRT_own", IsResolved: false,
			Comments: []gh.ThreadComment{{
				ID: "PRRC_own", Author: &gh.ThreadAuthor{Login: "Alice"},
				Path: "a.go", Line: &line, URL: "u", Body: "done",
			}},
		}}},
	}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if doc.PRs[0].Unaddressed {
		t.Fatal("unaddressed = true, want false when last comment is author")
	}
	if len(doc.PRs[0].BlockingComments) != 0 {
		t.Fatalf("blocking = %+v, want empty", doc.PRs[0].BlockingComments)
	}
}

func TestRunDeletedAuthorIsBlocking(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	g := &scriptGH{
		list: map[string]listResult{"acme/widgets": {prs: fixturePRs()}},
		threads: map[int][]gh.Thread{123: {{
			ID: "PRRT_gone", IsResolved: false,
			Comments: []gh.ThreadComment{{
				ID: "PRRC_gone", Author: nil, Path: "a.go", URL: "u", Body: "?",
			}},
		}}},
	}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if !doc.PRs[0].Unaddressed {
		t.Fatal("unaddressed = false, want true when author is deleted")
	}
	if doc.PRs[0].BlockingComments[0].Author != "" {
		t.Fatalf("author = %q, want empty", doc.PRs[0].BlockingComments[0].Author)
	}
}

func TestRunIncludesDrafts(t *testing.T) {
	t.Parallel()

	// dispatch_include_drafts is a dispatch-eligibility switch, not a scan filter.
	// Drafts must stay in the document so tabs --orphans --stdin can see them.
	tests := []struct {
		name          string
		includeDrafts bool
	}{
		{name: "by default", includeDrafts: false},
		{name: "when dispatch_include_drafts is true", includeDrafts: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := config.Defaults()
			cfg.Author = "alice"
			cfg.IncludeDrafts = tc.includeDrafts
			g := &scriptGH{
				list: map[string]listResult{
					"acme/widgets": {prs: append(fixturePRs(), fixtureDraftPR())},
				},
				threads: map[int][]gh.Thread{
					123: fixtureThreads(),
					200: fixtureThreads(),
				},
			}
			doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
			if err != nil {
				t.Fatalf("Run() unexpected error: %v", err)
			}
			if len(doc.PRs) != 2 {
				t.Fatalf("len(prs) = %d, want 2 (draft included)", len(doc.PRs))
			}
			if doc.PRs[0].Number != 123 || doc.PRs[0].IsDraft {
				t.Fatalf("pr = #%d is_draft=%v, want #123 non-draft", doc.PRs[0].Number, doc.PRs[0].IsDraft)
			}
			if doc.PRs[1].Number != 200 || !doc.PRs[1].IsDraft {
				t.Fatalf("second pr = #%d is_draft=%v, want #200 draft", doc.PRs[1].Number, doc.PRs[1].IsDraft)
			}
			if g.threadCalls[200] != 1 {
				t.Fatalf("ReviewThreads(#200) calls = %d, want 1", g.threadCalls[200])
			}
			want := Summary{Total: 2, TabHere: 1, OffMachine: 1, Drafts: 1}
			if doc.Summary != want {
				t.Fatalf("summary = %+v, want %+v", doc.Summary, want)
			}
		})
	}
}

func TestRunTargetsFetchesOnlyNamedPRs(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	item := fixturePRs()[0]
	g := &scriptGH{
		search: []string{"acme/other"},
		list: map[string]listResult{
			"acme/other": {prs: []gh.PRListItem{item}},
		},
		view: map[string]viewResult{
			"acme/widgets#123": {item: item},
			"acme/gone#4":      {err: gh.ErrInaccessible},
		},
		threads: map[int][]gh.Thread{123: fixtureThreads()},
	}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tC", "w2", "PROJ-123")},
		agents: []herdr.Agent{liveAgent("w2:pC", "w2:tC", "idle")},
	}
	doc, err := RunTargets(context.Background(), depsWith(g, h), cfg, []Target{
		{Repo: "acme/widgets", Number: 123},
		{Repo: "acme/widgets", Number: 123},
		{Repo: "acme/missing", Number: 9},
		{Repo: "acme/gone", Number: 4},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("RunTargets() unexpected error: %v", err)
	}
	if g.searchCalls != 0 || g.listCalls != 0 {
		t.Fatalf("searchCalls = %d, listCalls = %d, want 0", g.searchCalls, g.listCalls)
	}
	wantViews := []string{"acme/widgets#123", "acme/missing#9", "acme/gone#4"}
	if !slices.Equal(g.viewCalls, wantViews) {
		t.Fatalf("viewCalls = %v, want %v", g.viewCalls, wantViews)
	}
	if g.threadCalls[123] != 1 || g.threadCalls[9] != 0 || g.threadCalls[4] != 0 {
		t.Fatalf("threadCalls = %v, want only #123", g.threadCalls)
	}
	if len(doc.PRs) != 1 || doc.PRs[0].Number != 123 || doc.PRs[0].Repo != "acme/widgets" {
		t.Fatalf("prs = %+v, want only acme/widgets#123", doc.PRs)
	}
	if doc.PRs[0].Tab == nil || doc.PRs[0].Tab.PaneID == nil || *doc.PRs[0].Tab.PaneID != "w2:pC" {
		t.Fatalf("tab = %+v, want pane w2:pC", doc.PRs[0].Tab)
	}
	if len(doc.InaccessibleRepos) != 1 || doc.InaccessibleRepos[0] != "acme/gone" {
		t.Fatalf("inaccessible = %v, want [acme/gone]", doc.InaccessibleRepos)
	}
	wantRepos := []string{"acme/widgets", "acme/missing", "acme/gone"}
	if !slices.Equal(doc.Repos, wantRepos) {
		t.Fatalf("repos = %v, want %v", doc.Repos, wantRepos)
	}
}

func TestRunTargetsViewError(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	g := &scriptGH{
		view: map[string]viewResult{
			"acme/widgets#123": {err: errors.New("boom")},
		},
	}
	_, err := RunTargets(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []Target{
		{Repo: "acme/widgets", Number: 123},
	}, fixtureNow)
	if err == nil || !strings.Contains(err.Error(), "view pr acme/widgets#123") {
		t.Fatalf("RunTargets() error = %v, want view pr error", err)
	}
}

func TestRunDraftMatchesTab(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	g := &scriptGH{
		list: map[string]listResult{
			"acme/widgets": {prs: []gh.PRListItem{fixtureDraftPR()}},
		},
		threads: map[int][]gh.Thread{200: fixtureThreads()},
	}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tD", "w2", "PROJ-200")},
		agents: []herdr.Agent{liveAgent("w2:pD", "w2:tD", "idle")},
	}
	doc, err := Run(context.Background(), depsWith(g, h), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(doc.PRs) != 1 || !doc.PRs[0].IsDraft || doc.PRs[0].Number != 200 {
		t.Fatalf("prs = %+v, want one draft #200", doc.PRs)
	}
	if doc.PRs[0].Tab == nil || doc.PRs[0].Tab.TabID != "w2:tD" {
		t.Fatalf("tab = %+v, want w2:tD so orphans --stdin treats this as has_open_pr", doc.PRs[0].Tab)
	}
}

type fixtureGH struct{}

func (fixtureGH) AuthStatus(context.Context) error { return nil }

func (fixtureGH) UserLogin(context.Context) (string, error) { return "alice", nil }

func (fixtureGH) SearchOpenPRRepos(context.Context, string) ([]string, bool, error) {
	return []string{"acme/widgets"}, false, nil
}

func (fixtureGH) ListOpenPRs(_ context.Context, repo, _ string) ([]gh.PRListItem, error) {
	if repo != "acme/widgets" {
		return nil, gh.ErrInaccessible
	}
	return fixturePRs(), nil
}

func (fixtureGH) ViewPR(context.Context, string, int) (gh.PRListItem, error) {
	return gh.PRListItem{}, gh.ErrNotFound
}

func (fixtureGH) ReviewThreads(context.Context, string, string, int) ([]gh.Thread, error) {
	return fixtureThreads(), nil
}

func (fixtureGH) MarkerComments(context.Context, string, string, int) (gh.MarkerComments, error) {
	return gh.MarkerComments{}, nil
}

func (fixtureGH) ReviewSnapshot(context.Context, string, int) (string, []gh.LatestReview, error) {
	return "", nil, nil
}

func (fixtureGH) TeamMembers(context.Context, string, string) ([]gh.TeamMember, error) {
	return nil, nil
}

func (fixtureGH) ChangedFiles(context.Context, string, int) ([]string, error) {
	return nil, nil
}

func (fixtureGH) Codeowners(context.Context, string, string) (string, error) {
	return "", nil
}

func (fixtureGH) BranchHead(context.Context, string, string) (string, error) {
	return "base1", nil
}

func (fixtureGH) PathHistories(_ context.Context, _, _ string, _ []string, _ time.Time, batch, _ int) (gh.PathHistoryResult, error) {
	return gh.PathHistoryResult{Histories: map[string]gh.PathHistory{}, Batch: batch}, nil
}

func (fixtureGH) DirectReviewCount(context.Context, string, string) (int, error) {
	return 0, nil
}

func (fixtureGH) OrgActive(context.Context, string, string, time.Time) (bool, error) {
	return true, nil
}

type fixtureHerdr struct{}

func (fixtureHerdr) RequireMin(context.Context, string) error { return nil }

func (fixtureHerdr) TabList(context.Context) ([]herdr.Tab, error) {
	return []herdr.Tab{{
		TabID: "w2:tC", WorkspaceID: "w2", Label: "PROJ-123", AgentStatus: "idle", PaneCount: 1,
	}}, nil
}

func (fixtureHerdr) AgentList(context.Context) ([]herdr.Agent, error) {
	return []herdr.Agent{{
		PaneID: "w2:pC", TabID: "w2:tC", Agent: "codex", AgentStatus: "idle",
	}}, nil
}

type stubHerdr struct {
	minErr  error
	tabs    []herdr.Tab
	agents  []herdr.Agent
	listErr error
}

func (s stubHerdr) RequireMin(context.Context, string) error { return s.minErr }

func (s stubHerdr) TabList(context.Context) ([]herdr.Tab, error) {
	if s.listErr != nil {
		return nil, s.listErr
	}
	return s.tabs, nil
}

func (s stubHerdr) AgentList(context.Context) ([]herdr.Agent, error) {
	if s.listErr != nil {
		return nil, s.listErr
	}
	return s.agents, nil
}

type listResult struct {
	prs []gh.PRListItem
	err error
}

type viewResult struct {
	item gh.PRListItem
	err  error
}

type scriptGH struct {
	authErr       error
	login         string
	loginErr      error
	search        []string
	searchCapped  bool
	searchErr     error
	searchCalls   int
	list          map[string]listResult
	listCalls     int
	view          map[string]viewResult
	viewCalls     []string
	threads       map[int][]gh.Thread
	threadErr     error
	markers       map[int]gh.MarkerComments
	markerErr     error
	threadCalls   map[int]int
	snapshots     map[string]snapResult
	members       map[string]memberCall
	memberCalls   int
	files         map[string][]string
	fileErr       map[string]error
	codeowners    map[string]string
	codeownersErr map[string]error
	history       map[string]gh.PathHistory
	historyErr    error
	historyCalls  int
	historyPaths  [][]string
	historyRefs   []string
	// historyBatches records the batch each history call started at, and
	// historyMax the maximum it could grow to.
	historyBatches []int
	historyMax     []int
	// historyRequests counts the batches history calls fetched.
	historyRequests int
	// historyWorked is the Batch a history call returns; 0 returns the
	// batch asked for.
	historyWorked int
	// historySkip are paths a history call leaves out, as if each failed
	// on its own.
	historySkip map[string]bool
	heads       map[string]string
	headErr     error
	headCalls   int
	coverCalls  int
	fileCalls   int
	load        map[string]int
	loadErr     error
	loadCalls   int
	active      map[string]bool
	activeErr   error
}

type snapResult struct {
	author  string
	reviews []gh.LatestReview
	err     error
}

type memberCall struct {
	members []gh.TeamMember
	err     error
}

func (g *scriptGH) AuthStatus(context.Context) error { return g.authErr }

func (g *scriptGH) UserLogin(context.Context) (string, error) {
	if g.loginErr != nil {
		return "", g.loginErr
	}
	if g.login != "" {
		return g.login, nil
	}
	return "alice", nil
}

func (g *scriptGH) SearchOpenPRRepos(context.Context, string) ([]string, bool, error) {
	g.searchCalls++
	return g.search, g.searchCapped, g.searchErr
}

func (g *scriptGH) ListOpenPRs(_ context.Context, repo, _ string) ([]gh.PRListItem, error) {
	g.listCalls++
	res, ok := g.list[repo]
	if !ok {
		return nil, gh.ErrInaccessible
	}
	return res.prs, res.err
}

func (g *scriptGH) ViewPR(_ context.Context, repo string, number int) (gh.PRListItem, error) {
	key := fmt.Sprintf("%s#%d", repo, number)
	g.viewCalls = append(g.viewCalls, key)
	res, ok := g.view[key]
	if !ok {
		return gh.PRListItem{}, gh.ErrNotFound
	}
	return res.item, res.err
}

func (g *scriptGH) MarkerComments(_ context.Context, _, _ string, number int) (gh.MarkerComments, error) {
	if g.markerErr != nil {
		return gh.MarkerComments{}, g.markerErr
	}
	if g.markers == nil {
		return gh.MarkerComments{}, nil
	}
	return g.markers[number], nil
}

func (g *scriptGH) ReviewThreads(_ context.Context, _, _ string, number int) ([]gh.Thread, error) {
	if g.threadCalls == nil {
		g.threadCalls = map[int]int{}
	}
	g.threadCalls[number]++
	if g.threadErr != nil {
		return nil, g.threadErr
	}
	return g.threads[number], nil
}

// take spends one call of ctx's budget, as gh does for each invocation.
func take(ctx context.Context) error {
	return gh.CallBudgetFrom(ctx).Take() //nolint:wrapcheck // the fake returns gh's error as gh would
}

func (g *scriptGH) ReviewSnapshot(ctx context.Context, repo string, number int) (string, []gh.LatestReview, error) {
	if err := take(ctx); err != nil {
		return "", nil, err
	}
	g.coverCalls++
	if g.snapshots == nil {
		return "", nil, nil
	}
	res := g.snapshots[fmt.Sprintf("%s#%d", repo, number)]
	return res.author, res.reviews, res.err
}

func (g *scriptGH) TeamMembers(ctx context.Context, org, slug string) ([]gh.TeamMember, error) {
	if err := take(ctx); err != nil {
		return nil, err
	}
	g.coverCalls++
	g.memberCalls++
	if g.members == nil {
		return nil, fmt.Errorf("team members not configured for %s/%s", org, slug)
	}
	res, ok := g.members[org+"/"+slug]
	if !ok {
		return nil, fmt.Errorf("team members not configured for %s/%s", org, slug)
	}
	return res.members, res.err
}

func (g *scriptGH) ChangedFiles(ctx context.Context, repo string, number int) ([]string, error) {
	if err := take(ctx); err != nil {
		return nil, err
	}
	g.coverCalls++
	g.fileCalls++
	key := fmt.Sprintf("%s#%d", repo, number)
	if g.fileErr != nil && g.fileErr[key] != nil {
		return nil, g.fileErr[key]
	}
	if g.files == nil {
		return nil, nil
	}
	return g.files[key], nil
}

func (g *scriptGH) Codeowners(ctx context.Context, repo, ref string) (string, error) {
	if err := take(ctx); err != nil {
		return "", err
	}
	g.coverCalls++
	key := repo + "@" + ref
	if g.codeownersErr != nil && g.codeownersErr[key] != nil {
		return "", g.codeownersErr[key]
	}
	if g.codeowners == nil {
		return "", nil
	}
	return g.codeowners[key], nil
}

// BranchHead returns heads[repo@branch], or "base1" when unset.
func (g *scriptGH) BranchHead(ctx context.Context, repo, branch string) (string, error) {
	if err := take(ctx); err != nil {
		return "", err
	}
	g.coverCalls++
	g.headCalls++
	if g.headErr != nil {
		return "", g.headErr
	}
	if head, ok := g.heads[repo+"@"+branch]; ok {
		return head, nil
	}
	return "base1", nil
}

// PathHistories looks up history[repo+"\x00"+path], leaving out historySkip.
// It records each call's paths, ref, batch and maximum. It spends one
// budget call per batch of paths and defers the paths the budget cannot
// pay for.
func (g *scriptGH) PathHistories(ctx context.Context, repo, ref string, paths []string, _ time.Time, batch, maxBatch int) (gh.PathHistoryResult, error) {
	g.historyCalls++
	g.historyPaths = append(g.historyPaths, slices.Clone(paths))
	g.historyRefs = append(g.historyRefs, ref)
	g.historyBatches = append(g.historyBatches, batch)
	g.historyMax = append(g.historyMax, maxBatch)
	out := map[string]gh.PathHistory{}
	var deferred []string
	for start := 0; start < len(paths); start += max(batch, 1) {
		chunk := paths[start:min(start+max(batch, 1), len(paths))]
		if err := take(ctx); err != nil {
			if errors.Is(err, gh.ErrCallCap) {
				deferred = paths[start:]
				break
			}
			return gh.PathHistoryResult{}, err
		}
		g.coverCalls++
		g.historyRequests++
		if g.historyErr != nil {
			return gh.PathHistoryResult{}, g.historyErr
		}
		for _, path := range chunk {
			if !g.historySkip[path] {
				out[path] = g.history[repo+"\x00"+path]
			}
		}
	}
	worked := batch
	if g.historyWorked > 0 {
		worked = g.historyWorked
	}
	return gh.PathHistoryResult{Histories: out, Batch: worked, Deferred: deferred}, nil
}

func (g *scriptGH) DirectReviewCount(ctx context.Context, org, login string) (int, error) {
	if err := take(ctx); err != nil {
		return 0, err
	}
	g.coverCalls++
	g.loadCalls++
	if g.loadErr != nil {
		return 0, g.loadErr
	}
	if g.load == nil {
		return 0, nil
	}
	return g.load[org+"/"+strings.ToLower(login)], nil
}

func (g *scriptGH) OrgActive(ctx context.Context, org, login string, _ time.Time) (bool, error) {
	if err := take(ctx); err != nil {
		return false, err
	}
	g.coverCalls++
	if g.activeErr != nil {
		return false, g.activeErr
	}
	if g.active == nil {
		return true, nil
	}
	active, ok := g.active[org+"/"+strings.ToLower(login)]
	if !ok {
		return true, nil
	}
	return active, nil
}

func depsWith(g GH, h Herdr) Deps { return Deps{GH: g, Herdr: h} }

func fixturePRs() []gh.PRListItem {
	return []gh.PRListItem{{
		Number:           123,
		Title:            "[PROJ-123] Fix the widget",
		URL:              "https://github.com/acme/widgets/pull/123",
		State:            "OPEN",
		BaseRefName:      "main",
		HeadRefName:      "fix-widget",
		HeadRefOid:       "abc123def456",
		Mergeable:        "MERGEABLE",
		MergeStateStatus: "CLEAN",
		IsDraft:          false,
		ReviewDecision:   "APPROVED",
		ReviewRequests: []gh.ReviewReq{
			{Type: "User", Login: "reviewer-login"},
			{Type: "Team", Name: "Platform Reviewers", Slug: "platform-reviewers"},
		},
		LatestReviews: []gh.LatestReview{
			{Author: gh.ReviewAuthor{Login: "reviewer-login"}, State: "APPROVED", SubmittedAt: "2026-01-01T09:00:00Z"},
		},
		StatusCheckRollup: []gh.StatusCheck{
			{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"},
		},
	}}
}

func fixtureDraftPR() gh.PRListItem {
	item := fixturePRs()[0]
	item.Number = 200
	item.Title = "[PROJ-200] WIP the widget"
	item.URL = "https://github.com/acme/widgets/pull/200"
	item.HeadRefName = "wip-widget"
	item.IsDraft = true
	return item
}

func fixtureThreads() []gh.Thread {
	line := 42
	return []gh.Thread{{
		ID:         "PRRT_widget",
		IsResolved: false,
		Comments: []gh.ThreadComment{{
			ID:     "PRRC_widget",
			Author: &gh.ThreadAuthor{Login: "reviewer-login"},
			Path:   "src/widget.go",
			Line:   &line,
			URL:    "https://github.com/acme/widgets/pull/123#discussion_r1",
			Body:   "This should handle the nil case.",
		}},
	}}
}
