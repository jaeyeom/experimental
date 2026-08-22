package scan

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	executor "github.com/jaeyeom/go-cmdexec"
)

var update = flag.Bool("update", false, "update golden files")

var fixtureNow = time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC)

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

func TestRunOmitsDraftsByDefault(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
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
	if len(doc.PRs) != 1 {
		t.Fatalf("len(prs) = %d, want 1 (draft omitted)", len(doc.PRs))
	}
	if doc.PRs[0].Number != 123 || doc.PRs[0].IsDraft {
		t.Fatalf("pr = #%d is_draft=%v, want #123 non-draft", doc.PRs[0].Number, doc.PRs[0].IsDraft)
	}
	if g.threadCalls[200] != 0 {
		t.Fatalf("ReviewThreads(#200) calls = %d, want 0 for omitted draft", g.threadCalls[200])
	}
}

func TestRunIncludesDraftsWhenConfigured(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.IncludeDrafts = true
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
	if doc.PRs[1].Number != 200 || !doc.PRs[1].IsDraft {
		t.Fatalf("second pr = #%d is_draft=%v, want #200 draft", doc.PRs[1].Number, doc.PRs[1].IsDraft)
	}
	if g.threadCalls[200] != 1 {
		t.Fatalf("ReviewThreads(#200) calls = %d, want 1", g.threadCalls[200])
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

func (fixtureGH) ReviewThreads(context.Context, string, string, int) ([]gh.Thread, error) {
	return fixtureThreads(), nil
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

type scriptGH struct {
	authErr      error
	login        string
	loginErr     error
	search       []string
	searchCapped bool
	searchErr    error
	list         map[string]listResult
	threads      map[int][]gh.Thread
	threadErr    error
	threadCalls  map[int]int
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
	return g.search, g.searchCapped, g.searchErr
}

func (g *scriptGH) ListOpenPRs(_ context.Context, repo, _ string) ([]gh.PRListItem, error) {
	res, ok := g.list[repo]
	if !ok {
		return nil, gh.ErrInaccessible
	}
	return res.prs, res.err
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

func depsWith(g GH, h Herdr) Deps { return Deps{GH: g, Herdr: h} }

func fixturePRs() []gh.PRListItem {
	return []gh.PRListItem{{
		Number:           123,
		Title:            "[PROJ-123] Fix the widget",
		URL:              "https://github.com/acme/widgets/pull/123",
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
