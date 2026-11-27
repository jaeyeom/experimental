package comment

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

var fixtureNow = time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

func TestRunDryRunWouldDispatch(t *testing.T) {
	t.Parallel()

	g := &fakeGH{}
	got, err := Run(context.Background(), g, nil, config.Defaults(), Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, false)}},
		Body: "please retry",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if got.GeneratedAt != "2026-01-01T09:00:00Z" {
		t.Fatalf("generated_at = %q", got.GeneratedAt)
	}
	if len(got.Results) != 1 {
		t.Fatalf("len(results) = %d, want 1", len(got.Results))
	}
	item := got.Results[0]
	if item.Repo != "acme/widgets" || item.Number != 123 {
		t.Fatalf("item = %+v", item)
	}
	if item.Action != dispatch.ActionWouldDispatch {
		t.Fatalf("action = %q, want would_dispatch", item.Action)
	}
	if item.RenderedPrompt != "please retry" {
		t.Fatalf("rendered_prompt = %q, want please retry", item.RenderedPrompt)
	}
	if item.PaneID != "" {
		t.Fatalf("pane_id = %q, want empty (not tab-bound)", item.PaneID)
	}
	if len(g.calls) != 0 {
		t.Fatalf("CommentPR called on dry-run: %+v", g.calls)
	}
}

func TestRunDryRunCommentsOffMachinePR(t *testing.T) {
	t.Parallel()

	got, err := Run(context.Background(), &fakeGH{}, nil, config.Defaults(), Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, true)}},
		Body: "please retry",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch for no-tab PR", got.Results)
	}
}

func TestRunSkippedNotFound(t *testing.T) {
	t.Parallel()

	got, err := Run(context.Background(), &fakeGH{}, nil, config.Defaults(), Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, false)}},
		PRs:  []string{"acme/widgets#123", "acme/missing#9"},
		Body: "please retry",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2 (never drop)", len(got.Results))
	}
	byRepo := map[string]string{}
	for _, item := range got.Results {
		byRepo[item.Repo] = item.Action
	}
	if byRepo["acme/missing"] != dispatch.ActionSkippedNotFound {
		t.Fatalf("actions = %v", byRepo)
	}
	if byRepo["acme/widgets"] != dispatch.ActionWouldDispatch {
		t.Fatalf("actions = %v", byRepo)
	}
}

func TestRunLivePostsComment(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	g := &fakeGH{}
	got, err := Run(context.Background(), g, nil, cfg, Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, true), pr("acme/gizmos", 50, false)}},
		Body: "please retry",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if got.DryRun {
		t.Fatal("dry_run = true, want false")
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	if got.Results[0].Repo != "acme/gizmos" || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("first = %+v, want dispatched acme/gizmos#50 (sorted)", got.Results[0])
	}
	if got.Results[1].Repo != "acme/widgets" || got.Results[1].Action != dispatch.ActionDispatched {
		t.Fatalf("second = %+v, want dispatched acme/widgets#123", got.Results[1])
	}
	if got.Results[0].RenderedPrompt != "please retry" || got.Results[1].RenderedPrompt != "please retry" {
		t.Fatalf("missing comment body on results: %+v", got.Results)
	}
	want := []commentCall{
		{repo: "acme/gizmos", number: 50, body: "please retry"},
		{repo: "acme/widgets", number: 123, body: "please retry"},
	}
	if len(g.calls) != len(want) {
		t.Fatalf("calls = %+v, want %+v", g.calls, want)
	}
	for i, call := range g.calls {
		if call != want[i] {
			t.Fatalf("call[%d] = %+v, want %+v", i, call, want[i])
		}
	}
}

func TestRunLiveFailureStopsBatch(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	g := &fakeGH{err: errors.New("boom")}
	got, err := Run(context.Background(), g, nil, cfg, Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/gizmos", 50, false), pr("acme/widgets", 123, false)}},
		Body: "please retry",
	}, fixtureNow)
	if !errors.Is(err, dispatch.ErrFailed) {
		t.Fatalf("error = %v, want ErrFailed", err)
	}
	if len(got.Results) != 1 {
		t.Fatalf("len(results) = %d, want 1 (stop on failure)", len(got.Results))
	}
	if got.Results[0].Action != dispatch.ActionFailed || got.Results[0].Detail != "boom" {
		t.Fatalf("result = %+v, want failed boom", got.Results[0])
	}
	if len(g.calls) != 1 {
		t.Fatalf("calls = %+v, want 1", g.calls)
	}
}

func TestRunInvalidPRFlag(t *testing.T) {
	t.Parallel()

	_, err := Run(context.Background(), &fakeGH{}, nil, config.Defaults(), Request{
		PRs:  []string{"not-a-pr"},
		Body: "please retry",
	}, fixtureNow)
	if err == nil {
		t.Fatal("error = nil, want invalid --pr")
	}
	if !strings.Contains(err.Error(), "invalid --pr") {
		t.Fatalf("error = %v, want invalid --pr", err)
	}
}

func TestRunLiveDedupesSameTuple(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	store := liveStore(t)
	g := &fakeGH{}
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	req := Request{Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci"}
	got, err := Run(context.Background(), g, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("first Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("first results = %+v, want dispatched", got.Results)
	}
	st, err := dispatch.LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.DedupedPosted("acme/widgets#123", "abc123def456", "/ci") {
		t.Fatalf("state after post = %#v", st)
	}

	got2, err := Run(context.Background(), g, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("second results = %+v, want skipped_deduped", got2.Results)
	}
	if len(g.calls) != 1 {
		t.Fatalf("CommentPR calls = %d, want 1", len(g.calls))
	}
}

func TestRunLiveNewSHAPostsAgain(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	store := liveStore(t)
	g := &fakeGH{}
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	if _, err := Run(context.Background(), g, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci",
	}, fixtureNow); err != nil {
		t.Fatalf("first Run() unexpected error: %v", err)
	}

	p.HeadSHA = "fff000aaa111"
	got, err := Run(context.Background(), g, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("changed SHA Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("changed SHA results = %+v, want dispatched", got.Results)
	}
	if len(g.calls) != 2 {
		t.Fatalf("CommentPR calls = %d, want 2", len(g.calls))
	}
}

func TestRunLiveDifferentBodyPostsAgain(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	store := liveStore(t)
	g := &fakeGH{}
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	if _, err := Run(context.Background(), g, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci",
	}, fixtureNow); err != nil {
		t.Fatalf("first Run() unexpected error: %v", err)
	}

	got, err := Run(context.Background(), g, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{p}}, Body: "please retry",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("different body Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("different body results = %+v, want dispatched", got.Results)
	}
	if len(g.calls) != 2 {
		t.Fatalf("CommentPR calls = %d, want 2", len(g.calls))
	}
}

func TestRunDryRunDedupedDoesNotPost(t *testing.T) {
	t.Parallel()

	store := liveStore(t)
	st := dispatch.State{}
	st.RecordPosted("acme/widgets#123", "abc123def456", "/ci", fixtureNow)
	if err := store.Save(st); err != nil {
		t.Fatal(err)
	}
	g := &fakeGH{}
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	got, err := Run(context.Background(), g, store, config.Defaults(), Request{
		Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("results = %+v, want skipped_deduped", got.Results)
	}
	if len(g.calls) != 0 {
		t.Fatalf("CommentPR called on dry-run: %+v", g.calls)
	}
}

func TestRunAllowDuplicatePostsAgain(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	store := liveStore(t)
	g := &fakeGH{}
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	req := Request{Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci"}
	if _, err := Run(context.Background(), g, store, cfg, req, fixtureNow); err != nil {
		t.Fatalf("first Run() unexpected error: %v", err)
	}

	req.AllowDuplicate = true
	got, err := Run(context.Background(), g, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("allow-duplicate Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("allow-duplicate results = %+v, want dispatched", got.Results)
	}
	if len(g.calls) != 2 {
		t.Fatalf("CommentPR calls = %d, want 2", len(g.calls))
	}
}

func TestRunFailureDoesNotRecord(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	store := liveStore(t)
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	req := Request{Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci"}
	if _, err := Run(context.Background(), &fakeGH{err: errors.New("boom")}, store, cfg, req, fixtureNow); !errors.Is(err, dispatch.ErrFailed) {
		t.Fatalf("error = %v, want ErrFailed", err)
	}
	st, err := dispatch.LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if st.DedupedPosted("acme/widgets#123", "abc123def456", "/ci") {
		t.Fatalf("failed post must not record state: %#v", st)
	}

	g := &fakeGH{}
	got, err := Run(context.Background(), g, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("retry Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("retry results = %+v, want dispatched", got.Results)
	}
	if len(g.calls) != 1 {
		t.Fatalf("CommentPR calls = %d, want 1", len(g.calls))
	}
}

func TestRunDispatchStateDoesNotDedupeComment(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	store := liveStore(t)
	st := dispatch.State{}
	st.Record("acme/widgets#123", []string{"PRRC_widget"}, fixtureNow)
	st.RecordHead("acme/widgets#123", "abc123def456", fixtureNow)
	st.RecordCIFix("acme/widgets#123", "abc123def456", fixtureNow)
	if err := store.Save(st); err != nil {
		t.Fatal(err)
	}
	g := &fakeGH{}
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	got, err := Run(context.Background(), g, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("results = %+v, want dispatched (other modes must not skip comment)", got.Results)
	}
}

func TestRunDryRunDoesNotWriteState(t *testing.T) {
	t.Parallel()

	store := liveStore(t)
	p := pr("acme/widgets", 123, false)
	p.HeadSHA = "abc123def456"
	if _, err := Run(context.Background(), &fakeGH{}, store, config.Defaults(), Request{
		Doc: scan.Document{PRs: []scan.PR{p}}, Body: "/ci",
	}, fixtureNow); err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	st, err := dispatch.LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if len(st) != 0 {
		t.Fatalf("dry-run wrote state: %#v", st)
	}
}

type commentCall struct {
	repo   string
	number int
	body   string
}

func liveStore(t *testing.T) dispatch.FileStore {
	t.Helper()
	return dispatch.FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
}

type fakeGH struct {
	calls     []commentCall
	commentID string
	err       error
}

func (f *fakeGH) CommentPR(_ context.Context, repo string, number int, body string) error {
	f.calls = append(f.calls, commentCall{repo: repo, number: number, body: body})
	return f.err
}

func (f *fakeGH) CommentPRWithID(_ context.Context, repo string, number int, body string) (string, error) {
	f.calls = append(f.calls, commentCall{repo: repo, number: number, body: body})
	if f.err != nil {
		return "", f.err
	}
	return f.commentID, nil
}

func waiveCfg(repost bool) config.Config {
	cfg := config.Defaults()
	cfg.WaiveCommentPrefix = "/waive"
	cfg.WaiveChecks = []string{"ci/flake"}
	cfg.WaiveRepost = repost
	return cfg
}

func staleWaivePR(repo string, number int, oldHead, newHead, class string, failing []string, body string) scan.PR {
	p := pr(repo, number, true)
	p.HeadSHA = newHead
	p.StaleWaive = &scan.StaleWaive{
		OldHead:       oldHead,
		NewHead:       newHead,
		Class:         class,
		FailingChecks: failing,
		CommentID:     "100",
		CommentBody:   body,
	}
	return p
}

func TestWaiveRepostOffByDefault(t *testing.T) {
	t.Parallel()

	cfg := waiveCfg(false)
	cfg.DryRun = false
	g := &fakeGH{commentID: "777"}
	lgr := &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")}
	p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, "/waive\nreason\n")
	got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
		Doc:          scan.Document{PRs: []scan.PR{p}},
		RepostWaives: true,
		Ledger:       lgr,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedWaiveDisabled {
		t.Fatalf("results = %+v, want skipped_waive_disabled", got.Results)
	}
	if len(g.calls) != 0 {
		t.Fatalf("CommentPR called when waive_repost=false: %+v", g.calls)
	}
}

func TestWaiveRepostRequiresWaiveChecksAndFailingCheckAndBaseOnly(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		checks     []string
		class      string
		failing    []string
		wantAction string
	}{
		{
			name:       "empty waive_checks refuses repost",
			checks:     nil,
			class:      scan.WaiveClassBaseOnly,
			failing:    []string{"ci/flake"},
			wantAction: dispatch.ActionSkippedWaiveDisabled,
		},
		{
			name:       "author changes never reposted",
			checks:     []string{"ci/flake"},
			class:      scan.WaiveClassAuthorChanges,
			failing:    []string{"ci/flake"},
			wantAction: dispatch.ActionSkippedAuthorChanges,
		},
		{
			name:       "unrelated failing check does not repost",
			checks:     []string{"ci/flake"},
			class:      scan.WaiveClassBaseOnly,
			failing:    []string{"ci/build"},
			wantAction: dispatch.ActionSkippedNoFailingWaivedChecks,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := waiveCfg(true)
			cfg.DryRun = false
			cfg.WaiveChecks = tc.checks
			g := &fakeGH{commentID: "777"}
			p := staleWaivePR("acme/widgets", 123, "old1", "new1", tc.class, tc.failing, "/waive\n")
			got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
				Doc:          scan.Document{PRs: []scan.PR{p}},
				RepostWaives: true,
				Ledger:       &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")},
			}, fixtureNow)
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(got.Results) != 1 || got.Results[0].Action != tc.wantAction {
				t.Fatalf("results = %+v, want %q", got.Results, tc.wantAction)
			}
			if len(g.calls) != 0 {
				t.Fatalf("CommentPR called unexpectedly: %+v", g.calls)
			}
		})
	}
}

func TestWaiveRepostDryRunExactBodyAndNoWrites(t *testing.T) {
	t.Parallel()

	cfg := waiveCfg(true) // DryRun is true by default.
	g := &fakeGH{commentID: "777"}
	store := liveStore(t)
	lgr := &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")}
	origBody := "/waive\r\n  flaky integration test\nSee run #42\n"
	p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)

	got, err := Run(context.Background(), g, store, cfg, Request{
		Doc:          scan.Document{PRs: []scan.PR{p}},
		RepostWaives: true,
		Ledger:       lgr,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if !got.DryRun || len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("got = %+v", got)
	}
	if got.Results[0].RenderedPrompt != origBody {
		t.Fatalf("rendered_prompt = %q, want byte-identical %q", got.Results[0].RenderedPrompt, origBody)
	}
	if len(g.calls) != 0 {
		t.Fatalf("CommentPR called on dry-run: %+v", g.calls)
	}
	entries, err := lgr.Load()
	if err != nil || len(entries) != 0 {
		t.Fatalf("ledger written on dry-run: %+v, %v", entries, err)
	}
}

func TestWaiveRepostLiveLedgerDedupeAndCapAcrossRestarts(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	ledgerPath := filepath.Join(dir, "ledger.jsonl")
	cfg := waiveCfg(true)
	cfg.DryRun = false
	cfg.WaiveMaxPerPR = 2
	origBody := "/waive\nknown flake  \nkeep exact bytes\n"

	g := &fakeGH{commentID: "501"}
	p1 := staleWaivePR("acme/widgets", 123, "head0", "head1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
	got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
		Doc:          scan.Document{PRs: []scan.PR{p1}},
		RepostWaives: true,
		Ledger:       &ledger.Store{Path: ledgerPath},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("first Run() error = %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("first results = %+v, want dispatched", got.Results)
	}
	if len(g.calls) != 1 || g.calls[0].body != origBody {
		t.Fatalf("posted = %+v, want exact body %q", g.calls, origBody)
	}

	// Simulate restart with fresh state_file and reloaded ledger: same head must dedupe.
	restartedLedger := &ledger.Store{Path: ledgerPath}
	got2, err := Run(context.Background(), g, liveStore(t), cfg, Request{
		Doc:          scan.Document{PRs: []scan.PR{p1}},
		RepostWaives: true,
		Ledger:       restartedLedger,
	}, fixtureNow.Add(time.Minute))
	if err != nil {
		t.Fatalf("dedupe Run() error = %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("dedupe results = %+v, want skipped_deduped", got2.Results)
	}
	if len(g.calls) != 1 {
		t.Fatalf("calls = %d, want 1", len(g.calls))
	}

	// Second base-only head succeeds (count becomes 2 == waive_max_per_pr).
	g.commentID = "502"
	p2 := staleWaivePR("acme/widgets", 123, "head1", "head2", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
	got3, err := Run(context.Background(), g, liveStore(t), cfg, Request{
		Doc:          scan.Document{PRs: []scan.PR{p2}},
		RepostWaives: true,
		Ledger:       &ledger.Store{Path: ledgerPath},
	}, fixtureNow.Add(2*time.Minute))
	if err != nil {
		t.Fatalf("second head Run() error = %v", err)
	}
	if len(got3.Results) != 1 || got3.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("second head results = %+v, want dispatched", got3.Results)
	}

	// Third base-only head hits waive_max_per_pr=2 cap.
	p3 := staleWaivePR("acme/widgets", 123, "head2", "head3", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
	got4, err := Run(context.Background(), g, liveStore(t), cfg, Request{
		Doc:          scan.Document{PRs: []scan.PR{p3}},
		RepostWaives: true,
		Ledger:       &ledger.Store{Path: ledgerPath},
	}, fixtureNow.Add(3*time.Minute))
	if err != nil {
		t.Fatalf("cap Run() error = %v", err)
	}
	if len(got4.Results) != 1 || got4.Results[0].Action != dispatch.ActionSkippedWaiveCap {
		t.Fatalf("cap results = %+v, want skipped_waive_cap", got4.Results)
	}
	if len(g.calls) != 2 {
		t.Fatalf("calls = %d, want 2", len(g.calls))
	}

	entries, err := (&ledger.Store{Path: ledgerPath}).Load()
	if err != nil || len(entries) != 2 {
		t.Fatalf("ledger entries = %+v, %v", entries, err)
	}
	if entries[0].OldHeadSHA != "head0" || entries[0].NewHeadSHA != "head1" || entries[0].CommentID != "501" {
		t.Fatalf("entry[0] = %+v", entries[0])
	}
	if entries[1].OldHeadSHA != "head1" || entries[1].NewHeadSHA != "head2" || entries[1].CommentID != "502" {
		t.Fatalf("entry[1] = %+v", entries[1])
	}
}

func TestWaiveRepostRequireGreen(t *testing.T) {
	t.Parallel()

	origBody := "/waive\nknown flake\n"

	t.Run("all listed checks green reposts waive", func(t *testing.T) {
		t.Parallel()
		cfg := waiveCfg(true)
		cfg.DryRun = false
		cfg.WaiveRequireGreen = []string{"ci/lint", "ci/unit"}
		g := &fakeGH{commentID: "701"}
		p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
		p.StaleWaive.StatusChecks = []gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Context: "ci/unit", State: "SUCCESS"},
		}
		got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
			Doc:          scan.Document{PRs: []scan.PR{p}},
			RepostWaives: true,
			Ledger:       &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")},
		}, fixtureNow)
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
			t.Fatalf("results = %+v, want dispatched", got.Results)
		}
		if len(g.calls) != 1 || g.calls[0].body != origBody {
			t.Fatalf("calls = %+v, want 1 call with exact body", g.calls)
		}
	})

	t.Run("one listed check pending skips as pending", func(t *testing.T) {
		t.Parallel()
		cfg := waiveCfg(true)
		cfg.DryRun = false
		cfg.WaiveRequireGreen = []string{"ci/lint", "ci/unit"}
		g := &fakeGH{commentID: "702"}
		p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
		p.StaleWaive.StatusChecks = []gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "ci/unit", Status: "IN_PROGRESS"},
		}
		got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
			Doc:          scan.Document{PRs: []scan.PR{p}},
			RepostWaives: true,
			Ledger:       &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")},
		}, fixtureNow)
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedWaiveChecksPending {
			t.Fatalf("results = %+v, want skipped_waive_checks_pending", got.Results)
		}
		if len(g.calls) != 0 {
			t.Fatalf("CommentPR called unexpectedly: %+v", g.calls)
		}
	})

	t.Run("one listed check failed skips as failing naming check", func(t *testing.T) {
		t.Parallel()
		cfg := waiveCfg(true)
		cfg.DryRun = false
		cfg.WaiveRequireGreen = []string{"ci/lint", "ci/unit"}
		g := &fakeGH{commentID: "703"}
		p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
		p.StaleWaive.StatusChecks = []gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "ci/unit", Status: "COMPLETED", Conclusion: "FAILURE"},
		}
		got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
			Doc:          scan.Document{PRs: []scan.PR{p}},
			RepostWaives: true,
			Ledger:       &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")},
		}, fixtureNow)
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedWaiveChecksFailing {
			t.Fatalf("results = %+v, want skipped_waive_checks_failing", got.Results)
		}
		if !strings.Contains(got.Results[0].Detail, "ci/unit") {
			t.Fatalf("detail = %q, want check name ci/unit", got.Results[0].Detail)
		}
		if len(g.calls) != 0 {
			t.Fatalf("CommentPR called unexpectedly: %+v", g.calls)
		}
	})

	t.Run("one listed check missing skips as pending and reports missing", func(t *testing.T) {
		t.Parallel()
		cfg := waiveCfg(true)
		cfg.DryRun = false
		cfg.WaiveRequireGreen = []string{"ci/lint", "ci/unit"}
		g := &fakeGH{commentID: "704"}
		p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
		p.StaleWaive.StatusChecks = []gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
		}
		got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
			Doc:          scan.Document{PRs: []scan.PR{p}},
			RepostWaives: true,
			Ledger:       &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")},
		}, fixtureNow)
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedWaiveChecksPending {
			t.Fatalf("results = %+v, want skipped_waive_checks_pending", got.Results)
		}
		if !strings.Contains(strings.ToLower(got.Results[0].Detail), "missing") || !strings.Contains(got.Results[0].Detail, "ci/unit") {
			t.Fatalf("detail = %q, want missing + ci/unit", got.Results[0].Detail)
		}
		if len(g.calls) != 0 {
			t.Fatalf("CommentPR called unexpectedly: %+v", g.calls)
		}
	})

	t.Run("empty waive_require_green behaves as today", func(t *testing.T) {
		t.Parallel()
		cfg := waiveCfg(true)
		cfg.DryRun = false
		cfg.WaiveRequireGreen = nil
		g := &fakeGH{commentID: "705"}
		p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
		p.StaleWaive.StatusChecks = []gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/unit", Status: "IN_PROGRESS"},
		}
		got, err := Run(context.Background(), g, liveStore(t), cfg, Request{
			Doc:          scan.Document{PRs: []scan.PR{p}},
			RepostWaives: true,
			Ledger:       &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")},
		}, fixtureNow)
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
			t.Fatalf("results = %+v, want dispatched", got.Results)
		}
		if len(g.calls) != 1 {
			t.Fatalf("calls = %d, want 1", len(g.calls))
		}
	})

	t.Run("skips do not count toward waive_max_per_pr", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		store := dispatch.FileStore{Path: filepath.Join(dir, "state.json")}
		lgr := &ledger.Store{Path: filepath.Join(dir, "ledger.jsonl")}
		cfg := waiveCfg(true)
		cfg.DryRun = false
		cfg.WaiveMaxPerPR = 1
		cfg.WaiveRequireGreen = []string{"ci/lint", "ci/unit"}
		g := &fakeGH{commentID: "706"}

		runWithChecks := func(checks []gh.StatusCheck, offset time.Duration) dispatch.Item {
			t.Helper()
			p := staleWaivePR("acme/widgets", 123, "old1", "new1", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
			p.StaleWaive.StatusChecks = checks
			got, err := Run(context.Background(), g, store, cfg, Request{
				Doc:          scan.Document{PRs: []scan.PR{p}},
				RepostWaives: true,
				Ledger:       lgr,
			}, fixtureNow.Add(offset))
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(got.Results) != 1 {
				t.Fatalf("results = %+v, want 1", got.Results)
			}
			return got.Results[0]
		}

		// 1. Missing check -> skipped_waive_checks_pending.
		if item := runWithChecks([]gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
		}, 0); item.Action != dispatch.ActionSkippedWaiveChecksPending {
			t.Fatalf("missing action = %q, want skipped_waive_checks_pending", item.Action)
		}
		// 2. Pending check -> skipped_waive_checks_pending.
		if item := runWithChecks([]gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "ci/unit", Status: "IN_PROGRESS"},
		}, time.Minute); item.Action != dispatch.ActionSkippedWaiveChecksPending {
			t.Fatalf("pending action = %q, want skipped_waive_checks_pending", item.Action)
		}
		// 3. Failing check -> skipped_waive_checks_failing.
		if item := runWithChecks([]gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "ci/unit", Status: "COMPLETED", Conclusion: "FAILURE"},
		}, 2*time.Minute); item.Action != dispatch.ActionSkippedWaiveChecksFailing {
			t.Fatalf("failing action = %q, want skipped_waive_checks_failing", item.Action)
		}
		if len(g.calls) != 0 {
			t.Fatalf("calls after skips = %d, want 0", len(g.calls))
		}
		if count, err := lgr.CountWaiveReposts("acme/widgets#123"); err != nil || count != 0 {
			t.Fatalf("CountWaiveReposts() = %d, %v; want 0", count, err)
		}

		// 4. All green -> succeeds because skips did not count toward waive_max_per_pr=1.
		if item := runWithChecks([]gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "ci/unit", Status: "COMPLETED", Conclusion: "SUCCESS"},
		}, 3*time.Minute); item.Action != dispatch.ActionDispatched {
			t.Fatalf("green action = %q, want dispatched", item.Action)
		}
		if len(g.calls) != 1 {
			t.Fatalf("calls = %d, want 1", len(g.calls))
		}

		// 5. Next head now hits waive_max_per_pr=1.
		p2 := staleWaivePR("acme/widgets", 123, "new1", "new2", scan.WaiveClassBaseOnly, []string{"ci/flake"}, origBody)
		p2.StaleWaive.StatusChecks = []gh.StatusCheck{
			{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "ci/unit", Status: "COMPLETED", Conclusion: "SUCCESS"},
		}
		gotCap, err := Run(context.Background(), g, store, cfg, Request{
			Doc:          scan.Document{PRs: []scan.PR{p2}},
			RepostWaives: true,
			Ledger:       lgr,
		}, fixtureNow.Add(4*time.Minute))
		if err != nil {
			t.Fatalf("cap Run() error = %v", err)
		}
		if len(gotCap.Results) != 1 || gotCap.Results[0].Action != dispatch.ActionSkippedWaiveCap {
			t.Fatalf("cap results = %+v, want skipped_waive_cap", gotCap.Results)
		}
	})
}

func pr(repo string, number int, noTab bool) scan.PR {
	p := scan.PR{Repo: repo, Number: number}
	if !noTab {
		pane := "w2:pC"
		p.Tab = &scan.Tab{TabID: "w2:tC", PaneID: &pane, AgentStatus: "idle"}
	}
	return p
}
