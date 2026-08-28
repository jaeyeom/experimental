package scan

import (
	"context"
	"errors"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

func mergedOrphan(tabID, ticket string) OrphanTab {
	mergedAt := "2026-08-18T22:19:28Z"
	return OrphanTab{
		TabID:       tabID,
		WorkspaceID: "w2",
		Label:       ticket,
		Ticket:      ticket,
		AgentStatus: "idle",
		Bucket:      BucketMerged,
		PR: &OrphanPR{
			Repo: "acme/x", Number: 32347, URL: "https://gh/acme/x/pull/32347",
			State: "merged", MergedAt: &mergedAt,
		},
	}
}

func noPROrphan(tabID, ticket string) OrphanTab {
	return OrphanTab{
		TabID:       tabID,
		WorkspaceID: "w2",
		Label:       ticket,
		Ticket:      ticket,
		AgentStatus: "idle",
		Bucket:      BucketNoPR,
	}
}

func TestCloseMergedDryRunWouldCloseMergedOnly(t *testing.T) {
	t.Parallel()

	h := &closeHerdr{}
	g := &orphanGH{}
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, orphanCfg(), "alice", []OrphanTab{
		mergedOrphan("w2:tA", "AP-1306"),
		noPROrphan("w2:tB", "AP-1287"),
	}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if got.GeneratedAt != "2026-01-01T09:00:00Z" {
		t.Fatalf("generated_at = %q", got.GeneratedAt)
	}
	if len(got.Results) != 1 {
		t.Fatalf("results = %+v, want 1 merged tab", got.Results)
	}
	item := got.Results[0]
	if item.TabID != "w2:tA" || item.Ticket != "AP-1306" || item.Action != ActionWouldClose {
		t.Fatalf("item = %+v", item)
	}
	if len(h.closed) != 0 {
		t.Fatalf("TabClose called on dry-run: %v", h.closed)
	}
	if len(g.queried) != 0 {
		t.Fatalf("GitHub re-verify on dry-run: %v", g.queried)
	}
	if h.listed {
		t.Fatal("TabList called on dry-run")
	}
}

func TestCloseMergedLiveClosesMerged(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", State: "merged",
			ClosedAt: "2026-08-18T22:19:28Z", Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := &closeHerdr{tabs: []herdr.Tab{liveTab("w2:tA", "w2", "AP-1306")}}
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{
		mergedOrphan("w2:tA", "AP-1306"),
		noPROrphan("w2:tB", "AP-1287"),
	}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if got.DryRun {
		t.Fatal("dry_run = true, want false")
	}
	if len(got.Results) != 1 {
		t.Fatalf("results = %+v, want 1", got.Results)
	}
	item := got.Results[0]
	if item.TabID != "w2:tA" || item.Action != ActionClosed {
		t.Fatalf("item = %+v, want closed w2:tA", item)
	}
	if len(h.closed) != 1 || h.closed[0] != "w2:tA" {
		t.Fatalf("closed = %v, want [w2:tA]", h.closed)
	}
}

func TestCloseMergedLiveSkipsNotMerged(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 500, Title: "[AP-1306] reopened", State: "open",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := &closeHerdr{tabs: []herdr.Tab{liveTab("w2:tA", "w2", "AP-1306")}}
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{
		mergedOrphan("w2:tA", "AP-1306"),
	}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedNotMerged {
		t.Fatalf("results = %+v, want skipped_not_merged", got.Results)
	}
	if len(h.closed) != 0 {
		t.Fatalf("TabClose called: %v", h.closed)
	}
}

func TestCloseMergedLiveSkipsNotFound(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", State: "merged",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := &closeHerdr{} // tab list empty
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{
		mergedOrphan("w2:tA", "AP-1306"),
	}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedNotFound {
		t.Fatalf("results = %+v, want skipped_not_found", got.Results)
	}
	if len(h.closed) != 0 {
		t.Fatalf("TabClose called: %v", h.closed)
	}
}

func TestCloseMergedLiveTabCloseNotFound(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", State: "merged",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := &closeHerdr{
		tabs:     []herdr.Tab{liveTab("w2:tA", "w2", "AP-1306")},
		closeErr: herdr.ErrTabNotFound,
	}
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{
		mergedOrphan("w2:tA", "AP-1306"),
	}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedNotFound {
		t.Fatalf("results = %+v, want skipped_not_found", got.Results)
	}
}

func TestCloseMergedLiveFailureStopsBatch(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{Number: 1, Title: "[AP-1306] a", State: "merged", Repository: gh.SearchRepository{NameWithOwner: "acme/x"}}},
		"AP-1400": {{Number: 2, Title: "[AP-1400] b", State: "merged", Repository: gh.SearchRepository{NameWithOwner: "acme/x"}}},
	}}
	h := &closeHerdr{
		tabs: []herdr.Tab{
			liveTab("w2:tA", "w2", "AP-1306"),
			liveTab("w2:tC", "w2", "AP-1400"),
		},
		closeErr: errors.New("boom"),
	}
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{
		mergedOrphan("w2:tA", "AP-1306"),
		mergedOrphan("w2:tC", "AP-1400"),
	}, fixtureNow)
	if !errors.Is(err, ErrCloseFailed) {
		t.Fatalf("error = %v, want ErrCloseFailed", err)
	}
	if len(got.Results) != 1 {
		t.Fatalf("len(results) = %d, want 1 (stop on failure)", len(got.Results))
	}
	if got.Results[0].Action != ActionFailed {
		t.Fatalf("result = %+v, want failed", got.Results[0])
	}
	if len(h.closed) != 1 {
		t.Fatalf("closed = %v, want 1", h.closed)
	}
}

func TestCloseMergedLiveReverifyUsesResolvedAuthor(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", State: "merged",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := &closeHerdr{tabs: []herdr.Tab{liveTab("w2:tA", "w2", "AP-1306")}}
	_, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "bob", []OrphanTab{
		mergedOrphan("w2:tA", "AP-1306"),
	}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if g.searchedAuthor != "bob" {
		t.Fatalf("searched as %q, want bob", g.searchedAuthor)
	}
}

func TestCloseMergedEmptyResults(t *testing.T) {
	t.Parallel()

	h := &closeHerdr{}
	got, err := CloseMerged(context.Background(), CloseDeps{Herdr: h}, orphanCfg(), "alice", []OrphanTab{
		noPROrphan("w2:tB", "AP-1287"),
	}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if got.Results == nil {
		t.Fatal("results = nil, want empty slice")
	}
	if len(got.Results) != 0 {
		t.Fatalf("results = %+v, want empty", got.Results)
	}
	if h.listed {
		t.Fatal("TabList called with no merged candidates")
	}
}

type closeHerdr struct {
	tabs     []herdr.Tab
	closed   []string
	closeErr error
	listErr  error
	listed   bool
}

func (h *closeHerdr) TabList(context.Context) ([]herdr.Tab, error) {
	h.listed = true
	if h.listErr != nil {
		return nil, h.listErr
	}
	return h.tabs, nil
}

func (h *closeHerdr) TabClose(_ context.Context, tabID string) error {
	h.closed = append(h.closed, tabID)
	return h.closeErr
}
