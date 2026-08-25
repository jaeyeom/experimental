package scan

import (
	"context"
	"errors"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

func orphanCfg() config.Config {
	cfg := config.Defaults()
	cfg.Author = "alice"
	return cfg
}

func liveTab(id, ws, label string) herdr.Tab {
	return herdr.Tab{TabID: id, WorkspaceID: ws, Label: label, AgentStatus: "idle", PaneCount: 1}
}

func liveAgent(pane, tab, status string) herdr.Agent {
	return herdr.Agent{PaneID: pane, TabID: tab, Agent: "codex", AgentStatus: status}
}

func TestOrphansMergedBucket(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", URL: "https://gh/acme/x/pull/32347",
			State: "merged", ClosedAt: "2026-08-18T22:19:28Z",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "AP-1306")},
		agents: []herdr.Agent{liveAgent("w2:pA", "w2:tA", "idle")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 {
		t.Fatalf("orphan_tabs = %+v, want 1", doc.OrphanTabs)
	}
	got := doc.OrphanTabs[0]
	if got.Bucket != BucketMerged || got.Ticket != "AP-1306" || got.AgentStatus != "idle" {
		t.Fatalf("orphan = %+v", got)
	}
	if got.PR == nil || got.PR.Number != 32347 || got.PR.State != "merged" {
		t.Fatalf("pr = %+v", got.PR)
	}
	if got.PR.MergedAt == nil || *got.PR.MergedAt != "2026-08-18T22:19:28Z" {
		t.Fatalf("merged_at = %v", got.PR.MergedAt)
	}
}

func TestOrphansNoPRBucket(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{"AP-1287": {}}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tB", "w2", "AP-1287")},
		agents: []herdr.Agent{liveAgent("w2:pB", "w2:tB", "idle")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 {
		t.Fatalf("orphan_tabs = %+v, want 1", doc.OrphanTabs)
	}
	got := doc.OrphanTabs[0]
	if got.Bucket != BucketNoPR || got.PR != nil || got.AgentStatus != "idle" {
		t.Fatalf("orphan = %+v", got)
	}
}

func TestOrphansOmitsNoPRWhenAgentWorking(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{"AP-1287": {}}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tB", "w2", "AP-1287")},
		agents: []herdr.Agent{liveAgent("w2:pB", "w2:tB", "working")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty (working agent is in-progress, not an orphan)", doc.OrphanTabs)
	}
}

func TestOrphansNoPRBlockedStillSurfaced(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{"AP-1288": {}}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tH", "w2", "AP-1288")},
		agents: []herdr.Agent{liveAgent("w2:pH", "w2:tH", "blocked")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 {
		t.Fatalf("orphan_tabs = %+v, want 1", doc.OrphanTabs)
	}
	got := doc.OrphanTabs[0]
	if got.Bucket != BucketNoPR || got.PR != nil || got.AgentStatus != "blocked" {
		t.Fatalf("orphan = %+v", got)
	}
}

func TestOrphansMergedWorkingUnaffected(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", URL: "https://gh/acme/x/pull/32347",
			State: "merged", ClosedAt: "2026-08-18T22:19:28Z",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "AP-1306")},
		agents: []herdr.Agent{liveAgent("w2:pA", "w2:tA", "working")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 {
		t.Fatalf("orphan_tabs = %+v, want 1", doc.OrphanTabs)
	}
	got := doc.OrphanTabs[0]
	if got.Bucket != BucketMerged || got.AgentStatus != "working" {
		t.Fatalf("orphan = %+v", got)
	}
}

func TestOrphansHasOpenPROmitted(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1400": {{
			Number: 500, Title: "[AP-1400] WIP", State: "open", IsDraft: true,
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tC", "w2", "AP-1400")},
		agents: []herdr.Agent{liveAgent("w2:pC", "w2:tC", "idle")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty (open PR)", doc.OrphanTabs)
	}
}

func TestOrphansSkipsAgentlessAndUnparseable(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{}}
	h := stubHerdr{
		tabs: []herdr.Tab{
			liveTab("w2:tScratch", "w2", "AP-1500"), // live label but no agent session
			liveTab("w2:tNote", "w2", "scratchpad"), // agent present but no ticket in label
		},
		agents: []herdr.Agent{
			liveAgent("w2:pNote", "w2:tNote", "idle"),
			liveAgent("w2:pDead", "w2:tScratch", "none"), // not a live status
		},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty", doc.OrphanTabs)
	}
	if len(g.queried) != 0 {
		t.Fatalf("searched %v, want no GitHub search for skipped tabs", g.queried)
	}
}

func TestOrphansStdinReuseSkipsSearch(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		// Would look merged if searched — proves the shortcut skipped it.
		"AP-1600": {{Number: 9, Title: "[AP-1600] x", State: "merged", Repository: gh.SearchRepository{NameWithOwner: "acme/x"}}},
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tD", "w2", "AP-1600")},
		agents: []herdr.Agent{liveAgent("w2:pD", "w2:tD", "idle")},
	}
	openTabs := map[string]struct{}{"w2:tD": {}}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), openTabs, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty (open PR known from scan)", doc.OrphanTabs)
	}
	if len(g.queried) != 0 {
		t.Fatalf("searched %v, want no search when scan already matched the tab", g.queried)
	}
}

func TestOrphansClosedUnmergedHasNilMergedAt(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1700": {{
			Number: 42, Title: "[AP-1700] abandoned", State: "closed",
			ClosedAt: "2026-08-01T00:00:00Z", Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tE", "w2", "AP-1700")},
		agents: []herdr.Agent{liveAgent("w2:pE", "w2:tE", "idle")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	got := doc.OrphanTabs[0]
	if got.Bucket != BucketMerged || got.PR == nil || got.PR.State != "closed" {
		t.Fatalf("orphan = %+v pr=%+v", got, got.PR)
	}
	if got.PR.MergedAt != nil {
		t.Fatalf("merged_at = %v, want nil for closed-unmerged", *got.PR.MergedAt)
	}
}

func TestOrphansTitleFilterIgnoresBodyOnlyMatch(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1800": {{
			// Full-text hit whose title carries a different ticket.
			Number: 77, Title: "[AP-9999] unrelated", State: "merged",
			ClosedAt: "2026-08-10T00:00:00Z", Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tF", "w2", "AP-1800")},
		agents: []herdr.Agent{liveAgent("w2:pF", "w2:tF", "idle")},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 || doc.OrphanTabs[0].Bucket != BucketNoPR {
		t.Fatalf("orphan_tabs = %+v, want one no_pr", doc.OrphanTabs)
	}
}

func TestOrphansHerdrRequiredEmitsAuthorThenError(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{}}
	h := stubHerdr{minErr: herdr.ErrNotInstalled}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err == nil {
		t.Fatal("Orphans() error = nil, want herdr required")
	}
	if !OrphansStarted(doc) || doc.Author != "alice" {
		t.Fatalf("author = %q, want alice on partial document", doc.Author)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty", doc.OrphanTabs)
	}
}

func TestOrphansUnauthenticatedNoDocument(t *testing.T) {
	t.Parallel()

	g := &orphanGH{authErr: gh.ErrUnauthenticated}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: stubHerdr{}}, orphanCfg(), nil, fixtureNow)
	if !errors.Is(err, gh.ErrUnauthenticated) {
		t.Fatalf("Orphans() error = %v, want ErrUnauthenticated", err)
	}
	if OrphansStarted(doc) {
		t.Fatalf("document started before auth: author=%q", doc.Author)
	}
}

func TestOrphansResolvesAuthorWhenUnset(t *testing.T) {
	t.Parallel()

	g := &orphanGH{login: "bob", search: map[string][]gh.PRSearchItem{"AP-1900": {}}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tG", "w2", "AP-1900")},
		agents: []herdr.Agent{liveAgent("w2:pG", "w2:tG", "idle")},
	}
	cfg := config.Defaults() // no author set
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, cfg, nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if doc.Author != "bob" {
		t.Fatalf("author = %q, want bob", doc.Author)
	}
	if g.searchedAuthor != "bob" {
		t.Fatalf("searched as %q, want bob", g.searchedAuthor)
	}
}

type orphanGH struct {
	authErr        error
	login          string
	search         map[string][]gh.PRSearchItem
	queried        []string
	searchedAuthor string
}

func (g *orphanGH) AuthStatus(context.Context) error { return g.authErr }

func (g *orphanGH) UserLogin(context.Context) (string, error) {
	if g.login != "" {
		return g.login, nil
	}
	return "alice", nil
}

func (g *orphanGH) SearchAuthoredPRs(_ context.Context, author, query string) ([]gh.PRSearchItem, error) {
	g.queried = append(g.queried, query)
	g.searchedAuthor = author
	return g.search[query], nil
}
