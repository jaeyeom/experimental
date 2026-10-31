package scan

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

func repoFromCWD(cwd string) (string, error) {
	switch cwd {
	case "/work/widgets":
		return "acme/widgets", nil
	case "/work/other":
		return "acme/other", nil
	default:
		return "", fmt.Errorf("no github repo for %s", cwd)
	}
}

func numberDeps(g *orphanGH, h stubHerdr) OrphanDeps {
	return OrphanDeps{GH: g, Herdr: h, RepoOf: repoFromCWD}
}

func liveAgentAt(pane, tab, status, cwd string) herdr.Agent {
	agent := liveAgent(pane, tab, status)
	agent.Cwd = cwd
	return agent
}

func mergedNumberItem(number int) gh.PRSearchItem {
	return gh.PRSearchItem{
		Number:     number,
		Title:      "Fix the widget",
		URL:        fmt.Sprintf("https://github.com/acme/widgets/pull/%d", number),
		State:      "merged",
		ClosedAt:   "2026-08-18T22:19:28Z",
		Repository: gh.SearchRepository{NameWithOwner: "acme/widgets"},
	}
}

func TestOrphansPRNumberMerged(t *testing.T) {
	t.Parallel()

	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/widgets#42": mergedNumberItem(42),
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "#42")},
		agents: []herdr.Agent{liveAgentAt("w2:pA", "w2:tA", "idle", "/work/widgets")},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(g.queried) != 0 {
		t.Fatalf("authored search = %v, want none for a #number tab", g.queried)
	}
	if len(doc.OrphanTabs) != 1 {
		t.Fatalf("orphan_tabs = %+v, want 1", doc.OrphanTabs)
	}
	got := doc.OrphanTabs[0]
	if got.Bucket != BucketMerged || got.Ticket != "#42" || got.Label != "#42" {
		t.Fatalf("orphan = %+v", got)
	}
	if got.PR == nil || got.PR.Repo != "acme/widgets" || got.PR.Number != 42 || got.PR.State != "merged" {
		t.Fatalf("pr = %+v", got.PR)
	}
	if got.PR.MergedAt == nil || *got.PR.MergedAt != "2026-08-18T22:19:28Z" {
		t.Fatalf("merged_at = %v", got.PR.MergedAt)
	}
	if len(g.viewed) != 1 || g.viewed[0] != "acme/widgets#42" {
		t.Fatalf("viewed = %v", g.viewed)
	}
}

func TestOrphansPRNumberClosedUnmerged(t *testing.T) {
	t.Parallel()

	item := mergedNumberItem(42)
	item.State = "closed"
	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/widgets#42": item,
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "#42")},
		agents: []herdr.Agent{liveAgentAt("w2:pA", "w2:tA", "idle", "/work/widgets")},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 {
		t.Fatalf("orphan_tabs = %+v, want 1", doc.OrphanTabs)
	}
	got := doc.OrphanTabs[0]
	if got.Bucket != BucketMerged || got.PR == nil || got.PR.State != "closed" || got.PR.MergedAt != nil {
		t.Fatalf("orphan = %+v", got)
	}
}

func TestOrphansPRNumberOpenOmitted(t *testing.T) {
	t.Parallel()

	item := mergedNumberItem(43)
	item.State = "open"
	item.ClosedAt = ""
	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/widgets#43": item,
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tB", "w2", "#43")},
		agents: []herdr.Agent{liveAgentAt("w2:pB", "w2:tB", "idle", "/work/widgets")},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty (open PR)", doc.OrphanTabs)
	}
}

func TestOrphansPRNumberNotFoundIdle(t *testing.T) {
	t.Parallel()

	g := &orphanGH{}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "#42")},
		agents: []herdr.Agent{liveAgentAt("w2:pA", "w2:tA", "idle", "/work/widgets")},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 || doc.OrphanTabs[0].Bucket != BucketNoPR || doc.OrphanTabs[0].PR != nil {
		t.Fatalf("orphan_tabs = %+v, want one no_pr", doc.OrphanTabs)
	}
}

func TestOrphansPRNumberNotFoundWorkingOmitted(t *testing.T) {
	t.Parallel()

	g := &orphanGH{}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "#42")},
		agents: []herdr.Agent{liveAgentAt("w2:pA", "w2:tA", "working", "/work/widgets")},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty", doc.OrphanTabs)
	}
}

func TestOrphansPRNumberUnresolvedRepoWarns(t *testing.T) {
	t.Parallel()

	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/widgets#42": mergedNumberItem(42),
	}}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "#42")},
		agents: []herdr.Agent{liveAgentAt("w2:pA", "w2:tA", "idle", "/not/a/repo")},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want none when the repo cannot be resolved", doc.OrphanTabs)
	}
	if len(g.viewed) != 0 {
		t.Fatalf("viewed = %v, want no view without a repo", g.viewed)
	}
	if len(doc.Warnings) != 1 {
		t.Fatalf("warnings = %v, want 1", doc.Warnings)
	}
}

func TestOrphansPRNumberUsesForegroundCWD(t *testing.T) {
	t.Parallel()

	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/other#42": {
			Number: 42, Title: "other", State: "merged",
			URL:        "https://github.com/acme/other/pull/42",
			ClosedAt:   "2026-08-18T22:19:28Z",
			Repository: gh.SearchRepository{NameWithOwner: "acme/other"},
		},
	}}
	agent := liveAgentAt("w2:pA", "w2:tA", "idle", "/not/a/repo")
	agent.ForegroundCwd = "/work/other"
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "#42")},
		agents: []herdr.Agent{agent},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 1 || doc.OrphanTabs[0].PR == nil || doc.OrphanTabs[0].PR.Repo != "acme/other" {
		t.Fatalf("orphan_tabs = %+v, want acme/other", doc.OrphanTabs)
	}
}

func TestOrphansPRNumberIgnoresNonExactLabels(t *testing.T) {
	t.Parallel()

	g := &orphanGH{}
	h := stubHerdr{
		tabs: []herdr.Tab{
			liveTab("w2:tC", "w2", "#1#2"),
			liveTab("w2:tD", "w2", "#2#3"),
			liveTab("w2:tE", "w2", "#42 extra"),
			liveTab("w2:tF", "w2", "#0"),
		},
		agents: []herdr.Agent{
			liveAgentAt("w2:pC", "w2:tC", "idle", "/work/widgets"),
			liveAgentAt("w2:pD", "w2:tD", "idle", "/work/widgets"),
			liveAgentAt("w2:pE", "w2:tE", "idle", "/work/widgets"),
			liveAgentAt("w2:pF", "w2:tF", "idle", "/work/widgets"),
		},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 || len(g.viewed) != 0 || len(g.queried) != 0 {
		t.Fatalf("orphans=%+v viewed=%v queried=%v, want all skipped", doc.OrphanTabs, g.viewed, g.queried)
	}
}

func TestOrphansPRNumberViewErrorWarns(t *testing.T) {
	t.Parallel()

	g := &orphanGH{viewErr: errors.New("rate limited")}
	h := stubHerdr{
		tabs:   []herdr.Tab{liveTab("w2:tA", "w2", "#42")},
		agents: []herdr.Agent{liveAgentAt("w2:pA", "w2:tA", "idle", "/work/widgets")},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want none on view error", doc.OrphanTabs)
	}
	if len(doc.Warnings) != 1 {
		t.Fatalf("warnings = %v, want 1", doc.Warnings)
	}
}

func TestOrphansPRNumberDoesNotBlockTicketSearch(t *testing.T) {
	t.Parallel()

	g := &orphanGH{
		search: map[string][]gh.PRSearchItem{
			"AP-1306": {{
				Number: 32347, Title: "[AP-1306] Fix", State: "merged",
				ClosedAt:   "2026-08-18T22:19:28Z",
				Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
			}},
		},
		views: map[string]gh.PRSearchItem{
			"acme/widgets#42": mergedNumberItem(42),
		},
	}
	h := stubHerdr{
		tabs: []herdr.Tab{
			liveTab("w3:tA", "w3", "AP-1306"),
			liveTab("w2:tA", "w2", "#42"),
		},
		agents: []herdr.Agent{
			liveAgent("w3:pA", "w3:tA", "idle"),
			liveAgentAt("w2:pA", "w2:tA", "idle", "/work/widgets"),
		},
	}
	doc, err := Orphans(context.Background(), numberDeps(g, h), orphanCfg(), nil, fixtureNow())
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(g.queried) != 1 || g.queried[0] != "AP-1306" {
		t.Fatalf("queried = %v, want only the ticket", g.queried)
	}
	if len(doc.OrphanTabs) != 2 {
		t.Fatalf("orphan_tabs = %+v, want ticket and #number", doc.OrphanTabs)
	}
	if doc.OrphanTabs[0].Ticket != "AP-1306" || doc.OrphanTabs[1].Ticket != "#42" {
		t.Fatalf("order = %s then %s", doc.OrphanTabs[0].Ticket, doc.OrphanTabs[1].Ticket)
	}
}

func TestCloseMergedPRNumberReverifiesByView(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/widgets#42": mergedNumberItem(42),
	}}
	h := &closeHerdr{tabs: []herdr.Tab{liveTab("w2:tA", "w2", "#42")}}
	tab := numberMergedOrphan("w2:tA", 42)
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{tab}, fixtureNow())
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionClosed {
		t.Fatalf("results = %+v, want closed", got.Results)
	}
	if len(h.closed) != 1 || h.closed[0] != "w2:tA" {
		t.Fatalf("closed = %v", h.closed)
	}
	if len(g.queried) != 0 {
		t.Fatalf("authored search = %v, want view instead", g.queried)
	}
	if len(g.viewed) != 1 || g.viewed[0] != "acme/widgets#42" {
		t.Fatalf("viewed = %v", g.viewed)
	}
}

func TestCloseMergedPRNumberSkipsWhenReopened(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	item := mergedNumberItem(42)
	item.State = "open"
	item.ClosedAt = ""
	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/widgets#42": item,
	}}
	h := &closeHerdr{tabs: []herdr.Tab{liveTab("w2:tA", "w2", "#42")}}
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{
		numberMergedOrphan("w2:tA", 42),
	}, fixtureNow())
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

func TestCloseMergedPRNumberClosedStillCloses(t *testing.T) {
	t.Parallel()

	cfg := orphanCfg()
	cfg.DryRun = false
	item := mergedNumberItem(42)
	item.State = "closed"
	g := &orphanGH{views: map[string]gh.PRSearchItem{
		"acme/widgets#42": item,
	}}
	h := &closeHerdr{tabs: []herdr.Tab{liveTab("w2:tA", "w2", "#42")}}
	got, err := CloseMerged(context.Background(), CloseDeps{GH: g, Herdr: h}, cfg, "alice", []OrphanTab{
		numberMergedOrphan("w2:tA", 42),
	}, fixtureNow())
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionClosed {
		t.Fatalf("results = %+v, want closed", got.Results)
	}
}

func numberMergedOrphan(tabID string, number int) OrphanTab {
	mergedAt := "2026-08-18T22:19:28Z"
	label := fmt.Sprintf("#%d", number)
	return OrphanTab{
		TabID:       tabID,
		WorkspaceID: "w2",
		Label:       label,
		Ticket:      label,
		AgentStatus: "idle",
		Bucket:      BucketMerged,
		PR: &OrphanPR{
			Repo: "acme/widgets", Number: number,
			URL:   fmt.Sprintf("https://github.com/acme/widgets/pull/%d", number),
			State: "merged", MergedAt: &mergedAt,
		},
	}
}
