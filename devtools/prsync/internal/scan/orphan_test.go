package scan

import (
	"context"
	"errors"
	"strings"
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

func TestTicketSearchQueries(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		tickets []string
		want    []string
	}{
		{name: "empty", tickets: nil, want: nil},
		{name: "one passes through", tickets: []string{"AP-1"}, want: []string{"AP-1"}},
		{
			name:    "two joined with OR",
			tickets: []string{"AP-1", "AP-2"},
			want:    []string{"(AP-1 OR AP-2)"},
		},
		{
			name:    "six is one query of five ORs",
			tickets: []string{"A-1", "A-2", "A-3", "A-4", "A-5", "A-6"},
			want:    []string{"(A-1 OR A-2 OR A-3 OR A-4 OR A-5 OR A-6)"},
		},
		{
			name:    "seven splits after five ORs",
			tickets: []string{"A-1", "A-2", "A-3", "A-4", "A-5", "A-6", "A-7"},
			want:    []string{"(A-1 OR A-2 OR A-3 OR A-4 OR A-5 OR A-6)", "A-7"},
		},
		{
			name: "splits before exceeding 256-char query",
			tickets: []string{
				"TICKET-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
				"TICKET-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
				"TICKET-CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
				"TICKET-DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD",
				"TICKET-EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE",
			},
			want: []string{
				"(TICKET-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA OR TICKET-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB OR TICKET-CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC OR TICKET-DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD)",
				"TICKET-EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE",
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := ticketSearchQueries(tc.tickets)
			if len(got) != len(tc.want) {
				t.Fatalf("queries = %v, want %v", got, tc.want)
			}
			for i := range got {
				if got[i] != tc.want[i] {
					t.Fatalf("queries[%d] = %q, want %q", i, got[i], tc.want[i])
				}
				if len(got[i]) > maxSearchQueryLen && len(tc.tickets) > 1 {
					t.Fatalf("queries[%d] length %d exceeds %d", i, len(got[i]), maxSearchQueryLen)
				}
			}
		})
	}
}

func TestOrphansBatchesMultipleTicketsIntoOneSearch(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", URL: "https://gh/acme/x/pull/32347",
			State: "merged", ClosedAt: "2026-08-18T22:19:28Z",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
		"AP-1287": {},
		"AP-1400": {{
			Number: 500, Title: "[AP-1400] WIP", State: "open",
			Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := stubHerdr{
		tabs: []herdr.Tab{
			liveTab("w2:tA", "w2", "AP-1306"),
			liveTab("w2:tB", "w2", "AP-1287"),
			liveTab("w2:tC", "w2", "AP-1400"),
		},
		agents: []herdr.Agent{
			liveAgent("w2:pA", "w2:tA", "idle"),
			liveAgent("w2:pB", "w2:tB", "idle"),
			liveAgent("w2:pC", "w2:tC", "idle"),
		},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(g.queried) != 1 || g.queried[0] != "(AP-1306 OR AP-1287 OR AP-1400)" {
		t.Fatalf("queried = %v, want one batched OR query", g.queried)
	}
	if len(doc.OrphanTabs) != 2 {
		t.Fatalf("orphan_tabs = %+v, want 2 (merged + no_pr; open omitted)", doc.OrphanTabs)
	}
	if doc.OrphanTabs[0].Ticket != "AP-1306" || doc.OrphanTabs[0].Bucket != BucketMerged {
		t.Fatalf("orphan[0] = %+v, want AP-1306 merged", doc.OrphanTabs[0])
	}
	if doc.OrphanTabs[1].Ticket != "AP-1287" || doc.OrphanTabs[1].Bucket != BucketNoPR {
		t.Fatalf("orphan[1] = %+v, want AP-1287 no_pr", doc.OrphanTabs[1])
	}
}

func TestOrphansChunksWhenOverFiveOROperators(t *testing.T) {
	t.Parallel()

	tickets := []string{"AP-1", "AP-2", "AP-3", "AP-4", "AP-5", "AP-6", "AP-7"}
	search := make(map[string][]gh.PRSearchItem, len(tickets))
	tabs := make([]herdr.Tab, 0, len(tickets))
	agents := make([]herdr.Agent, 0, len(tickets))
	for _, ticket := range tickets {
		search[ticket] = nil
		id := "w2:t" + ticket
		tabs = append(tabs, liveTab(id, "w2", ticket))
		agents = append(agents, liveAgent("w2:p"+ticket, id, "idle"))
	}
	g := &orphanGH{search: search}
	h := stubHerdr{tabs: tabs, agents: agents}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	want := []string{
		"(AP-1 OR AP-2 OR AP-3 OR AP-4 OR AP-5 OR AP-6)",
		"AP-7",
	}
	if len(g.queried) != len(want) {
		t.Fatalf("queried = %v, want %v", g.queried, want)
	}
	for i := range want {
		if g.queried[i] != want[i] {
			t.Fatalf("queried[%d] = %q, want %q", i, g.queried[i], want[i])
		}
	}
	if len(doc.OrphanTabs) != 7 {
		t.Fatalf("len(orphan_tabs) = %d, want 7", len(doc.OrphanTabs))
	}
}

func TestOrphansDuplicateTicketSharesSearch(t *testing.T) {
	t.Parallel()

	g := &orphanGH{search: map[string][]gh.PRSearchItem{
		"AP-1306": {{
			Number: 32347, Title: "[AP-1306] Fix", State: "merged",
			ClosedAt: "2026-08-18T22:19:28Z", Repository: gh.SearchRepository{NameWithOwner: "acme/x"},
		}},
	}}
	h := stubHerdr{
		tabs: []herdr.Tab{
			liveTab("w2:tA", "w2", "AP-1306"),
			liveTab("w2:tB", "w2", "AP-1306"),
		},
		agents: []herdr.Agent{
			liveAgent("w2:pA", "w2:tA", "idle"),
			liveAgent("w2:pB", "w2:tB", "idle"),
		},
	}
	doc, err := Orphans(context.Background(), OrphanDeps{GH: g, Herdr: h}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	if len(g.queried) != 1 || g.queried[0] != "AP-1306" {
		t.Fatalf("queried = %v, want one AP-1306 search", g.queried)
	}
	if len(doc.OrphanTabs) != 2 {
		t.Fatalf("orphan_tabs = %+v, want both tabs", doc.OrphanTabs)
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
	if items, ok := g.search[query]; ok {
		return items, nil
	}
	var out []gh.PRSearchItem
	for _, ticket := range ticketsInSearchQuery(query) {
		out = append(out, g.search[ticket]...)
	}
	if out == nil {
		out = []gh.PRSearchItem{}
	}
	return out, nil
}

func ticketsInSearchQuery(query string) []string {
	q := strings.TrimSuffix(strings.TrimPrefix(query, "("), ")")
	if q == "" {
		return nil
	}
	return strings.Split(q, " OR ")
}
