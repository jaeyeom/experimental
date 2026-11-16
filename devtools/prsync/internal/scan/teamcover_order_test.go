package scan

import (
	"context"
	"fmt"
	"slices"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
)

const orderUpdatedAt = "2026-11-01T11:00:00Z"

// orderCache is a stored cache holding the members of acme/cov, which
// "someone" (requested on every test PR) covers, and of the uncovered
// teams acme/foo, acme/bar and acme/skip.
func orderCache(t *testing.T) *roundcache.Cache {
	t.Helper()
	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	cache.PutTeamMembers("acme", "cov", []gh.TeamMember{{Login: "someone", Type: "User"}})
	for _, slug := range []string{"foo", "bar", "skip"} {
		cache.PutTeamMembers("acme", slug, []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}})
	}
	return cache
}

// orderPR is a PR on main that requests "someone" and the given teams.
// cached stores an empty review snapshot for it in cache.
func orderPR(cache HistoryCache, number int, created string, cached bool, slugs ...string) PR {
	reqs := []string{"User:someone"}
	for _, slug := range slugs {
		reqs = append(reqs, "Team:"+slug)
	}
	pr := PR{
		Repo: "acme/widgets", Number: number, Base: "main",
		HeadSHA: "h", UpdatedAt: orderUpdatedAt, CreatedAt: created, ReviewRequests: reqs,
	}
	if cached {
		cache.PutReviewSnapshot(pr.Repo, pr.Number, pr.HeadSHA, pr.UpdatedAt, "alice", nil)
	}
	return pr
}

func workNumbers(prs []PR, order []workItem) []int {
	out := make([]int, len(order))
	for i, item := range order {
		out[i] = prs[item.index].Number
	}
	return out
}

func TestTeamCoverWorkOrder(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		prs  func(HistoryCache) []PR
		want []int
	}{
		{
			name: "uncovered before covered",
			prs: func(c HistoryCache) []PR {
				return []PR{orderPR(c, 1, "", true, "cov"), orderPR(c, 2, "", true, "foo")}
			},
			want: []int{2, 1},
		},
		{
			name: "more uncovered teams, then newer",
			prs: func(c HistoryCache) []PR {
				return []PR{
					orderPR(c, 1, "2026-10-01T00:00:00Z", true, "foo"),
					orderPR(c, 2, "2026-10-03T00:00:00Z", true, "foo"),
					orderPR(c, 3, "2026-09-01T00:00:00Z", true, "foo", "bar"),
					orderPR(c, 4, "2026-10-03T00:00:00Z", true, "bar"),
				}
			},
			want: []int{3, 2, 4, 1},
		},
		{
			name: "skipped team counts as covered",
			prs: func(c HistoryCache) []PR {
				return []PR{orderPR(c, 1, "", true, "cov", "skip"), orderPR(c, 2, "", false, "cov")}
			},
			want: []int{2, 1},
		},
		{
			name: "unknown between uncovered and covered",
			prs: func(c HistoryCache) []PR {
				return []PR{
					orderPR(c, 1, "", true, "cov"),
					orderPR(c, 2, "", false, "foo"),
					orderPR(c, 3, "", true, "foo"),
				}
			},
			want: []int{3, 2, 1},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			cfg := teamCoverCfg()
			cfg.TeamCoverSkipTeams = []string{"acme/skip"}
			cache := orderCache(t)
			prs := tt.prs(cache)
			c := newCoverCache(cfg, fixtureNow)
			c.stored = cache
			if got := workNumbers(prs, c.workOrder(cfg, prs)); !slices.Equal(got, tt.want) {
				t.Fatalf("work order = %v, want %v", got, tt.want)
			}
		})
	}
}

// orderRound runs one round over prs with the CODEOWNERS signal, so each
// PR that ranks spends a files and a CODEOWNERS call in its first pass.
func orderRound(t *testing.T, cache HistoryCache, maxCalls int, prs ...gh.PRListItem) (*scriptGH, Document) {
	t.Helper()
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
		"acme/bar": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.list["acme/widgets"] = listResult{prs: prs}
	g.files = map[string][]string{}
	for _, pr := range prs {
		g.files[fmt.Sprintf("acme/widgets#%d", pr.Number)] = []string{"src/a.go"}
	}
	g.codeowners = map[string]string{"acme/widgets@main": "/src/ @carol\n"}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"codeowners"}
	cfg.TeamCoverActiveDays = 0
	cfg.TeamCoverMaxCalls = maxCalls
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	return g, doc
}

// listPR is a listed PR that requests "someone" and slugs; cached stores an
// empty review snapshot for it in cache.
func listPR(cache HistoryCache, number int, cached bool, slugs ...string) gh.PRListItem {
	item := teamPRSlug(number, slugs...)
	item.HeadRefOid = "h"
	item.UpdatedAt = orderUpdatedAt
	if cached {
		cache.PutReviewSnapshot("acme/widgets", number, "h", orderUpdatedAt, "alice", nil)
	}
	return item
}

func TestRunTeamCoverCapGoesToUncoveredPRFirst(t *testing.T) {
	t.Parallel()

	cache := orderCache(t)
	// PR 1 is covered and PR 2 is not; both are known from the cache.
	// The cap fits PR 2's files and CODEOWNERS calls.
	g, doc := orderRound(t, cache, 2, listPR(cache, 1, true, "cov"), listPR(cache, 2, true, "foo"))
	if !slices.Equal(g.fileKeys, []string{"acme/widgets#2"}) {
		t.Fatalf("file calls = %q, want only the uncovered PR", g.fileKeys)
	}
	if got := doc.PRs[1].TeamReviewSuggestions; len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("uncovered PR suggestions = %+v, want carol", got)
	}
	if got := doc.PRs[0].TeamReviewSuggestions; len(got) != 1 || got[0].CoveredBy == nil || *got[0].CoveredBy != "someone" {
		t.Fatalf("covered PR suggestions = %+v, want covered by someone", got)
	}
}

func TestRunTeamCoverCapDefersUnknownPRBeforeUncovered(t *testing.T) {
	t.Parallel()

	cache := orderCache(t)
	// PR 1 has no stored review snapshot, so its coverage is unknown. The
	// cap fits PR 2's two calls; PR 1 waits.
	g, doc := orderRound(t, cache, 2, listPR(cache, 1, false, "bar"), listPR(cache, 2, true, "foo"))
	if !slices.Equal(g.fileKeys, []string{"acme/widgets#2"}) {
		t.Fatalf("file calls = %q, want the uncovered PR first", g.fileKeys)
	}
	if got := doc.PRs[1].TeamReviewSuggestions; len(got) != 1 || len(got[0].Suggest) != 1 {
		t.Fatalf("uncovered PR suggestions = %+v, want one suggestion", got)
	}
	if got := doc.PRs[0].TeamReviewSuggestions; got != nil {
		t.Fatalf("unknown PR suggestions = %+v, want none until next round", got)
	}
	want := "team-cover: call cap 2 reached during reviews; 1 PRs deferred"
	if !slices.Contains(doc.TeamCoverNotes, want) {
		t.Fatalf("notes = %q, want %q", doc.TeamCoverNotes, want)
	}
}

func TestRunTeamCoverKeepsDocumentOrder(t *testing.T) {
	t.Parallel()

	cache := orderCache(t)
	g, doc := orderRound(t, cache, 40,
		listPR(cache, 1, true, "cov"), listPR(cache, 2, false, "bar"), listPR(cache, 3, true, "foo"))
	if !slices.Equal(g.fileKeys, []string{"acme/widgets#3", "acme/widgets#2"}) {
		t.Fatalf("file calls = %q, want uncovered, then unknown", g.fileKeys)
	}
	var numbers []int
	var teams []string
	for _, pr := range doc.PRs {
		numbers = append(numbers, pr.Number)
		if len(pr.TeamReviewSuggestions) != 1 {
			t.Fatalf("pr %d suggestions = %+v, want one team", pr.Number, pr.TeamReviewSuggestions)
		}
		teams = append(teams, pr.TeamReviewSuggestions[0].Team)
	}
	if !slices.Equal(numbers, []int{1, 2, 3}) || !slices.Equal(teams, []string{"acme/cov", "acme/bar", "acme/foo"}) {
		t.Fatalf("output = %v %q, want the listed order with each PR's own team", numbers, teams)
	}
}
