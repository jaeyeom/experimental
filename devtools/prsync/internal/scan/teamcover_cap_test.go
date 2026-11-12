package scan

import (
	"context"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
)

func manyPaths(n int) []string {
	paths := make([]string, n)
	for i := range paths {
		paths[i] = fmt.Sprintf("src/f%03d.go", i)
	}
	return paths
}

// capRound runs one round for one PR that touches paths and needs cover
// for acme/foo, ranking by history with team_cover_max_calls=40.
func capRound(t *testing.T, cache HistoryCache, paths []string) (*scriptGH, Document) {
	t.Helper()
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.files = map[string][]string{"acme/widgets#1": paths}
	g.history = map[string]gh.PathHistory{"acme/widgets\x00" + paths[len(paths)-1]: {Authors: []string{"carol"}}}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	cfg.TeamCoverMaxCalls = 40
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	return g, doc
}

func TestRunTeamCoverCallCapDefersPathsToNextRound(t *testing.T) {
	t.Parallel()

	cache := &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	paths := manyPaths(300)
	first, doc := capRound(t, cache, paths)
	if first.coverCalls != 40 {
		t.Fatalf("calls = %d, want exactly the cap of 40", first.coverCalls)
	}
	// Snapshot, members and files take 3 calls; 37 batches of 5 follow.
	if !slices.Contains(doc.TeamCoverNotes, "team-cover: call cap 40 reached during history; 115 paths deferred") {
		t.Fatalf("notes = %q, want the cap line", doc.TeamCoverNotes)
	}
	if got := doc.PRs[0].TeamReviewSuggestions; got != nil {
		t.Fatalf("suggestions = %+v, want none until the history is complete", got)
	}

	second, doc := capRound(t, cache, paths)
	if len(second.historyPaths) != 1 || !slices.Equal(second.historyPaths[0], paths[185:]) {
		t.Fatalf("second round history paths = %d, want the 115 deferred ones", len(second.historyPaths[0]))
	}
	if second.coverCalls != 3+23 {
		t.Fatalf("second round calls = %d, want 26", second.coverCalls)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggestions = %+v, want carol once the history is complete", got)
	}
	want := "team-cover: paths 300/300 need history (185 hits); team members 0 hits/1 calls; files 0/1; reviews 0/1; calls 26 (cap 40)"
	if !slices.Contains(doc.TeamCoverNotes, want) {
		t.Fatalf("notes = %q, want %q", doc.TeamCoverNotes, want)
	}
}

func TestRunTeamCoverRateLimitStopsRound(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}}},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPR(1), teamPR(2)}}
	g.snapshots = map[string]snapResult{
		"acme/widgets#1": {err: fmt.Errorf("%w: You have exceeded a secondary rate limit", gh.ErrRateLimited)},
	}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"codeowners", "history", "load"}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.coverCalls != 1 {
		t.Fatalf("calls = %d, want none after the rate limit", g.coverCalls)
	}
	for _, pr := range doc.PRs {
		if pr.TeamReviewSuggestions != nil {
			t.Fatalf("pr %d suggestions = %+v, want none this round", pr.Number, pr.TeamReviewSuggestions)
		}
	}
	if !slices.Contains(doc.TeamCoverNotes, "team-cover: stopped (rate limited)") {
		t.Fatalf("notes = %q, want the stopped line", doc.TeamCoverNotes)
	}
	if countSubstr(doc.Warnings, "team-cover: stopped (rate limited)") != 1 {
		t.Fatalf("warnings = %q", doc.Warnings)
	}
}

func TestRunTeamCoverLoadCachedAcrossRounds(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), func() time.Time { return clock })
	if err != nil {
		t.Fatal(err)
	}
	cache.SetLoadTTL(time.Hour)
	round := func(now time.Time) *scriptGH {
		t.Helper()
		g := teamCoverGH(map[string]memberCall{
			"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}}},
		})
		g.load = map[string]int{"acme/bob": 2}
		cfg := teamCoverCfg()
		cfg.TeamCoverSignals = []string{"load"}
		deps := depsWith(g, fixtureHerdr{})
		deps.History = cache
		doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, now)
		if err != nil {
			t.Fatal(err)
		}
		got := doc.PRs[0].TeamReviewSuggestions
		if len(got) != 1 || len(got[0].Suggest) != 1 || !slices.Contains(got[0].Suggest[0].Reasons, "2 open direct requests") {
			t.Fatalf("suggestions = %+v, want bob with his load", got)
		}
		return g
	}
	if g := round(clock); g.loadCalls != 1 {
		t.Fatalf("first round searches = %d, want 1", g.loadCalls)
	}
	clock = fixtureNow.Add(30 * time.Minute)
	if g := round(clock); g.loadCalls != 0 {
		t.Fatalf("within the load TTL: searches = %d, want 0", g.loadCalls)
	}
	clock = fixtureNow.Add(61 * time.Minute)
	if g := round(clock); g.loadCalls != 1 {
		t.Fatalf("past the load TTL: searches = %d, want 1", g.loadCalls)
	}
}

func TestRunTeamCoverSummaryNote(t *testing.T) {
	t.Parallel()

	g, doc, _ := historyRound(t, &mapHistoryCache{entries: map[string]gh.PathHistory{}}, "h", "base1")
	want := fmt.Sprintf("team-cover: paths 2/2 need history (0 hits); team members 0 hits/1 calls; files 0/2; reviews 0/2; calls %d (cap 40)", g.coverCalls)
	if !slices.Equal(doc.TeamCoverNotes, []string{want}) {
		t.Fatalf("notes = %q, want %q", doc.TeamCoverNotes, want)
	}
	for _, warning := range doc.Warnings {
		if strings.Contains(warning, "need history") {
			t.Fatalf("warnings = %q, want the summary only in the round output", doc.Warnings)
		}
	}
}
