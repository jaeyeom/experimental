package scan

import (
	"context"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
)

const (
	staleTTL      = 6 * time.Hour
	staleMaxStale = 72 * time.Hour
)

// staleCache is a round cache at fixtureNow with a 6h history TTL and a
// 72h max stale, and the history version a round at fixtureNow uses.
func staleCache(t *testing.T, clock *time.Time) (*roundcache.Cache, string) {
	t.Helper()
	*clock = fixtureNow
	cache := newRoundCache(t, clock)
	cache.SetHistoryTTL(staleTTL)
	cache.SetHistoryMaxStale(staleMaxStale)
	return cache, "since:" + fixtureNow.AddDate(0, 0, -90).UTC().Format(time.DateOnly)
}

// storeAt stores history for path as of at, authored by login.
func storeAt(cache *roundcache.Cache, clock *time.Time, at time.Time, path, version, login string) {
	saved := *clock
	*clock = at
	cache.PutPathHistory("acme/widgets", "main", path, version, gh.PathHistory{Authors: []string{login}})
	*clock = saved
}

// staleRound runs one round at fixtureNow for one PR touching paths that
// needs cover for acme/foo (bob and carol), ranking by history. GitHub
// says bob authored every path.
func staleRound(t *testing.T, cache *roundcache.Cache, paths []string, maxCalls, batch int, refresh bool, refreshMax int) (*scriptGH, Document) {
	t.Helper()
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.files = map[string][]string{"acme/widgets#1": paths}
	g.history = map[string]gh.PathHistory{}
	for _, path := range paths {
		g.history["acme/widgets\x00"+path] = gh.PathHistory{Authors: []string{"bob"}}
	}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	cfg.TeamCoverMaxCalls = maxCalls
	cfg.TeamCoverBatchPaths = batch
	cfg.TeamCoverHistoryTTL = staleTTL
	cfg.TeamCoverHistoryMaxStale = staleMaxStale
	cfg.TeamCoverRefreshMaxCalls = refreshMax
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	deps.HistoryRefresh = refresh
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	return g, doc
}

func TestRunTeamCoverUsesStaleHistoryUnderCap(t *testing.T) {
	t.Parallel()

	var clock time.Time
	cache, version := staleCache(t, &clock)
	storedAt := fixtureNow.Add(-7 * time.Hour)
	storeAt(cache, &clock, storedAt, "src/a.go", version, "carol")

	// Snapshot, members and files take the whole cap of 3.
	g, doc := staleRound(t, cache, []string{"src/a.go"}, 3, 5, false, 10)
	if g.historyCalls != 0 {
		t.Fatalf("history calls = %d, want none past the cap", g.historyCalls)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggestions = %+v, want carol from the stale history", got)
	}
	reason := "history as of " + storedAt.UTC().Format(time.DateOnly)
	if !slices.Contains(got[0].Suggest[0].Reasons, reason) {
		t.Fatalf("reasons = %q, want %q", got[0].Suggest[0].Reasons, reason)
	}
	for _, note := range doc.TeamCoverNotes {
		if strings.Contains(note, "deferred") {
			t.Fatalf("notes = %q, want nothing deferred", doc.TeamCoverNotes)
		}
	}
}

func TestRunTeamCoverDefersHistoryPastMaxStale(t *testing.T) {
	t.Parallel()

	var clock time.Time
	cache, version := staleCache(t, &clock)
	storeAt(cache, &clock, fixtureNow.Add(-staleMaxStale-time.Hour), "src/a.go", version, "carol")

	_, doc := staleRound(t, cache, []string{"src/a.go"}, 3, 5, false, 10)
	assertSkipped(t, doc.PRs[0].TeamReviewSuggestions, "skipped: call cap", "acme/foo")
	if !slices.Contains(doc.TeamCoverNotes, "team-cover: call cap 3 reached during history; 1 paths deferred") {
		t.Fatalf("notes = %q, want the path deferred", doc.TeamCoverNotes)
	}
}

func TestRunTeamCoverRefetchesStaleHistoryWithBudget(t *testing.T) {
	t.Parallel()

	var clock time.Time
	cache, version := staleCache(t, &clock)
	storeAt(cache, &clock, fixtureNow.Add(-7*time.Hour), "src/a.go", version, "carol")

	g, doc := staleRound(t, cache, []string{"src/a.go"}, 40, 5, false, 10)
	if g.historyCalls != 1 || !slices.Equal(g.historyPaths[0], []string{"src/a.go"}) {
		t.Fatalf("history calls = %d paths = %#v, want the stale path refetched", g.historyCalls, g.historyPaths)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "bob" {
		t.Fatalf("suggestions = %+v, want bob from the refetched history", got)
	}
	for _, reason := range got[0].Suggest[0].Reasons {
		if strings.HasPrefix(reason, "history as of") {
			t.Fatalf("reasons = %q, want no stale reason", got[0].Suggest[0].Reasons)
		}
	}
	if hist, ok := cache.PathHistory("acme/widgets", "main", "src/a.go", version); !ok || !slices.Equal(hist.Authors, []string{"bob"}) {
		t.Fatalf("stored = %+v, %v; want the refetched history fresh", hist, ok)
	}
}

func TestRunTeamCoverRefreshesOldestStaleHistoryFirst(t *testing.T) {
	t.Parallel()

	var clock time.Time
	cache, version := staleCache(t, &clock)
	// Stored newest to oldest, so the oldest-first order is the reverse.
	paths := []string{"src/a.go", "src/b.go", "src/c.go", "src/d.go"}
	for i, path := range paths {
		storeAt(cache, &clock, fixtureNow.Add(-time.Duration(7+i)*time.Hour), path, version, "carol")
	}
	// A fresh entry near expiry the PR does not touch.
	storeAt(cache, &clock, fixtureNow.Add(-staleTTL/2), "src/fresh.go", version, "carol")

	// Snapshot, members and files take 3 calls; the rest is the refresh
	// share, which the PR pass leaves to the refresh, one path a call.
	const refreshMax = 2
	g, doc := staleRound(t, cache, paths, 3+refreshMax, 1, true, refreshMax)
	if g.coverCalls != 3+refreshMax {
		t.Fatalf("calls = %d, want %d", g.coverCalls, 3+refreshMax)
	}
	oldest := []string{"src/d.go", "src/c.go", "src/b.go", "src/a.go"}
	if g.historyCalls != 1 || !slices.Equal(g.historyPaths[0][:len(oldest)], oldest) {
		t.Fatalf("history calls = %d paths = %#v, want one refresh, oldest stale first %v", g.historyCalls, g.historyPaths, oldest)
	}
	for i, path := range oldest {
		hist, ok := cache.PathHistory("acme/widgets", "main", path, version)
		if refreshed := i < refreshMax; refreshed != (ok && slices.Equal(hist.Authors, []string{"bob"})) {
			t.Fatalf("%s = %+v, %v; refreshed want %v", path, hist, ok, refreshed)
		}
	}
	if hist, ok := cache.PathHistory("acme/widgets", "main", "src/fresh.go", version); !ok || !slices.Equal(hist.Authors, []string{"carol"}) {
		t.Fatalf("fresh entry = %+v, %v; want it left alone", hist, ok)
	}
	if !strings.Contains(strings.Join(doc.TeamCoverNotes, "\n"), "refresh 2") {
		t.Fatalf("notes = %q, want refresh 2", doc.TeamCoverNotes)
	}
}
