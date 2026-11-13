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
	want := "team-cover: paths 300/300 need history (185 hits); team members 0 hits/1 calls; files 0/1; reviews 0/1; refresh 0; calls 26 (cap 40)"
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

// pathHistoryLife is the age at which path stops being a hit.
func pathHistoryLife(c *roundcache.Cache, clock *time.Time, start time.Time, repo, ref, path, version string, ttl time.Duration) time.Duration {
	lo, hi := time.Duration(0), ttl
	for hi-lo > time.Millisecond {
		mid := lo + (hi-lo)/2
		*clock = start.Add(mid)
		if _, ok := c.PathHistory(repo, ref, path, version); ok {
			lo = mid
			continue
		}
		hi = mid
	}
	*clock = start
	return hi
}

// historyRefreshFixture stores paths at one instant and returns them nearest
// expiry first. The PR in the round is already covered, so team-cover's own
// history fetch is empty and only an early refresh calls PathHistories.
func historyRefreshFixture(t *testing.T) (clock time.Time, cache *roundcache.Cache, order []string, version string) {
	t.Helper()
	clock = fixtureNow
	cache = newRoundCache(t, &clock)
	const ttl = 6 * time.Hour
	cache.SetHistoryTTL(ttl)
	version = "since:" + clock.AddDate(0, 0, -90).UTC().Format(time.DateOnly)
	paths := make([]string, 8)
	for i := range paths {
		paths[i] = fmt.Sprintf("src/p%02d.go", i)
		cache.PutPathHistory("acme/widgets", "main", paths[i], version, gh.PathHistory{Authors: []string{"old"}})
	}
	start := clock
	lives := make(map[string]time.Duration, len(paths))
	for _, path := range paths {
		lives[path] = pathHistoryLife(cache, &clock, start, "acme/widgets", "main", path, version, ttl)
	}
	order = slices.Clone(paths)
	slices.SortFunc(order, func(a, b string) int {
		if lives[a] != lives[b] {
			if lives[a] < lives[b] {
				return -1
			}
			return 1
		}
		return strings.Compare(a, b)
	})
	return clock, cache, order, version
}

func runCoveredHistoryRound(t *testing.T, cache *roundcache.Cache, now time.Time, paths []string, maxCalls, batch int, refresh bool, refreshMax int) (*scriptGH, Document) {
	t.Helper()
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "someone", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.history = map[string]gh.PathHistory{}
	for _, path := range paths {
		g.history["acme/widgets\x00"+path] = gh.PathHistory{Authors: []string{"new"}}
	}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	cfg.TeamCoverMaxCalls = maxCalls
	cfg.TeamCoverBatchPaths = batch
	cfg.TeamCoverHistoryTTL = 6 * time.Hour
	if refreshMax > 0 {
		cfg.TeamCoverRefreshMaxCalls = refreshMax
	}
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	deps.HistoryRefresh = refresh
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, now)
	if err != nil {
		t.Fatal(err)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || got[0].CoveredBy == nil || *got[0].CoveredBy != "someone" {
		t.Fatalf("suggestions = %+v, want the team covered by someone", got)
	}
	return g, doc
}

func TestRunTeamCoverSpareBudgetRefreshesNearestHistory(t *testing.T) {
	t.Parallel()

	// Snapshot and members take 2 calls. Two calls remain, one path per
	// batch, so the two entries nearest expiry are refreshed and the cap holds.
	const maxCalls = 4
	const spare = 2
	clock, cache, order, version := historyRefreshFixture(t)
	g, doc := runCoveredHistoryRound(t, cache, clock, order, maxCalls, 1, true, 0)
	if g.coverCalls != maxCalls {
		t.Fatalf("calls = %d, want exactly the cap of %d", g.coverCalls, maxCalls)
	}
	if g.historyCalls != 1 || !slices.Equal(g.historyPaths[0], order) {
		t.Fatalf("history calls = %d paths = %#v, want one batched refresh, nearest first %v", g.historyCalls, g.historyPaths, order)
	}
	for i, path := range order {
		got, ok := cache.PathHistory("acme/widgets", "main", path, version)
		if !ok {
			t.Fatalf("%s missing after the round", path)
		}
		want := "old"
		if i < spare {
			want = "new"
		}
		if len(got.Authors) != 1 || got.Authors[0] != want {
			t.Fatalf("%s authors = %v, want %s", path, got.Authors, want)
		}
	}
	if !strings.Contains(strings.Join(doc.TeamCoverNotes, "\n"), "refresh 2") {
		t.Fatalf("notes = %q, want the refresh count", doc.TeamCoverNotes)
	}
}

func TestRunTeamCoverScopedRunSkipsHistoryRefresh(t *testing.T) {
	t.Parallel()

	// A single-PR scan has spare budget and entries near expiry, and
	// still refetches nothing. RunTargets is that scan.
	clock, cache, order, version := historyRefreshFixture(t)
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "someone", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.view = map[string]viewResult{"acme/widgets#1": {item: teamPR(1)}}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	cfg.TeamCoverMaxCalls = 40
	cfg.TeamCoverHistoryTTL = 6 * time.Hour
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	deps.HistoryRefresh = true
	doc, err := RunTargets(context.Background(), deps, cfg, []Target{{Repo: "acme/widgets", Number: 1}}, clock)
	if err != nil {
		t.Fatal(err)
	}
	if g.historyCalls != 0 {
		t.Fatalf("history calls = %d paths = %#v, want no early refresh", g.historyCalls, g.historyPaths)
	}
	for _, path := range order {
		got, ok := cache.PathHistory("acme/widgets", "main", path, version)
		if !ok || len(got.Authors) != 1 || got.Authors[0] != "old" {
			t.Fatalf("%s = %+v, %v; want the stored history left as it was", path, got, ok)
		}
	}
	if !strings.Contains(strings.Join(doc.TeamCoverNotes, "\n"), "refresh 0") {
		t.Fatalf("notes = %q, want refresh 0", doc.TeamCoverNotes)
	}
}

func TestRunTeamCoverRefreshCapLimitsSpareBudget(t *testing.T) {
	t.Parallel()

	// The covered PR leaves far more than the refresh cap. Only the cap's
	// worth of nearest entries is refetched, and those calls count.
	const refreshMax = 2
	clock, cache, order, version := historyRefreshFixture(t)
	g, doc := runCoveredHistoryRound(t, cache, clock, order, 40, 1, true, refreshMax)
	if g.historyCalls != 1 || !slices.Equal(g.historyPaths[0], order) {
		t.Fatalf("history calls = %d paths = %#v, want one batched refresh, nearest first %v", g.historyCalls, g.historyPaths, order)
	}
	if g.coverCalls != 2+refreshMax {
		t.Fatalf("calls = %d, want the two setup calls plus the refresh cap", g.coverCalls)
	}
	for i, path := range order {
		got, ok := cache.PathHistory("acme/widgets", "main", path, version)
		if !ok {
			t.Fatalf("%s missing after the round", path)
		}
		want := "old"
		if i < refreshMax {
			want = "new"
		}
		if len(got.Authors) != 1 || got.Authors[0] != want {
			t.Fatalf("%s authors = %v, want %s", path, got.Authors, want)
		}
	}
	if !strings.Contains(strings.Join(doc.TeamCoverNotes, "\n"), "refresh 2") {
		t.Fatalf("notes = %q, want the refresh count", doc.TeamCoverNotes)
	}
}

func TestRunTeamCoverNoSpareBudgetSkipsHistoryRefresh(t *testing.T) {
	t.Parallel()

	// The covered PR uses the whole cap on its snapshot and member lookup.
	const maxCalls = 2
	clock, cache, order, version := historyRefreshFixture(t)
	g, _ := runCoveredHistoryRound(t, cache, clock, order, maxCalls, 1, true, 0)
	if g.coverCalls != maxCalls {
		t.Fatalf("calls = %d, want the cap of %d with nothing past it", g.coverCalls, maxCalls)
	}
	if g.historyCalls != 0 {
		t.Fatalf("history calls = %d paths = %#v, want no early refresh", g.historyCalls, g.historyPaths)
	}
	for _, path := range order {
		got, ok := cache.PathHistory("acme/widgets", "main", path, version)
		if !ok || len(got.Authors) != 1 || got.Authors[0] != "old" {
			t.Fatalf("%s = %+v, %v; want the stored history left as it was", path, got, ok)
		}
	}
}

func TestRefreshGroupsDoNotSkipANearerRepo(t *testing.T) {
	t.Parallel()

	got := refreshGroups([]refreshPath{
		{repo: "acme/a", ref: "main", path: "near.go"},
		{repo: "acme/b", ref: "main", path: "mid.go"},
		{repo: "acme/a", ref: "main", path: "far.go"},
		{repo: "acme/a", ref: "main", path: "also-far.go"},
	})
	want := []historyGroup{
		{repo: "acme/a", base: "main", paths: []string{"near.go"}},
		{repo: "acme/b", base: "main", paths: []string{"mid.go"}},
		{repo: "acme/a", base: "main", paths: []string{"far.go", "also-far.go"}},
	}
	if len(got) != len(want) {
		t.Fatalf("groups = %+v, want %+v", got, want)
	}
	for i := range want {
		if got[i].repo != want[i].repo || got[i].base != want[i].base || !slices.Equal(got[i].paths, want[i].paths) {
			t.Fatalf("group %d = %+v, want %+v", i, got[i], want[i])
		}
	}
}

func TestRunTeamCoverSummaryNote(t *testing.T) {
	t.Parallel()

	g, doc, _ := historyRound(t, &mapHistoryCache{entries: map[string]gh.PathHistory{}}, "h", "base1")
	want := fmt.Sprintf("team-cover: paths 2/2 need history (0 hits); team members 0 hits/1 calls; files 0/2; reviews 0/2; refresh 0; calls %d (cap 40)", g.coverCalls)
	if !slices.Equal(doc.TeamCoverNotes, []string{want}) {
		t.Fatalf("notes = %q, want %q", doc.TeamCoverNotes, want)
	}
	for _, warning := range doc.Warnings {
		if strings.Contains(warning, "need history") {
			t.Fatalf("warnings = %q, want the summary only in the round output", doc.Warnings)
		}
	}
}
