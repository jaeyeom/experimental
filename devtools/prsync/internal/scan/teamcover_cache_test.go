package scan

import (
	"context"
	"path/filepath"
	"slices"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
)

// lookupRound is one team-cover round over two PRs on main that request
// acme/foo and rank by CODEOWNERS, whose rules name acme/bar. heads holds
// each PR's head SHA; updatedAt is the same for both.
type lookupRound struct {
	cache     HistoryCache
	heads     [2]string
	maxCalls  int
	codeowner string
}

func (r lookupRound) run(t *testing.T, now time.Time) (*scriptGH, Document) {
	t.Helper()
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
		"acme/bar": {members: []gh.TeamMember{{Login: "carol", Type: "User"}}},
	})
	one, two := teamPR(1), teamPR(2)
	one.HeadRefOid, two.HeadRefOid = r.heads[0], r.heads[1]
	one.UpdatedAt, two.UpdatedAt = "2026-11-01T11:00:00Z", "2026-11-01T11:00:00Z"
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{one, two}}
	g.files = map[string][]string{
		"acme/widgets#1": {"src/a.go"},
		"acme/widgets#2": {"src/a.go", "src/b.go"},
	}
	g.codeowners = map[string]string{"acme/widgets@main": "/src/ @acme/bar\n"}
	if r.codeowner != "" {
		g.codeownersAt = map[string]string{"acme/widgets@main": r.codeowner}
	}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"codeowners"}
	if r.maxCalls > 0 {
		cfg.TeamCoverMaxCalls = r.maxCalls
	}
	deps := depsWith(g, fixtureHerdr{})
	deps.History = r.cache
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, now)
	if err != nil {
		t.Fatal(err)
	}
	for _, pr := range doc.PRs {
		got := pr.TeamReviewSuggestions
		if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
			t.Fatalf("pr %d suggestions = %+v, want carol from CODEOWNERS", pr.Number, got)
		}
	}
	return g, doc
}

func newRoundCache(t *testing.T, clock *time.Time) *roundcache.Cache {
	t.Helper()
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), func() time.Time { return *clock })
	if err != nil {
		t.Fatal(err)
	}
	cache.SetMembersTTL(24 * time.Hour)
	cache.SetCodeownersTTL(6 * time.Hour)
	return cache
}

func TestRunTeamCoverSecondRoundReusesLookups(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	round := lookupRound{cache: newRoundCache(t, &clock), heads: [2]string{"h1", "h2"}}
	first, doc := round.run(t, clock)
	if first.memberCalls != 2 || first.fileCalls != 2 || len(first.codeownersFirst) != 1 || len(first.snapshotKeys) != 2 {
		t.Fatalf("first round members=%d files=%d codeowners=%d reviews=%d, want 2, 2, 1, 2",
			first.memberCalls, first.fileCalls, len(first.codeownersFirst), len(first.snapshotKeys))
	}
	want := "team-cover: paths 0/2 need history (0 hits); team members 0 hits/2 calls; files 0/2; reviews 0/2; calls 7 (cap 40)"
	if !slices.Contains(doc.TeamCoverNotes, want) {
		t.Fatalf("notes = %q, want %q", doc.TeamCoverNotes, want)
	}

	clock = fixtureNow.Add(time.Hour)
	second, doc := round.run(t, clock)
	if second.memberCalls != 0 || second.fileCalls != 0 || len(second.codeownersFirst) != 0 || len(second.snapshotKeys) != 0 {
		t.Fatalf("second round members=%d files=%d codeowners=%d reviews=%d, want none within the TTLs",
			second.memberCalls, second.fileCalls, len(second.codeownersFirst), len(second.snapshotKeys))
	}
	want = "team-cover: paths 0/2 need history (0 hits); team members 2 hits/0 calls; files 2/0; reviews 2/0; calls 0 (cap 40)"
	if !slices.Contains(doc.TeamCoverNotes, want) {
		t.Fatalf("notes = %q, want %q", doc.TeamCoverNotes, want)
	}
}

func TestRunTeamCoverNewHeadRefetchesOnlyThatPR(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	lookupRound{cache: cache, heads: [2]string{"h1", "h2"}}.run(t, clock)

	clock = fixtureNow.Add(time.Minute)
	g, _ := lookupRound{cache: cache, heads: [2]string{"h1", "h2-new"}}.run(t, clock)
	if !slices.Equal(g.fileKeys, []string{"acme/widgets#2"}) {
		t.Fatalf("file calls = %q, want only the PR whose head moved", g.fileKeys)
	}
	if !slices.Equal(g.snapshotKeys, []string{"acme/widgets#2"}) {
		t.Fatalf("review calls = %q, want only the PR whose head moved", g.snapshotKeys)
	}
}

func TestRunTeamCoverCodeownersTriesKnownLocation(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	round := lookupRound{cache: cache, heads: [2]string{"h1", "h2"}, codeowner: "CODEOWNERS"}
	first, _ := round.run(t, clock)
	if !slices.Equal(first.codeownersFirst, []string{""}) {
		t.Fatalf("first round codeowners = %q, want one lookup in GitHub's order", first.codeownersFirst)
	}
	if err := cache.Save(); err != nil {
		t.Fatal(err)
	}

	// Past team_cover_codeowners_ttl the body is fetched again, at the
	// location found last time.
	clock = fixtureNow.Add(7 * time.Hour)
	second, _ := round.run(t, clock)
	if !slices.Equal(second.codeownersFirst, []string{"CODEOWNERS"}) {
		t.Fatalf("second round codeowners = %q, want the known location first", second.codeownersFirst)
	}
}

func TestRunTeamCoverCacheHitsDoNotUseTheCap(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	lookupRound{cache: cache, heads: [2]string{"h1", "h2"}}.run(t, clock)

	// Only PR 2's files and reviews miss; every other lookup is a hit.
	clock = fixtureNow.Add(time.Minute)
	g, doc := lookupRound{cache: cache, heads: [2]string{"h1", "h2-new"}, maxCalls: 2}.run(t, clock)
	if g.coverCalls != 2 {
		t.Fatalf("calls = %d, want 2", g.coverCalls)
	}
	want := "team-cover: paths 0/2 need history (0 hits); team members 2 hits/0 calls; files 1/1; reviews 1/1; calls 2 (cap 2)"
	if !slices.Equal(doc.TeamCoverNotes, []string{want}) {
		t.Fatalf("notes = %q, want only %q", doc.TeamCoverNotes, want)
	}
}

func TestRunTeamCoverCapDuringTeamMembersNamesTheStep(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}}},
		"acme/bar": {members: []gh.TeamMember{{Login: "carol", Type: "User"}}},
		"acme/baz": {members: []gh.TeamMember{{Login: "dave", Type: "User"}}},
		"acme/qux": {members: []gh.TeamMember{{Login: "erin", Type: "User"}}},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{
		teamPRSlug(1, "foo", "bar", "baz"),
		teamPRSlug(2, "baz", "qux"),
	}}
	cfg := teamCoverCfg()
	cfg.TeamCoverMaxCalls = 2
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	// One review snapshot and acme/foo's members use the cap.
	want := "team-cover: call cap 2 reached during team members; 3 teams deferred"
	if !slices.Contains(doc.TeamCoverNotes, want) {
		t.Fatalf("notes = %q, want %q", doc.TeamCoverNotes, want)
	}
	if countSubstr(doc.Warnings, want) != 1 {
		t.Fatalf("warnings = %q, want the cap line once", doc.Warnings)
	}
}
