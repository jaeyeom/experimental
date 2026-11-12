package scan

import (
	"bytes"
	"context"
	"errors"
	"log/slog"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

func TestSuggestTeamsExclusion(t *testing.T) {
	t.Parallel()

	members := []gh.TeamMember{
		{Login: "alice", Type: "User"},
		{Login: "dependabot[bot]", Type: "Bot"},
		{Login: "left", Type: "User"},
		{Login: "approver", Type: "User"},
		{Login: "carol", Type: "User"},
	}
	got := suggestTeams(coverFacts{
		author:  "alice",
		exclude: []string{"left"},
		reviews: []gh.LatestReview{{Author: gh.ReviewAuthor{Login: "approver"}, State: "APPROVED"}},
		members: map[string][]gh.TeamMember{"acme/foo": members},
		perTeam: 1,
		teams:   []string{"acme/foo"},
	})
	if len(got) != 1 || got[0].CoveredBy == nil || *got[0].CoveredBy != "approver" || len(got[0].Suggest) != 0 {
		t.Fatalf("suggestion = %+v, want covered by approver", got)
	}

	open := []gh.TeamMember{
		{Login: "alice", Type: "User"},
		{Login: "dependabot[bot]", Type: "Bot"},
		{Login: "left", Type: "User"},
		{Login: "carol", Type: "User"},
	}
	uncovered := suggestTeams(coverFacts{
		author:  "alice",
		exclude: []string{"left"},
		reviews: []gh.LatestReview{{Author: gh.ReviewAuthor{Login: "outsider"}, State: "APPROVED"}},
		members: map[string][]gh.TeamMember{"acme/foo": open},
		perTeam: 1,
		teams:   []string{"acme/foo"},
	})
	if len(uncovered) != 1 || uncovered[0].CoveredBy != nil || uncovered[0].Error != "" {
		t.Fatalf("suggestion = %+v", uncovered)
	}
	if len(uncovered[0].Suggest) != 1 || uncovered[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggest = %+v, want carol", uncovered[0].Suggest)
	}
}

func TestSuggestTeamsCoveredByRequestOrReview(t *testing.T) {
	t.Parallel()

	members := []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}
	tests := []struct {
		name      string
		requested []string
		reviews   []gh.LatestReview
		want      string
	}{
		{
			name:      "individual request",
			requested: []string{"carol"},
			want:      "carol",
		},
		{
			name:    "submitted review",
			reviews: []gh.LatestReview{{Author: gh.ReviewAuthor{Login: "bob"}, State: "COMMENTED"}},
			want:    "bob",
		},
		{
			name:      "request wins over review and is login-ordered",
			requested: []string{"carol"},
			reviews:   []gh.LatestReview{{Author: gh.ReviewAuthor{Login: "bob"}, State: "COMMENTED"}},
			want:      "carol",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := suggestTeams(coverFacts{
				author:    "alice",
				requested: tc.requested,
				reviews:   tc.reviews,
				members:   map[string][]gh.TeamMember{"acme/foo": members},
				perTeam:   1,
				teams:     []string{"acme/foo"},
			})
			if len(got) != 1 || got[0].CoveredBy == nil || *got[0].CoveredBy != tc.want || len(got[0].Suggest) != 0 {
				t.Fatalf("suggestion = %+v, want covered by %s", got, tc.want)
			}
		})
	}
}

func TestSuggestTeamsRankingAndTiebreak(t *testing.T) {
	t.Parallel()

	rules := parseCODEOWNERS("* @acme/everyone\n/src/foo/** @acme/foo-team\n/src/foo/special.go @carol\n")
	members := map[string][]gh.TeamMember{
		"acme/foo": {
			{Login: "carol", Type: "User"},
			{Login: "bob", Type: "User"},
			{Login: "dave", Type: "User"},
		},
		"acme/foo-team": {{Login: "bob", Type: "User"}, {Login: "dave", Type: "User"}},
		"acme/everyone": {{Login: "carol", Type: "User"}, {Login: "bob", Type: "User"}, {Login: "dave", Type: "User"}},
	}
	// Only special.go is touched, so carol is the sole codeowner. Dave's
	// history and lighter load would win if ownership did not.
	files := []string{"src/foo/special.go"}
	history := map[string]gh.PathHistory{
		"src/foo/special.go": {Authors: []string{"dave", "dave"}, Reviewers: []string{"bob"}},
	}
	got := suggestTeams(coverFacts{
		author:  "alice",
		files:   files,
		rules:   rules,
		members: members,
		history: history,
		load:    map[string]int{"carol": 5, "bob": 1, "dave": 0},
		loadOn:  true,
		signals: []string{"codeowners", "history", "load"},
		perTeam: 1,
		teams:   []string{"acme/foo"},
	})
	if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggest = %+v, want carol by codeowner over history and load", got)
	}
	reasons := got[0].Suggest[0].Reasons
	if len(reasons) < 2 || reasons[0] != "codeowner: /src/foo/special.go" || reasons[len(reasons)-1] != "5 open direct requests" {
		t.Fatalf("reasons = %#v", reasons)
	}

	tied := suggestTeams(coverFacts{
		author:  "alice",
		members: map[string][]gh.TeamMember{"acme/foo": members["acme/foo"]},
		perTeam: 2,
		teams:   []string{"acme/foo"},
	})
	if len(tied) != 1 || len(tied[0].Suggest) != 2 || tied[0].Suggest[0].Login != "bob" || tied[0].Suggest[1].Login != "carol" {
		t.Fatalf("tiebreak = %+v, want bob then carol", tied)
	}
}

func TestSuggestTeamsSharesSmallestTeam(t *testing.T) {
	t.Parallel()

	got := suggestTeams(coverFacts{
		author: "alice",
		members: map[string][]gh.TeamMember{
			"acme/small": {{Login: "carol", Type: "User"}, {Login: "dave", Type: "User"}},
			"acme/big": {
				{Login: "carol", Type: "User"},
				{Login: "bob", Type: "User"},
				{Login: "dave", Type: "User"},
				{Login: "eve", Type: "User"},
			},
		},
		perTeam: 1,
		teams:   []string{"acme/big", "acme/small"},
	})
	if len(got) != 2 {
		t.Fatalf("len = %d", len(got))
	}
	if got[0].Team != "acme/big" || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("big = %+v, want carol reused", got[0])
	}
	if got[1].Team != "acme/small" || len(got[1].Suggest) != 1 || got[1].Suggest[0].Login != "carol" {
		t.Fatalf("small = %+v, want carol", got[1])
	}
}

func TestSuggestTeamsMemberErrorAndInactive(t *testing.T) {
	t.Parallel()

	got := suggestTeams(coverFacts{
		author: "alice",
		members: map[string][]gh.TeamMember{
			"acme/live": {{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}},
		},
		memberErr: map[string]string{"acme/missing": "cannot list members of acme/missing: HTTP 403"},
		inactive:  map[string]bool{"bob": true},
		perTeam:   1,
		teams:     []string{"acme/missing", "acme/live"},
	})
	if got[0].Error == "" || len(got[0].Suggest) != 0 {
		t.Fatalf("missing = %+v", got[0])
	}
	if len(got[1].Suggest) != 1 || got[1].Suggest[0].Login != "carol" {
		t.Fatalf("live = %+v, want carol because bob is inactive", got[1])
	}
}

func TestTeamRequestsSkipsTeams(t *testing.T) {
	t.Parallel()

	teams, users := teamRequests([]string{"User:bob", "Team:skip-me", "Team:keep", "Team:keep"}, "acme", []string{"acme/skip-me"})
	if len(teams) != 1 || teams[0] != "acme/keep" {
		t.Fatalf("teams = %#v", teams)
	}
	if len(users) != 1 || users[0] != "bob" {
		t.Fatalf("users = %#v", users)
	}
}

func TestTeamRequestsQualifiedSlug(t *testing.T) {
	t.Parallel()

	teams, users := teamRequests(
		[]string{"Team:acme/backend-reviewers", "Team:backend-reviewers", "User:bob"},
		"acme",
		nil,
	)
	if len(teams) != 1 || teams[0] != "acme/backend-reviewers" {
		t.Fatalf("teams = %#v, want one acme/backend-reviewers", teams)
	}
	if len(users) != 1 || users[0] != "bob" {
		t.Fatalf("users = %#v", users)
	}

	other, _ := teamRequests([]string{"Team:other/backend-reviewers"}, "acme", nil)
	if len(other) != 1 || other[0] != "other/backend-reviewers" {
		t.Fatalf("other org = %#v", other)
	}

	skipped, _ := teamRequests(
		[]string{"Team:backend-reviewers", "Team:acme/backend-reviewers"},
		"acme",
		[]string{"acme/backend-reviewers"},
	)
	if len(skipped) != 0 {
		t.Fatalf("skipped = %#v, want neither form", skipped)
	}
}

func TestRunTeamCoverQualifiedAndBareSlug(t *testing.T) {
	t.Parallel()

	members := []gh.TeamMember{
		{Login: "alice", Type: "User"},
		{Login: "carol", Type: "User"},
	}
	tests := []struct {
		name string
		slug string
	}{
		{name: "org slash slug", slug: "acme/backend-reviewers"},
		{name: "bare slug", slug: "backend-reviewers"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			g := teamCoverGH(map[string]memberCall{
				"acme/backend-reviewers": {members: members},
			})
			g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPRSlug(1, tc.slug)}}
			doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), teamCoverCfg(), []string{"acme/widgets"}, fixtureNow)
			if err != nil {
				t.Fatal(err)
			}
			if g.memberCalls != 1 {
				t.Fatalf("memberCalls = %d, want 1", g.memberCalls)
			}
			got := doc.PRs[0].TeamReviewSuggestions
			if len(got) != 1 || got[0].Team != "acme/backend-reviewers" || got[0].Error != "" {
				t.Fatalf("suggestion = %+v", got)
			}
			if len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
				t.Fatalf("suggest = %+v, want carol", got[0].Suggest)
			}
		})
	}
}

func TestRunTeamCoverDedupesQualifiedAndBareSlug(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/backend-reviewers": {members: []gh.TeamMember{
			{Login: "alice", Type: "User"},
			{Login: "carol", Type: "User"},
		}},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPRSlug(1, "backend-reviewers", "acme/backend-reviewers")}}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), teamCoverCfg(), []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.memberCalls != 1 {
		t.Fatalf("memberCalls = %d, want 1", g.memberCalls)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || got[0].Team != "acme/backend-reviewers" || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggestion = %+v", got)
	}
}

func TestRunTeamCoverSkipQualifiedSlugSkipsBothForms(t *testing.T) {
	t.Parallel()

	for _, slug := range []string{"acme/backend-reviewers", "backend-reviewers"} {
		t.Run(slug, func(t *testing.T) {
			t.Parallel()
			g := teamCoverGH(map[string]memberCall{
				"acme/backend-reviewers": {members: []gh.TeamMember{{Login: "carol", Type: "User"}}},
			})
			g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPRSlug(1, slug)}}
			cfg := teamCoverCfg()
			cfg.TeamCoverSkipTeams = []string{"acme/backend-reviewers"}
			doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
			if err != nil {
				t.Fatal(err)
			}
			if g.memberCalls != 0 {
				t.Fatalf("memberCalls = %d, want 0", g.memberCalls)
			}
			if doc.PRs[0].TeamReviewSuggestions != nil {
				t.Fatalf("suggestions = %+v, want none", doc.PRs[0].TeamReviewSuggestions)
			}
		})
	}
}

func TestRunTeamCoverEveryLookupFailedWarnsOnce(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/backend-reviewers": {err: errors.New("HTTP 404")},
		"acme/other":             {err: errors.New("HTTP 403")},
	})
	one := teamPRSlug(1, "acme/backend-reviewers", "other")
	two := teamPRSlug(2, "acme/backend-reviewers", "other")
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{one, two}}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), teamCoverCfg(), []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.memberCalls != 2 {
		t.Fatalf("memberCalls = %d, want 2", g.memberCalls)
	}
	for _, pr := range doc.PRs {
		if len(pr.TeamReviewSuggestions) != 2 {
			t.Fatalf("suggestions = %+v", pr.TeamReviewSuggestions)
		}
		for _, suggestion := range pr.TeamReviewSuggestions {
			if suggestion.Error == "" || len(suggestion.Suggest) != 0 {
				t.Fatalf("suggestion = %+v, want error and empty suggest", suggestion)
			}
		}
	}
	if got := countSubstr(doc.Warnings, "every team lookup failed"); got != 1 {
		t.Fatalf("warnings = %#v, want one every-lookup-failed warning", doc.Warnings)
	}
}

func TestRunTeamCoverMixedLookupDoesNotWarnEveryFailed(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/backend-reviewers": {members: []gh.TeamMember{
			{Login: "alice", Type: "User"},
			{Login: "carol", Type: "User"},
		}},
		"acme/other": {err: errors.New("HTTP 404")},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPRSlug(1, "acme/backend-reviewers", "other")}}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), teamCoverCfg(), []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 2 {
		t.Fatalf("suggestions = %+v", got)
	}
	if got[0].Team != "acme/backend-reviewers" || got[0].Error != "" || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("live = %+v, want carol", got[0])
	}
	if got[1].Team != "acme/other" || got[1].Error == "" || len(got[1].Suggest) != 0 {
		t.Fatalf("failed = %+v, want error and empty suggest", got[1])
	}
	if countSubstr(doc.Warnings, "every team lookup failed") != 0 {
		t.Fatalf("warnings = %#v, mixed lookups must not say every lookup failed", doc.Warnings)
	}
}

func TestRunTeamCoverSnapshotFailureSkipsLookupWarning(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/backend-reviewers": {err: errors.New("HTTP 404")},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPRSlug(1, "backend-reviewers")}}
	g.snapshots = map[string]snapResult{
		"acme/widgets#1": {err: errors.New("HTTP 500")},
	}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), teamCoverCfg(), []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.memberCalls != 0 {
		t.Fatalf("memberCalls = %d, want 0", g.memberCalls)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || got[0].Error == "" || len(got[0].Suggest) != 0 {
		t.Fatalf("suggestion = %+v, want snapshot error and empty suggest", got)
	}
	if countSubstr(doc.Warnings, "every team lookup failed") != 0 {
		t.Fatalf("warnings = %#v, snapshot failure is not a team lookup", doc.Warnings)
	}
}

func TestRunTeamCoverOffDoesNotFetch(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(nil)
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), config.Defaults(), []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.memberCalls != 0 || g.loadCalls != 0 {
		t.Fatalf("memberCalls=%d loadCalls=%d, want 0", g.memberCalls, g.loadCalls)
	}
	if len(doc.PRs) != 1 || doc.PRs[0].TeamReviewSuggestions != nil || doc.Summary.UncoveredTeams != nil {
		t.Fatalf("prs=%+v summary=%+v", doc.PRs, doc.Summary)
	}
}

func TestRunTeamCoverSuggestsAndCachesMembers(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{
			{Login: "alice", Type: "User"},
			{Login: "carol", Type: "User"},
			{Login: "bob", Type: "User"},
		}},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPR(1), teamPR(2)}}
	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.TeamCover = true
	cfg.TeamCoverSignals = nil
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.memberCalls != 1 {
		t.Fatalf("memberCalls = %d, want 1", g.memberCalls)
	}
	if doc.Summary.UncoveredTeams == nil || *doc.Summary.UncoveredTeams != 2 {
		t.Fatalf("uncovered = %v", doc.Summary.UncoveredTeams)
	}
	for _, pr := range doc.PRs {
		if len(pr.TeamReviewSuggestions) != 1 || pr.TeamReviewSuggestions[0].Suggest[0].Login != "bob" {
			t.Fatalf("pr %+v", pr.TeamReviewSuggestions)
		}
	}
}

func TestRunTeamCoverPermissionAndRateLimit(t *testing.T) {
	t.Parallel()

	t.Run("missing read org", func(t *testing.T) {
		t.Parallel()
		g := teamCoverGH(map[string]memberCall{
			"acme/foo": {err: errors.New("HTTP 403: Resource not accessible by integration")},
		})
		cfg := teamCoverCfg()
		doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
		if err != nil {
			t.Fatal(err)
		}
		got := doc.PRs[0].TeamReviewSuggestions
		if len(got) != 1 || got[0].Error == "" || len(got[0].Suggest) != 0 {
			t.Fatalf("suggestion = %+v", got)
		}
		if len(doc.Warnings) == 0 {
			t.Fatal("expected a warning")
		}
		if doc.Summary.UncoveredTeams == nil || *doc.Summary.UncoveredTeams != 1 {
			t.Fatalf("uncovered = %v", doc.Summary.UncoveredTeams)
		}
	})

	t.Run("rate limit skips load", func(t *testing.T) {
		t.Parallel()
		g := teamCoverGH(map[string]memberCall{
			"acme/foo": {members: []gh.TeamMember{{Login: "carol", Type: "User"}, {Login: "bob", Type: "User"}}},
		})
		g.loadErr = gh.ErrRateLimited
		cfg := teamCoverCfg()
		cfg.TeamCoverSignals = []string{"load"}
		doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
		if err != nil {
			t.Fatal(err)
		}
		got := doc.PRs[0].TeamReviewSuggestions
		if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "bob" {
			t.Fatalf("suggestion = %+v, want bob by login after load is skipped", got)
		}
		if len(got[0].Suggest[0].Reasons) != 0 {
			t.Fatalf("reasons = %#v, want none when load is skipped", got[0].Suggest[0].Reasons)
		}
		if len(doc.Warnings) == 0 {
			t.Fatal("expected a rate-limit warning")
		}
	})
}

func teamCoverCfg() config.Config {
	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.TeamCover = true
	cfg.TeamCoverSignals = nil
	return cfg
}

func teamCoverGH(members map[string]memberCall) *scriptGH {
	return &scriptGH{
		list: map[string]listResult{
			"acme/widgets": {prs: []gh.PRListItem{teamPR(1)}},
		},
		members: members,
	}
}

func teamPR(number int) gh.PRListItem {
	return teamPRSlug(number, "foo")
}

func teamPRSlug(number int, slugs ...string) gh.PRListItem {
	reqs := make([]gh.ReviewReq, 0, 1+len(slugs))
	reqs = append(reqs, gh.ReviewReq{Type: "User", Login: "someone"})
	for _, slug := range slugs {
		reqs = append(reqs, gh.ReviewReq{Type: "Team", Slug: slug})
	}
	return gh.PRListItem{
		Number:         number,
		Title:          "Fix",
		State:          "OPEN",
		BaseRefName:    "main",
		URL:            "https://github.com/acme/widgets/pull/1",
		ReviewRequests: reqs,
	}
}

func countSubstr(warnings []string, needle string) int {
	n := 0
	for _, warning := range warnings {
		if strings.Contains(warning, needle) {
			n++
		}
	}
	return n
}

type mapHistoryCache struct {
	entries map[string]gh.PathHistory
	batch   int
}

func (m *mapHistoryCache) HistoryBatch() int { return m.batch }

func (m *mapHistoryCache) PutHistoryBatch(n int) { m.batch = n }

func (m *mapHistoryCache) PathHistory(repo, ref, path, head string) (gh.PathHistory, bool) {
	h, ok := m.entries[repo+"@"+ref+"\x00"+path+"\x00"+head]
	return h, ok
}

func (m *mapHistoryCache) PutPathHistory(repo, ref, path, head string, h gh.PathHistory) {
	m.entries[repo+"@"+ref+"\x00"+path+"\x00"+head] = h
}

type countRounds struct{ n map[string]int }

func (c *countRounds) NextRound(job string) int {
	c.n[job]++
	return c.n[job]
}

// historyRound runs one scan of two PRs on main that need cover for
// acme/foo. Both touch src/a.go; PR 2 also touches src/b.go. It runs with
// team_cover_history_ttl=0, so history is keyed by the base head.
func historyRound(t *testing.T, cache HistoryCache, prHead, baseHead string) (*scriptGH, Document, string) {
	t.Helper()
	return historyRoundAt(t, cache, prHead, baseHead, fixtureNow, 0)
}

func historyRoundAt(t *testing.T, cache HistoryCache, prHead, baseHead string, now time.Time, ttl time.Duration) (*scriptGH, Document, string) {
	t.Helper()
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	one, two := teamPR(1), teamPR(2)
	one.HeadRefOid, two.HeadRefOid = prHead+"-1", prHead+"-2"
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{one, two}}
	g.files = map[string][]string{
		"acme/widgets#1": {"src/a.go"},
		"acme/widgets#2": {"src/a.go", "src/b.go"},
	}
	g.history = map[string]gh.PathHistory{"acme/widgets\x00src/a.go": {Authors: []string{"carol"}}}
	g.heads = map[string]string{"acme/widgets@main": baseHead}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	cfg.TeamCoverHistoryTTL = ttl
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	var logs bytes.Buffer
	ctx := runlog.WithLogger(context.Background(), slog.New(slog.NewJSONHandler(&logs, nil)))
	doc, err := Run(ctx, deps, cfg, []string{"acme/widgets"}, now)
	if err != nil {
		t.Fatal(err)
	}
	return g, doc, logs.String()
}

func TestRunTeamCoverBatchesPathsAcrossPRs(t *testing.T) {
	t.Parallel()

	g, doc, _ := historyRound(t, &mapHistoryCache{entries: map[string]gh.PathHistory{}}, "h", "base1")
	if g.historyCalls != 1 {
		t.Fatalf("historyCalls = %d, want one batched call for both PRs", g.historyCalls)
	}
	if !slices.Equal(g.historyPaths[0], []string{"src/a.go", "src/b.go"}) {
		t.Fatalf("paths = %#v, want each path once", g.historyPaths)
	}
	if g.historyRefs[0] != "base1" {
		t.Fatalf("ref = %q, want the base head the cache is keyed by", g.historyRefs[0])
	}
	for _, pr := range doc.PRs {
		got := pr.TeamReviewSuggestions
		if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
			t.Fatalf("pr %d suggestions = %+v, want carol from history", pr.Number, got)
		}
	}
}

func TestRunTeamCoverReusesHistoryWhileBaseHeadIsUnchanged(t *testing.T) {
	t.Parallel()

	cache := &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	first, _, firstLogs := historyRound(t, cache, "h1", "base1")
	if first.historyCalls != 1 {
		t.Fatalf("first round historyCalls = %d, want 1", first.historyCalls)
	}
	if !strings.Contains(firstLogs, `"msg":"team-cover cache","hits":0,"misses":2`) {
		t.Fatalf("logs = %s, want team-cover cache hits 0 misses 2", firstLogs)
	}

	// The PR heads moved but the base head did not.
	second, doc, logs := historyRound(t, cache, "h2", "base1")
	if second.historyCalls != 0 {
		t.Fatalf("unchanged base head: historyCalls = %d, want 0", second.historyCalls)
	}
	if second.headCalls != 1 {
		t.Fatalf("headCalls = %d, want one per repo and base", second.headCalls)
	}
	if !strings.Contains(logs, `"msg":"team-cover cache","hits":2,"misses":0`) {
		t.Fatalf("logs = %s, want team-cover cache hits 2 misses 0", logs)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggestions = %+v, want carol from the cached history", got)
	}

	third, _, _ := historyRound(t, cache, "h2", "base2")
	if third.historyCalls != 1 {
		t.Fatalf("moved base head: historyCalls = %d, want 1", third.historyCalls)
	}
}

func TestRunTeamCoverHistoryCachedByTimeAcrossBaseHeads(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), func() time.Time { return clock })
	if err != nil {
		t.Fatal(err)
	}
	cache.SetHistoryTTL(6 * time.Hour)

	first, _, _ := historyRoundAt(t, cache, "h1", "base1", clock, 6*time.Hour)
	if first.historyCalls != 1 || first.historyRefs[0] != "main" {
		t.Fatalf("first round history calls = %d refs = %#v, want one call on the branch", first.historyCalls, first.historyRefs)
	}
	if first.headCalls != 0 {
		t.Fatalf("headCalls = %d, want no base head lookup with a TTL", first.headCalls)
	}

	clock = fixtureNow.Add(time.Hour)
	second, doc, logs := historyRoundAt(t, cache, "h2", "base2", clock, 6*time.Hour)
	if second.historyCalls != 0 || second.headCalls != 0 {
		t.Fatalf("moved base head within the TTL: history=%d heads=%d, want none", second.historyCalls, second.headCalls)
	}
	if !strings.Contains(logs, `"msg":"team-cover cache","hits":2,"misses":0`) {
		t.Fatalf("logs = %s, want team-cover cache hits 2 misses 0", logs)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggestions = %+v, want carol from the cached history", got)
	}

	clock = fixtureNow.Add(7 * time.Hour)
	third, _, _ := historyRoundAt(t, cache, "h3", "base3", clock, 6*time.Hour)
	if third.historyCalls != 1 || !slices.Equal(third.historyPaths[0], []string{"src/a.go", "src/b.go"}) {
		t.Fatalf("past the TTL: history calls = %d paths = %#v, want both paths again", third.historyCalls, third.historyPaths)
	}
}

func TestRunTeamCoverFetchesHistoryOnlyForPRsWithCandidates(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
		"acme/bar": {members: []gh.TeamMember{{Login: "dave", Type: "User"}}},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPR(1), teamPRSlug(2, "bar")}}
	g.files = map[string][]string{
		"acme/widgets#1": {"src/a.go"},
		"acme/widgets#2": {"src/b.go"},
	}
	g.history = map[string]gh.PathHistory{"acme/widgets\x00src/a.go": {Authors: []string{"carol"}}}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"codeowners", "history"}
	// acme/bar is uncovered on PR 2, but its only member is excluded.
	cfg.TeamCoverExcludeLogins = []string{"dave"}
	deps := depsWith(g, fixtureHerdr{})
	deps.History = &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	var logs bytes.Buffer
	ctx := runlog.WithLogger(context.Background(), slog.New(slog.NewJSONHandler(&logs, nil)))
	if _, err := Run(ctx, deps, cfg, []string{"acme/widgets"}, fixtureNow); err != nil {
		t.Fatal(err)
	}
	if g.historyCalls != 1 || !slices.Equal(g.historyPaths[0], []string{"src/a.go"}) {
		t.Fatalf("history calls = %d paths = %#v, want only PR 1's path", g.historyCalls, g.historyPaths)
	}
	if !strings.Contains(logs.String(), `"msg":"team-cover: 1 of 2 paths need history"`) {
		t.Fatalf("logs = %s, want the paths-needing-history count", logs.String())
	}
}

// batchRound runs one history round with team_cover_batch_paths=20.
func batchRound(t *testing.T, cache *mapHistoryCache, worked int, skip map[string]bool) *scriptGH {
	t.Helper()
	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.files = map[string][]string{"acme/widgets#1": {"src/a.go", "src/b.go"}}
	g.history = map[string]gh.PathHistory{"acme/widgets\x00src/a.go": {Authors: []string{"carol"}}}
	g.historyWorked = worked
	g.historySkip = skip
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	cfg.TeamCoverBatchPaths = 20
	cfg.TeamCoverHistoryTTL = 0
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	if _, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow); err != nil {
		t.Fatal(err)
	}
	return g
}

func TestRunTeamCoverHistoryBatchStartsAtCachedSize(t *testing.T) {
	t.Parallel()

	cache := &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	first := batchRound(t, cache, 5, nil)
	if !slices.Equal(first.historyBatches, []int{20}) {
		t.Fatalf("first round batches = %v, want the configured 20", first.historyBatches)
	}
	if cache.batch != 5 {
		t.Fatalf("cached batch = %d, want 5, the size that worked", cache.batch)
	}

	cache.entries = map[string]gh.PathHistory{}
	second := batchRound(t, cache, 0, nil)
	if !slices.Equal(second.historyBatches, []int{5}) {
		t.Fatalf("next round batches = %v, want the cached 5", second.historyBatches)
	}
}

func TestRunTeamCoverSkippedPathIsNotStored(t *testing.T) {
	t.Parallel()

	cache := &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	batchRound(t, cache, 0, map[string]bool{"src/b.go": true})
	if _, ok := cache.PathHistory("acme/widgets", "main", "src/b.go", "base1"); ok {
		t.Fatalf("cache = %#v, want the skipped path left out", cache.entries)
	}
	if _, ok := cache.PathHistory("acme/widgets", "main", "src/a.go", "base1"); !ok {
		t.Fatalf("cache = %#v, want src/a.go stored", cache.entries)
	}
}

func TestRunTeamCoverBaseHeadFailureStillFetchesHistory(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.files = map[string][]string{"acme/widgets#1": {"src/a.go"}}
	g.history = map[string]gh.PathHistory{"acme/widgets\x00src/a.go": {Authors: []string{"carol"}}}
	g.headErr = errors.New("HTTP 500")
	cache := &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	cfg.TeamCoverHistoryTTL = 0
	deps := depsWith(g, fixtureHerdr{})
	deps.History = cache
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.historyCalls != 1 || g.historyRefs[0] != "main" {
		t.Fatalf("history calls = %d refs = %#v, want one call on the branch", g.historyCalls, g.historyRefs)
	}
	if len(cache.entries) != 0 {
		t.Fatalf("cache = %#v, want nothing stored without a base head", cache.entries)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || got[0].Suggest[0].Login != "carol" {
		t.Fatalf("suggestions = %+v", got)
	}
}

// A load signal that fails with something other than a rate limit (the
// search POST that returned 404) must not stop cover: history and
// CODEOWNERS still pick reviewers, and the failure is a warning.
func TestRunTeamCoverLoadFailureKeepsOtherSignals(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{
			{Login: "bob", Type: "User"},
			{Login: "carol", Type: "User"},
			{Login: "dave", Type: "User"},
		}},
		"acme/foo-two": {members: []gh.TeamMember{
			{Login: "bob", Type: "User"},
			{Login: "erin", Type: "User"},
		}},
	})
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{teamPRSlug(1, "foo", "foo-two")}}
	g.files = map[string][]string{"acme/widgets#1": {"src/a.go", "src/b.go"}}
	g.codeowners = map[string]string{"acme/widgets@main": "/src/b.go @erin\n"}
	g.history = map[string]gh.PathHistory{"acme/widgets\x00src/a.go": {Authors: []string{"carol"}}}
	g.loadErr = errors.New("gh 404: Not Found (HTTP 404)")
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"codeowners", "history", "load"}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 2 {
		t.Fatalf("suggestions = %+v, want two teams", got)
	}
	if len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "carol" || !slices.Equal(got[0].Suggest[0].Reasons, []string{"authored 1 recent change"}) {
		t.Fatalf("acme/foo = %+v, want carol from history", got[0])
	}
	if len(got[1].Suggest) != 1 || got[1].Suggest[0].Login != "erin" || !slices.Equal(got[1].Suggest[0].Reasons, []string{"codeowner: /src/b.go"}) {
		t.Fatalf("acme/foo-two = %+v, want erin from CODEOWNERS", got[1])
	}
	if countSubstr(doc.Warnings, "skipping team_cover load signal: gh 404") != 1 {
		t.Fatalf("warnings = %#v, want the load signal warning once", doc.Warnings)
	}
}

func TestSuggestTeamsUncoveredReason(t *testing.T) {
	t.Parallel()

	got := suggestTeams(coverFacts{
		author: "alice",
		members: map[string][]gh.TeamMember{
			"acme/empty": {},
			"acme/gone": {
				{Login: "alice", Type: "User"},
				{Login: "dependabot[bot]", Type: "Bot"},
				{Login: "left", Type: "User"},
				{Login: "idle", Type: "User"},
				{Login: "idle2", Type: "User"},
			},
			"acme/live": {{Login: "carol", Type: "User"}},
		},
		memberErr: map[string]string{"acme/missing": "cannot list members of acme/missing: HTTP 403"},
		exclude:   []string{"left"},
		inactive:  map[string]bool{"idle": true, "idle2": true},
		perTeam:   1,
		teams:     []string{"acme/empty", "acme/gone", "acme/live", "acme/missing"},
	})
	if len(got) != 4 {
		t.Fatalf("got = %+v", got)
	}
	want := []string{
		"no members",
		"all 5 members excluded (1 author, 1 bot, 1 excluded login, 2 inactive)",
		"",
		"",
	}
	for i, item := range got {
		if item.UncoveredReason != want[i] {
			t.Errorf("%s reason = %q, want %q", item.Team, item.UncoveredReason, want[i])
		}
	}
	if got[0].CoveredBy != nil || got[1].CoveredBy != nil {
		t.Fatalf("got = %+v, want both uncovered", got)
	}
}

func TestRunTeamCoverHistoryRateLimitDropsSignal(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "carol", Type: "User"}, {Login: "bob", Type: "User"}}},
	})
	g.files = map[string][]string{"acme/widgets#1": {"src/a.go"}}
	g.historyErr = gh.ErrRateLimited
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"history"}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || len(got[0].Suggest) != 1 || got[0].Suggest[0].Login != "bob" || len(got[0].Suggest[0].Reasons) != 0 {
		t.Fatalf("suggestions = %+v, want bob by login with no history reason", got)
	}
	if countSubstr(doc.Warnings, "skipping team_cover history signal") != 1 {
		t.Fatalf("warnings = %#v", doc.Warnings)
	}
}

func TestRunTeamCoverCoveredPRQueriesNoPaths(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{
		"acme/foo": {members: []gh.TeamMember{{Login: "someone", Type: "User"}, {Login: "carol", Type: "User"}}},
	})
	g.files = map[string][]string{"acme/widgets#1": {"src/a.go"}}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"codeowners", "history", "load"}
	deps := depsWith(g, fixtureHerdr{})
	deps.History = &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.fileCalls != 0 || g.headCalls != 0 || g.historyCalls != 0 {
		t.Fatalf("files=%d heads=%d history=%d, want none for a covered PR", g.fileCalls, g.headCalls, g.historyCalls)
	}
	got := doc.PRs[0].TeamReviewSuggestions
	if len(got) != 1 || got[0].CoveredBy == nil || *got[0].CoveredBy != "someone" {
		t.Fatalf("suggestions = %+v, want covered by someone", got)
	}
}

func TestRunTeamCoverNoPRNeedsCoverMakesNoCalls(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(nil)
	pr := teamPR(1)
	pr.ReviewRequests = []gh.ReviewReq{{Type: "User", Login: "someone"}}
	g.list["acme/widgets"] = listResult{prs: []gh.PRListItem{pr}}
	cfg := teamCoverCfg()
	cfg.TeamCoverSignals = []string{"codeowners", "history", "load"}
	deps := depsWith(g, fixtureHerdr{})
	deps.History = &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	rounds := &countRounds{n: map[string]int{}}
	deps.Rounds = rounds
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.coverCalls != 0 {
		t.Fatalf("coverCalls = %d, want no team-cover GitHub calls", g.coverCalls)
	}
	if doc.PRs[0].TeamReviewSuggestions != nil {
		t.Fatalf("suggestions = %+v, want none", doc.PRs[0].TeamReviewSuggestions)
	}
}

func TestRunTeamCoverEveryNthRound(t *testing.T) {
	t.Parallel()

	rounds := &countRounds{n: map[string]int{}}
	cfg := teamCoverCfg()
	cfg.TeamCoverEvery = 2
	var ran []bool
	for range 3 {
		g := teamCoverGH(map[string]memberCall{
			"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}}},
		})
		deps := depsWith(g, fixtureHerdr{})
		deps.Rounds = rounds
		doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
		if err != nil {
			t.Fatal(err)
		}
		if (g.coverCalls == 0) != (doc.PRs[0].TeamReviewSuggestions == nil) {
			t.Fatalf("calls=%d suggestions=%+v disagree", g.coverCalls, doc.PRs[0].TeamReviewSuggestions)
		}
		ran = append(ran, g.coverCalls > 0)
	}
	if !slices.Equal(ran, []bool{true, false, true}) {
		t.Fatalf("ran = %v, want every 2nd round starting with the first", ran)
	}

	g := teamCoverGH(map[string]memberCall{"acme/foo": {members: []gh.TeamMember{{Login: "bob", Type: "User"}}}})
	if _, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow); err != nil {
		t.Fatal(err)
	}
	if g.coverCalls == 0 {
		t.Fatal("without a round counter the job must run every round")
	}
}
