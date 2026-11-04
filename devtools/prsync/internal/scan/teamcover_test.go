package scan

import (
	"context"
	"errors"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
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
	return gh.PRListItem{
		Number:      number,
		Title:       "Fix",
		State:       "OPEN",
		BaseRefName: "main",
		URL:         "https://github.com/acme/widgets/pull/1",
		ReviewRequests: []gh.ReviewReq{
			{Type: "User", Login: "someone"},
			{Type: "Team", Slug: "foo"},
		},
	}
}
