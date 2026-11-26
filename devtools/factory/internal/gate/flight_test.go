package gate

import (
	"errors"
	"fmt"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestCountInFlightDedupes(t *testing.T) {
	pattern := func(source string) (string, error) {
		if source != "jira:acme" {
			return "", errors.New("unexpected source " + source)
		}
		return `\[{id}\]`, nil
	}
	claims := []store.Claim{
		{
			Source: "jira:acme", ID: "PRJ-1",
			PRs: []store.ClaimPR{{
				Repo: "acme/widgets", Number: 1, URL: "https://github.com/acme/widgets/pull/1",
			}},
		},
		{
			Source: "jira:acme", ID: "PRJ-2",
			PRs: []store.ClaimPR{{
				Repo: "acme/widgets", Number: 20, URL: "https://github.com/acme/widgets/pull/20",
				Head: "feature/PRJ-2",
			}},
		},
		{Source: "jira:acme", ID: "PRJ-3"},
	}
	prs := []gh.PR{
		{Repo: "acme/widgets", Number: 1, URL: "https://github.com/acme/widgets/pull/1", Title: "unrelated", Head: "other"},
		{Repo: "acme/widgets", Number: 8, URL: "https://github.com/acme/widgets/pull/8", Title: "nope", Head: "feature/PRJ-2"},
		{Repo: "acme/widgets", Number: 3, URL: "https://github.com/acme/widgets/pull/3", Title: "fix [PRJ-3] yes", Head: "random"},
		{Repo: "acme/widgets", Number: 4, URL: "https://github.com/acme/widgets/pull/4", Title: "elsewhere", Head: "wip"},
		{Repo: "acme/widgets", Number: 5, URL: "https://github.com/acme/widgets/pull/5", Title: "also", Head: "wip-2"},
	}
	got, err := countInFlight(claims, prs, pattern)
	if err != nil {
		t.Fatal(err)
	}
	want := Flight{InFlight: 5, Claims: 3, OpenPRs: 5, Overlap: 3}
	if got != want {
		t.Fatalf("count = %+v, want %+v", got, want)
	}

	claims = append(claims, store.Claim{Source: "jira:acme", ID: "PRJ-4"})
	got, err = countInFlight(claims, prs, pattern)
	if err != nil {
		t.Fatal(err)
	}
	want = Flight{InFlight: 6, Claims: 4, OpenPRs: 5, Overlap: 3}
	if got != want {
		t.Fatalf("unopened = %+v, want %+v", got, want)
	}
}

func TestCountInFlightCustomTitlePattern(t *testing.T) {
	claims := []store.Claim{{Source: "jira:acme", ID: "PRJ-9"}}
	prs := []gh.PR{{
		Repo: "acme/widgets", Number: 9, URL: "https://github.com/acme/widgets/pull/9",
		Title: "ID:PRJ-9 now", Head: "other",
	}}
	bracket, err := countInFlight(claims, prs, func(string) (string, error) { return `\[{id}\]`, nil })
	if err != nil {
		t.Fatal(err)
	}
	if bracket.Overlap != 0 || bracket.InFlight != 2 {
		t.Fatalf("default pattern = %+v", bracket)
	}
	custom, err := countInFlight(claims, prs, func(string) (string, error) { return `ID:{id}`, nil })
	if err != nil {
		t.Fatal(err)
	}
	if custom.Overlap != 1 || custom.InFlight != 1 {
		t.Fatalf("custom pattern = %+v", custom)
	}
}

func TestCountInFlightHeadContainsID(t *testing.T) {
	claims := []store.Claim{{Source: "jira:acme", ID: "PRJ-7"}}
	prs := []gh.PR{{
		Repo: "acme/widgets", Number: 7, URL: "https://github.com/acme/widgets/pull/7",
		Title: "no key", Head: "feature/PRJ-7-fix",
	}}
	got, err := countInFlight(claims, prs, func(string) (string, error) { return `\[{id}\]`, nil })
	if err != nil {
		t.Fatal(err)
	}
	if got.Overlap != 1 || got.InFlight != 1 {
		t.Fatalf("count = %+v", got)
	}
}

func TestCountInFlightURLBeatsTitle(t *testing.T) {
	claims := []store.Claim{{
		Source: "jira:acme", ID: "PRJ-1",
		PRs: []store.ClaimPR{{
			Repo: "acme/widgets", Number: 1, URL: "https://github.com/acme/widgets/pull/1",
		}},
	}}
	prs := []gh.PR{
		{Repo: "acme/widgets", Number: 1, URL: "https://github.com/acme/widgets/pull/1", Title: "other", Head: "a"},
		{Repo: "acme/widgets", Number: 2, URL: "https://github.com/acme/widgets/pull/2", Title: "fix [PRJ-1] too", Head: "b"},
	}
	got, err := countInFlight(claims, prs, func(string) (string, error) { return `\[{id}\]`, nil })
	if err != nil {
		t.Fatal(err)
	}
	want := Flight{InFlight: 2, Claims: 1, OpenPRs: 2, Overlap: 1}
	if got != want {
		t.Fatalf("count = %+v, want %+v", got, want)
	}
}

func TestCountInFlightGitHubEmptyPattern(t *testing.T) {
	cfg := &config.Config{
		Sources: []config.Source{{Kind: "github", Name: "widgets", PRTitlePattern: ""}},
	}
	unrelated := gh.PR{Repo: "acme/other", Number: 99, URL: "https://github.com/acme/other/pull/99", Title: "other", Head: "other"}

	t.Run("unrelated open PRs succeed", func(t *testing.T) {
		claims := []store.Claim{{Source: "github:widgets", ID: "acme/widgets#1"}}
		got, err := countInFlight(claims, []gh.PR{unrelated}, cfg.PRTitlePattern)
		if err != nil {
			t.Fatalf("countInFlight() error = %v", err)
		}
		want := Flight{InFlight: 2, Claims: 1, OpenPRs: 1, Overlap: 0}
		if got != want {
			t.Fatalf("count = %+v, want %+v", got, want)
		}
	})

	t.Run("dedupes via ClosingIssues and head substring", func(t *testing.T) {
		claims := []store.Claim{
			{Source: "github:widgets", ID: "acme/widgets#1"},
			{Source: "github:widgets", ID: "acme/widgets#2"},
		}
		prs := []gh.PR{
			{Repo: "acme/widgets", Number: 10, URL: "https://github.com/acme/widgets/pull/10", Title: "fix", Head: "topic-a", ClosingIssues: []string{"acme/widgets#1"}},
			{Repo: "acme/widgets", Number: 11, URL: "https://github.com/acme/widgets/pull/11", Title: "fix", Head: "wip/acme/widgets#2"},
			unrelated,
		}
		got, err := countInFlight(claims, prs, cfg.PRTitlePattern)
		if err != nil {
			t.Fatalf("countInFlight() error = %v", err)
		}
		want := Flight{InFlight: 3, Claims: 2, OpenPRs: 3, Overlap: 2}
		if got != want {
			t.Fatalf("count = %+v, want %+v", got, want)
		}
	})

	t.Run("explicit pr_title_pattern matches title", func(t *testing.T) {
		explicitCfg := &config.Config{
			Sources: []config.Source{{Kind: "github", Name: "widgets", PRTitlePattern: `\[{id}\]`}},
		}
		claims := []store.Claim{{Source: "github:widgets", ID: "acme/widgets#3"}}
		prs := []gh.PR{
			{Repo: "acme/widgets", Number: 12, URL: "https://github.com/acme/widgets/pull/12", Title: "fix [acme/widgets#3]", Head: "topic-b"},
		}
		got, err := countInFlight(claims, prs, explicitCfg.PRTitlePattern)
		if err != nil {
			t.Fatalf("countInFlight() error = %v", err)
		}
		want := Flight{InFlight: 1, Claims: 1, OpenPRs: 1, Overlap: 1}
		if got != want {
			t.Fatalf("count = %+v, want %+v", got, want)
		}
	})
}

func TestCountInFlightUnconfiguredSources(t *testing.T) {
	cfg := &config.Config{
		Sources: []config.Source{{Kind: "github", Name: "agentic-factory", PRTitlePattern: ""}},
	}
	claims := []store.Claim{
		{Source: "github:shared-githooks", ID: "jaeyeom/shared-githooks#9"},
		{Source: "jira:other", ID: "OTH-7"},
		{Source: "jira:missing-unopened", ID: "OTH-8"},
	}
	prs := []gh.PR{
		{Repo: "jaeyeom/shared-githooks", Number: 1, URL: "https://github.com/jaeyeom/shared-githooks/pull/1", Title: "hook fix", Head: "fix", ClosingIssues: []string{"jaeyeom/shared-githooks#9"}},
		{Repo: "jaeyeom/other", Number: 2, URL: "https://github.com/jaeyeom/other/pull/2", Title: "ship [OTH-7]", Head: "oth-7"},
		{Repo: "jaeyeom/agentic-factory", Number: 3, URL: "https://github.com/jaeyeom/agentic-factory/pull/3", Title: "unrelated", Head: "main"},
	}
	got, err := countInFlight(claims, prs, cfg.PRTitlePattern)
	if err != nil {
		t.Fatalf("countInFlight() error = %v", err)
	}
	want := Flight{InFlight: 4, Claims: 3, OpenPRs: 3, Overlap: 2}
	if got != want {
		t.Fatalf("count = %+v, want %+v", got, want)
	}

	// Non-not-found errors from pattern still fail.
	_, err = countInFlight(claims[:1], prs, func(s string) (string, error) {
		return "", fmt.Errorf("source %s: boom", s)
	})
	if err == nil {
		t.Fatal("countInFlight() error = nil, want error for non-ErrSourceNotFound")
	}
}
