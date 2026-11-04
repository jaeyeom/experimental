package gh

import (
	"context"
	"encoding/base64"
	"errors"
	"strings"
	"testing"
	"time"
)

func TestReviewSnapshot(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "pr", "view", "5", "--repo", "acme/widgets", "--json", "author,latestReviews").
		WillSucceed(`{"author":{"login":"alice"},"latestReviews":[{"author":{"login":"bob"},"state":"APPROVED"}]}`, 0).Build()
	author, reviews, err := NewClient(mock, testGHBin).ReviewSnapshot(context.Background(), "acme/widgets", 5)
	if err != nil {
		t.Fatal(err)
	}
	if author != "alice" || len(reviews) != 1 || reviews[0].Author.Login != "bob" {
		t.Fatalf("author=%q reviews=%+v", author, reviews)
	}
}

func TestTeamMembers(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "--paginate", "orgs/acme/teams/foo/members").
		WillSucceed(`[{"login":"carol","type":"User"}]`, 0).Build()
	got, err := NewClient(mock, testGHBin).TeamMembers(context.Background(), "acme", "foo")
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].Login != "carol" || got[0].Type != "User" {
		t.Fatalf("members = %+v", got)
	}
}

func TestTeamMembersForbidden(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "--paginate", "orgs/acme/teams/foo/members").
		WillFail("HTTP 403: Resource not accessible by integration", 1).Build()
	_, err := NewClient(mock, testGHBin).TeamMembers(context.Background(), "acme", "foo")
	if err == nil || !strings.Contains(err.Error(), "Resource not accessible") {
		t.Fatalf("err = %v", err)
	}
}

func TestChangedFiles(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "--paginate", "repos/acme/widgets/pulls/5/files", "--jq", ".[].filename").
		WillSucceed("src/a.go\n\nsrc/b.go\n", 0).Build()
	got, err := NewClient(mock, testGHBin).ChangedFiles(context.Background(), "acme/widgets", 5)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 2 || got[0] != "src/a.go" || got[1] != "src/b.go" {
		t.Fatalf("files = %#v", got)
	}
}

func TestCodeownersSkipsMissing(t *testing.T) {
	t.Parallel()

	body := base64.StdEncoding.EncodeToString([]byte("* @acme/everyone\n"))
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "repos/acme/widgets/contents/.github/CODEOWNERS?ref=main").
		WillFail("HTTP 404: Not Found", 1).Build()
	mock.ExpectCommandWithArgs(testGHBin, "api", "repos/acme/widgets/contents/CODEOWNERS?ref=main").
		WillSucceed(`{"encoding":"base64","content":"`+body+`"}`, 0).Build()
	got, err := NewClient(mock, testGHBin).Codeowners(context.Background(), "acme/widgets", "main")
	if err != nil {
		t.Fatal(err)
	}
	if got != "* @acme/everyone\n" {
		t.Fatalf("codeowners = %q", got)
	}
}

func TestDirectReviewCountRateLimit(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "search/issues",
		"-f", "q=user-review-requested:carol org:acme is:pr is:open", "-f", "per_page=1").
		WillFail("HTTP 403: API rate limit exceeded", 1).Once().Build()
	_, err := NewClient(mock, testGHBin).DirectReviewCount(context.Background(), "acme", "carol")
	if !errors.Is(err, ErrRateLimited) {
		t.Fatalf("err = %v, want rate limit", err)
	}
}

func TestDirectReviewCount(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "search/issues",
		"-f", "q=user-review-requested:carol org:acme is:pr is:open", "-f", "per_page=1").
		WillSucceed(`{"total_count":3,"items":[]}`, 0).Build()
	got, err := NewClient(mock, testGHBin).DirectReviewCount(context.Background(), "acme", "carol")
	if err != nil || got != 3 {
		t.Fatalf("count=%d err=%v", got, err)
	}
}

func TestPathHistory(t *testing.T) {
	t.Parallel()

	since := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "graphql",
		"-f", "query="+pathHistoryQuery,
		"-F", "owner=acme",
		"-F", "name=widgets",
		"-F", "expression=main",
		"-F", "path=src/a.go",
		"-F", "since="+since.Format(time.RFC3339),
	).WillSucceed(`{"data":{"repository":{"object":{"history":{"nodes":[{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"nodes":[{"reviews":{"nodes":[{"author":{"login":"bob"}}]}}]}}]}}}}}`, 0).Build()
	got, err := NewClient(mock, testGHBin).PathHistory(context.Background(), "acme/widgets", "main", "src/a.go", since)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Authors) != 1 || got.Authors[0] != "alice" || len(got.Reviewers) != 1 || got.Reviewers[0] != "bob" {
		t.Fatalf("history = %+v", got)
	}
}

func TestOrgActive(t *testing.T) {
	t.Parallel()

	since := time.Date(2026, 8, 1, 0, 0, 0, 0, time.UTC)
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "users/carol/events/orgs/acme?per_page=100").
		WillSucceed(`[{"created_at":"2026-07-01T00:00:00Z"},{"created_at":"2026-08-02T00:00:00Z"}]`, 0).Build()
	got, err := NewClient(mock, testGHBin).OrgActive(context.Background(), "acme", "carol", since)
	if err != nil || !got {
		t.Fatalf("active=%v err=%v", got, err)
	}
}
