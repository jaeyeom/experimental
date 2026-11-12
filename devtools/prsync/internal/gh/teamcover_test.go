package gh

import (
	"context"
	"encoding/base64"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
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
	mock.ExpectCommandWithArgs(testGHBin, "api", "search/issues", "--method", "GET",
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
	mock.ExpectCommandWithArgs(testGHBin, "api", "search/issues", "--method", "GET",
		"-f", "q=user-review-requested:carol org:acme is:pr is:open", "-f", "per_page=1").
		WillSucceed(`{"total_count":3,"items":[]}`, 0).Build()
	got, err := NewClient(mock, testGHBin).DirectReviewCount(context.Background(), "acme", "carol")
	if err != nil || got != 3 {
		t.Fatalf("count=%d err=%v", got, err)
	}
}

// GitHub's search endpoint is GET only. gh api with -f fields and no
// --method sends a POST, which search/issues answers with 404.
func TestDirectReviewCountSendsGET(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return isSearchIssues(cfg.Args) && !hasGETMethod(cfg.Args)
	}).WillFail("gh: Not Found (HTTP 404)", 1).Build()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return isSearchIssues(cfg.Args) && hasGETMethod(cfg.Args)
	}).WillSucceed(`{"total_count":4,"items":[]}`, 0).Build()
	got, err := NewClient(mock, testGHBin).DirectReviewCount(context.Background(), "acme", "carol")
	if err != nil || got != 4 {
		t.Fatalf("count=%d err=%v, want 4 from a GET", got, err)
	}
}

func isSearchIssues(args []string) bool {
	return len(args) > 1 && args[0] == "api" && args[1] == "search/issues"
}

func hasGETMethod(args []string) bool {
	for i := 0; i+1 < len(args); i++ {
		if (args[i] == "--method" || args[i] == "-X") && strings.EqualFold(args[i+1], "GET") {
			return true
		}
	}
	return false
}

func TestPathHistoriesBatchesPaths(t *testing.T) {
	t.Parallel()

	since := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	paths := make([]string, 25)
	for i := range paths {
		paths[i] = fmt.Sprintf("src/f%02d.go", i)
	}
	mock := newGHMock()
	mock.ExpectCustom(isHistoryRequest).
		WillSucceed(`{"data":{"repository":{"object":{"p0":{"nodes":[{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"nodes":[{"number":7}]}}]}}}}}`, 0).Build()
	mock.ExpectCustom(isReviewsRequest).
		WillSucceed(`{"data":{"repository":{"pr7":{"reviews":{"nodes":[{"author":{"login":"bob"}}]}}}}}`, 0).Build()
	result, err := NewClient(mock, testGHBin).PathHistories(context.Background(), "acme/widgets", "main", paths, since, 20, 20)
	if err != nil {
		t.Fatal(err)
	}
	got := result.Histories
	var history []string
	for _, call := range mock.Executions() {
		if isHistoryRequest(context.Background(), call) {
			history = append(history, strings.Join(call.Args, " "))
		}
	}
	if len(history) != 2 {
		t.Fatalf("history requests = %d, want 2 for 25 paths in batches of 20", len(history))
	}
	if !strings.Contains(history[0], `p19:history(first:30,path:"src/f19.go",since:$since)`) || strings.Contains(history[0], "p20:") {
		t.Fatalf("first request = %s, want paths 0-19", history[0])
	}
	if !strings.Contains(history[1], `p4:history(first:30,path:"src/f24.go",since:$since)`) || strings.Contains(history[1], "p5:") {
		t.Fatalf("second request = %s, want paths 20-24", history[1])
	}
	for _, want := range []string{"expression=main", "owner=acme", "name=widgets", "since=" + since.Format(time.RFC3339)} {
		if !strings.Contains(history[0], want) {
			t.Fatalf("request lacks %q: %s", want, history[0])
		}
	}
	for _, path := range []string{"src/f00.go", "src/f20.go"} {
		h := got[path]
		if len(h.Authors) != 1 || h.Authors[0] != "alice" || len(h.Reviewers) != 1 || h.Reviewers[0] != "bob" {
			t.Fatalf("%s history = %+v", path, h)
		}
	}
	if h, ok := got["src/f01.go"]; !ok || len(h.Authors) != 0 {
		t.Fatalf("f01 history = %+v, %v, want an empty entry", h, ok)
	}
}

func TestPathHistoriesFetchesSharedPullRequestOnce(t *testing.T) {
	t.Parallel()

	since := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	mock := newGHMock()
	mock.ExpectCustom(isHistoryRequest).Once().
		WillSucceed(`{"data":{"repository":{"object":{
			"p0":{"nodes":[{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"nodes":[{"number":7}]}}]},
			"p1":{"nodes":[{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"nodes":[{"number":7}]}},
				{"author":{"user":{"login":"carol"}},"associatedPullRequests":{"nodes":[{"number":8}]}}]}}}}}`, 0).Build()
	mock.ExpectCustom(isReviewsRequest).Once().
		WillSucceed(`{"data":{"repository":{"pr7":{"reviews":{"nodes":[{"author":{"login":"bob"}}]}},"pr8":{"reviews":{"nodes":[{"author":{"login":"dave"}}]}}}}}`, 0).Build()
	result, err := NewClient(mock, testGHBin).PathHistories(context.Background(), "acme/widgets", "main", []string{"src/a.go", "src/b.go"}, since, 20, 20)
	if err != nil {
		t.Fatal(err)
	}
	got := result.Histories
	var reviews []string
	for _, call := range mock.Executions() {
		if isReviewsRequest(context.Background(), call) {
			reviews = append(reviews, strings.Join(call.Args, " "))
		}
	}
	if len(reviews) != 1 || strings.Count(reviews[0], "pr7:pullRequest(number:7)") != 1 || strings.Count(reviews[0], "pr8:pullRequest(number:8)") != 1 {
		t.Fatalf("review requests = %#v, want PR 7 and 8 once each in one request", reviews)
	}
	if a := got["src/a.go"]; !slices.Equal(a.Authors, []string{"alice"}) || !slices.Equal(a.Reviewers, []string{"bob"}) {
		t.Fatalf("a = %+v", a)
	}
	if b := got["src/b.go"]; !slices.Equal(b.Authors, []string{"alice", "carol"}) || !slices.Equal(b.Reviewers, []string{"bob", "dave"}) {
		t.Fatalf("b = %+v", b)
	}
}

func TestPathHistoriesNoPullRequestsSkipsReviews(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCustom(isHistoryRequest).Once().
		WillSucceed(`{"data":{"repository":{"object":{"p0":{"nodes":[{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"nodes":[]}}]}}}}}`, 0).Build()
	result, err := NewClient(mock, testGHBin).PathHistories(context.Background(), "acme/widgets", "main", []string{"src/a.go"}, time.Now(), 20, 20)
	if err != nil {
		t.Fatal(err)
	}
	got := result.Histories
	if calls := len(mock.Executions()); calls != 1 {
		t.Fatalf("calls = %d, want only the history request", calls)
	}
	if !slices.Equal(got["src/a.go"].Authors, []string{"alice"}) {
		t.Fatalf("history = %+v", got)
	}
}

func TestPathHistoriesQuotesPaths(t *testing.T) {
	t.Parallel()

	query := pathHistoryQuery([]string{`src/a"b\\c.go`})
	if !strings.Contains(query, `path:"src/a\"b\\\\c.go"`) {
		t.Fatalf("query = %s, want the path escaped as a GraphQL string", query)
	}
}

func TestBranchHead(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "repos/acme/widgets/git/ref/heads/release/v1", "--jq", ".object.sha").
		WillSucceed("abc123\n", 0).Build()
	got, err := NewClient(mock, testGHBin).BranchHead(context.Background(), "acme/widgets", "release/v1")
	if err != nil || got != "abc123" {
		t.Fatalf("head=%q err=%v", got, err)
	}
}

func isHistoryRequest(_ context.Context, cfg executor.ToolConfig) bool {
	return isGraphQL(cfg.Args) && strings.Contains(strings.Join(cfg.Args, " "), "history(first:30")
}

func isReviewsRequest(_ context.Context, cfg executor.ToolConfig) bool {
	return isGraphQL(cfg.Args) && strings.Contains(strings.Join(cfg.Args, " "), ":pullRequest(number:")
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
