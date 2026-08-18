package gh

import (
	"context"
	"encoding/json"
	"errors"
	"strconv"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

const testGHBin = "/tmp/prsync-gh-fake"

func TestAuthStatus(t *testing.T) {
	t.Parallel()

	t.Run("ok", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "auth", "status").WillSucceed("", 0).Build()
		if err := NewClient(mock, testGHBin).AuthStatus(context.Background()); err != nil {
			t.Fatalf("AuthStatus() unexpected error: %v", err)
		}
	})

	t.Run("unauthenticated", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "auth", "status").
			WillFail("You are not logged into any GitHub hosts", 1).Build()
		err := NewClient(mock, testGHBin).AuthStatus(context.Background())
		if !errors.Is(err, ErrUnauthenticated) {
			t.Fatalf("AuthStatus() error = %v, want ErrUnauthenticated", err)
		}
	})

	t.Run("missing binary", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "auth", "status").
			WillError(&executor.ExecutableNotFoundError{Command: testGHBin}).Build()
		err := NewClient(mock, testGHBin).AuthStatus(context.Background())
		var notFound *executor.ExecutableNotFoundError
		if !errors.As(err, &notFound) {
			t.Fatalf("AuthStatus() error = %v, want ExecutableNotFoundError", err)
		}
	})
}

func TestUserLogin(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "user", "--jq", ".login").
		WillSucceed("alice\n", 0).Build()
	got, err := NewClient(mock, testGHBin).UserLogin(context.Background())
	if err != nil {
		t.Fatalf("UserLogin() unexpected error: %v", err)
	}
	if got != "alice" {
		t.Fatalf("UserLogin() = %q, want %q", got, "alice")
	}
}

func TestSearchOpenPRRepos(t *testing.T) {
	t.Parallel()

	t.Run("unique preserves first-seen order", func(t *testing.T) {
		t.Parallel()
		body, err := json.Marshal([]map[string]any{
			{"repository": map[string]string{"nameWithOwner": "acme/widgets"}},
			{"repository": map[string]string{"nameWithOwner": "acme/gizmos"}},
			{"repository": map[string]string{"nameWithOwner": "acme/widgets"}},
		})
		if err != nil {
			t.Fatal(err)
		}
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "search", "prs",
			"--author", "alice", "--state", "open", "--limit", "1000", "--json", "repository").
			WillSucceed(string(body), 0).Build()
		repos, capped, err := NewClient(mock, testGHBin).SearchOpenPRRepos(context.Background(), "alice")
		if err != nil {
			t.Fatalf("SearchOpenPRRepos() unexpected error: %v", err)
		}
		if capped {
			t.Fatal("capped = true, want false")
		}
		want := []string{"acme/widgets", "acme/gizmos"}
		if !equalStrings(repos, want) {
			t.Fatalf("repos = %v, want %v", repos, want)
		}
	})

	t.Run("capped when result length is 1000", func(t *testing.T) {
		t.Parallel()
		items := make([]map[string]any, 1000)
		for i := range items {
			items[i] = map[string]any{
				"repository": map[string]string{"nameWithOwner": "acme/r" + strconv.Itoa(i)},
			}
		}
		body, err := json.Marshal(items)
		if err != nil {
			t.Fatal(err)
		}
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "search", "prs",
			"--author", "alice", "--state", "open", "--limit", "1000", "--json", "repository").
			WillSucceed(string(body), 0).Build()
		repos, capped, err := NewClient(mock, testGHBin).SearchOpenPRRepos(context.Background(), "alice")
		if err != nil {
			t.Fatalf("SearchOpenPRRepos() unexpected error: %v", err)
		}
		if !capped {
			t.Fatal("capped = false, want true")
		}
		if len(repos) != 1000 {
			t.Fatalf("len(repos) = %d, want 1000", len(repos))
		}
	})
}

func TestListOpenPRs(t *testing.T) {
	t.Parallel()

	const jsonFields = "number,title,url,baseRefName,headRefName,headRefOid,mergeable,isDraft,reviewDecision,reviewRequests,latestReviews,statusCheckRollup"

	t.Run("parses list", func(t *testing.T) {
		t.Parallel()
		body := `[{
			"number":123,
			"title":"[PROJ-123] Fix the widget",
			"url":"https://github.com/acme/widgets/pull/123",
			"baseRefName":"main",
			"headRefName":"fix-widget",
			"headRefOid":"abc123def456",
			"mergeable":"MERGEABLE",
			"isDraft":false,
			"reviewDecision":"APPROVED",
			"reviewRequests":[{"__typename":"User","login":"reviewer"}],
			"latestReviews":[{"author":{"login":"reviewer"},"state":"APPROVED","submittedAt":"2026-01-01T00:00:00Z"}],
			"statusCheckRollup":[{"name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]
		}]`
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "list",
			"--repo", "acme/widgets", "--author", "alice", "--state", "open",
			"--limit", "1000", "--json", jsonFields).
			WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
		if err != nil {
			t.Fatalf("ListOpenPRs() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].Number != 123 || got[0].Title == "" || got[0].IsDraft {
			t.Fatalf("ListOpenPRs() = %+v", got)
		}
		if got[0].ReviewDecision != "APPROVED" || len(got[0].ReviewRequests) != 1 {
			t.Fatalf("reviews = %+v", got[0])
		}
		if got[0].HeadRefOid != "abc123def456" {
			t.Fatalf("HeadRefOid = %q, want abc123def456", got[0].HeadRefOid)
		}
	})

	t.Run("inaccessible stderr", func(t *testing.T) {
		t.Parallel()
		tests := []struct {
			name   string
			stderr string
		}{
			{name: "http 404", stderr: "gh: HTTP 404: Not Found"},
			{name: "could not resolve", stderr: "Could not resolve to a Repository"},
			{name: "not found", stderr: "GraphQL: Not Found"},
			{name: "archived", stderr: "Repository has been archived"},
		}
		for _, tc := range tests {
			t.Run(tc.name, func(t *testing.T) {
				t.Parallel()
				mock := newGHMock()
				mock.ExpectCommandWithArgs(testGHBin, "pr", "list",
					"--repo", "acme/gone", "--author", "alice", "--state", "open",
					"--limit", "1000", "--json", jsonFields).
					WillFail(tc.stderr, 1).Build()
				_, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/gone", "alice")
				if !errors.Is(err, ErrInaccessible) {
					t.Fatalf("ListOpenPRs() error = %v, want ErrInaccessible", err)
				}
			})
		}
	})

	t.Run("http 403 rate limit is fatal", func(t *testing.T) {
		t.Parallel()
		tests := []struct {
			name   string
			stderr string
		}{
			{name: "api rate limit", stderr: "HTTP 403: API rate limit exceeded"},
			{name: "secondary", stderr: "HTTP 403: secondary rate limit"},
		}
		for _, tc := range tests {
			t.Run(tc.name, func(t *testing.T) {
				t.Parallel()
				mock := newGHMock()
				mock.ExpectCommandWithArgs(testGHBin, "pr", "list",
					"--repo", "acme/widgets", "--author", "alice", "--state", "open",
					"--limit", "1000", "--json", jsonFields).
					WillFail(tc.stderr, 1).Build()
				_, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
				if err == nil {
					t.Fatal("ListOpenPRs() error = nil, want fatal")
				}
				if errors.Is(err, ErrInaccessible) {
					t.Fatalf("ListOpenPRs() treated 403 as inaccessible: %v", err)
				}
			})
		}
	})
}

func TestReviewThreads(t *testing.T) {
	t.Parallel()

	t.Run("paginates and omits cursor on first page", func(t *testing.T) {
		t.Parallel()
		page1 := `{
			"data":{"repository":{"pullRequest":{"reviewThreads":{
				"pageInfo":{"hasNextPage":true,"endCursor":"CURSOR1"},
				"nodes":[{
					"id":"PRRT_a",
					"isResolved":false,
					"comments":{"nodes":[{"id":"PRRC_a","author":{"login":"rev"},"path":"a.go","line":1,"url":"https://ex/a","body":"fix"}]}
				}]
			}}}}
		}`
		page2 := `{
			"data":{"repository":{"pullRequest":{"reviewThreads":{
				"pageInfo":{"hasNextPage":false,"endCursor":"CURSOR2"},
				"nodes":[{
					"id":"PRRT_b",
					"isResolved":true,
					"comments":{"nodes":[{"id":"PRRC_b","author":null,"path":"b.go","line":null,"url":"https://ex/b","body":"ok"}]}
				}]
			}}}}
		}`
		mock := newGHMock()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args) && !hasCursor(cfg.Args)
		}).WillSucceed(page1, 0).Once().Build()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args) && hasCursorValue(cfg.Args, "CURSOR1")
		}).WillSucceed(page2, 0).Once().Build()

		got, err := NewClient(mock, testGHBin).ReviewThreads(context.Background(), "acme", "widgets", 123)
		if err != nil {
			t.Fatalf("ReviewThreads() unexpected error: %v", err)
		}
		if len(got) != 2 {
			t.Fatalf("len(threads) = %d, want 2", len(got))
		}
		if got[0].ID != "PRRT_a" || got[0].IsResolved || len(got[0].Comments) != 1 {
			t.Fatalf("thread0 = %+v", got[0])
		}
		if got[0].Comments[0].Author == nil || got[0].Comments[0].Author.Login != "rev" {
			t.Fatalf("thread0 author = %+v", got[0].Comments[0].Author)
		}
		if got[1].ID != "PRRT_b" || !got[1].IsResolved {
			t.Fatalf("thread1 = %+v", got[1])
		}
		if got[1].Comments[0].Author != nil {
			t.Fatalf("deleted author should be nil, got %+v", got[1].Comments[0].Author)
		}
		if got[1].Comments[0].Line != nil {
			t.Fatalf("null line should be nil, got %v", got[1].Comments[0].Line)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("page cap fails", func(t *testing.T) {
		t.Parallel()
		page := `{
			"data":{"repository":{"pullRequest":{"reviewThreads":{
				"pageInfo":{"hasNextPage":true,"endCursor":"NEXT"},
				"nodes":[{"id":"PRRT_x","isResolved":false,"comments":{"nodes":[]}}]
			}}}}
		}`
		mock := newGHMock()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args)
		}).WillSucceed(page, 0).Build()
		_, err := NewClient(mock, testGHBin).ReviewThreads(context.Background(), "acme", "widgets", 9)
		if err == nil {
			t.Fatal("ReviewThreads() error = nil, want page cap")
		}
		if !strings.Contains(err.Error(), "acme/widgets#9") {
			t.Fatalf("error %q should name owner/repo#N", err)
		}
	})
}

func newGHMock() *executor.MockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testGHBin, true)
	return mock
}

func isGraphQL(args []string) bool {
	return len(args) >= 2 && args[0] == "api" && args[1] == "graphql"
}

func hasCursor(args []string) bool {
	for _, arg := range args {
		if strings.HasPrefix(arg, "cursor=") {
			return true
		}
	}
	return false
}

func hasCursorValue(args []string, cursor string) bool {
	want := "cursor=" + cursor
	for _, arg := range args {
		if arg == want {
			return true
		}
	}
	return false
}

func equalStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
