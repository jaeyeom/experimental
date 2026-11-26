package gh

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestCommentPRWithID(t *testing.T) {
	t.Parallel()

	t.Run("extracts issuecomment ID from URL", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "comment", "123",
			"--repo", "acme/widgets", "--body", "/waive\nflaky").
			WillSucceed("https://github.com/acme/widgets/pull/123#issuecomment-2468135790\n", 0).Build()
		id, err := NewClient(mock, testGHBin).CommentPRWithID(context.Background(), "acme/widgets", 123, "/waive\nflaky")
		if err != nil {
			t.Fatalf("CommentPRWithID() error = %v", err)
		}
		if id != "2468135790" {
			t.Fatalf("id = %q, want 2468135790", id)
		}
	})

	t.Run("empty output returns empty ID", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "comment", "123",
			"--repo", "acme/widgets", "--body", "/waive").
			WillSucceed("", 0).Build()
		id, err := NewClient(mock, testGHBin).CommentPRWithID(context.Background(), "acme/widgets", 123, "/waive")
		if err != nil {
			t.Fatalf("CommentPRWithID() error = %v", err)
		}
		if id != "" {
			t.Fatalf("id = %q, want empty", id)
		}
	})
}

func TestWaiveTimelineAndPagination(t *testing.T) {
	t.Parallel()

	page1 := `{
		"data":{"repository":{"pullRequest":{
			"comments":{
				"pageInfo":{"hasNextPage":true,"endCursor":"C1"},
				"nodes":[
					{"id":"IC_1","databaseId":101,"author":{"login":"bob"},"body":"/waive\nknown flake\n","createdAt":"2026-01-01T10:00:00Z","url":"https://example/c1"}
				]
			},
			"commits":{
				"pageInfo":{"hasNextPage":false,"endCursor":""},
				"nodes":[
					{"commit":{"oid":"aaa111","committedDate":"2026-01-01T09:00:00Z","pushedDate":"2026-01-01T09:05:00Z","parents":{"nodes":[{"oid":"base0"}]}}},
					{"commit":{"oid":"bbb222","committedDate":"2026-01-01T11:00:00Z","pushedDate":null,"parents":{"nodes":[{"oid":"aaa111"},{"oid":"base1"}]}}}
				]
			},
			"timelineItems":{
				"pageInfo":{"hasNextPage":false,"endCursor":""},
				"nodes":[
					{"createdAt":"2026-01-01T09:30:00Z","beforeCommit":{"oid":"old0"},"afterCommit":{"oid":"aaa111"}}
				]
			}
		}}}
	}`
	page2 := `{
		"data":{"repository":{"pullRequest":{
			"comments":{
				"pageInfo":{"hasNextPage":false,"endCursor":""},
				"nodes":[
					{"id":"IC_2","databaseId":null,"author":null,"body":"deleted user comment","createdAt":"2026-01-01T12:00:00Z","url":"https://example/c2"}
				]
			}
		}}}
	}`
	mock := newGHMock()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == testGHBin && isGraphQL(cfg.Args) && !hasCursor(cfg.Args)
	}).WillSucceed(page1, 0).Once().Build()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == testGHBin && isGraphQL(cfg.Args) && slices.Contains(cfg.Args, "cursor=C1")
	}).WillSucceed(page2, 0).Once().Build()

	got, err := NewClient(mock, testGHBin).WaiveTimeline(context.Background(), "acme", "widgets", 123)
	if err != nil {
		t.Fatalf("WaiveTimeline() error = %v", err)
	}
	if len(got.Comments) != 2 {
		t.Fatalf("comments = %+v, want 2", got.Comments)
	}
	if got.Comments[0].ID != "101" || got.Comments[0].Author != "bob" || got.Comments[0].Body != "/waive\nknown flake\n" {
		t.Fatalf("comment[0] = %+v", got.Comments[0])
	}
	if got.Comments[1].ID != "IC_2" || got.Comments[1].Author != "" {
		t.Fatalf("comment[1] = %+v", got.Comments[1])
	}
	if len(got.Commits) != 2 || got.Commits[0].OID != "aaa111" || !got.Commits[0].PushedAt.Equal(time.Date(2026, 1, 1, 9, 5, 0, 0, time.UTC)) {
		t.Fatalf("commits[0] = %+v", got.Commits)
	}
	if !got.Commits[1].PushedAt.IsZero() || !slices.Equal(got.Commits[1].Parents, []string{"aaa111", "base1"}) {
		t.Fatalf("commits[1] = %+v", got.Commits[1])
	}
	if len(got.ForcePushes) != 1 || got.ForcePushes[0].BeforeOID != "old0" || got.ForcePushes[0].AfterOID != "aaa111" {
		t.Fatalf("forcePushes = %+v", got.ForcePushes)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestWaiveTimelineNotFound(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == testGHBin && isGraphQL(cfg.Args)
	}).WillSucceed(`{"data":{"repository":{"pullRequest":null}}}`, 0).Once().Build()

	_, err := NewClient(mock, testGHBin).WaiveTimeline(context.Background(), "acme", "widgets", 99)
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("error = %v, want ErrNotFound", err)
	}
}

func TestCompareCommitsAndDiff(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "repos/acme/widgets/compare/main...head1").
		WillSucceed(`{"status":"ahead","merge_base_commit":{"sha":"mb123"}}`, 0).Once().Build()
	mock.ExpectCommandWithArgs(testGHBin, "api", "-H", "Accept: application/vnd.github.v3.diff", "repos/acme/widgets/compare/mb123..head1").
		WillSucceed("diff --git a/f b/f\n+hi\n", 0).Once().Build()

	client := NewClient(mock, testGHBin)
	status, mb, err := client.CompareCommits(context.Background(), "acme", "widgets", "main", "head1")
	if err != nil {
		t.Fatalf("CompareCommits() error = %v", err)
	}
	if status != "ahead" || mb != "mb123" {
		t.Fatalf("got (%q, %q), want (ahead, mb123)", status, mb)
	}
	diff, err := client.CompareDiff(context.Background(), "acme", "widgets", "mb123", "head1")
	if err != nil {
		t.Fatalf("CompareDiff() error = %v", err)
	}
	if !strings.Contains(string(diff), "+hi\n") {
		t.Fatalf("diff = %q", string(diff))
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}
