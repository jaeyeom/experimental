package gh

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestBotRereviewSnapshot(t *testing.T) {
	t.Parallel()

	t.Run("decodes single page with pushedDate and first thread comment", func(t *testing.T) {
		t.Parallel()
		raw := `{
			"data":{"repository":{"pullRequest":{
				"commits":{"nodes":[{"commit":{"oid":"head123","committedDate":"2026-06-01T10:00:00Z","pushedDate":"2026-06-01T10:05:00Z"}}]},
				"reviews":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[
					{"author":{"login":"coderabbitai"},"state":"COMMENTED","submittedAt":"2026-06-01T10:01:00Z"},
					{"author":null,"state":"APPROVED","submittedAt":null}
				]},
				"reviewThreads":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[
					{"id":"t1","isResolved":true,"comments":{"nodes":[{"author":{"login":"coderabbitai"}}]}},
					{"id":"t2","isResolved":false,"comments":{"nodes":[{"author":null}]}}
				]},
				"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[
					{"body":"@coderabbitai review","createdAt":"2026-06-01T09:59:00Z"}
				]}
			}}}
		}`
		mock := newGHMock()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args) && !hasCursor(cfg.Args)
		}).WillSucceed(raw, 0).Once().Build()

		got, err := NewClient(mock, testGHBin).BotRereviewSnapshot(context.Background(), "acme", "widgets", 42)
		if err != nil {
			t.Fatalf("BotRereviewSnapshot() error = %v", err)
		}
		if got.HeadSHA != "head123" {
			t.Fatalf("HeadSHA = %q, want head123", got.HeadSHA)
		}
		wantCommitAt := time.Date(2026, 6, 1, 10, 5, 0, 0, time.UTC)
		if !got.LastCommitAt.Equal(wantCommitAt) {
			t.Fatalf("LastCommitAt = %s, want %s (pushedDate)", got.LastCommitAt, wantCommitAt)
		}
		if len(got.Reviews) != 2 || got.Reviews[0].Author != "coderabbitai" || got.Reviews[0].State != "COMMENTED" || got.Reviews[1].Author != "" {
			t.Fatalf("Reviews = %+v", got.Reviews)
		}
		if len(got.Threads) != 2 || got.Threads[0].StarterAuthor != "coderabbitai" || !got.Threads[0].IsResolved || got.Threads[1].StarterAuthor != "" || got.Threads[1].IsResolved {
			t.Fatalf("Threads = %+v", got.Threads)
		}
		if len(got.Comments) != 1 || got.Comments[0].Body != "@coderabbitai review" {
			t.Fatalf("Comments = %+v", got.Comments)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("falls back to committedDate when pushedDate is null", func(t *testing.T) {
		t.Parallel()
		raw := `{
			"data":{"repository":{"pullRequest":{
				"commits":{"nodes":[{"commit":{"oid":"head123","committedDate":"2026-06-01T10:00:00Z","pushedDate":null}}]},
				"reviews":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]},
				"reviewThreads":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]},
				"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}
			}}}
		}`
		mock := newGHMock()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args)
		}).WillSucceed(raw, 0).Once().Build()

		got, err := NewClient(mock, testGHBin).BotRereviewSnapshot(context.Background(), "acme", "widgets", 42)
		if err != nil {
			t.Fatalf("BotRereviewSnapshot() error = %v", err)
		}
		wantCommitAt := time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC)
		if !got.LastCommitAt.Equal(wantCommitAt) {
			t.Fatalf("LastCommitAt = %s, want %s", got.LastCommitAt, wantCommitAt)
		}
	})

	t.Run("paginates reviews, threads, and comments", func(t *testing.T) {
		t.Parallel()
		initial := `{
			"data":{"repository":{"pullRequest":{
				"commits":{"nodes":[{"commit":{"oid":"h1","committedDate":"2026-06-01T10:00:00Z","pushedDate":null}}]},
				"reviews":{"pageInfo":{"hasNextPage":true,"endCursor":"REV1"},"nodes":[{"author":{"login":"bot"},"state":"APPROVED","submittedAt":"2026-06-01T08:00:00Z"}]},
				"reviewThreads":{"pageInfo":{"hasNextPage":true,"endCursor":"THR1"},"nodes":[{"id":"t1","isResolved":true,"comments":{"nodes":[{"author":{"login":"bot"}}]}}]},
				"comments":{"pageInfo":{"hasNextPage":true,"endCursor":"COM1"},"nodes":[{"body":"hi","createdAt":"2026-06-01T07:00:00Z"}]}
			}}}
		}`
		revPage2 := `{"data":{"repository":{"pullRequest":{"reviews":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"author":{"login":"bot"},"state":"CHANGES_REQUESTED","submittedAt":"2026-06-01T09:00:00Z"}]}}}}}`
		thrPage2 := `{"data":{"repository":{"pullRequest":{"reviewThreads":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"id":"t2","isResolved":false,"comments":{"nodes":[{"author":{"login":"bot"}}]}}]}}}}}`
		comPage2 := `{"data":{"repository":{"pullRequest":{"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"body":"@bot review","createdAt":"2026-06-01T11:00:00Z"}]}}}}}`

		mock := newGHMock()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args) && !hasCursor(cfg.Args)
		}).WillSucceed(initial, 0).Once().Build()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args) && hasCursorValue(cfg.Args, "REV1")
		}).WillSucceed(revPage2, 0).Once().Build()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args) && hasCursorValue(cfg.Args, "THR1")
		}).WillSucceed(thrPage2, 0).Once().Build()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args) && hasCursorValue(cfg.Args, "COM1")
		}).WillSucceed(comPage2, 0).Once().Build()

		got, err := NewClient(mock, testGHBin).BotRereviewSnapshot(context.Background(), "acme", "widgets", 42)
		if err != nil {
			t.Fatalf("BotRereviewSnapshot() error = %v", err)
		}
		if len(got.Reviews) != 2 || got.Reviews[1].State != "CHANGES_REQUESTED" {
			t.Fatalf("Reviews = %+v", got.Reviews)
		}
		if len(got.Threads) != 2 || got.Threads[1].IsResolved {
			t.Fatalf("Threads = %+v", got.Threads)
		}
		if len(got.Comments) != 2 || got.Comments[1].Body != "@bot review" {
			t.Fatalf("Comments = %+v", got.Comments)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("missing pull request returns ErrNotFound", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			return cfg.Command == testGHBin && isGraphQL(cfg.Args)
		}).WillSucceed(`{"data":{"repository":{"pullRequest":null}}}`, 0).Once().Build()
		_, err := NewClient(mock, testGHBin).BotRereviewSnapshot(context.Background(), "acme", "widgets", 99)
		if !errors.Is(err, ErrNotFound) {
			t.Fatalf("error = %v, want ErrNotFound", err)
		}
	})

	t.Run("query uses comments(first:1) not comments(last:1)", func(t *testing.T) {
		t.Parallel()
		if !strings.Contains(botRereviewQuery, "comments(first:1)") {
			t.Fatalf("botRereviewQuery must fetch thread starter via comments(first:1): %s", botRereviewQuery)
		}
	})
}
