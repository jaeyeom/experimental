package scan

import (
	"bytes"
	"context"
	"errors"
	"log/slog"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

// threadsRound is one scan round of acme/widgets that lists prs and
// answers ReviewThreads with threads, or threadErr.
type threadsRound struct {
	cache     ThreadCache
	prs       []gh.PRListItem
	threads   map[int][]gh.Thread
	threadErr error
	ttl       *time.Duration
}

func (r threadsRound) run(t *testing.T, now time.Time) (*scriptGH, Document, string, error) {
	t.Helper()
	g := &scriptGH{
		list:      map[string]listResult{"acme/widgets": {prs: r.prs}},
		threads:   r.threads,
		threadErr: r.threadErr,
	}
	cfg := config.Defaults()
	cfg.Author = "alice"
	if r.ttl != nil {
		cfg.ReviewThreadsTTL = *r.ttl
	}
	deps := depsWith(g, fixtureHerdr{})
	deps.Threads = r.cache
	var logs bytes.Buffer
	ctx := runlog.WithLogger(context.Background(), slog.New(slog.NewJSONHandler(&logs, nil)))
	doc, err := Run(ctx, deps, cfg, []string{"acme/widgets"}, now)
	return g, doc, logs.String(), err
}

func threadsPR(number int, head, updatedAt string) gh.PRListItem {
	item := fixturePRs()[0]
	item.Number = number
	item.HeadRefOid = head
	item.UpdatedAt = updatedAt
	return item
}

func mustRound(t *testing.T, r threadsRound, now time.Time) (*scriptGH, Document, string) {
	t.Helper()
	g, doc, logs, err := r.run(t, now)
	if err != nil {
		t.Fatal(err)
	}
	return g, doc, logs
}

func TestRunReusesStoredReviewThreads(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	prs := []gh.PRListItem{threadsPR(123, "h1", "t1")}
	first, _, logs := mustRound(t, threadsRound{cache: cache, prs: prs, threads: map[int][]gh.Thread{123: fixtureThreads()}}, clock)
	if first.threadCalls[123] != 1 {
		t.Fatalf("first round thread calls = %d, want 1", first.threadCalls[123])
	}
	if !strings.Contains(logs, "review threads 0 hits/1 calls") {
		t.Fatalf("logs = %s, want the review-thread summary", logs)
	}

	second, doc, logs := mustRound(t, threadsRound{cache: cache, prs: prs}, clock)
	if second.threadCalls[123] != 0 {
		t.Fatalf("second round thread calls = %d, want 0", second.threadCalls[123])
	}
	if !doc.PRs[0].Unaddressed || len(doc.PRs[0].BlockingComments) != 1 {
		t.Fatalf("pr = %+v, want the stored thread to classify it", doc.PRs[0])
	}
	if !strings.Contains(logs, "review threads 1 hits/0 calls") {
		t.Fatalf("logs = %s, want 1 hit", logs)
	}
}

func TestRunRefetchesReviewThreadsWhenPRChanges(t *testing.T) {
	t.Parallel()

	for name, changed := range map[string]gh.PRListItem{
		"updatedAt": threadsPR(123, "h1", "t2"),
		"head":      threadsPR(123, "h2", "t1"),
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()

			clock := fixtureNow
			cache := newRoundCache(t, &clock)
			mustRound(t, threadsRound{
				cache: cache, prs: []gh.PRListItem{threadsPR(123, "h1", "t1")},
				threads: map[int][]gh.Thread{123: fixtureThreads()},
			}, clock)

			second, doc, _ := mustRound(t, threadsRound{cache: cache, prs: []gh.PRListItem{changed}}, clock)
			if second.threadCalls[123] != 1 {
				t.Fatalf("thread calls after a change = %d, want 1", second.threadCalls[123])
			}
			if doc.PRs[0].Unaddressed {
				t.Fatal("classified from the stale entry, want the refetched threads")
			}

			third, doc, _ := mustRound(t, threadsRound{
				cache: cache, prs: []gh.PRListItem{changed},
				threads: map[int][]gh.Thread{123: fixtureThreads()},
			}, clock)
			if third.threadCalls[123] != 0 || doc.PRs[0].Unaddressed {
				t.Fatalf("third round calls = %d unaddressed = %v, want the updated entry reused",
					third.threadCalls[123], doc.PRs[0].Unaddressed)
			}
		})
	}
}

func TestRunRefetchesReviewThreadsPastMaxAge(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	round := threadsRound{cache: cache, prs: []gh.PRListItem{threadsPR(123, "h1", "t1")}}
	mustRound(t, round, clock)
	clock = clock.Add(config.Defaults().ReviewThreadsTTL + time.Second)
	g, _, _ := mustRound(t, round, clock)
	if g.threadCalls[123] != 1 {
		t.Fatalf("thread calls past the max age = %d, want 1", g.threadCalls[123])
	}
}

func TestRunReviewThreadsTTLZeroFetchesEveryRound(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	var zero time.Duration
	round := threadsRound{cache: newRoundCache(t, &clock), prs: []gh.PRListItem{threadsPR(123, "h1", "t1")}, ttl: &zero}
	mustRound(t, round, clock)
	g, _, _ := mustRound(t, round, clock)
	if g.threadCalls[123] != 1 {
		t.Fatalf("thread calls with review_threads_ttl=0 = %d, want 1", g.threadCalls[123])
	}
}

func TestRunPrunesReviewThreadsOfClosedPRs(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	mustRound(t, threadsRound{cache: cache, prs: []gh.PRListItem{threadsPR(123, "h1", "t1"), threadsPR(200, "h2", "t1")}}, clock)
	if _, ok := cache.ReviewThreads("acme/widgets", 200, "h2", "t1"); !ok {
		t.Fatal("PR 200 not stored")
	}
	mustRound(t, threadsRound{cache: cache, prs: []gh.PRListItem{threadsPR(123, "h1", "t1")}}, clock)
	if _, ok := cache.ReviewThreads("acme/widgets", 200, "h2", "t1"); ok {
		t.Fatal("PR 200 is no longer open but its threads are still stored")
	}
	if _, ok := cache.ReviewThreads("acme/widgets", 123, "h1", "t1"); !ok {
		t.Fatal("open PR 123 pruned")
	}
}

func TestRunDoesNotStoreFailedReviewThreads(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	prs := []gh.PRListItem{threadsPR(123, "h1", "t1")}
	_, _, _, err := threadsRound{cache: cache, prs: prs, threadErr: errors.New("graphql: boom")}.run(t, clock)
	if err == nil {
		t.Fatal("want the fetch error")
	}
	if _, ok := cache.ReviewThreads("acme/widgets", 123, "h1", "t1"); ok {
		t.Fatal("failed fetch was stored")
	}
	g, _, _ := mustRound(t, threadsRound{cache: cache, prs: prs}, clock)
	if g.threadCalls[123] != 1 {
		t.Fatalf("thread calls after a failed round = %d, want 1", g.threadCalls[123])
	}
}
