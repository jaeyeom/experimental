package gh_test

import (
	"context"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
	executor "github.com/jaeyeom/go-cmdexec"
)

const rollupTTLBin = "/tmp/prsync-gh-fake"

const settledRollupJSON = `{"statusCheckRollup":[{"name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}`

type ttlClock struct{ t time.Time }

func (c *ttlClock) now() time.Time { return c.t }

func newRollupMock() *executor.MockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(rollupTTLBin, true)
	return mock
}

func expectOpenPRs(mock *executor.MockExecutor, node string) {
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		if cfg.Command != rollupTTLBin {
			return false
		}
		for _, arg := range cfg.Args {
			if strings.Contains(arg, "search(query:") {
				return true
			}
		}
		return false
	}).WillSucceed(`{"data":{"search":{"nodes":[`+node+`]}}}`, 0).Once().Build()
}

func expectRollupView(mock *executor.MockExecutor) {
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == rollupTTLBin && slices.Contains(cfg.Args, "statusCheckRollup")
	}).WillSucceed(settledRollupJSON, 0).Once().Build()
}

func viewCalls(mock *executor.MockExecutor) int {
	n := 0
	for _, cfg := range mock.Executions() {
		if slices.Contains(cfg.Args, "statusCheckRollup") {
			n++
		}
	}
	return n
}

func TestListOpenPRsReusesRollupYoungerThanTTL(t *testing.T) {
	t.Parallel()

	const ttl = 30 * time.Minute
	node := `{"number":7,"headRefOid":"h1","updatedAt":"2026-11-01T00:00:00Z"}`
	clk := &ttlClock{t: time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)}
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), clk.now)
	if err != nil {
		t.Fatal(err)
	}
	cache.SetStatusChecksTTL(ttl)
	ctx := gh.WithRollupStats(context.Background())

	round := func(t *testing.T, view bool) {
		t.Helper()
		mock := newRollupMock()
		expectOpenPRs(mock, node)
		if view {
			expectRollupView(mock)
		}
		if _, err := gh.NewClient(mock, rollupTTLBin).WithRollupCache(cache).ListOpenPRs(ctx, "acme/widgets", "alice"); err != nil {
			t.Fatal(err)
		}
		got := viewCalls(mock)
		want := 0
		if view {
			want = 1
		}
		if got != want {
			t.Fatalf("pr view calls = %d, want %d", got, want)
		}
	}

	round(t, true)
	clk.t = clk.t.Add(ttl - time.Second)
	round(t, false)
	hits, calls := gh.RollupStats(ctx)
	if hits != 1 || calls != 1 {
		t.Fatalf("stats = %d hits/%d calls, want 1/1", hits, calls)
	}
}

func TestListOpenPRsRefetchesRollupOlderThanTTL(t *testing.T) {
	t.Parallel()

	const ttl = 30 * time.Minute
	node := `{"number":7,"headRefOid":"h1","updatedAt":"2026-11-01T00:00:00Z"}`
	clk := &ttlClock{t: time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)}
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), clk.now)
	if err != nil {
		t.Fatal(err)
	}
	cache.SetStatusChecksTTL(ttl)

	first := newRollupMock()
	expectOpenPRs(first, node)
	expectRollupView(first)
	if _, err := gh.NewClient(first, rollupTTLBin).WithRollupCache(cache).ListOpenPRs(context.Background(), "acme/widgets", "alice"); err != nil {
		t.Fatal(err)
	}

	clk.t = clk.t.Add(ttl + time.Second)
	next := newRollupMock()
	expectOpenPRs(next, node)
	expectRollupView(next)
	if _, err := gh.NewClient(next, rollupTTLBin).WithRollupCache(cache).ListOpenPRs(context.Background(), "acme/widgets", "alice"); err != nil {
		t.Fatal(err)
	}
	if got := viewCalls(next); got != 1 {
		t.Fatalf("pr view calls = %d, want 1", got)
	}
}

func TestListOpenPRsFetchesRollupEveryRoundWhenTTLIsZero(t *testing.T) {
	t.Parallel()

	// status_checks_ttl=0 does not attach the cache, so a stored rollup is unused.
	clk := &ttlClock{t: time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)}
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), clk.now)
	if err != nil {
		t.Fatal(err)
	}
	cache.PutRollup("acme/widgets", 7, "h1", "t1", []gh.StatusCheck{{
		Name: "old", Status: "COMPLETED", Conclusion: "SUCCESS",
	}})
	node := `{"number":7,"headRefOid":"h1","updatedAt":"t1"}`
	for range 2 {
		mock := newRollupMock()
		expectOpenPRs(mock, node)
		expectRollupView(mock)
		got, err := gh.NewClient(mock, rollupTTLBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
		if err != nil {
			t.Fatal(err)
		}
		if calls := viewCalls(mock); calls != 1 {
			t.Fatalf("pr view calls = %d, want 1", calls)
		}
		if got[0].StatusCheckRollup[0].Name != "ci" {
			t.Fatalf("rollup = %+v, want the fresh view", got[0].StatusCheckRollup)
		}
	}
}
