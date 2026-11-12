package gh

import (
	"strings"
	"sync"
	"testing"
	"time"
)

// TestQueryPageSizes pins the connection sizes chosen to keep GraphQL CPU
// time down under GitHub's secondary limit (#679).
func TestQueryPageSizes(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		query string
		want  []string
	}{
		{
			name:  "path history",
			query: pathHistoryQuery([]string{"src/a.go"}),
			want:  []string{"history(first:30,", "associatedPullRequests(first:2)"},
		},
		{
			name:  "pull request reviews",
			query: prReviewsQuery([]int{7}),
			want:  []string{"reviews(first:10)"},
		},
		{
			name:  "open pr search",
			query: openPRSearchQuery,
			want:  []string{"reviewRequests(first:20)"},
		},
		{
			name:  "open pr view",
			query: openPRViewQuery,
			want:  []string{"reviewRequests(first:20)", "latestReviews(first:20)", "contexts(first:100)"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			for _, want := range tc.want {
				if !strings.Contains(tc.query, want) {
					t.Errorf("query lacks %q: %s", want, tc.query)
				}
			}
			for _, conn := range []string{"history", "associatedPullRequests", "reviews", "reviewRequests", "latestReviews"} {
				if strings.Contains(tc.query, conn+"(first:100") {
					t.Errorf("query still asks for %s(first:100): %s", conn, tc.query)
				}
			}
		})
	}
	if openPRSearchPage != 50 {
		t.Fatalf("openPRSearchPage = %d, want 50", openPRSearchPage)
	}
}

func TestSmallConnectionsAskForHasNextPage(t *testing.T) {
	t.Parallel()

	for name, query := range map[string]string{
		"path history":   pathHistoryQuery([]string{"src/a.go"}),
		"pr reviews":     prReviewsQuery([]int{7}),
		"open pr search": openPRSearchQuery,
		"open pr view":   openPRViewQuery,
	} {
		for _, conn := range []string{"history(", "associatedPullRequests(", "reviews(", "reviewRequests(", "latestReviews("} {
			i := strings.Index(query, conn)
			if i < 0 {
				continue
			}
			rest := query[i:]
			open := strings.Index(rest, "{")
			if !strings.HasPrefix(rest[open:], "{pageInfo{hasNextPage}") {
				t.Errorf("%s: %s does not select pageInfo{hasNextPage} first: %s", name, conn, rest)
			}
		}
	}
}

type mapRollupCache struct {
	mu      sync.Mutex
	entries map[int]rollupCacheEntry
	puts    int
}

type rollupCacheEntry struct {
	head, updatedAt string
	checks          []StatusCheck
}

func newMapRollupCache() *mapRollupCache {
	return &mapRollupCache{entries: map[int]rollupCacheEntry{}}
}

func (m *mapRollupCache) Rollup(_ string, number int, head, updatedAt string) ([]StatusCheck, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	entry, ok := m.entries[number]
	if !ok || entry.head != head || entry.updatedAt != updatedAt {
		return nil, false
	}
	return entry.checks, true
}

func (m *mapRollupCache) PutRollup(_ string, number int, head, updatedAt string, checks []StatusCheck) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.entries[number] = rollupCacheEntry{head: head, updatedAt: updatedAt, checks: checks}
	m.puts++
}

const settledRollup = `{"statusCheckRollup":[{"name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}`

func TestListOpenPRsSkipsRollupViewForUnchangedPR(t *testing.T) {
	t.Parallel()

	cache := newMapRollupCache()
	node := `{"number":7,"headRefOid":"h1","updatedAt":"2026-11-01T00:00:00Z"}`

	first := newGHMock()
	expectPRList(first).WillSucceed(openPRSearchJSON(node), 0).Once().Build()
	expectStatusChecks(first, 7, settledRollup).Once().Build()
	if _, err := NewClient(first, testGHBin).WithRollupCache(cache).ListOpenPRs(t.Context(), "acme/widgets", "alice"); err != nil {
		t.Fatal(err)
	}
	if cache.puts != 1 {
		t.Fatalf("puts = %d, want the settled rollup stored", cache.puts)
	}

	next := newGHMock()
	expectPRList(next).WillSucceed(openPRSearchJSON(node), 0).Once().Build()
	got, err := NewClient(next, testGHBin).WithRollupCache(cache).ListOpenPRs(t.Context(), "acme/widgets", "alice")
	if err != nil {
		t.Fatal(err)
	}
	if calls := len(next.Executions()); calls != 1 {
		t.Fatalf("calls = %d, want only the search", calls)
	}
	if len(got) != 1 || len(got[0].StatusCheckRollup) != 1 || got[0].StatusCheckRollup[0].Name != "ci" || got[0].StatusCheckError != "" {
		t.Fatalf("ListOpenPRs() = %+v, want the stored rollup", got)
	}
}

func TestListOpenPRsViewsChangedPR(t *testing.T) {
	t.Parallel()

	for name, node := range map[string]string{
		"new head":       `{"number":7,"headRefOid":"h2","updatedAt":"2026-11-01T00:00:00Z"}`,
		"new updated at": `{"number":7,"headRefOid":"h1","updatedAt":"2026-11-02T00:00:00Z"}`,
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			cache := newMapRollupCache()
			cache.PutRollup("acme/widgets", 7, "h1", "2026-11-01T00:00:00Z", []StatusCheck{{Name: "old"}})
			mock := newGHMock()
			expectPRList(mock).WillSucceed(openPRSearchJSON(node), 0).Once().Build()
			expectStatusChecks(mock, 7, settledRollup).Once().Build()
			got, err := NewClient(mock, testGHBin).WithRollupCache(cache).ListOpenPRs(t.Context(), "acme/widgets", "alice")
			if err != nil {
				t.Fatal(err)
			}
			if calls := len(mock.Executions()); calls != 2 {
				t.Fatalf("calls = %d, want search and view", calls)
			}
			if got[0].StatusCheckRollup[0].Name != "ci" {
				t.Fatalf("rollup = %+v, want the fresh view", got[0].StatusCheckRollup)
			}
		})
	}
}

func TestListOpenPRsDoesNotStoreUnsettledRollup(t *testing.T) {
	t.Parallel()

	for name, body := range map[string]string{
		"check run in progress": `{"statusCheckRollup":[{"name":"ci","status":"IN_PROGRESS","conclusion":""}]}`,
		"status pending":        `{"statusCheckRollup":[{"context":"ci/legacy","state":"PENDING"}]}`,
		"no checks yet":         `{"statusCheckRollup":[]}`,
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			cache := newMapRollupCache()
			mock := newGHMock()
			expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":7,"headRefOid":"h1","updatedAt":"t1"}`), 0).Once().Build()
			expectStatusChecks(mock, 7, body).Once().Build()
			if _, err := NewClient(mock, testGHBin).WithRollupCache(cache).ListOpenPRs(t.Context(), "acme/widgets", "alice"); err != nil {
				t.Fatal(err)
			}
			if cache.puts != 0 {
				t.Fatalf("puts = %d, want an unsettled rollup left uncached", cache.puts)
			}
		})
	}
}

func TestListOpenPRsWarnsOnTruncatedReviewRequests(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":7,"reviewRequests":{"pageInfo":{"hasNextPage":true},"nodes":[]}}`), 0).Once().Build()
	expectStatusChecks(mock, 7, settledRollup).Once().Build()
	client, probe := newProbedClient(t, mock)
	if _, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice"); err != nil {
		t.Fatal(err)
	}
	assertTruncationWarned(t, probe.logs.String(), "reviewRequests")
}

func TestViewPRWarnsOnTruncatedLatestReviews(t *testing.T) {
	t.Parallel()

	body := `{"data":{"repository":{"pullRequest":{"number":7,"state":"OPEN",
		"latestReviews":{"pageInfo":{"hasNextPage":true},"nodes":[]}}}}}`
	mock := newGHMock()
	expectOpenPRView(t, mock, "acme/widgets", 7).WillSucceed(body, 0).Build()
	client, probe := newProbedClient(t, mock)
	if _, err := client.ViewPR(probe.ctx(), "acme/widgets", 7); err != nil {
		t.Fatal(err)
	}
	assertTruncationWarned(t, probe.logs.String(), "latestReviews")
}

func TestPathHistoriesWarnWhenTruncated(t *testing.T) {
	t.Parallel()

	since := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	mock := newGHMock()
	mock.ExpectCustom(isHistoryRequest).WillSucceed(`{"data":{"repository":{"object":{"p0":{"pageInfo":{"hasNextPage":true},"nodes":[
		{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"pageInfo":{"hasNextPage":true},"nodes":[{"number":7}]}}]}}}}}`, 0).Build()
	mock.ExpectCustom(isReviewsRequest).WillSucceed(`{"data":{"repository":{"pr7":{"reviews":{"pageInfo":{"hasNextPage":true},"nodes":[{"author":{"login":"bob"}}]}}}}}`, 0).Build()
	client, probe := newProbedClient(t, mock)
	got, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", []string{"src/a.go"}, since)
	if err != nil {
		t.Fatal(err)
	}
	if h := got["src/a.go"]; len(h.Authors) != 1 || len(h.Reviewers) != 1 {
		t.Fatalf("history = %+v", got)
	}
	logs := probe.logs.String()
	for _, conn := range []string{"history", "associatedPullRequests", "reviews"} {
		assertTruncationWarned(t, logs, conn)
	}
}

func assertTruncationWarned(t *testing.T, logs, connection string) {
	t.Helper()
	for _, line := range strings.Split(logs, "\n") {
		if strings.Contains(line, `"msg":"github connection truncated"`) &&
			strings.Contains(line, `"level":"WARN"`) &&
			strings.Contains(line, `"connection":"`+connection+`"`) {
			return
		}
	}
	t.Fatalf("logs = %s, want a truncation warning for %s", logs, connection)
}
