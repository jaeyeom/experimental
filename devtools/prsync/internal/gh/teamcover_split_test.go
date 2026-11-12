package gh

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

// GitHub's GraphQL timeout came back as exit 1 with a short JSON body.
const graphQLTimeoutBody = `{"data":null,"errors":[{"message":"Something went wrong while executing your query. This may be the result of a timeout, or it could be a GitHub bug."}]}`

// aliasLimitGH fails any history request that aliases more than limit
// paths, or that asks for bad, the way GitHub times one out. Other
// requests get one commit by alice per path and no pull requests.
type aliasLimitGH struct {
	limit int
	bad   string

	mu    sync.Mutex
	sizes []int
}

func (g *aliasLimitGH) IsAvailable(string) bool { return true }

func (g *aliasLimitGH) Execute(_ context.Context, cfg executor.ToolConfig) (*executor.ExecutionResult, error) {
	query := strings.Join(cfg.Args, " ")
	n := strings.Count(query, ":history(first:30")
	g.mu.Lock()
	g.sizes = append(g.sizes, n)
	g.mu.Unlock()
	if n > g.limit || (g.bad != "" && strings.Contains(query, graphQLString(g.bad))) {
		return &executor.ExecutionResult{
			ExitCode: 1,
			Output:   graphQLTimeoutBody,
			Stderr:   "gh: Something went wrong while executing your query. This may be the result of a timeout, or it could be a GitHub bug.\n",
		}, nil
	}
	var b strings.Builder
	b.WriteString(`{"data":{"repository":{"object":{`)
	for i := range n {
		if i > 0 {
			b.WriteString(",")
		}
		fmt.Fprintf(&b, `"p%d":{"nodes":[{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"nodes":[]}}]}`, i)
	}
	b.WriteString(`}}}}`)
	return &executor.ExecutionResult{Output: b.String()}, nil
}

func historyPaths(n int) []string {
	paths := make([]string, n)
	for i := range paths {
		paths[i] = fmt.Sprintf("src/f%02d.go", i)
	}
	return paths
}

func probedAliasClient(t *testing.T, fake *aliasLimitGH) (*Client, *ghRetryProbe) {
	t.Helper()
	probe := &ghRetryProbe{}
	client := NewClient(fake, testGHBin)
	client.errOut = &probe.stderr
	client.sleep = func(context.Context, time.Duration) error { return nil }
	return client, probe
}

func TestPathHistoriesSplitsTimedOutBatches(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 6}
	client, probe := probedAliasClient(t, fake)
	paths := historyPaths(25)
	got, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", paths, time.Now(), 20, 20)
	if err != nil {
		t.Fatal(err)
	}
	for _, path := range paths {
		if h, ok := got.Histories[path]; !ok || len(h.Authors) != 1 || h.Authors[0] != "alice" {
			t.Fatalf("%s history = %+v, %v; want alice", path, h, ok)
		}
	}
	if got.Batch != 5 {
		t.Fatalf("Batch = %d, want 5, the size that worked", got.Batch)
	}
	// 20 fails, each 10 fails, the 5s work; the last 5 paths start at 5.
	want := []int{20, 10, 5, 5, 10, 5, 5, 5}
	if fmt.Sprint(fake.sizes) != fmt.Sprint(want) {
		t.Fatalf("request sizes = %v, want %v", fake.sizes, want)
	}
	logs := probe.logs.String()
	for _, want := range []string{
		`team-cover: batch of 20 timed out; retrying as 2×10`,
		`team-cover: batch of 10 timed out; retrying as 2×5`,
		`"stdout":"{\"data\":null,\"errors\":[{\"message\":\"Something went wrong`,
		`"stderr":"gh: Something went wrong`,
	} {
		if !strings.Contains(logs, want) {
			t.Fatalf("logs = %s, want %s", logs, want)
		}
	}
}

func TestPathHistoriesSkipsPathThatAlwaysFails(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 100, bad: "src/f02.go"}
	client, probe := probedAliasClient(t, fake)
	paths := historyPaths(8)
	got, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", paths, time.Now(), 5, 5)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := got.Histories["src/f02.go"]; ok {
		t.Fatalf("histories = %+v, want the failing path left out", got.Histories)
	}
	for _, path := range paths {
		if path == "src/f02.go" {
			continue
		}
		if h, ok := got.Histories[path]; !ok || len(h.Authors) != 1 {
			t.Fatalf("%s history = %+v, %v; want it fetched", path, h, ok)
		}
	}
	if got.Batch < 1 {
		t.Fatalf("Batch = %d, want the size that worked", got.Batch)
	}
	logs := probe.logs.String()
	if !strings.Contains(logs, `"level":"WARN"`) || !strings.Contains(logs, `team-cover: skipping path src/f02.go`) {
		t.Fatalf("logs = %s, want a warning that src/f02.go was skipped", logs)
	}
}

func TestPathHistoriesWithoutTimeoutKeepsBatch(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 100}
	client, probe := probedAliasClient(t, fake)
	got, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", historyPaths(3), time.Now(), 5, 5)
	if err != nil {
		t.Fatal(err)
	}
	if got.Batch != 5 {
		t.Fatalf("Batch = %d, want 5: fewer paths than the batch is not a limit", got.Batch)
	}
	if len(fake.sizes) != 1 {
		t.Fatalf("requests = %v, want one", fake.sizes)
	}
}

func TestPathHistoriesDoesNotSplitOtherErrors(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCustom(isHistoryRequest).WillFail("HTTP 401: Bad credentials", 1).Build()
	_, err := NewClient(mock, testGHBin).PathHistories(context.Background(), "acme/widgets", "main", historyPaths(4), time.Now(), 4, 4)
	if err == nil || !strings.Contains(err.Error(), "Bad credentials") {
		t.Fatalf("err = %v, want the credentials error", err)
	}
	if calls := len(mock.Executions()); calls != 1 {
		t.Fatalf("calls = %d, want no split for a non-timeout error", calls)
	}
}

func TestIsSplittableHistoryError(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name string
		err  error
		want bool
	}{
		{"graphql timeout body", &ProcError{ExitCode: 1, Stdout: graphQLTimeoutBody}, true},
		{"resource limits", &ProcError{ExitCode: 1, Stdout: `{"errors":[{"type":"RESOURCE_LIMITS_EXCEEDED","message":"x"}]}`}, true},
		{"bad gateway", fmt.Errorf("transient upstream error: %w", &ProcError{ExitCode: 1, Stderr: "HTTP 502: Bad Gateway"}), true},
		{"gateway timeout", &ProcError{ExitCode: 1, Stderr: "HTTP 504"}, true},
		{"graphql errors in a 200", fmt.Errorf("path history graphql: RESOURCE_LIMITS_EXCEEDED: too much"), true},
		{"bad credentials", &ProcError{ExitCode: 1, Stderr: "HTTP 401: Bad credentials"}, false},
		{"rate limit", ErrRateLimited, false},
	} {
		if got := isSplittable(tc.err); got != tc.want {
			t.Errorf("%s: isSplittable = %v, want %v", tc.name, got, tc.want)
		}
	}
}

func TestPathHistoryQueryHasNoReviewerFields(t *testing.T) {
	t.Parallel()

	query := pathHistoryQuery([]string{"src/a.go", "src/b.go"})
	for _, field := range []string{"review", "Review"} {
		if strings.Contains(query, field) {
			t.Fatalf("query = %s, want no %s fields; reviewers come from a separate step", query, field)
		}
	}
	if !strings.Contains(query, "associatedPullRequests(first:2){pageInfo{hasNextPage}nodes{number}}") {
		t.Fatalf("query = %s, want only pull request numbers per commit", query)
	}
	if strings.Count(query, "history(first:30,") != 2 {
		t.Fatalf("query = %s, want history(first:30) per path", query)
	}
}
