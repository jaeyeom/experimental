package ghlimit

import (
	"bufio"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestClassifyCall(t *testing.T) {
	const listQuery = `query=query($o: String!) { rateLimit { cost } viewer { login } r: repository(owner: $o, name: "x") { id } }`
	for _, tt := range []struct {
		args    []string
		kind    string
		cmd     string
		points  int
		graphql bool
	}{
		{[]string{"api", "graphql", "-f", "query={ viewer { login } }"}, KindGraphQL, "", 1, true},
		{[]string{"api", "graphql", "-f", "query=mutation { addStar(input: {}) { clientMutationId } }"}, KindGraphQLMutation, "", 5, true},
		{[]string{"api", "graphql", "-F", listQuery}, KindGraphQL, "", 1, true},
		{strings.Fields("api repos/o/r/pulls"), KindRESTGet, "", 1, false},
		{strings.Fields("api -X GET repos/o/r -f per_page=1"), KindRESTGet, "", 1, false},
		{strings.Fields("api repos/o/r/issues -f title=t"), KindRESTWrite, "", 5, false},
		{strings.Fields("api -X DELETE repos/o/r/labels/x"), KindRESTWrite, "", 5, false},
		{strings.Fields("pr view 1 --json title"), KindSubcommand, "pr view", 1, true},
		{strings.Fields("pr merge 1 --squash"), KindSubcommand, "pr merge", 5, true},
		{strings.Fields("issue comment 1 --body hi"), KindSubcommand, "issue comment", 5, true},
		{strings.Fields("--version"), KindSubcommand, "--version", 1, true},
	} {
		c := ClassifyCall(tt.args)
		if c.Kind != tt.kind || c.Cmd != tt.cmd || c.Points != tt.points || c.GraphQL() != tt.graphql {
			t.Errorf("ClassifyCall(%q) = %+v (graphql %v); want kind %s cmd %q points %d graphql %v",
				tt.args, c, c.GraphQL(), tt.kind, tt.cmd, tt.points, tt.graphql)
		}
	}
}

func TestClassifyCallQueryHashAndFields(t *testing.T) {
	q := `query Q($o: String!) {
  # a comment { with braces }
  rateLimit { cost remaining }
  r: repository(owner: $o, name: "a{b") { pullRequests(first: 1) { nodes { id } } }
  ...F @include(if: true)
  viewer { login }
}
fragment F on Query { node(id: "x") { id } }`
	a := ClassifyCall([]string{"api", "graphql", "-f", "query=" + q, "-F", "o=me"})
	if want := []string{"rateLimit", "repository", "viewer"}; !slices.Equal(a.QueryFields, want) {
		t.Errorf("QueryFields = %q, want %q", a.QueryFields, want)
	}
	if len(a.QueryHash) != 12 {
		t.Errorf("QueryHash = %q, want 12 hex digits", a.QueryHash)
	}
	b := ClassifyCall([]string{"api", "graphql", "-F", "o=other", "--raw-field", "query=" + strings.ReplaceAll(q, "\n", "\n\t ")})
	if b.QueryHash != a.QueryHash {
		t.Errorf("hash depends on whitespace or variables: %q vs %q", a.QueryHash, b.QueryHash)
	}
	c := ClassifyCall([]string{"api", "graphql", "-f", "query={ viewer { id } }"})
	if c.QueryHash == a.QueryHash {
		t.Error("different queries share a hash")
	}
	if !slices.Equal(c.QueryFields, []string{"viewer"}) {
		t.Errorf("anonymous query fields = %q", c.QueryFields)
	}
	if d := ClassifyCall(strings.Fields("pr view 1")); d.QueryHash != "" || d.QueryFields != nil {
		t.Errorf("a gh subcommand got a query hash: %+v", d)
	}
}

func TestParseRateLimit(t *testing.T) {
	cost, remaining, ok := ParseRateLimit([]byte(`{"data":{"viewer":{"login":"me"},"rateLimit":{"cost":3,"remaining":4990}}}`))
	if !ok || cost != 3 || remaining != 4990 {
		t.Fatalf("ParseRateLimit = %d, %d, %v", cost, remaining, ok)
	}
	for _, out := range []string{`{"data":{"viewer":{}}}`, `me`, ``, strings.Repeat(" ", maxRateLimitParse+1) + `{"data":{"rateLimit":{"cost":1,"remaining":1}}}`} {
		if _, _, ok := ParseRateLimit([]byte(out)); ok {
			t.Errorf("ParseRateLimit(%.40q) found a rate limit", out)
		}
	}
}

func TestLimiterLogsEveryCall(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	var calls []CallEntry
	l.LogCall = func(e CallEntry) error {
		calls = append(calls, e)
		return nil
	}
	out := `{"data":{"rateLimit":{"cost":2,"remaining":100}}}`
	run := func(context.Context) (Result, error) {
		clock.Sleep(250 * time.Millisecond)
		return Result{Stdout: []byte(out), StdoutKept: true, StdoutLen: int64(len(out))}, nil
	}
	args := []string{"api", "graphql", "-f", "query=mutation { rateLimit { cost remaining } x: addStar { id } }"}
	if _, err := l.Run(context.Background(), args, run); err != nil {
		t.Fatal(err)
	}
	if len(calls) != 1 {
		t.Fatalf("logged %d calls, want 1", len(calls))
	}
	e := calls[0]
	if e.Kind != KindGraphQLMutation || e.Points != 5 || e.DurationMS != 250 || e.ExitCode != 0 ||
		e.RateLimited || e.Args != "api graphql -f _" || len(e.QueryHash) != 12 ||
		!slices.Equal(e.QueryFields, []string{"rateLimit", "addStar"}) ||
		e.RateLimitCost == nil || *e.RateLimitCost != 2 || e.RateLimitRemaining == nil || *e.RateLimitRemaining != 100 {
		t.Fatalf("call entry = %+v", e)
	}
}

func TestLimiterLogsEachRunOfARetry(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	var calls []CallEntry
	l.LogCall = func(e CallEntry) error {
		calls = append(calls, e)
		return nil
	}
	runs := 0
	run := func(context.Context) (Result, error) {
		runs++
		if runs == 1 {
			return Result{ExitCode: 1, Stderr: []byte("secondary rate limit\n")}, nil
		}
		return Result{}, nil
	}
	if _, err := l.Run(context.Background(), strings.Fields("pr view 1"), run); err != nil {
		t.Fatal(err)
	}
	if len(calls) != 2 || !calls[0].RateLimited || calls[0].ExitCode != 1 || calls[1].RateLimited || calls[1].Cmd != "pr view" {
		t.Fatalf("calls = %+v; want a rate-limited run then a clean one", calls)
	}
}

func TestCacheHitIsLoggedAsFreeCall(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	l.Config.PointsPerMin = 1
	var calls []CallEntry
	l.LogCall = func(e CallEntry) error {
		calls = append(calls, e)
		return nil
	}
	c := newCache(t, l, nil)
	gh := &countingGH{out: `{"title":"x"}`}
	for range 3 {
		cacheRun(t, c, l, gh, prView...)
	}
	if len(calls) != 3 || calls[0].CacheHit || !calls[1].CacheHit || calls[1].Points != 0 || calls[1].Kind != KindSubcommand {
		t.Fatalf("calls = %+v; want a run then cache hits with 0 points", calls)
	}
	if len(clock.slept) != 0 {
		t.Fatalf("slept %v; cache hits must not use the points budget", clock.slept)
	}
}

func TestAppendCallLogRotates(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state", "calls.jsonl")
	e := CallEntry{Time: time.Unix(1_700_000_000, 0), Kind: KindRESTGet, Points: 1, Args: "api _"}
	line, err := json.Marshal(e)
	if err != nil {
		t.Fatal(err)
	}
	limit := int64(3*(len(line)+1) + 1)
	for range 4 {
		if err := AppendCallLog(path, e, limit); err != nil {
			t.Fatal(err)
		}
	}
	countLines := func(p string) int {
		f, err := os.Open(p)
		if err != nil {
			t.Fatal(err)
		}
		defer f.Close()
		n := 0
		for s := bufio.NewScanner(f); s.Scan(); {
			n++
		}
		return n
	}
	if cur, old := countLines(path), countLines(path+".1"); cur != 1 || old != 3 {
		t.Fatalf("lines: current %d, rotated %d; want 1 and 3", cur, old)
	}
	for range 3 {
		if err := AppendCallLog(path, e, limit); err != nil {
			t.Fatal(err)
		}
	}
	if _, err := os.Stat(path + ".2"); err == nil {
		t.Fatal("rotation kept a third file")
	}
	if cur, old := countLines(path), countLines(path+".1"); cur != 1 || old != 3 {
		t.Fatalf("after a second rotation: current %d, rotated %d", cur, old)
	}
}
