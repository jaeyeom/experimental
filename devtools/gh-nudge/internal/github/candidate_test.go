package github

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
	"github.com/jaeyeom/experimental/devtools/pkg/ghlimit"
)

func TestCandidateStatesBatchesNPRsInOneQuery(t *testing.T) {
	urls := []string{
		"https://github.com/acme/widgets/pull/1",
		"https://github.com/acme/widgets/pull/2",
		"https://github.com/acme/widgets/pull/3",
		"https://github.com/acme/widgets/pull/4",
		"https://github.com/acme/gadgets/pull/5",
	}
	body := `{"data":{` +
		`"p0":{"pullRequest":{"mergeable":"MERGEABLE","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"CheckRun","name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}}}}]}}},` +
		`"p1":{"pullRequest":{"mergeable":"CONFLICTING","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"CheckRun","name":"ci","status":"COMPLETED","conclusion":"FAILURE"}]}}}}]}}},` +
		`"p2":{"pullRequest":{"mergeable":"UNKNOWN","commits":{"nodes":[{"commit":{"statusCheckRollup":null}}]}}},` +
		`"p3":{"pullRequest":{"mergeable":"MERGEABLE","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"CheckRun","name":"build","status":"IN_PROGRESS","conclusion":null},{"__typename":"StatusContext","context":"ci/circleci","state":"SUCCESS"}]}}}}]}}},` +
		`"p4":{"pullRequest":{"mergeable":"MERGEABLE","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"StatusContext","context":"lint","state":"FAILURE"}]}}}}]}}}` +
		`}}`
	exec := &argsExec{views: map[string]string{}}
	batchExec := &graphqlScriptExec{outputs: []string{body}, fallback: exec}
	client := NewClientWithExecutor(batchExec)

	got, err := client.CandidateStates(urls)
	if err != nil {
		t.Fatalf("CandidateStates() error = %v", err)
	}
	if len(batchExec.calls) != 1 {
		t.Fatalf("gh calls = %d, want 1: %v", len(batchExec.calls), batchExec.calls)
	}
	if client.GraphQLCalls() != 1 {
		t.Fatalf("GraphQLCalls() = %d, want 1", client.GraphQLCalls())
	}
	query := strings.Join(batchExec.calls[0], " ")
	for i := range urls {
		alias := fmt.Sprintf("p%d:repository(", i)
		if !strings.Contains(query, alias) {
			t.Fatalf("query missing alias %s:\n%s", alias, query)
		}
	}
	if got[urls[0]].Mergeable != "MERGEABLE" || len(got[urls[0]].StatusCheckRollup) != 1 || got[urls[0]].StatusCheckRollup[0].Conclusion != "SUCCESS" {
		t.Fatalf("url[0] state = %+v", got[urls[0]])
	}
	if got[urls[1]].Mergeable != "CONFLICTING" {
		t.Fatalf("url[1] mergeable = %q", got[urls[1]].Mergeable)
	}
	if got[urls[2]].Mergeable != "UNKNOWN" || len(got[urls[2]].StatusCheckRollup) != 0 {
		t.Fatalf("url[2] state = %+v", got[urls[2]])
	}
	if len(got[urls[3]].StatusCheckRollup) != 2 || got[urls[3]].StatusCheckRollup[1].Context != "ci/circleci" || got[urls[3]].StatusCheckRollup[1].State != "SUCCESS" {
		t.Fatalf("url[3] checks = %+v", got[urls[3]].StatusCheckRollup)
	}
	if len(got[urls[4]].StatusCheckRollup) != 1 || got[urls[4]].StatusCheckRollup[0].State != "FAILURE" {
		t.Fatalf("url[4] checks = %+v", got[urls[4]].StatusCheckRollup)
	}

	// StatusChecks should now be cached and make 0 additional calls.
	cached, err := client.StatusChecks(urls[0])
	if err != nil {
		t.Fatalf("cached StatusChecks() error = %v", err)
	}
	if len(cached) != 1 || cached[0].Name != "ci" {
		t.Fatalf("cached = %+v", cached)
	}
	if len(batchExec.calls) != 1 {
		t.Fatalf("gh calls after cache = %d, want 1", len(batchExec.calls))
	}
}

func TestCandidateStatesChunksOverBatchLimit(t *testing.T) {
	urls := make([]string, candidateBatchSize+1)
	var first strings.Builder
	first.WriteString(`{"data":{`)
	for i := range urls {
		urls[i] = fmt.Sprintf("https://github.com/acme/widgets/pull/%d", i+1)
	}
	for i := range candidateBatchSize {
		if i > 0 {
			first.WriteByte(',')
		}
		fmt.Fprintf(&first, `"p%d":{"pullRequest":{"mergeable":"MERGEABLE","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[]}}}}]}}}`, i)
	}
	first.WriteString(`}}`)
	second := `{"data":{"p0":{"pullRequest":{"mergeable":"CONFLICTING","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[]}}}}]}}}}}`

	exec := &graphqlScriptExec{outputs: []string{first.String(), second}}
	got, err := NewClientWithExecutor(exec).CandidateStates(urls)
	if err != nil {
		t.Fatalf("CandidateStates() error = %v", err)
	}
	if len(exec.calls) != 2 {
		t.Fatalf("calls = %d, want 2", len(exec.calls))
	}
	if strings.Contains(strings.Join(exec.calls[0], " "), fmt.Sprintf("p%d:", candidateBatchSize)) {
		t.Fatal("first batch exceeded candidateBatchSize")
	}
	if got[urls[0]].Mergeable != "MERGEABLE" || got[urls[candidateBatchSize]].Mergeable != "CONFLICTING" {
		t.Fatalf("first=%+v last=%+v", got[urls[0]], got[urls[candidateBatchSize]])
	}
}

func TestCandidateStatesDeduplicatesAndEmptyMakesNoCalls(t *testing.T) {
	exec := &graphqlScriptExec{}
	client := NewClientWithExecutor(exec)
	got, err := client.CandidateStates(nil)
	if err != nil || len(got) != 0 || len(exec.calls) != 0 {
		t.Fatalf("empty CandidateStates = %v, %v, calls=%d", got, err, len(exec.calls))
	}

	u := "https://github.com/acme/widgets/pull/7"
	exec.outputs = []string{`{"data":{"p0":{"pullRequest":{"mergeable":"MERGEABLE","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[]}}}}]}}}}}`}
	got, err = client.CandidateStates([]string{u, u})
	if err != nil {
		t.Fatalf("CandidateStates() error = %v", err)
	}
	if len(exec.calls) != 1 {
		t.Fatalf("calls = %d, want 1", len(exec.calls))
	}
	if strings.Contains(strings.Join(exec.calls[0], " "), "p1:") {
		t.Fatalf("duplicate URL created second alias: %v", exec.calls[0])
	}
	if got[u].Mergeable != "MERGEABLE" {
		t.Fatalf("got = %+v", got)
	}
}

func TestCandidateStatesHasNextPageFallsBackToStatusChecks(t *testing.T) {
	u1 := "https://github.com/acme/widgets/pull/1"
	u2 := "https://github.com/acme/widgets/pull/2"
	body := `{"data":{` +
		`"p0":{"pullRequest":{"mergeable":"MERGEABLE","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"CheckRun","name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}}}}]}}},` +
		`"p1":{"pullRequest":{"mergeable":"MERGEABLE","commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":true},"nodes":[{"__typename":"CheckRun","name":"partial","status":"COMPLETED","conclusion":"SUCCESS"}]}}}}]}}}` +
		`}}`
	fallback := &argsExec{views: map[string]string{
		u2 + " statusCheckRollup": `{"statusCheckRollup":[{"name":"full1","status":"COMPLETED","conclusion":"SUCCESS"},{"name":"full2","status":"COMPLETED","conclusion":"FAILURE"}]}`,
	}}
	exec := &graphqlScriptExec{outputs: []string{body}, fallback: fallback}
	got, err := NewClientWithExecutor(exec).CandidateStates([]string{u1, u2})
	if err != nil {
		t.Fatalf("CandidateStates() error = %v", err)
	}
	if len(exec.calls) != 2 {
		t.Fatalf("total calls = %d, want 2 (1 graphql + 1 pr view)", len(exec.calls))
	}
	if len(got[u2].StatusCheckRollup) != 2 || got[u2].StatusCheckRollup[1].Name != "full2" {
		t.Fatalf("u2 checks = %+v", got[u2].StatusCheckRollup)
	}
}

func TestCandidateStatesErrorsAndCancelledContext(t *testing.T) {
	u := "https://github.com/acme/widgets/pull/9"
	exec := &graphqlScriptExec{outputs: []string{`{"data":null,"errors":[{"type":"RATE_LIMITED","message":"slow down"}]}`}}
	_, err := NewClientWithExecutor(exec).CandidateStates([]string{u})
	if err == nil || !strings.Contains(err.Error(), "status checks") || !strings.Contains(err.Error(), "slow down") {
		t.Fatalf("err = %v, want status checks + slow down", err)
	}

	exec = &graphqlScriptExec{outputs: []string{`{"data":{"p0":{"pullRequest":null}},"errors":[{"type":"NOT_FOUND","path":["p0","pullRequest"],"message":"Could not resolve"}]}`}}
	_, err = NewClientWithExecutor(exec).CandidateStates([]string{u})
	if err == nil || !strings.Contains(err.Error(), "status checks") {
		t.Fatalf("err = %v, want status checks error", err)
	}

	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	exec = &graphqlScriptExec{}
	_, err = NewClientWithExecutor(exec).CandidateStatesContext(ctx, []string{u})
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("cancelled ctx err = %v, want context.Canceled", err)
	}
	if len(exec.calls) != 0 {
		t.Fatalf("cancelled ctx made %d calls, want 0", len(exec.calls))
	}
}

func TestPullRequestsByRefBatchesInOneQuery(t *testing.T) {
	body := `{"data":{` +
		`"p0":{"pullRequest":{"url":"https://github.com/acme/widgets/pull/10","title":"PR Ten","isDraft":false,"mergeable":"MERGEABLE","headRefName":"feat-10","labels":{"nodes":[{"name":"ready"}]},"reviewRequests":{"nodes":[]},"commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"CheckRun","name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}}}}]}}},` +
		`"p1":{"pullRequest":{"url":"https://github.com/acme/widgets/pull/11","title":"PR Eleven","isDraft":true,"mergeable":"UNKNOWN","headRefName":"feat-11","labels":{"nodes":[]},"reviewRequests":{"nodes":[{"requestedReviewer":{"__typename":"User","login":"alice"}},{"requestedReviewer":{"__typename":"Team","name":"Core","slug":"core"}}]},"commits":{"nodes":[{"commit":{"statusCheckRollup":null}}]}}},` +
		`"p2":{"pullRequest":{"url":"https://github.com/acme/gadgets/pull/20","title":"PR Twenty","isDraft":false,"mergeable":"CONFLICTING","headRefName":"feat-20","labels":{"nodes":[{"name":"do-not-merge"}]},"reviewRequests":{"nodes":[]},"commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"StatusContext","context":"ci/circleci","state":"PENDING"}]}}}}]}}}` +
		`}}`
	exec := &graphqlScriptExec{outputs: []string{body}}
	client := NewClientWithExecutor(exec)
	refs := []PullRef{
		{Owner: "acme", Repo: "widgets", Number: 10, Raw: "acme/widgets#10"},
		{Owner: "acme", Repo: "widgets", Number: 11, Raw: "acme/widgets#11"},
		{Owner: "acme", Repo: "gadgets", Number: 20, Raw: "acme/gadgets#20"},
		{Owner: "Acme", Repo: "Widgets", Number: 10, Raw: "Acme/Widgets#10"},
	}
	prs, err := client.PullRequestsByRef(refs)
	if err != nil {
		t.Fatalf("PullRequestsByRef() error = %v", err)
	}
	if len(exec.calls) != 1 {
		t.Fatalf("calls = %d, want 1: %v", len(exec.calls), exec.calls)
	}
	if len(prs) != 3 {
		t.Fatalf("len(prs) = %d, want 3 (deduped)", len(prs))
	}
	if prs[0].Title != "PR Ten" || prs[0].Mergeable != "MERGEABLE" || len(prs[0].Labels) != 1 || prs[0].Labels[0].Name != "ready" {
		t.Fatalf("prs[0] = %+v", prs[0])
	}
	if len(prs[0].StatusCheckRollup) != 1 || prs[0].StatusCheckRollup[0].Conclusion != "SUCCESS" {
		t.Fatalf("prs[0].StatusCheckRollup = %+v", prs[0].StatusCheckRollup)
	}
	if !prs[1].IsDraft || len(prs[1].ReviewRequests) != 2 || prs[1].ReviewRequests[0].Login != "alice" || prs[1].ReviewRequests[1].Slug != "core" {
		t.Fatalf("prs[1] = %+v", prs[1])
	}
	if prs[2].Mergeable != "CONFLICTING" || prs[2].Labels[0].Name != "do-not-merge" {
		t.Fatalf("prs[2] = %+v", prs[2])
	}
}

func TestPullRequestsByRefNotFoundAndEmpty(t *testing.T) {
	exec := &graphqlScriptExec{}
	prs, err := NewClientWithExecutor(exec).PullRequestsByRef(nil)
	if err != nil || len(prs) != 0 || len(exec.calls) != 0 {
		t.Fatalf("empty = %v, %v, calls=%d", prs, err, len(exec.calls))
	}

	exec.outputs = []string{`{"data":{"p0":{"pullRequest":null}},"errors":[{"type":"NOT_FOUND","path":["p0","pullRequest"],"message":"Could not resolve to a PullRequest"}]}`}
	_, err = NewClientWithExecutor(exec).PullRequestsByRef([]PullRef{{Owner: "acme", Repo: "widgets", Number: 999, Raw: "acme/widgets#999"}})
	if err == nil || !strings.Contains(err.Error(), "acme/widgets#999") {
		t.Fatalf("err = %v, want acme/widgets#999", err)
	}
}

func TestSuggestedConcurrency(t *testing.T) {
	tests := []struct {
		name     string
		env      map[string]string
		lookPath func(string) (string, error)
		evalSym  func(string) (string, error)
		want     int
	}{
		{
			name:     "default gh unthrottled",
			env:      map[string]string{},
			lookPath: func(string) (string, error) { return "/usr/local/bin/gh", nil },
			evalSym:  func(p string) (string, error) { return p, nil },
			want:     DefaultConcurrency,
		},
		{
			name:     "GH_BIN=gh-throttle defaults to 1",
			env:      map[string]string{ghcli.BinEnv: "gh-throttle"},
			lookPath: func(string) (string, error) { return "/opt/bin/gh-throttle", nil },
			evalSym:  func(p string) (string, error) { return p, nil },
			want:     1,
		},
		{
			name:     "GH_BIN full path gh-throttle with GH_THROTTLE_MAX_CONCURRENT=2",
			env:      map[string]string{ghcli.BinEnv: "/opt/bin/gh-throttle", ghlimit.EnvMaxConcurrent: "2"},
			lookPath: func(p string) (string, error) { return p, nil },
			evalSym:  func(p string) (string, error) { return p, nil },
			want:     2,
		},
		{
			name:     "GH_BIN=gh-throttle with higher GH_THROTTLE_MAX_CONCURRENT caps at DefaultConcurrency",
			env:      map[string]string{ghcli.BinEnv: "gh-throttle", ghlimit.EnvMaxConcurrent: "10"},
			lookPath: func(string) (string, error) { return "/opt/bin/gh-throttle", nil },
			evalSym:  func(p string) (string, error) { return p, nil },
			want:     DefaultConcurrency,
		},
		{
			name:     "GH_THROTTLE_OFF=1 restores DefaultConcurrency",
			env:      map[string]string{ghcli.BinEnv: "gh-throttle", "GH_THROTTLE_OFF": "1"},
			lookPath: func(string) (string, error) { return "/opt/bin/gh-throttle", nil },
			evalSym:  func(p string) (string, error) { return p, nil },
			want:     DefaultConcurrency,
		},
		{
			name:     "gh symlinked to gh-throttle on PATH",
			env:      map[string]string{},
			lookPath: func(string) (string, error) { return "/home/u/.local/gh-throttle/bin/gh", nil },
			evalSym:  func(string) (string, error) { return "/opt/bin/gh-throttle", nil },
			want:     1,
		},
		{
			name:     "explicit GH_THROTTLE_MAX_CONCURRENT=1 respected even with gh",
			env:      map[string]string{ghlimit.EnvMaxConcurrent: "1"},
			lookPath: func(string) (string, error) { return "/usr/local/bin/gh", nil },
			evalSym:  func(p string) (string, error) { return p, nil },
			want:     1,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := suggestedConcurrency(func(k string) string { return tc.env[k] }, tc.lookPath, tc.evalSym)
			if got != tc.want {
				t.Fatalf("suggestedConcurrency() = %d, want %d", got, tc.want)
			}
		})
	}
}

type graphqlScriptExec struct {
	calls    [][]string
	outputs  []string
	n        int
	fallback CommandExecutor
}

func (g *graphqlScriptExec) Execute(cmd string, args ...string) (string, error) {
	stdout, _, err := g.ExecuteSeparated(cmd, args...)
	return stdout, err
}

func (g *graphqlScriptExec) ExecuteWithStdin(_ string, cmd string, args ...string) (string, error) {
	return g.Execute(cmd, args...)
}

func (g *graphqlScriptExec) ExecuteSeparated(cmd string, args ...string) (string, string, error) {
	g.calls = append(g.calls, append([]string(nil), args...))
	if len(args) >= 2 && args[0] == "api" && args[1] == "graphql" {
		if g.n >= len(g.outputs) {
			return "", "", errors.New("unexpected graphql call")
		}
		out := g.outputs[g.n]
		g.n++
		return out, "", nil
	}
	if g.fallback != nil && !slices.Contains(args, "graphql") {
		if sep, ok := g.fallback.(interface {
			ExecuteSeparated(string, ...string) (string, string, error)
		}); ok {
			stdout, stderr, err := sep.ExecuteSeparated(cmd, args...)
			if err != nil {
				return stdout, stderr, fmt.Errorf("fallback execute separated: %w", err)
			}
			return stdout, stderr, nil
		}
		out, err := g.fallback.Execute(cmd, args...)
		if err != nil {
			return out, "", fmt.Errorf("fallback execute: %w", err)
		}
		return out, "", nil
	}
	return "", "", errors.New("unexpected gh call: " + strings.Join(args, " "))
}
