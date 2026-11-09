package gh

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"log/slog"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
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

	t.Run("parses list", func(t *testing.T) {
		t.Parallel()
		body := openPRSearchJSON(`{
			"number":123,
			"title":"[PROJ-123] Fix the widget",
			"url":"https://github.com/acme/widgets/pull/123",
			"state":"OPEN",
			"baseRefName":"main",
			"headRefName":"fix-widget",
			"headRefOid":"abc123def456",
			"mergeable":"MERGEABLE",
			"mergeStateStatus":"BEHIND",
			"isDraft":false,
			"reviewDecision":"APPROVED",
			"reviewRequests":{"nodes":[
				{"requestedReviewer":{"__typename":"User","login":"reviewer"}},
				{"requestedReviewer":{"__typename":"Team","name":"Widgets","slug":"widgets"}}
			]}
		}`)
		mock := newGHMock()
		expectOpenPRSearch(mock, "acme/widgets", "alice", openPRSearchPage).WillSucceed(body, 0).Build()
		expectStatusChecks(mock, 123, `{"statusCheckRollup":[{"name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}`).Once().Build()
		got, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
		if err != nil {
			t.Fatalf("ListOpenPRs() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].Number != 123 || got[0].Title == "" || got[0].IsDraft {
			t.Fatalf("ListOpenPRs() = %+v", got)
		}
		if got[0].ReviewDecision != "APPROVED" || len(got[0].ReviewRequests) != 2 {
			t.Fatalf("reviews = %+v", got[0])
		}
		if got[0].ReviewRequests[0].Type != "User" || got[0].ReviewRequests[0].Login != "reviewer" {
			t.Fatalf("user request = %+v", got[0].ReviewRequests[0])
		}
		if got[0].ReviewRequests[1].Type != "Team" || got[0].ReviewRequests[1].Slug != "widgets" {
			t.Fatalf("team request = %+v", got[0].ReviewRequests[1])
		}
		if got[0].HeadRefOid != "abc123def456" {
			t.Fatalf("HeadRefOid = %q, want abc123def456", got[0].HeadRefOid)
		}
		if got[0].MergeStateStatus != "BEHIND" {
			t.Fatalf("MergeStateStatus = %q, want BEHIND", got[0].MergeStateStatus)
		}
		if got[0].State != "open" {
			t.Fatalf("State = %q, want open", got[0].State)
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
				expectOpenPRSearch(mock, "acme/gone", "alice", openPRSearchPage).
					WillFail(tc.stderr, 1).Build()
				_, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/gone", "alice")
				if !errors.Is(err, ErrInaccessible) {
					t.Fatalf("ListOpenPRs() error = %v, want ErrInaccessible", err)
				}
			})
		}
	})

	t.Run("http 403 primary rate limit is fatal", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		expectOpenPRSearch(mock, "acme/widgets", "alice", openPRSearchPage).
			WillFail("HTTP 403: API rate limit exceeded", 1).Once().Build()
		_, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
		if err == nil {
			t.Fatal("ListOpenPRs() error = nil, want fatal")
		}
		if errors.Is(err, ErrInaccessible) {
			t.Fatalf("ListOpenPRs() treated 403 as inaccessible: %v", err)
		}
		if strings.Contains(err.Error(), "transient upstream error") {
			t.Fatalf("ListOpenPRs() retried primary rate limit: %v", err)
		}
		if calls := len(mock.Executions()); calls != 1 {
			t.Fatalf("calls = %d, want 1", calls)
		}
	})
}

func TestOpenPRViewQueryNestsStatusChecks(t *testing.T) {
	t.Parallel()

	const nested = `statusCheckRollup{contexts(first:100){nodes{... on CheckRun{name status conclusion} ... on StatusContext{context state}}}}`
	if !strings.Contains(openPRViewQuery, nested) {
		t.Fatalf("query = %s", openPRViewQuery)
	}
	if strings.Contains(openPRViewQuery, "statusCheckRollup{... on CheckRun") || strings.Contains(openPRViewQuery, "statusCheckRollup{... on StatusContext") {
		t.Fatalf("fragments are direct selections of statusCheckRollup: %s", openPRViewQuery)
	}
	if strings.Contains(openPRSearchQuery, "statusCheckRollup") {
		t.Fatalf("search query = %s", openPRSearchQuery)
	}
}

func TestDecodeOpenPRViewStatusChecks(t *testing.T) {
	t.Parallel()

	raw := `{
		"data":{"repository":{"pullRequest":{
			"number":123,
			"state":"OPEN",
			"statusCheckRollup":{"contexts":{"nodes":[
				{"name":"ci","status":"COMPLETED","conclusion":"SUCCESS"},
				{"context":"ci/circleci: unit","state":"FAILURE"}
			]}}
		}}}
	}`
	got, err := decodeOpenPRView(raw)
	if err != nil {
		t.Fatalf("decodeOpenPRView() error = %v", err)
	}
	if len(got.StatusCheckRollup) != 2 {
		t.Fatalf("rollup = %+v", got.StatusCheckRollup)
	}
	check := got.StatusCheckRollup[0]
	if check.Name != "ci" || check.Status != "COMPLETED" || check.Conclusion != "SUCCESS" {
		t.Fatalf("check run = %+v", check)
	}
	status := got.StatusCheckRollup[1]
	if status.Context != "ci/circleci: unit" || status.State != "FAILURE" || status.Name != "" {
		t.Fatalf("status context = %+v", status)
	}
}

func TestOpenPRQueriesOmitOrganization(t *testing.T) {
	t.Parallel()

	for _, query := range []string{openPRSearchQuery, openPRViewQuery} {
		if strings.Contains(strings.ToLower(query), "organization") {
			t.Fatalf("query selects organization: %s", query)
		}
		if !strings.Contains(query, "requestedReviewer") {
			t.Fatalf("query = %s, want requestedReviewer", query)
		}
		team, ok := inlineFragment(query, "Team")
		if !ok {
			t.Fatalf("query = %s, want a Team fragment", query)
		}
		if strings.Contains(strings.ToLower(team), "organization") || strings.Contains(team, "login") {
			t.Fatalf("Team fragment = %q, want name and slug only", team)
		}
		if !strings.Contains(team, "name") || !strings.Contains(team, "slug") {
			t.Fatalf("Team fragment = %q, want name and slug", team)
		}
		user, ok := inlineFragment(query, "User")
		if !ok || !strings.Contains(user, "login") {
			t.Fatalf("User fragment = %q, want login", user)
		}
	}
	if openPRSearchPage >= 1000 {
		t.Fatalf("openPRSearchPage = %d, want below 1000", openPRSearchPage)
	}
}

func TestListOpenPRsShrinksPageAfterResourceLimit(t *testing.T) {
	t.Parallel()

	body := openPRSearchJSON(`{
		"number":123,
		"title":"[PROJ-123] Fix the widget",
		"url":"https://github.com/acme/widgets/pull/123",
		"state":"OPEN",
		"reviewRequests":{"nodes":[
			{"requestedReviewer":{"__typename":"User","login":"reviewer"}},
			{"requestedReviewer":{"__typename":"Team","name":"Platform Reviewers","slug":"platform-reviewers"}}
		]}
	}`)
	mock := newGHMock()
	expectOpenPRSearch(mock, "acme/widgets", "alice", openPRSearchPage).
		WillFail("GraphQL: Resource limits for this query exceeded", 1).Once().Build()
	expectOpenPRSearch(mock, "acme/widgets", "alice", smallerOpenPRPage(openPRSearchPage)).
		WillSucceed(body, 0).Once().Build()
	expectStatusChecks(mock, 123, `{"statusCheckRollup":[]}`).Once().Build()
	client, probe := newProbedClient(t, mock)
	got, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice")
	if err != nil {
		t.Fatalf("ListOpenPRs() unexpected error: %v", err)
	}
	if len(got) != 1 || got[0].Number != 123 {
		t.Fatalf("ListOpenPRs() = %+v, want PR 123", got)
	}
	if len(got[0].ReviewRequests) != 2 {
		t.Fatalf("review requests = %+v", got[0].ReviewRequests)
	}
	user := got[0].ReviewRequests[0]
	team := got[0].ReviewRequests[1]
	if user.Type != "User" || "User:"+user.Login != "User:reviewer" {
		t.Fatalf("user = %+v, want User:reviewer", user)
	}
	if team.Type != "Team" || team.Name != "Platform Reviewers" || "Team:"+team.Slug != "Team:platform-reviewers" {
		t.Fatalf("team = %+v, want Team:platform-reviewers", team)
	}
	if len(probe.waits) != 0 {
		t.Fatalf("waits = %v, want no transient backoff", probe.waits)
	}
	var pages []string
	for _, cfg := range mock.Executions() {
		joined := strings.Join(cfg.Args, " ")
		if strings.Contains(joined, "requestedReviewer") && strings.Contains(strings.ToLower(joined), "organization") {
			t.Fatalf("query selects organization: %s", joined)
		}
		if !isOpenPRSearchArgs(cfg.Args) {
			continue
		}
		for _, arg := range cfg.Args {
			if strings.HasPrefix(arg, "first=") {
				pages = append(pages, arg)
			}
		}
	}
	wantPages := []string{
		"first=" + strconv.Itoa(openPRSearchPage),
		"first=" + strconv.Itoa(smallerOpenPRPage(openPRSearchPage)),
	}
	if !slices.Equal(pages, wantPages) {
		t.Fatalf("pages = %v, want %v", pages, wantPages)
	}
	log := probe.logs.String()
	failedPage := `"failed_page":` + strconv.Itoa(openPRSearchPage)
	nextPage := `"next_page":` + strconv.Itoa(smallerOpenPRPage(openPRSearchPage))
	if !strings.Contains(log, `"msg":"open pr search page halved"`) ||
		!strings.Contains(log, failedPage) ||
		!strings.Contains(log, nextPage) ||
		!strings.Contains(log, `"level":"INFO"`) {
		t.Fatalf("log = %s, want an info line with %s and %s", log, failedPage, nextPage)
	}
}

func TestListOpenPRsHalvesPageOnHTTP502(t *testing.T) {
	t.Parallel()

	body := openPRSearchJSON(`{
		"number":123,
		"title":"[PROJ-123] Fix the widget",
		"url":"https://github.com/acme/widgets/pull/123",
		"state":"OPEN"
	}`)
	mock := newGHMock()
	expectOpenPRSearch(mock, "acme/widgets", "alice", 100).
		WillFail("HTTP 502: 502 Bad Gateway", 1).Once().Build()
	expectOpenPRSearch(mock, "acme/widgets", "alice", 50).
		WillFail("HTTP 502: 502 Bad Gateway", 1).Once().Build()
	expectOpenPRSearch(mock, "acme/widgets", "alice", 25).
		WillSucceed(body, 0).Once().Build()
	expectStatusChecks(mock, 123, `{"statusCheckRollup":[]}`).Once().Build()
	expectOpenPRSearch(mock, "acme/gadgets", "alice", 25).
		WillSucceed(openPRSearchJSON(), 0).Once().Build()
	client, probe := newProbedClient(t, mock)
	got, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice")
	if err != nil {
		t.Fatalf("ListOpenPRs() unexpected error: %v", err)
	}
	if len(got) != 1 || got[0].Number != 123 {
		t.Fatalf("ListOpenPRs() = %+v, want PR 123", got)
	}
	if pages := searchPages(mock); !slices.Equal(pages, []string{"first=100", "first=50", "first=25"}) {
		t.Fatalf("pages = %v, want 100, 50, 25", pages)
	}
	if !slices.Equal(probe.waits, ghListHTTPRetryBackoffs[:2]) {
		t.Fatalf("waits = %v, want %v", probe.waits, ghListHTTPRetryBackoffs[:2])
	}
	var halved []string
	for _, line := range strings.Split(strings.TrimSpace(probe.logs.String()), "\n") {
		if strings.Contains(line, `"msg":"open pr search page halved"`) {
			halved = append(halved, line)
		}
	}
	if len(halved) != 2 {
		t.Fatalf("halving lines = %v, want 2", halved)
	}
	for i, want := range [][2]int{{100, 50}, {50, 25}} {
		line := halved[i]
		if !strings.Contains(line, `"level":"INFO"`) ||
			!strings.Contains(line, `"failed_page":`+strconv.Itoa(want[0])) ||
			!strings.Contains(line, `"next_page":`+strconv.Itoa(want[1])) ||
			!strings.Contains(line, `"reason":"http 502"`) {
			t.Fatalf("halving line %d = %s, want info %d -> %d reason http 502", i, line, want[0], want[1])
		}
	}

	if _, err := client.ListOpenPRs(probe.ctx(), "acme/gadgets", "alice"); err != nil {
		t.Fatalf("second ListOpenPRs() unexpected error: %v", err)
	}
	if pages := searchPages(mock); !slices.Equal(pages, []string{"first=100", "first=50", "first=25", "first=25"}) {
		t.Fatalf("pages = %v, want the second list at 25", pages)
	}
}

func searchPages(mock *executor.MockExecutor) []string {
	var pages []string
	for _, cfg := range mock.Executions() {
		if !isOpenPRSearchArgs(cfg.Args) {
			continue
		}
		for _, arg := range cfg.Args {
			if strings.HasPrefix(arg, "first=") {
				pages = append(pages, arg)
			}
		}
	}
	return pages
}

func inlineFragment(query, typeName string) (string, bool) {
	marker := "on " + typeName + "{"
	i := strings.Index(query, marker)
	if i < 0 {
		return "", false
	}
	rest := query[i+len(marker):]
	end := strings.IndexByte(rest, '}')
	if end < 0 {
		return "", false
	}
	return rest[:end], true
}

func TestViewPR(t *testing.T) {
	t.Parallel()

	body := `{
		"data":{"repository":{"pullRequest":{
			"number":123,
			"title":"[PROJ-123] Fix the widget",
			"url":"https://github.com/acme/widgets/pull/123",
			"state":"CLOSED",
			"baseRefName":"main",
			"headRefName":"fix-widget",
			"headRefOid":"abc123def456",
			"mergeable":"MERGEABLE",
			"mergeStateStatus":"BEHIND",
			"isDraft":false,
			"reviewDecision":"APPROVED",
			"reviewRequests":{"nodes":[{"requestedReviewer":{"__typename":"User","login":"reviewer"}}]},
			"latestReviews":{"nodes":[{"author":{"login":"reviewer"},"state":"APPROVED","submittedAt":"2026-01-01T00:00:00Z"}]},
			"statusCheckRollup":{"contexts":{"nodes":[{"name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}}
		}}}
	}`

	t.Run("parses one pull request", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		expectOpenPRView(t, mock, "acme/widgets", 123).WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testGHBin).ViewPR(context.Background(), "acme/widgets", 123)
		if err != nil {
			t.Fatalf("ViewPR() unexpected error: %v", err)
		}
		if got.Number != 123 || got.HeadRefOid != "abc123def456" || got.MergeStateStatus != "BEHIND" {
			t.Fatalf("ViewPR() = %+v", got)
		}
		if got.State != "closed" {
			t.Fatalf("State = %q, want closed", got.State)
		}
		if len(got.ReviewRequests) != 1 || got.ReviewRequests[0].Login != "reviewer" {
			t.Fatalf("review requests = %+v", got.ReviewRequests)
		}
		if len(got.LatestReviews) != 1 || got.LatestReviews[0].Author.Login != "reviewer" {
			t.Fatalf("latest reviews = %+v", got.LatestReviews)
		}
		if len(got.StatusCheckRollup) != 1 || got.StatusCheckRollup[0].Name != "ci" {
			t.Fatalf("rollup = %+v", got.StatusCheckRollup)
		}
		if calls := len(mock.Executions()); calls != 1 {
			t.Fatalf("calls = %d, want 1", calls)
		}
	})

	t.Run("missing pull request", func(t *testing.T) {
		t.Parallel()
		tests := []struct {
			name   string
			stderr string
		}{
			{name: "graphql pull request", stderr: "GraphQL: Could not resolve to a PullRequest with the number of 9. (repository.pullRequest)"},
			{name: "pull request not found", stderr: "pull request not found"},
			{name: "no pull requests found", stderr: "no pull requests found for branch \"missing\""},
		}
		for _, tc := range tests {
			t.Run(tc.name, func(t *testing.T) {
				t.Parallel()
				mock := newGHMock()
				expectOpenPRView(t, mock, "acme/widgets", 9).WillFail(tc.stderr, 1).Build()
				_, err := NewClient(mock, testGHBin).ViewPR(context.Background(), "acme/widgets", 9)
				if !errors.Is(err, ErrNotFound) {
					t.Fatalf("ViewPR() error = %v, want ErrNotFound", err)
				}
				if errors.Is(err, ErrInaccessible) {
					t.Fatalf("ViewPR() treated a missing PR as an inaccessible repo: %v", err)
				}
				if calls := len(mock.Executions()); calls != 1 {
					t.Fatalf("calls = %d, want 1", calls)
				}
			})
		}
	})

	t.Run("inaccessible repository", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		expectOpenPRView(t, mock, "acme/gone", 9).
			WillFail("Could not resolve to a Repository with the name 'acme/gone'.", 1).Build()
		_, err := NewClient(mock, testGHBin).ViewPR(context.Background(), "acme/gone", 9)
		if !errors.Is(err, ErrInaccessible) {
			t.Fatalf("ViewPR() error = %v, want ErrInaccessible", err)
		}
	})
}

func TestViewPRState(t *testing.T) {
	t.Parallel()

	const jsonFields = "number,title,url,state,isDraft,closedAt"

	t.Run("normalizes merged state and fills repo", func(t *testing.T) {
		t.Parallel()
		body := `{"number":42,"title":"Fix the widget","url":"https://github.com/acme/widgets/pull/42","state":"MERGED","isDraft":false,"closedAt":"2026-08-18T22:19:28Z"}`
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "view", "42",
			"--repo", "acme/widgets", "--json", jsonFields).
			WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testGHBin).ViewPRState(context.Background(), "acme/widgets", 42)
		if err != nil {
			t.Fatalf("ViewPRState() unexpected error: %v", err)
		}
		if got.Number != 42 || got.State != "merged" || got.ClosedAt != "2026-08-18T22:19:28Z" {
			t.Fatalf("ViewPRState() = %+v", got)
		}
		if got.Repository.NameWithOwner != "acme/widgets" {
			t.Fatalf("repo = %q", got.Repository.NameWithOwner)
		}
	})

	t.Run("missing pull request", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "view", "9",
			"--repo", "acme/widgets", "--json", jsonFields).
			WillFail("GraphQL: Could not resolve to a PullRequest with the number of 9. (repository.pullRequest)", 1).Build()
		_, err := NewClient(mock, testGHBin).ViewPRState(context.Background(), "acme/widgets", 9)
		if !errors.Is(err, ErrNotFound) {
			t.Fatalf("ViewPRState() error = %v, want ErrNotFound", err)
		}
	})
}

func TestSearchAuthoredPRs(t *testing.T) {
	t.Parallel()

	const jsonFields = "number,title,url,state,isDraft,closedAt,repository"

	t.Run("parses states and repository", func(t *testing.T) {
		t.Parallel()
		body := `[
			{"number":32347,"title":"[AP-1306] Fix","url":"https://github.com/acme/x/pull/32347",
			 "state":"merged","isDraft":false,"closedAt":"2026-08-18T22:19:28Z",
			 "repository":{"nameWithOwner":"acme/x"}},
			{"number":100,"title":"[AP-1306] Draft","url":"https://github.com/acme/x/pull/100",
			 "state":"open","isDraft":true,"closedAt":"0001-01-01T00:00:00Z",
			 "repository":{"nameWithOwner":"acme/x"}}
		]`
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "search", "prs", "AP-1306",
			"--author", "alice", "--limit", "100", "--json", jsonFields).
			WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testGHBin).SearchAuthoredPRs(context.Background(), "alice", "AP-1306")
		if err != nil {
			t.Fatalf("SearchAuthoredPRs() unexpected error: %v", err)
		}
		if len(got) != 2 {
			t.Fatalf("len = %d, want 2", len(got))
		}
		if got[0].Number != 32347 || got[0].State != "merged" || got[0].ClosedAt != "2026-08-18T22:19:28Z" {
			t.Fatalf("item0 = %+v", got[0])
		}
		if got[0].Repository.NameWithOwner != "acme/x" {
			t.Fatalf("repo = %q, want acme/x", got[0].Repository.NameWithOwner)
		}
		if got[1].State != "open" || !got[1].IsDraft {
			t.Fatalf("item1 = %+v", got[1])
		}
	})

	t.Run("empty result is non-nil", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "search", "prs", "AP-9999",
			"--author", "alice", "--limit", "100", "--json", jsonFields).
			WillSucceed("[]", 0).Build()
		got, err := NewClient(mock, testGHBin).SearchAuthoredPRs(context.Background(), "alice", "AP-9999")
		if err != nil {
			t.Fatalf("SearchAuthoredPRs() unexpected error: %v", err)
		}
		if got == nil || len(got) != 0 {
			t.Fatalf("got = %v, want empty non-nil", got)
		}
	})

	// gh phrase-quotes a single argv that contains spaces, so
	// `search prs "(A OR B)"` becomes q="(A OR B)" and matches nothing.
	// Each token must be its own argv: search prs A OR B --author ...
	t.Run("OR query is separate argv not one phrase", func(t *testing.T) {
		t.Parallel()
		body := `[{"number":1,"title":"[AP-1306] Fix","url":"https://gh/acme/x/pull/1",
			"state":"merged","isDraft":false,"closedAt":"2026-08-18T22:19:28Z",
			"repository":{"nameWithOwner":"acme/x"}}]`
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "search", "prs", "AP-1306", "OR", "AP-1287",
			"--author", "alice", "--limit", "100", "--json", jsonFields).
			WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testGHBin).SearchAuthoredPRs(context.Background(), "alice", "AP-1306 OR AP-1287")
		if err != nil {
			t.Fatalf("SearchAuthoredPRs() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].Number != 1 {
			t.Fatalf("got = %+v, want the merged AP-1306 hit", got)
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

func TestMarkerComments(t *testing.T) {
	t.Parallel()

	body := `{
		"data":{"repository":{"pullRequest":{
			"commits":{"nodes":[{"commit":{"committedDate":"2026-01-02T03:04:05Z"}}]},
			"comments":{
				"pageInfo":{"hasNextPage":false,"endCursor":null},
				"nodes":[{"url":"https://example/c","body":"RCA EVT class=our-code","createdAt":"2026-01-03T00:00:00Z"}]
			}
		}}}
	}`
	mock := newGHMock()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == testGHBin && isGraphQL(cfg.Args) && !hasCursor(cfg.Args)
	}).WillSucceed(body, 0).Once().Build()

	got, err := NewClient(mock, testGHBin).MarkerComments(context.Background(), "acme", "widgets", 12)
	if err != nil {
		t.Fatalf("MarkerComments() unexpected error: %v", err)
	}
	if !got.HeadCommittedAt.Equal(time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)) {
		t.Fatalf("committed = %s", got.HeadCommittedAt)
	}
	if len(got.Comments) != 1 || got.Comments[0].URL != "https://example/c" || got.Comments[0].Body != "RCA EVT class=our-code" {
		t.Fatalf("comments = %+v", got.Comments)
	}
	if !got.Comments[0].CreatedAt.Equal(time.Date(2026, 1, 3, 0, 0, 0, 0, time.UTC)) {
		t.Fatalf("created = %s", got.Comments[0].CreatedAt)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestCommentPR(t *testing.T) {
	t.Parallel()

	t.Run("ok", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "comment", "123",
			"--repo", "acme/widgets", "--body", "please retry").
			WillSucceed("", 0).Build()
		if err := NewClient(mock, testGHBin).CommentPR(context.Background(), "acme/widgets", 123, "please retry"); err != nil {
			t.Fatalf("CommentPR() unexpected error: %v", err)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("nonzero exit", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "comment", "9",
			"--repo", "acme/widgets", "--body", "hello").
			WillFail("GraphQL: Could not resolve to a PullRequest", 1).Build()
		err := NewClient(mock, testGHBin).CommentPR(context.Background(), "acme/widgets", 9, "hello")
		if err == nil {
			t.Fatal("CommentPR() error = nil, want failure")
		}
		var proc *ProcError
		if !errors.As(err, &proc) {
			t.Fatalf("CommentPR() error = %v, want ProcError", err)
		}
	})
}

const retryListBody = `{"data":{"search":{"nodes":[{"number":7}]}}}`

func TestListOpenPRsRetriesTransientFailure(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		fail func(*executor.MockExpectationBuilder)
	}{
		{
			name: "http 504",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillFail("HTTP 504: 504 Gateway Timeout (https://api.github.com/graphql)", 1)
			},
		},
		{
			name: "http 502",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillFail("HTTP 502: Bad Gateway", 1)
			},
		},
		{
			name: "http 503",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillFail("HTTP 503: Service Unavailable", 1)
			},
		},
		{
			name: "unexpected end of json input",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillFail("unexpected end of JSON input", 1)
			},
		},
		{
			name: "unexpected eof",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillFail("unexpected EOF", 1)
			},
		},
		{
			name: "empty output",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillFail("", 1)
			},
		},
		{
			name: "timeout",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillTimeout(60 * time.Second)
			},
		},
		{
			name: "truncated json",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillSucceed(`[{"number":`, 0)
			},
		},
		{
			name: "secondary rate limit",
			fail: func(b *executor.MockExpectationBuilder) {
				b.WillFail("HTTP 403: You have exceeded a secondary rate limit. Please wait a few minutes before you try again.", 1)
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := newGHMock()
			first := expectPRList(mock).Once()
			tc.fail(first)
			first.Build()
			retryPage := openPRSearchPage
			if strings.HasPrefix(tc.name, "http ") {
				retryPage = smallerOpenPRPage(openPRSearchPage)
			}
			expectOpenPRSearch(mock, "acme/widgets", "alice", retryPage).WillSucceed(retryListBody, 0).Once().Build()
			expectStatusChecks(mock, 7, `{"statusCheckRollup":[]}`).Once().Build()
			client, probe := newProbedClient(t, mock)
			got, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice")
			if err != nil {
				t.Fatalf("ListOpenPRs() unexpected error: %v", err)
			}
			if len(got) != 1 || got[0].Number != 7 {
				t.Fatalf("ListOpenPRs() = %+v, want PR 7", got)
			}
			if calls := len(mock.Executions()); calls != 3 {
				t.Fatalf("calls = %d, want 3", calls)
			}
			if !slices.Equal(probe.waits, []time.Duration{2 * time.Second}) {
				t.Fatalf("waits = %v, want [2s]", probe.waits)
			}
			if !strings.Contains(probe.stderr.String(), "transient") || !strings.Contains(probe.logs.String(), "gh retry") {
				t.Fatalf("stderr=%q logs=%q, want a retry log", probe.stderr.String(), probe.logs.String())
			}
		})
	}
}

func TestListOpenPRsTransientExhausted(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	for _, page := range []int{100, 50, 25, 12, 6} {
		expectOpenPRSearch(mock, "acme/widgets", "alice", page).
			WillFail("HTTP 504: 504 Gateway Timeout (https://api.github.com/graphql)", 1).Once().Build()
	}
	client, probe := newProbedClient(t, mock)
	_, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice")
	if err == nil {
		t.Fatal("ListOpenPRs() error = nil, want exhausted transient error")
	}
	if !strings.Contains(err.Error(), "transient upstream error") {
		t.Fatalf("ListOpenPRs() error = %v, want transient upstream error", err)
	}
	if !strings.Contains(err.Error(), "HTTP 504") {
		t.Fatalf("ListOpenPRs() error = %v, want the upstream 504", err)
	}
	if calls := len(mock.Executions()); calls != ghListHTTPRetryAttempts {
		t.Fatalf("calls = %d, want %d", calls, ghListHTTPRetryAttempts)
	}
	if pages := searchPages(mock); !slices.Equal(pages, []string{"first=100", "first=50", "first=25", "first=12", "first=6"}) {
		t.Fatalf("pages = %v, want each retry at half the page", pages)
	}
	if !slices.Equal(probe.waits, ghListHTTPRetryBackoffs) {
		t.Fatalf("waits = %v, want %v", probe.waits, ghListHTTPRetryBackoffs)
	}
	stderr := probe.stderr.String()
	if !strings.Contains(stderr, "attempt 2/5") || !strings.Contains(stderr, "waiting 2s") ||
		!strings.Contains(stderr, "attempt 5/5") || !strings.Contains(stderr, "waiting 1m0s") {
		t.Fatalf("stderr = %q, want retry lines through 60s", stderr)
	}
	if !strings.Contains(probe.logs.String(), `"msg":"gh retry"`) {
		t.Fatalf("logs = %q, want gh retry", probe.logs.String())
	}
}

func TestListOpenPRsHonorsSecondaryRetryAfter(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		stderr string
		want   time.Duration
	}{
		{
			name:   "header seconds",
			stderr: "HTTP 403: secondary rate limit\nRetry-After: 30",
			want:   30 * time.Second,
		},
		{
			name:   "phrase seconds",
			stderr: "HTTP 403: secondary rate limit; retry after 45 seconds",
			want:   45 * time.Second,
		},
		{
			name:   "header http date",
			stderr: "HTTP 403: You have exceeded a secondary rate limit.\nRetry-After: Wed, 21 Oct 2015 07:28:30 GMT",
			want:   30 * time.Second,
		},
		{
			name:   "past http date waits zero",
			stderr: "HTTP 403: secondary rate limit\nRetry-After: Wed, 21 Oct 2015 07:27:00 GMT",
			want:   0,
		},
		{
			name:   "gateway timeout keeps the schedule",
			stderr: "HTTP 504: 504 Gateway Timeout\nRetry-After: 30",
			want:   2 * time.Second,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := newGHMock()
			expectPRList(mock).WillFail(tc.stderr, 1).Once().Build()
			retryPage := openPRSearchPage
			if strings.HasPrefix(tc.stderr, "HTTP 50") {
				retryPage = smallerOpenPRPage(openPRSearchPage)
			}
			expectOpenPRSearch(mock, "acme/widgets", "alice", retryPage).WillSucceed(retryListBody, 0).Once().Build()
			expectStatusChecks(mock, 7, `{"statusCheckRollup":[]}`).Once().Build()
			client, probe := newProbedClient(t, mock)
			if _, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice"); err != nil {
				t.Fatalf("ListOpenPRs() unexpected error: %v", err)
			}
			if !slices.Equal(probe.waits, []time.Duration{tc.want}) {
				t.Fatalf("waits = %v, want [%s]", probe.waits, tc.want)
			}
		})
	}
}

func TestListOpenPRsDoesNotRetryClientErrors(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		stderr string
	}{
		{name: "http 401", stderr: "HTTP 401: Bad credentials"},
		{name: "http 403 forbidden", stderr: "HTTP 403: Must have admin rights to Repository"},
		{name: "permission", stderr: "HTTP 403: Resource not accessible by integration"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := newGHMock()
			expectPRList(mock).WillFail(tc.stderr, 1).Once().Build()
			client, probe := newProbedClient(t, mock)
			_, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice")
			if err == nil {
				t.Fatal("ListOpenPRs() error = nil, want client error")
			}
			if strings.Contains(err.Error(), "transient upstream error") {
				t.Fatalf("ListOpenPRs() retried client error: %v", err)
			}
			if calls := len(mock.Executions()); calls != 1 {
				t.Fatalf("calls = %d, want 1", calls)
			}
			if len(probe.waits) != 0 {
				t.Fatalf("waits = %v, want none", probe.waits)
			}
		})
	}
}

func TestListOpenPRsDoesNotRetryWrongJSONShape(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillSucceed(`{"number":1}`, 0).Once().Build()
	client, probe := newProbedClient(t, mock)
	_, err := client.ListOpenPRs(probe.ctx(), "acme/widgets", "alice")
	if err == nil || !strings.Contains(err.Error(), "decode pr list") {
		t.Fatalf("ListOpenPRs() error = %v, want decode pr list", err)
	}
	if strings.Contains(err.Error(), "transient upstream error") {
		t.Fatalf("ListOpenPRs() retried a complete JSON object: %v", err)
	}
	if calls := len(mock.Executions()); calls != 1 {
		t.Fatalf("calls = %d, want 1", calls)
	}
}

func TestListOpenPRsStopsWhenRetryWaitIsCanceled(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillFail("HTTP 504: 504 Gateway Timeout", 1).Once().Build()
	client, probe := newProbedClient(t, mock)
	client.sleep = func(_ context.Context, d time.Duration) error {
		probe.waits = append(probe.waits, d)
		return context.Canceled
	}
	_, err := client.ListOpenPRs(context.Background(), "acme/widgets", "alice")
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("ListOpenPRs() error = %v, want context.Canceled", err)
	}
	if strings.Contains(err.Error(), "transient upstream error") {
		t.Fatalf("canceled wait reported as exhausted: %v", err)
	}
	if calls := len(mock.Executions()); calls != 1 {
		t.Fatalf("calls = %d, want 1", calls)
	}
}

func TestListOpenPRsAttachesRollup(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":123}`), 0).Once().Build()
	expectStatusChecks(mock, 123, `{"statusCheckRollup":[{"name":"ci","status":"COMPLETED","conclusion":"SUCCESS"}]}`).Once().Build()
	got, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
	if err != nil {
		t.Fatalf("ListOpenPRs() unexpected error: %v", err)
	}
	if len(got) != 1 || got[0].StatusCheckError != "" {
		t.Fatalf("ListOpenPRs() = %+v, want one PR and no rollup error", got)
	}
	if len(got[0].StatusCheckRollup) != 1 || got[0].StatusCheckRollup[0].Name != "ci" {
		t.Fatalf("rollup = %+v, want check ci", got[0].StatusCheckRollup)
	}
	if calls := len(mock.Executions()); calls != 2 {
		t.Fatalf("calls = %d, want 2", calls)
	}
}

func TestListOpenPRsFillsRollupsInListOrder(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":7}`, `{"number":9}`), 0).Once().Build()
	expectStatusChecks(mock, 7, `{"statusCheckRollup":[{"name":"alpha","status":"COMPLETED","conclusion":"SUCCESS"}]}`).Once().Build()
	expectStatusChecks(mock, 9, `{"statusCheckRollup":[{"name":"beta","status":"COMPLETED","conclusion":"FAILURE"}]}`).Once().Build()
	got, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
	if err != nil {
		t.Fatalf("ListOpenPRs() unexpected error: %v", err)
	}
	if len(got) != 2 || got[0].Number != 7 || got[1].Number != 9 {
		t.Fatalf("order = %+v, want 7 then 9", got)
	}
	if len(got[0].StatusCheckRollup) != 1 || got[0].StatusCheckRollup[0].Name != "alpha" {
		t.Fatalf("PR 7 rollup = %+v, want alpha", got[0].StatusCheckRollup)
	}
	if len(got[1].StatusCheckRollup) != 1 || got[1].StatusCheckRollup[0].Name != "beta" {
		t.Fatalf("PR 9 rollup = %+v, want beta", got[1].StatusCheckRollup)
	}
}

func TestListOpenPRsEmptySkipsRollup(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillSucceed(openPRSearchJSON(), 0).Once().Build()
	got, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
	if err != nil {
		t.Fatalf("ListOpenPRs() unexpected error: %v", err)
	}
	if got == nil || len(got) != 0 {
		t.Fatalf("ListOpenPRs() = %#v, want empty slice", got)
	}
	if calls := len(mock.Executions()); calls != 1 {
		t.Fatalf("calls = %d, want 1", calls)
	}
}

func TestListOpenPRsRollupFailureKeepsTheList(t *testing.T) {
	t.Parallel()

	t.Run("http 502", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":7}`, `{"number":9}`), 0).Once().Build()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "view", "7",
			"--repo", "acme/widgets", "--json", "statusCheckRollup").
			WillFail("HTTP 502: 502 Bad Gateway (https://api.github.com/graphql)", 1).Times(3).Build()
		expectStatusChecks(mock, 9, `{"statusCheckRollup":[{"name":"beta","status":"COMPLETED","conclusion":"SUCCESS"}]}`).Once().Build()
		client, _ := newProbedClient(t, mock)
		got, err := client.ListOpenPRs(context.Background(), "acme/widgets", "alice")
		if err != nil {
			t.Fatalf("ListOpenPRs() unexpected error: %v", err)
		}
		if len(got) != 2 {
			t.Fatalf("len = %d, want 2", len(got))
		}
		if got[0].Number != 7 || got[0].StatusCheckRollup != nil || !strings.Contains(got[0].StatusCheckError, "transient upstream error") {
			t.Fatalf("PR 7 = %+v, want rollup error", got[0])
		}
		if got[1].Number != 9 || len(got[1].StatusCheckRollup) != 1 || got[1].StatusCheckRollup[0].Name != "beta" || got[1].StatusCheckError != "" {
			t.Fatalf("PR 9 = %+v, want check beta", got[1])
		}
		lists, views := 0, 0
		for _, cfg := range mock.Executions() {
			if isOpenPRSearchArgs(cfg.Args) {
				lists++
			}
			if len(cfg.Args) > 1 && cfg.Args[0] == "pr" && cfg.Args[1] == "view" && len(cfg.Args) > 2 && cfg.Args[2] == "7" {
				views++
			}
		}
		if lists != 1 || views != 3 {
			t.Fatalf("lists = %d views for 7 = %d, want 1 and 3", lists, views)
		}
	})

	t.Run("resource limit", func(t *testing.T) {
		t.Parallel()
		mock := newGHMock()
		expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":7}`), 0).Once().Build()
		mock.ExpectCommandWithArgs(testGHBin, "pr", "view", "7",
			"--repo", "acme/widgets", "--json", "statusCheckRollup").
			WillFail("GraphQL: Resource limits for this query exceeded", 1).Once().Build()
		got, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
		if err != nil {
			t.Fatalf("ListOpenPRs() unexpected error: %v", err)
		}
		if len(got) != 1 || !strings.Contains(got[0].StatusCheckError, "Resource limits") {
			t.Fatalf("PR = %+v, want resource limit on the rollup", got)
		}
		views := 0
		for _, cfg := range mock.Executions() {
			if len(cfg.Args) > 2 && cfg.Args[0] == "pr" && cfg.Args[1] == "view" && cfg.Args[2] == "7" {
				views++
			}
		}
		if views != 1 {
			t.Fatalf("views = %d, want 1", views)
		}
	})
}

func TestListOpenPRsRollupPrimaryRateLimitIsFatal(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":7}`), 0).Once().Build()
	mock.ExpectCommandWithArgs(testGHBin, "pr", "view", "7",
		"--repo", "acme/widgets", "--json", "statusCheckRollup").
		WillFail("HTTP 403: API rate limit exceeded", 1).Once().Build()
	_, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
	if err == nil {
		t.Fatal("ListOpenPRs() error = nil, want fatal")
	}
	if !strings.Contains(err.Error(), "rate limit") || strings.Contains(err.Error(), "transient upstream error") {
		t.Fatalf("ListOpenPRs() error = %v, want a primary rate limit", err)
	}
	if errors.Is(err, ErrInaccessible) {
		t.Fatalf("ListOpenPRs() treated rate limit as inaccessible: %v", err)
	}
	if calls := len(mock.Executions()); calls != 2 {
		t.Fatalf("calls = %d, want 2", calls)
	}
}

func TestListOpenPRsRollupCanceledIsFatal(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	expectPRList(mock).WillSucceed(openPRSearchJSON(`{"number":7}`), 0).Once().Build()
	mock.ExpectCommandWithArgs(testGHBin, "pr", "view", "7",
		"--repo", "acme/widgets", "--json", "statusCheckRollup").
		WillError(context.Canceled).Once().Build()
	_, err := NewClient(mock, testGHBin).ListOpenPRs(context.Background(), "acme/widgets", "alice")
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("ListOpenPRs() error = %v, want context.Canceled", err)
	}
}

func TestUserLoginRetriesEmptyOutput(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, "api", "user", "--jq", ".login").
		WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs(testGHBin, "api", "user", "--jq", ".login").
		WillSucceed("alice\n", 0).Once().Build()
	client, probe := newProbedClient(t, mock)
	got, err := client.UserLogin(probe.ctx())
	if err != nil {
		t.Fatalf("UserLogin() unexpected error: %v", err)
	}
	if got != "alice" {
		t.Fatalf("UserLogin() = %q, want alice", got)
	}
	if calls := len(mock.Executions()); calls != 2 {
		t.Fatalf("calls = %d, want 2", calls)
	}
	if !slices.Equal(probe.waits, []time.Duration{2 * time.Second}) {
		t.Fatalf("waits = %v, want [2s]", probe.waits)
	}
}

type ghRetryProbe struct {
	waits  []time.Duration
	stderr bytes.Buffer
	logs   bytes.Buffer
}

func newProbedClient(t *testing.T, mock *executor.MockExecutor) (*Client, *ghRetryProbe) {
	t.Helper()
	probe := &ghRetryProbe{}
	client := NewClient(mock, testGHBin)
	client.errOut = &probe.stderr
	client.now = func() time.Time { return time.Date(2015, 10, 21, 7, 28, 0, 0, time.UTC) }
	client.sleep = func(_ context.Context, d time.Duration) error {
		probe.waits = append(probe.waits, d)
		return nil
	}
	return client, probe
}

func (p *ghRetryProbe) ctx() context.Context {
	handler := slog.NewJSONHandler(&p.logs, &slog.HandlerOptions{Level: slog.LevelDebug})
	return runlog.WithLogger(context.Background(), slog.New(handler))
}

func expectPRList(mock *executor.MockExecutor) *executor.MockExpectationBuilder {
	return expectOpenPRSearch(mock, "acme/widgets", "alice", openPRSearchPage)
}

func expectOpenPRSearch(mock *executor.MockExecutor, repo, author string, page int) *executor.MockExpectationBuilder {
	return mock.ExpectCommandWithArgs(testGHBin, openPRSearchArgs(repo, author, page)...)
}

func expectOpenPRView(t *testing.T, mock *executor.MockExecutor, repo string, number int) *executor.MockExpectationBuilder {
	t.Helper()
	args, err := openPRViewArgs(repo, number)
	if err != nil {
		t.Fatal(err)
	}
	return mock.ExpectCommandWithArgs(testGHBin, args...)
}

func openPRSearchJSON(nodes ...string) string {
	return `{"data":{"search":{"nodes":[` + strings.Join(nodes, ",") + `]}}}`
}

func isOpenPRSearchArgs(args []string) bool {
	for _, arg := range args {
		if strings.Contains(arg, "search(query:$q") {
			return true
		}
	}
	return false
}

func expectStatusChecks(mock *executor.MockExecutor, number int, body string) *executor.MockExpectationBuilder {
	return mock.ExpectCommandWithArgs(testGHBin, "pr", "view", strconv.Itoa(number),
		"--repo", "acme/widgets", "--json", "statusCheckRollup").WillSucceed(body, 0)
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
