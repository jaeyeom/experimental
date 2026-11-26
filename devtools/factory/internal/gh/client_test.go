package gh

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	testGHBin = "/tmp/factory-gh-fake"

	// wantSearchQuery is the GraphQL search from the issue plan.
	wantSearchQuery = "query($q:String!,$after:String){ search(query:$q,type:ISSUE,first:100,after:$after){" +
		" pageInfo{hasNextPage endCursor}" +
		" nodes{ ... on PullRequest { number title url isDraft merged mergedAt createdAt closedAt headRefName" +
		" repository{nameWithOwner}" +
		" closingIssuesReferences(first:5){ nodes{ number repository{nameWithOwner} } } } } } }"

	openByMeSearch = "is:pr is:open author:@me archived:false"
)

func TestOpenByMeTwoPages(t *testing.T) {
	t.Parallel()

	const cursor = "cursor-page-2"
	page1 := searchPageJSON(true, cursor, `[
		{
			"number": 7,
			"title": "wip [PRJ-1]",
			"url": "https://github.com/acme/widgets/pull/7",
			"isDraft": true,
			"merged": false,
			"mergedAt": null,
			"createdAt": "2026-01-01T00:00:00Z",
			"closedAt": null,
			"headRefName": "feature",
			"repository": {"nameWithOwner": "acme/widgets"},
			"closingIssuesReferences": {"nodes": [
				{"number": 9, "repository": {"nameWithOwner": "acme/widgets"}},
				{"number": 12, "repository": {"nameWithOwner": "acme/other"}}
			]}
		}
	]`)
	page2 := searchPageJSON(false, "", `[
		{
			"number": 8,
			"title": "follow-up",
			"url": "https://github.com/acme/widgets/pull/8",
			"isDraft": false,
			"merged": false,
			"mergedAt": null,
			"createdAt": "2026-01-01T00:00:00Z",
			"closedAt": null,
			"headRefName": "feature",
			"repository": {"nameWithOwner": "acme/widgets"},
			"closingIssuesReferences": {"nodes": []}
		}
	]`)

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, "")...).
		WillSucceed(page1, 0).Once().Build()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, cursor)...).
		WillSucceed(page2, 0).Once().Build()

	got, err := NewClient(mock, testGHBin).OpenByMe(context.Background())
	if err != nil {
		t.Fatalf("OpenByMe() unexpected error: %v", err)
	}
	assertCalls(t, mock, [][]string{
		graphqlArgs(openByMeSearch, ""),
		graphqlArgs(openByMeSearch, cursor),
	})
	if len(got) != 2 {
		t.Fatalf("OpenByMe() len = %d, want 2", len(got))
	}
	wantDraft := PR{
		Repo:          "acme/widgets",
		Number:        7,
		Title:         "wip [PRJ-1]",
		URL:           "https://github.com/acme/widgets/pull/7",
		Draft:         true,
		ClosingIssues: []string{"acme/widgets#9", "acme/other#12"},
	}
	if got[0].Repo != wantDraft.Repo || got[0].Number != wantDraft.Number || got[0].Title != wantDraft.Title ||
		got[0].URL != wantDraft.URL || got[0].Draft != true || got[0].Merged || !got[0].MergedAt.IsZero() ||
		got[0].Head != "feature" ||
		strings.Join(got[0].ClosingIssues, ",") != strings.Join(wantDraft.ClosingIssues, ",") {
		t.Fatalf("OpenByMe()[0] = %+v, want %+v", got[0], wantDraft)
	}
	if got[1].Number != 8 || got[1].Draft || got[1].ClosingIssues == nil || len(got[1].ClosingIssues) != 0 {
		t.Fatalf("OpenByMe()[1] = %+v, want open PR 8 with no closing issues", got[1])
	}
	assertCallTimeout(t, mock, 0, defaultCallTimeout)
}

func TestOpenByMeEmpty(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, "")...).
		WillSucceed(searchPageJSON(false, "", "[]"), 0).Once().Build()

	got, err := NewClient(mock, testGHBin).OpenByMe(context.Background())
	if err != nil {
		t.Fatalf("OpenByMe() unexpected error: %v", err)
	}
	if got == nil || len(got) != 0 {
		t.Fatalf("OpenByMe() = %#v, want empty slice", got)
	}
}

func TestMergedByMe(t *testing.T) {
	t.Parallel()

	const wantSearch = "is:pr is:merged author:@me"
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(wantSearch, "")...).
		WillSucceed(searchPageJSON(false, "", "[]"), 0).Once().Build()

	got, err := NewClient(mock, testGHBin).MergedByMe(context.Background())
	if err != nil {
		t.Fatalf("MergedByMe() unexpected error: %v", err)
	}
	assertCalls(t, mock, [][]string{graphqlArgs(wantSearch, "")})
	if got == nil || len(got) != 0 {
		t.Fatalf("MergedByMe() = %#v, want empty slice", got)
	}
}

func TestMergedByMeSince(t *testing.T) {
	t.Parallel()

	mergedAt := time.Date(2026, 10, 29, 18, 0, 0, 0, time.UTC)
	// 20:00 in a -07:00 zone is the next calendar day in UTC. The search date is t's own date.
	since := time.Date(2026, 10, 30, 20, 0, 0, 0, time.FixedZone("PST", -7*3600))
	wantSearch := "is:pr is:merged author:@me merged:>=2026-10-30"
	body := searchPageJSON(false, "", `[{
		"number": 4,
		"title": "landed [PRJ-2]",
		"url": "https://github.com/acme/widgets/pull/4",
		"isDraft": false,
		"merged": true,
		"mergedAt": "2026-10-29T18:00:00Z",
		"createdAt": "2026-10-28T09:30:00Z",
		"closedAt": "2026-10-29T18:00:00Z",
		"headRefName": "feature",
		"repository": {"nameWithOwner": "acme/widgets"},
		"closingIssuesReferences": {"nodes": [
			{"number": 2, "repository": {"nameWithOwner": "acme/widgets"}}
		]}
	}]`)

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(wantSearch, "")...).
		WillSucceed(body, 0).Once().Build()

	got, err := NewClient(mock, testGHBin).MergedByMeSince(context.Background(), since)
	if err != nil {
		t.Fatalf("MergedByMeSince() unexpected error: %v", err)
	}
	assertCalls(t, mock, [][]string{graphqlArgs(wantSearch, "")})
	if len(got) != 1 {
		t.Fatalf("MergedByMeSince() len = %d, want 1", len(got))
	}
	if want := time.Date(2026, 10, 28, 9, 30, 0, 0, time.UTC); !got[0].CreatedAt.Equal(want) || !got[0].ClosedAt.Equal(mergedAt) {
		t.Fatalf("MergedByMeSince()[0] created %s closed %s, want %s and %s", got[0].CreatedAt, got[0].ClosedAt, want, mergedAt)
	}
	if !got[0].Merged || !got[0].MergedAt.Equal(mergedAt) || got[0].Draft {
		t.Fatalf("MergedByMeSince()[0] = %+v, want merged at %s", got[0], mergedAt)
	}
	if len(got[0].ClosingIssues) != 1 || got[0].ClosingIssues[0] != "acme/widgets#2" {
		t.Fatalf("closing issues = %#v, want [acme/widgets#2]", got[0].ClosingIssues)
	}
}

func TestGraphQLErrors(t *testing.T) {
	t.Parallel()

	const body = `{
		"data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": []}},
		"errors": [{"message": "Something went wrong"}]
	}`
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, "")...).
		WillSucceed(body, 0).Once().Build()

	_, err := NewClient(mock, testGHBin).OpenByMe(context.Background())
	if err == nil {
		t.Fatal("OpenByMe() error = nil, want graphql error")
	}
	if !strings.Contains(err.Error(), "Something went wrong") {
		t.Fatalf("OpenByMe() error = %q, want the graphql message", err)
	}
}

func TestGHFailureIncludesStderr(t *testing.T) {
	t.Parallel()

	const stderr = "HTTP 401: Bad credentials"
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, "")...).
		WillFail(stderr, 1).Once().Build()

	_, err := NewClient(mock, testGHBin).OpenByMe(context.Background())
	if err == nil || !strings.Contains(err.Error(), stderr) {
		t.Fatalf("OpenByMe() error = %v, want stderr %q", err, stderr)
	}
}

func TestGHMissingBinary(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, "")...).
		WillError(&executor.ExecutableNotFoundError{Command: testGHBin}).Once().Build()

	_, err := NewClient(mock, testGHBin).OpenByMe(context.Background())
	var notFound *executor.ExecutableNotFoundError
	if !errors.As(err, &notFound) {
		t.Fatalf("OpenByMe() error = %v, want ExecutableNotFoundError", err)
	}
}

func TestSearchUnexpectedShape(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		want []string
	}{
		{
			name: "missing number",
			body: searchPageJSON(false, "", `[{
				"title": "x",
				"url": "https://github.com/acme/widgets/pull/1",
				"isDraft": false,
				"merged": false,
				"mergedAt": null,
				"createdAt": "2026-01-01T00:00:00Z",
				"closedAt": null,
				"headRefName": "feature",
				"repository": {"nameWithOwner": "acme/widgets"},
				"closingIssuesReferences": {"nodes": []}
			}]`),
			want: []string{"number", "missing"},
		},
		{
			name: "null node",
			body: searchPageJSON(false, "", `[null]`),
			want: []string{"PullRequest", "null"},
		},
		{
			name: "merged without mergedAt",
			body: searchPageJSON(false, "", `[{
				"number": 4,
				"title": "landed",
				"url": "https://github.com/acme/widgets/pull/4",
				"isDraft": false,
				"merged": true,
				"mergedAt": null,
				"createdAt": "2026-01-01T00:00:00Z",
				"closedAt": null,
				"headRefName": "feature",
				"repository": {"nameWithOwner": "acme/widgets"},
				"closingIssuesReferences": {"nodes": []}
			}]`),
			want: []string{"mergedAt", "null"},
		},
		{
			name: "bad repository",
			body: searchPageJSON(false, "", `[{
				"number": 4,
				"title": "landed",
				"url": "https://github.com/acme/widgets/pull/4",
				"isDraft": false,
				"merged": false,
				"mergedAt": null,
				"createdAt": "2026-01-01T00:00:00Z",
				"closedAt": null,
				"headRefName": "feature",
				"repository": {"nameWithOwner": "widgets"},
				"closingIssuesReferences": {"nodes": []}
			}]`),
			want: []string{"owner/repo", "widgets"},
		},
		{
			name: "missing head",
			body: searchPageJSON(false, "", `[{
				"number": 4,
				"title": "landed",
				"url": "https://github.com/acme/widgets/pull/4",
				"isDraft": false,
				"merged": false,
				"mergedAt": null,
				"createdAt": "2026-01-01T00:00:00Z",
				"closedAt": null,
				"repository": {"nameWithOwner": "acme/widgets"},
				"closingIssuesReferences": {"nodes": []}
			}]`),
			want: []string{"headRefName", "missing"},
		},
		{
			name: "missing createdAt",
			body: searchPageJSON(false, "", `[{
				"number": 4,
				"title": "landed",
				"url": "https://github.com/acme/widgets/pull/4",
				"isDraft": false,
				"merged": false,
				"mergedAt": null,
				"closedAt": null,
				"headRefName": "feature",
				"repository": {"nameWithOwner": "acme/widgets"},
				"closingIssuesReferences": {"nodes": []}
			}]`),
			want: []string{"createdAt", "missing"},
		},
		{
			name: "bad closedAt",
			body: searchPageJSON(false, "", `[{
				"number": 4,
				"title": "landed",
				"url": "https://github.com/acme/widgets/pull/4",
				"isDraft": false,
				"merged": false,
				"mergedAt": null,
				"createdAt": "2026-01-01T00:00:00Z",
				"closedAt": "yesterday",
				"headRefName": "feature",
				"repository": {"nameWithOwner": "acme/widgets"},
				"closingIssuesReferences": {"nodes": []}
			}]`),
			want: []string{"closedAt", "yesterday"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := newGHMock()
			mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, "")...).
				WillSucceed(tc.body, 0).Once().Build()
			_, err := NewClient(mock, testGHBin).OpenByMe(context.Background())
			if err == nil {
				t.Fatal("OpenByMe() error = nil, want a shape error")
			}
			msg := err.Error()
			for _, part := range tc.want {
				if !strings.Contains(msg, part) {
					t.Fatalf("OpenByMe() error = %q, want it to contain %q", msg, part)
				}
			}
		})
	}
}

func TestSearchRepeatedCursor(t *testing.T) {
	t.Parallel()

	const cursor = "same-cursor"
	body := searchPageJSON(true, cursor, "[]")
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, "")...).
		WillSucceed(body, 0).Once().Build()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(openByMeSearch, cursor)...).
		WillSucceed(body, 0).Once().Build()

	_, err := NewClient(mock, testGHBin).OpenByMe(context.Background())
	if err == nil || !strings.Contains(err.Error(), cursor) {
		t.Fatalf("OpenByMe() error = %v, want it to name the repeated cursor", err)
	}
}

func TestMergedByTitle(t *testing.T) {
	t.Parallel()

	const wantSearch = `is:pr is:merged author:@me in:title "PRJ-9" archived:false`
	if strings.Contains(wantSearch, "merged:") {
		t.Fatalf("search = %q, want no merged-after date", wantSearch)
	}
	body := searchPageJSON(false, "", `[{
		"number": 9,
		"title": "[PRJ-9] landed",
		"url": "https://github.com/acme/widgets/pull/9",
		"isDraft": false,
		"merged": true,
		"mergedAt": "2020-01-15T00:00:00Z",
		"createdAt": "2026-01-01T00:00:00Z",
		"closedAt": null,
		"headRefName": "feature",
		"repository": {"nameWithOwner": "acme/widgets"},
		"closingIssuesReferences": {"nodes": []}
	}]`)
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(wantSearch, "")...).
		WillSucceed(body, 0).Once().Build()

	got, err := NewClient(mock, testGHBin).MergedByTitle(context.Background(), "PRJ-9")
	if err != nil {
		t.Fatalf("MergedByTitle() unexpected error: %v", err)
	}
	if len(got) != 1 || got[0].Number != 9 || !got[0].Merged || got[0].Title != "[PRJ-9] landed" {
		t.Fatalf("MergedByTitle() = %+v", got)
	}
	if got[0].MergedAt.IsZero() || !got[0].MergedAt.Before(time.Now().Add(-7*24*time.Hour)) {
		t.Fatalf("MergedAt = %s, want older than the recent window", got[0].MergedAt)
	}
	if _, err := NewClient(mock, testGHBin).MergedByTitle(context.Background(), ""); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty error = %v", err)
	}
	if _, err := NewClient(mock, testGHBin).MergedByTitle(context.Background(), "PRJ-\n9"); err == nil || !strings.Contains(err.Error(), "without a quote") {
		t.Fatalf("newline error = %v", err)
	}
}

func TestByTitle(t *testing.T) {
	t.Parallel()

	const wantSearch = `is:pr author:@me in:title "PRJ-9" archived:false`
	body := searchPageJSON(false, "", `[{
		"number": 9,
		"title": "[PRJ-9] landed",
		"url": "https://github.com/acme/widgets/pull/9",
		"isDraft": false,
		"merged": true,
		"mergedAt": "2026-10-29T18:00:00Z",
		"createdAt": "2026-01-01T00:00:00Z",
		"closedAt": null,
		"headRefName": "feature",
		"repository": {"nameWithOwner": "acme/widgets"},
		"closingIssuesReferences": {"nodes": []}
	}]`)
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, graphqlArgs(wantSearch, "")...).
		WillSucceed(body, 0).Once().Build()

	got, err := NewClient(mock, testGHBin).ByTitle(context.Background(), "PRJ-9")
	if err != nil {
		t.Fatalf("ByTitle() unexpected error: %v", err)
	}
	if len(got) != 1 || got[0].Number != 9 || !got[0].Merged || got[0].Title != "[PRJ-9] landed" {
		t.Fatalf("ByTitle() = %+v", got)
	}
	if _, err := NewClient(mock, testGHBin).ByTitle(context.Background(), ""); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty error = %v", err)
	}
	if _, err := NewClient(mock, testGHBin).ByTitle(context.Background(), `PRJ-"9"`); err == nil || !strings.Contains(err.Error(), "without a quote") {
		t.Fatalf("quote error = %v", err)
	}
}

func TestSearchQueryText(t *testing.T) {
	t.Parallel()

	if searchQuery != wantSearchQuery {
		t.Fatalf("searchQuery = %q, want %q", searchQuery, wantSearchQuery)
	}
}

func graphqlArgs(query, after string) []string {
	args := []string{"api", "graphql", "-f", "query=" + wantSearchQuery, "-F", "q=" + query}
	if after != "" {
		args = append(args, "-F", "after="+after)
	}
	return args
}

func searchPageJSON(hasNext bool, cursor, nodes string) string {
	next := "false"
	end := "null"
	if hasNext {
		next = "true"
		end = `"` + cursor + `"`
	}
	return `{
		"data": {
			"search": {
				"pageInfo": {"hasNextPage": ` + next + `, "endCursor": ` + end + `},
				"nodes": ` + nodes + `
			}
		}
	}`
}

func newGHMock() *executor.MockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testGHBin, true)
	mock.SetDefaultBehavior(nil, errors.New("unexpected gh command"))
	return mock
}

func assertCalls(t *testing.T, mock *executor.MockExecutor, want [][]string) {
	t.Helper()
	history := mock.GetCallHistory()
	if len(history) != len(want) {
		t.Fatalf("calls = %d, want %d", len(history), len(want))
	}
	for i, args := range want {
		got := history[i].Config.Args
		if strings.Join(got, "\n") != strings.Join(args, "\n") {
			t.Fatalf("call %d args = %#v, want %#v", i, got, args)
		}
		if history[i].Config.Command != testGHBin {
			t.Fatalf("call %d command = %q, want %q", i, history[i].Config.Command, testGHBin)
		}
	}
}

func assertCallTimeout(t *testing.T, mock *executor.MockExecutor, call int, want time.Duration) {
	t.Helper()
	history := mock.GetCallHistory()
	if history[call].Config.Timeout != want {
		t.Fatalf("call %d timeout = %s, want %s", call, history[call].Config.Timeout, want)
	}
}
