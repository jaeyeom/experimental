package github

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

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const testBin = "/tmp/factory-gh-fake"

func TestSyncQuery(t *testing.T) {
	parts := []string{
		"query($owner:String!,$name:String!,$after:String,$since:DateTime)",
		"repository(owner:$owner,name:$name)",
		"issues(first:50,after:$after,orderBy:{field:UPDATED_AT,direction:ASC},filterBy:{since:$since},states:[OPEN,CLOSED])",
		"pageInfo{hasNextPage endCursor}",
		"nodes{" + nodeFields + "}",
	}
	for _, part := range parts {
		if !strings.Contains(syncQuery, part) {
			t.Fatalf("syncQuery missing %s\n%s", part, syncQuery)
		}
	}
	if strings.Count(syncQuery, "{") != strings.Count(syncQuery, "}") {
		t.Fatalf("syncQuery braces = %d open %d close", strings.Count(syncQuery, "{"), strings.Count(syncQuery, "}"))
	}
	getParts := []string{
		"query($owner:String!,$name:String!,$number:Int!)",
		"issue(number:$number){" + nodeFields + "}",
	}
	for _, part := range getParts {
		if !strings.Contains(getQuery, part) {
			t.Fatalf("getQuery missing %s\n%s", part, getQuery)
		}
	}
	if strings.Count(getQuery, "{") != strings.Count(getQuery, "}") {
		t.Fatalf("getQuery braces unbalanced: %s", getQuery)
	}
	for _, field := range []string{
		"author{login}",
		"authorAssociation",
		"locked",
		"issueType{name}",
		"labels(first:50){nodes{name}}",
		"assignees(first:10){nodes{login}}",
		"milestone{dueOn}",
		"parent{number repository{nameWithOwner}}",
		"blockedBy(first:50){pageInfo{hasNextPage} nodes{id number repository{nameWithOwner}}}",
	} {
		if !strings.Contains(nodeFields, field) {
			t.Fatalf("nodeFields missing %s", field)
		}
	}
}

func TestSyncTwoPages(t *testing.T) {
	var logBuf bytes.Buffer
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&logBuf, &slog.HandlerOptions{Level: slog.LevelDebug})))
	t.Cleanup(func() { slog.SetDefault(prev) })

	const cursor = "cursor-page-2"
	openIssue := issueNode(7, map[string]any{
		"title":     "Ship the parser",
		"state":     "OPEN",
		"createdAt": "2026-10-01T00:00:00Z",
		"updatedAt": "2026-10-02T00:00:00Z",
		"issueType": map[string]any{"name": "Bug"},
		"labels":    labelConn("P0", "agent-ready", "P1"),
		"assignees": assigneeConn("alice", "bob"),
		"parent": map[string]any{
			"number":     3,
			"repository": map[string]any{"nameWithOwner": "other/parents"},
		},
		"blockedBy": blockedConn(false, blockNode("I_block", 9, "acme/other")),
	})
	closedIssue := issueNode(8, map[string]any{
		"title":     "Old bug",
		"state":     "CLOSED",
		"createdAt": "2026-09-01T00:00:00Z",
		"updatedAt": "2026-10-03T00:00:00Z",
	})
	laterIssue := issueNode(11, map[string]any{
		"title":  "Later",
		"labels": labelConn("later", "untagged"),
	})

	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, true, cursor, openIssue), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs(cursor)...).
		WillSucceed(issuesPage(t, false, "", closedIssue, laterIssue), 0).Once().Build()

	got, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	assertArgs(t, mock, [][]string{fullSyncArgs(""), fullSyncArgs(cursor)})
	if len(got) != 3 {
		t.Fatalf("len = %d, want 3", len(got))
	}
	wantOpen := source.Item{
		Source:   "github:experimental",
		ID:       "acme/widgets#7",
		Title:    "Ship the parser",
		URL:      "https://github.com/acme/widgets/issues/7",
		Type:     "Bug",
		State:    "OPEN",
		Done:     false,
		Parent:   "other/parents#3",
		Labels:   []string{"P0", "agent-ready", "P1"},
		Assignee: "alice",
		Priority: 0,
		BlockedBy: []source.BlockRef{{
			ID:     "acme/other#9",
			LinkID: "I_block",
		}},
		Created: time.Date(2026, 10, 1, 0, 0, 0, 0, time.UTC),
		Updated: time.Date(2026, 10, 2, 0, 0, 0, 0, time.UTC),
	}
	assertItem(t, got[0], wantOpen)
	if got[1].ID != "acme/widgets#8" || !got[1].Done || got[1].State != "CLOSED" || got[1].Type != "Issue" ||
		got[1].Parent != "" || got[1].Assignee != "" || got[1].Priority != 4 || len(got[1].BlockedBy) != 0 || len(got[1].Labels) != 0 {
		t.Fatalf("closed = %+v", got[1])
	}
	if got[2].ID != "acme/widgets#11" || got[2].Priority != 5 || got[2].Type != "Issue" {
		t.Fatalf("later = %+v, want priority 5", got[2])
	}
	logText := logBuf.String()
	if strings.Count(logText, "github issue extra assignees") != 1 || !strings.Contains(logText, "bob") || !strings.Contains(logText, "acme/widgets#7") {
		t.Fatalf("log = %q, want one extra-assignee line for bob", logText)
	}
}

func TestSyncSince(t *testing.T) {
	since := time.Date(2026, 10, 29, 12, 12, 0, 0, time.FixedZone("KST", 9*3600))
	wantSince := "since=2026-10-29T03:12:00Z"
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, syncArgs("acme", "widgets", "", since)...).
		WillSucceed(issuesPage(t, false, ""), 0).Once().Build()

	got, err := newTestSource(mock).Sync(context.Background(), since)
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("Sync() = %#v, want none", got)
	}
	calls := mock.Executions()
	if len(calls) != 1 {
		t.Fatalf("calls = %d", len(calls))
	}
	joined := strings.Join(calls[0].Args, "\n")
	if !strings.Contains(joined, wantSince) {
		t.Fatalf("args = %#v, want %s", calls[0].Args, wantSince)
	}
	if strings.Contains(strings.Join(fullSyncArgs(""), "\n"), "since=") {
		t.Fatal("full sync args include since")
	}
}

func TestSyncLabelTruncation(t *testing.T) {
	names := make([]string, maxLabels)
	for i := range names {
		names[i] = "L" + strconv.Itoa(i)
	}
	node := issueNode(12, map[string]any{"labels": labelConn(names...)})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, false, "", node), 0).Once().Build()

	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "acme/widgets#12") || !strings.Contains(err.Error(), "found 50") {
		t.Fatalf("Sync() error = %v, want the issue and 50 labels", err)
	}
}

func TestSyncFortyNineLabels(t *testing.T) {
	names := make([]string, maxLabels-1)
	for i := range names {
		names[i] = "L" + strconv.Itoa(i)
	}
	node := issueNode(13, map[string]any{"labels": labelConn(names...)})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, false, "", node), 0).Once().Build()

	got, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	if len(got) != 1 || len(got[0].Labels) != maxLabels-1 || got[0].Labels[0] != "L0" {
		t.Fatalf("labels = %#v", got[0].Labels)
	}
}

func TestSyncBlockedByTruncation(t *testing.T) {
	node := issueNode(12, map[string]any{
		"blockedBy": blockedConn(true, blockNode("I_block", 1, "acme/other")),
	})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, false, "", node), 0).Once().Build()

	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "acme/widgets#12") || !strings.Contains(err.Error(), "another page") {
		t.Fatalf("Sync() error = %v, want blockedBy truncation for the issue", err)
	}
}

func TestSyncGraphQLError(t *testing.T) {
	body := `{"data":{"repository":{"issues":{"pageInfo":{"hasNextPage":false,"endCursor":null},"nodes":[]}}},"errors":[{"message":"Something went wrong"}]}`
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).WillSucceed(body, 0).Once().Build()
	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "Something went wrong") {
		t.Fatalf("Sync() error = %v", err)
	}
}

func TestSyncRepeatedCursor(t *testing.T) {
	const cursor = "same-cursor"
	body := issuesPage(t, true, cursor, issueNode(1, nil))
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).WillSucceed(body, 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs(cursor)...).WillSucceed(body, 0).Once().Build()
	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), cursor) {
		t.Fatalf("Sync() error = %v, want the repeated cursor", err)
	}
}

func TestSyncEmptyFollowsPage(t *testing.T) {
	body := issuesPage(t, true, "cursor-2")
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).WillSucceed(body, 0).Once().Build()
	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "found none") {
		t.Fatalf("Sync() error = %v", err)
	}
	if len(mock.Executions()) != 1 {
		t.Fatalf("calls = %d, want 1", len(mock.Executions()))
	}
}

func TestSyncGHFailure(t *testing.T) {
	const stderr = "HTTP 401: Bad credentials"
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).WillFail(stderr, 1).Once().Build()
	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), stderr) {
		t.Fatalf("Sync() error = %v", err)
	}
}

func TestSyncMissingBinary(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillError(&executor.ExecutableNotFoundError{Command: testBin}).Once().Build()
	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	var notFound *executor.ExecutableNotFoundError
	if !errors.As(err, &notFound) {
		t.Fatalf("Sync() error = %v, want ExecutableNotFoundError", err)
	}
}

func TestSyncNilResult(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).WillReturn(nil, nil).Once().Build()
	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "found nil") {
		t.Fatalf("Sync() error = %v", err)
	}
}

func TestSyncRejectsBadInput(t *testing.T) {
	mock := newMock()
	mock.SetDefaultBehavior(nil, errors.New("unexpected gh command"))
	src := newTestSource(mock)
	var noCtx context.Context
	if _, err := src.Sync(noCtx, time.Time{}); err == nil || !strings.Contains(err.Error(), "found nil") {
		t.Fatalf("nil context error = %v", err)
	}
	src.cfg.Repo = "widgets"
	if _, err := src.Sync(context.Background(), time.Time{}); err == nil || !strings.Contains(err.Error(), "widgets") {
		t.Fatalf("bad repo error = %v", err)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
	bare := New(config.Source{Repo: "acme/widgets"}, nil, testBin)
	if _, err := bare.Sync(context.Background(), time.Time{}); err == nil || !strings.Contains(err.Error(), "executor") {
		t.Fatalf("nil executor error = %v", err)
	}
	emptyBin := New(testConfig(), mock, "")
	if _, err := emptyBin.Sync(context.Background(), time.Time{}); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty bin error = %v", err)
	}
}

func TestSyncBadState(t *testing.T) {
	node := issueNode(4, map[string]any{"state": "open"})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, false, "", node), 0).Once().Build()
	_, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "acme/widgets#4") || !strings.Contains(err.Error(), `"open"`) {
		t.Fatalf("Sync() error = %v", err)
	}
}

func TestGet(t *testing.T) {
	node := issueNode(7, map[string]any{
		"title":     "Ship the parser",
		"issueType": map[string]any{"name": "Bug"},
		"labels":    labelConn("P1"),
		"assignees": assigneeConn("alice"),
		"parent": map[string]any{
			"number":     3,
			"repository": map[string]any{"nameWithOwner": "acme/widgets"},
		},
		"blockedBy": blockedConn(false, blockNode("I_block", 9, "acme/other")),
	})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 7)...).
		WillSucceed(getPage(t, node), 0).Once().Build()

	got, err := newTestSource(mock).Get(context.Background(), "acme/widgets#7")
	if err != nil {
		t.Fatalf("Get() error = %v", err)
	}
	assertArgs(t, mock, [][]string{getArgs("acme", "widgets", 7)})
	if got.ID != "acme/widgets#7" || got.Type != "Bug" || got.Parent != "acme/widgets#3" ||
		got.Assignee != "alice" || got.Priority != 1 || len(got.BlockedBy) != 1 ||
		got.BlockedBy[0].ID != "acme/other#9" || got.BlockedBy[0].LinkID != "I_block" {
		t.Fatalf("Get() = %+v", got)
	}
	calls := mock.Executions()
	if !slices.Contains(calls[0].Args, "-F") || !slices.Contains(calls[0].Args, "number=7") {
		t.Fatalf("args = %#v, want a typed number", calls[0].Args)
	}
}

func TestGetNotFound(t *testing.T) {
	tests := []struct {
		name string
		body string
	}{
		{name: "null issue", body: `{"data":{"repository":{"issue":null}}}`},
		{
			name: "not found error",
			body: `{"data":{"repository":{"issue":null}},"errors":[{"type":"NOT_FOUND","message":"Could not resolve to an Issue with the number of 99."}]}`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mock := newMock()
			mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 99)...).
				WillSucceed(tc.body, 0).Once().Build()
			_, err := newTestSource(mock).Get(context.Background(), "acme/widgets#99")
			if !errors.Is(err, source.ErrNotFound) {
				t.Fatalf("Get() error = %v, want ErrNotFound", err)
			}
		})
	}
}

func TestGetOtherError(t *testing.T) {
	body := `{"data":{"repository":{"issue":null}},"errors":[{"type":"FORBIDDEN","message":"boom"}]}`
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 99)...).
		WillSucceed(body, 0).Once().Build()
	_, err := newTestSource(mock).Get(context.Background(), "acme/widgets#99")
	if err == nil || errors.Is(err, source.ErrNotFound) || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("Get() error = %v", err)
	}
}

func TestGetRejectsBadID(t *testing.T) {
	mock := newMock()
	mock.SetDefaultBehavior(nil, errors.New("unexpected gh command"))
	src := newTestSource(mock)
	_, err := src.Get(context.Background(), "acme/other#7")
	if err == nil || !strings.Contains(err.Error(), "acme/widgets") || !strings.Contains(err.Error(), "acme/other#7") {
		t.Fatalf("other repo error = %v", err)
	}
	_, err = src.Get(context.Background(), "acme/widgets#0")
	if err == nil || !strings.Contains(err.Error(), "acme/widgets#0") {
		t.Fatalf("bad id error = %v", err)
	}
	var noCtx context.Context
	if _, err := src.Get(noCtx, "acme/widgets#7"); err == nil || !strings.Contains(err.Error(), "found nil") {
		t.Fatalf("nil context error = %v", err)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestMeCached(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, "api", "user", "--jq", ".login").
		WillSucceed("octocat\n", 0).Once().Build()
	src := newTestSource(mock)
	first, err := src.Me(context.Background())
	if err != nil {
		t.Fatalf("Me() error = %v", err)
	}
	second, err := src.Me(context.Background())
	if err != nil {
		t.Fatalf("second Me() error = %v", err)
	}
	if first != "octocat" || second != "octocat" {
		t.Fatalf("Me() = %q then %q", first, second)
	}
	assertArgs(t, mock, [][]string{{"api", "user", "--jq", ".login"}})
}

func TestMeRetriesAfterFailure(t *testing.T) {
	mock := newMock()
	userArgs := []string{"api", "user", "--jq", ".login"}
	mock.ExpectCommandWithArgs(testBin, userArgs...).WillFail("HTTP 401: Bad credentials", 1).Once().Build()
	mock.ExpectCommandWithArgs(testBin, userArgs...).WillSucceed("octocat\n", 0).Once().Build()
	src := newTestSource(mock)
	if _, err := src.Me(context.Background()); err == nil || !strings.Contains(err.Error(), "Bad credentials") {
		t.Fatalf("first Me() error = %v", err)
	}
	login, err := src.Me(context.Background())
	if err != nil || login != "octocat" {
		t.Fatalf("second Me() = %q, %v", login, err)
	}
	if len(mock.Executions()) != 2 {
		t.Fatalf("calls = %d, want 2", len(mock.Executions()))
	}
}

func TestMeRejectsBadLogin(t *testing.T) {
	tests := []struct {
		name   string
		output string
		want   string
	}{
		{name: "empty", output: "\n", want: "found empty"},
		{name: "quoted", output: `"octocat"`, want: "expected a login"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mock := newMock()
			mock.ExpectCommandWithArgs(testBin, "api", "user", "--jq", ".login").
				WillSucceed(tc.output, 0).Once().Build()
			_, err := newTestSource(mock).Me(context.Background())
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Me() error = %v, want %s", err, tc.want)
			}
		})
	}
}

func TestName(t *testing.T) {
	src := New(testConfig(), nil, "")
	if got := src.Name(); got != "github:experimental" {
		t.Fatalf("Name() = %q", got)
	}
}

func testConfig() config.Source {
	return config.Source{
		Name:           "github:experimental",
		Kind:           "github",
		Repo:           "acme/widgets",
		CreateAssignee: config.CreateAssigneeMe,
		PriorityLabels: map[string]int{
			"P0":    0,
			"P1":    1,
			"later": 5,
		},
	}
}

func newTestSource(exec executor.Executor) *Source {
	return New(testConfig(), exec, testBin)
}

func fullSyncArgs(after string) []string {
	return syncArgs("acme", "widgets", after, time.Time{})
}

func newMock() *executor.MockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testBin, true)
	mock.SetDefaultBehavior(nil, errors.New("unexpected gh command"))
	return mock
}

func assertArgs(t *testing.T, mock *executor.MockExecutor, want [][]string) {
	t.Helper()
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	calls := mock.Executions()
	if len(calls) != len(want) {
		t.Fatalf("calls = %d, want %d", len(calls), len(want))
	}
	for i, args := range want {
		if calls[i].Command != testBin {
			t.Fatalf("call %d command = %q", i, calls[i].Command)
		}
		if !slices.Equal(calls[i].Args, args) {
			t.Fatalf("call %d args = %#v\nwant %#v", i, calls[i].Args, args)
		}
		if calls[i].Timeout != callTimeout {
			t.Fatalf("call %d timeout = %s, want %s", i, calls[i].Timeout, callTimeout)
		}
	}
}

func assertItem(t *testing.T, got, want source.Item) {
	t.Helper()
	if got.Source != want.Source || got.ID != want.ID || got.Title != want.Title || got.URL != want.URL ||
		got.Type != want.Type || got.State != want.State || got.Done != want.Done || got.Parent != want.Parent ||
		got.Assignee != want.Assignee || got.Priority != want.Priority ||
		!got.Created.Equal(want.Created) || !got.Updated.Equal(want.Updated) ||
		!sameDue(got.Due, want.Due) ||
		!slices.Equal(got.Labels, want.Labels) || !slices.Equal(got.BlockedBy, want.BlockedBy) {
		t.Fatalf("item = %+v\nwant %+v", got, want)
	}
}

func sameDue(got, want *time.Time) bool {
	if got == nil || want == nil {
		return got == nil && want == nil
	}
	return got.Equal(*want)
}

func issuesPage(t *testing.T, hasNext bool, cursor string, nodes ...any) string {
	t.Helper()
	var end any
	if hasNext {
		end = cursor
	}
	return mustJSON(t, map[string]any{
		"data": map[string]any{
			"repository": map[string]any{
				"issues": map[string]any{
					"pageInfo": map[string]any{"hasNextPage": hasNext, "endCursor": end},
					"nodes":    nodeList(nodes...),
				},
			},
		},
	})
}

func getPage(t *testing.T, issue any) string {
	t.Helper()
	return mustJSON(t, map[string]any{
		"data": map[string]any{
			"repository": map[string]any{"issue": issue},
		},
	})
}

func issueNode(number int, extra map[string]any) map[string]any {
	node := map[string]any{
		"id":        "I_" + strconv.Itoa(number),
		"number":    number,
		"title":     "Issue " + strconv.Itoa(number),
		"url":       "https://github.com/acme/widgets/issues/" + strconv.Itoa(number),
		"state":     "OPEN",
		"createdAt": "2026-10-01T00:00:00Z",
		"updatedAt": "2026-10-02T00:00:00Z",
		"issueType": nil,
		"labels":    labelConn(),
		"assignees": assigneeConn(),
		"parent":    nil,
		"blockedBy": blockedConn(false),
	}
	for key, value := range extra {
		node[key] = value
	}
	return node
}

func labelConn(names ...string) map[string]any {
	nodes := make([]any, len(names))
	for i, name := range names {
		nodes[i] = map[string]string{"name": name}
	}
	return map[string]any{"nodes": nodes}
}

func assigneeConn(logins ...string) map[string]any {
	nodes := make([]any, len(logins))
	for i, login := range logins {
		nodes[i] = map[string]string{"login": login}
	}
	return map[string]any{"nodes": nodes}
}

func blockedConn(hasNext bool, nodes ...any) map[string]any {
	return map[string]any{
		"pageInfo": map[string]any{"hasNextPage": hasNext},
		"nodes":    nodeList(nodes...),
	}
}

func blockNode(id string, number int, repo string) map[string]any {
	return map[string]any{
		"id":         id,
		"number":     number,
		"repository": map[string]any{"nameWithOwner": repo},
	}
}

func nodeList(nodes ...any) []any {
	if len(nodes) == 0 {
		return []any{}
	}
	return nodes
}

func mustJSON(t *testing.T, value any) string {
	t.Helper()
	raw, err := json.Marshal(value)
	if err != nil {
		t.Fatal(err)
	}
	return string(raw)
}

func TestMapDue(t *testing.T) {
	cfg := testConfig()
	nov7 := time.Date(2026, 11, 7, 0, 0, 0, 0, time.UTC)
	nov8 := time.Date(2026, 11, 8, 0, 0, 0, 0, time.UTC)
	tests := []struct {
		name      string
		milestone any
		omit      bool
		want      *time.Time
		wantErr   string
	}{
		{name: "absent", omit: true},
		{name: "null milestone", milestone: nil},
		{name: "no dueOn", milestone: map[string]any{}},
		{name: "null dueOn", milestone: map[string]any{"dueOn": nil}},
		{name: "utc", milestone: map[string]any{"dueOn": "2026-11-07T00:00:00Z"}, want: &nov7},
		{name: "offset crosses utc date", milestone: map[string]any{"dueOn": "2026-11-07T23:30:00-05:00"}, want: &nov8},
		{name: "date only", milestone: map[string]any{"dueOn": "2026-11-07"}, wantErr: `expected milestone.dueOn RFC3339 timestamp, found "2026-11-07"`},
		{name: "not an object", milestone: "soon", wantErr: "expected milestone object"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			extra := map[string]any{}
			if !tc.omit {
				extra["milestone"] = tc.milestone
			}
			raw := json.RawMessage(mustJSON(t, issueNode(7, extra)))
			got, err := mapNode(&cfg, raw)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("mapNode() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("mapNode() error = %v", err)
			}
			if !sameDue(got.Due, tc.want) {
				t.Fatalf("Due = %v, want %v", got.Due, tc.want)
			}
		})
	}
}

func TestMapTrustFields(t *testing.T) {
	cfg := testConfig()
	tests := []struct {
		name      string
		extra     map[string]any
		wantAuth  string
		wantAssoc string
		wantLock  bool
		wantErr   string
	}{
		{
			name: "owner locked",
			extra: map[string]any{
				"author":            map[string]any{"login": "jaeyeom"},
				"authorAssociation": "OWNER",
				"locked":            true,
			},
			wantAuth:  "jaeyeom",
			wantAssoc: "OWNER",
			wantLock:  true,
		},
		{
			name: "deleted user null author",
			extra: map[string]any{
				"author":            nil,
				"authorAssociation": "NONE",
				"locked":            false,
			},
			wantAuth:  "",
			wantAssoc: "NONE",
			wantLock:  false,
		},
		{
			name: "author with null login",
			extra: map[string]any{
				"author":            map[string]any{"login": nil},
				"authorAssociation": "CONTRIBUTOR",
				"locked":            false,
			},
			wantAuth:  "",
			wantAssoc: "CONTRIBUTOR",
			wantLock:  false,
		},
		{
			name:    "invalid author type",
			extra:   map[string]any{"author": "alice"},
			wantErr: "expected github issue acme/widgets#7: author object",
		},
		{
			name:    "invalid authorAssociation type",
			extra:   map[string]any{"authorAssociation": 123},
			wantErr: "expected authorAssociation string",
		},
		{
			name:    "invalid locked type",
			extra:   map[string]any{"locked": "yes"},
			wantErr: "expected locked bool",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			raw := json.RawMessage(mustJSON(t, issueNode(7, tc.extra)))
			got, err := mapNode(&cfg, raw)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("mapNode() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("mapNode() error = %v", err)
			}
			if got.Author != tc.wantAuth || got.AuthorAssociation != tc.wantAssoc || got.Locked != tc.wantLock {
				t.Fatalf("got author=%q assoc=%q locked=%v, want %q %q %v",
					got.Author, got.AuthorAssociation, got.Locked, tc.wantAuth, tc.wantAssoc, tc.wantLock)
			}
		})
	}
}
