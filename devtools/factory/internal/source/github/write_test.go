package github

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestWriteQueries(t *testing.T) {
	checks := []struct {
		name  string
		query string
		want  []string
	}{
		{
			name:  "addBlockedBy",
			query: addBlockedByQuery,
			want:  []string{"AddBlockedByInput", "addBlockedBy(input:$input)", "issue{id}", "blockingIssue{id}"},
		},
		{
			name:  "removeBlockedBy",
			query: removeBlockedByQuery,
			want:  []string{"RemoveBlockedByInput", "removeBlockedBy(input:$input)", "issue{id}", "blockingIssue{id}"},
		},
		{
			name:  "addSubIssue",
			query: addSubIssueQuery,
			want:  []string{"AddSubIssueInput", "addSubIssue(input:$input)", "issue{id}", "subIssue{id}"},
		},
	}
	for _, tc := range checks {
		t.Run(tc.name, func(t *testing.T) {
			if strings.Count(tc.query, "{") != strings.Count(tc.query, "}") {
				t.Fatalf("braces unbalanced: %s", tc.query)
			}
			for _, part := range tc.want {
				if !strings.Contains(tc.query, part) {
					t.Fatalf("query %s missing %s", tc.query, part)
				}
			}
		})
	}
}

func TestSetPriority(t *testing.T) {
	mock := newMock()
	src := newTestSource(mock)
	args := src.editArgs(7, "--add-label", "P0")
	mock.ExpectCommandWithArgs(testBin, args...).WillSucceed("", 0).Once().Build()
	if err := src.SetPriority(context.Background(), "acme/widgets#7", 0); err != nil {
		t.Fatal(err)
	}
	assertArgs(t, mock, [][]string{args})
}

func TestSetPriorityRejectsUnnamedRank(t *testing.T) {
	src := newTestSource(nil)
	err := src.SetPriority(context.Background(), "acme/widgets#7", 2)
	if err == nil || !strings.Contains(err.Error(), "expected one name for rank 2") || !strings.Contains(err.Error(), "found none") {
		t.Fatalf("error = %v", err)
	}
}

func TestClaim(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, claimArgs(7)...).WillSucceed("", 0).Once().Build()
	if err := claimSource(mock).Claim(context.Background(), "acme/widgets#7"); err != nil {
		t.Fatalf("Claim() error = %v", err)
	}
	assertArgs(t, mock, [][]string{claimArgs(7)})
}

func TestClaimMissingLabel(t *testing.T) {
	tests := []struct {
		name   string
		stderr string
	}{
		{name: "single quotes", stderr: "could not add label: 'in-progress' not found"},
		{name: "double quotes", stderr: `could not add label: "in-progress" not found`},
	}
	const want = `create the "in-progress" label in acme/widgets first`
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mock := newMock()
			mock.ExpectCommandWithArgs(testBin, claimArgs(7)...).WillFail(tc.stderr, 1).Once().Build()
			err := claimSource(mock).Claim(context.Background(), "acme/widgets#7")
			if err == nil || !strings.Contains(err.Error(), want) {
				t.Fatalf("Claim() error = %v, want %s", err, want)
			}
			assertArgs(t, mock, [][]string{claimArgs(7)})
		})
	}
}

func TestClaimRejects(t *testing.T) {
	t.Run("nil context", func(t *testing.T) {
		mock := newMock()
		var ctx context.Context
		if err := claimSource(mock).Claim(ctx, "acme/widgets#7"); err == nil || !strings.Contains(err.Error(), "found nil") {
			t.Fatalf("Claim() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("empty label", func(t *testing.T) {
		mock := newMock()
		err := newTestSource(mock).Claim(context.Background(), "acme/widgets#7")
		if err == nil || !strings.Contains(err.Error(), "found empty") {
			t.Fatalf("Claim() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("bad id", func(t *testing.T) {
		mock := newMock()
		err := claimSource(mock).Claim(context.Background(), "acme/widgets#0")
		if err == nil || !strings.Contains(err.Error(), "acme/widgets#0") {
			t.Fatalf("Claim() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("other repo", func(t *testing.T) {
		mock := newMock()
		err := claimSource(mock).Claim(context.Background(), "acme/other#7")
		if err == nil || !strings.Contains(err.Error(), "acme/widgets") || !strings.Contains(err.Error(), "acme/other#7") {
			t.Fatalf("Claim() error = %v", err)
		}
	})
	t.Run("other gh error", func(t *testing.T) {
		mock := newMock()
		mock.ExpectCommandWithArgs(testBin, claimArgs(7)...).
			WillFail("HTTP 404: Not Found", 1).Once().Build()
		err := claimSource(mock).Claim(context.Background(), "acme/widgets#7")
		if err == nil || !strings.Contains(err.Error(), "404") || strings.Contains(err.Error(), "create the") {
			t.Fatalf("Claim() error = %v", err)
		}
	})
}

func TestRelease(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, releaseArgs(7)...).WillSucceed("", 0).Once().Build()
	if err := claimSource(mock).Release(context.Background(), "acme/widgets#7"); err != nil {
		t.Fatalf("Release() error = %v", err)
	}
	assertArgs(t, mock, [][]string{releaseArgs(7)})
}

func TestReleaseRejects(t *testing.T) {
	t.Run("nil context", func(t *testing.T) {
		mock := newMock()
		var ctx context.Context
		if err := claimSource(mock).Release(ctx, "acme/widgets#7"); err == nil || !strings.Contains(err.Error(), "found nil") {
			t.Fatalf("Release() error = %v", err)
		}
	})
	t.Run("empty label", func(t *testing.T) {
		err := newTestSource(newMock()).Release(context.Background(), "acme/widgets#7")
		if err == nil || !strings.Contains(err.Error(), "found empty") {
			t.Fatalf("Release() error = %v", err)
		}
	})
	t.Run("gh error is not a missing label", func(t *testing.T) {
		mock := newMock()
		mock.ExpectCommandWithArgs(testBin, releaseArgs(7)...).
			WillFail("could not add label: 'in-progress' not found", 1).Once().Build()
		err := claimSource(mock).Release(context.Background(), "acme/widgets#7")
		if err == nil || !strings.Contains(err.Error(), "could not add label") || strings.Contains(err.Error(), "create the") {
			t.Fatalf("Release() error = %v", err)
		}
	})
}

func TestFinish(t *testing.T) {
	mock := newMock()
	src := newTestSource(mock)
	if err := src.Finish(context.Background(), "not-an-id"); err != nil {
		t.Fatalf("Finish() error = %v", err)
	}
	var ctx context.Context
	if err := src.Finish(ctx, ""); err != nil {
		t.Fatalf("Finish(nil) error = %v", err)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestClose(t *testing.T) {
	t.Run("with comment", func(t *testing.T) {
		mock := newMock()
		want := []string{"issue", "close", "7", "-R", "acme/widgets", "--comment", "All child issues merged."}
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		if err := newTestSource(mock).Close(context.Background(), "acme/widgets#7", "All child issues merged."); err != nil {
			t.Fatalf("Close() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})
	t.Run("without comment", func(t *testing.T) {
		for _, tc := range []struct {
			name    string
			comment string
		}{
			{name: "empty", comment: ""},
			{name: "whitespace", comment: "  \n\t  "},
		} {
			t.Run(tc.name, func(t *testing.T) {
				mock := newMock()
				want := []string{"issue", "close", "7", "-R", "acme/widgets"}
				mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
				if err := newTestSource(mock).Close(context.Background(), "acme/widgets#7", tc.comment); err != nil {
					t.Fatalf("Close() error = %v", err)
				}
				assertArgs(t, mock, [][]string{want})
			})
		}
	})
}

func TestCloseRejects(t *testing.T) {
	t.Run("nil context", func(t *testing.T) {
		mock := newMock()
		var ctx context.Context
		if err := newTestSource(mock).Close(ctx, "acme/widgets#7", "done"); err == nil || !strings.Contains(err.Error(), "found nil") {
			t.Fatalf("Close() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("empty id", func(t *testing.T) {
		mock := newMock()
		if err := newTestSource(mock).Close(context.Background(), "", "done"); err == nil || !strings.Contains(err.Error(), "expected") {
			t.Fatalf("Close() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("other repo", func(t *testing.T) {
		mock := newMock()
		err := newTestSource(mock).Close(context.Background(), "acme/other#7", "done")
		if err == nil || !strings.Contains(err.Error(), "acme/widgets") || !strings.Contains(err.Error(), "acme/other#7") {
			t.Fatalf("Close() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("gh failure", func(t *testing.T) {
		mock := newMock()
		want := []string{"issue", "close", "7", "-R", "acme/widgets"}
		mock.ExpectCommandWithArgs(testBin, want...).WillFail("HTTP 404: Not Found", 1).Once().Build()
		if err := newTestSource(mock).Close(context.Background(), "acme/widgets#7", ""); err == nil || !strings.Contains(err.Error(), "404") {
			t.Fatalf("Close() error = %v", err)
		}
	})
}

func TestLink(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 1)...).
		WillSucceed(getPage(t, issueNode(1, nil)), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 2)...).
		WillSucceed(getPage(t, issueNode(2, nil)), 0).Once().Build()
	mutation := blockedByArgs(addBlockedByQuery, "I_2", "I_1")
	mock.ExpectCommandWithArgs(testBin, mutation...).
		WillSucceed(idPayload(t, "addBlockedBy", "issue", "I_2", "blockingIssue", "I_1"), 0).Once().Build()

	err := newTestSource(mock).Link(context.Background(), "acme/widgets#1", "acme/widgets#2")
	if err != nil {
		t.Fatalf("Link() error = %v", err)
	}
	assertArgs(t, mock, [][]string{
		getArgs("acme", "widgets", 1),
		getArgs("acme", "widgets", 2),
		mutation,
	})
	joined := strings.Join(mutation, "\n")
	if strings.Contains(joined, "clientMutationId") {
		t.Fatalf("args include clientMutationId: %#v", mutation)
	}
}

func TestLinkRejects(t *testing.T) {
	t.Run("nil context", func(t *testing.T) {
		var ctx context.Context
		err := newTestSource(newMock()).Link(ctx, "acme/widgets#1", "acme/widgets#2")
		if err == nil || !strings.Contains(err.Error(), "found nil") {
			t.Fatalf("Link() error = %v", err)
		}
	})
	t.Run("other repo", func(t *testing.T) {
		mock := newMock()
		err := newTestSource(mock).Link(context.Background(), "acme/other#9", "acme/widgets#2")
		if err == nil || !strings.Contains(err.Error(), "acme/widgets") || !strings.Contains(err.Error(), "acme/other#9") {
			t.Fatalf("Link() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
}

func TestLinkGraphQLError(t *testing.T) {
	body := `{"data":{"addBlockedBy":null},"errors":[{"message":"already blocked"}]}`
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 1)...).
		WillSucceed(getPage(t, issueNode(1, nil)), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 2)...).
		WillSucceed(getPage(t, issueNode(2, nil)), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, blockedByArgs(addBlockedByQuery, "I_2", "I_1")...).
		WillSucceed(body, 0).Once().Build()
	err := newTestSource(mock).Link(context.Background(), "acme/widgets#1", "acme/widgets#2")
	if err == nil || errors.Is(err, source.ErrNotFound) || !strings.Contains(err.Error(), "already blocked") {
		t.Fatalf("Link() error = %v", err)
	}
}

func TestUnlink(t *testing.T) {
	blocked := issueNode(2, map[string]any{
		"blockedBy": blockedConn(false, blockNode("I_1", 1, "acme/widgets")),
	})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 2)...).
		WillSucceed(getPage(t, blocked), 0).Once().Build()
	mutation := blockedByArgs(removeBlockedByQuery, "I_2", "I_1")
	mock.ExpectCommandWithArgs(testBin, mutation...).
		WillSucceed(idPayload(t, "removeBlockedBy", "issue", "I_2", "blockingIssue", "I_1"), 0).Once().Build()

	err := newTestSource(mock).Unlink(context.Background(), "acme/widgets#1", "acme/widgets#2")
	if err != nil {
		t.Fatalf("Unlink() error = %v", err)
	}
	assertArgs(t, mock, [][]string{getArgs("acme", "widgets", 2), mutation})
}

func TestUnlinkNotFound(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 2)...).
		WillSucceed(getPage(t, issueNode(2, nil)), 0).Once().Build()
	err := newTestSource(mock).Unlink(context.Background(), "acme/widgets#1", "acme/widgets#2")
	if !errors.Is(err, source.ErrNotFound) || !strings.Contains(err.Error(), "does not block") {
		t.Fatalf("Unlink() error = %v, want ErrNotFound", err)
	}
	assertArgs(t, mock, [][]string{getArgs("acme", "widgets", 2)})
}

func TestUnlinkCrossRepo(t *testing.T) {
	blocked := issueNode(2, map[string]any{
		"blockedBy": blockedConn(false, blockNode("I_other", 9, "acme/other")),
	})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 2)...).
		WillSucceed(getPage(t, blocked), 0).Once().Build()
	mutation := blockedByArgs(removeBlockedByQuery, "I_2", "I_other")
	mock.ExpectCommandWithArgs(testBin, mutation...).
		WillSucceed(idPayload(t, "removeBlockedBy", "issue", "I_2", "blockingIssue", "I_other"), 0).Once().Build()
	err := newTestSource(mock).Unlink(context.Background(), "acme/other#9", "acme/widgets#2")
	if err != nil {
		t.Fatalf("Unlink() error = %v", err)
	}
	assertArgs(t, mock, [][]string{getArgs("acme", "widgets", 2), mutation})
}

func TestCreate(t *testing.T) {
	req := source.CreateRequest{Title: "Ship it", Body: "", Labels: []string{"bug", "agent-ready"}}
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, mustCreateArgs(t, req)...).
		WillSucceed("https://github.com/acme/widgets/issues/12\n", 0).Once().Build()
	id, err := newTestSource(mock).Create(context.Background(), req)
	if err != nil || id != "acme/widgets#12" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	assertArgs(t, mock, [][]string{mustCreateArgs(t, req)})
}

func TestCreateParentAndLinks(t *testing.T) {
	req := source.CreateRequest{
		Title:     "Ship it",
		Body:      "the body",
		Parent:    "acme/widgets#3",
		Blocks:    []string{"acme/widgets#4"},
		BlockedBy: []string{"acme/widgets#5"},
	}
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, mustCreateArgs(t, req)...).
		WillSucceed("https://github.com/acme/widgets/issues/12\n", 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 12)...).
		WillSucceed(getPage(t, issueNode(12, nil)), 0).Times(3).Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 3)...).
		WillSucceed(getPage(t, issueNode(3, nil)), 0).Once().Build()
	parentMutation := addSubIssueArgs("I_3", "I_12")
	mock.ExpectCommandWithArgs(testBin, parentMutation...).
		WillSucceed(idPayload(t, "addSubIssue", "issue", "I_3", "subIssue", "I_12"), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 4)...).
		WillSucceed(getPage(t, issueNode(4, nil)), 0).Once().Build()
	blocksMutation := blockedByArgs(addBlockedByQuery, "I_4", "I_12")
	mock.ExpectCommandWithArgs(testBin, blocksMutation...).
		WillSucceed(idPayload(t, "addBlockedBy", "issue", "I_4", "blockingIssue", "I_12"), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 5)...).
		WillSucceed(getPage(t, issueNode(5, nil)), 0).Once().Build()
	blockedByMutation := blockedByArgs(addBlockedByQuery, "I_12", "I_5")
	mock.ExpectCommandWithArgs(testBin, blockedByMutation...).
		WillSucceed(idPayload(t, "addBlockedBy", "issue", "I_12", "blockingIssue", "I_5"), 0).Once().Build()

	id, err := newTestSource(mock).Create(context.Background(), req)
	if err != nil || id != "acme/widgets#12" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	assertArgs(t, mock, [][]string{
		mustCreateArgs(t, req),
		getArgs("acme", "widgets", 12),
		getArgs("acme", "widgets", 3),
		parentMutation,
		getArgs("acme", "widgets", 12),
		getArgs("acme", "widgets", 4),
		blocksMutation,
		getArgs("acme", "widgets", 5),
		getArgs("acme", "widgets", 12),
		blockedByMutation,
	})
}

func TestCreateParentFails(t *testing.T) {
	req := source.CreateRequest{Title: "Ship it", Body: "the body", Parent: "acme/widgets#3"}
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, mustCreateArgs(t, req)...).
		WillSucceed("https://github.com/acme/widgets/issues/12\n", 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 12)...).
		WillSucceed(getPage(t, issueNode(12, nil)), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 3)...).
		WillSucceed(getPage(t, issueNode(3, nil)), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, addSubIssueArgs("I_3", "I_12")...).
		WillFail("GraphQL: parent boom", 1).Once().Build()

	id, err := newTestSource(mock).Create(context.Background(), req)
	if id != "acme/widgets#12" || err == nil || !strings.Contains(err.Error(), "exists") || !strings.Contains(err.Error(), "parent boom") {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestCreateMissingLinks(t *testing.T) {
	req := source.CreateRequest{
		Title:  "Ship it",
		Body:   "the body",
		Blocks: []string{"acme/widgets#4"},
	}
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, mustCreateArgs(t, req)...).
		WillSucceed("https://github.com/acme/widgets/issues/12\n", 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 12)...).
		WillSucceed(getPage(t, issueNode(12, nil)), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, getArgs("acme", "widgets", 4)...).
		WillSucceed(getPage(t, issueNode(4, nil)), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, blockedByArgs(addBlockedByQuery, "I_4", "I_12")...).
		WillFail("GraphQL: link boom", 1).Once().Build()

	id, err := newTestSource(mock).Create(context.Background(), req)
	if id != "acme/widgets#12" || err == nil || !strings.Contains(err.Error(), "exists but is missing links") || !strings.Contains(err.Error(), "link boom") {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	if errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Create() error = %v, want a partial failure, not ErrNotFound", err)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestCreateRejects(t *testing.T) {
	t.Run("nil context", func(t *testing.T) {
		var ctx context.Context
		id, err := newTestSource(newMock()).Create(ctx, source.CreateRequest{Title: "Ship it"})
		if id != "" || err == nil || !strings.Contains(err.Error(), "found nil") {
			t.Fatalf("Create() = %q, %v", id, err)
		}
	})
	t.Run("empty title", func(t *testing.T) {
		mock := newMock()
		id, err := newTestSource(mock).Create(context.Background(), source.CreateRequest{Body: "x"})
		if id != "" || err == nil || !strings.Contains(err.Error(), "found empty") {
			t.Fatalf("Create() = %q, %v", id, err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("empty label", func(t *testing.T) {
		mock := newMock()
		id, err := newTestSource(mock).Create(context.Background(), source.CreateRequest{
			Title:  "Ship it",
			Labels: []string{"bug", ""},
		})
		if id != "" || err == nil || !strings.Contains(err.Error(), "found empty") {
			t.Fatalf("Create() = %q, %v", id, err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("bad url", func(t *testing.T) {
		req := source.CreateRequest{Title: "Ship it", Body: "the body"}
		mock := newMock()
		mock.ExpectCommandWithArgs(testBin, mustCreateArgs(t, req)...).
			WillSucceed("not a url\n", 0).Once().Build()
		id, err := newTestSource(mock).Create(context.Background(), req)
		if id != "" || err == nil || !strings.Contains(err.Error(), "not a url") {
			t.Fatalf("Create() = %q, %v", id, err)
		}
	})
	t.Run("gh failure", func(t *testing.T) {
		req := source.CreateRequest{Title: "Ship it", Body: "the body"}
		mock := newMock()
		mock.ExpectCommandWithArgs(testBin, mustCreateArgs(t, req)...).
			WillFail("HTTP 401: Bad credentials", 1).Once().Build()
		id, err := newTestSource(mock).Create(context.Background(), req)
		if id != "" || err == nil || !strings.Contains(err.Error(), "Bad credentials") {
			t.Fatalf("Create() = %q, %v", id, err)
		}
	})
}

func claimSource(exec executor.Executor) *Source {
	src := newTestSource(exec)
	src.cfg.ClaimLabel = "in-progress"
	return src
}

func claimArgs(number int) []string {
	return claimSource(nil).editArgs(number, "--add-label", "in-progress", "--add-assignee", "@me")
}

func releaseArgs(number int) []string {
	return claimSource(nil).editArgs(number, "--remove-label", "in-progress", "--remove-assignee", "@me")
}

func idPayload(t *testing.T, field, leftKey, leftID, rightKey, rightID string) string {
	t.Helper()
	return mustJSON(t, map[string]any{
		"data": map[string]any{
			field: map[string]any{
				leftKey:  map[string]string{"id": leftID},
				rightKey: map[string]string{"id": rightID},
			},
		},
	})
}

func mustCreateArgs(t *testing.T, req source.CreateRequest) []string {
	t.Helper()
	args, err := createArgs("acme/widgets", config.CreateAssigneeMe, req)
	if err != nil {
		t.Fatal(err)
	}
	return args
}

func TestCreateArgsAssignee(t *testing.T) {
	req := source.CreateRequest{Title: "Ship it", Labels: []string{"bug"}}
	got, err := createArgs("acme/widgets", config.CreateAssigneeMe, req)
	want := []string{"issue", "create", "-R", "acme/widgets", "--title", "Ship it", "--body", "", "--label", "bug", "--assignee", "@me"}
	if err != nil || !slices.Equal(got, want) {
		t.Fatalf("createArgs(me) = %q, %v, want %q", got, err, want)
	}
	got, err = createArgs("acme/widgets", config.CreateAssigneeUnassigned, req)
	if err != nil || slices.Contains(got, "--assignee") {
		t.Fatalf("createArgs(unassigned) = %q, %v, want no --assignee", got, err)
	}
	if _, err := createArgs("acme/widgets", "", req); err == nil || !strings.Contains(err.Error(), "create_assignee") {
		t.Fatalf("createArgs(empty) error = %v", err)
	}
}
