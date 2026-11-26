package snapshot_test

import (
	"context"
	"database/sql"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/snapshot"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	testHerdr = "herdr"
	testGH    = "gh"

	workspacesJSON = `{
	  "id": "cli:workspace:list",
	  "result": {
	    "type": "workspace_list",
	    "workspaces": [
	      {"workspace_id": "w1", "label": "factory", "agent_status": "idle"}
	    ]
	  }
	}`
	tabsJSON = `{
	  "id": "cli:tab:list",
	  "result": {
	    "type": "tab_list",
	    "tabs": [
	      {"tab_id": "t1", "workspace_id": "w1", "label": "PRJ-1", "agent_status": "idle", "pane_count": 1}
	    ]
	  }
	}`
	prsJSON = `{
	  "data": {
	    "search": {
	      "pageInfo": {"hasNextPage": false, "endCursor": null},
	      "nodes": [{
	        "number": 7,
	        "title": "[PRJ-1] work",
	        "url": "https://github.com/acme/widgets/pull/7",
	        "isDraft": false,
	        "merged": false,
	        "mergedAt": null,
	        "createdAt": "2026-01-01T00:00:00Z",
	        "closedAt": null,
	        "headRefName": "feature",
	        "repository": {"nameWithOwner": "acme/widgets"},
	        "closingIssuesReferences": {"nodes": []}
	      }]
	    }
	  }
	}`
)

func TestBuildLoadsMirrorClaimsTabsAndPRs(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	item := source.Item{
		Source: "jira:acme", ID: "PRJ-1", Title: "Ship it", URL: "https://example.test/PRJ-1",
		Type: "Task", State: "To Do", Parent: "EPIC-A", Labels: []string{"agent-ready"},
		Assignee: "alice", Priority: 2, Created: updated, Updated: updated,
		BlockedBy: []source.BlockRef{{ID: "PRJ-9", LinkID: "l1"}},
	}
	if err := st.UpsertItems(ctx, "jira:acme", []source.Item{item}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	other := item
	other.Source = "jira:cub"
	other.ID = "CUB-1"
	other.BlockedBy = nil
	if err := st.UpsertItems(ctx, "jira:cub", []source.Item{other}); err != nil {
		t.Fatalf("UpsertItems cub: %v", err)
	}
	claimedAt := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	if err := st.InsertClaim(ctx, store.Claim{
		Source: "jira:acme", ID: "PRJ-1", ClaimedAt: claimedAt, Machine: "laptop",
		State: store.Started, UpdatedAt: claimedAt,
	}); err != nil {
		t.Fatalf("InsertClaim: %v", err)
	}
	if err := st.InsertClaim(ctx, store.Claim{
		Source: "jira:cub", ID: "CUB-1", ClaimedAt: claimedAt, State: store.Claimed, UpdatedAt: claimedAt,
	}); err != nil {
		t.Fatalf("InsertClaim cub: %v", err)
	}
	if err := st.InsertClaim(ctx, store.Claim{
		Source: "jira:acme", ID: "PRJ-9", ClaimedAt: claimedAt, State: store.Released, UpdatedAt: claimedAt,
	}); err != nil {
		t.Fatalf("InsertClaim released: %v", err)
	}
	wantApproval := store.Approval{
		Source:      "jira:acme",
		ItemID:      "PRJ-1",
		ContentHash: source.ContentHash("Ship it", "body"),
		ApprovedAt:  claimedAt,
	}
	if err := st.SetApproval(ctx, wantApproval); err != nil {
		t.Fatalf("SetApproval: %v", err)
	}

	got, err := snapshot.Build(ctx, st, herdr.NewClient(scriptOK(t), testHerdr), gh.NewClient(scriptOK(t), testGH), "jira:acme", "alice")
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if got.Me != "alice" || len(got.Items) != 1 || got.Items["PRJ-1"].Title != "Ship it" {
		t.Fatalf("items = %+v me = %q", got.Items, got.Me)
	}
	if len(got.Items["PRJ-1"].BlockedBy) != 1 || got.Items["PRJ-1"].BlockedBy[0].ID != "PRJ-9" {
		t.Fatalf("blocked by = %+v", got.Items["PRJ-1"].BlockedBy)
	}
	if len(got.Claims) != 1 || got.Claims["PRJ-1"] != store.Started {
		t.Fatalf("claims = %+v", got.Claims)
	}
	if _, ok := got.Claims["PRJ-9"]; ok {
		t.Fatalf("released claim was included: %+v", got.Claims)
	}
	if len(got.Workspaces) != 1 || got.Workspaces[0].Label != "factory" {
		t.Fatalf("workspaces = %+v", got.Workspaces)
	}
	if len(got.Tabs) != 1 || got.Tabs[0].Label != "PRJ-1" {
		t.Fatalf("tabs = %+v", got.Tabs)
	}
	if len(got.OpenPRs) != 1 || got.OpenPRs[0].Number != 7 {
		t.Fatalf("prs = %+v", got.OpenPRs)
	}
	if len(got.Approvals) != 1 || got.Approvals["PRJ-1"].ContentHash != wantApproval.ContentHash {
		t.Fatalf("approvals = %+v, want %+v", got.Approvals, wantApproval)
	}
}

func TestBuildHerdrError(t *testing.T) {
	st := openStore(t)
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs(testHerdr, "workspace", "list").WillFail("herdr down", 1).Build()
	_, err := snapshot.Build(context.Background(), st, herdr.NewClient(mock, testHerdr), gh.NewClient(mock, testGH), "jira:acme", "alice")
	if err == nil || !strings.Contains(err.Error(), "herdr down") {
		t.Fatalf("Build error = %v", err)
	}
}

func TestBuildGHError(t *testing.T) {
	st := openStore(t)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs(testHerdr, "workspace", "list").WillSucceed(workspacesJSON, 0).Build()
	mock.ExpectCommandWithArgs(testHerdr, "tab", "list").WillSucceed(tabsJSON, 0).Build()
	mock.ExpectCommand(testGH).WillFail("gh down", 1).Build()
	_, err := snapshot.Build(context.Background(), st, herdr.NewClient(mock, testHerdr), gh.NewClient(mock, testGH), "jira:acme", "alice")
	if err == nil || !strings.Contains(err.Error(), "gh down") {
		t.Fatalf("Build error = %v", err)
	}
}

func TestBuildRejectsNilAndEmpty(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	h := herdr.NewClient(executor.NewMockExecutor(), testHerdr)
	g := gh.NewClient(executor.NewMockExecutor(), testGH)
	tests := []struct {
		name   string
		ctx    context.Context
		st     *store.Store
		herdr  *herdr.Client
		gh     *gh.Client
		source string
		me     string
		want   string
	}{
		{name: "nil context", st: st, herdr: h, gh: g, source: "jira:acme", me: "alice", want: "found nil"},
		{name: "nil store", ctx: ctx, herdr: h, gh: g, source: "jira:acme", me: "alice", want: "found nil"},
		{name: "nil herdr", ctx: ctx, st: st, gh: g, source: "jira:acme", me: "alice", want: "herdr client"},
		{name: "nil gh", ctx: ctx, st: st, herdr: h, source: "jira:acme", me: "alice", want: "gh client"},
		{name: "empty source", ctx: ctx, st: st, herdr: h, gh: g, me: "alice", want: "found empty"},
		{name: "empty me", ctx: ctx, st: st, herdr: h, gh: g, source: "jira:acme", want: "found empty"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := snapshot.Build(tc.ctx, tc.st, tc.herdr, tc.gh, tc.source, tc.me)
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Build error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestBuildDuplicateActiveClaim(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("close store: %v", err)
		}
	})
	ctx := context.Background()
	first := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	second := first.Add(time.Minute)
	if err := st.InsertClaim(ctx, store.Claim{
		Source: "jira:acme", ID: "PRJ-1", ClaimedAt: first, State: store.Claimed, UpdatedAt: first,
	}); err != nil {
		t.Fatalf("InsertClaim: %v", err)
	}
	rawDB, err := sql.Open("sqlite", dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := rawDB.Close(); err != nil {
			t.Errorf("close raw db: %v", err)
		}
	}()
	if _, err := rawDB.ExecContext(ctx, `
		INSERT INTO claims(source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at)
		VALUES (?, ?, ?, '', '', '', ?, '', ?)`,
		"jira:acme", "PRJ-1", second.UTC().Format("2006-01-02T15:04:05.000000000Z07:00"),
		string(store.Claimed), second.UTC().Format("2006-01-02T15:04:05.000000000Z07:00"),
	); err != nil {
		t.Fatalf("insert duplicate legacy claim: %v", err)
	}
	_, err = snapshot.Build(ctx, st, herdr.NewClient(scriptOK(t), testHerdr), gh.NewClient(scriptOK(t), testGH), "jira:acme", "alice")
	if err == nil || !strings.Contains(err.Error(), "found more than one") {
		t.Fatalf("Build error = %v", err)
	}
}

func scriptOK(t *testing.T) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs(testHerdr, "workspace", "list").WillSucceed(workspacesJSON, 0).Build()
	mock.ExpectCommandWithArgs(testHerdr, "tab", "list").WillSucceed(tabsJSON, 0).Build()
	mock.ExpectCommand(testGH).WillSucceed(prsJSON, 0).Build()
	return mock
}

func openStore(t *testing.T) *store.Store {
	t.Helper()
	st, err := store.Open(context.Background(), filepath.Join(t.TempDir(), "factory.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	})
	return st
}
