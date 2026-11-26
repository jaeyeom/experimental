package adopt

import (
	"context"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/jira"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	adoptIssueToDo = `{
		"key": "PRJ-1",
		"fields": {
			"summary": "Ship it",
			"status": {"name": "To Do", "statusCategory": {"key": "new"}},
			"issuetype": {"name": "Task"},
			"created": "2026-01-02T03:04:05.000-0700",
			"updated": "2026-01-03T04:05:06.000-0700"
		}
	}`
	adoptIssueInProgress = `{
		"key": "PRJ-1",
		"fields": {
			"summary": "Ship it",
			"status": {"name": "In Progress", "statusCategory": {"key": "indeterminate"}},
			"issuetype": {"name": "Task"},
			"created": "2026-01-02T03:04:05.000-0700",
			"updated": "2026-01-03T04:05:06.000-0700"
		}
	}`
	adoptTransitions = `{
		"transitions": [
			{"id": "11", "name": "Start progress", "to": {"name": "In Progress"}},
			{"id": "21", "name": "Backlog", "to": {"name": "To Do"}}
		]
	}`
)

func TestAdoptJiraPostsOneTransition(t *testing.T) {
	posts := 0
	gets := 0
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case r.Method == http.MethodGet && r.URL.Path == "/rest/api/3/issue/PRJ-1":
			gets++
			_, _ = w.Write([]byte(adoptIssueToDo))
		case r.Method == http.MethodGet && r.URL.Path == "/rest/api/3/issue/PRJ-1/transitions":
			_, _ = w.Write([]byte(adoptTransitions))
		case r.Method == http.MethodPost && r.URL.Path == "/rest/api/3/issue/PRJ-1/transitions":
			posts++
			w.WriteHeader(http.StatusNoContent)
		default:
			t.Errorf("unexpected %s %s", r.Method, r.URL.Path)
			http.Error(w, "unexpected", http.StatusInternalServerError)
		}
	}))
	t.Cleanup(srv.Close)

	st := openAdoptStore(t)
	in := jiraInput(t, srv, st)
	now := time.Date(2026, 11, 5, 8, 0, 0, 0, time.UTC)
	in.Now = func() time.Time { return now }
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1"}); err != nil {
		t.Fatalf("Adopt: %v", err)
	}
	if posts != 1 {
		t.Fatalf("transition posts = %d, want 1", posts)
	}
	claim := latestAdopt(t, st, "jira:acme", "PRJ-1")
	if claim.State != store.Claimed || claim.TabID != "t1" || claim.WorkspaceID != "w1" || !claim.ClaimedAt.Equal(now) {
		t.Fatalf("claim = %+v", claim)
	}

	in.Now = func() time.Time { return now.Add(time.Minute) }
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1"}); err != nil {
		t.Fatalf("second Adopt: %v", err)
	}
	if posts != 1 || gets != 2 {
		t.Fatalf("after second adopt posts = %d gets = %d, want 1 and 2", posts, gets)
	}
	again := latestAdopt(t, st, "jira:acme", "PRJ-1")
	if !again.ClaimedAt.Equal(now) || again.State != store.Claimed {
		t.Fatalf("second claim = %+v", again)
	}
}

func TestAdoptJiraAlreadyInStatusPostsNoTransition(t *testing.T) {
	posts := 0
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case r.Method == http.MethodGet && r.URL.Path == "/rest/api/3/issue/PRJ-1":
			_, _ = w.Write([]byte(adoptIssueInProgress))
		case r.Method == http.MethodPost && strings.Contains(r.URL.Path, "/transitions"):
			posts++
			t.Errorf("posted a transition")
		default:
			t.Errorf("unexpected %s %s", r.Method, r.URL.Path)
			http.Error(w, "unexpected", http.StatusInternalServerError)
		}
	}))
	t.Cleanup(srv.Close)

	st := openAdoptStore(t)
	in := jiraInput(t, srv, st)
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1"}); err != nil {
		t.Fatalf("Adopt: %v", err)
	}
	if posts != 0 {
		t.Fatalf("posts = %d, want 0", posts)
	}
	claim := latestAdopt(t, st, "jira:acme", "PRJ-1")
	if claim.State != store.Claimed || claim.TabID != "t1" {
		t.Fatalf("claim = %+v", claim)
	}
}

func TestAdoptGitHubLabel(t *testing.T) {
	tests := []struct {
		name  string
		label bool
		want  int
	}{
		{name: "missing", want: 1},
		{name: "present", label: true, want: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			labels := []string(nil)
			if tc.label {
				labels = []string{"in-progress"}
			}
			src := &fake.Source{
				SourceName: "github:experimental",
				Account:    "ada",
				Items: map[string]source.Item{
					"acme/widgets#12": {
						Source: "github:experimental", ID: "acme/widgets#12", State: "open", Labels: labels,
					},
				},
			}
			st := openAdoptStore(t)
			in := Input{
				Src: src, Store: st, Hostname: "laptop", Kind: "github",
				Cfg: config.Source{Kind: "github", Repo: "acme/widgets", Workspace: "widgets", ClaimLabel: "in-progress", PRTitlePattern: `\[{id}\]`},
				Now: func() time.Time { return time.Date(2026, 11, 5, 8, 0, 0, 0, time.UTC) },
				Snap: ready.Snapshot{
					Items: map[string]source.Item{
						"acme/widgets#12": {Source: "github:experimental", ID: "acme/widgets#12"},
					},
					Workspaces: []herdr.Workspace{{ID: "wg", Label: "widgets"}},
					Tabs:       []herdr.Tab{{ID: "tg", WorkspaceID: "wg", Label: "#12"}},
				},
			}
			if err := Adopt(context.Background(), in, Request{ID: "acme/widgets#12"}); err != nil {
				t.Fatalf("Adopt: %v", err)
			}
			if got := callCount(src, "Claim"); got != tc.want {
				t.Fatalf("Claim calls = %d, want %d; calls = %#v", got, tc.want, src.Calls)
			}
			claim := latestAdopt(t, st, "github:experimental", "acme/widgets#12")
			if claim.TabID != "tg" || claim.WorkspaceID != "wg" || claim.State != store.Claimed {
				t.Fatalf("claim = %+v", claim)
			}
		})
	}
}

func TestAdoptPullRequestWithoutTab(t *testing.T) {
	src := jiraFakeItem("To Do")
	st := openAdoptStore(t)
	in := jiraFakeInput(src, st)
	in.Snap.Tabs = nil
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1", PRs: []string{"acme/widgets#12"}}); err != nil {
		t.Fatalf("Adopt ref: %v", err)
	}
	claim := latestAdopt(t, st, "jira:acme", "PRJ-1")
	if claim.TabID != "" || len(claim.PRs) != 1 || claim.PRs[0].URL != "https://github.com/acme/widgets/pull/12" {
		t.Fatalf("claim = %+v", claim)
	}
	if callCount(src, "Claim") != 1 {
		t.Fatalf("calls = %#v", src.Calls)
	}
}

func TestAdoptURLAndHeadBranch(t *testing.T) {
	src := jiraFakeItem("To Do")
	st := openAdoptStore(t)
	in := jiraFakeInput(src, st)
	in.Snap.OpenPRs = []gh.PR{{
		Repo: "acme/widgets", Number: 4, Title: "unrelated",
		URL: "https://github.com/acme/widgets/pull/4", Head: "feature/PRJ-1-fix",
	}}
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1"}); err != nil {
		t.Fatalf("Adopt head: %v", err)
	}
	claim := latestAdopt(t, st, "jira:acme", "PRJ-1")
	if len(claim.PRs) != 1 || claim.PRs[0].Number != 4 || claim.PRs[0].Title != "unrelated" {
		t.Fatalf("head prs = %#v", claim.PRs)
	}

	src = jiraFakeItem("To Do")
	st = openAdoptStore(t)
	in = jiraFakeInput(src, st)
	in.Snap.Tabs = nil
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1", PRs: []string{"https://www.github.com/acme/widgets/pull/9/"}}); err != nil {
		t.Fatalf("Adopt url: %v", err)
	}
	claim = latestAdopt(t, st, "jira:acme", "PRJ-1")
	if len(claim.PRs) != 1 || claim.PRs[0].URL != "https://github.com/acme/widgets/pull/9" || claim.TabID != "" {
		t.Fatalf("url prs = %#v tab %q", claim.PRs, claim.TabID)
	}
}

func TestAdoptRejectsMissingTabAndPullRequest(t *testing.T) {
	src := jiraFakeItem("To Do")
	st := openAdoptStore(t)
	in := jiraFakeInput(src, st)
	in.Snap.Tabs = nil
	err := Adopt(context.Background(), in, Request{ID: "PRJ-1"})
	if err == nil || !strings.Contains(err.Error(), "expected a tab or a pull request") {
		t.Fatalf("error = %v", err)
	}
	if callCount(src, "Claim") != 0 || callCount(src, "Get") != 0 {
		t.Fatalf("calls = %#v", src.Calls)
	}
	if _, err := st.LatestClaim(context.Background(), "jira:acme", "PRJ-1"); err == nil || !strings.Contains(err.Error(), "not found") {
		t.Fatalf("claim error = %v", err)
	}
	if src.Items["PRJ-1"].State != "To Do" {
		t.Fatalf("state = %q", src.Items["PRJ-1"].State)
	}
}

func TestAdoptPaneRecordsContainingTab(t *testing.T) {
	src := jiraFakeItem("To Do")
	st := openAdoptStore(t)
	in := jiraFakeInput(src, st)
	in.Snap.Tabs = []herdr.Tab{
		{ID: "t1", WorkspaceID: "w1", Label: "PRJ-1"},
		{ID: "t9", WorkspaceID: "w9", Label: "notes"},
	}
	in.Panes = []herdr.Pane{{ID: "p9", TabID: "t9", WorkspaceID: "w9"}}
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1", Pane: "p9"}); err != nil {
		t.Fatalf("Adopt: %v", err)
	}
	claim := latestAdopt(t, st, "jira:acme", "PRJ-1")
	if claim.TabID != "t9" || claim.WorkspaceID != "w9" {
		t.Fatalf("claim = %+v", claim)
	}
}

func TestAdoptSecondFillsMissingPullRequest(t *testing.T) {
	src := jiraFakeItem("To Do")
	st := openAdoptStore(t)
	in := jiraFakeInput(src, st)
	now := time.Date(2026, 11, 5, 8, 0, 0, 0, time.UTC)
	in.Now = func() time.Time { return now }
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1"}); err != nil {
		t.Fatalf("first: %v", err)
	}
	in.Now = func() time.Time { return now.Add(time.Minute) }
	if err := Adopt(context.Background(), in, Request{ID: "PRJ-1", PRs: []string{"acme/widgets#12"}}); err != nil {
		t.Fatalf("second: %v", err)
	}
	if callCount(src, "Claim") != 1 {
		t.Fatalf("calls = %#v", src.Calls)
	}
	claim := latestAdopt(t, st, "jira:acme", "PRJ-1")
	if !claim.ClaimedAt.Equal(now) || claim.TabID != "t1" || len(claim.PRs) != 1 || claim.PRs[0].Number != 12 {
		t.Fatalf("claim = %+v", claim)
	}
	if !claim.UpdatedAt.Equal(now.Add(time.Minute)) {
		t.Fatalf("updated_at = %s", claim.UpdatedAt)
	}
}

func TestAdoptBadPullRequest(t *testing.T) {
	src := jiraFakeItem("To Do")
	st := openAdoptStore(t)
	in := jiraFakeInput(src, st)
	err := Adopt(context.Background(), in, Request{ID: "PRJ-1", PRs: []string{"https://example.com/acme/widgets/pull/12"}})
	if err == nil || !strings.Contains(err.Error(), "expected owner/repo#N or a pull request URL") {
		t.Fatalf("error = %v", err)
	}
	if callCount(src, "Claim") != 0 {
		t.Fatalf("calls = %#v", src.Calls)
	}
}

func jiraInput(t *testing.T, srv *httptest.Server, st *store.Store) Input {
	t.Helper()
	src := jira.New(config.Source{
		Name:           "jira:acme",
		Kind:           "jira",
		Site:           srv.URL,
		Email:          "dev@example.com",
		Token:          "secret",
		ClaimStatus:    "In Progress",
		PRTitlePattern: `\[{id}\]`,
	}, srv.Client())
	return Input{
		Src: src, Store: st, Hostname: "laptop", Kind: "jira",
		Cfg: config.Source{Kind: "jira", ClaimStatus: "In Progress", PRTitlePattern: `\[{id}\]`},
		Now: func() time.Time { return time.Date(2026, 11, 5, 8, 0, 0, 0, time.UTC) },
		Snap: ready.Snapshot{
			Items: map[string]source.Item{
				"PRJ-1": {Source: "jira:acme", ID: "PRJ-1", Title: "Ship it", State: "To Do"},
			},
			Tabs: []herdr.Tab{{ID: "t1", WorkspaceID: "w1", Label: "PRJ-1"}},
			Me:   "ada",
		},
	}
}

func jiraFakeItem(state string) *fake.Source {
	return &fake.Source{
		SourceName: "jira:acme",
		Account:    "ada",
		Items: map[string]source.Item{
			"PRJ-1": {Source: "jira:acme", ID: "PRJ-1", Title: "Fix it", State: state},
		},
	}
}

func jiraFakeInput(src *fake.Source, st *store.Store) Input {
	return Input{
		Src: src, Store: st, Hostname: "laptop", Kind: "jira",
		Cfg: config.Source{Kind: "jira", ClaimStatus: "In Progress", PRTitlePattern: `\[{id}\]`},
		Now: func() time.Time { return time.Date(2026, 11, 5, 8, 0, 0, 0, time.UTC) },
		Snap: ready.Snapshot{
			Items: map[string]source.Item{
				"PRJ-1": {Source: "jira:acme", ID: "PRJ-1", Title: "Fix it", State: "To Do"},
			},
			Tabs: []herdr.Tab{{ID: "t1", WorkspaceID: "w1", Label: "PRJ-1"}},
		},
	}
}

func openAdoptStore(t *testing.T) *store.Store {
	t.Helper()
	st, err := store.Open(context.Background(), filepath.Join(t.TempDir(), "factory.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("close store: %v", err)
		}
	})
	return st
}

func latestAdopt(t *testing.T, st *store.Store, sourceName, id string) store.Claim {
	t.Helper()
	got, err := st.LatestClaim(context.Background(), sourceName, id)
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func callCount(src *fake.Source, method string) int {
	n := 0
	for _, call := range src.Calls {
		if call.Method == method {
			n++
		}
	}
	return n
}
