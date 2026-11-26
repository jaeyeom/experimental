package cli

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestAdoptUsage(t *testing.T) {
	stdout, stderr, code := runCmd(t, "adopt")
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitUsage, stderr)
	}
	if stdout != "" || strings.HasPrefix(stderr, "factory:") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func TestAdoptByTabLabel(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	tabs := tabsJSON(tabSpec{id: "t1", label: "PRJ-1", status: "idle"})
	mock := adoptMock(t, rankWorkspaces, tabs, emptyPRs, "")
	stdout, stderr, code := runCmd(t, "adopt", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	assertSourceCalls(t, src, []string{"Get", "Claim"})
	claim := readAdoptClaim(t, dbPath)
	host, err := os.Hostname()
	if err != nil {
		t.Fatal(err)
	}
	if claim.State != store.Claimed || claim.TabID != "t1" || claim.WorkspaceID != "w1" || claim.Machine != host {
		t.Fatalf("claim = %+v host %q", claim, host)
	}

	useHerdrGH(t, rankWorkspaces, tabs, emptyPRs)
	shown, stderr, code := runCmd(t, "show", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("show exit = %d stderr = %s", code, stderr)
	}
	for _, want := range []string{"state: claimed\n", "tab_id: t1\n", "label: PRJ-1\n", "\n  id: t1\n"} {
		if !strings.Contains(shown, want) {
			t.Fatalf("show missing %q\n%s", want, shown)
		}
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestAdoptPullRequestShowsWithoutTitleMatch(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := adoptMock(t, rankWorkspaces, emptyTabs, emptyPRs, "")
	stdout, stderr, code := runCmd(t, "adopt", "PRJ-1", "--pr", "acme/widgets#12", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	claim := readAdoptClaim(t, dbPath)
	if claim.TabID != "" || len(claim.PRs) != 1 || claim.PRs[0].URL != "https://github.com/acme/widgets/pull/12" {
		t.Fatalf("claim = %+v", claim)
	}
	useHerdrGH(t, rankWorkspaces, emptyTabs, emptyPRs)
	shown, stderr, code := runCmd(t, "show", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("show exit = %d stderr = %s", code, stderr)
	}
	if !strings.Contains(shown, "https://github.com/acme/widgets/pull/12") {
		t.Fatalf("show =\n%s", shown)
	}
	if !strings.Contains(shown, "Tab:\n  workspace: factory\n  label: PRJ-1\n  none\n") {
		t.Fatalf("show tab section =\n%s", shown)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestAdoptHeadBranchWithoutTitleMatch(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	prs := `{
	  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": [{
	    "number": 4,
	    "title": "unrelated",
	    "url": "https://github.com/acme/widgets/pull/4",
	    "isDraft": false,
	    "merged": false,
	    "mergedAt": null,
	    "createdAt": "2026-01-01T00:00:00Z",
	    "closedAt": null,
	    "headRefName": "feature/PRJ-1-fix",
	    "repository": {"nameWithOwner": "acme/widgets"},
	    "closingIssuesReferences": {"nodes": []}
	  }]}}
	}`
	mock := adoptMock(t, rankWorkspaces, emptyTabs, prs, "")
	stdout, stderr, code := runCmd(t, "adopt", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	useHerdrGH(t, rankWorkspaces, emptyTabs, prs)
	shown, stderr, code := runCmd(t, "show", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("show exit = %d stderr = %s", code, stderr)
	}
	if !strings.Contains(shown, "https://github.com/acme/widgets/pull/4  unrelated\n") {
		t.Fatalf("show =\n%s", shown)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestAdoptNeitherLeavesStoreUnchanged(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := adoptMock(t, rankWorkspaces, emptyTabs, emptyPRs, "")
	stdout, stderr, code := runCmd(t, "adopt", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || stdout != "" || !strings.Contains(stderr, "expected a tab or a pull request, found neither") {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	assertNoClaimCall(t, src)
	if src.Items["PRJ-1"].State != "To Do" {
		t.Fatalf("state = %q", src.Items["PRJ-1"].State)
	}
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := st.Close(); err != nil {
			t.Errorf("close: %v", err)
		}
	}()
	if _, err := st.LatestClaim(context.Background(), "jira:acme", "PRJ-1"); err == nil || !strings.Contains(err.Error(), "not found") {
		t.Fatalf("claim error = %v", err)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestAdoptPane(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	tabs := tabsJSON(
		tabSpec{id: "t1", label: "PRJ-1", status: "idle"},
		tabSpec{id: "t9", label: "notes", status: "idle"},
	)
	panes := panesJSON(paneSpec{id: "p9", tab: "t9", status: "idle"})
	mock := adoptMock(t, rankWorkspaces, tabs, emptyPRs, panes)
	stdout, stderr, code := runCmd(t, "adopt", "PRJ-1", "--pane", "p9", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	claim := readAdoptClaim(t, dbPath)
	if claim.TabID != "t9" || claim.WorkspaceID != "w1" {
		t.Fatalf("claim = %+v", claim)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestAdoptSecondDoesNotClaimAgain(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	src.Items["PRJ-1"] = source.Item{Source: "jira:acme", ID: "PRJ-1", Title: "Fix it", State: "In Progress"}
	useSources(t, map[string]*fake.Source{"acme": src})
	tabs := tabsJSON(tabSpec{id: "t1", label: "PRJ-1", status: "idle"})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	for range 2 {
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	}
	useExec(t, mock)
	args := []string{"adopt", "PRJ-1", "--config", cfgPath, "--db", dbPath}
	if _, stderr, code := runCmd(t, args...); code != ExitOK || stderr != "" {
		t.Fatalf("first exit = %d stderr = %s", code, stderr)
	}
	first := readAdoptClaim(t, dbPath)
	if _, stderr, code := runCmd(t, args...); code != ExitOK || stderr != "" {
		t.Fatalf("second exit = %d stderr = %s", code, stderr)
	}
	second := readAdoptClaim(t, dbPath)
	if !second.ClaimedAt.Equal(first.ClaimedAt) || second.State != store.Claimed {
		t.Fatalf("first = %+v second = %+v", first, second)
	}
	if got := callCountFake(src, "Claim"); got != 1 {
		t.Fatalf("Claim calls = %d, calls = %#v", got, src.Calls)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func adoptMock(t *testing.T, workspaces, tabs, prs, panes string) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(workspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(prs, 0).Once().Build()
	if panes != "" {
		mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(panes, 0).Once().Build()
	}
	useExec(t, mock)
	return mock
}

func readAdoptClaim(t *testing.T, dbPath string) store.Claim {
	t.Helper()
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := st.Close(); err != nil {
			t.Errorf("close store: %v", err)
		}
	}()
	got, err := st.LatestClaim(context.Background(), "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func callCountFake(src *fake.Source, method string) int {
	n := 0
	for _, call := range src.Calls {
		if call.Method == method {
			n++
		}
	}
	return n
}
