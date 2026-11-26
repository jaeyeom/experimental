package cli

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestTickApplyStarts(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := tickMock(t, emptyTabs, true, cwd)
	stdout, stderr, code := runCmd(t, "tick", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if !strings.Contains(stdout, "started PRJ-1 in factory/PRJ-1\n") {
		t.Fatalf("stdout = %q", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertStartedRow(t, dbPath)
}

func TestTickCallerDryRun(t *testing.T) {
	clearPaneID(t)
	t.Setenv("HERDR_PANE_ID", "w1:p9")
	cfgPath, cwd := writeStartConfig(t, "[defaults]\ncaller = \"from-config\"\n", "report to {caller} about {id}")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := tickMock(t, emptyTabs, false, cwd)
	stdout, stderr, code := runCmd(t, "tick", "--caller", "coord", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "prompt   report to coord about PRJ-1…\n") || strings.Contains(stdout, "{caller}") {
		t.Fatalf("stdout =\n%s", stdout)
	}
	if !strings.Contains(stdout, "dry-run PRJ-1 in factory/PRJ-1\n") {
		t.Fatalf("stdout =\n%s", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestTickWrongBranch(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, `
[gate]
base_branch = "main"
require = "on_base"
`, "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "rev-parse", "--abbrev-ref", "HEAD").WillSucceed("feature\n", 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "tick", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
	}
	want := "blocked  yes\n" + startPlan(cwd) + "gate: " + cwd + ": expected branch main, found feature\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestTickSyncErrorExit(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := startFake()
	src.FailOn = map[string]error{"Sync": errors.New("sync broke")}
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "tick", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, "sync broke") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
	assertNoClaimCall(t, src)
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestSettleUsage(t *testing.T) {
	stdout, stderr, code := runCmd(t, "settle", "PRJ-1", "PRJ-1")
	if code != ExitUsage {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, "expected distinct ids, found PRJ-1 repeated") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func TestSettleHelp(t *testing.T) {
	stdout, stderr, code := runCmd(t, "settle", "--help")
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" || !strings.Contains(stdout, "Finish merged work without starting an agent") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func TestTickUsage(t *testing.T) {
	stdout, stderr, code := runCmd(t, "tick", "PRJ-1")
	if code != ExitUsage {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, "expected no arguments, found 1") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func TestRunRejectsShortInterval(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "missing", args: []string{"run"}, want: "found 0s"},
		{name: "thirty seconds", args: []string{"run", "--interval", "30s"}, want: "found 30s"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			stdout, stderr, code := runCmd(t, tc.args...)
			if code != ExitUsage {
				t.Fatalf("exit = %d, stderr = %s", code, stderr)
			}
			if stdout != "" || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, "expected interval >= 1m") || !strings.Contains(stderr, tc.want) {
				t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
			}
		})
	}
}

func TestRunUsage(t *testing.T) {
	stdout, stderr, code := runCmd(t, "run", "--interval", "1m", "extra")
	if code != ExitUsage {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, "expected no arguments, found 1") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func tickMock(t *testing.T, tabs string, apply bool, cwd string) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	if apply {
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").
			WillSucceed(startTab, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").
			WillSucceed(startAgent, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "prompt", "w1:p1", startPrompt, "--wait", "--until", "working", "--timeout", "30000").
			WillSucceed(startPrompted, 0).Once().Build()
	}
	useExec(t, mock)
	return mock
}

func TestSettleAndShowFindOldMergedPullRequest(t *testing.T) {
	mergedAt := time.Date(2020, 1, 15, 0, 0, 0, 0, time.UTC)
	if !mergedAt.Before(time.Now().Add(-7 * 24 * time.Hour)) {
		t.Fatalf("merged at %s is inside the 7-day window", mergedAt)
	}
	cfgPath := writeNextConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	it := rankItem("jira:acme", "PRJ-9", "Old work", "Task", "To Do", "", "ada", 2, false, day1, nil)
	src := &fake.Source{
		SourceName: "jira:acme",
		Account:    "ada",
		Items:      map[string]source.Item{"PRJ-9": it},
	}
	useSources(t, map[string]*fake.Source{"acme": src})
	claimedAt := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", claimedAt, claimedAt); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "ada"); err != nil {
			t.Fatal(err)
		}
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-9", ClaimedAt: claimedAt, Machine: "laptop",
			State: store.Started, Detail: "started", UpdatedAt: claimedAt,
		}); err != nil {
			t.Fatal(err)
		}
	})
	merged := `{
	  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": [{
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
	  }]}}
	}`
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommand("gh").WillSucceed(merged, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(merged, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Times(2).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Times(2).Build()
	useExec(t, mock)

	stdout, stderr, code := runCmd(t, "settle", "PRJ-9", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("settle exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
	}
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	claim, err := st.LatestClaim(context.Background(), "jira:acme", "PRJ-9")
	if closeErr := st.Close(); closeErr != nil {
		t.Fatal(closeErr)
	}
	if err != nil {
		t.Fatal(err)
	}
	if claim.State != store.Done {
		t.Fatalf("claim state = %s, want done", claim.State)
	}

	stdout, stderr, code = runCmd(t, "show", "PRJ-9", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("show exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "https://github.com/acme/widgets/pull/9  [PRJ-9] landed\n") {
		t.Fatalf("stdout = %s", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	openN, titleN := 0, 0
	for _, call := range mock.GetCallHistory() {
		if call.Config.Command != "gh" {
			continue
		}
		args := strings.Join(call.Config.Args, " ")
		if strings.Contains(args, "merged:") {
			t.Fatalf("query has a merged-after date: %s", args)
		}
		switch {
		case strings.Contains(args, "is:open"):
			openN++
		case strings.Contains(args, "is:merged") && strings.Contains(args, `in:title "PRJ-9"`):
			titleN++
		default:
			t.Fatalf("gh args = %s", args)
		}
	}
	if openN != 1 || titleN != 2 {
		t.Fatalf("open queries = %d, title queries = %d", openN, titleN)
	}
}

// TestTickEnqueuePrintsOnlyJobsOnStdout: stdout is one JSON job per ready
// item, the summary goes to stderr, and nothing is claimed or started.
func TestTickEnqueuePrintsOnlyJobsOnStdout(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "tick", "--enqueue", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
	}
	want := `{"kind":"start-item","subject":"PRJ-1","source":"jira:acme","title":"Fix it","dedupe":"start-item:jira:acme:PRJ-1","priority":1,"needs":{"machine":"any","checkout":"` + cwd + `","executor":"new-tab"},"command":["factory","start","--apply","PRJ-1"]}` + "\n"
	if stdout != want {
		t.Fatalf("stdout = %q\nwant     %q", stdout, want)
	}
	if stderr != "enqueued 1 start-item jobs\n" {
		t.Fatalf("stderr = %q", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestSettleMergeModeCLI(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	appendConfig(t, cfgPath, "\n[output]\nmode = \"merge\"\n")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})

	// Seed an active merge-mode claim.
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.InsertClaim(ctx, store.Claim{
			Source:      "jira:acme",
			ID:          "PRJ-1",
			ClaimedAt:   time.Date(2026, 11, 1, 10, 0, 0, 0, time.UTC),
			UpdatedAt:   time.Date(2026, 11, 1, 10, 0, 0, 0, time.UTC),
			Machine:     "test-host",
			TabID:       "w1:t1",
			WorkspaceID: "w1",
			State:       store.Started,
			OutputMode:  "merge",
		}); err != nil {
			t.Fatal(err)
		}
	})

	// 1. settle PRJ-1 before commit lands errors clearly.
	missMock := executor.NewMockExecutor()
	missMock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	missMock.ExpectCommandWithArgs("git", "-C", cwd, "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
		WillSucceed("", 0).Once().Build()
	useExec(t, missMock)

	_, stderr, code := runCmd(t, "settle", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, "expected a commit referencing PRJ-1 on origin/main, found none") {
		t.Fatalf("code = %d, stderr = %q", code, stderr)
	}
	if err := missMock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}

	// 2. settle PRJ-1 after commit lands marks claim done and closes tab.
	hitMock := executor.NewMockExecutor()
	hitMock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	hitMock.ExpectCommandWithArgs("git", "-C", cwd, "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
		WillSucceed("abc1234\x1f0000000\x1f[PRJ-1] Fix it\x1e\n", 0).Once().Build()
	hitMock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	hitMock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), 0).Once().Build()
	hitMock.ExpectCommandWithArgs("herdr", "tab", "close", "w1:t1").WillSucceed(`{"id":"cli:tab:close","result":{"type":"tab_closed","tab_id":"w1:t1","workspace_id":"w1"}}`, 0).Once().Build()
	useExec(t, hitMock)

	stdout, stderr, code := runCmd(t, "settle", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("code = %d, stderr = %q, stdout = %q", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "closed tab PRJ-1 (w1:t1)\n") {
		t.Fatalf("stdout = %q", stdout)
	}
	if err := hitMock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestSettleMergeModeWorkingTabSelfAndExternalCLI(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	appendConfig(t, cfgPath, "\n[output]\nmode = \"merge\"\n")
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})

	t.Run("self-settle", func(t *testing.T) {
		clearPaneID(t)
		t.Setenv("HERDR_PANE_ID", "w1:p1")
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedDB(t, dbPath, func(st *store.Store) {
			if err := st.InsertClaim(context.Background(), store.Claim{
				Source: "jira:acme", ID: "PRJ-1",
				ClaimedAt: time.Date(2026, 11, 1, 10, 0, 0, 0, time.UTC),
				UpdatedAt: time.Date(2026, 11, 1, 10, 0, 0, 0, time.UTC),
				Machine:   "test-host", TabID: "w1:t1", WorkspaceID: "w1",
				State: store.Started, OutputMode: "merge",
			}); err != nil {
				t.Fatal(err)
			}
		})
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("git", "-C", cwd, "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed("abc1234\x1f0000000\x1f[PRJ-1] Fix it\x1e\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "working"}), 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(panesJSON(paneSpec{id: "w1:p1", tab: "w1:t1", status: "working", cwd: cwd}), 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "close", "w1:t1").WillSucceed(`{"id":"cli:tab:close","result":{"type":"tab_closed","tab_id":"w1:t1","workspace_id":"w1"}}`, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "settle", "PRJ-1", "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("code = %d, stderr = %q, stdout = %q", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "closed tab PRJ-1 (w1:t1)\n") {
			t.Fatalf("stdout = %q", stdout)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("external settle waits then closes", func(t *testing.T) {
		clearPaneID(t)
		t.Setenv("HERDR_PANE_ID", "w1:p9")
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedDB(t, dbPath, func(st *store.Store) {
			if err := st.InsertClaim(context.Background(), store.Claim{
				Source: "jira:acme", ID: "PRJ-1",
				ClaimedAt: time.Date(2026, 11, 1, 10, 0, 0, 0, time.UTC),
				UpdatedAt: time.Date(2026, 11, 1, 10, 0, 0, 0, time.UTC),
				Machine:   "test-host", TabID: "w1:t1", WorkspaceID: "w1",
				State: store.Started, OutputMode: "merge",
			}); err != nil {
				t.Fatal(err)
			}
		})
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("git", "-C", cwd, "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed("abc1234\x1f0000000\x1f[PRJ-1] Fix it\x1e\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "working"}), 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(panesJSON(
			paneSpec{id: "w1:p1", tab: "w1:t1", status: "working", cwd: cwd},
			paneSpec{id: "w1:p9", tab: "w1:t9", status: "working", cwd: cwd},
		), 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:t1", "--until", "idle", "--until", "done", "--timeout", "5000").
			WillSucceed(agentIdle, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "close", "w1:t1").WillSucceed(`{"id":"cli:tab:close","result":{"type":"tab_closed","tab_id":"w1:t1","workspace_id":"w1"}}`, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "settle", "PRJ-1", "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("code = %d, stderr = %q, stdout = %q", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "closed tab PRJ-1 (w1:t1)\n") {
			t.Fatalf("stdout = %q", stdout)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})
}
