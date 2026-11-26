package cli

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestVerifyPass(t *testing.T) {
	cfgPath, cwd := writeVerifyConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedVerifyItem(t, dbPath, 0)
	mock := verifyMock(t, cwd, true, "")
	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "ok\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	// verify records the pull request it read in the prs table.
	seedDB(t, dbPath, func(st *store.Store) {
		pr, err := st.PR(context.Background(), "acme/widgets", 9)
		if err != nil {
			t.Fatal(err)
		}
		opened := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
		if pr.ItemID != "PRJ-1" || pr.Source != "jira:acme" || !pr.Draft || !pr.OpenedAt.Equal(opened) {
			t.Fatalf("pr = %+v", pr)
		}
		claim, err := st.LatestClaim(context.Background(), "jira:acme", "PRJ-1")
		if err != nil {
			t.Fatal(err)
		}
		if len(claim.PRs) != 1 || claim.PRs[0].Repo != "acme/widgets" || claim.PRs[0].Number != 9 || claim.PRs[0].URL != "https://github.com/acme/widgets/pull/9" || claim.PRs[0].Head != "feature" {
			t.Fatalf("claim prs = %#v", claim.PRs)
		}
	})
}

func TestVerifyReportsDraft(t *testing.T) {
	cfgPath, cwd := writeVerifyConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedVerifyItem(t, dbPath, 0)
	mock := verifyMock(t, cwd, false, "")
	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if !strings.Contains(stdout, "draft: expected a draft, found a ready pull request\n") {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "verify PRJ-1: 1 checks failed") {
		t.Fatalf("stderr = %q", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestVerifyUsesClaimWorktree(t *testing.T) {
	cfgPath, cwd := writeVerifyConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	wtDir := filepath.Join(filepath.Dir(cwd), ".wt-PRJ-1")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := rankItem("jira:acme", "PRJ-1", "Ship it", "Task", "In Progress", "", "alice", 2, false, day1, nil)
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		stash := 0
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: day1, Machine: "laptop",
			State: store.Started, UpdatedAt: day1, StashCount: &stash, Worktree: wtDir,
		}); err != nil {
			t.Fatal(err)
		}
	})
	mock := verifyMock(t, wtDir, true, "")
	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "ok\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func writeVerifyConfig(t *testing.T) (string, string) {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	template := "## Summary\n\n<!-- one line -->\n\n## Test plan\n\n<!-- checks -->\n"
	if err := os.WriteFile(filepath.Join(cwd, "pull_request_template.md"), []byte(template), 0o600); err != nil {
		t.Fatal(err)
	}
	body := `
[[source]]
name = "acme"
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = "` + token + `"
jql = "project = PRJ"
claim_status = "In Progress"
cwd = "` + cwd + `"
base = "main"
start_prompt = "work {id}"
workspace = "factory"
pr_template = "pull_request_template.md"
`
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path, cwd
}

func seedVerifyItem(t *testing.T, dbPath string, stash int) {
	t.Helper()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := rankItem("jira:acme", "PRJ-1", "Ship it", "Task", "In Progress", "", "alice", 2, false, day1, nil)
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: day1, Machine: "laptop",
			State: store.Started, UpdatedAt: day1, StashCount: &stash,
		}); err != nil {
			t.Fatal(err)
		}
	})
}

// verifyGH is the canned open-PR search and pr view output for PRJ-1.
func verifyGH(draft bool) (open, view string) {
	open = `{
	  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": [{
	    "number": 9,
	    "title": "[PRJ-1] ship",
	    "url": "https://github.com/acme/widgets/pull/9",
	    "isDraft": true,
	    "merged": false,
	    "mergedAt": null,
	    "createdAt": "2026-01-01T00:00:00Z",
	    "closedAt": null,
	    "headRefName": "feature",
	    "repository": {"nameWithOwner": "acme/widgets"},
	    "closingIssuesReferences": {"nodes": []}
	  }]}}
	}`
	view = `{
	  "isDraft": ` + boolJSON(draft) + `,
	  "title": "[PRJ-1] ship",
	  "body": "## Summary\n\nshipped\n\n## Test plan\n\ngo test\n",
	  "url": "https://github.com/acme/widgets/pull/9",
	  "baseRefName": "main",
	  "labels": [],
	  "reviews": [],
	  "reviewRequests": []
	}`
	return open, view
}

func verifyMock(t *testing.T, cwd string, draft bool, stashList string) *executor.MockExecutor {
	t.Helper()
	open, view := verifyGH(draft)
	when := time.Now().UTC().Add(-time.Hour).Format(time.RFC3339)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(open, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(view, 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "merge-base", "HEAD", "origin/main").WillSucceed("abc123\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "merge-base", "--is-ancestor", "abc123", "origin/main").WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "log", "-1", "--format=%cI", "abc123").WillSucceed(when+"\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "stash", "list").WillSucceed(stashList, 0).Once().Build()
	useExec(t, mock)
	return mock
}

func boolJSON(value bool) string {
	if value {
		return "true"
	}
	return "false"
}

func writeMergeVerifyConfig(t *testing.T) (string, string) {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	body := `
[output]
mode = "merge"

[[source]]
name = "acme"
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = "` + token + `"
jql = "project = PRJ"
claim_status = "In Progress"
cwd = "` + cwd + `"
base = "main"
start_prompt = "work {id}"
workspace = "factory"
`
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path, cwd
}

func TestVerifyMergeModePass(t *testing.T) {
	cfgPath, cwd := writeMergeVerifyConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := rankItem("jira:acme", "PRJ-1", "Ship it", "Task", "In Progress", "", "alice", 2, false, day1, nil)
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		stash := 0
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: day1, Machine: "laptop",
			State: store.Started, UpdatedAt: day1, StashCount: &stash, OutputMode: "merge",
		}); err != nil {
			t.Fatal(err)
		}
	})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("git", "-C", cwd, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
		WillSucceed("aaa1111\x1f0000000\x1f[PRJ-1] ship it\x1e\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "stash", "list").WillSucceed("", 0).Once().Build()
	useExec(t, mock)

	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "ok\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestVerifyMergeModeFailsOnMergeCommitAndDirty(t *testing.T) {
	cfgPath, cwd := writeMergeVerifyConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedVerifyItem(t, dbPath, 0)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("git", "-C", cwd, "status", "--porcelain").WillSucceed(" M a.go\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
		WillSucceed("mmm3333\x1f0000000 aaa1111\x1fMerge branch 'feat' for PRJ-1\x1e\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "stash", "list").WillSucceed("", 0).Once().Build()
	useExec(t, mock)

	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if !strings.Contains(stdout, "checkout: expected a clean status") || !strings.Contains(stdout, "found merge commit mmm3333") {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "verify PRJ-1: 2 checks failed") {
		t.Fatalf("stderr = %q", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}
