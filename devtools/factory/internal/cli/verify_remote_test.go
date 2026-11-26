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

const remoteRepo = "/remote/repo"

// writeRemoteVerifyConfig is writeVerifyConfig plus a remote mac lane.
// exec is the lane's exec line, or empty for none.
func writeRemoteVerifyConfig(t *testing.T, exec string) string {
	t.Helper()
	cfgPath, _ := writeVerifyConfig(t)
	f, err := os.OpenFile(cfgPath, os.O_APPEND|os.O_WRONLY, 0o600) //nolint:gosec // G304: the temp config this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := f.Close(); err != nil {
			t.Error(err)
		}
	}()
	lane := "\n[[lane]]\nname = \"mac\"\nmachine = \"mac1\"\ncheckout = \"" + remoteRepo + "\"\nmax_workers = 2\n" + exec + "\n\n[lanes]\ndefault = [\"mac\"]\n"
	if _, err := f.WriteString(lane); err != nil {
		t.Fatal(err)
	}
	return cfgPath
}

func seedRemoteVerifyItem(t *testing.T, dbPath string) {
	t.Helper()
	stash := 0
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
			State: store.Started, UpdatedAt: day1, StashCount: &stash, Lane: "mac",
		}); err != nil {
			t.Fatal(err)
		}
	})
}

// remoteVerifyMock expects the local GitHub reads, then every checkout read
// as `ssh mac1 'git' '-C' '/remote/repo' ...` with each argument quoted.
func remoteVerifyMock(t *testing.T, reachable bool) *executor.MockExecutor {
	t.Helper()
	fresh := executor.NewMockExecutor()
	fresh.SetDefaultBehavior(nil, errors.New("unexpected command"))
	open, view := verifyGH(true)
	fresh.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
	fresh.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	fresh.ExpectCommand("gh").WillSucceed(open, 0).Once().Build()
	fresh.ExpectCommand("gh").WillSucceed(view, 0).Once().Build()
	ssh := func(args ...string) *executor.MockExpectationBuilder {
		quoted := []string{"mac1"}
		for _, a := range args {
			quoted = append(quoted, "'"+a+"'")
		}
		return fresh.ExpectCommandWithArgs("ssh", quoted...)
	}
	if !reachable {
		ssh("true").WillFail("ssh: connect to host mac1 port 22: Operation timed out", 255).Once().Build()
		useExec(t, fresh)
		return fresh
	}
	when := time.Now().UTC().Add(-time.Hour).Format(time.RFC3339)
	ssh("true").WillSucceed("", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "merge-base", "HEAD", "origin/main").WillSucceed("abc123\n", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "merge-base", "--is-ancestor", "abc123", "origin/main").WillSucceed("", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "log", "-1", "--format=%cI", "abc123").WillSucceed(when+"\n", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "stash", "list").WillSucceed("", 0).Once().Build()
	ssh("cat", "--", filepath.Join(remoteRepo, "pull_request_template.md")).
		WillSucceed("## Summary\n\n<!-- one line -->\n\n## Test plan\n\n<!-- checks -->\n", 0).Once().Build()
	useExec(t, fresh)
	return fresh
}

func TestVerifyRemoteLaneRunsChecksThere(t *testing.T) {
	cfgPath := writeRemoteVerifyConfig(t, `exec = ["ssh", "mac1"]`)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedRemoteVerifyItem(t, dbPath)
	mock := remoteVerifyMock(t, true)
	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "checked on mac1:"+remoteRepo+"\nok\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	for _, call := range mock.Executions() {
		if call.Command == "git" {
			t.Fatalf("ran git locally: %v", call.Args)
		}
	}
}

func TestVerifyRemoteLaneUnreachable(t *testing.T) {
	cfgPath := writeRemoteVerifyConfig(t, `exec = ["ssh", "mac1"]`)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedRemoteVerifyItem(t, dbPath)
	remoteVerifyMock(t, false)
	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || strings.Contains(stdout, "ok") {
		t.Fatalf("exit = %d, stdout = %q", code, stdout)
	}
	if !strings.Contains(stderr, "verify PRJ-1: machine mac1 unreachable: ssh: connect to host mac1 port 22: Operation timed out") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestVerifyRemoteLaneWithoutExec(t *testing.T) {
	cfgPath := writeRemoteVerifyConfig(t, "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedRemoteVerifyItem(t, dbPath)
	remoteVerifyMock(t, false)
	_, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, `lane mac on machine mac1 has no exec; set exec = ["ssh", "HOST"]`) {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
}

func TestVerifyRemoteLaneMergeMode(t *testing.T) {
	cfgPath := writeRemoteVerifyConfig(t, `exec = ["ssh", "mac1"]`)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	stash := 0
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
			State: store.Started, UpdatedAt: day1, StashCount: &stash, Lane: "mac", OutputMode: "merge",
		}); err != nil {
			t.Fatal(err)
		}
	})
	fresh := executor.NewMockExecutor()
	fresh.SetDefaultBehavior(nil, errors.New("unexpected command"))
	ssh := func(args ...string) *executor.MockExpectationBuilder {
		quoted := []string{"mac1"}
		for _, a := range args {
			quoted = append(quoted, "'"+a+"'")
		}
		return fresh.ExpectCommandWithArgs("ssh", quoted...)
	}
	ssh("true").WillSucceed("", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
		WillSucceed("aaa1111\x1f0000000\x1f[PRJ-1] ship it\x1e\n", 0).Once().Build()
	ssh("git", "-C", remoteRepo, "stash", "list").WillSucceed("", 0).Once().Build()
	useExec(t, fresh)

	stdout, stderr, code := runCmd(t, "verify", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "checked on mac1:"+remoteRepo+"\nok\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := fresh.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}
