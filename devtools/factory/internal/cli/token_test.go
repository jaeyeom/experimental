package cli

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestLocalCommandsIgnoreMissingTokenFile(t *testing.T) {
	cfgPath, dbPath, tokenPath := missingTokenFixture(t)
	useMirrorExec(t)

	commands := []struct {
		name string
		args []string
		want string
	}{
		{name: "next", args: []string{"next", "--config", cfgPath, "--db", dbPath}, want: "PRJ-1"},
		{name: "show", args: []string{"show", "PRJ-1", "--config", cfgPath, "--db", dbPath}, want: "PRJ-1"},
		{name: "doctor", args: []string{"doctor", "--config", cfgPath, "--db", dbPath}, want: ""},
		{name: "gate", args: []string{"gate", "--config", cfgPath, "--db", dbPath}, want: "OK: 0 in flight (max 3)\n"},
		{name: "logs", args: []string{"logs", "--config", cfgPath, "--db", dbPath}, want: "sync"},
	}
	for _, tc := range commands {
		t.Run(tc.name, func(t *testing.T) {
			if tc.name == "doctor" {
				doctorToolOK(t)
			}
			stdout, stderr, code := runCmd(t, tc.args...)
			wantMax := ExitOK
			if tc.name == "doctor" {
				wantMax = 1
			}
			if code > wantMax {
				t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
			}
			if stderr != "" {
				t.Fatalf("stderr = %q", stderr)
			}
			if strings.Contains(stdout, tokenPath) || strings.Contains(stdout, "token_file") {
				t.Fatalf("stdout mentioned the token:\n%s", stdout)
			}
			if tc.want != "" && !strings.Contains(stdout, tc.want) {
				t.Fatalf("stdout = %q, want %q", stdout, tc.want)
			}
		})
	}
}

func TestSyncMissingTokenFileNamesSourceAndField(t *testing.T) {
	cfgPath, dbPath, tokenPath := missingTokenFixture(t)
	stdout, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, want %d\nstdout:\n%s\nstderr:\n%s", code, ExitError, stdout, stderr)
	}
	for _, part := range []string{`jira:acme`, `field "token_file"`, tokenPath} {
		if !strings.Contains(stderr, part) && !strings.Contains(stdout, part) {
			t.Fatalf("missing %q\nstdout:\n%s\nstderr:\n%s", part, stdout, stderr)
		}
	}
}

func TestNextSyncMissingTokenFile(t *testing.T) {
	cfgPath, dbPath, tokenPath := missingTokenFixture(t)
	stdout, stderr, code := runCmd(t, "next", "--sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, want %d\nstdout:\n%s\nstderr:\n%s", code, ExitError, stdout, stderr)
	}
	for _, part := range []string{`jira:acme`, `field "token_file"`, tokenPath} {
		if !strings.Contains(stderr, part) {
			t.Fatalf("stderr = %q, want %q", stderr, part)
		}
	}
}

func missingTokenFixture(t *testing.T) (string, string, string) {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	tokenPath := filepath.Join(dir, "missing-token")
	body := fmt.Sprintf(`
[[source]]
name = "acme"
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = %q
jql = "project = PRJ"
claim_status = "In Progress"
cwd = %q
base = "main"
start_prompt = "work {id}"
workspace = "factory"
`, tokenPath, cwd)
	cfgPath := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(cfgPath, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := t.Context()
		it := rankItem("jira:acme", "PRJ-1", "Ship it", "Task", "To Do", "", "alice", 2, false, day1, nil)
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		if err := st.AppendRun(ctx, store.Run{
			StartedAt:  day1,
			Command:    "sync",
			Outcome:    "ok",
			DetailJSON: `[]`,
		}); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath, tokenPath
}

func useMirrorExec(t *testing.T) {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Build()
	useExec(t, mock)
}
