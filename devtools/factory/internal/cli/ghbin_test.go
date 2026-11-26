package cli

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

const fakeGH = "/opt/fake/gh-throttle"

// prependTOML puts line before every table in the config at path, where a
// top-level key such as gh_bin must go.
func prependTOML(t *testing.T, path, line string) {
	t.Helper()
	data, err := os.ReadFile(path) //nolint:gosec // G304: path is the temp config this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, append([]byte(line+"\n"), data...), 0o600); err != nil {
		t.Fatal(err)
	}
}

// ghCalls counts executions of bin and fails on any plain gh call.
func ghCalls(t *testing.T, mock *executor.MockExecutor, bin string) int {
	t.Helper()
	n := 0
	for _, call := range mock.Executions() {
		switch call.Command {
		case bin:
			n++
		case "gh":
			if bin != "gh" {
				t.Fatalf("ran gh %v, want only %s", call.Args, bin)
			}
		}
	}
	return n
}

func TestGateGHBin(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme"})
	prependTOML(t, cfgPath, `gh_bin = "`+fakeGH+`"`)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabsJSON(), 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand(fakeGH).WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	if got := ghCalls(t, mock, fakeGH); got != 1 {
		t.Fatalf("%s calls = %d, want 1 (checkout %s)", fakeGH, got, cwds["jira:acme"])
	}
}

func TestStartGHBin(t *testing.T) {
	tests := []struct {
		name string
		key  string // gh_bin line, or empty
		env  string // GH_BIN, or empty for unset
		want string
	}{
		{name: "key", key: `gh_bin = "` + fakeGH + `"`, want: fakeGH},
		{name: "key over env", key: `gh_bin = "` + fakeGH + `"`, env: "/env/gh", want: fakeGH},
		{name: "env", env: "/env/gh", want: "/env/gh"},
		{name: "default", want: "gh"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if tc.env != "" {
				t.Setenv("GH_BIN", tc.env)
			}
			cfgPath, cwd := writeStartConfig(t, "", "")
			if tc.key != "" {
				prependTOML(t, cfgPath, tc.key)
			}
			dbPath := filepath.Join(t.TempDir(), "factory.db")
			seedStartItem(t, dbPath, "Fix it", true)
			useSources(t, map[string]*fake.Source{"acme": startFake()})
			mock := startMockGH(t, tc.want, cwd)
			stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
			if code != ExitOK || stderr != "" {
				t.Fatalf("exit = %d, stderr = %s", code, stderr)
			}
			if !strings.HasPrefix(stdout, startPlan(cwd)+"factory start PRJ-1: started (tab w1:t1, claim ") {
				t.Fatalf("stdout =\n%s\nwant prefix =\n%s", stdout, startPlan(cwd))
			}
			if err := mock.AssertExpectationsMet(); err != nil {
				t.Fatal(err)
			}
			// One start --apply searches the open pull requests once.
			if got := ghCalls(t, mock, tc.want); got != 1 {
				t.Fatalf("%s calls = %d, want 1", tc.want, got)
			}
		})
	}
}

// startMockGH is startMock for an applied start whose gh binary is bin.
// gh may run any number of times here so ghCalls can count the calls.
func startMockGH(t *testing.T, bin, cwd string) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Times(2).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Times(2).Build()
	mock.ExpectCommand(bin).WillSucceed(emptyPRs, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Times(2).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").
		WillSucceed(startTab, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").
		WillSucceed(startAgent, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "prompt", "w1:p1", startPrompt, "--wait", "--until", "working", "--timeout", "30000").
		WillSucceed(startPrompted, 0).Once().Build()
	useExec(t, mock)
	return mock
}
