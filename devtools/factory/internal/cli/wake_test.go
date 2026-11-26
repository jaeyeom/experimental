package cli

import (
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

// useAgentbusWaker restores the real wake hook for one test.
func useAgentbusWaker(t *testing.T) {
	t.Helper()
	prev := newWaker
	newWaker = agentbusWaker
	t.Cleanup(func() { newWaker = prev })
}

func TestStartWake(t *testing.T) {
	tests := []struct {
		name     string
		defaults string
		wake     func(*executor.MockExecutor)
		wantLog  string
	}{
		{
			name: "hibernated agent is woken first",
			wake: func(m *executor.MockExecutor) {
				m.ExpectCommandWithArgs("agentbus", "wake", "--if-hibernated", "--wait", "--machine", "local", "w1:p1").
					WillSucceed("woke w1:p1\n", 0).Once().Build()
			},
			wantLog: "started PRJ-1 (woke hibernated agent)",
		},
		{
			name: "awake agent",
			wake: func(m *executor.MockExecutor) {
				m.ExpectCommandWithArgs("agentbus", "wake", "--if-hibernated", "--wait", "--machine", "local", "w1:p1").
					WillSucceed("", 0).Once().Build()
			},
			wantLog: "started PRJ-1",
		},
		{
			name: "agentbus missing",
			wake: func(m *executor.MockExecutor) {
				m.ExpectCommand("agentbus").WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Once().Build()
			},
			wantLog: "started PRJ-1",
		},
		{
			name:     "off",
			defaults: "[defaults]\nwake_before_prompt = \"off\"\n",
			wake:     func(*executor.MockExecutor) {},
			wantLog:  "started PRJ-1",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			useAgentbusWaker(t)
			cfgPath, cwd := writeStartConfig(t, tc.defaults, "")
			dbPath := filepath.Join(t.TempDir(), "factory.db")
			seedStartItem(t, dbPath, "Fix it", true)
			useSources(t, map[string]*fake.Source{"acme": startFake()})
			mock := startMock(t, rankWorkspaces, emptyTabs, true, cwd)
			tc.wake(mock)
			_, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
			if code != ExitOK || stderr != "" {
				t.Fatalf("exit = %d, stderr = %s", code, stderr)
			}
			if err := mock.AssertExpectationsMet(); err != nil {
				t.Fatal(err)
			}
			assertWakeBeforePrompt(t, mock, tc.name != "off")
			logs, _, _ := runCmd(t, "logs", "--config", cfgPath, "--db", dbPath)
			if !strings.Contains(logs, tc.wantLog+"\n") {
				t.Fatalf("logs = %q, want %q", logs, tc.wantLog)
			}
		})
	}
}

func TestStartWakeFailed(t *testing.T) {
	useAgentbusWaker(t)
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").
		WillSucceed(startTab, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").
		WillSucceed(startAgent, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
	mock.ExpectCommand("agentbus").WillFail("agentbus wake: w1:p1: relaunch timed out\n", 1).Once().Build()
	_, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartClaimKept {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stderr, "wake before prompt") || !strings.Contains(stderr, "relaunch timed out") ||
		!strings.Contains(stderr, "prompt not sent") {
		t.Fatalf("stderr = %s", stderr)
	}
	for _, call := range mock.Executions() {
		if call.Command == "herdr" && len(call.Args) > 1 && call.Args[1] == "prompt" {
			t.Fatalf("prompt sent after a failed wake: %v", call.Args)
		}
	}
}

// assertWakeBeforePrompt checks that agentbus ran (or not) and, when it
// ran, that it ran before herdr agent prompt.
func assertWakeBeforePrompt(t *testing.T, mock *executor.MockExecutor, want bool) {
	t.Helper()
	wake, prompt := -1, -1
	for i, call := range mock.Executions() {
		switch {
		case call.Command == "agentbus":
			wake = i
		case call.Command == "herdr" && len(call.Args) > 1 && call.Args[0] == "agent" && call.Args[1] == "prompt":
			prompt = i
		}
	}
	if prompt < 0 {
		t.Fatal("no herdr agent prompt")
	}
	if !want {
		if wake >= 0 {
			t.Fatalf("agentbus ran at %d, want no wake", wake)
		}
		return
	}
	if wake < 0 || wake > prompt {
		t.Fatalf("agentbus at %d, prompt at %d: want the wake first", wake, prompt)
	}
}
