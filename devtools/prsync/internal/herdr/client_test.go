package herdr

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const testHerdrBin = "/tmp/prsync-herdr-fake"

const tabListJSON = `{
  "result": {
    "tabs": [
      {
        "tab_id": "w2:tC",
        "workspace_id": "w2",
        "label": "PROJ-123",
        "agent_status": "idle",
        "pane_count": 1
      }
    ]
  }
}`

const agentListJSON = `{
  "result": {
    "agents": [
      {
        "pane_id": "w2:pC",
        "tab_id": "w2:tC",
        "agent": "codex",
        "agent_status": "idle"
      }
    ]
  }
}`

const paneCurrentJSON = `{
  "result": {
    "pane_id": "w2:pC",
    "tab_id": "w2:tC",
    "workspace_id": "w2"
  }
}`

const paneCurrentNestedJSON = `{
  "result": {
    "pane": {
      "pane_id": "w2:pC",
      "tab_id": "w2:tC",
      "workspace_id": "w2"
    }
  }
}`

const promptSuccessJSON = `{
  "result": {
    "agent": {
      "pane_id": "w2:pC",
      "tab_id": "w2:tC",
      "agent": "codex",
      "agent_status": "idle"
    }
  }
}`

func TestRequireMin(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		output  string
		execErr error
		wantErr error
	}{
		{name: "0.8.1", output: "herdr 0.8.1\n"},
		{name: "0.8", output: "herdr 0.8\n"},
		{name: "0.7.9", output: "herdr 0.7.9\n", wantErr: ErrUnsupported},
		{name: "garbage", output: "herdr bananas\n", wantErr: ErrUnsupported},
		{
			name:    "missing binary",
			execErr: &executor.ExecutableNotFoundError{Command: testHerdrBin},
			wantErr: ErrNotInstalled,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := newHerdrMock()
			exp := mock.ExpectCommandWithArgs(testHerdrBin, "--version")
			if tc.execErr != nil {
				exp.WillError(tc.execErr).Build()
			} else {
				exp.WillSucceed(tc.output, 0).Build()
			}
			err := NewClient(mock, testHerdrBin).RequireMin(context.Background(), "0.8.0")
			if tc.wantErr == nil {
				if err != nil {
					t.Fatalf("RequireMin() unexpected error: %v", err)
				}
				return
			}
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("RequireMin() error = %v, want %v", err, tc.wantErr)
			}
		})
	}
}

func TestTabList(t *testing.T) {
	t.Parallel()

	t.Run("parses envelope", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").WillSucceed(tabListJSON, 0).Build()
		got, err := NewClient(mock, testHerdrBin).TabList(context.Background())
		if err != nil {
			t.Fatalf("TabList() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].TabID != "w2:tC" || got[0].Label != "PROJ-123" {
			t.Fatalf("TabList() = %+v", got)
		}
	})

	t.Run("missing tabs is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").
			WillSucceed(`{"result":{}}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).TabList(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("TabList() error = %v, want ErrDegraded", err)
		}
	})

	t.Run("empty tabs is ok", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").
			WillSucceed(`{"result":{"tabs":[]}}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).TabList(context.Background())
		if err != nil {
			t.Fatalf("TabList() unexpected error: %v", err)
		}
		if got == nil || len(got) != 0 {
			t.Fatalf("TabList() = %#v, want empty slice", got)
		}
	})
}

func TestTabClose(t *testing.T) {
	t.Parallel()

	t.Run("success", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w2:tC").
			WillSucceed(`{"result":{"closed":true}}`, 0).Build()
		if err := NewClient(mock, testHerdrBin).TabClose(context.Background(), "w2:tC"); err != nil {
			t.Fatalf("TabClose() unexpected error: %v", err)
		}
	})

	t.Run("tab_not_found", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w2:tMissing").WillReturn(&executor.ExecutionResult{
			Output:   `{"error":{"code":"tab_not_found","message":"tab w2:tMissing not found"},"id":"cli:tab:close"}`,
			ExitCode: 1,
		}, nil).Build()
		err := NewClient(mock, testHerdrBin).TabClose(context.Background(), "w2:tMissing")
		if !errors.Is(err, ErrTabNotFound) {
			t.Fatalf("TabClose() error = %v, want ErrTabNotFound", err)
		}
	})

	t.Run("other error", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w2:tC").WillReturn(&executor.ExecutionResult{
			Stderr:   "herdr: boom",
			ExitCode: 1,
		}, nil).Build()
		err := NewClient(mock, testHerdrBin).TabClose(context.Background(), "w2:tC")
		var proc *ProcError
		if !errors.As(err, &proc) {
			t.Fatalf("TabClose() error = %v, want ProcError", err)
		}
		if proc.ExitCode != 1 {
			t.Fatalf("exit = %d, want 1", proc.ExitCode)
		}
	})
}

func TestAgentList(t *testing.T) {
	t.Parallel()

	t.Run("parses envelope", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "agent", "list").WillSucceed(agentListJSON, 0).Build()
		got, err := NewClient(mock, testHerdrBin).AgentList(context.Background())
		if err != nil {
			t.Fatalf("AgentList() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].PaneID != "w2:pC" || got[0].Agent != "codex" {
			t.Fatalf("AgentList() = %+v", got)
		}
	})

	t.Run("parses state_change_seq and revision", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "agent", "list").WillSucceed(`{
  "result": {
    "agents": [
      {
        "pane_id": "w2:pC",
        "tab_id": "w2:tC",
        "agent": "codex",
        "agent_status": "idle",
        "state_change_seq": 12,
        "revision": 4
      }
    ]
  }
}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).AgentList(context.Background())
		if err != nil {
			t.Fatalf("AgentList() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].StateChangeSeq != 12 || got[0].Revision != 4 {
			t.Fatalf("AgentList() = %+v", got)
		}
	})

	t.Run("missing agents is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "agent", "list").
			WillSucceed(`{"result":{}}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).AgentList(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("AgentList() error = %v, want ErrDegraded", err)
		}
	})
}

func TestPaneCurrent(t *testing.T) {
	t.Parallel()

	t.Run("flat result", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "current", "--pane", "w2:pC").
			WillSucceed(paneCurrentJSON, 0).Build()
		got, err := NewClient(mock, testHerdrBin).PaneCurrent(context.Background(), "w2:pC")
		if err != nil {
			t.Fatalf("PaneCurrent() unexpected error: %v", err)
		}
		if got.PaneID != "w2:pC" || got.TabID != "w2:tC" || got.WorkspaceID != "w2" {
			t.Fatalf("PaneCurrent() = %+v", got)
		}
	})

	t.Run("nested pane", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "current", "--pane", "w2:pC").
			WillSucceed(paneCurrentNestedJSON, 0).Build()
		got, err := NewClient(mock, testHerdrBin).PaneCurrent(context.Background(), "w2:pC")
		if err != nil {
			t.Fatalf("PaneCurrent() unexpected error: %v", err)
		}
		if got.PaneID != "w2:pC" {
			t.Fatalf("PaneCurrent() = %+v", got)
		}
	})
}

func TestRunnerPaneUnsetDoesNotCallPaneCurrent(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "")
	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "pane", "current", "--pane", "w2:pFocus").
		WillSucceed(`{"result":{"pane_id":"w2:pFocus","tab_id":"w2:tFocus","workspace_id":"w2"}}`, 0).Build()

	got := NewClient(mock, testHerdrBin).RunnerPane(context.Background())
	if got != "" {
		t.Fatalf("RunnerPane() = %q, want empty", got)
	}
	for _, call := range mock.GetCallHistory() {
		if looksLikePaneCurrent(call.Config.Args) {
			t.Fatalf("pane current was called: %v", call.Config.Args)
		}
	}
}

func TestRunnerPaneSetKeepsEnvOnVerifyError(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "w2:pC")
	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "pane", "current", "--pane", "w2:pC").
		WillError(errors.New("transport")).Build()

	got := NewClient(mock, testHerdrBin).RunnerPane(context.Background())
	if got != "w2:pC" {
		t.Fatalf("RunnerPane() = %q, want %q", got, "w2:pC")
	}
}

func TestPrompt(t *testing.T) {
	t.Parallel()

	until := []string{"idle", "done"}
	timeout := 30 * time.Second

	t.Run("matched", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		expectPrompt(mock).WillSucceed(promptSuccessJSON, 0).Build()
		out := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", until, timeout)
		if out.Status != PromptMatched {
			t.Fatalf("status = %q, want %q (err=%v)", out.Status, PromptMatched, out.Err)
		}
		if out.Agent.PaneID != "w2:pC" || out.Agent.AgentStatus != "idle" {
			t.Fatalf("agent = %+v", out.Agent)
		}
	})

	t.Run("stalled", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		expectPrompt(mock).WillReturn(&executor.ExecutionResult{
			Stderr:   `{"error":{"code":"agent_prompt_stalled","message":"agent did not change state within 5s"}}`,
			ExitCode: 1,
		}, nil).Build()
		out := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", until, timeout)
		if out.Status != PromptStalled {
			t.Fatalf("status = %q, want %q (err=%v)", out.Status, PromptStalled, out.Err)
		}
	})

	t.Run("herdr timeout code", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		expectPrompt(mock).WillReturn(&executor.ExecutionResult{
			Stderr:   `{"error":{"code":"timeout","message":"wait exceeded timeout_ms"}}`,
			ExitCode: 1,
		}, nil).Build()
		out := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", until, timeout)
		if out.Status != PromptTimeout {
			t.Fatalf("status = %q, want %q (err=%v)", out.Status, PromptTimeout, out.Err)
		}
	})

	t.Run("process kill is PromptError", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		expectPrompt(mock).WillTimeout(timeout + 5*time.Second).Build()
		out := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", until, timeout)
		if out.Status != PromptError {
			t.Fatalf("status = %q, want %q", out.Status, PromptError)
		}
		var timeoutErr *executor.TimeoutError
		if !errors.As(out.Err, &timeoutErr) {
			t.Fatalf("err = %v, want TimeoutError", out.Err)
		}
	})

	t.Run("unparseable is PromptError", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		expectPrompt(mock).WillSucceed("not-json", 0).Build()
		out := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", until, timeout)
		if out.Status != PromptError {
			t.Fatalf("status = %q, want %q", out.Status, PromptError)
		}
	})

	t.Run("passes until flags and timeout ms", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		var saw []string
		mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
			if cfg.Command != testHerdrBin {
				return false
			}
			saw = append([]string(nil), cfg.Args...)
			return true
		}).WillSucceed(promptSuccessJSON, 0).Build()
		_ = NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", until, timeout)
		wantPrefix := []string{"agent", "prompt", "w2:pC", "hello", "--wait", "--until", "idle", "--until", "done", "--timeout", "30000"}
		if !equalStrings(saw, wantPrefix) {
			t.Fatalf("args = %v, want %v", saw, wantPrefix)
		}
	})
}

func newHerdrMock() *executor.MockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testHerdrBin, true)
	return mock
}

func expectPrompt(mock *executor.MockExecutor) *executor.MockExpectationBuilder {
	return mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == testHerdrBin && len(cfg.Args) >= 4 &&
			cfg.Args[0] == "agent" && cfg.Args[1] == "prompt"
	})
}

func looksLikePaneCurrent(args []string) bool {
	return len(args) >= 2 && args[0] == "pane" && args[1] == "current"
}

func equalStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func TestPromptErrorCodeLookupOrder(t *testing.T) {
	t.Parallel()
	mock := newHerdrMock()
	expectPrompt(mock).WillReturn(&executor.ExecutionResult{
		Output:   `{"error":{"type":"agent_prompt_stalled"},"code":"timeout"}`,
		ExitCode: 1,
	}, nil).Build()
	out := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", []string{"idle"}, time.Second)
	if out.Status != PromptStalled {
		t.Fatalf("status = %q, want stalled from error.type before top-level code (err=%v)", out.Status, out.Err)
	}
}

func TestRequireMinUsesStderr(t *testing.T) {
	t.Parallel()
	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "--version").WillReturn(&executor.ExecutionResult{
		Stderr:   "herdr 0.8.1\n",
		ExitCode: 0,
	}, nil).Build()
	if err := NewClient(mock, testHerdrBin).RequireMin(context.Background(), "0.8.0"); err != nil {
		t.Fatalf("RequireMin() unexpected error: %v", err)
	}
}

func TestPromptOtherCodeIsError(t *testing.T) {
	t.Parallel()
	mock := newHerdrMock()
	expectPrompt(mock).WillReturn(&executor.ExecutionResult{
		Stderr:   `{"error":{"code":"pane_not_found","message":"nope"}}`,
		ExitCode: 1,
	}, nil).Build()
	out := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w2:pC", "hello", []string{"idle"}, time.Second)
	if out.Status != PromptError {
		t.Fatalf("status = %q, want %q", out.Status, PromptError)
	}
	if out.Err == nil || !strings.Contains(out.Err.Error(), "pane_not_found") {
		t.Fatalf("err = %v, want pane_not_found", out.Err)
	}
}
