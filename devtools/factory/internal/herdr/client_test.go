package herdr

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const testHerdrBin = "/tmp/factory-herdr-fake"

func TestRequireMin(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		output  string
		stderr  string
		execErr error
		wantErr error
	}{
		{name: "equal to minimum", output: "herdr 0.9.0\n"},
		{name: "newer patch", output: "herdr 0.9.5\n"},
		{name: "newer major", output: "herdr 1.0.0\n"},
		{name: "short form", output: "herdr 0.9\n"},
		{name: "version on stderr", stderr: "herdr 0.9.5\n"},
		{name: "too old", output: "herdr 0.8.9\n", wantErr: ErrUnsupported},
		{name: "unparseable", output: "herdr bananas\n", wantErr: ErrUnsupported},
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
				exp.WillReturn(&executor.ExecutionResult{
					Output:   tc.output,
					Stderr:   tc.stderr,
					ExitCode: 0,
				}, nil).Build()
			}
			err := NewClient(mock, testHerdrBin).RequireMin(context.Background(), "0.9.0")
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

func TestWorkspaces(t *testing.T) {
	t.Parallel()

	const body = `{
	  "id": "cli:workspace:list",
	  "result": {
	    "type": "workspace_list",
	    "workspaces": [
	      {
	        "active_tab_id": "w1:tG",
	        "agent_status": "unknown",
	        "focused": false,
	        "label": "experimental",
	        "number": 1,
	        "pane_count": 1,
	        "tab_count": 1,
	        "workspace_id": "w1"
	      }
	    ]
	  }
	}`

	t.Run("argv and envelope", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "workspace", "list").WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Workspaces(context.Background())
		if err != nil {
			t.Fatalf("Workspaces() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].ID != "w1" || got[0].Label != "experimental" || got[0].AgentStatus != AgentStatusUnknown {
			t.Fatalf("Workspaces() = %+v", got)
		}
	})

	t.Run("missing workspaces is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "workspace", "list").
			WillSucceed(`{"id":"cli:workspace:list","result":{"type":"workspace_list"}}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Workspaces(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Workspaces() error = %v, want ErrDegraded", err)
		}
	})

	t.Run("empty workspaces is ok", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "workspace", "list").
			WillSucceed(`{"id":"cli:workspace:list","result":{"type":"workspace_list","workspaces":[]}}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Workspaces(context.Background())
		if err != nil {
			t.Fatalf("Workspaces() unexpected error: %v", err)
		}
		if got == nil || len(got) != 0 {
			t.Fatalf("Workspaces() = %#v, want empty slice", got)
		}
	})

	t.Run("missing binary", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "workspace", "list").
			WillError(&executor.ExecutableNotFoundError{Command: testHerdrBin}).Build()
		_, err := NewClient(mock, testHerdrBin).Workspaces(context.Background())
		if !errors.Is(err, ErrNotInstalled) {
			t.Fatalf("Workspaces() error = %v, want ErrNotInstalled", err)
		}
	})
}

func TestTabs(t *testing.T) {
	t.Parallel()

	const body = `{
	  "id": "cli:tab:list",
	  "result": {
	    "type": "tab_list",
	    "tabs": [
	      {
	        "agent_status": "idle",
	        "focused": false,
	        "label": "PRJ-2366",
	        "number": 131,
	        "pane_count": 1,
	        "tab_id": "w3:t43",
	        "workspace_id": "w3"
	      }
	    ]
	  }
	}`

	t.Run("argv and envelope", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if err != nil {
			t.Fatalf("Tabs() unexpected error: %v", err)
		}
		if len(got) != 1 {
			t.Fatalf("Tabs() len = %d, want 1", len(got))
		}
		want := Tab{ID: "w3:t43", WorkspaceID: "w3", Label: "PRJ-2366", AgentStatus: AgentStatusIdle, PaneCount: 1}
		if got[0] != want {
			t.Fatalf("Tabs()[0] = %+v, want %+v", got[0], want)
		}
	})

	t.Run("missing tabs is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").
			WillSucceed(`{"id":"cli:tab:list","result":{"type":"tab_list"}}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Tabs() error = %v, want ErrDegraded", err)
		}
		if !strings.Contains(err.Error(), "tabs") {
			t.Fatalf("Tabs() error = %q, want it to name tabs", err)
		}
	})

	t.Run("empty tabs is ok", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").
			WillSucceed(`{"id":"cli:tab:list","result":{"type":"tab_list","tabs":[]}}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if err != nil {
			t.Fatalf("Tabs() unexpected error: %v", err)
		}
		if got == nil || len(got) != 0 {
			t.Fatalf("Tabs() = %#v, want empty slice", got)
		}
	})

	t.Run("missing pane_count is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").WillSucceed(`{
		  "id": "cli:tab:list",
		  "result": {
		    "type": "tab_list",
		    "tabs": [{
		      "agent_status": "idle",
		      "label": "PRJ-1",
		      "tab_id": "w1:t1",
		      "workspace_id": "w1"
		    }]
		  }
		}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Tabs() error = %v, want ErrDegraded", err)
		}
		if !strings.Contains(err.Error(), "pane_count") {
			t.Fatalf("Tabs() error = %q, want it to name pane_count", err)
		}
	})

	t.Run("zero pane_count is ok", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").WillSucceed(`{
		  "id": "cli:tab:list",
		  "result": {
		    "type": "tab_list",
		    "tabs": [{
		      "agent_status": "done",
		      "label": "PRJ-1",
		      "pane_count": 0,
		      "tab_id": "w1:t1",
		      "workspace_id": "w1"
		    }]
		  }
		}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if err != nil {
			t.Fatalf("Tabs() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].PaneCount != 0 || got[0].AgentStatus != AgentStatusDone {
			t.Fatalf("Tabs() = %+v", got)
		}
	})

	t.Run("unexpected agent status", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").WillSucceed(`{
		  "id": "cli:tab:list",
		  "result": {
		    "type": "tab_list",
		    "tabs": [{
		      "agent_status": "sleeping",
		      "label": "PRJ-1",
		      "pane_count": 1,
		      "tab_id": "w1:t1",
		      "workspace_id": "w1"
		    }]
		  }
		}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Tabs() error = %v, want ErrDegraded", err)
		}
		msg := err.Error()
		if !strings.Contains(msg, "expected") || !strings.Contains(msg, `"sleeping"`) {
			t.Fatalf("Tabs() error = %q, want expected and found status", msg)
		}
	})

	t.Run("wrong result type", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").
			WillSucceed(`{"id":"cli:tab:list","result":{"type":"workspace_list","tabs":[]}}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Tabs() error = %v, want ErrDegraded", err)
		}
		msg := err.Error()
		if !strings.Contains(msg, "tab_list") || !strings.Contains(msg, "workspace_list") {
			t.Fatalf("Tabs() error = %q, want expected and found type", msg)
		}
	})

	t.Run("missing envelope id", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").
			WillSucceed(`{"result":{"type":"tab_list","tabs":[]}}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Tabs() error = %v, want ErrDegraded", err)
		}
	})

	t.Run("nonzero exit", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").WillReturn(&executor.ExecutionResult{
			Stderr:   "herdr: boom",
			ExitCode: 2,
		}, nil).Build()
		_, err := NewClient(mock, testHerdrBin).Tabs(context.Background())
		var proc *ProcError
		if !errors.As(err, &proc) {
			t.Fatalf("Tabs() error = %v, want ProcError", err)
		}
		if proc.ExitCode != 2 || proc.Error() != "herdr: boom" {
			t.Fatalf("ProcError = %+v, error %q", proc, proc.Error())
		}
	})
}

func TestCloseTab(t *testing.T) {
	t.Parallel()

	const closed = `{
	  "id": "cli:tab:close",
	  "result": {"type": "tab_closed", "tab_id": "w1:t1", "workspace_id": "w1"}
	}`

	t.Run("success", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w1:t1").WillSucceed(closed, 0).Build()
		got, err := NewClient(mock, testHerdrBin).CloseTab(context.Background(), "w1:t1")
		if err != nil {
			t.Fatalf("CloseTab() unexpected error: %v", err)
		}
		if got != "tab_closed" {
			t.Fatalf("CloseTab() = %q, want tab_closed", got)
		}
	})

	t.Run("ok", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w1:t1").WillSucceed(`{
		  "id": "cli:tab:close",
		  "result": {"type": "ok"}
		}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).CloseTab(context.Background(), "w1:t1")
		if err != nil {
			t.Fatalf("CloseTab() unexpected error: %v", err)
		}
		if got != "ok" {
			t.Fatalf("CloseTab() = %q, want ok", got)
		}
	})

	t.Run("other success type", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w1:t1").WillSucceed(`{
		  "id": "cli:tab:close",
		  "result": {"type": "config_reload"}
		}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).CloseTab(context.Background(), "w1:t1")
		if err != nil {
			t.Fatalf("CloseTab() unexpected error: %v", err)
		}
		if got != "config_reload" {
			t.Fatalf("CloseTab() = %q, want config_reload", got)
		}
	})

	t.Run("empty id", func(t *testing.T) {
		t.Parallel()
		_, err := NewClient(newHerdrMock(), testHerdrBin).CloseTab(context.Background(), "")
		if err == nil || !strings.Contains(err.Error(), "expected a tab id, found empty") {
			t.Fatalf("CloseTab() error = %v", err)
		}
	})

	t.Run("tab not found", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w1:missing").WillReturn(&executor.ExecutionResult{
			Output:   `{"error":{"code":"tab_not_found","message":"tab w1:missing not found"},"id":"cli:tab:close"}`,
			ExitCode: 1,
		}, nil).Build()
		_, err := NewClient(mock, testHerdrBin).CloseTab(context.Background(), "w1:missing")
		if !errors.Is(err, ErrTabNotFound) {
			t.Fatalf("CloseTab() error = %v, want ErrTabNotFound", err)
		}
	})

	t.Run("wrong tab id", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w1:other").WillSucceed(closed, 0).Build()
		_, err := NewClient(mock, testHerdrBin).CloseTab(context.Background(), "w1:other")
		if err == nil || !strings.Contains(err.Error(), "expected tab w1:other, found w1:t1") {
			t.Fatalf("CloseTab() error = %v", err)
		}
	})

	t.Run("other error", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "tab", "close", "w1:t1").WillReturn(&executor.ExecutionResult{
			Stderr:   "herdr: boom",
			ExitCode: 1,
		}, nil).Build()
		_, err := NewClient(mock, testHerdrBin).CloseTab(context.Background(), "w1:t1")
		var proc *ProcError
		if !errors.As(err, &proc) {
			t.Fatalf("CloseTab() error = %v, want ProcError", err)
		}
	})
}

func TestCreateWorkspace(t *testing.T) {
	t.Parallel()

	const body = `{
	  "id": "cli:workspace:create",
	  "result": {
	    "type": "workspace_created",
	    "workspace": {
	      "workspace_id": "w9",
	      "number": 9,
	      "label": "factory",
	      "focused": false,
	      "pane_count": 1,
	      "tab_count": 1,
	      "active_tab_id": "w9:t1",
	      "agent_status": "idle"
	    },
	    "tab": {
	      "tab_id": "w9:t1",
	      "workspace_id": "w9",
	      "number": 1,
	      "label": "factory",
	      "focused": false,
	      "pane_count": 1,
	      "agent_status": "idle"
	    },
	    "root_pane": {
	      "pane_id": "w9:p1",
	      "terminal_id": "w9:term1",
	      "workspace_id": "w9",
	      "tab_id": "w9:t1",
	      "focused": true,
	      "agent_status": "idle",
	      "revision": 1
	    }
	  }
	}`

	t.Run("argv and envelope", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "workspace", "create", "--label", "factory", "--cwd", "/work", "--no-focus").
			WillSucceed(body, 0).Build()
		ws, pane, err := NewClient(mock, testHerdrBin).CreateWorkspace(context.Background(), "factory", "/work", nil)
		if err != nil {
			t.Fatalf("CreateWorkspace() unexpected error: %v", err)
		}
		if ws.ID != "w9" || ws.Label != "factory" || ws.AgentStatus != AgentStatusIdle {
			t.Fatalf("workspace = %+v", ws)
		}
		if pane != (Pane{ID: "w9:p1", TabID: "w9:t1", WorkspaceID: "w9", AgentStatus: AgentStatusIdle}) {
			t.Fatalf("pane = %+v", pane)
		}
	})

	t.Run("missing root pane is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "workspace", "create", "--label", "factory", "--cwd", "/work", "--no-focus").
			WillSucceed(`{"id":"cli:workspace:create","result":{"type":"workspace_created","workspace":{"workspace_id":"w9","label":"factory","agent_status":"idle"},"tab":{"tab_id":"w9:t1","workspace_id":"w9","label":"factory","agent_status":"idle","pane_count":1}}}`, 0).Build()
		_, _, err := NewClient(mock, testHerdrBin).CreateWorkspace(context.Background(), "factory", "/work", nil)
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("CreateWorkspace() error = %v, want ErrDegraded", err)
		}
	})

	t.Run("missing tab is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "workspace", "create", "--label", "factory", "--cwd", "/work", "--no-focus").
			WillSucceed(`{"id":"cli:workspace:create","result":{"type":"workspace_created","workspace":{"workspace_id":"w9","label":"factory","agent_status":"idle"},"root_pane":{"pane_id":"w9:p1","tab_id":"w9:t1","workspace_id":"w9"}}}`, 0).Build()
		_, _, err := NewClient(mock, testHerdrBin).CreateWorkspace(context.Background(), "factory", "/work", nil)
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("CreateWorkspace() error = %v, want ErrDegraded", err)
		}
	})
}

func TestCreateTab(t *testing.T) {
	t.Parallel()

	const body = `{
	  "id": "cli:tab:create",
	  "result": {
	    "type": "tab_created",
	    "tab": {
	      "tab_id": "w3:t44",
	      "workspace_id": "w3",
	      "number": 132,
	      "label": "PRJ-2367",
	      "focused": false,
	      "pane_count": 1,
	      "agent_status": "idle"
	    },
	    "root_pane": {
	      "pane_id": "w3:p44",
	      "terminal_id": "w3:term44",
	      "workspace_id": "w3",
	      "tab_id": "w3:t44",
	      "focused": false,
	      "agent_status": "idle",
	      "revision": 1
	    }
	  }
	}`

	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "tab", "create", "--workspace", "w3", "--label", "PRJ-2367", "--cwd", "/repo",
		"--env", "FACTORY_ITEM_SOURCE=jira:acme", "--no-focus").
		WillSucceed(body, 0).Build()
	tab, pane, err := NewClient(mock, testHerdrBin).CreateTab(context.Background(), "w3", "PRJ-2367", "/repo", []string{"FACTORY_ITEM_SOURCE=jira:acme"})
	if err != nil {
		t.Fatalf("CreateTab() unexpected error: %v", err)
	}
	want := Tab{ID: "w3:t44", WorkspaceID: "w3", Label: "PRJ-2367", AgentStatus: AgentStatusIdle, PaneCount: 1}
	if tab != want {
		t.Fatalf("tab = %+v, want %+v", tab, want)
	}
	if pane.ID != "w3:p44" || pane.TabID != "w3:t44" || pane.WorkspaceID != "w3" {
		t.Fatalf("pane = %+v", pane)
	}
}

func TestStartAgent(t *testing.T) {
	t.Parallel()

	const body = `{
	  "id": "cli:agent:start",
	  "result": {
	    "type": "agent_started",
	    "agent": {
	      "terminal_id": "w9:term1",
	      "agent_status": "idle",
	      "workspace_id": "w9",
	      "tab_id": "w9:t1",
	      "pane_id": "w9:p1",
	      "focused": true,
	      "revision": 2
	    },
	    "argv": ["claude"]
	  }
	}`

	t.Run("argv and envelope", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin,
			"agent", "start", "factory", "--kind", "claude", "--pane", "w9:p1", "--timeout", "30000",
		).WillSucceed(body, 0).Build()
		err := NewClient(mock, testHerdrBin).StartAgent(context.Background(), "factory", "claude", "w9:p1", 30*time.Second, nil)
		if err != nil {
			t.Fatalf("StartAgent() unexpected error: %v", err)
		}
		assertCallTimeout(t, mock, 30*time.Second+promptTimeoutGrace)
	})

	t.Run("agent args follow --", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin,
			"agent", "start", "factory", "--kind", "claude", "--pane", "w9:p1", "--timeout", "30000",
			"--", "--model", "opus",
		).WillSucceed(body, 0).Build()
		err := NewClient(mock, testHerdrBin).StartAgent(context.Background(), "factory", "claude", "w9:p1", 30*time.Second, []string{"--model", "opus"})
		if err != nil {
			t.Fatalf("StartAgent() unexpected error: %v", err)
		}
	})

	t.Run("missing agent is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin,
			"agent", "start", "factory", "--kind", "claude", "--pane", "w9:p1", "--timeout", "1000",
		).WillSucceed(`{"id":"cli:agent:start","result":{"type":"agent_started","argv":["claude"]}}`, 0).Build()
		err := NewClient(mock, testHerdrBin).StartAgent(context.Background(), "factory", "claude", "w9:p1", time.Second, nil)
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("StartAgent() error = %v, want ErrDegraded", err)
		}
	})
}

func TestPrompt(t *testing.T) {
	t.Parallel()

	const body = `{
	  "id": "cli:agent:prompt",
	  "result": {
	    "type": "agent_prompted",
	    "agent": {
	      "terminal_id": "w9:term1",
	      "agent_status": "working",
	      "workspace_id": "w9",
	      "tab_id": "w9:t1",
	      "pane_id": "w9:p1",
	      "focused": true,
	      "revision": 3
	    }
	  }
	}`

	until := []string{AgentStatusWorking, AgentStatusBlocked}
	timeout := 30 * time.Second

	t.Run("argv and envelope", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin,
			"agent", "prompt", "w9:p1", "do the work", "--wait",
			"--until", "working", "--until", "blocked", "--timeout", "30000",
		).WillSucceed(body, 0).Build()
		err := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w9:p1", "do the work", until, timeout)
		if err != nil {
			t.Fatalf("Prompt() unexpected error: %v", err)
		}
		assertCallTimeout(t, mock, timeout+promptTimeoutGrace)
	})

	t.Run("nonzero exit", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin,
			"agent", "prompt", "w9:p1", "do the work", "--wait", "--timeout", "1000",
		).WillReturn(&executor.ExecutionResult{
			Stderr:   `{"error":{"code":"timeout","message":"wait exceeded timeout_ms"}}`,
			ExitCode: 1,
		}, nil).Build()
		err := NewClient(mock, testHerdrBin).Prompt(context.Background(), "w9:p1", "do the work", nil, time.Second)
		var proc *ProcError
		if !errors.As(err, &proc) {
			t.Fatalf("Prompt() error = %v, want ProcError", err)
		}
		if proc.ExitCode != 1 {
			t.Fatalf("exit = %d, want 1", proc.ExitCode)
		}
	})
}

func TestFindWorkspace(t *testing.T) {
	t.Parallel()

	ws := []Workspace{
		{ID: "w1", Label: "experimental", AgentStatus: AgentStatusUnknown},
		{ID: "w2", Label: "factory", AgentStatus: AgentStatusIdle},
		{ID: "w3", Label: "factory", AgentStatus: AgentStatusWorking},
	}

	t.Run("one match", func(t *testing.T) {
		t.Parallel()
		got, ok, err := FindWorkspace(ws, "experimental")
		if err != nil || !ok || got.ID != "w1" {
			t.Fatalf("FindWorkspace() = (%+v, %v, %v)", got, ok, err)
		}
	})

	t.Run("none", func(t *testing.T) {
		t.Parallel()
		got, ok, err := FindWorkspace(ws, "missing")
		if err != nil || ok || got != (Workspace{}) {
			t.Fatalf("FindWorkspace() = (%+v, %v, %v), want none", got, ok, err)
		}
	})

	t.Run("ambiguous", func(t *testing.T) {
		t.Parallel()
		_, ok, err := FindWorkspace(ws, "factory")
		if ok || !errors.Is(err, ErrAmbiguous) {
			t.Fatalf("FindWorkspace() = (ok %v, err %v), want ErrAmbiguous", ok, err)
		}
		msg := err.Error()
		if !strings.Contains(msg, "w2") || !strings.Contains(msg, "w3") {
			t.Fatalf("error %q does not include both ids", msg)
		}
	})
}

func TestFindTab(t *testing.T) {
	t.Parallel()

	tabs := []Tab{
		{ID: "w1:t1", WorkspaceID: "w1", Label: "#8", AgentStatus: AgentStatusIdle, PaneCount: 1},
		{ID: "w2:t1", WorkspaceID: "w2", Label: "#8", AgentStatus: AgentStatusDone, PaneCount: 1},
		{ID: "w2:t2", WorkspaceID: "w2", Label: "#8", AgentStatus: AgentStatusIdle, PaneCount: 2},
		{ID: "w2:t3", WorkspaceID: "w2", Label: "PRJ-1", AgentStatus: AgentStatusWorking, PaneCount: 1},
	}

	t.Run("scoped match", func(t *testing.T) {
		t.Parallel()
		got, ok, err := FindTab(tabs, "w2", "PRJ-1")
		if err != nil || !ok || got.ID != "w2:t3" {
			t.Fatalf("FindTab() = (%+v, %v, %v)", got, ok, err)
		}
	})

	t.Run("empty workspace searches all", func(t *testing.T) {
		t.Parallel()
		got, ok, err := FindTab(tabs, "", "PRJ-1")
		if err != nil || !ok || got.ID != "w2:t3" {
			t.Fatalf("FindTab() = (%+v, %v, %v)", got, ok, err)
		}
	})

	t.Run("scoped ambiguous", func(t *testing.T) {
		t.Parallel()
		_, ok, err := FindTab(tabs, "w2", "#8")
		if ok || !errors.Is(err, ErrAmbiguous) {
			t.Fatalf("FindTab() = (ok %v, err %v), want ErrAmbiguous", ok, err)
		}
		msg := err.Error()
		if !strings.Contains(msg, "w2:t1") || !strings.Contains(msg, "w2:t2") {
			t.Fatalf("error %q does not include both ids", msg)
		}
	})

	t.Run("unscoped ambiguous", func(t *testing.T) {
		t.Parallel()
		_, ok, err := FindTab([]Tab{tabs[0], tabs[3], {ID: "w9:t1", WorkspaceID: "w9", Label: "PRJ-1"}}, "", "PRJ-1")
		if ok || !errors.Is(err, ErrAmbiguous) {
			t.Fatalf("FindTab() = (ok %v, err %v), want ErrAmbiguous", ok, err)
		}
	})

	t.Run("other workspace is not a match", func(t *testing.T) {
		t.Parallel()
		got, ok, err := FindTab(tabs, "w1", "PRJ-1")
		if err != nil || ok || got != (Tab{}) {
			t.Fatalf("FindTab() = (%+v, %v, %v), want none", got, ok, err)
		}
	})
}

func TestAnyWorking(t *testing.T) {
	t.Parallel()

	tabs := []Tab{
		{ID: "w1:t1", AgentStatus: AgentStatusIdle},
		{ID: "w1:t2", AgentStatus: AgentStatusBlocked},
		{ID: "w1:t3", AgentStatus: AgentStatusWorking},
		{ID: "w1:t4", AgentStatus: AgentStatusWorking},
	}
	got, ok := AnyWorking(tabs)
	if !ok || got.ID != "w1:t3" {
		t.Fatalf("AnyWorking() = (%+v, %v), want the first working tab", got, ok)
	}
	if _, ok := AnyWorking([]Tab{{ID: "w1:t1", AgentStatus: AgentStatusDone}}); ok {
		t.Fatal("AnyWorking() ok = true, want false")
	}
	if _, ok := AnyWorking(nil); ok {
		t.Fatal("AnyWorking(nil) ok = true, want false")
	}
}

func TestPanes(t *testing.T) {
	t.Parallel()

	const body = `{
	  "id": "cli:pane:list",
	  "result": {
	    "type": "pane_list",
	    "panes": [
	      {
	        "agent_status": "working",
	        "focused": true,
	        "label": "coord",
	        "pane_id": "w1:p1",
	        "tab_id": "w1:t1",
	        "workspace_id": "w1"
	      }
	    ]
	  }
	}`

	t.Run("null cwd is empty", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "list").WillSucceed(`{
		  "id": "cli:pane:list",
		  "result": {
		    "type": "pane_list",
		    "panes": [{
		      "agent_status": "working",
		      "cwd": null,
		      "foreground_cwd": "/src/pkg",
		      "pane_id": "w1:p1",
		      "tab_id": "w1:t1",
		      "workspace_id": "w1"
		    }]
		  }
		}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Panes(context.Background())
		if err != nil {
			t.Fatalf("Panes() unexpected error: %v", err)
		}
		if len(got) != 1 || got[0].Cwd != "" || got[0].ForegroundCwd != "/src/pkg" {
			t.Fatalf("Panes() = %+v", got)
		}
	})

	t.Run("cwd number is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "list").WillSucceed(`{
		  "id": "cli:pane:list",
		  "result": {
		    "type": "pane_list",
		    "panes": [{
		      "agent_status": "idle",
		      "cwd": 1,
		      "pane_id": "w1:p1",
		      "tab_id": "w1:t1",
		      "workspace_id": "w1"
		    }]
		  }
		}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Panes(context.Background())
		if !errors.Is(err, ErrDegraded) || !strings.Contains(err.Error(), "string or null") {
			t.Fatalf("Panes() error = %v", err)
		}
	})

	t.Run("argv and extra fields", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "list").WillSucceed(body, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Panes(context.Background())
		if err != nil {
			t.Fatalf("Panes() unexpected error: %v", err)
		}
		want := []Pane{{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: AgentStatusWorking}}
		if len(got) != 1 || got[0] != want[0] {
			t.Fatalf("Panes() = %+v, want %+v", got, want)
		}
	})

	t.Run("empty list is non-nil", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "list").
			WillSucceed(`{"id":"cli:pane:list","result":{"type":"pane_list","panes":[]}}`, 0).Build()
		got, err := NewClient(mock, testHerdrBin).Panes(context.Background())
		if err != nil {
			t.Fatalf("Panes() unexpected error: %v", err)
		}
		if got == nil || len(got) != 0 {
			t.Fatalf("Panes() = %#v, want empty slice", got)
		}
	})

	t.Run("missing agent_status is degraded", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "list").WillSucceed(`{
		  "id": "cli:pane:list",
		  "result": {
		    "type": "pane_list",
		    "panes": [{
		      "pane_id": "w1:p1",
		      "tab_id": "w1:t1",
		      "workspace_id": "w1"
		    }]
		  }
		}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Panes(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Panes() error = %v, want ErrDegraded", err)
		}
		if !strings.Contains(err.Error(), "agent_status") {
			t.Fatalf("Panes() error = %q, want it to name agent_status", err)
		}
	})

	t.Run("unexpected agent status", func(t *testing.T) {
		t.Parallel()
		mock := newHerdrMock()
		mock.ExpectCommandWithArgs(testHerdrBin, "pane", "list").WillSucceed(`{
		  "id": "cli:pane:list",
		  "result": {
		    "type": "pane_list",
		    "panes": [{
		      "agent_status": "sleeping",
		      "pane_id": "w1:p1",
		      "tab_id": "w1:t1",
		      "workspace_id": "w1"
		    }]
		  }
		}`, 0).Build()
		_, err := NewClient(mock, testHerdrBin).Panes(context.Background())
		if !errors.Is(err, ErrDegraded) {
			t.Fatalf("Panes() error = %v, want ErrDegraded", err)
		}
		msg := err.Error()
		if !strings.Contains(msg, "pane \"w1:p1\"") || !strings.Contains(msg, `"sleeping"`) {
			t.Fatalf("Panes() error = %q", msg)
		}
	})
}

func newHerdrMock() *executor.MockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testHerdrBin, true)
	return mock
}

func assertCallTimeout(t *testing.T, mock *executor.MockExecutor, want time.Duration) {
	t.Helper()
	history := mock.GetCallHistory()
	if len(history) != 1 {
		t.Fatalf("calls = %d, want 1", len(history))
	}
	if history[0].Config.Timeout != want {
		t.Fatalf("process timeout = %s, want %s", history[0].Config.Timeout, want)
	}
}

func TestCreateRejectsBadEnv(t *testing.T) {
	client := NewClient(newHerdrMock(), testHerdrBin)
	for _, kv := range []string{"NOEQUALS", "=value"} {
		if _, _, err := client.CreateTab(context.Background(), "w1", "T", "/repo", []string{kv}); err == nil {
			t.Fatalf("CreateTab env %q: expected an error", kv)
		}
		if _, _, err := client.CreateWorkspace(context.Background(), "ws", "/repo", []string{kv}); err == nil {
			t.Fatalf("CreateWorkspace env %q: expected an error", kv)
		}
	}
}
