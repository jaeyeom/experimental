package herdr

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
)

func TestReadBuildsSnapshotFromFixtures(t *testing.T) {
	run := scriptRunner{
		out: map[string]string{
			"workspace list":          workspaceFixture,
			"tab list --workspace w1": tabFixture,
			"agent list":              agentFixture,
			"pane list":               paneFixture,
		},
	}
	got, err := Read(context.Background(), run.call)
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	want := Snapshot{
		Workspaces: []Workspace{{ID: "w1", Label: "hq"}},
		Tabs:       []Tab{{ID: "w1:t1", Workspace: "w1", Label: "lead"}},
		Agents: []Agent{
			{
				Name: "lead", Pane: "w1:p1", Tab: "w1:t1", Cwd: "/work",
				Status: "idle", Kind: "grok", SessionID: "11111111-1111-1111-1111-111111111111",
			},
			{
				Name: "plain", Pane: "w1:p2", Tab: "w1:t1", Cwd: "/work",
				Status: "working",
			},
		},
		Panes: []Pane{{ID: "w1:p1", Tab: "w1:t1", Cwd: "/work"}},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("snapshot = %#v, want %#v", got, want)
	}
	wantArgs := [][]string{
		{"workspace", "list"},
		{"tab", "list", "--workspace", "w1"},
		{"agent", "list"},
		{"pane", "list"},
	}
	if !reflect.DeepEqual(run.got, wantArgs) {
		t.Fatalf("commands = %#v, want %#v", run.got, wantArgs)
	}
}

func TestReadListsTabsPerWorkspace(t *testing.T) {
	run := scriptRunner{
		out: map[string]string{
			"workspace list": `{
				"result": {"type": "workspace_list", "workspaces": [
					{"workspace_id": "w1", "label": "hq"},
					{"workspace_id": "w2", "label": "lab"}
				]}
			}`,
			"tab list --workspace w1": `{"result":{"type":"tab_list","tabs":[{"tab_id":"w1:t1","workspace_id":"w1","label":"lead"}]}}`,
			"tab list --workspace w2": `{"result":{"type":"tab_list","tabs":[{"tab_id":"w2:t1","workspace_id":"w2","label":"bench"}]}}`,
			"agent list":              `{"result":{"type":"agent_list","agents":[]}}`,
			"pane list":               `{"result":{"type":"pane_list","panes":[]}}`,
		},
	}
	got, err := Read(context.Background(), run.call)
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if len(got.Tabs) != 2 || got.Tabs[0].ID != "w1:t1" || got.Tabs[1].Workspace != "w2" {
		t.Fatalf("tabs = %#v", got.Tabs)
	}
	for _, args := range run.got {
		if !allowed(args) {
			t.Fatalf("command not allowlisted: %#v", args)
		}
	}
}

func TestReadUnreachableHerdr(t *testing.T) {
	run := func(context.Context, []string) ([]byte, error) {
		return nil, errors.New("herdr unreachable")
	}
	_, err := Read(context.Background(), run)
	var nr *NotReadError
	if !errors.As(err, &nr) {
		t.Fatalf("err = %v, want NotReadError", err)
	}
	if err.Error() != "herdr: not read (herdr unreachable)" {
		t.Fatalf("err = %q", err.Error())
	}
}

type scriptRunner struct {
	out map[string]string
	got [][]string
}

func (s *scriptRunner) call(_ context.Context, args []string) ([]byte, error) {
	s.got = append(s.got, append([]string(nil), args...))
	body, ok := s.out[strings.Join(args, " ")]
	if !ok {
		return nil, errors.New("unexpected command: " + strings.Join(args, " "))
	}
	return []byte(body), nil
}

func allowed(args []string) bool {
	switch strings.Join(args, " ") {
	case "workspace list", "agent list", "pane list":
		return true
	default:
		return len(args) == 4 && args[0] == "tab" && args[1] == "list" && args[2] == "--workspace" && args[3] != ""
	}
}

const workspaceFixture = `{
  "id": "1",
  "result": {
    "type": "workspace_list",
    "workspaces": [{
      "workspace_id": "w1",
      "label": "hq",
      "active_tab_id": "w1:t1",
      "focused": true
    }]
  }
}`

const tabFixture = `{
  "id": "2",
  "result": {
    "type": "tab_list",
    "tabs": [{
      "tab_id": "w1:t1",
      "workspace_id": "w1",
      "label": "lead"
    }]
  }
}`

const agentFixture = `{
  "id": "3",
  "result": {
    "type": "agent_list",
    "agents": [
      {
        "agent": "lead",
        "pane_id": "w1:p1",
        "tab_id": "w1:t1",
        "cwd": "/work",
        "agent_status": "idle",
        "agent_session": {
          "source": "herdr:grok",
          "agent": "lead",
          "kind": "id",
          "value": "11111111-1111-1111-1111-111111111111"
        }
      },
      {
        "agent": "plain",
        "pane_id": "w1:p2",
        "tab_id": "w1:t1",
        "cwd": "/work",
        "agent_status": "working"
      }
    ]
  }
}`

const paneFixture = `{
  "id": "4",
  "result": {
    "type": "pane_list",
    "panes": [{
      "pane_id": "w1:p1",
      "tab_id": "w1:t1",
      "cwd": "/work",
      "foreground_cwd": "/other"
    }]
  }
}`
