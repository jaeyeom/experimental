package daemon

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

func TestServerDialogsBuiltinConfiguredAndMachineFilter(t *testing.T) {
	const (
		localPerm   = "herdr:local/s1/w1:p1"
		localIdle   = "herdr:local/s1/w1:p2"
		remoteCust  = "herdr:m2/s1/w2:p1"
		claudeRule  = "──────────────────────────────"
		idleScreen  = "Earlier text: Do you want to proceed?\n" + claudeRule + "\n❯ \n" + claudeRule + "\n"
		permScreen  = "⏺ Bash(ls)\nDo you want to proceed?\n❯ 1. Yes\n"
		trustScreen = "Do you trust the files in this folder? [y/N]\n"
	)
	ad := &fake.Adapter{
		AdapterName: "herdr",
		InventoryAgents: []adapter.Agent{
			{Handle: localPerm, PaneID: "w1:p1", Kind: "claude", Status: "blocked"},
			{Handle: localIdle, PaneID: "w1:p2", Kind: "claude", Status: "idle"},
			{Handle: remoteCust, PaneID: "w2:p1", Kind: "codex", Status: "idle"},
		},
		PaneHandles: map[string]string{"w1:p1": localPerm, "w1:p2": localIdle, "w2:p1": remoteCust},
		Reach:       map[string]adapter.State{localPerm: adapter.StateBlocked, localIdle: adapter.StateIdle, remoteCust: adapter.StateIdle},
		PaneScreens: map[string]string{
			localPerm:  permScreen,
			localIdle:  idleScreen,
			remoteCust: trustScreen,
		},
	}
	reg, err := registry.Open(t.TempDir(), []adapter.Adapter{ad}, nil)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := reg.Register(t.Context(), []string{"HERDR_PANE_ID=w1:p1"}, "ops/perm", "testing"); err != nil {
		t.Fatal(err)
	}
	srv := &server{
		ctx:         t.Context(),
		reg:         reg,
		adapters:    []adapter.Adapter{ad},
		dialogRules: []config.DialogRule{{Name: "trust", Pattern: `Do you trust.*\? \[y/N\]`}},
	}

	resp := srv.dialogs(Request{Op: "dialogs"})
	if !resp.OK {
		t.Fatalf("dialogs failed: %s", resp.Error)
	}
	if len(resp.Dialogs) != 2 {
		t.Fatalf("Dialogs = %+v, want 2", resp.Dialogs)
	}
	if resp.Dialogs[0].Pane != "w1:p1" || resp.Dialogs[0].Name != "ops/perm" || resp.Dialogs[0].Machine != "local" || resp.Dialogs[0].Rule != "permission prompt" {
		t.Fatalf("Dialogs[0] = %+v", resp.Dialogs[0])
	}
	if resp.Dialogs[1].Pane != "w2:p1" || resp.Dialogs[1].Name != "" || resp.Dialogs[1].Machine != "m2" || resp.Dialogs[1].Rule != "trust" {
		t.Fatalf("Dialogs[1] = %+v", resp.Dialogs[1])
	}

	m2 := srv.dialogs(Request{Op: "dialogs", Machine: "m2"})
	if !m2.OK || len(m2.Dialogs) != 1 || m2.Dialogs[0].Pane != "w2:p1" {
		t.Fatalf("Machine=m2 = %+v", m2)
	}
}
