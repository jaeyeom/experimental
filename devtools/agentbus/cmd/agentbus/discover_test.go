package main

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestSearchFindsWhenAndUnregisteredTitle(t *testing.T) {
	runtime := shortRuntime(t)
	stateHome := t.TempDir()
	home := t.TempDir()
	base := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + stateHome,
		"HOME=" + home,
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	holder := "herdr:local/default/w1:p1"
	raw := "herdr:local/default/w2:p3"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{holder: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": holder},
		InventoryAgents: []adapter.Agent{{
			Handle: holder, PaneID: "p1", SessionID: "default", TabLabel: "inbox", Title: "Holder", Status: "idle",
		}},
	}
	stop := startBus(t, dirs, ad)
	t.Cleanup(stop)
	env := append(append([]string{}, base...), "HERDR_PANE_ID=p1")
	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews pull requests"}, env)

	listed, _ := mustCmd(t, []string{"list"}, base)
	rows := parseRows(listed)
	if len(rows) != 1 || rows[0]["name"] != "ops/inbox" || rows[0]["when"] != "reviews pull requests" || rows[0]["liveness"] != "alive" || rows[0]["transport"] != "herdr" {
		t.Fatalf("list = %q", listed)
	}

	byWhen, _ := mustCmd(t, []string{"search", "pull requests"}, base)
	whenRows := parseRows(byWhen)
	if len(whenRows) != 1 || whenRows[0]["name"] != "ops/inbox" || whenRows[0]["when"] != "reviews pull requests" {
		t.Fatalf("search when = %q", byWhen)
	}

	miss, _ := mustCmd(t, []string{"search", "Release notes"}, base)
	if strings.TrimSpace(miss) != "" {
		t.Fatalf("search before the agent appeared = %q", miss)
	}
	ad.InventoryAgents = append(ad.InventoryAgents, adapter.Agent{
		Handle: raw, PaneID: "p3", SessionID: "default", TabLabel: "notes", Title: "Release notes", Status: "idle",
	})
	byTitle, _ := mustCmd(t, []string{"search", "Release notes"}, base)
	titleRows := parseRows(byTitle)
	if len(titleRows) != 1 || titleRows[0]["handle"] != raw || titleRows[0]["title"] != "Release notes" || titleRows[0]["name"] != "" {
		t.Fatalf("search title = %q", byTitle)
	}
}

func parseRows(stdout string) []map[string]string {
	var rows []map[string]string
	cur := map[string]string{}
	flush := func() {
		if len(cur) == 0 {
			return
		}
		rows = append(rows, cur)
		cur = map[string]string{}
	}
	for _, line := range strings.Split(stdout, "\n") {
		if strings.TrimSpace(line) == "" {
			flush()
			continue
		}
		key, val, ok := strings.Cut(line, ": ")
		if !ok {
			continue
		}
		cur[key] = val
	}
	flush()
	return rows
}
