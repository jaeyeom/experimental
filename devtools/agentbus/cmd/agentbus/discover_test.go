package main

import (
	"encoding/json"
	"reflect"
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

func TestListJSON(t *testing.T) {
	runtime := shortRuntime(t)
	base := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	live := "herdr:local/default/w1:p1"
	gone := "herdr:local/default/w1:p2"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{live: adapter.StateIdle, gone: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": live, "p2": gone},
	}
	stop := startBus(t, dirs, ad)
	t.Cleanup(stop)

	empty, _ := mustCmd(t, []string{"list", "--json"}, base)
	if strings.TrimSpace(empty) != "[]" {
		t.Fatalf("empty list --json = %q", empty)
	}

	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews pull requests"}, append(append([]string{}, base...), "HERDR_PANE_ID=p1"))
	mustCmd(t, []string{"register", "ops/notes", "--when", "writes release notes"}, append(append([]string{}, base...), "HERDR_PANE_ID=p2"))
	ad.Reach[gone] = adapter.StateGone

	out, _ := mustCmd(t, []string{"list", "--json"}, base)
	var got []map[string]any
	if err := json.Unmarshal([]byte(out), &got); err != nil {
		t.Fatalf("list --json = %q: %v", out, err)
	}
	want := []map[string]any{
		{"name": "ops/inbox", "when": "reviews pull requests", "liveness": "alive", "transport": "herdr", "handle": live},
		{"name": "ops/notes", "when": "writes release notes", "liveness": "dead", "transport": "herdr", "handle": gone},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("list --json = %v, want %v", got, want)
	}
}

func TestListJSONWithoutDaemon(t *testing.T) {
	env := []string{"XDG_RUNTIME_DIR=" + shortRuntime(t), "XDG_STATE_HOME=" + t.TempDir(), "HOME=" + t.TempDir()}
	code, stdout, stderr := runCmd([]string{"list", "--json"}, env)
	if code == 0 || stdout != "" || stderr == "" {
		t.Fatalf("list --json without daemon = %d stdout %q stderr %q", code, stdout, stderr)
	}
}

func TestListRejectsUnknownFlag(t *testing.T) {
	noDaemon(t)
	if code, _, stderr := runCmd([]string{"list", "--yaml"}, helpEnv); code != 2 || !strings.Contains(stderr, "unexpected arguments") {
		t.Fatalf("list --yaml = %d %q", code, stderr)
	}
}

func TestJSONLiveness(t *testing.T) {
	for in, want := range map[string]string{
		"alive": "alive", "blocked": "alive", "gone": "dead", "unreachable": "unknown", "": "unknown",
	} {
		if got := jsonLiveness(in); got != want {
			t.Errorf("jsonLiveness(%q) = %q, want %q", in, got, want)
		}
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
