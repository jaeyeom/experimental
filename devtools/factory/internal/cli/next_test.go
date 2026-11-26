package cli

import (
	"bytes"
	"cmp"
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

var updateGoldens = flag.Bool("update", false, "rewrite golden files under testdata") //nolint:pkgvar // go test golden update flag

const (
	rankWorkspaces = `{
	  "id": "cli:workspace:list",
	  "result": {
	    "type": "workspace_list",
	    "workspaces": [
	      {"workspace_id": "w1", "label": "factory", "agent_status": "idle"}
	    ]
	  }
	}`
	rankTabs = `{
	  "id": "cli:tab:list",
	  "result": {
	    "type": "tab_list",
	    "tabs": [
	      {"tab_id": "t3", "workspace_id": "w1", "label": "PRJ-3", "agent_status": "idle", "pane_count": 1},
	      {"tab_id": "t6", "workspace_id": "w1", "label": "PRJ-6", "agent_status": "working", "pane_count": 1}
	    ]
	  }
	}`
	rankPRs = `{
	  "data": {
	    "search": {
	      "pageInfo": {"hasNextPage": false, "endCursor": null},
	      "nodes": [
	        {
	          "number": 8,
	          "title": "[PRJ-8] review",
	          "url": "https://github.com/acme/widgets/pull/8",
	          "isDraft": false,
	          "merged": false,
	          "mergedAt": null,
	          "createdAt": "2026-01-01T00:00:00Z",
	          "closedAt": null,
	          "headRefName": "feature",
	          "repository": {"nameWithOwner": "acme/widgets"},
	          "closingIssuesReferences": {"nodes": []}
	        },
	        {
	          "number": 3,
	          "title": "[PRJ-3] still blocked",
	          "url": "https://github.com/acme/widgets/pull/3",
	          "isDraft": false,
	          "merged": false,
	          "mergedAt": null,
	          "createdAt": "2026-01-01T00:00:00Z",
	          "closedAt": null,
	          "headRefName": "feature",
	          "repository": {"nameWithOwner": "acme/widgets"},
	          "closingIssuesReferences": {"nodes": []}
	        }
	      ]
	    }
	  }
	}`
	emptyWorkspaces = `{
	  "id": "cli:workspace:list",
	  "result": {"type": "workspace_list", "workspaces": []}
	}`
	emptyTabs = `{
	  "id": "cli:tab:list",
	  "result": {"type": "tab_list", "tabs": []}
	}`
	emptyPanes = `{
	  "id": "cli:pane:list",
	  "result": {"type": "pane_list", "panes": []}
	}`
	emptyPRs = `{
	  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": []}}
	}`
	countTabs = `{
	  "id": "cli:tab:list",
	  "result": {
	    "type": "tab_list",
	    "tabs": [
	      {"tab_id": "tT", "workspace_id": "w1", "label": "PRJ-T", "agent_status": "working", "pane_count": 1}
	    ]
	  }
	}`
	githubWorkspaces = `{
	  "id": "cli:workspace:list",
	  "result": {
	    "type": "workspace_list",
	    "workspaces": [
	      {"workspace_id": "wg", "label": "widgets", "agent_status": "idle"}
	    ]
	  }
	}`
	githubTabs = `{
	  "id": "cli:tab:list",
	  "result": {
	    "type": "tab_list",
	    "tabs": [
	      {"tab_id": "tg", "workspace_id": "wg", "label": "#12", "agent_status": "idle", "pane_count": 2}
	    ]
	  }
	}`
)

var (
	day1 = time.Date(2026, 10, 1, 0, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
	day2 = time.Date(2026, 10, 2, 0, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
	day3 = time.Date(2026, 10, 3, 0, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
)

func TestNextTableGolden(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	lines := dataLines(stdout)
	if len(lines) != 3 {
		t.Fatalf("rows = %d, output:\n%s", len(lines), stdout)
	}
	for i, id := range []string{"CUB-1", "PRJ-2", "PRJ-1"} {
		if !strings.Contains(lines[i], id) {
			t.Fatalf("row %d = %q, want %s\n%s", i, lines[i], id, stdout)
		}
	}
	if strings.Contains(stdout, "ZZZZ") {
		t.Fatalf("title was not truncated:\n%s", stdout)
	}
	if !strings.Contains(stdout, strings.Repeat("a", 59)+"é") {
		t.Fatalf("truncated title missing the 60th rune:\n%s", stdout)
	}
	if strings.Contains(stdout, "PRJ-4") || strings.Contains(stdout, "PRJ-6") || strings.Contains(stdout, "PRJ-8") {
		t.Fatalf("not-ready item was listed:\n%s", stdout)
	}
	assertGolden(t, "next_table.txt", stdout)
}

func TestNextJSONGolden(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath, "--json")
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	var got []map[string]any
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(got) != 3 || got[0]["id"] != "CUB-1" || got[0]["source"] != "jira:cub" {
		t.Fatalf("json items = %#v", got)
	}
	if got[1]["id"] != "PRJ-2" || got[2]["id"] != "PRJ-1" {
		t.Fatalf("order = %v %v", got[1]["id"], got[2]["id"])
	}
	score, _ := got[2]["score"].(map[string]any)
	if score["unblocks"] != float64(2) || score["parent_rank"] != float64(0) || score["priority"] != float64(2) {
		t.Fatalf("score = %#v", score)
	}
	if !strings.Contains(fmt.Sprint(got[1]["title"]), "ZZZZ") {
		t.Fatalf("json title was truncated: %#v", got[2]["title"])
	}
	assertGolden(t, "next_json.txt", stdout)
}

func TestNextExplainGolden(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath, "--explain", "PRJ-GHOST")
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "PASS labels\n") {
		t.Fatalf("missing pass line:\n%s", stdout)
	}
	if !strings.Contains(stdout, "FAIL blockers  blocker NOPE not in mirror (not fetched yet)\n") {
		t.Fatalf("missing fail line:\n%s", stdout)
	}
	if !strings.Contains(stdout, "ancestor  EPIC-A\n") {
		t.Fatalf("missing ancestor:\n%s", stdout)
	}
	if !strings.Contains(stdout, "rank_order  priority, unblocks, parent_rank, created\n") {
		t.Fatalf("missing rank order:\n%s", stdout)
	}
	if !strings.Contains(stdout, "score  priority=2  unblocks=0  parent_rank=0  created=2026-10-01T00:00:00Z\n") {
		t.Fatalf("missing score:\n%s", stdout)
	}
	if strings.Count(stdout, "\n") != 17 {
		t.Fatalf("lines = %d, want 17\n%s", strings.Count(stdout, "\n"), stdout)
	}
	if !strings.Contains(stdout, "args     (default)\n") {
		t.Fatalf("missing agent args:\n%s", stdout)
	}
	assertGolden(t, "next_explain.txt", stdout)
}

func TestNextExplainOpenChildren(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	parent := rankItem("jira:acme", "PRJ-30", "Parent task", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"})
	childA := rankItem("jira:acme", "PRJ-32", "Second sub-task", "Sub-task", "To Do", "PRJ-30", "alice", 2, false, day1, []string{"agent-ready"})
	childB := rankItem("jira:acme", "PRJ-31", "First sub-task", "Sub-task", "To Do", "PRJ-30", "alice", 2, false, day1, []string{"agent-ready"})
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.UpsertItems(context.Background(), "jira:acme", []source.Item{parent, childA, childB}); err != nil {
			t.Fatal(err)
		}
	})

	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("next exit = %d, stderr = %s", code, stderr)
	}
	var seen31, seen32 bool
	for _, line := range dataLines(stdout) {
		fields := strings.Fields(line)
		if len(fields) < 2 {
			continue
		}
		switch fields[1] {
		case "PRJ-30":
			t.Fatalf("parent with open children listed as ready:\n%s", stdout)
		case "PRJ-31":
			seen31 = true
		case "PRJ-32":
			seen32 = true
		}
	}
	if !seen31 || !seen32 {
		t.Fatalf("open children should still be ready:\n%s", stdout)
	}

	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code = runCmd(t, "next", "--explain", "PRJ-30", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "FAIL children  has open child PRJ-31; has open child PRJ-32\n") {
		t.Fatalf("explain missing open-child reason:\n%s", stdout)
	}

	childA.Done = true
	childA.State = "Done"
	childB.Done = true
	childB.State = "Done"
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.UpsertItems(context.Background(), "jira:acme", []source.Item{childA, childB}); err != nil {
			t.Fatal(err)
		}
	})

	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code = runCmd(t, "next", "--explain", "PRJ-30", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain after done exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "PASS children\n") {
		t.Fatalf("explain should pass children once all children are done:\n%s", stdout)
	}
}

func TestNextPriorityBeforeUnblocks(t *testing.T) {
	cfgPath, dbPath := priorityRankFixture(t, nil)
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("next exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	lines := dataLines(stdout)
	if len(lines) != 2 || !strings.Contains(lines[0], "PRJ-1") || !strings.Contains(lines[1], "PRJ-2") {
		t.Fatalf("rows = %v\n%s", lines, stdout)
	}
	if !strings.Contains(stdout, "PRI") || !strings.Contains(stdout, "UNBLOCKS") || !strings.Contains(stdout, "CREATED") {
		t.Fatalf("header missing score columns:\n%s", stdout)
	}
	if strings.Index(stdout, "PRI") > strings.Index(stdout, "UNBLOCKS") {
		t.Fatalf("default columns:\n%s", stdout)
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	explain, stderr, code := runCmd(t, "next", "--explain", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain exit = %d, stderr = %s\n%s", code, stderr, explain)
	}
	if !strings.Contains(explain, "rank_order  priority, unblocks, parent_rank, created\n") {
		t.Fatalf("explain order:\n%s", explain)
	}
	if !strings.Contains(explain, "score  priority=0  unblocks=0  parent_rank=0  created=2026-10-01T00:00:00Z\n") {
		t.Fatalf("explain score:\n%s", explain)
	}
}

func TestNextExplicitRankOrder(t *testing.T) {
	old := []string{"unblocks", "parent_rank", "priority", "created"}
	cfgPath, dbPath := priorityRankFixture(t, old)
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("next exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	lines := dataLines(stdout)
	if len(lines) != 2 || !strings.Contains(lines[0], "PRJ-2") || !strings.Contains(lines[1], "PRJ-1") {
		t.Fatalf("rows = %v\n%s", lines, stdout)
	}
	header := ""
	for _, line := range strings.Split(stdout, "\n") {
		if strings.HasPrefix(strings.TrimSpace(line), "#") {
			header = line
			break
		}
	}
	if strings.Index(header, "UNBLOCKS") > strings.Index(header, "PARENT") ||
		strings.Index(header, "PARENT") > strings.Index(header, "PRI") ||
		strings.Index(header, "PRI") > strings.Index(header, "CREATED") ||
		!strings.HasSuffix(strings.TrimSpace(header), "TITLE") {
		t.Fatalf("explicit columns = %q", header)
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	explain, stderr, code := runCmd(t, "next", "--explain", "PRJ-2", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain exit = %d, stderr = %s\n%s", code, stderr, explain)
	}
	if !strings.Contains(explain, "rank_order  unblocks, parent_rank, priority, created\n") {
		t.Fatalf("explain order:\n%s", explain)
	}
	if !strings.Contains(explain, "score  unblocks=2  parent_rank=0  priority=2  created=2026-10-01T00:00:00Z\n") {
		t.Fatalf("explain score:\n%s", explain)
	}
}

func priorityRankFixture(t *testing.T, rankOrder []string) (string, string) {
	t.Helper()
	cfgPath := writeNextConfig(t, nextSource{name: "acme", rankOrder: rankOrder})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ready := []string{"agent-ready"}
	items := []source.Item{
		rankItem("jira:acme", "PRJ-1", "highest", "Task", "To Do", "", "alice", 0, false, day1, ready),
		rankItem("jira:acme", "PRJ-2", "medium", "Task", "To Do", "", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "PRJ-3", "one", "Task", "To Do", "", "alice", 2, false, day1, ready, "PRJ-2"),
		rankItem("jira:acme", "PRJ-4", "two", "Task", "To Do", "", "alice", 2, false, day1, ready, "PRJ-2"),
	}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, "jira:acme", items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

func TestNextParentBlocksChildren(t *testing.T) {
	defNext, defChild, defParent := parentBlocksRun(t, "")
	ignNext, ignChild, ignParent := parentBlocksRun(t, "ignore")
	if defNext != ignNext || defChild != ignChild || defParent != ignParent {
		t.Fatalf("explicit ignore differed from the default\ndefault child:\n%s\nignore child:\n%s", defChild, ignChild)
	}
	for _, id := range []string{"PRJ-101", "PRJ-102", "PRJ-103"} {
		if !strings.Contains(defNext, id) {
			t.Fatalf("ignore next missing %s:\n%s", id, defNext)
		}
	}
	for _, line := range dataLines(defNext) {
		if fields := strings.Fields(line); len(fields) >= 2 && fields[1] == "PRJ-100" {
			t.Fatalf("ignore next listed parent with open children:\n%s", defNext)
		}
	}
	const ignored = "PASS blockers  ignored: PRJ-100 is the parent of PRJ-101; a parent blocking its own child is usually a linking mistake\n"
	if !strings.Contains(defChild, ignored) {
		t.Fatalf("ignore explain:\n%s", defChild)
	}
	if strings.Contains(defChild, "FAIL blockers") {
		t.Fatalf("ignore explain gated the child:\n%s", defChild)
	}
	if !strings.Contains(defParent, "FAIL children  has open child PRJ-101; has open child PRJ-102; has open child PRJ-103\n") {
		t.Fatalf("ignore parent missing open children reason:\n%s", defParent)
	}
	if !strings.Contains(defParent, "unblocks=0  ") {
		t.Fatalf("ignore parent score:\n%s", defParent)
	}

	honNext, honChild, honParent := parentBlocksRun(t, "honor")
	if !strings.Contains(honNext, "no ready items (4 open, 3 blocked, 1 not eligible)") {
		t.Fatalf("honor next unexpected output:\n%s", honNext)
	}
	for _, id := range []string{"PRJ-101", "PRJ-102", "PRJ-103"} {
		if strings.Contains(honNext, id) {
			t.Fatalf("honor next listed gated child %s:\n%s", id, honNext)
		}
	}
	const honored = "FAIL blockers  honored: PRJ-100 is the parent of PRJ-101; a parent blocking its own child is usually a linking mistake; blocker PRJ-100 is not done\n"
	if !strings.Contains(honChild, honored) {
		t.Fatalf("honor explain:\n%s", honChild)
	}
	if !strings.Contains(honParent, "unblocks=3  ") {
		t.Fatalf("honor parent score:\n%s", honParent)
	}
}

func parentBlocksRun(t *testing.T, mode string) (nextOut, childExplain, parentExplain string) {
	t.Helper()
	cfgPath, dbPath := parentBlocksFixture(t, mode)
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "-n", "10", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("next exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	child, stderr, code := runCmd(t, "next", "--explain", "PRJ-101", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain child exit = %d, stderr = %s\n%s", code, stderr, child)
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	parent, stderr, code := runCmd(t, "next", "--explain", "PRJ-100", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain parent exit = %d, stderr = %s\n%s", code, stderr, parent)
	}
	return stdout, child, parent
}

func parentBlocksFixture(t *testing.T, mode string) (string, string) {
	t.Helper()
	cfgPath := writeNextConfig(t, nextSource{name: "acme", parentBlocks: mode})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ready := []string{"agent-ready"}
	const src = "jira:acme"
	items := []source.Item{
		rankItem(src, "PRJ-100", "umbrella", "Task", "To Do", "", "alice", 2, false, day1, ready),
		rankItem(src, "PRJ-101", "one", "Task", "To Do", "PRJ-100", "alice", 2, false, day1, ready, "PRJ-100"),
		rankItem(src, "PRJ-102", "two", "Task", "To Do", "PRJ-100", "alice", 2, false, day1, ready, "PRJ-100"),
		rankItem(src, "PRJ-103", "three", "Task", "To Do", "PRJ-100", "alice", 2, false, day1, ready, "PRJ-100"),
	}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, src, items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, src, day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, src, "alice"); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

func TestNextParentRankFirst(t *testing.T) {
	parents := []string{"AP-2396", "AP-1106", "AP-1428", "AP-1919"}
	cfgPath := writeNextConfig(t, nextSource{
		name:      "acme",
		parents:   parents,
		rankOrder: []string{"parent_rank", "unblocks", "priority", "created"},
	})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ready := []string{"agent-ready"}
	items := []source.Item{
		rankItem("jira:acme", "AP-2399", "cost down", "Task", "To Do", "AP-2396", "alice", 1, false, day1, ready),
		rankItem("jira:acme", "AP-1107", "second theme", "Task", "To Do", "AP-1106", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "STORY-1", "story", "Story", "To Do", "AP-1428", "", 2, false, day1, nil),
		rankItem("jira:acme", "AP-1430", "sub-task", "Sub-task", "To Do", "STORY-1", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "AP-1920", "fourth theme", "Task", "To Do", "AP-1919", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "DEVOPS-995", "unlisted chain", "Task", "To Do", "DEVOPS-1", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "DEVOPS-A", "follower", "Task", "To Do", "DEVOPS-1", "alice", 2, false, day1, nil, "DEVOPS-995"),
		rankItem("jira:acme", "DEVOPS-B", "follower", "Task", "To Do", "DEVOPS-1", "alice", 2, false, day1, nil, "DEVOPS-995"),
		rankItem("jira:acme", "AP-1201", "other chain", "Task", "To Do", "AP-OTHER", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "AP-1202", "follower", "Task", "To Do", "AP-OTHER", "alice", 2, false, day1, nil, "AP-1201"),
	}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, "jira:acme", items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	lines := dataLines(stdout)
	want := []string{"AP-2399", "AP-1107", "AP-1430", "AP-1920", "DEVOPS-995", "AP-1201"}
	if len(lines) != len(want) {
		t.Fatalf("rows = %#v\n%s", lines, stdout)
	}
	for i, id := range want {
		if !strings.Contains(lines[i], id) {
			t.Fatalf("row %d = %q, want %s\n%s", i, lines[i], id, stdout)
		}
	}

	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "next", "--explain", "AP-1430", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "ancestor  AP-1428\n") {
		t.Fatalf("explain sub-task exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "next", "--explain", "AP-2399", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "ancestor  AP-2396\n") {
		t.Fatalf("explain direct parent exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "next", "--explain", "DEVOPS-995", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "ancestor  none\n") {
		t.Fatalf("explain none exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
}

func TestShowGolden(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "show", "PRJ-3", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	for _, want := range []string{
		"source: jira:acme\n",
		"id: PRJ-3\n",
		"Blocked by:\n",
		"  PRJ-1  To Do  not done\n",
		"  PRJ-9  Done  done\n",
		"  NOPE  not in mirror\n",
		"Blocks:\n",
		"  PRJ-10\n",
		"state: started\n",
		"tab_id: t3\n",
		"label: PRJ-3\n",
		"id: t3\n",
		"https://github.com/acme/widgets/pull/3  [PRJ-3] still blocked\n",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("missing %q\n%s", want, stdout)
		}
	}
	assertGolden(t, "show.txt", stdout)
}

func TestNextHerdrError(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillFail("herdr down", 1).Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q, want empty", stdout)
	}
	if !strings.Contains(stderr, "herdr down") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestNextGHError(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(rankTabs, 0).Build()
	mock.ExpectCommand("gh").WillFail("gh down", 1).Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q, want empty", stdout)
	}
	if !strings.Contains(stderr, "gh down") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestNextNoReadyCounts(t *testing.T) {
	cfgPath, dbPath := countFixture(t)
	useHerdrGH(t, emptyWorkspaces, countTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	const want = "mirror age unknown\nno ready items (7 open, 1 blocked, 5 not eligible)\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
}

func TestNextJSONEmpty(t *testing.T) {
	cfgPath, dbPath := countFixture(t)
	useHerdrGH(t, emptyWorkspaces, countTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath, "--json")
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "[]\n" {
		t.Fatalf("stdout = %q", stdout)
	}
}

func TestNextMissingMe(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	clearMe(t, dbPath, "jira:acme")
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "jira:acme: run factory sync first") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestNextSyncStoresMe(t *testing.T) {
	src := &fake.Source{
		Account: "alice",
		Items: map[string]source.Item{
			"PRJ-1": rankItem("jira:acme", "PRJ-1", "Ship the parser", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"}),
		},
	}
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeNextConfig(t, nextSource{name: "acme", parents: []string{"EPIC-A"}})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	stdout, stderr, code := runCmd(t, "next", "--sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "PRJ-1") || !strings.Contains(stdout, "Ship the parser") {
		t.Fatalf("stdout = %q", stdout)
	}
	if me := storedMe(t, dbPath, "jira:acme"); me != "alice" {
		t.Fatalf("Me = %q", me)
	}
}

func TestNextSourceFilter(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "--source", "jira:cub", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "CUB-1") || strings.Contains(stdout, "PRJ-1") {
		t.Fatalf("stdout = %q", stdout)
	}
	_, stderr, code = runCmd(t, "next", "--source", "nope", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, `found "nope"`) || !strings.Contains(stderr, "jira:acme") {
		t.Fatalf("unknown source exit = %d, stderr = %s", code, stderr)
	}
}

func TestNextLimit(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "-n", "1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	lines := dataLines(stdout)
	if len(lines) != 1 || !strings.Contains(lines[0], "CUB-1") {
		t.Fatalf("rows = %#v\n%s", lines, stdout)
	}
}

func TestNextUsage(t *testing.T) {
	cfgPath := writeNextConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	_, stderr, code := runCmd(t, "next", "-n", "0", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, "found 0") {
		t.Fatalf("bad -n exit = %d, stderr = %q", code, stderr)
	}
	_, stderr, code = runCmd(t, "next", "--explain", "PRJ-1", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || !strings.Contains(stderr, "found both") {
		t.Fatalf("both flags exit = %d, stderr = %q", code, stderr)
	}
	_, stderr, code = runCmd(t, "show", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || !strings.Contains(stderr, "found 0") {
		t.Fatalf("show args exit = %d, stderr = %q", code, stderr)
	}
	_, stderr, code = runCmd(t, "next", "--explain", "PRJ-1", "--human", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || !strings.Contains(stderr, "expected") || !strings.Contains(stderr, "found both") {
		t.Fatalf("--explain --human exit = %d, stderr = %q", code, stderr)
	}
	_, stderr, code = runCmd(t, "next", "--explain", "PRJ-1", "--only-label", "needs-human", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || !strings.Contains(stderr, "expected") || !strings.Contains(stderr, "--only-label") {
		t.Fatalf("--explain --only-label exit = %d, stderr = %q", code, stderr)
	}
	_, stderr, code = runCmd(t, "next", "--mine", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || !strings.Contains(stderr, "expected --mine") || !strings.Contains(stderr, "found --mine alone") {
		t.Fatalf("--mine exit = %d, stderr = %q", code, stderr)
	}
	_, stderr, code = runCmd(t, "next", "--only-label", "", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || !strings.Contains(stderr, "expected a non-empty --only-label") || !strings.Contains(stderr, "found empty") {
		t.Fatalf("empty label exit = %d, stderr = %q", code, stderr)
	}
	_, stderr, code = runCmd(t, "next", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitUsage || !strings.Contains(stderr, "unknown flag: --apply") {
		t.Fatalf("--apply exit = %d, stderr = %q", code, stderr)
	}
}

func TestNextHuman(t *testing.T) {
	db := seedHumanDB(t)
	cfg := humanConfig(t, nil)
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	want := []string{"H-PRI", "H-DUE", "H-A", "H-B", "H-UN", "H-LATE", "H-NIL", "H-OK"}
	absent := []string{"H-EXT", "H-REQ", "H-BLOCK", "H-DONE", "H-OTHER", "H-REV", "H-STUCK", "H-OPEN"}

	t.Run("table", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "--human", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		assertHumanHeader(t, stdout)
		assertIDs(t, stdout, want)
		if !strings.Contains(lineFor(t, stdout, "H-UN"), "H-C, H-E, H-OK") {
			t.Fatalf("H-UN row = %q", lineFor(t, stdout, "H-UN"))
		}
		if !strings.Contains(lineFor(t, stdout, "H-PRI"), "2026-12-01") {
			t.Fatalf("H-PRI row = %q", lineFor(t, stdout, "H-PRI"))
		}
		if strings.Contains(lineFor(t, stdout, "H-NIL"), "2026-") {
			t.Fatalf("H-NIL row = %q", lineFor(t, stdout, "H-NIL"))
		}
		assertAbsent(t, stdout, absent...)
	})

	t.Run("json", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "--human", "--json", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		if strings.Contains(stdout, "mirror age") || !strings.Contains(stderr, "mirror age") {
			t.Fatalf("stdout = %q\nstderr = %q", stdout, stderr)
		}
		assertHumanJSON(t, stdout, want)
	})

	t.Run("mine", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "--human", "--mine", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		assertIDs(t, stdout, []string{"H-DUE", "H-A", "H-B", "H-UN", "H-LATE", "H-OK"})
		assertAbsent(t, stdout, "H-PRI", "H-NIL")
	})

	t.Run("limit", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "-n", "1", "--human", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		assertIDs(t, stdout, []string{"H-PRI"})
	})

	t.Run("only label", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "--only-label", "review", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		assertIDs(t, stdout, []string{"H-REV"})
		stdout, stderr, code = runCmd(t, "next", "--human", "--only-label", "review", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("both exit = %d, stderr = %s", code, stderr)
		}
		assertIDs(t, stdout, []string{"H-REV"})
		assertAbsent(t, stdout, "H-PRI")
		stdout, stderr, code = runCmd(t, "next", "--only-label", "review", "--only-label", "needs-human", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("two labels exit = %d, stderr = %s", code, stderr)
		}
		assertIDs(t, stdout, append(append([]string{}, want...), "H-REV"))
	})

	t.Run("unchanged next", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		if !strings.Contains(stdout, "H-OTHER") || !strings.Contains(stdout, "H-REV") {
			t.Fatalf("stdout = %s", stdout)
		}
		if strings.Contains(stdout, "H-PRI") || strings.Contains(stdout, "KEY") {
			t.Fatalf("stdout = %s", stdout)
		}
	})

	t.Run("empty human labels", func(t *testing.T) {
		emptyCfg := humanConfig(t, []string{})
		stdout, stderr, code := runCmd(t, "next", "--human", "--config", emptyCfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		if !strings.Contains(stdout, "no ready items (0 open, 0 blocked, 0 not eligible)\n") {
			t.Fatalf("stdout = %s", stdout)
		}
		stdout, stderr, code = runCmd(t, "next", "--human", "--json", "--config", emptyCfg, "--db", db)
		if code != ExitOK || stdout != "[]\n" || !strings.Contains(stderr, "mirror age") || strings.Contains(stdout, "mirror age") {
			t.Fatalf("json stdout = %q\nstderr = %q\nexit = %d", stdout, stderr, code)
		}
	})

	t.Run("configured human labels", func(t *testing.T) {
		revCfg := humanConfig(t, []string{"review"})
		stdout, stderr, code := runCmd(t, "next", "--human", "--config", revCfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		assertIDs(t, stdout, []string{"H-REV"})
		assertAbsent(t, stdout, "H-PRI")
	})

	t.Run("source filter", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "--source", "jira:acme", "--human", "--config", cfg, "--db", db)
		if code != ExitOK || !strings.Contains(stdout, "H-PRI") {
			t.Fatalf("exit = %d, stderr = %s\nstdout = %s", code, stderr, stdout)
		}
		_, stderr, code = runCmd(t, "next", "--source", "nope", "--human", "--config", cfg, "--db", db)
		if code != ExitError || !strings.Contains(stderr, `found "nope"`) {
			t.Fatalf("unknown source exit = %d, stderr = %s", code, stderr)
		}
	})

	t.Run("only label blocked", func(t *testing.T) {
		stdout, stderr, code := runCmd(t, "next", "--only-label", "stuck", "--config", cfg, "--db", db)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		if !strings.Contains(stdout, "no ready items (1 open, 1 blocked, 0 not eligible)\n") {
			t.Fatalf("stdout = %s", stdout)
		}
	})

	t.Run("mine drops are not tallied", func(t *testing.T) {
		mineDB := seedMineDropDB(t)
		stdout, stderr, code := runCmd(t, "next", "--human", "--mine", "--config", cfg, "--db", mineDB)
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		if !strings.Contains(stdout, "no ready items (1 open, 1 blocked, 0 not eligible)\n") {
			t.Fatalf("stdout = %s", stdout)
		}
	})
}

func TestNextHumanKeepsLabelThatIsAlsoExcluded(t *testing.T) {
	items := []source.Item{
		humanItem("H-KEEP", "alice", 2, nil, day1, false, []string{"agent-ready", "needs-human", "coord"}),
		humanItem("H-OUT", "alice", 0, nil, day1, false, []string{"agent-ready", "needs-human", "coord", "blocked-external"}),
	}
	db := seedHumanItems(t, items)
	cfg := writeNextConfig(t, nextSource{
		name:          "acme",
		assigneeOK:    "any",
		humanLabels:   []string{"needs-human", "coord"},
		excludeLabels: []string{"needs-human", "coord", "blocked-external"},
	})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)

	stdout, stderr, code := runCmd(t, "next", "--human", "--config", cfg, "--db", db)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	assertIDs(t, stdout, []string{"H-KEEP"})
	assertAbsent(t, stdout, "H-OUT")

	stdout, stderr, code = runCmd(t, "next", "--only-label", "needs-human", "--only-label", "coord", "--config", cfg, "--db", db)
	if code != ExitOK {
		t.Fatalf("only-label exit = %d, stderr = %s", code, stderr)
	}
	assertIDs(t, stdout, []string{"H-KEEP"})
	assertAbsent(t, stdout, "H-OUT")
}

func TestNextHumanJiraDueDate(t *testing.T) {
	search, err := os.ReadFile("../source/jira/testdata/search_due.json")
	if err != nil {
		t.Fatal(err)
	}
	myself, err := os.ReadFile("../source/jira/testdata/myself.json")
	if err != nil {
		t.Fatal(err)
	}
	srv := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if _, err := io.Copy(io.Discard, r.Body); err != nil {
			t.Errorf("read body: %v", err)
		}
		w.Header().Set("Content-Type", "application/json")
		var body []byte
		switch r.Method + " " + r.URL.Path {
		case http.MethodGet + " /rest/api/3/myself":
			body = myself
		case http.MethodPost + " /rest/api/3/search/jql":
			body = search
		default:
			t.Errorf("unexpected %s %s", r.Method, r.URL.Path)
			http.Error(w, "unexpected", http.StatusInternalServerError)
			return
		}
		if _, err := w.Write(body); err != nil {
			t.Errorf("write response: %v", err)
		}
	}))
	t.Cleanup(srv.Close)

	db := filepath.Join(t.TempDir(), "factory.db")
	cfg := writeNextConfig(t, nextSource{name: "acme", site: srv.URL})
	useJiraHTTP(t, srv.Client())
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)

	stdout, stderr, code := runCmd(t, "next", "--human", "--json", "--sync", "--config", cfg, "--db", db)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s\nstdout = %s", code, stderr, stdout)
	}
	var rows []struct {
		Key string  `json:"key"`
		Due *string `json:"due"`
	}
	if err := json.Unmarshal([]byte(stdout), &rows); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(rows) != 1 || rows[0].Key != "PRJ-15" || rows[0].Due == nil || *rows[0].Due != "2026-12-15" {
		t.Fatalf("rows = %+v\n%s", rows, stdout)
	}

	stdout, stderr, code = runCmd(t, "next", "--human", "--config", cfg, "--db", db)
	if code != ExitOK {
		t.Fatalf("table exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(lineFor(t, stdout, "PRJ-15"), "2026-12-15") {
		t.Fatalf("table row = %q", stdout)
	}
}

func TestNextHumanDoesNotClaim(t *testing.T) {
	due := time.Date(2026, 11, 7, 0, 0, 0, 0, time.UTC)
	it := rankItem("jira:acme", "PRJ-1", "Needs a person", "Task", "To Do", "", "alice", 2, false, day1, []string{"agent-ready", "needs-human"})
	it.Due = &due
	src := &fake.Source{
		Account: "alice",
		Items:   map[string]source.Item{"PRJ-1": it},
	}
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfg := writeNextConfig(t, nextSource{name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	stdout, stderr, code := runCmd(t, "next", "--human", "--sync", "--config", cfg, "--db", db)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "PRJ-1") || !strings.Contains(stdout, "2026-11-07") {
		t.Fatalf("stdout = %q", stdout)
	}
	sawSync, sawMe := false, false
	for _, call := range src.Calls {
		switch call.Method {
		case "Claim", "Finish", "Create":
			t.Fatalf("next --human called %s", call.Method)
		case "Sync":
			sawSync = true
		case "Me":
			sawMe = true
		}
	}
	if !sawSync || !sawMe {
		t.Fatalf("calls = %+v", src.Calls)
	}
}

func seedHumanDB(t *testing.T) string {
	t.Helper()
	ready := []string{"agent-ready", "needs-human"}
	items := []source.Item{
		humanItem("H-PRI", "bob", 0, utcDay(2026, 12, 1), day3, false, ready),
		humanItem("H-DUE", "alice", 2, utcDay(2026, 11, 1), day1, false, ready),
		humanItem("H-A", "alice", 2, utcDay(2026, 11, 3), day1, false, ready),
		humanItem("H-B", "alice", 2, utcDay(2026, 11, 3), day1, false, ready),
		humanItem("H-UN", "alice", 2, utcDay(2026, 11, 5), day2, false, ready),
		humanItem("H-LATE", "alice", 2, utcDay(2026, 11, 5), day2, false, ready),
		humanItem("H-NIL", "", 2, nil, day2, false, ready),
		humanItem("H-OK", "alice", 3, nil, day3, false, ready, "H-D"),
		humanItem("H-EXT", "alice", 0, utcDay(2026, 11, 1), day1, false, []string{"agent-ready", "needs-human", "blocked-external"}),
		humanItem("H-REQ", "alice", 0, nil, day1, false, []string{"needs-human"}),
		humanItem("H-BLOCK", "alice", 0, nil, day1, false, ready, "H-OPEN"),
		humanItem("H-DONE", "alice", 0, nil, day1, true, ready),
		humanItem("H-OTHER", "alice", 0, nil, day1, false, []string{"agent-ready"}),
		humanItem("H-REV", "alice", 4, nil, day3, false, []string{"agent-ready", "review"}),
		humanItem("H-STUCK", "alice", 0, nil, day1, false, []string{"agent-ready", "stuck"}, "H-OPEN"),
		humanItem("H-OPEN", "alice", 2, nil, day1, false, nil),
		humanItem("H-C", "alice", 2, nil, day1, false, nil, "H-UN"),
		humanItem("H-D", "alice", 2, nil, day1, true, nil, "H-UN"),
		humanItem("H-E", "alice", 2, nil, day1, false, nil, "H-D"),
	}
	return seedHumanItems(t, items)
}

func seedMineDropDB(t *testing.T) string {
	t.Helper()
	ready := []string{"agent-ready", "needs-human"}
	items := []source.Item{
		humanItem("H-BOB", "bob", 0, nil, day1, false, ready),
		humanItem("H-WAIT", "alice", 1, nil, day1, false, ready, "H-OPEN"),
		humanItem("H-OPEN", "alice", 2, nil, day1, false, nil),
	}
	return seedHumanItems(t, items)
}

func seedHumanItems(t *testing.T, items []source.Item) string {
	t.Helper()
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, "jira:acme", items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day3, day3); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	return dbPath
}

func humanConfig(t *testing.T, labels []string) string {
	t.Helper()
	return writeNextConfig(t, nextSource{
		name:        "acme",
		assigneeOK:  "any",
		rankOrder:   []string{"created", "unblocks", "parent_rank", "priority"},
		humanLabels: labels,
	})
}

func humanItem(id, assignee string, pri int, due *time.Time, created time.Time, done bool, labels []string, blockers ...string) source.Item {
	state := "To Do"
	if done {
		state = "Done"
	}
	it := rankItem("jira:acme", id, id, "Task", state, "", assignee, pri, done, created, labels, blockers...)
	it.Due = due
	return it
}

func utcDay(year int, month time.Month, day int) *time.Time {
	t := time.Date(year, month, day, 0, 0, 0, 0, time.UTC)
	return &t
}

func assertHumanHeader(t *testing.T, stdout string) {
	t.Helper()
	lines := dataLines(stdout)
	if len(lines) == 0 {
		t.Fatalf("stdout = %q", stdout)
	}
	header := lines[0]
	prev := -1
	for _, col := range []string{"KEY", "PRI", "DUE", "TITLE", "URL", "UNBLOCKS"} {
		i := strings.Index(header, col)
		if i < 0 || i <= prev {
			t.Fatalf("header = %q, missing %s in order", header, col)
		}
		prev = i
	}
}

func assertIDs(t *testing.T, stdout string, want []string) {
	t.Helper()
	got := rowIDs(t, stdout)
	if !slices.Equal(got, want) {
		t.Fatalf("ids = %v, want %v\n%s", got, want, stdout)
	}
}

func rowIDs(t *testing.T, stdout string) []string {
	t.Helper()
	lines := dataLines(stdout)
	if len(lines) < 2 {
		t.Fatalf("rows = %#v\n%s", lines, stdout)
	}
	ids := make([]string, 0, len(lines)-1)
	for _, line := range lines[1:] {
		fields := strings.Fields(line)
		if len(fields) == 0 {
			t.Fatalf("empty row %q", line)
		}
		ids = append(ids, fields[0])
	}
	return ids
}

func lineFor(t *testing.T, stdout, id string) string {
	t.Helper()
	for _, line := range dataLines(stdout) {
		fields := strings.Fields(line)
		if len(fields) > 0 && fields[0] == id {
			return line
		}
	}
	t.Fatalf("missing row %s\n%s", id, stdout)
	return ""
}

func assertAbsent(t *testing.T, stdout string, ids ...string) {
	t.Helper()
	for _, id := range ids {
		if strings.Contains(stdout, id) {
			t.Fatalf("stdout contains %s\n%s", id, stdout)
		}
	}
}

func assertHumanJSON(t *testing.T, stdout string, want []string) {
	t.Helper()
	var rows []struct {
		Key      string   `json:"key"`
		Priority int      `json:"priority"`
		Due      *string  `json:"due"`
		Title    string   `json:"title"`
		URL      string   `json:"url"`
		Unblocks []string `json:"unblocks"`
	}
	if err := json.Unmarshal([]byte(stdout), &rows); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(rows) != len(want) {
		t.Fatalf("len = %d, want %d\n%s", len(rows), len(want), stdout)
	}
	var objects []map[string]json.RawMessage
	if err := json.Unmarshal([]byte(stdout), &objects); err != nil {
		t.Fatal(err)
	}
	keys := []string{"key", "priority", "due", "title", "url", "unblocks"}
	for i, row := range rows {
		if row.Key != want[i] {
			t.Fatalf("row %d key = %s, want %s", i, row.Key, want[i])
		}
		raw := string(mustRaw(t, objects[i], "key"))
		pos := strings.Index(stdout, raw)
		for _, key := range keys[1:] {
			next := strings.Index(stdout[pos:], `"`+key+`"`)
			if next <= 0 {
				t.Fatalf("row %d missing %s after key\n%s", i, key, stdout)
			}
			pos += next
		}
		for _, extra := range []string{"source", "score"} {
			if _, ok := objects[i][extra]; ok {
				t.Fatalf("row %d has %s", i, extra)
			}
		}
	}
	if rows[0].Priority != 0 || rows[0].Due == nil || *rows[0].Due != "2026-12-01" || len(rows[0].Unblocks) != 0 {
		t.Fatalf("first = %+v", rows[0])
	}
	if !strings.Contains(stdout, `"unblocks": []`) {
		t.Fatalf("empty unblocks encoded as null\n%s", stdout)
	}
	nilRow := rows[6]
	if nilRow.Key != "H-NIL" || nilRow.Due != nil {
		t.Fatalf("H-NIL = %+v", nilRow)
	}
	un := rows[4]
	if un.Key != "H-UN" || !slices.Equal(un.Unblocks, []string{"H-C", "H-E", "H-OK"}) {
		t.Fatalf("H-UN = %+v", un)
	}
}

func mustRaw(t *testing.T, obj map[string]json.RawMessage, key string) json.RawMessage {
	t.Helper()
	raw, ok := obj[key]
	if !ok {
		t.Fatalf("missing %s", key)
	}
	return raw
}

func TestShowAmbiguousID(t *testing.T) {
	cfgPath, dbPath := ambiguousFixture(t)
	stdout, stderr, code := runCmd(t, "show", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "jira:acme") || !strings.Contains(stderr, "jira:cub") || !strings.Contains(stderr, "expected one source") {
		t.Fatalf("stderr = %q", stderr)
	}
	_, stderr, code = runCmd(t, "next", "--explain", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, "expected one source") {
		t.Fatalf("explain exit = %d, stderr = %s", code, stderr)
	}
}

func TestNextExplainUnknown(t *testing.T) {
	cfgPath, dbPath := rankingFixture(t)
	stdout, stderr, code := runCmd(t, "next", "--explain", "MISSING", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, "found none") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func TestShowFindsMergedPRByTitle(t *testing.T) {
	cfgPath := writeNextConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := rankItem("jira:acme", "PRJ-9", "Landed work", "Task", "Done", "EPIC-A", "alice", 2, true, day1, nil)
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	merged := `{
	  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": [{
	    "number": 9,
	    "title": "[PRJ-9] landed",
	    "url": "https://github.com/acme/widgets/pull/9",
	    "isDraft": false,
	    "merged": true,
	    "mergedAt": "2026-10-29T18:00:00Z",
	    "createdAt": "2026-01-01T00:00:00Z",
	    "closedAt": null,
	    "headRefName": "feature",
	    "repository": {"nameWithOwner": "acme/widgets"},
	    "closingIssuesReferences": {"nodes": []}
	  }]}}
	}`
	other := `{
	  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": [{
	    "number": 8,
	    "title": "[PRJ-99] other",
	    "url": "https://github.com/acme/widgets/pull/8",
	    "isDraft": false,
	    "merged": true,
	    "mergedAt": "2026-10-29T18:00:00Z",
	    "createdAt": "2026-01-01T00:00:00Z",
	    "closedAt": null,
	    "headRefName": "feature",
	    "repository": {"nameWithOwner": "acme/widgets"},
	    "closingIssuesReferences": {"nodes": []}
	  }]}}
	}`
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(merged, 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "show", "PRJ-9", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "https://github.com/acme/widgets/pull/9  [PRJ-9] landed\n") {
		t.Fatalf("stdout = %s", stdout)
	}
	mock = executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(other, 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code = runCmd(t, "show", "PRJ-9", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("filtered exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "PRs:\n  none\n") {
		t.Fatalf("stdout = %s", stdout)
	}
}

func TestShowGitHubTab(t *testing.T) {
	cfgPath := writeGitHubConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := rankItem("github:experimental", "acme/widgets#12", "GitHub issue", "issue", "open", "", "alice", 1, false, day1, []string{"agent-ready"})
		it.URL = "https://github.com/acme/widgets/issues/12"
		if err := st.UpsertItems(ctx, "github:experimental", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "github:experimental", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "github:experimental", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	useHerdrGH(t, githubWorkspaces, githubTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "show", "acme/widgets#12", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	for _, want := range []string{
		"workspace: widgets\n",
		"label: #12\n",
		"id: tg\n",
		"Blocked by:\n  none\n",
		"PRs:\n  none\n",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("missing %q\n%s", want, stdout)
		}
	}
}

func rankingFixture(t *testing.T) (string, string) {
	t.Helper()
	cfgPath := writeNextConfig(t,
		nextSource{name: "acme", parents: []string{"EPIC-A"}},
		nextSource{name: "cub"},
	)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	longTitle := strings.Repeat("a", 59) + "é" + "ZZZZ"
	acme := []source.Item{
		rankItem("jira:acme", "PRJ-1", "Ship the parser", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"}, "PRJ-9"),
		rankItem("jira:acme", "PRJ-2", longTitle, "Task", "To Do", "EPIC-B", "", 1, false, day2, []string{"agent-ready"}),
		rankItem("jira:acme", "PRJ-3", "Waiting on parser", "Task", "To Do", "EPIC-A", "alice", 2, false, day3, []string{"agent-ready"}, "PRJ-1", "PRJ-9", "NOPE"),
		rankItem("jira:acme", "PRJ-4", "Needs a label", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, nil),
		rankItem("jira:acme", "PRJ-5", "Already done", "Task", "Done", "EPIC-A", "alice", 2, true, day1, []string{"agent-ready"}),
		rankItem("jira:acme", "PRJ-6", "Has a tab", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"}),
		rankItem("jira:acme", "PRJ-8", "Has a pull request", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"}),
		rankItem("jira:acme", "PRJ-9", "Blocker done", "Task", "Done", "EPIC-A", "alice", 2, true, day1, []string{"agent-ready"}),
		rankItem("jira:acme", "PRJ-10", "Follow-up", "Task", "To Do", "EPIC-A", "alice", 2, false, day2, []string{"agent-ready"}, "PRJ-3"),
		rankItem("jira:acme", "PRJ-GHOST", "Missing blocker", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"}, "NOPE"),
		rankItem("jira:acme", "EPIC-A", "Parent epic", "Epic", "To Do", "", "", 2, false, day1, nil),
	}
	cub := []source.Item{
		rankItem("jira:cub", "CUB-1", "Cub task", "Task", "To Do", "", "bob", 0, false, day3, []string{"agent-ready"}),
	}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, "jira:acme", acme); err != nil {
			t.Fatal(err)
		}
		if err := st.UpsertItems(ctx, "jira:cub", cub); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day3, day3); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:cub", day3, day3); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:cub", "bob"); err != nil {
			t.Fatal(err)
		}
		claimedAt := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-3", ClaimedAt: claimedAt, Machine: "laptop",
			TabID: "t3", WorkspaceID: "w1", State: store.Started, Detail: "started",
			UpdatedAt: claimedAt.Add(5 * time.Minute),
		}); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

func countFixture(t *testing.T) (string, string) {
	t.Helper()
	cfgPath := writeNextConfig(t, nextSource{name: "acme", allow: []string{"EPIC-OK"}})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ready := []string{"agent-ready"}
	items := []source.Item{
		rankItem("jira:acme", "PRJ-D", "done", "Task", "Done", "EPIC-OK", "alice", 2, true, day1, ready),
		rankItem("jira:acme", "PRJ-B", "blocked", "Task", "To Do", "EPIC-OK", "alice", 2, false, day1, ready, "GONE"),
		rankItem("jira:acme", "PRJ-L", "no label", "Task", "To Do", "EPIC-OK", "alice", 2, false, day1, nil),
		rankItem("jira:acme", "PRJ-E", "epic", "Epic", "To Do", "EPIC-OK", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "PRJ-S", "status", "Task", "Backlog", "EPIC-OK", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "PRJ-A", "assignee", "Task", "To Do", "EPIC-OK", "carol", 2, false, day1, ready),
		rankItem("jira:acme", "PRJ-P", "parent", "Task", "To Do", "EPIC-NO", "alice", 2, false, day1, ready),
		rankItem("jira:acme", "PRJ-T", "tab", "Task", "To Do", "EPIC-OK", "alice", 2, false, day1, ready),
	}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, "jira:acme", items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

func ambiguousFixture(t *testing.T) (string, string) {
	t.Helper()
	cfgPath := writeNextConfig(t, nextSource{name: "acme"}, nextSource{name: "cub"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		for _, src := range []string{"jira:acme", "jira:cub"} {
			it := rankItem(src, "PRJ-1", "Same id", "Task", "To Do", "", "alice", 2, false, day1, []string{"agent-ready"})
			if err := st.UpsertItems(ctx, src, []source.Item{it}); err != nil {
				t.Fatal(err)
			}
			if err := st.SetSyncState(ctx, src, day1, day1); err != nil {
				t.Fatal(err)
			}
			if err := st.SetMe(ctx, src, "alice"); err != nil {
				t.Fatal(err)
			}
		}
	})
	return cfgPath, dbPath
}

type nextSource struct {
	name          string
	parents       []string
	allow         []string
	rankOrder     []string
	parentBlocks  string
	assigneeOK    string
	humanLabels   []string
	excludeLabels []string
	site          string
	cwd           string // Empty uses one temp dir shared by every source.
	extra         string // More TOML lines at the end of the source table.
}

func writeNextConfig(t *testing.T, sources ...nextSource) string {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var body strings.Builder
	for _, src := range sources {
		site := "https://example.atlassian.net"
		if src.site != "" {
			site = src.site
		}
		fmt.Fprintf(&body, `
[[source]]
name = %q
kind = "jira"
site = %q
email = "dev@example.com"
token_file = %q
jql = "project = PRJ"
claim_status = "In Progress"
cwd = %q
base = "main"
start_prompt = "work {id}"
workspace = "factory"
require_labels = ["agent-ready"]
ready_statuses = ["To Do"]
`, src.name, site, token, cmp.Or(src.cwd, cwd))
		if len(src.parents) > 0 {
			fmt.Fprintf(&body, "parent_priority = [%s]\n", quotedList(src.parents))
		}
		if len(src.rankOrder) > 0 {
			fmt.Fprintf(&body, "rank_order = [%s]\n", quotedList(src.rankOrder))
		}
		if len(src.allow) > 0 {
			fmt.Fprintf(&body, "parent_allowlist = [%s]\n", quotedList(src.allow))
		}
		if src.parentBlocks != "" {
			fmt.Fprintf(&body, "parent_blocks_child = %q\n", src.parentBlocks)
		}
		if src.assigneeOK != "" {
			fmt.Fprintf(&body, "assignee_ok = %q\n", src.assigneeOK)
		}
		if src.humanLabels != nil {
			fmt.Fprintf(&body, "human_labels = [%s]\n", quotedList(src.humanLabels))
		}
		if src.excludeLabels != nil {
			fmt.Fprintf(&body, "exclude_labels = [%s]\n", quotedList(src.excludeLabels))
		}
		body.WriteString(src.extra)
	}
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body.String()), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func writeGitHubConfig(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	body := fmt.Sprintf(`
[[source]]
name = "experimental"
kind = "github"
repo = "acme/widgets"
cwd = %q
base = "main"
start_prompt = "work {id}"
workspace = "widgets"
require_labels = ["agent-ready"]
`, cwd)
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func quotedList(ids []string) string {
	parts := make([]string, len(ids))
	for i, id := range ids {
		parts[i] = fmt.Sprintf("%q", id)
	}
	return strings.Join(parts, ", ")
}

func rankItem(src, id, title, typ, state, parent, assignee string, pri int, done bool, created time.Time, labels []string, blockers ...string) source.Item {
	refs := make([]source.BlockRef, len(blockers))
	for i, blocker := range blockers {
		refs[i] = source.BlockRef{ID: blocker, LinkID: fmt.Sprintf("%s-%d", id, i)}
	}
	return source.Item{
		Source: src, ID: id, Title: title, URL: "https://example.test/browse/" + id,
		Type: typ, State: state, Done: done, Parent: parent, Labels: labels,
		Assignee: assignee, Priority: pri, BlockedBy: refs, Created: created, Updated: created,
	}
}

func seedDB(t *testing.T, path string, seed func(*store.Store)) {
	t.Helper()
	st, err := store.Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	seed(st)
	if err := st.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func clearMe(t *testing.T, path, sourceName string) {
	t.Helper()
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer func() {
		if err := db.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	}()
	res, err := db.ExecContext(context.Background(), `UPDATE sync_state SET me = NULL WHERE source = ?`, sourceName)
	if err != nil {
		t.Fatalf("clear me: %v", err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		t.Fatalf("clear me rows: %v", err)
	}
	if n != 1 {
		t.Fatalf("clear me: expected 1 row, found %d", n)
	}
}

func useExec(t *testing.T, exec executor.Executor) {
	t.Helper()
	prev := newCommandExec
	newCommandExec = func() executor.Executor { return exec }
	t.Cleanup(func() { newCommandExec = prev })
}

func useJiraHTTP(t *testing.T, hc *http.Client) {
	t.Helper()
	prev := newSource
	newSource = func(cfg *config.Source, deps Deps) (source.Source, error) {
		deps.HTTP = hc
		return defaultSource(cfg, deps)
	}
	t.Cleanup(func() { newSource = prev })
}

func useHerdrGH(t *testing.T, workspaces, tabs, prs string) {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(workspaces, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Build()
	mock.ExpectCommand("gh").WillSucceed(prs, 0).Build()
	useExec(t, mock)
}

func runCmd(t *testing.T, args ...string) (string, string, int) {
	t.Helper()
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), args, &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}

func dataLines(text string) []string {
	var out []string
	for _, line := range strings.Split(strings.TrimSuffix(text, "\n"), "\n") {
		if line == "" || strings.HasPrefix(strings.TrimSpace(line), "#") || strings.HasPrefix(line, "mirror age ") {
			continue
		}
		out = append(out, line)
	}
	return out
}

func assertGolden(t *testing.T, name, got string) {
	t.Helper()
	path := filepath.Join("testdata", name)
	if *updateGoldens {
		if err := os.MkdirAll("testdata", 0o700); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(got), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := fs.ReadFile(os.DirFS("testdata"), name)
	if err != nil {
		t.Fatalf("read golden %s: %v (go test -update)", path, err)
	}
	if string(want) != got {
		t.Fatalf("golden %s mismatch\n got:\n%s\nwant:\n%s", name, got, want)
	}
}
