package cli

import (
	"context"
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

// outsideFake mirrors PRJ-1 (blocked only by a Done X-DONE), PRJ-2 (blocked
// by an open X-OPEN), and PRJ-3 (blocked by X-GONE, which does not exist).
// X-* are outside the source query and answer Get only.
func outsideFake() *fake.Source {
	ref := func(id string) []source.BlockRef { return []source.BlockRef{{ID: id}} }
	return &fake.Source{
		SourceName: "jira:acme",
		Account:    "ada",
		Items: map[string]source.Item{
			"PRJ-1":  {Source: "jira:acme", ID: "PRJ-1", Title: "Fix it", Type: "Task", State: "To Do", BlockedBy: ref("X-DONE"), Created: day1, Updated: day1},
			"PRJ-2":  {Source: "jira:acme", ID: "PRJ-2", Title: "Two", Type: "Task", State: "To Do", BlockedBy: ref("X-OPEN"), Created: day1, Updated: day1},
			"PRJ-3":  {Source: "jira:acme", ID: "PRJ-3", Title: "Three", Type: "Task", State: "To Do", BlockedBy: ref("X-GONE"), Created: day1, Updated: day1},
			"X-DONE": {Source: "jira:acme", ID: "X-DONE", Title: "Other team", Type: "Task", State: "Done", Done: true, Assignee: "bob", Updated: day1},
			"X-OPEN": {Source: "jira:acme", ID: "X-OPEN", Title: "Other team", Type: "Task", State: "In Review", Assignee: "bob", Updated: day1},
		},
		Query: func(it source.Item) bool { return !strings.HasPrefix(it.ID, "X-") },
	}
}

func TestOutsideBlockersResolvedInSync(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := outsideFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	stdout, stderr, code := runCmd(t, "sync", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("sync exit = %d, stderr = %s", code, stderr)
	}
	var rows []struct {
		Source   string           `json:"source"`
		External *syncer.External `json:"external_blockers"` //nolint:tagliatelle // documented factory JSON output field
	}
	if err := json.Unmarshal([]byte(stdout), &rows); err != nil {
		t.Fatalf("sync --json: %v\n%s", err, stdout)
	}
	want := syncer.External{Referenced: 3, Fetched: 3, Done: 1}
	if len(rows) != 1 || rows[0].Source != "jira:acme" || rows[0].External == nil || *rows[0].External != want {
		t.Fatalf("sync --json:\n%s", stdout)
	}

	useHerdrGH(t, rankWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("next exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "PRJ-1") || strings.Contains(stdout, "PRJ-2") || strings.Contains(stdout, "PRJ-3") || strings.Contains(stdout, "X-") {
		t.Fatalf("next:\n%s", stdout)
	}

	for id, want := range map[string]string{
		"PRJ-2": "FAIL blockers  blocker X-OPEN is In Review (outside mirror, assigned to bob)\n",
		"PRJ-3": "FAIL blockers  blocker X-GONE not found or no access\n",
	} {
		stdout, stderr, code := runCmd(t, "next", "--explain", id, "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || !strings.Contains(stdout, want) {
			t.Fatalf("explain %s exit = %d, stderr = %s\n%s", id, code, stderr, stdout)
		}
	}

	stdout, _, code = runCmd(t, "show", "PRJ-2", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "  X-OPEN  In Review  not done  (outside mirror, assigned to bob)\n") {
		t.Fatalf("show exit = %d\n%s", code, stdout)
	}

	stdout, _, code = runCmd(t, "stats", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || strings.Contains(stdout, "X-") {
		t.Fatalf("stats exit = %d, mentions an external blocker:\n%s", code, stdout)
	}

	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	stdout, stderr, code = runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != startPlan(cwd) {
		t.Fatalf("start exit = %d, stderr = %s\nstdout =\n%s", code, stderr, stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	for _, call := range src.Calls {
		if call.Method == "Claim" {
			t.Fatalf("dry run claimed: %#v", src.Calls)
		}
	}
}

func TestOutsideBlockerNotFetchedRefusesStart(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	seedDB(t, dbPath, func(st *store.Store) {
		it := outsideFake().Items["PRJ-1"]
		if err := st.UpsertItems(context.Background(), "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
	})
	src := outsideFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	_, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused || !strings.Contains(stderr, "blocker X-DONE not in mirror (not fetched yet)") {
		t.Fatalf("start exit = %d, stderr = %s", code, stderr)
	}
	assertNoClaimCall(t, src)
}

func TestSyncTextCountsOutsideBlockers(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	useSources(t, map[string]*fake.Source{"acme": outsideFake()})
	stdout, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("sync exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "jira:acme  full  3 items (0 removed)  outside blockers: 3 fetched, 1 done, 0 skipped\n" {
		t.Fatalf("stdout = %q", stdout)
	}
}
