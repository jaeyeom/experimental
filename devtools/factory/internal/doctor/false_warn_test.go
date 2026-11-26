package doctor

import (
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestDroppedClaimSkipsParents(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"EPIC-1":   jiraItem("EPIC-1", "Epic", "In Progress", "", false, nil),
		"STORY-1":  jiraItem("STORY-1", "story", "In Progress", "EPIC-1", false, nil),
		"UMB-1":    jiraItem("UMB-1", "Task", "In Progress", "EPIC-1", false, nil),
		"CHILD-1":  jiraItem("CHILD-1", "Sub-task", "To Do", "UMB-1", false, nil),
		"DONEPA-1": jiraItem("DONEPA-1", "Task", "In Progress", "EPIC-1", false, nil),
		"DCHILD-1": jiraItem("DCHILD-1", "Sub-task", "Done", "DONEPA-1", true, nil),
		"LEAF-1":   jiraItem("LEAF-1", "Task", "In Progress", "EPIC-1", false, nil),
	}
	in := inputWith(items, func(in *Input) {
		in.Config.ClaimParentTypes = []string{"Epic", "Story"}
	})
	got := checkOnly(t, in, CheckDroppedClaim)
	want := []Finding{
		finding(CheckDroppedClaim, SeverityWarn, "DONEPA-1 is In Progress with no tab and no open PR", "factory release DONEPA-1 or start it", "DONEPA-1"),
		finding(CheckDroppedClaim, SeverityWarn, "LEAF-1 is In Progress with no tab and no open PR", "factory release LEAF-1 or start it", "LEAF-1"),
	}
	equalFindings(t, got, want)
}

func TestDroppedClaimReadsLaneTabs(t *testing.T) {
	t.Parallel()

	young := doctorNow.Add(-time.Minute)
	items := map[string]source.Item{
		"EPIC-1":   jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
		"REMOTE-1": jiraItem("REMOTE-1", "Task", "In Progress", "EPIC-1", false, nil),
		"AWAY-1":   jiraItem("AWAY-1", "Task", "In Progress", "EPIC-1", false, nil),
		"LOCAL-1":  jiraItem("LOCAL-1", "Task", "In Progress", "EPIC-1", false, nil),
	}
	in := inputWith(items, func(in *Input) {
		// LOCAL-1's tab is on the local server, but its claim says mac.
		in.Snapshot.Tabs = []herdr.Tab{{ID: "t-l", WorkspaceID: "w1", Label: "LOCAL-1"}}
		in.Claims = []store.Claim{
			laneClaim("REMOTE-1", "mac", young),
			laneClaim("AWAY-1", "far", young),
			laneClaim("LOCAL-1", "mac", young),
		}
		in.Lanes = map[string]LaneTabs{
			"mac": {Tabs: []herdr.Tab{{ID: "t-r", WorkspaceID: "m1", Label: "REMOTE-1"}}},
			"far": {Unreachable: "host unreachable: ssh timeout"},
		}
	})
	got := checkOnly(t, in, CheckDroppedClaim)
	want := []Finding{
		finding(CheckDroppedClaim, SeverityInfo, "AWAY-1 is In Progress: lane far unreachable; tab not checked", "run doctor again when the lane answers", "AWAY-1"),
		finding(CheckDroppedClaim, SeverityWarn, "LOCAL-1 is In Progress with no tab and no open PR", "factory release LOCAL-1 or start it", "LOCAL-1"),
	}
	equalFindings(t, got, want)
}

func TestUnknownBlockerLookups(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"EPIC-1": jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
		"PRJ-A":  jiraItem("PRJ-A", "Task", "To Do", "EPIC-1", false, nil, "X-DONE", "X-OPEN", "X-GONE", "X-SKIP", "X-NEW"),
	}
	in := inputWith(items, func(in *Input) {
		in.Blockers = map[string]BlockerLookup{
			"X-DONE": {Status: BlockerFound, Item: source.Item{ID: "X-DONE", State: "Done", Done: true}},
			"X-OPEN": {Status: BlockerFound, Item: source.Item{ID: "X-OPEN", State: "In Review", Assignee: "bob"}},
			"X-GONE": {Status: BlockerNotFound},
			"X-SKIP": {Status: BlockerUnchecked, Reason: "over external_blocker_lookups (3)"},
		}
	})
	got := checkOnly(t, in, CheckUnknownBlocker)
	want := []Finding{
		finding(CheckUnknownBlocker, SeverityWarn, "PRJ-A is blocked by X-GONE, which was not found or is not visible", "check the link, or unlink", "PRJ-A", "X-GONE"),
		finding(CheckUnknownBlocker, SeverityWarn, "PRJ-A is blocked by X-NEW, which is not in the mirror", "widen jql, or unlink", "PRJ-A", "X-NEW"),
		finding(CheckUnknownBlocker, SeverityWarn, "PRJ-A is blocked by X-OPEN (In Review, assignee bob), which is not in the mirror", "wait for it, widen jql, or unlink", "PRJ-A", "X-OPEN"),
		finding(CheckUnknownBlocker, SeverityInfo, "PRJ-A is blocked by X-SKIP, which is not in the mirror and was not checked: over external_blocker_lookups (3)", "widen jql, or raise external_blocker_lookups", "PRJ-A", "X-SKIP"),
	}
	equalFindings(t, got, want)
}

func TestOrphanOptions(t *testing.T) {
	t.Parallel()

	options := func(id string) []string {
		return []string{
			"link it to its umbrella item (relates or blocks)",
			"move it under that item's epic",
			"add " + id + " to orphan_ignore",
		}
	}
	items := map[string]source.Item{
		"EPIC-1":   jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
		"UMB-1":    jiraItem("UMB-1", "Task", "To Do", "EPIC-1", false, nil, "LINKED-1"),
		"SUB-1":    jiraItem("SUB-1", "Sub-task", "To Do", "", false, nil),
		"TASK-1":   jiraItem("TASK-1", "task", "To Do", "", false, nil),
		"LINKED-1": jiraItem("LINKED-1", "Task", "To Do", "", false, nil),
	}
	in := inputWith(items, func(in *Input) {
		in.Config.OrphanEpicChildTypes = []string{"Story", "Task", "Bug"}
	})
	got := checkOnly(t, in, CheckOrphan)
	linked := []string{"link it to UMB-1 (relates or blocks)", "move it under UMB-1's epic EPIC-1", "add LINKED-1 to orphan_ignore"}
	want := []Finding{
		finding(CheckOrphan, SeverityWarn, "LINKED-1 has no parent", strings.Join(linked, "; "), "LINKED-1"),
		finding(CheckOrphan, SeverityWarn, "SUB-1 has no parent", strings.Join(options("SUB-1"), "; "), "SUB-1"),
		finding(CheckOrphan, SeverityWarn, "TASK-1 has no parent", "set its parent epic", "TASK-1"),
	}
	want[0].Suggestions = linked
	want[1].Suggestions = options("SUB-1")
	equalFindings(t, got, want)

	raw, err := FormatJSON(got)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(raw, `"suggestions": [`) || strings.Count(raw, `"suggestions"`) != 2 {
		t.Fatalf("json:\n%s", raw)
	}
}

// TestNoIO keeps the doctor package free of I/O: the CLI does lookups and
// herdr calls and passes the results in through Input.
func TestNoIO(t *testing.T) {
	t.Parallel()

	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	banned := map[string]bool{
		"os": true, "os/exec": true, "net": true, "net/http": true, "io/fs": true,
		"database/sql": true, "github.com/jaeyeom/go-cmdexec": true,
	}
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		parsed, err := parser.ParseFile(token.NewFileSet(), file, nil, parser.ImportsOnly)
		if err != nil {
			t.Fatal(err)
		}
		for _, spec := range parsed.Imports {
			path, err := strconv.Unquote(spec.Path.Value)
			if err != nil {
				t.Fatal(err)
			}
			if banned[path] {
				t.Errorf("%s imports %s", file, path)
			}
		}
	}
	if _, err := os.Stat("doctor.go"); err != nil {
		t.Fatalf("expected to run in the doctor package directory: %v", err)
	}
}

func laneClaim(id, lane string, at time.Time) store.Claim {
	c := claim(id, store.Started, at, at)
	c.Lane = lane
	return c
}

func checkOnly(t *testing.T, in Input, check string) []Finding {
	t.Helper()
	var out []Finding
	for _, found := range mustCheck(t, in) {
		if found.Check == check {
			out = append(out, found)
		}
	}
	return out
}
