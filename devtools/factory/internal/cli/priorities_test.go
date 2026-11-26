package cli

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestPrioritiesReport(t *testing.T) {
	cfgPath, dbPath := seedPriorityDB(t, "jira:acme", exampleChain("jira:acme"))
	stdout, stderr, code := runCmd(t, "priorities", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	const want = "" +
		"PRJ-10  priority Medium (2)  blocks PRJ-20 (Highest, 0) via PRJ-10 → PRJ-11 → PRJ-20  suggest Highest\n" +
		"PRJ-11  priority Medium (2)  blocks PRJ-20 (Highest, 0)  suggest Highest\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s", stdout)
	}
	if got := storedPriority(t, dbPath, "jira:acme", "PRJ-10"); got != 2 {
		t.Fatalf("priority = %d, want unchanged 2", got)
	}
}

func TestPrioritiesExplainAndDoctor(t *testing.T) {
	cfgPath, dbPath := seedPriorityDB(t, "jira:acme", exampleChain("jira:acme"))
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--explain", "PRJ-10", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain blocker exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	const blocker = "inversion  blocks PRJ-20 priority Highest (0), this priority Medium (2), via PRJ-10 → PRJ-11 → PRJ-20\n"
	if !strings.Contains(stdout, blocker) {
		t.Fatalf("explain blocker =\n%s", stdout)
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "next", "--explain", "PRJ-20", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("explain target exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "inversion  blocked by PRJ-10 priority Medium (2), this priority Highest (0), via PRJ-10 → PRJ-11 → PRJ-20\n") {
		t.Fatalf("missing transitive target line\n%s", stdout)
	}
	if !strings.Contains(stdout, "inversion  blocked by PRJ-11 priority Medium (2), this priority Highest (0)\n") {
		t.Fatalf("missing direct target line\n%s", stdout)
	}
	doctorToolOK(t)
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != 1 || stderr != "" {
		t.Fatalf("doctor exit = %d, want 1, stderr = %s\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "priority-inversion") || !strings.Contains(stdout, "factory priorities --apply") {
		t.Fatalf("doctor =\n%s", stdout)
	}
	if got := storedPriority(t, dbPath, "jira:acme", "PRJ-10"); got != 2 {
		t.Fatalf("doctor wrote priority %d", got)
	}
}

func TestPrioritiesApply(t *testing.T) {
	items := exampleChain("jira:acme")
	cfgPath, dbPath := seedPriorityDB(t, "jira:acme", items)
	src := &fake.Source{SourceName: "jira:acme", Items: copyItems(items)}
	useSources(t, map[string]*fake.Source{"acme": src})
	stdout, stderr, code := runCmd(t, "priorities", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	const want = "" +
		"PRJ-10  priority Medium (2)  blocks PRJ-20 (Highest, 0) via PRJ-10 → PRJ-11 → PRJ-20  suggest Highest\n" +
		"PRJ-11  priority Medium (2)  blocks PRJ-20 (Highest, 0)  suggest Highest\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s", stdout)
	}
	if got := storedPriority(t, dbPath, "jira:acme", "PRJ-10"); got != 0 {
		t.Fatalf("PRJ-10 priority = %d, want 0", got)
	}
	if got := storedPriority(t, dbPath, "jira:acme", "PRJ-11"); got != 0 {
		t.Fatalf("PRJ-11 priority = %d, want 0", got)
	}
	if got := storedPriority(t, dbPath, "jira:acme", "PRJ-20"); got != 0 {
		t.Fatalf("PRJ-20 priority = %d, want 0", got)
	}
	var sets [][]string
	for _, call := range src.Calls {
		if call.Method == "SetPriority" {
			sets = append(sets, call.Args)
		}
	}
	if len(sets) != 2 || sets[0][0] != "PRJ-10" || sets[1][0] != "PRJ-11" {
		t.Fatalf("SetPriority calls = %#v", sets)
	}
}

func TestPrioritiesApplyNeedsAName(t *testing.T) {
	items := []source.Item{
		rankItem("github:experimental", "acme/widgets#10", "low", "Issue", "OPEN", "", "", 4, false, day1, nil),
		rankItem("github:experimental", "acme/widgets#20", "high", "Issue", "OPEN", "", "", 2, false, day1, nil, "acme/widgets#10"),
	}
	cfgPath := writeGitHubConfig(t)
	appendConfig(t, cfgPath, "\npriority_field = \"\"\n")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.UpsertItems(context.Background(), "github:experimental", items); err != nil {
			t.Fatal(err)
		}
	})
	src := &fake.Source{SourceName: "github:experimental", Items: copyItems(items)}
	useSources(t, map[string]*fake.Source{"experimental": src})
	stdout, stderr, code := runCmd(t, "priorities", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stderr, "expected one name for rank 2") || !strings.Contains(stderr, "found none") {
		t.Fatalf("stderr = %q", stderr)
	}
	for _, call := range src.Calls {
		if call.Method == "SetPriority" {
			t.Fatalf("wrote without a name: %#v", src.Calls)
		}
	}
	if got := storedPriority(t, dbPath, "github:experimental", "acme/widgets#10"); got != 4 {
		t.Fatalf("priority = %d, want 4", got)
	}
}

func TestPrioritiesApplyGitHubStarterField(t *testing.T) {
	items := []source.Item{
		rankItem("github:experimental", "acme/widgets#10", "low", "Issue", "Todo", "", "", 3, false, day1, nil),
		rankItem("github:experimental", "acme/widgets#20", "urgent", "Issue", "Todo", "", "", 0, false, day1, nil, "acme/widgets#10"),
	}
	cfgPath := writeGitHubConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.UpsertItems(context.Background(), "github:experimental", items); err != nil {
			t.Fatal(err)
		}
	})
	src := &fake.Source{SourceName: "github:experimental", Items: copyItems(items)}
	useSources(t, map[string]*fake.Source{"experimental": src})
	stdout, stderr, code := runCmd(t, "priorities", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "suggest Urgent") {
		t.Fatalf("stdout =\n%s", stdout)
	}
	if got := storedPriority(t, dbPath, "github:experimental", "acme/widgets#10"); got != 0 {
		t.Fatalf("priority = %d, want 0", got)
	}
}

func TestPrioritiesUsage(t *testing.T) {
	stdout, stderr, code := runCmd(t, "priorities", "PRJ-1")
	if code != ExitUsage {
		t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
}

func exampleChain(src string) []source.Item {
	return []source.Item{
		rankItem(src, "PRJ-10", "ten", "Task", "To Do", "", "", 2, false, day1, nil),
		rankItem(src, "PRJ-11", "eleven", "Task", "To Do", "", "", 2, false, day1, nil, "PRJ-10"),
		rankItem(src, "PRJ-20", "twenty", "Task", "To Do", "", "", 0, false, day1, nil, "PRJ-11"),
	}
}

func seedPriorityDB(t *testing.T, sourceName string, items []source.Item) (string, string) {
	t.Helper()
	cfgPath := writeNextConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, sourceName, items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, sourceName, day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, sourceName, "alice"); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

func copyItems(items []source.Item) map[string]source.Item {
	out := make(map[string]source.Item, len(items))
	for _, it := range items {
		out[it.ID] = it
	}
	return out
}

func storedPriority(t *testing.T, dbPath, sourceName, id string) int {
	t.Helper()
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Error(err)
		}
	})
	it, err := st.Item(context.Background(), sourceName, id)
	if err != nil {
		t.Fatal(err)
	}
	return it.Priority
}
