package priority

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestFindExample(t *testing.T) {
	items := map[string]source.Item{
		"PRJ-10": item("PRJ-10", "", 2, false),
		"PRJ-11": item("PRJ-11", "", 2, false, "PRJ-10"),
		"PRJ-20": item("PRJ-20", "", 0, false, "PRJ-11"),
	}
	got, err := Find(items, jiraCfg(config.ParentBlocksIgnore))
	if err != nil {
		t.Fatal(err)
	}
	want := []string{
		"PRJ-10  priority Medium (2)  blocks PRJ-20 (Highest, 0) via PRJ-10 → PRJ-11 → PRJ-20  suggest Highest",
		"PRJ-11  priority Medium (2)  blocks PRJ-20 (Highest, 0)  suggest Highest",
	}
	if lines := formats(got); strings.Join(lines, "\n") != strings.Join(want, "\n") {
		t.Fatalf("lines =\n%s\nwant\n%s", strings.Join(lines, "\n"), strings.Join(want, "\n"))
	}
	if got[0].SuggestName != "Highest" || got[0].TargetPriority != 0 {
		t.Fatalf("suggestion = %s %d", got[0].SuggestName, got[0].TargetPriority)
	}
}

func TestExplainNamesBothEnds(t *testing.T) {
	items := map[string]source.Item{
		"PRJ-10": item("PRJ-10", "", 2, false),
		"PRJ-11": item("PRJ-11", "", 2, false, "PRJ-10"),
		"PRJ-20": item("PRJ-20", "", 0, false, "PRJ-11"),
	}
	cfg := jiraCfg(config.ParentBlocksIgnore)
	blocker, err := Explain("PRJ-10", items, cfg)
	if err != nil {
		t.Fatal(err)
	}
	const via = "inversion  blocks PRJ-20 priority Highest (0), this priority Medium (2), via PRJ-10 → PRJ-11 → PRJ-20"
	if len(blocker) != 1 || blocker[0] != via {
		t.Fatalf("blocker = %#v", blocker)
	}
	direct, err := Explain("PRJ-11", items, cfg)
	if err != nil {
		t.Fatal(err)
	}
	if len(direct) != 1 || direct[0] != "inversion  blocks PRJ-20 priority Highest (0), this priority Medium (2)" {
		t.Fatalf("middle = %#v", direct)
	}
	target, err := Explain("PRJ-20", items, cfg)
	if err != nil {
		t.Fatal(err)
	}
	if len(target) != 2 || !strings.Contains(target[0], "blocked by PRJ-10") || !strings.Contains(target[1], "blocked by PRJ-11") {
		t.Fatalf("target = %#v", target)
	}
	if strings.Contains(target[1], "via ") {
		t.Fatalf("direct target included a chain: %q", target[1])
	}
}

func TestFindSkipsDoneAndFamily(t *testing.T) {
	items := map[string]source.Item{
		"PRJ-10":  item("PRJ-10", "", 2, false),
		"PRJ-11":  item("PRJ-11", "", 2, true, "PRJ-10"),
		"PRJ-20":  item("PRJ-20", "", 0, false, "PRJ-11"),
		"PRJ-100": item("PRJ-100", "", 2, false),
		"PRJ-101": item("PRJ-101", "PRJ-100", 0, false, "PRJ-100"),
		"PRJ-200": item("PRJ-200", "PRJ-201", 2, false),
		"PRJ-201": item("PRJ-201", "", 0, false, "PRJ-200"),
		"DONE":    item("DONE", "", 3, true),
		"OPEN":    item("OPEN", "", 0, false, "DONE"),
		"PRJ-30":  item("PRJ-30", "", 2, false),
		"PRJ-31":  item("PRJ-31", "", 0, true, "PRJ-30"),
	}
	for _, mode := range []string{config.ParentBlocksIgnore, config.ParentBlocksHonor} {
		got, err := Find(items, jiraCfg(mode))
		if err != nil {
			t.Fatal(err)
		}
		text := strings.Join(formats(got), "\n")
		for _, absent := range []string{"PRJ-10", "PRJ-11", "PRJ-20", "PRJ-100", "PRJ-101", "PRJ-200", "PRJ-201", "DONE", "OPEN", "PRJ-30", "PRJ-31"} {
			if strings.Contains(text, absent) {
				t.Fatalf("mode %s listed %s\n%s", mode, absent, text)
			}
		}
	}
}

func TestHonorReachesPastChild(t *testing.T) {
	items := map[string]source.Item{
		"PRJ-100": item("PRJ-100", "", 2, false),
		"PRJ-101": item("PRJ-101", "PRJ-100", 2, false, "PRJ-100"),
		"PRJ-20":  item("PRJ-20", "", 0, false, "PRJ-101"),
	}
	ignored, err := Find(items, jiraCfg(config.ParentBlocksIgnore))
	if err != nil {
		t.Fatal(err)
	}
	if text := strings.Join(formats(ignored), "\n"); text != "PRJ-101  priority Medium (2)  blocks PRJ-20 (Highest, 0)  suggest Highest" {
		t.Fatalf("ignore =\n%s", text)
	}
	honored, err := Find(items, jiraCfg(config.ParentBlocksHonor))
	if err != nil {
		t.Fatal(err)
	}
	want := []string{
		"PRJ-100  priority Medium (2)  blocks PRJ-20 (Highest, 0) via PRJ-100 → PRJ-101 → PRJ-20  suggest Highest",
		"PRJ-101  priority Medium (2)  blocks PRJ-20 (Highest, 0)  suggest Highest",
	}
	if text := strings.Join(formats(honored), "\n"); text != strings.Join(want, "\n") {
		t.Fatalf("honor =\n%s", text)
	}
}

func TestFindPicksMostUrgent(t *testing.T) {
	items := map[string]source.Item{
		"PRJ-1": item("PRJ-1", "", 3, false),
		"PRJ-2": item("PRJ-2", "", 1, false, "PRJ-1"),
		"PRJ-3": item("PRJ-3", "", 0, false, "PRJ-1"),
		"PRJ-A": item("PRJ-A", "", 0, false, "PRJ-B"),
		"PRJ-B": item("PRJ-B", "", 2, false),
		"PRJ-C": item("PRJ-C", "", 0, false, "PRJ-B"),
	}
	got, err := Find(items, jiraCfg(config.ParentBlocksIgnore))
	if err != nil {
		t.Fatal(err)
	}
	byBlocker := map[string]Inversion{}
	for _, row := range got {
		byBlocker[row.BlockerID] = row
	}
	if byBlocker["PRJ-1"].TargetID != "PRJ-3" || byBlocker["PRJ-1"].SuggestName != "Highest" {
		t.Fatalf("most urgent = %+v", byBlocker["PRJ-1"])
	}
	if byBlocker["PRJ-B"].TargetID != "PRJ-A" {
		t.Fatalf("tie = %+v", byBlocker["PRJ-B"])
	}
}

func TestFindRejectsBadShape(t *testing.T) {
	items := map[string]source.Item{
		"PRJ-1": {ID: "PRJ-2"},
	}
	if _, err := Find(items, jiraCfg(config.ParentBlocksIgnore)); err == nil || !strings.Contains(err.Error(), `expected item id "PRJ-1"`) {
		t.Fatalf("id error = %v", err)
	}
	items = map[string]source.Item{"PRJ-1": item("PRJ-1", "", 2, false, "")}
	if _, err := Find(items, jiraCfg(config.ParentBlocksIgnore)); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("blocker error = %v", err)
	}
	if _, err := Find(nil, jiraCfg("nope")); err == nil || !strings.Contains(err.Error(), "found \"nope\"") {
		t.Fatalf("mode error = %v", err)
	}
}

func TestNames(t *testing.T) {
	cfg := jiraCfg(config.ParentBlocksIgnore)
	cfg.PriorityRanks = map[string]int{"Highest": 9, "P0": 0}
	cfg.KnownPriorities = []string{"Untriaged"}
	name, err := Name("jira", cfg, 0)
	if err != nil || name != "P0" {
		t.Fatalf("rank 0 = %q, %v", name, err)
	}
	name, err = Name("jira", cfg, 9)
	if err != nil || name != "Highest" {
		t.Fatalf("rank 9 = %q, %v", name, err)
	}
	name, err = Name("jira", cfg, 2)
	if err != nil || name != "Medium" {
		t.Fatalf("rank 2 = %q, %v", name, err)
	}
	if _, err := Name("jira", cfg, 8); err == nil || !strings.Contains(err.Error(), "found none") {
		t.Fatalf("missing = %v", err)
	}
	ambiguous := config.Source{Kind: "jira", PriorityRanks: map[string]int{"Highest": 9, "P0": 0, "Urgent": 0}}
	if _, err := Name("jira", ambiguous, 0); err == nil || !strings.Contains(err.Error(), `"P0"`) || !strings.Contains(err.Error(), `"Urgent"`) {
		t.Fatalf("ambiguous = %v", err)
	}
	gh := config.Source{Kind: "github", PriorityLabels: map[string]int{"p0": 0, "p1": 0}}
	if _, err := Name("github", gh, 0); err == nil || !strings.Contains(err.Error(), "expected one name") {
		t.Fatalf("github = %v", err)
	}
	if _, err := Name("other", cfg, 0); err == nil || !strings.Contains(err.Error(), `found "other"`) {
		t.Fatalf("kind = %v", err)
	}
}

func TestGitHubLabelName(t *testing.T) {
	items := map[string]source.Item{
		"acme/widgets#10": item("acme/widgets#10", "", 2, false),
		"acme/widgets#20": item("acme/widgets#20", "", 0, false, "acme/widgets#10"),
	}
	cfg := config.Source{
		Kind:              "github",
		ParentBlocksChild: config.ParentBlocksIgnore,
		PriorityLabels:    map[string]int{"priority:high": 0, "priority:medium": 2},
	}
	got, err := Find(items, cfg)
	if err != nil {
		t.Fatal(err)
	}
	const want = "acme/widgets#10  priority priority:medium (2)  blocks acme/widgets#20 (priority:high, 0)  suggest priority:high"
	if len(got) != 1 || Format(got[0]) != want {
		t.Fatalf("line = %#v", formats(got))
	}
}

func TestGitHubStarterFieldPriorityNames(t *testing.T) {
	items := map[string]source.Item{
		"acme/widgets#10": item("acme/widgets#10", "", 3, false),
		"acme/widgets#20": item("acme/widgets#20", "", 0, false, "acme/widgets#10"),
	}
	cfg := config.Source{
		Kind:              "github",
		ParentBlocksChild: config.ParentBlocksIgnore,
		PriorityField:     config.DefaultGitHubPriorityField,
	}
	got, err := Find(items, cfg)
	if err != nil {
		t.Fatal(err)
	}
	const want = "acme/widgets#10  priority Low (3)  blocks acme/widgets#20 (Urgent, 0)  suggest Urgent"
	if len(got) != 1 || Format(got[0]) != want {
		t.Fatalf("line = %#v", formats(got))
	}
	for rank, wantName := range map[int]string{0: "Urgent", 1: "High", 2: "Medium", 3: "Low"} {
		name, err := Name("github", cfg, rank)
		if err != nil || name != wantName {
			t.Fatalf("Name(github, %d) = %q, %v; want %q", rank, name, err, wantName)
		}
	}
}

func TestCycleDoesNotLoop(t *testing.T) {
	items := map[string]source.Item{
		"A": item("A", "", 2, false, "B"),
		"B": item("B", "", 0, false, "A"),
	}
	got, err := Find(items, jiraCfg(config.ParentBlocksIgnore))
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].BlockerID != "A" || got[0].TargetID != "B" {
		t.Fatalf("got = %+v", got)
	}
}

func jiraCfg(mode string) config.Source {
	return config.Source{Kind: "jira", ParentBlocksChild: mode}
}

func item(id, parent string, rank int, done bool, blockers ...string) source.Item {
	it := source.Item{ID: id, Parent: parent, Priority: rank, Done: done}
	for _, blocker := range blockers {
		it.BlockedBy = append(it.BlockedBy, source.BlockRef{ID: blocker})
	}
	return it
}

func formats(rows []Inversion) []string {
	out := make([]string, len(rows))
	for i, row := range rows {
		out[i] = Format(row)
	}
	return out
}
