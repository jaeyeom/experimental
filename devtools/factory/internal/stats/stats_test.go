package stats_test

import (
	"bytes"
	"context"
	"encoding/json"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/stats"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const key = "jira:acme"

// now is a Wednesday. The 7d window starts the Wednesday before at noon.
var (
	now   = time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
	start = now.Add(-7 * 24 * time.Hour)                   //nolint:pkgvar // time.Date with time.UTC does not load zone data
	old   = time.Date(2026, 9, 1, 0, 0, 0, 0, time.UTC)    //nolint:pkgvar // time.Date with time.UTC does not load zone data
)

func at(day, hour int) time.Time {
	return time.Date(2026, 11, day, hour, 0, 0, 0, time.UTC)
}

func testConfig() *config.Config {
	return &config.Config{Sources: []config.Source{{
		Name: "acme", Kind: "jira", AssigneeOK: "any",
		ExcludeLabels: []string{"needs-human"}, HumanLabels: []string{"needs-human"},
		SkipTypes: []string{"Epic"}, ParentPriority: []string{"EPIC-2"},
		ParentBlocksChild: config.ParentBlocksIgnore, PRTitlePattern: `\[{id}\]`,
	}}}
}

func item(id, typ, parent string, created time.Time) source.Item {
	return source.Item{Source: key, ID: id, Title: "title " + id, Type: typ, State: "To Do", Parent: parent, Created: created, Updated: created}
}

// seed builds:
//
//	EPIC-1 (Epic)
//	  STORY-1
//	    T1 done, created in the window, claimed at the window start (no rule recorded)
//	    T2 in progress: claimed by rule heavy / opus, two open draft PRs
//	    T3 ready
//	    F1 finished history only, finished just before the window
//	  T4 blocked by T3
//	EPIC-2 (Epic, parent_priority)
//	  U1 ready, created in the window, released claim by rule light / sonnet, merged PR
//	H1 needs-human
func seed(t *testing.T) *store.Store {
	t.Helper()
	ctx := context.Background()
	st, err := store.Open(ctx, filepath.Join(t.TempDir(), "factory.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Error(err)
		}
	})
	t1 := item("T1", "Task", "STORY-1", at(4, 15))
	t2 := item("T2", "Task", "STORY-1", old)
	t2.State = "In Progress"
	t4 := item("T4", "Task", "EPIC-1", old)
	t4.BlockedBy = []source.BlockRef{{ID: "T3"}}
	h1 := item("H1", "Task", "", old)
	h1.Labels = []string{"needs-human"}
	items := []source.Item{
		item("EPIC-1", "Epic", "", old), item("STORY-1", "Story", "EPIC-1", old),
		t1, t2, item("T3", "Task", "STORY-1", old), t4,
		item("EPIC-2", "Epic", "", old), item("U1", "Task", "EPIC-2", at(8, 0)), h1,
		item("F1", "Task", "STORY-1", old),
	}
	must(t, st.UpsertItems(ctx, key, items))
	must(t, st.SetSyncState(ctx, key, now, now))
	must(t, st.SetMe(ctx, key, "me"))
	must(t, st.SetLastSync(ctx, key, now.Add(-time.Hour)))

	// F1 finishes just before the window, then leaves the mirror.
	f1 := items[9]
	f1.Done, f1.Updated = true, start.Add(-time.Minute)
	must(t, st.UpsertItems(ctx, key, []source.Item{f1}))
	if _, err := st.DeleteItemsNotIn(ctx, key, keepAllBut(items, "F1")); err != nil {
		t.Fatal(err)
	}
	// T1: claimed exactly at the window start, finished by a sync on the 6th.
	must(t, st.InsertClaim(ctx, store.Claim{Source: key, ID: "T1", ClaimedAt: start, State: store.HandedOff, UpdatedAt: start}))
	t1.Done, t1.Updated = true, at(6, 9)
	must(t, st.UpsertItems(ctx, key, []source.Item{t1}))
	must(t, st.InsertClaim(ctx, store.Claim{Source: key, ID: "T2", ClaimedAt: at(5, 10), State: store.HandedOff, UpdatedAt: at(5, 10), AgentRule: "heavy", Model: "opus"}))
	must(t, st.InsertClaim(ctx, store.Claim{Source: key, ID: "U1", ClaimedAt: at(8, 9), State: store.Released, UpdatedAt: at(8, 9), AgentRule: "light", Model: "sonnet"}))
	// A failed claim is not a start.
	must(t, st.InsertClaim(ctx, store.Claim{Source: key, ID: "T3", ClaimedAt: at(7, 0), State: store.Failed, UpdatedAt: at(7, 0)}))

	must(t, st.UpsertPRs(ctx, []store.PR{
		{Repo: "acme/w", Number: 1, Source: key, ItemID: "T1", OpenedAt: at(5, 12), MergedAt: at(6, 8), ClosedAt: at(6, 8), LastSeen: at(6, 8)},
		{Repo: "acme/w", Number: 2, Source: key, ItemID: "T2", OpenedAt: at(7, 10), Draft: true, LastSeen: at(10, 0)},
		{Repo: "acme/w", Number: 5, Source: key, ItemID: "T2", OpenedAt: at(6, 10), Draft: true, LastSeen: at(10, 0)},
		{Repo: "acme/w", Number: 4, Source: key, ItemID: "U1", OpenedAt: at(8, 12), MergedAt: at(9, 12), ClosedAt: at(9, 12), LastSeen: at(9, 12)},
	}))
	return st
}

func keepAllBut(items []source.Item, drop string) map[string]bool {
	keep := map[string]bool{}
	for _, it := range items {
		keep[it.ID] = it.ID != drop
	}
	return keep
}

func must(t *testing.T, err error) {
	t.Helper()
	if err != nil {
		t.Fatal(err)
	}
}

func build(t *testing.T, st *store.Store, opts stats.Options) stats.Report {
	t.Helper()
	opts.Now = now
	if opts.Since == 0 {
		opts.Since = 7 * 24 * time.Hour
	}
	rep, err := stats.Build(context.Background(), st, testConfig(), opts)
	if err != nil {
		t.Fatal(err)
	}
	return rep
}

func TestThroughputByDayAndWeek(t *testing.T) {
	st := seed(t)
	rep := build(t, st, stats.Options{})
	if rep.Throughput.By != stats.ByDay || len(rep.Throughput.Buckets) != 8 {
		t.Fatalf("by %s buckets %d, want day and 8", rep.Throughput.By, len(rep.Throughput.Buckets))
	}
	want := map[int]stats.Counts{
		4: {Started: 1}, // The claim at exactly the window start counts.
		5: {Started: 1, PRsOpened: 1},
		6: {Finished: 1, PRsOpened: 1, PRsMerged: 1},
		7: {PRsOpened: 1},
		8: {Started: 1, PRsOpened: 1},
		9: {PRsMerged: 1},
	}
	for _, b := range rep.Throughput.Buckets {
		if b.Counts != want[b.Start.Day()] {
			t.Fatalf("bucket %s = %+v, want %+v", b.Start.Format(time.DateOnly), b.Counts, want[b.Start.Day()])
		}
	}
	if total := (stats.Counts{Finished: 1, Started: 3, PRsOpened: 4, PRsMerged: 2}); rep.Throughput.Total != total {
		t.Fatalf("total = %+v, want %+v", rep.Throughput.Total, total)
	}

	weekly := build(t, st, stats.Options{By: stats.ByWeek})
	if len(weekly.Throughput.Buckets) != 2 {
		t.Fatalf("weekly buckets = %+v", weekly.Throughput.Buckets)
	}
	w1, w2 := weekly.Throughput.Buckets[0], weekly.Throughput.Buckets[1]
	if !w1.Start.Equal(at(2, 0)) || w1.Counts != (stats.Counts{Finished: 1, Started: 3, PRsOpened: 4, PRsMerged: 1}) {
		t.Fatalf("week 1 = %+v", w1)
	}
	if !w2.Start.Equal(at(9, 0)) || w2.Counts != (stats.Counts{PRsMerged: 1}) {
		t.Fatalf("week 2 = %+v", w2)
	}
	if long := build(t, st, stats.Options{Since: 15 * 24 * time.Hour}); long.Throughput.By != stats.ByWeek {
		t.Fatalf("15d window by = %s, want week", long.Throughput.By)
	}
}

func TestBacklogDeltaAgainstNearestSnapshot(t *testing.T) {
	st := seed(t)
	rep := build(t, st, stats.Options{})
	wantNow := stats.BacklogCounts{Ready: 2, Blocked: 1, InFlight: 1, NeedsHuman: 1, ByStatus: map[string]int{"To Do": 7, "In Progress": 1}}
	if !reflect.DeepEqual(rep.Backlog.Now, wantNow) {
		t.Fatalf("now = %+v, want %+v", rep.Backlog.Now, wantNow)
	}
	if rep.Backlog.Change != nil {
		t.Fatalf("change with no snapshot = %+v, want nil", rep.Backlog.Change)
	}
	var text bytes.Buffer
	must(t, rep.WriteText(&text))
	if !strings.Contains(strings.Join(strings.Fields(text.String()), " "), "change n/a n/a n/a n/a") {
		t.Fatalf("text has no n/a change:\n%s", text.String())
	}

	ctx := context.Background()
	for _, snap := range []store.BacklogSnapshot{
		{Source: key, TakenAt: start.Add(-48 * time.Hour), Ready: 9, ByStatus: map[string]int{"To Do": 9}},
		{Source: key, TakenAt: start.Add(-2 * time.Hour), Ready: 1, ByStatus: map[string]int{"To Do": 5}},
		{Source: key, TakenAt: start.Add(5 * time.Hour), Ready: 7, ByStatus: map[string]int{"To Do": 1}},
	} {
		if _, err := st.RecordBacklogSnapshot(ctx, snap, time.Hour, 90*24*time.Hour); err != nil {
			t.Fatal(err)
		}
	}
	rep = build(t, st, stats.Options{})
	want := stats.BacklogCounts{Ready: 1, Blocked: 1, InFlight: 1, NeedsHuman: 1, ByStatus: map[string]int{"To Do": 2, "In Progress": 1}}
	if rep.Backlog.Change == nil || !reflect.DeepEqual(*rep.Backlog.Change, want) {
		t.Fatalf("change = %+v, want %+v", rep.Backlog.Change, want)
	}
	if len(rep.Backlog.SnapshotAt) != 1 || !rep.Backlog.SnapshotAt[0].At.Equal(start.Add(-2*time.Hour)) {
		t.Fatalf("snapshot at = %+v", rep.Backlog.SnapshotAt)
	}
}

func TestEpicRollup(t *testing.T) {
	rep := build(t, seed(t), stats.Options{})
	if rep.Epics.Note != stats.EpicNote {
		t.Fatalf("note = %q", rep.Epics.Note)
	}
	wantParents := []stats.EpicRow{
		{Source: key, ID: "EPIC-1", Title: "title EPIC-1", Remaining: 2},
		{Source: key, ID: "EPIC-2", Title: "title EPIC-2", Priority: true, Remaining: 1, Added: 1},
		// F1 (finished history) and T1 are done, T2 is in progress, T3 remains.
		{Source: key, ID: "STORY-1", Title: "title STORY-1", Done: 2, InProgress: 1, Remaining: 1, Percent: 50, Added: 1},
	}
	if !reflect.DeepEqual(rep.Epics.Parents, wantParents) {
		t.Fatalf("parents =\n%+v\nwant\n%+v", rep.Epics.Parents, wantParents)
	}
	// EPIC-1 rolls up its leaves F1, T1, T2, T3, and T4.
	wantTop := []stats.EpicRow{
		{Source: key, ID: "EPIC-1", Title: "title EPIC-1", Done: 2, InProgress: 1, Remaining: 2, Percent: 40, Added: 1},
		{Source: key, ID: "EPIC-2", Title: "title EPIC-2", Priority: true, Remaining: 1, Added: 1},
	}
	if !reflect.DeepEqual(rep.Epics.TopLevel, wantTop) {
		t.Fatalf("top level =\n%+v\nwant\n%+v", rep.Epics.TopLevel, wantTop)
	}
	var text bytes.Buffer
	must(t, rep.WriteText(&text))
	if !strings.Contains(text.String(), "*EPIC-2") || !strings.Contains(text.String(), stats.EpicNote) {
		t.Fatalf("text does not mark parent_priority or the note:\n%s", text.String())
	}
}

func TestFlowMedianAndDrafts(t *testing.T) {
	rep := build(t, seed(t), stats.Options{})
	hours := func(h int64) *int64 { s := h * 3600; return &s }
	// Claim to first PR: U1 3h, T1 24h. PR open to merge: #1 20h, #4 24h.
	if want := (stats.Dist{Count: 2, MedianSeconds: hours(3), P90Seconds: hours(24)}); !reflect.DeepEqual(rep.Flow.ClaimToPR, want) {
		t.Fatalf("claim to PR = %+v", rep.Flow.ClaimToPR)
	}
	if want := (stats.Dist{Count: 2, MedianSeconds: hours(20), P90Seconds: hours(24)}); !reflect.DeepEqual(rep.Flow.PRToMerge, want) {
		t.Fatalf("PR to merge = %+v", rep.Flow.PRToMerge)
	}
	if len(rep.Flow.Drafts) != 2 || rep.Flow.Drafts[0].Number != 5 || rep.Flow.Drafts[1].Number != 2 {
		t.Fatalf("drafts = %+v, want #5 then #2", rep.Flow.Drafts)
	}
	if age := rep.Flow.Drafts[0].AgeSeconds; age != int64(now.Sub(at(6, 10))/time.Second) {
		t.Fatalf("draft age = %d", age)
	}
}

func TestSplitModelWithUnknown(t *testing.T) {
	rep := build(t, seed(t), stats.Options{Split: stats.SplitModel})
	if rep.Split == nil || rep.Split.By != stats.SplitModel {
		t.Fatalf("split = %+v", rep.Split)
	}
	got := map[string]stats.SplitRow{}
	var order []string
	for _, row := range rep.Split.Rows {
		got[row.Group] = row
		order = append(order, row.Group)
	}
	if strings.Join(order, ",") != "opus,sonnet,unknown" {
		t.Fatalf("groups = %v", order)
	}
	if c := got["opus"].Counts; c != (stats.Counts{Started: 1, PRsOpened: 2}) {
		t.Fatalf("opus = %+v", c)
	}
	if c := got["sonnet"].Counts; c != (stats.Counts{Started: 1, PRsOpened: 1, PRsMerged: 1}) || *got["sonnet"].ClaimToPR.MedianSeconds != 3*3600 {
		t.Fatalf("sonnet = %+v", got["sonnet"])
	}
	// T1's claim recorded no agent rule.
	if c := got["unknown"].Counts; c != (stats.Counts{Finished: 1, Started: 1, PRsOpened: 1, PRsMerged: 1}) || *got["unknown"].PRToMerge.MedianSeconds != 20*3600 {
		t.Fatalf("unknown = %+v", got["unknown"])
	}
	rules := build(t, seed(t), stats.Options{Split: stats.SplitRule})
	var groups []string
	for _, row := range rules.Split.Rows {
		groups = append(groups, row.Group)
	}
	if strings.Join(groups, ",") != "heavy,light,unknown" {
		t.Fatalf("rule groups = %v", groups)
	}
}

func TestEpicLimitsEverySection(t *testing.T) {
	rep := build(t, seed(t), stats.Options{Epic: "STORY-1"})
	if rep.Throughput.Total != (stats.Counts{Finished: 1, Started: 2, PRsOpened: 3, PRsMerged: 1}) {
		t.Fatalf("throughput = %+v", rep.Throughput.Total)
	}
	wantNow := stats.BacklogCounts{Ready: 1, InFlight: 1, ByStatus: map[string]int{"To Do": 2, "In Progress": 1}}
	if !reflect.DeepEqual(rep.Backlog.Now, wantNow) || rep.Backlog.Change != nil {
		t.Fatalf("backlog = %+v", rep.Backlog)
	}
	if len(rep.Epics.Parents) != 1 || rep.Epics.Parents[0].ID != "STORY-1" || len(rep.Epics.TopLevel) != 1 || rep.Epics.TopLevel[0].ID != "STORY-1" {
		t.Fatalf("epics = %+v", rep.Epics)
	}
	if rep.Flow.PRToMerge.Count != 1 || rep.Flow.ClaimToPR.Count != 1 || len(rep.Flow.Drafts) != 2 {
		t.Fatalf("flow = %+v", rep.Flow)
	}
	wantTree := []stats.TreeNode{
		{Depth: 0, ID: "STORY-1", State: "other", Title: "title STORY-1"},
		{Depth: 1, ID: "F1", State: "done", Title: "title F1"},
		{Depth: 1, ID: "T1", State: "done", Title: "title T1"},
		{Depth: 1, ID: "T2", State: "in_progress", Title: "title T2"},
		{Depth: 1, ID: "T3", State: "ready", Title: "title T3"},
	}
	if !reflect.DeepEqual(rep.Tree, wantTree) {
		t.Fatalf("tree =\n%+v\nwant\n%+v", rep.Tree, wantTree)
	}
	if _, err := stats.Build(context.Background(), seed(t), testConfig(), stats.Options{Now: now, Since: time.Hour, Epic: "NOPE-1"}); err == nil {
		t.Fatal("Build(--epic NOPE-1) error = nil, want not found")
	}
}

// TestJSONMatchesText decodes the JSON back into a report equal to the
// one the text came from, and checks the text shows the same numbers.
func TestJSONMatchesText(t *testing.T) {
	rep := build(t, seed(t), stats.Options{Split: stats.SplitModel})
	raw, err := json.Marshal(rep)
	must(t, err)
	var top map[string]json.RawMessage
	must(t, json.Unmarshal(raw, &top))
	for _, section := range []string{"throughput", "backlog", "epics", "flow", "split"} {
		if _, ok := top[section]; !ok {
			t.Fatalf("JSON has no %q object: %s", section, raw)
		}
	}
	// The burndown and the text zone are text-only, so compare what JSON carries.
	var back stats.Report
	must(t, json.Unmarshal(raw, &back))
	again, err := json.Marshal(back)
	must(t, err)
	if string(again) != string(raw) {
		t.Fatalf("JSON round trip differs:\n%s\n%s", again, raw)
	}
	var text bytes.Buffer
	must(t, rep.WriteText(&text))
	total := rep.Throughput.Total
	wantLines := []string{
		fields("total", total.Finished, total.Started, total.PRsOpened, total.PRsMerged),
		fields("now", rep.Backlog.Now.Ready, rep.Backlog.Now.Blocked, rep.Backlog.Now.InFlight, rep.Backlog.Now.NeedsHuman),
		"claim to first PR 2 3h 1d",
		"PR open to merge 2 20h 1d",
		"STORY-1 2 1 1 50 1 title STORY-1",
	}
	lines := map[string]bool{}
	for _, line := range strings.Split(text.String(), "\n") {
		lines[strings.Join(strings.Fields(line), " ")] = true
	}
	for _, want := range wantLines {
		if !lines[want] {
			t.Fatalf("text has no line %q:\n%s", want, text.String())
		}
	}
}

func fields(head string, nums ...int) string {
	parts := []string{head}
	for _, n := range nums {
		parts = append(parts, strconv.Itoa(n))
	}
	return strings.Join(parts, " ")
}

func TestParseSince(t *testing.T) {
	for in, want := range map[string]time.Duration{"7d": 7 * 24 * time.Hour, "2w": 14 * 24 * time.Hour, "36h": 36 * time.Hour} {
		got, err := stats.ParseSince(in)
		if err != nil || got != want {
			t.Fatalf("ParseSince(%q) = %s, %v, want %s", in, got, err, want)
		}
	}
	for _, in := range []string{"", "0d", "d", "-1h", "soon"} {
		if _, err := stats.ParseSince(in); err == nil {
			t.Fatalf("ParseSince(%q) error = nil", in)
		}
	}
}
