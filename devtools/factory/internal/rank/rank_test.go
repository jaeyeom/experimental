package rank

import (
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

var (
	day1 = time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
	day2 = time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
	day3 = time.Date(2026, 1, 3, 0, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
)

func TestRankChain(t *testing.T) {
	t.Parallel()

	// A blocks B blocks C. A unblocks 2.
	a := item("A", "", 2, day1, false)
	b := item("B", "", 2, day1, false, "A")
	c := item("C", "", 2, day1, false, "B")
	ready := []source.Item{c, b, a}

	got := mustRank(t, ready, allOf(t, a, b, c), nil, nil)

	assertOrder(t, got, []string{"A", "B", "C"})
	assertUnblocks(t, got, "A", 2)
	assertUnblocks(t, got, "B", 1)
	assertUnblocks(t, got, "C", 0)
	assertIDs(t, ready, []string{"C", "B", "A"})
}

func TestRankDiamond(t *testing.T) {
	t.Parallel()

	// A blocks B and C, and both block D. D is one item, so A unblocks 3, not 4.
	a := item("A", "", 2, day1, false)
	b := item("B", "", 2, day1, false, "A")
	c := item("C", "", 2, day1, false, "A")
	d := item("D", "", 2, day1, false, "B", "C")

	got := mustRank(t, []source.Item{d, c, b, a}, allOf(t, a, b, c, d), nil, nil)

	assertOrder(t, got, []string{"A", "B", "C", "D"})
	assertUnblocks(t, got, "A", 3)
	assertUnblocks(t, got, "B", 1)
	assertUnblocks(t, got, "C", 1)
	assertUnblocks(t, got, "D", 0)
}

func TestRankCycle(t *testing.T) {
	t.Parallel()

	// A and B block each other. Each counts the other once and not itself.
	a := item("A", "", 2, day1, false, "B")
	b := item("B", "", 2, day1, false, "A")

	got := mustRank(t, []source.Item{b, a}, allOf(t, a, b), nil, nil)

	assertOrder(t, got, []string{"A", "B"})
	assertUnblocks(t, got, "A", 1)
	assertUnblocks(t, got, "B", 1)
}

func TestRankDoneDescendantsNotCounted(t *testing.T) {
	t.Parallel()

	// A blocks B (open) and D (done). B blocks C (done). D blocks E (open).
	// Done items C and D are not counted. E is still reachable through D.
	a := item("A", "", 2, day1, false)
	b := item("B", "", 2, day1, false, "A")
	c := item("C", "", 2, day1, true, "B")
	d := item("D", "", 2, day1, true, "A")
	e := item("E", "", 2, day1, false, "D")

	got := mustRank(t, []source.Item{e, d, c, b, a}, allOf(t, a, b, c, d, e), nil, nil)

	assertUnblocks(t, got, "A", 2)
	assertUnblocks(t, got, "B", 0)
	assertUnblocks(t, got, "C", 0)
	assertUnblocks(t, got, "D", 1)
	assertUnblocks(t, got, "E", 0)
}

func TestRankTieBreakUnblocks(t *testing.T) {
	t.Parallel()

	// Parent, priority, and created match. B unblocks Y; A unblocks nothing.
	// A would win a sort by ID alone.
	a := item("A", "", 2, day1, false)
	b := item("B", "", 2, day1, false)
	y := item("Y", "", 2, day1, false, "B")

	got := mustRank(t, []source.Item{a, b}, allOf(t, a, b, y), nil, nil)

	assertOrder(t, got, []string{"B", "A"})
	assertUnblocks(t, got, "B", 1)
	assertUnblocks(t, got, "A", 0)
	if got[0].Score.ParentRank != got[1].Score.ParentRank ||
		got[0].Score.Priority != got[1].Score.Priority ||
		!got[0].Score.Created.Equal(got[1].Score.Created) {
		t.Fatalf("other score fields differ: %+v vs %+v", got[0].Score, got[1].Score)
	}
}

func TestRankTieBreakParentRank(t *testing.T) {
	t.Parallel()

	// Unblocks, priority, and created match. A listed parent beats a later
	// one, and both beat an unlisted or empty parent. IDs run backwards.
	parents := []string{"epic-high", "epic-mid"}
	high := item("C", "epic-high", 2, day1, false)
	mid := item("B", "epic-mid", 2, day1, false)
	missing := item("A", "epic-other", 2, day1, false)
	none := item("D", "", 2, day1, false)

	got := mustRank(t, []source.Item{none, missing, mid, high}, allOf(t, high, mid, missing, none), parents, nil)

	assertOrder(t, got, []string{"C", "B", "A", "D"})
	assertAncestor(t, got, "C", "epic-high", 0)
	assertAncestor(t, got, "B", "epic-mid", 1)
	assertAncestor(t, got, "A", "", len(parents))
	assertAncestor(t, got, "D", "", len(parents))
	assertSameExcept(t, got, "parent")
}

func TestRankTieBreakPriority(t *testing.T) {
	t.Parallel()

	// Unblocks, parent rank, and created match. Lower priority wins.
	// A would win a sort by ID alone.
	low := item("B", "", 0, day1, false)
	mid := item("C", "", 1, day1, false)
	high := item("A", "", 2, day1, false)

	got := mustRank(t, []source.Item{high, mid, low}, allOf(t, low, mid, high), nil, nil)

	assertOrder(t, got, []string{"B", "C", "A"})
	if got[0].Score.Priority != 0 || got[1].Score.Priority != 1 || got[2].Score.Priority != 2 {
		t.Fatalf("priorities = %d %d %d, want 0 1 2", got[0].Score.Priority, got[1].Score.Priority, got[2].Score.Priority)
	}
	assertSameExcept(t, got, "priority")
}

func TestRankTieBreakCreated(t *testing.T) {
	t.Parallel()

	// Unblocks, parent rank, and priority match. Older created wins.
	// A would win a sort by ID alone.
	oldest := item("C", "", 2, day1, false)
	middle := item("B", "", 2, day2, false)
	newest := item("A", "", 2, day3, false)

	got := mustRank(t, []source.Item{newest, oldest, middle}, allOf(t, oldest, middle, newest), nil, nil)

	assertOrder(t, got, []string{"C", "B", "A"})
	if !got[0].Score.Created.Equal(day1) || !got[1].Score.Created.Equal(day2) || !got[2].Score.Created.Equal(day3) {
		t.Fatalf("created = %s %s %s, want %s %s %s",
			got[0].Score.Created, got[1].Score.Created, got[2].Score.Created, day1, day2, day3)
	}
	assertSameExcept(t, got, "created")
}

func TestRankTieBreakID(t *testing.T) {
	t.Parallel()

	a := item("A", "", 2, day1, false)
	b := item("B", "", 2, day1, false)
	c := item("C", "", 2, day1, false)
	ready := []source.Item{c, a, b}

	got := mustRank(t, ready, allOf(t, a, b, c), nil, nil)

	assertOrder(t, got, []string{"A", "B", "C"})
	assertSameExcept(t, got, "")
	assertIDs(t, ready, []string{"C", "A", "B"})

	// ID stays last under a different key order when every other key ties.
	reordered := mustRank(t, ready, allOf(t, a, b, c), nil, []string{"created", "priority", "parent_rank", "unblocks"})
	assertOrder(t, reordered, []string{"A", "B", "C"})
}

func TestRankSortKeyOrder(t *testing.T) {
	t.Parallel()

	// Each item loses to the previous one on exactly one earlier key, and
	// would win if that key were ignored. Priority dominates unblocks, which
	// dominates parent rank, which dominates created, which dominates ID.
	parents := []string{"top"}
	blocked := item("blocked-by-u1", "", 2, day1, false, "Z")
	blocked2 := item("blocked-by-u2", "", 2, day1, false, "Z")
	one := item("one", "", 2, day1, false, "Y", "X", "B", "M")
	u := item("Z", "nope", 5, day3, false)
	p := item("Y", "top", 5, day3, false)
	r := item("X", "nope", 0, day3, false)
	c := item("B", "nope", 1, day1, false)
	id := item("M", "nope", 1, day1, false)

	all := allOf(t, u, p, r, c, id, blocked, blocked2, one)
	ready := []source.Item{id, c, r, p, u}
	got := mustRank(t, ready, all, parents, nil)

	assertOrder(t, got, []string{"X", "B", "M", "Z", "Y"})
	if got[0].Score.Priority != 0 || got[1].Score.Priority != 1 || got[3].Score.Priority != 5 {
		t.Fatalf("priorities = %d %d %d, want 0 1 5", got[0].Score.Priority, got[1].Score.Priority, got[3].Score.Priority)
	}

	explicit := mustRank(t, ready, all, parents, []string{"unblocks", "parent_rank", "priority", "created"})
	assertOrder(t, explicit, []string{"Z", "Y", "X", "B", "M"})
	assertUnblocks(t, explicit, "Z", 2)
	assertUnblocks(t, explicit, "Y", 1)
	if explicit[1].Score.ParentRank != 0 || explicit[2].Score.ParentRank != 1 {
		t.Fatalf("parent ranks = %d %d, want 0 1", explicit[1].Score.ParentRank, explicit[2].Score.ParentRank)
	}
}

func TestRankPriorityBeforeUnblocks(t *testing.T) {
	t.Parallel()

	// PRJ-1 is Highest and unblocks nothing. PRJ-2 is Medium and unblocks 2.
	// Unset rank_order lists the higher priority first. The old explicit
	// order still lists the larger unblock count first.
	one := item("PRJ-1", "", 0, day1, false)
	two := item("PRJ-2", "", 2, day1, false)
	a := item("A", "", 2, day1, false, "PRJ-2")
	b := item("B", "", 2, day1, false, "PRJ-2")
	all := allOf(t, one, two, a, b)
	ready := []source.Item{two, one}

	got := mustRank(t, ready, all, nil, nil)
	assertOrder(t, got, []string{"PRJ-1", "PRJ-2"})
	assertUnblocks(t, got, "PRJ-1", 0)
	assertUnblocks(t, got, "PRJ-2", 2)

	old := []string{"unblocks", "parent_rank", "priority", "created"}
	explicit := mustRank(t, ready, all, nil, old)
	assertOrder(t, explicit, []string{"PRJ-2", "PRJ-1"})
}

func TestRankMissingPriorityWithLowest(t *testing.T) {
	t.Parallel()

	// Rank 2 is an unknown name (Medium). Rank 3 is Low. Rank 4 is Lowest
	// and a missing priority. Low comes first. The two rank-4 items then
	// follow created, then id.
	unknown := item("UNK", "", 2, day3, false)
	low := item("LOW", "", 3, day2, false)
	lowest := item("LOWEST", "", 4, day1, false)
	missing := item("NONE", "", 4, day1, false)

	got := mustRank(t, []source.Item{missing, lowest, low, unknown}, allOf(t, unknown, low, lowest, missing), nil, nil)
	assertOrder(t, got, []string{"UNK", "LOW", "LOWEST", "NONE"})
}

func TestRankEmpty(t *testing.T) {
	t.Parallel()

	got := mustRank(t, nil, nil, nil, nil)
	if len(got) != 0 {
		t.Fatalf("len = %d, want 0", len(got))
	}
}

func TestRankParentRankFirst(t *testing.T) {
	t.Parallel()

	// Four listed epics. parent_rank outranks unblocks, including a sub-task
	// whose listed epic is a grandparent. The item's own id is not a match.
	parents := []string{"AP-2396", "AP-1106", "AP-1428", "AP-1919"}
	order := []string{"parent_rank", "unblocks", "priority", "created"}
	story := item("STORY-1", "AP-1428", 2, day1, false)
	first := item("AP-2399", "AP-2396", 1, day1, false)
	head := item("AP-2400", "AP-2396", 2, day2, false)
	follower := item("AP-2401", "", 2, day1, false, "AP-2400")
	second := item("AP-1107", "AP-1106", 2, day1, false)
	sub := item("AP-1430", "STORY-1", 2, day1, false)
	fourth := item("AP-1920", "AP-1919", 2, day1, false)
	devops := item("DEVOPS-995", "DEVOPS-1", 2, day1, false)
	d1 := item("DEVOPS-A", "", 2, day1, false, "DEVOPS-995")
	d2 := item("DEVOPS-B", "", 2, day1, false, "DEVOPS-995")
	other := item("AP-1201", "AP-OTHER", 2, day1, false)
	o1 := item("AP-1202", "", 2, day1, false, "AP-1201")
	self := item("AP-2396", "AP-OTHER", 0, day1, false)

	ready := []source.Item{other, devops, self, fourth, sub, second, head, first}
	all := allOf(t, story, first, head, follower, second, sub, fourth, devops, d1, d2, other, o1, self)
	got := mustRank(t, ready, all, parents, order)

	assertOrder(t, got, []string{"AP-2400", "AP-2399", "AP-1107", "AP-1430", "AP-1920", "DEVOPS-995", "AP-1201", "AP-2396"})
	assertAncestor(t, got, "AP-2400", "AP-2396", 0)
	assertAncestor(t, got, "AP-2399", "AP-2396", 0)
	assertAncestor(t, got, "AP-1107", "AP-1106", 1)
	assertAncestor(t, got, "AP-1430", "AP-1428", 2)
	assertAncestor(t, got, "AP-1920", "AP-1919", 3)
	assertAncestor(t, got, "DEVOPS-995", "", len(parents))
	assertAncestor(t, got, "AP-1201", "", len(parents))
	assertAncestor(t, got, "AP-2396", "", len(parents))
	assertUnblocks(t, got, "DEVOPS-995", 2)
	assertUnblocks(t, got, "AP-1201", 1)
	assertUnblocks(t, got, "AP-2400", 1)

	// The default order lets priority outrank unblocks. Equal priorities
	// still let unblocks outrank parent rank.
	def := mustRank(t, ready, all, parents, nil)
	assertOrder(t, def, []string{"AP-2396", "AP-2399", "DEVOPS-995", "AP-2400", "AP-1201", "AP-1107", "AP-1430", "AP-1920"})
}

func TestRankNearestAncestor(t *testing.T) {
	t.Parallel()

	parents := []string{"EPIC-FAR", "EPIC-NEAR"}
	near := item("EPIC-NEAR", "EPIC-FAR", 2, day1, false)
	story := item("STORY", "EPIC-NEAR", 2, day1, false)
	sub := item("SUB", "STORY", 2, day1, false)

	got := mustRank(t, []source.Item{sub}, allOf(t, near, story, sub), parents, nil)

	assertAncestor(t, got, "SUB", "EPIC-NEAR", 1)
}

func TestRankParentCycle(t *testing.T) {
	t.Parallel()

	parents := []string{"EPIC"}
	a := item("A", "B", 2, day1, false)
	b := item("B", "C", 2, day1, false)
	c := item("C", "B", 2, day1, false)
	got := mustRank(t, []source.Item{a, b, c}, allOf(t, a, b, c), parents, nil)
	assertAncestor(t, got, "A", "", len(parents))
	assertAncestor(t, got, "B", "", len(parents))
	assertAncestor(t, got, "C", "", len(parents))

	// A listed ancestor before the cycle still matches, and the walk stops.
	child := item("CHILD", "EPIC", 2, day1, false)
	epic := item("EPIC", "CHILD", 2, day1, false)
	got = mustRank(t, []source.Item{child}, allOf(t, child, epic), parents, nil)
	assertAncestor(t, got, "CHILD", "EPIC", 0)

	// A self-parent is the item itself, so a listed id does not match.
	self := item("EPIC", "EPIC", 2, day1, false)
	got = mustRank(t, []source.Item{self}, allOf(t, self), parents, nil)
	assertAncestor(t, got, "EPIC", "", len(parents))
}

func TestRankUnmirroredParentStops(t *testing.T) {
	t.Parallel()

	parents := []string{"EPIC"}
	// STORY is not in the mirror, so the listed epic above it is not visible.
	sub := item("SUB", "STORY", 2, day1, false)
	got := mustRank(t, []source.Item{sub}, allOf(t, sub), parents, nil)
	assertAncestor(t, got, "SUB", "", len(parents))

	// A listed direct parent matches even when that parent is not mirrored.
	child := item("CHILD", "EPIC", 2, day1, false)
	got = mustRank(t, []source.Item{child}, allOf(t, child), parents, nil)
	assertAncestor(t, got, "CHILD", "EPIC", 0)
}

func TestRankIgnoreAncestorBlocks(t *testing.T) {
	t.Parallel()

	// PRJ-100 blocks three children and one grandchild, plus an unrelated item.
	// PRJ-120 is a child that blocks PRJ-100. That reverse edge still counts.
	parent := item("PRJ-100", "", 2, day1, false, "PRJ-120")
	c1 := item("PRJ-101", "PRJ-100", 2, day1, false, "PRJ-100")
	c2 := item("PRJ-102", "PRJ-100", 2, day1, false, "PRJ-100")
	c3 := item("PRJ-103", "PRJ-100", 2, day1, false, "PRJ-100")
	gc := item("PRJ-110", "PRJ-101", 2, day1, false, "PRJ-100")
	other := item("PRJ-9", "", 2, day1, false, "PRJ-100")
	rev := item("PRJ-120", "PRJ-100", 2, day1, false)
	all := allOf(t, parent, c1, c2, c3, gc, other, rev)
	ready := []source.Item{parent, rev}

	ignored := mustRankMode(t, ready, all, nil, nil, true)
	assertUnblocks(t, ignored, "PRJ-100", 1)
	assertUnblocks(t, ignored, "PRJ-120", 2)

	honored := mustRankMode(t, ready, all, nil, nil, false)
	assertUnblocks(t, honored, "PRJ-100", 5)
	assertUnblocks(t, honored, "PRJ-120", 6)
}

func TestRankEmptyParentIgnoresBlankListing(t *testing.T) {
	t.Parallel()

	parents := []string{"", "EPIC"}
	none := item("NONE", "", 2, day1, false)
	got := mustRank(t, []source.Item{none}, allOf(t, none), parents, nil)
	assertAncestor(t, got, "NONE", "", len(parents))
}

func TestRankOrderRejected(t *testing.T) {
	t.Parallel()

	itemA := item("A", "", 2, day1, false)
	tests := []struct {
		name  string
		order []string
		want  []string
	}{
		{
			name:  "unknown",
			order: []string{"parent_rank", "unblocks", "priority", "created", "id"},
			want:  []string{"unknown key", `"id"`},
		},
		{
			name:  "duplicate",
			order: []string{"unblocks", "unblocks", "priority", "created"},
			want:  []string{"duplicate", `"unblocks"`},
		},
		{
			name:  "missing",
			order: []string{"unblocks", "priority", "created"},
			want:  []string{"missing", `"parent_rank"`},
		},
		{
			name:  "empty list",
			order: []string{},
			want:  []string{"missing", `"parent_rank"`, `"unblocks"`, `"priority"`, `"created"`},
		},
		{
			name:  "empty key",
			order: []string{"unblocks", "", "priority", "created"},
			want:  []string{"found empty"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			_, err := Rank([]source.Item{itemA}, nil, nil, tc.order, false)
			if err == nil {
				t.Fatal("Rank() error = nil")
			}
			for _, part := range tc.want {
				if !strings.Contains(err.Error(), part) {
					t.Fatalf("Rank() error = %q, want %q", err.Error(), part)
				}
			}
		})
	}
}

func TestCommonOrder(t *testing.T) {
	t.Parallel()

	def := []string{"priority", "unblocks", "parent_rank", "created"}
	got, err := CommonOrder([]NamedOrder{
		{Name: "jira:acme", Order: nil},
		{Name: "jira:cub", Order: def},
	})
	if err != nil {
		t.Fatalf("CommonOrder() error = %v", err)
	}
	if !slices.Equal(got.keys, def) {
		t.Fatalf("keys = %v, want %v", got.keys, def)
	}

	empty, err := CommonOrder(nil)
	if err != nil {
		t.Fatalf("CommonOrder(nil) error = %v", err)
	}
	if !slices.Equal(empty.keys, def) {
		t.Fatalf("nil keys = %v, want %v", empty.keys, def)
	}

	_, err = CommonOrder([]NamedOrder{
		{Name: "jira:acme", Order: nil},
		{Name: "jira:cub", Order: []string{"parent_rank", "unblocks", "priority", "created"}},
	})
	if err == nil {
		t.Fatal("CommonOrder() error = nil")
	}
	for _, part := range []string{"expected", "found", "jira:acme", "jira:cub", "parent_rank"} {
		if !strings.Contains(err.Error(), part) {
			t.Fatalf("CommonOrder() error = %q, want %q", err.Error(), part)
		}
	}
}

func mustRank(t *testing.T, ready []source.Item, all map[string]source.Item, parentPriority []string, rankOrder []string) []Ranked {
	t.Helper()
	return mustRankMode(t, ready, all, parentPriority, rankOrder, false)
}

func mustRankMode(t *testing.T, ready []source.Item, all map[string]source.Item, parentPriority []string, rankOrder []string, ignoreAncestorBlocks bool) []Ranked {
	t.Helper()
	got, err := Rank(ready, all, parentPriority, rankOrder, ignoreAncestorBlocks)
	if err != nil {
		t.Fatalf("Rank() error = %v", err)
	}
	return got
}

func item(id, parent string, priority int, created time.Time, done bool, blockedBy ...string) source.Item {
	it := source.Item{
		ID:       id,
		Parent:   parent,
		Priority: priority,
		Created:  created,
		Done:     done,
	}
	if len(blockedBy) == 0 {
		return it
	}
	it.BlockedBy = make([]source.BlockRef, len(blockedBy))
	for i, blocker := range blockedBy {
		it.BlockedBy[i] = source.BlockRef{ID: blocker}
	}
	return it
}

func allOf(t *testing.T, items ...source.Item) map[string]source.Item {
	t.Helper()
	all := make(map[string]source.Item, len(items))
	for _, it := range items {
		if _, ok := all[it.ID]; ok {
			t.Fatalf("duplicate item %q", it.ID)
		}
		all[it.ID] = it
	}
	return all
}

func assertOrder(t *testing.T, got []Ranked, want []string) {
	t.Helper()
	gotIDs := make([]string, len(got))
	for i, r := range got {
		gotIDs[i] = r.Item.ID
	}
	if !slices.Equal(gotIDs, want) {
		t.Fatalf("order = %v, want %v", gotIDs, want)
	}
}

func assertAncestor(t *testing.T, got []Ranked, id, ancestor string, rank int) {
	t.Helper()
	for _, r := range got {
		if r.Item.ID != id {
			continue
		}
		if r.Score.Ancestor != ancestor || r.Score.ParentRank != rank {
			t.Fatalf("%s ancestor = %q rank %d, want %q rank %d",
				id, r.Score.Ancestor, r.Score.ParentRank, ancestor, rank)
		}
		return
	}
	t.Fatalf("missing %s", id)
}

func assertUnblocks(t *testing.T, got []Ranked, id string, want int) {
	t.Helper()
	for _, r := range got {
		if r.Item.ID != id {
			continue
		}
		if r.Score.Unblocks != want {
			t.Fatalf("%s unblocks = %d, want %d", id, r.Score.Unblocks, want)
		}
		return
	}
	t.Fatalf("missing %s", id)
}

func assertIDs(t *testing.T, items []source.Item, want []string) {
	t.Helper()
	got := make([]string, len(items))
	for i, it := range items {
		got[i] = it.ID
	}
	if !slices.Equal(got, want) {
		t.Fatalf("ids = %v, want %v", got, want)
	}
}

// assertSameExcept checks that the deciding key is the only score difference.
// key is "parent", "priority", "created", or "" when every score matches.
func assertSameExcept(t *testing.T, got []Ranked, key string) {
	t.Helper()
	for i := 1; i < len(got); i++ {
		a, b := got[0].Score, got[i].Score
		if key != "parent" && a.ParentRank != b.ParentRank {
			t.Fatalf("ParentRank differs: %+v vs %+v", a, b)
		}
		if key != "priority" && a.Priority != b.Priority {
			t.Fatalf("Priority differs: %+v vs %+v", a, b)
		}
		if key != "created" && !a.Created.Equal(b.Created) {
			t.Fatalf("Created differs: %+v vs %+v", a, b)
		}
		if a.Unblocks != b.Unblocks {
			t.Fatalf("Unblocks differs: %+v vs %+v", a, b)
		}
	}
}

func TestUnblockedIDs(t *testing.T) {
	t.Parallel()

	// A blocks B (open) and D (done). D blocks E (open). B blocks C (done).
	// The set is B and E, not the done items, and E is reachable through D.
	a := item("A", "", 2, day1, false)
	b := item("B", "", 2, day1, false, "A")
	c := item("C", "", 2, day1, true, "B")
	d := item("D", "", 2, day1, true, "A")
	e := item("E", "", 2, day1, false, "D")
	all := allOf(t, a, b, c, d, e)
	got := Unblocked("A", all, false)
	if !slices.Equal(got, []string{"B", "E"}) {
		t.Fatalf("Unblocked(A) = %#v, want [B E]", got)
	}
	ranked := mustRankMode(t, []source.Item{a}, all, nil, nil, false)
	if ranked[0].Score.Unblocks != len(got) {
		t.Fatalf("score %d, len %d", ranked[0].Score.Unblocks, len(got))
	}
	missing := Unblocked("missing", all, false)
	if missing == nil || len(missing) != 0 {
		t.Fatalf("missing = %#v, want an empty slice", missing)
	}

	parent := item("PRJ-100", "", 2, day1, false, "PRJ-120")
	c1 := item("PRJ-101", "PRJ-100", 2, day1, false, "PRJ-100")
	other := item("PRJ-9", "", 2, day1, false, "PRJ-100")
	rev := item("PRJ-120", "PRJ-100", 2, day1, false, "PRJ-100")
	ancestors := allOf(t, parent, c1, other, rev)
	ignored := Unblocked("PRJ-100", ancestors, true)
	if !slices.Equal(ignored, []string{"PRJ-9"}) {
		t.Fatalf("ignored = %#v, want [PRJ-9]", ignored)
	}
	honored := Unblocked("PRJ-100", ancestors, false)
	if !slices.Equal(honored, []string{"PRJ-101", "PRJ-120", "PRJ-9"}) {
		t.Fatalf("honored = %#v", honored)
	}
	ignoredRank := mustRankMode(t, []source.Item{parent}, ancestors, nil, nil, true)
	if ignoredRank[0].Score.Unblocks != len(ignored) {
		t.Fatalf("ignored score %d, len %d", ignoredRank[0].Score.Unblocks, len(ignored))
	}
}
