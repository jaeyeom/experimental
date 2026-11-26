package ready

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const (
	parentBlocksChildIgnored = "ignored: PRJ-100 is the parent of PRJ-105; a parent blocking its own child is usually a linking mistake"
	parentBlocksChildHonored = "honored: PRJ-100 is the parent of PRJ-105; a parent blocking its own child is usually a linking mistake"
)

func TestParentBlocksChildNote(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		mode       string
		done       bool
		mirrored   bool
		wantPass   bool
		wantDetail string
	}{
		{
			name:       "ignore",
			mode:       config.ParentBlocksIgnore,
			mirrored:   true,
			wantPass:   true,
			wantDetail: parentBlocksChildIgnored,
		},
		{
			name:       "honor",
			mode:       config.ParentBlocksHonor,
			mirrored:   true,
			wantDetail: parentBlocksChildHonored + "; blocker PRJ-100 is not done",
		},
		{
			name:       "honor done blocker",
			mode:       config.ParentBlocksHonor,
			done:       true,
			mirrored:   true,
			wantPass:   true,
			wantDetail: parentBlocksChildHonored,
		},
		{
			name:       "ignore missing parent",
			mode:       config.ParentBlocksIgnore,
			wantPass:   true,
			wantDetail: parentBlocksChildIgnored,
		},
		{
			name:       "honor missing parent",
			mode:       config.ParentBlocksHonor,
			wantDetail: parentBlocksChildHonored + "; blocker PRJ-100 not in mirror",
		},
		{
			name:       "unknown mode",
			mode:       "drop",
			mirrored:   true,
			wantDetail: `parent_blocks_child: expected ignore or honor, found "drop"; blocker PRJ-100 is not done`,
		},
		{
			name:       "empty mode",
			mode:       "",
			mirrored:   true,
			wantDetail: "parent_blocks_child: expected ignore or honor, found empty; blocker PRJ-100 is not done",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := parentChildFixture()
			cfg.ParentBlocksChild = tc.mode
			if tc.mirrored {
				parent := snap.Items["PRJ-100"]
				parent.Done = tc.done
				snap.Items["PRJ-100"] = parent
			} else {
				delete(snap.Items, "PRJ-100")
			}
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleBlockers, tc.wantPass, tc.wantDetail)
			wantReady := tc.wantPass && !tc.done
			if got.Ready != wantReady {
				t.Fatalf("Ready = %v, want %v", got.Ready, wantReady)
			}
		})
	}
}

func TestGrandparentBlocksGrandchild(t *testing.T) {
	t.Parallel()

	const ignored = "ignored: PRJ-100 is an ancestor of PRJ-110; a parent blocking its own child is usually a linking mistake"
	const honored = "honored: PRJ-100 is an ancestor of PRJ-110; a parent blocking its own child is usually a linking mistake; blocker PRJ-100 is not done"

	tests := []struct {
		name       string
		mode       string
		wantPass   bool
		wantDetail string
	}{
		{name: "ignore", mode: config.ParentBlocksIgnore, wantPass: true, wantDetail: ignored},
		{name: "honor", mode: config.ParentBlocksHonor, wantDetail: honored},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := parentChildFixture()
			it.ID = "PRJ-110"
			it.Parent = "PRJ-101"
			cfg.ParentBlocksChild = tc.mode
			snap.Items["PRJ-101"] = source.Item{ID: "PRJ-101", Parent: "PRJ-100"}
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleBlockers, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestChildBlocksParentStaysGating(t *testing.T) {
	t.Parallel()

	it, snap, cfg := parentChildFixture()
	it.ID = "PRJ-100"
	it.Parent = ""
	it.BlockedBy = []source.BlockRef{{ID: "PRJ-105"}}
	snap.Items["PRJ-105"] = source.Item{ID: "PRJ-105", Parent: "PRJ-100"}
	cfg.ParentBlocksChild = config.ParentBlocksIgnore
	got := eval(t, it, snap, cfg, "jira")
	assertRule(t, got, RuleBlockers, false, "blocker PRJ-105 is not done")
	if got.Ready {
		t.Fatal("Ready = true, want the child to still gate its parent")
	}
}

func TestParentBlockKeepsOtherBlockers(t *testing.T) {
	t.Parallel()

	it, snap, cfg := parentChildFixture()
	it.BlockedBy = []source.BlockRef{{ID: "PRJ-100"}, {ID: "PRJ-9"}}
	snap.Items["PRJ-9"] = source.Item{ID: "PRJ-9"}
	cfg.ParentBlocksChild = config.ParentBlocksIgnore
	got := eval(t, it, snap, cfg, "jira")
	assertRule(t, got, RuleBlockers, false, parentBlocksChildIgnored+"; blocker PRJ-9 is not done")
}

func TestThreeChildrenIgnoreAndHonor(t *testing.T) {
	t.Parallel()

	for _, mode := range []string{config.ParentBlocksIgnore, config.ParentBlocksHonor} {
		t.Run(mode, func(t *testing.T) {
			t.Parallel()
			_, snap, cfg := parentChildFixture()
			cfg.ParentBlocksChild = mode
			for _, id := range []string{"PRJ-101", "PRJ-102", "PRJ-103"} {
				it := source.Item{
					ID: id, Type: "Task", State: "To Do", Parent: "PRJ-100",
					Labels: []string{"agent-ready"}, BlockedBy: []source.BlockRef{{ID: "PRJ-100"}},
				}
				snap.Items[id] = it
				got := eval(t, it, snap, cfg, "jira")
				wantPass := mode == config.ParentBlocksIgnore
				if got.Ready != wantPass {
					t.Fatalf("%s Ready = %v, want %v, checks = %+v", id, got.Ready, wantPass, got.Checks)
				}
				detail := "ignored: PRJ-100 is the parent of " + id + "; a parent blocking its own child is usually a linking mistake"
				if mode == config.ParentBlocksHonor {
					detail = "honored: PRJ-100 is the parent of " + id + "; a parent blocking its own child is usually a linking mistake; blocker PRJ-100 is not done"
				}
				assertRule(t, got, RuleBlockers, wantPass, detail)
			}
		})
	}
}

func parentChildFixture() (source.Item, Snapshot, config.Source) {
	it, snap, cfg := jiraFixture()
	it.ID = "PRJ-105"
	it.Parent = "PRJ-100"
	it.BlockedBy = []source.BlockRef{{ID: "PRJ-100"}}
	snap.Items["PRJ-105"] = it
	snap.Items["PRJ-100"] = source.Item{ID: "PRJ-100"}
	return it, snap, cfg
}
