package doctor

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func parentChildLinks(in Input) ([]Finding, error) {
	if _, err := config.IgnoreAncestorBlocks(in.Config.ParentBlocksChild); err != nil {
		return nil, fmt.Errorf("doctor: parent_blocks_child: %w", err)
	}
	var out []Finding
	seen := make(map[string]struct{})
	items := in.Snapshot.Items
	for _, id := range sortedIDs(items) {
		blocked := items[id]
		for _, ref := range blocked.BlockedBy {
			found, ok, err := classifyBlock(in.Config.ParentBlocksChild, blocked, ref.ID, items, seen)
			if err != nil {
				return nil, err
			}
			if ok {
				out = append(out, found)
			}
		}
	}
	return out, nil
}

func classifyBlock(mode string, blocked source.Item, blockerID string, items map[string]source.Item, seen map[string]struct{}) (Finding, bool, error) {
	if related, direct := source.AncestorRelation(blockerID, blocked, items); related {
		return parentBlocksDescendant(mode, blockerID, blocked.ID, direct, seen)
	}
	blocker, ok := items[blockerID]
	if !ok {
		return Finding{}, false, nil
	}
	related, direct := source.AncestorRelation(blocked.ID, blocker, items)
	if !related {
		return Finding{}, false, nil
	}
	return descendantBlocksAncestor(blockerID, blocked.ID, direct, seen)
}

func parentBlocksDescendant(mode, ancestor, descendant string, direct bool, seen map[string]struct{}) (Finding, bool, error) {
	if !markEdge(seen, "parent", ancestor, descendant) {
		return Finding{}, false, nil
	}
	note, err := ready.ParentBlockNote(mode, ancestor, descendant, direct)
	if err != nil {
		return Finding{}, false, fmt.Errorf("doctor: %w", err)
	}
	return finding(
		CheckParentBlocksChild,
		SeverityWarn,
		note,
		fmt.Sprintf("factory unlink %s %s", ancestor, descendant),
		ancestor, descendant,
	), true, nil
}

func descendantBlocksAncestor(descendant, ancestor string, direct bool, seen map[string]struct{}) (Finding, bool, error) {
	if !markEdge(seen, "child", descendant, ancestor) {
		return Finding{}, false, nil
	}
	return finding(
		CheckChildBlocksParent,
		SeverityInfo,
		childBlockMessage(descendant, ancestor, direct),
		fmt.Sprintf("factory unlink %s %s", descendant, ancestor),
		descendant, ancestor,
	), true, nil
}

func childBlockMessage(descendant, ancestor string, direct bool) string {
	if direct {
		return fmt.Sprintf("%s blocks its parent %s; a child blocking its own parent is usually redundant", descendant, ancestor)
	}
	return fmt.Sprintf("%s blocks its ancestor %s; a child blocking its own ancestor is usually redundant", descendant, ancestor)
}

func markEdge(seen map[string]struct{}, kind, a, b string) bool {
	key := kind + "\x00" + a + "\x00" + b
	if _, ok := seen[key]; ok {
		return false
	}
	seen[key] = struct{}{}
	return true
}
