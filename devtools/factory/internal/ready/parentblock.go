package ready

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// ParentBlockNote describes a blocks link from an ancestor to its descendant.
// direct means ancestor is the descendant's Parent, not a further ancestor.
// mode is ignore or honor. Any other mode is an error that names what was
// expected and what was found.
func ParentBlockNote(mode, ancestor, descendant string, direct bool) (string, error) {
	ignore, err := config.IgnoreAncestorBlocks(mode)
	if err != nil {
		return "", fmt.Errorf("parent_blocks_child: %w", err)
	}
	verb := "honored"
	if ignore {
		verb = "ignored"
	}
	relation := "an ancestor"
	if direct {
		relation = "the parent"
	}
	return fmt.Sprintf(
		"%s: %s is %s of %s; a parent blocking its own child is usually a linking mistake",
		verb, ancestor, relation, descendant,
	), nil
}

func blockerNotes(it source.Item, ref source.BlockRef, snap Snapshot, cfg config.Source) ([]string, bool) {
	related, direct := source.AncestorRelation(ref.ID, it, snap.Items)
	if !related {
		return plainBlocker(ref, snap, cfg)
	}
	note, err := ParentBlockNote(cfg.ParentBlocksChild, ref.ID, it.ID, direct)
	extra, gate := plainBlocker(ref, snap, cfg)
	if err != nil {
		return append([]string{err.Error()}, extra...), true
	}
	if cfg.ParentBlocksChild == config.ParentBlocksIgnore {
		return []string{note}, false
	}
	return append([]string{note}, extra...), gate
}

func plainBlocker(ref source.BlockRef, snap Snapshot, cfg config.Source) ([]string, bool) {
	blocker, ok := snap.Items[ref.ID]
	switch {
	case !ok:
		return outsideBlocker(ref.ID, snap, cfg)
	case !blocker.Done:
		return []string{fmt.Sprintf("blocker %s is not done", ref.ID)}, true
	default:
		return nil, false
	}
}

// outsideBlocker judges a blocker that is not in the mirror by the record
// the last sync stored for it. Only a fetched, done record does not block.
func outsideBlocker(id string, snap Snapshot, cfg config.Source) ([]string, bool) {
	rec, ok := snap.External[id]
	if !ok {
		if cfg.Kind == "jira" && cfg.ExternalBlockerLookups > 0 {
			return []string{fmt.Sprintf("blocker %s not in mirror (not fetched yet)", id)}, true
		}
		return []string{fmt.Sprintf("blocker %s not in mirror", id)}, true
	}
	switch rec.Status {
	case store.ExternalFound:
		if rec.Done {
			return nil, false
		}
		assignee := "unassigned"
		if rec.Assignee != "" {
			assignee = "assigned to " + rec.Assignee
		}
		return []string{fmt.Sprintf("blocker %s is %s (outside mirror, %s)", id, rec.State, assignee)}, true
	case store.ExternalNotFound:
		return []string{fmt.Sprintf("blocker %s not found or no access", id)}, true
	default:
		return []string{fmt.Sprintf("blocker %s not in mirror (not checked: %s)", id, rec.Reason)}, true
	}
}
