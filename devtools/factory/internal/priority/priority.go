// Package priority reports a lower-priority item blocking a higher-priority one.
// A smaller rank is more urgent. Finding inversions does not write.
package priority

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// Inversion is one not-done blocker that reaches a more urgent not-done item.
// Chain includes the blocker and the target. A direct block has length 2.
type Inversion struct {
	BlockerID       string
	BlockerPriority int
	BlockerName     string
	TargetID        string
	TargetPriority  int
	TargetName      string
	SuggestName     string
	Chain           []string
}

// Find reads items and returns inversions ordered by blocker id.
// Done items are neither blockers nor targets, and a path does not go through
// them. An ancestor→descendant blocks edge is omitted when parent_blocks_child
// is ignore. honor still reaches through that edge, but a descendant is never
// the target. A child→ancestor edge is always omitted.
func Find(items map[string]source.Item, cfg config.Source) ([]Inversion, error) {
	if err := requireIDs(items); err != nil {
		return nil, err
	}
	ignore, err := config.IgnoreAncestorBlocks(cfg.ParentBlocksChild)
	if err != nil {
		return nil, fmt.Errorf("priority: parent_blocks_child: %w", err)
	}
	names, err := newTable(cfg)
	if err != nil {
		return nil, err
	}
	blocks, err := blockEdges(items, ignore)
	if err != nil {
		return nil, err
	}
	var out []Inversion
	for _, id := range sortedIDs(items) {
		row, ok, err := oneInversion(items[id], blocks, items, names)
		if err != nil {
			return nil, err
		}
		if ok {
			out = append(out, row)
		}
	}
	return out, nil
}

// Explain returns lines for id when it blocks something more urgent, and when
// something less urgent blocks it. The chain is included only when transitive.
func Explain(id string, items map[string]source.Item, cfg config.Source) ([]string, error) {
	rows, err := Find(items, cfg)
	if err != nil {
		return nil, err
	}
	var lines []string
	for _, row := range rows {
		if row.BlockerID == id {
			lines = append(lines, explainBlocks(row))
		}
	}
	for _, row := range rows {
		if row.TargetID == id {
			lines = append(lines, explainBlocked(row))
		}
	}
	return lines, nil
}

// Format prints one inversion in the priorities report shape.
func Format(inv Inversion) string {
	line := fmt.Sprintf("%s  priority %s  blocks %s (%s)",
		inv.BlockerID,
		rankText(inv.BlockerName, inv.BlockerPriority),
		inv.TargetID,
		pairText(inv.TargetName, inv.TargetPriority),
	)
	if via := viaText(inv.Chain); via != "" {
		line += " " + via
	}
	return line + "  suggest " + suggestText(inv)
}

// Name is the single priority name to write for rank.
// None, or more than one with no canonical name, is an error.
func Name(kind string, cfg config.Source, rank int) (string, error) {
	table, err := newTable(config.Source{
		Kind:            kind,
		PriorityField:   cfg.PriorityField,
		PriorityRanks:   cfg.PriorityRanks,
		PriorityLabels:  cfg.PriorityLabels,
		KnownPriorities: cfg.KnownPriorities,
	})
	if err != nil {
		return "", err
	}
	name, err := table.lookup(rank)
	if err != nil {
		return "", err
	}
	if name == "" {
		return "", fmt.Errorf("priority: expected one name for rank %d, found none", rank)
	}
	return name, nil
}

func requireIDs(items map[string]source.Item) error {
	for id, it := range items {
		if it.ID != id {
			return fmt.Errorf("priority: expected item id %q, found %q", id, it.ID)
		}
	}
	return nil
}

func oneInversion(it source.Item, blocks map[string][]string, items map[string]source.Item, names table) (Inversion, bool, error) {
	if it.Done {
		return Inversion{}, false, nil
	}
	best := reach(it, blocks, items)
	if !best.ok {
		return Inversion{}, false, nil
	}
	blockerName, err := names.lookup(it.Priority)
	if err != nil {
		return Inversion{}, false, err
	}
	targetName, err := names.lookup(best.item.Priority)
	if err != nil {
		return Inversion{}, false, err
	}
	return Inversion{
		BlockerID:       it.ID,
		BlockerPriority: it.Priority,
		BlockerName:     blockerName,
		TargetID:        best.item.ID,
		TargetPriority:  best.item.Priority,
		TargetName:      targetName,
		SuggestName:     targetName,
		Chain:           best.path,
	}, true, nil
}

type candidate struct {
	item source.Item
	path []string
	ok   bool
}

func (c candidate) losesTo(other source.Item, path []string) bool {
	if !c.ok {
		return true
	}
	if other.Priority != c.item.Priority {
		return other.Priority < c.item.Priority
	}
	if len(path) != len(c.path) {
		return len(path) < len(c.path)
	}
	if other.ID != c.item.ID {
		return other.ID < c.item.ID
	}
	return strings.Join(path, "\x00") < strings.Join(c.path, "\x00")
}

type step struct {
	id   string
	path []string
}

func reach(start source.Item, blocks map[string][]string, items map[string]source.Item) candidate {
	seen := map[string]struct{}{start.ID: {}}
	queue := []step{{id: start.ID, path: []string{start.ID}}}
	var best candidate
	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]
		queue = walk(cur, start, blocks, items, seen, queue, &best)
	}
	return best
}

func walk(cur step, start source.Item, blocks map[string][]string, items map[string]source.Item, seen map[string]struct{}, queue []step, best *candidate) []step {
	for _, next := range blocks[cur.id] {
		if _, ok := seen[next]; ok {
			continue
		}
		it, ok := items[next]
		if !ok || it.Done {
			continue
		}
		seen[next] = struct{}{}
		path := slices.Concat(cur.path, []string{next})
		queue = append(queue, step{id: next, path: path})
		if !eligible(start, it, items) || it.Priority >= start.Priority {
			continue
		}
		if best.losesTo(it, path) {
			*best = candidate{item: it, path: path, ok: true}
		}
	}
	return queue
}

func eligible(start, candidate source.Item, items map[string]source.Item) bool {
	if candidate.ID == start.ID {
		return false
	}
	if related, _ := source.AncestorRelation(start.ID, candidate, items); related {
		return false
	}
	related, _ := source.AncestorRelation(candidate.ID, start, items)
	return !related
}

func blockEdges(items map[string]source.Item, ignoreAncestor bool) (map[string][]string, error) {
	blocks := make(map[string][]string)
	for _, id := range sortedIDs(items) {
		it := items[id]
		for _, ref := range it.BlockedBy {
			if ref.ID == "" {
				return nil, fmt.Errorf("priority: item %s: expected a non-empty blocker id, found empty", it.ID)
			}
			if !includeEdge(ref.ID, it, items, ignoreAncestor) {
				continue
			}
			blocks[ref.ID] = append(blocks[ref.ID], it.ID)
		}
	}
	for id := range blocks {
		slices.Sort(blocks[id])
	}
	return blocks, nil
}

// includeEdge drops unknown blockers, every child→ancestor edge, and an
// ancestor→descendant edge when ignoreAncestor is set.
func includeEdge(blockerID string, blocked source.Item, items map[string]source.Item, ignoreAncestor bool) bool {
	blocker, ok := items[blockerID]
	if !ok {
		return false
	}
	if related, _ := source.AncestorRelation(blocked.ID, blocker, items); related {
		return false
	}
	if !ignoreAncestor {
		return true
	}
	related, _ := source.AncestorRelation(blockerID, blocked, items)
	return !related
}

func explainBlocks(inv Inversion) string {
	line := fmt.Sprintf("inversion  blocks %s priority %s, this priority %s",
		inv.TargetID,
		rankText(inv.TargetName, inv.TargetPriority),
		rankText(inv.BlockerName, inv.BlockerPriority),
	)
	if via := viaText(inv.Chain); via != "" {
		line += ", " + via
	}
	return line
}

func explainBlocked(inv Inversion) string {
	line := fmt.Sprintf("inversion  blocked by %s priority %s, this priority %s",
		inv.BlockerID,
		rankText(inv.BlockerName, inv.BlockerPriority),
		rankText(inv.TargetName, inv.TargetPriority),
	)
	if via := viaText(inv.Chain); via != "" {
		line += ", " + via
	}
	return line
}

func rankText(name string, rank int) string {
	if name == "" {
		return strconv.Itoa(rank)
	}
	return fmt.Sprintf("%s (%d)", name, rank)
}

func pairText(name string, rank int) string {
	if name == "" {
		return strconv.Itoa(rank)
	}
	return fmt.Sprintf("%s, %d", name, rank)
}

func suggestText(inv Inversion) string {
	if inv.SuggestName != "" {
		return inv.SuggestName
	}
	return strconv.Itoa(inv.TargetPriority)
}

func viaText(chain []string) string {
	if len(chain) <= 2 {
		return ""
	}
	return "via " + strings.Join(chain, " → ")
}

func sortedIDs(items map[string]source.Item) []string {
	ids := make([]string, 0, len(items))
	for id := range items {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	return ids
}
