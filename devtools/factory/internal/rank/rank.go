// Package rank orders ready work items deterministically.
// It performs no I/O.
package rank

import (
	"cmp"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const (
	keyParentRank = "parent_rank"
	keyUnblocks   = "unblocks"
	keyPriority   = "priority"
	keyCreated    = "created"
)

// allowedKeys is the order used when an error names a missing key.
var allowedKeys = []string{keyParentRank, keyUnblocks, keyPriority, keyCreated}

// Score is the sort key for one ready item.
type Score struct {
	// Unblocks is the number of distinct not-done items reachable through
	// blocks edges. The item itself is not included.
	Unblocks int
	// ParentRank is the index of the nearest listed ancestor in parentPriority.
	// An item with no listed ancestor uses len(parentPriority).
	ParentRank int
	// Ancestor is that ancestor's id. Empty when none matched.
	Ancestor string
	// Priority is copied from the item. Zero is highest.
	Priority int
	// Created is copied from the item.
	Created time.Time
}

// Ranked is one ready item and the score that ordered it.
type Ranked struct {
	Item  source.Item
	Score Score
}

// Order is a validated rank_order.
// The zero Order is not a default. Use ParseOrder.
type Order struct {
	keys []string
}

// NamedOrder is one configured rank_order.
// Name is included in errors. Order nil means the default order.
type NamedOrder struct {
	Name  string
	Order []string
}

// ParseOrder validates rank_order.
// A nil order is the default: priority, unblocks, parent_rank, created.
// A non-nil order must be a permutation of parent_rank, unblocks, priority,
// and created. An unknown key, a duplicate key, or a missing key is an error.
func ParseOrder(order []string) (Order, error) {
	if order == nil {
		return Order{keys: defaultKeys()}, nil
	}
	seen := make(map[string]struct{}, len(order))
	for _, key := range order {
		if err := acceptKey(key, seen); err != nil {
			return Order{}, err
		}
		seen[key] = struct{}{}
	}
	if err := missingKeys(seen); err != nil {
		return Order{}, err
	}
	return Order{keys: append([]string(nil), order...)}, nil
}

func defaultKeys() []string {
	return []string{keyPriority, keyUnblocks, keyParentRank, keyCreated}
}

// Keys returns the rank_order keys, best key first.
// It panics on a zero Order. ParseOrder does not return one.
func (o Order) Keys() []string {
	if len(o.keys) == 0 {
		panic("rank_order: expected a parsed order, found empty")
	}
	return append([]string(nil), o.keys...)
}

func acceptKey(key string, seen map[string]struct{}) error {
	if key == "" {
		return errors.New("expected parent_rank, unblocks, priority, or created, found empty")
	}
	if !knownKey(key) {
		return fmt.Errorf("expected parent_rank, unblocks, priority, and created, found unknown key %q", key)
	}
	if _, ok := seen[key]; ok {
		return fmt.Errorf("expected parent_rank, unblocks, priority, and created, found duplicate %q", key)
	}
	return nil
}

func knownKey(key string) bool {
	switch key {
	case keyParentRank, keyUnblocks, keyPriority, keyCreated:
		return true
	default:
		return false
	}
}

func missingKeys(seen map[string]struct{}) error {
	var missing []string
	for _, key := range allowedKeys {
		if _, ok := seen[key]; !ok {
			missing = append(missing, key)
		}
	}
	if len(missing) == 0 {
		return nil
	}
	return fmt.Errorf("expected parent_rank, unblocks, priority, and created, found missing %s", quoteList(missing))
}

func quoteList(keys []string) string {
	quoted := make([]string, len(keys))
	for i, key := range keys {
		quoted[i] = fmt.Sprintf("%q", key)
	}
	return strings.Join(quoted, ", ")
}

// CommonOrder parses every order and requires one shared sequence.
// A nil order is the default. Differing orders are an error that names
// what was expected and what was found.
func CommonOrder(in []NamedOrder) (Order, error) {
	if len(in) == 0 {
		return ParseOrder(nil)
	}
	base, err := parseNamed(in[0])
	if err != nil {
		return Order{}, err
	}
	for _, n := range in[1:] {
		next, err := parseNamed(n)
		if err != nil {
			return Order{}, err
		}
		if !slices.Equal(base.keys, next.keys) {
			return Order{}, fmt.Errorf("rank_order: expected %s, found %s",
				describeOrder(in[0].Name, base.keys), describeOrder(n.Name, next.keys))
		}
	}
	return base, nil
}

func parseNamed(n NamedOrder) (Order, error) {
	parsed, err := ParseOrder(n.Order)
	if err != nil {
		if n.Name == "" {
			return Order{}, fmt.Errorf("rank_order: %w", err)
		}
		return Order{}, fmt.Errorf("source %q field \"rank_order\": %w", n.Name, err)
	}
	return parsed, nil
}

func describeOrder(name string, keys []string) string {
	list := strings.Join(keys, ", ")
	if name == "" {
		return list
	}
	return list + " from " + fmt.Sprintf("%q", name)
}

// Compare orders a and b by this order, then by ID ascending.
// It panics on a zero Order or an unknown key. ParseOrder accepts neither.
func (o Order) Compare(a, b Ranked) int {
	if len(o.keys) == 0 {
		panic("rank_order: expected a parsed order, found empty")
	}
	for _, key := range o.keys {
		if c := compareKey(key, a, b); c != 0 {
			return c
		}
	}
	return cmp.Compare(a.Item.ID, b.Item.ID)
}

func compareKey(key string, a, b Ranked) int {
	switch key {
	case keyUnblocks:
		return cmp.Compare(b.Score.Unblocks, a.Score.Unblocks)
	case keyParentRank:
		return cmp.Compare(a.Score.ParentRank, b.Score.ParentRank)
	case keyPriority:
		return cmp.Compare(a.Score.Priority, b.Score.Priority)
	case keyCreated:
		return a.Score.Created.Compare(b.Score.Created)
	default:
		panic(fmt.Sprintf("rank_order: expected parent_rank, unblocks, priority, or created, found %q", key))
	}
}

// Rank orders ready items deterministically.
// rankOrder nil keeps priority asc, unblocks desc, parent rank asc, then
// created asc. ID ascending is the final tie-break and is not a rank_order key.
// all supplies the blocks graph and the parent chain: for each item X and each
// blocker in X.BlockedBy, that blocker blocks X. Parent links walk all[id].Parent.
// parentPriority is the configured parent order. An empty parent chain, or one
// with no listed ancestor, ranks after every listed parent.
// ignoreAncestorBlocks drops a blocks edge whose blocker is an ancestor of the
// blocked item, so that edge does not add to Unblocks. false keeps the edge.
// Rank does not modify ready.
func Rank(ready []source.Item, all map[string]source.Item, parentPriority []string, rankOrder []string, ignoreAncestorBlocks bool) ([]Ranked, error) {
	order, err := ParseOrder(rankOrder)
	if err != nil {
		return nil, fmt.Errorf("rank_order: %w", err)
	}
	byID, blocks := blockGraph(all, ignoreAncestorBlocks)
	index := priorityIndex(parentPriority)
	ranked := make([]Ranked, len(ready))
	for i := range ready {
		it := ready[i]
		parentRank, ancestor := ancestorRank(it, byID, index, len(parentPriority))
		ranked[i] = Ranked{
			Item: it,
			Score: Score{
				Unblocks:   countUnblocks(it.ID, blocks, byID),
				ParentRank: parentRank,
				Ancestor:   ancestor,
				Priority:   it.Priority,
				Created:    it.Created,
			},
		}
	}
	slices.SortStableFunc(ranked, order.Compare)
	return ranked, nil
}

// blockGraph indexes items by ID and the items each blocker blocks.
// X.BlockedBy lists blockers of X, so the edge used for unblocks runs from
// the blocker to X. When ignoreAncestorBlocks is set, an edge whose blocker
// is an ancestor of X is omitted. Items behind that edge are not counted.
func blockGraph(all map[string]source.Item, ignoreAncestorBlocks bool) (map[string]source.Item, map[string][]string) {
	byID := make(map[string]source.Item, len(all))
	for _, it := range all {
		byID[it.ID] = it
	}
	blocks := make(map[string][]string)
	for _, it := range all {
		for _, b := range it.BlockedBy {
			if ignoreAncestorBlocks && ancestorEdge(b.ID, it, byID) {
				continue
			}
			blocks[b.ID] = append(blocks[b.ID], it.ID)
		}
	}
	return byID, blocks
}

func ancestorEdge(blockerID string, blocked source.Item, byID map[string]source.Item) bool {
	related, _ := source.AncestorRelation(blockerID, blocked, byID)
	return related
}

// Unblocked returns the distinct not-done item ids reachable from id through
// blocks edges, sorted by id. Done items are omitted, but edges through them
// still count, so a not-done item behind a done item is included. id itself
// is not included. ignoreAncestorBlocks drops an edge whose blocker is an
// ancestor of the blocked item. The set is the one Rank counts as Unblocks.
// The result is never nil.
func Unblocked(id string, all map[string]source.Item, ignoreAncestorBlocks bool) []string {
	byID, blocks := blockGraph(all, ignoreAncestorBlocks)
	ids := collectUnblocked(id, blocks, byID)
	slices.Sort(ids)
	if ids == nil {
		return []string{}
	}
	return ids
}

// countUnblocks counts distinct not-done items reachable from id along blocks
// edges. Done items are not counted, but edges through them still count, so a
// not-done item behind a done item is reachable. id itself is not counted. The
// visited set ends cycles.
func countUnblocks(id string, blocks map[string][]string, byID map[string]source.Item) int {
	return len(collectUnblocked(id, blocks, byID))
}

func collectUnblocked(id string, blocks map[string][]string, byID map[string]source.Item) []string {
	seen := map[string]struct{}{id: {}}
	queue := []string{id}
	var ids []string
	for head := 0; head < len(queue); head++ {
		for _, next := range blocks[queue[head]] {
			if _, ok := seen[next]; ok {
				continue
			}
			seen[next] = struct{}{}
			queue = append(queue, next)
			if byID[next].Done {
				continue
			}
			ids = append(ids, next)
		}
	}
	return ids
}

// priorityIndex maps a parent id to its first index in parentPriority.
func priorityIndex(parentPriority []string) map[string]int {
	index := make(map[string]int, len(parentPriority))
	for i, id := range parentPriority {
		if _, ok := index[id]; ok {
			continue
		}
		index[id] = i
	}
	return index
}

// ancestorRank returns the index and id of the nearest listed ancestor.
// The walk starts at Parent, so the item itself is not a match. A direct
// parent matches when it is listed, even if that parent is not mirrored.
// A later ancestor matches only through mirrored steps. A parent cycle or an
// unmirrored step stops the walk. No listed ancestor returns unlisted and "".
func ancestorRank(it source.Item, byID map[string]source.Item, index map[string]int, unlisted int) (int, string) {
	for _, cur := range source.Ancestors(it, byID) {
		if rank, ok := index[cur]; ok {
			return rank, cur
		}
	}
	return unlisted, ""
}
