// Package parentclose evaluates whether open parent items have all known
// children completed in the local cache. It performs no I/O.
package parentclose

import (
	"errors"
	"fmt"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Result is the close-eligibility evaluation of one mirrored item.
type Result struct {
	Source       string
	ID           string
	Title        string
	Eligible     bool
	DoneChildren []string
	OpenChildren []string
	Reason       string
}

// Evaluate checks whether id is an open parent in items whose known children
// (across mirrored items and finished history) are all done.
func Evaluate(id string, items []source.Item, finished []store.Finished) (Result, error) {
	if id == "" {
		return Result{}, errors.New("parentclose: expected a non-empty item id, found empty")
	}
	byID := indexItems(items)
	target, ok := byID[id]
	if !ok {
		return Result{}, fmt.Errorf("parentclose: item %s: %w", id, source.ErrNotFound)
	}
	children := buildChildren(byID, items, finished)
	return evaluateItem(target, children[id]), nil
}

// Find returns all eligible open parents in items, sorted deterministically by ID.
func Find(items []source.Item, finished []store.Finished) ([]Result, error) {
	byID := indexItems(items)
	children := buildChildren(byID, items, finished)
	ids := make([]string, 0, len(byID))
	for id := range byID {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	out := []Result{}
	for _, id := range ids {
		res := evaluateItem(byID[id], children[id])
		if res.Eligible {
			out = append(out, res)
		}
	}
	return out, nil
}

func indexItems(items []source.Item) map[string]source.Item {
	byID := make(map[string]source.Item, len(items))
	for i := range items {
		it := items[i]
		if it.ID == "" {
			continue
		}
		byID[it.ID] = it
	}
	return byID
}

// buildChildren maps each parent ID to its deduplicated child IDs and whether
// each child is done. Finished-history rows are added first, then mirrored
// items overwrite them so a reopened mirrored child always takes precedence.
func buildChildren(byID map[string]source.Item, items []source.Item, finished []store.Finished) map[string]map[string]bool {
	children := make(map[string]map[string]bool)
	for i := range finished {
		fin := finished[i]
		if fin.ID == "" || fin.Parent == "" || fin.ID == fin.Parent {
			continue
		}
		if parent, ok := byID[fin.Parent]; ok && !sameSource(parent.Source, fin.Source) {
			continue
		}
		setChildState(children, fin.Parent, fin.ID, true)
	}
	for i := range items {
		it := items[i]
		if it.ID == "" || it.Parent == "" || it.ID == it.Parent {
			continue
		}
		if parent, ok := byID[it.Parent]; ok && !sameSource(parent.Source, it.Source) {
			continue
		}
		setChildState(children, it.Parent, it.ID, it.Done)
	}
	return children
}

func setChildState(children map[string]map[string]bool, parentID, childID string, done bool) {
	group, ok := children[parentID]
	if !ok {
		group = make(map[string]bool)
		children[parentID] = group
	}
	group[childID] = done
}

func sameSource(a, b string) bool {
	return a == "" || b == "" || a == b
}

func evaluateItem(it source.Item, childStates map[string]bool) Result {
	var doneChildren, openChildren []string
	for childID, done := range childStates {
		if done {
			doneChildren = append(doneChildren, childID)
		} else {
			openChildren = append(openChildren, childID)
		}
	}
	slices.Sort(doneChildren)
	slices.Sort(openChildren)

	res := Result{
		Source:       it.Source,
		ID:           it.ID,
		Title:        it.Title,
		DoneChildren: doneChildren,
		OpenChildren: openChildren,
	}
	switch {
	case it.Done:
		res.Reason = "already done"
	case len(doneChildren) == 0 && len(openChildren) == 0:
		res.Reason = "no children in local cache"
	case len(openChildren) == 1:
		res.Reason = fmt.Sprintf("1 open child: %s", openChildren[0])
	case len(openChildren) > 1:
		res.Reason = fmt.Sprintf("%d open children: %s", len(openChildren), strings.Join(openChildren, ", "))
	default:
		res.Eligible = true
	}
	return res
}
