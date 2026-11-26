package stats

import (
	"cmp"
	"fmt"
	"slices"

	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
)

// EpicNote is the header of the per-epic section.
const EpicNote = "counts cover only items in the source queries, plus finished history"

// Node states in the epic counts and the --epic tree.
const (
	StateDone       = "done"
	StateInProgress = "in_progress"
	StateUnknown    = "not_mirrored"
)

// Epics is the per-parent section.
type Epics struct {
	Note    string    `json:"note"`
	Parents []EpicRow `json:"parents"` // Each direct parent, over its children.
	// Each top-level parent, over its leaf descendants.
	TopLevel []EpicRow `json:"top_level"` //nolint:tagliatelle // documented factory JSON output field
}

// EpicRow is one parent's progress.
type EpicRow struct {
	Source     string `json:"source"`
	ID         string `json:"id"`
	Title      string `json:"title"`
	Priority   bool   `json:"parent_priority"` //nolint:tagliatelle // documented factory JSON output field
	Done       int    `json:"done"`
	InProgress int    `json:"in_progress"` //nolint:tagliatelle // documented factory JSON output field
	Remaining  int    `json:"remaining"`
	// done * 100 / total, rounded down.
	Percent int `json:"percent_complete"` //nolint:tagliatelle // documented factory JSON output field
	Added   int `json:"added"`            // Counted items created in the window.
}

// TreeNode is one line of the --epic tree.
type TreeNode struct {
	Depth int    `json:"depth"`
	ID    string `json:"id"`
	State string `json:"state"`
	Title string `json:"title"`
}

func (d *data) epics() (Epics, error) {
	out := Epics{Note: EpicNote, Parents: []EpicRow{}, TopLevel: []EpicRow{}}
	tops := map[itemKey]bool{}
	for p, kids := range d.children {
		if !d.inScope(p) {
			continue
		}
		out.Parents = append(out.Parents, d.epicRow(p, kids))
		top, err := d.top(p)
		if err != nil {
			return Epics{}, err
		}
		tops[top] = true
	}
	for t := range tops {
		leaves, err := d.leaves(t, map[itemKey]bool{})
		if err != nil {
			return Epics{}, err
		}
		out.TopLevel = append(out.TopLevel, d.epicRow(t, leaves))
	}
	sortRows(out.Parents)
	sortRows(out.TopLevel)
	return out, nil
}

func sortRows(rows []EpicRow) {
	slices.SortFunc(rows, func(a, b EpicRow) int {
		if c := cmp.Compare(b.Remaining, a.Remaining); c != 0 {
			return c
		}
		if c := cmp.Compare(a.Source, b.Source); c != 0 {
			return c
		}
		return cmp.Compare(a.ID, b.ID)
	})
}

// top climbs parents while the parent is mirrored or finished and in scope.
func (d *data) top(k itemKey) (itemKey, error) {
	seen := map[itemKey]bool{}
	for {
		if seen[k] {
			return itemKey{}, fmt.Errorf("stats: expected a parent tree, found a cycle at %s", k)
		}
		seen[k] = true
		n, ok := d.nodes[k]
		if !ok || n.parent == "" {
			return k, nil
		}
		up := itemKey{k.source, n.parent}
		if !d.inScope(up) {
			return k, nil
		}
		k = up
	}
}

// leaves is every descendant of k that has no children.
func (d *data) leaves(k itemKey, seen map[itemKey]bool) ([]itemKey, error) {
	var out []itemKey
	for _, c := range d.children[k] {
		if seen[c] {
			return nil, fmt.Errorf("stats: expected a parent tree, found a cycle at %s", c)
		}
		seen[c] = true
		if len(d.children[c]) == 0 {
			out = append(out, c)
			continue
		}
		more, err := d.leaves(c, seen)
		if err != nil {
			return nil, err
		}
		out = append(out, more...)
	}
	return out, nil
}

func (d *data) epicRow(p itemKey, items []itemKey) EpicRow {
	row := EpicRow{Source: p.source, ID: p.id, Title: d.nodes[p].title, Priority: d.priority(p)}
	for _, k := range items {
		switch d.state(k) {
		case StateDone:
			row.Done++
		case StateInProgress:
			row.InProgress++
		default:
			row.Remaining++
		}
		if d.inWindow(d.nodes[k].created) {
			row.Added++
		}
	}
	if total := row.Done + row.InProgress + row.Remaining; total > 0 {
		row.Percent = row.Done * 100 / total
	}
	return row
}

func (d *data) priority(p itemKey) bool {
	view, ok := d.views[p.source]
	return ok && slices.Contains(view.Cfg.ParentPriority, p.id)
}

// state is done, in_progress, the backlog bucket of an open mirrored item,
// or not_mirrored for a parent known only from its children.
func (d *data) state(k itemKey) string {
	n, ok := d.nodes[k]
	switch {
	case !ok:
		return StateUnknown
	case n.done:
		return StateDone
	case !n.mirrored:
		return StateUnknown
	}
	bucket := d.views[k.source].Classify(n.item)
	if bucket == backlog.InFlight {
		return StateInProgress
	}
	return string(bucket)
}

func (d *data) tree() []TreeNode {
	var out []TreeNode
	var walk func(k itemKey, depth int)
	walk = func(k itemKey, depth int) {
		out = append(out, TreeNode{Depth: depth, ID: k.id, State: d.state(k), Title: d.nodes[k].title})
		for _, c := range d.children[k] {
			walk(c, depth+1)
		}
	}
	walk(d.root, 0)
	return out
}
