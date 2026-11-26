package doctor

import (
	"fmt"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const cycleArrow = " → "

// cycles reports each strongly connected component of size greater than one,
// plus every self-loop. A self-loop inside a larger component is its own finding.
func cycles(in Input) ([]Finding, error) {
	graph := blockGraph(in.Snapshot.Items)
	var out []Finding
	for _, id := range selfLoops(graph) {
		found, err := cycleFinding([]string{id, id})
		if err != nil {
			return nil, err
		}
		out = append(out, found)
	}
	sccs, err := tarjanSCCs(graph)
	if err != nil {
		return nil, err
	}
	for _, scc := range sccs {
		if len(scc) < 2 {
			continue
		}
		path, ok := cycleThrough(scc, graph)
		if !ok {
			return nil, fmt.Errorf("cycle: expected a cycle among %s, found none", strings.Join(scc, ", "))
		}
		found, err := cycleFinding(path)
		if err != nil {
			return nil, err
		}
		out = append(out, found)
	}
	return out, nil
}

func cycleFinding(path []string) (Finding, error) {
	if len(path) < 2 {
		return Finding{}, fmt.Errorf("cycle: expected a path of at least two ids, found %d", len(path))
	}
	items := path
	if path[0] == path[len(path)-1] {
		items = path[:len(path)-1]
	}
	return finding(
		CheckCycle,
		SeverityWarn,
		strings.Join(path, cycleArrow),
		fmt.Sprintf("factory unlink %s %s", path[0], path[1]),
		items...,
	), nil
}

// blockGraph returns blocker → blocked edges.
// Item.BlockedBy lists blockers of that item, so the edge runs from the
// blocker to the item. Unknown blockers are omitted. Duplicate edges are one.
func blockGraph(items map[string]source.Item) map[string][]string {
	graph := make(map[string][]string, len(items))
	ids := sortedIDs(items)
	for _, id := range ids {
		graph[id] = []string{}
	}
	seen := make(map[string]struct{})
	for _, blocked := range ids {
		for _, ref := range items[blocked].BlockedBy {
			if _, ok := items[ref.ID]; !ok {
				continue
			}
			key := ref.ID + "\x00" + blocked
			if _, ok := seen[key]; ok {
				continue
			}
			seen[key] = struct{}{}
			graph[ref.ID] = append(graph[ref.ID], blocked)
		}
	}
	for id := range graph {
		slices.Sort(graph[id])
	}
	return graph
}

func selfLoops(graph map[string][]string) []string {
	var ids []string
	for id, next := range graph {
		if slices.Contains(next, id) {
			ids = append(ids, id)
		}
	}
	slices.Sort(ids)
	return ids
}

func cycleThrough(scc []string, graph map[string][]string) ([]string, bool) {
	allow := make(map[string]struct{}, len(scc))
	for _, id := range scc {
		allow[id] = struct{}{}
	}
	start := slices.Min(scc)
	for _, next := range graph[start] {
		if next == start {
			continue
		}
		if _, ok := allow[next]; !ok {
			continue
		}
		if path, ok := pathTo(start, next, allow, graph); ok {
			return path, true
		}
	}
	return nil, false
}

type walk struct {
	id   string
	next int
}

// pathTo finds a simple cycle start → first → … → start inside allow.
func pathTo(start, first string, allow map[string]struct{}, graph map[string][]string) ([]string, bool) {
	visited := map[string]bool{start: true, first: true}
	path := []string{start, first}
	stack := []walk{{id: first}}
	for len(stack) > 0 {
		top := len(stack) - 1
		id := stack[top].id
		edges := graph[id]
		if stack[top].next >= len(edges) {
			stack = stack[:top]
			path = path[:len(path)-1]
			delete(visited, id)
			continue
		}
		next := edges[stack[top].next]
		stack[top].next++
		if next == start {
			return append(append([]string{}, path...), start), true
		}
		if _, ok := allow[next]; !ok || visited[next] {
			continue
		}
		visited[next] = true
		path = append(path, next)
		stack = append(stack, walk{id: next})
	}
	return nil, false
}

type tarjan struct {
	graph map[string][]string
	index int
	stack []string
	on    map[string]bool
	idx   map[string]int
	low   map[string]int
	sccs  [][]string
}

func tarjanSCCs(graph map[string][]string) ([][]string, error) {
	ids := make([]string, 0, len(graph))
	for id := range graph {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	t := &tarjan{
		graph: graph,
		on:    map[string]bool{},
		idx:   map[string]int{},
		low:   map[string]int{},
	}
	for _, id := range ids {
		if _, seen := t.idx[id]; seen {
			continue
		}
		if err := t.visit(id); err != nil {
			return nil, err
		}
	}
	return t.sccs, nil
}

func (t *tarjan) visit(v string) error {
	t.index++
	t.idx[v] = t.index
	t.low[v] = t.index
	t.stack = append(t.stack, v)
	t.on[v] = true
	for _, w := range t.graph[v] {
		if _, seen := t.idx[w]; !seen {
			if err := t.visit(w); err != nil {
				return err
			}
			t.low[v] = min(t.low[v], t.low[w])
			continue
		}
		if t.on[w] {
			t.low[v] = min(t.low[v], t.idx[w])
		}
	}
	if t.low[v] == t.idx[v] {
		return t.popSCC(v)
	}
	return nil
}

func (t *tarjan) popSCC(v string) error {
	var scc []string
	for {
		if len(t.stack) == 0 {
			return fmt.Errorf("cycle: expected %s on the stack, found empty", v)
		}
		n := len(t.stack) - 1
		w := t.stack[n]
		t.stack = t.stack[:n]
		t.on[w] = false
		scc = append(scc, w)
		if w == v {
			t.sccs = append(t.sccs, scc)
			return nil
		}
	}
}
