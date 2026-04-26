// Package course implements topological sorting algorithms for course
// scheduling: Kahn's algorithm (BFS) and DFS-based.
package course

import (
	"errors"
	"fmt"
	"slices"
	"strings"
)

var ErrCycleDetected = errors.New("cycle detected")

type Graph struct {
	AdjList       [][]int
	IncomingEdges []int
}

func BuildGraph(numCourses int, prerequisites [][]int) *Graph {
	adjList := make([][]int, numCourses)
	incomingEdges := make([]int, numCourses)
	for _, p := range prerequisites {
		incomingEdges[p[0]]++
		adjList[p[1]] = append(adjList[p[1]], p[0])
	}
	return &Graph{
		AdjList:       adjList,
		IncomingEdges: incomingEdges,
	}
}

func (g Graph) String() string {
	w := strings.Builder{}
	for i := range g.AdjList {
		fmt.Fprintf(&w, "%d (incoming: %d): %v\n", i, g.IncomingEdges[i], g.AdjList[i])
	}
	return w.String()
}

// Queue is a simple queue implementation using a slice and a head index. It
// supports enqueue and dequeue operations in O(1) time, but does not support
// shrinking the underlying slice, so it may use more memory than necessary if
// many elements are dequeued.
type Queue struct {
	data []int
	head int
}

// NewQueue creates a new Queue with the given capacity. The capacity is used to
// preallocate the underlying slice, but the queue can grow beyond this capacity
// if more elements are enqueued.
func NewQueue(capacity int) *Queue {
	return &Queue{
		data: make([]int, 0, capacity),
		head: 0,
	}
}

// Enqueue adds an element to the back of the queue.
func (q *Queue) Enqueue(e int) {
	q.data = append(q.data, e)
}

// Dequeue removes and returns the element at the front of the queue. If the
// queue is empty, it returns an error.
func (q *Queue) Dequeue() (int, bool) {
	if q.head >= len(q.data) {
		return 0, false
	}
	retVal := q.data[q.head]
	q.head++
	return retVal, true
}

// Empty returns true if the queue is empty, and false otherwise.
func (q *Queue) Empty() bool {
	return q.head == len(q.data)
}

// KahnAlgorithm returns a valid topological ordering of the courses given the
// prerequisites, or an error if a cycle is detected.
func KahnAlgorithm(g *Graph) ([]int, error) {
	sorted := make([]int, 0, len(g.AdjList))
	q := NewQueue(len(g.AdjList))
	for i, incoming := range g.IncomingEdges {
		if incoming == 0 {
			q.Enqueue(i)
		}
	}
	incomingEdges := slices.Clone(g.IncomingEdges)
	for !q.Empty() {
		next, _ := q.Dequeue()
		sorted = append(sorted, next)
		for _, neighbor := range g.AdjList[next] {
			incomingEdges[neighbor]--
			if incomingEdges[neighbor] == 0 {
				q.Enqueue(neighbor)
			}
		}
	}
	if len(sorted) != len(g.AdjList) {
		return nil, ErrCycleDetected
	}
	return sorted, nil
}

// DFS performs a depth-first search on the graph starting from startNode, marking
// nodes as removed in the removed map and accumulating the topological order in
// acc. It returns an error if a cycle is detected.
func DFS(g *Graph, startNode int, removed map[int]struct{}, acc *[]int) error {
	// Node and current index
	if _, ok := removed[startNode]; ok {
		return nil
	}
	var dfsStack [][2]int
	visited := map[int]struct{}{}
	dfsStack = append(dfsStack, [2]int{startNode, len(g.AdjList[startNode]) - 1})
outer:
	for len(dfsStack) != 0 {
		current := dfsStack[len(dfsStack)-1]
		curNeighbors := g.AdjList[current[0]]
		for i := current[1]; i >= 0; i-- {
			neighbor := curNeighbors[i]
			if _, ok := removed[neighbor]; ok {
				continue
			}
			if _, ok := visited[neighbor]; ok {
				return ErrCycleDetected
			}
			dfsStack[len(dfsStack)-1][1] = i - 1
			dfsStack = append(dfsStack, [2]int{neighbor, len(g.AdjList[neighbor]) - 1})
			visited[neighbor] = struct{}{}
			continue outer
		}
		removed[current[0]] = struct{}{}
		*acc = append(*acc, current[0])
		dfsStack = dfsStack[:len(dfsStack)-1]
	}
	return nil
}

// DFSTopologicalSort returns a valid topological ordering of the courses given
// the prerequisites, or an error if a cycle is detected. It uses a depth-first
// search approach to perform the topological sort. The time complexity is O(V +
// E) where V is the number of vertices (courses) and E is the number of edges
// (prerequisites). The space complexity is O(V) due to the recursion stack and
// the removed map.
func DFSTopologicalSort(g *Graph) ([]int, error) {
	result := make([]int, 0, len(g.AdjList))
	removed := map[int]struct{}{}
	for i := range g.AdjList {
		if err := DFS(g, i, removed, &result); err != nil {
			return nil, err
		}
	}
	slices.Reverse(result)
	return result, nil
}
