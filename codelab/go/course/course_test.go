package course

import (
	"errors"
	"fmt"
	"testing"
)

func ExampleBuildGraph() {
	numCourses := 4
	// Must take [1] before [0]: [0] <- [1]
	prerequisites := [][]int{{1, 0}, {2, 0}, {3, 1}, {3, 2}}
	graph := BuildGraph(numCourses, prerequisites)
	fmt.Println(graph)
	// Output:
	// 0 (incoming: 0): [1 2]
	// 1 (incoming: 1): [3]
	// 2 (incoming: 1): [3]
	// 3 (incoming: 2): []
}

func ExampleKahnAlgorithm() {
	numCourses := 4
	// Must take [1] before [0]: [0] <- [1]
	prerequisites := [][]int{{1, 0}, {2, 0}, {3, 1}, {3, 2}}
	graph := BuildGraph(numCourses, prerequisites)
	order, err := KahnAlgorithm(graph)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Course order:", order)
	}
	// Output:
	// Course order: [0 1 2 3]
}

func ExampleKahnAlgorithm_cycle() {
	numCourses := 2
	// Must take [1] before [0]: [0] <- [1]
	// Must take [0] before [1]: [1] <- [0]
	prerequisites := [][]int{{1, 0}, {0, 1}}
	graph := BuildGraph(numCourses, prerequisites)
	order, err := KahnAlgorithm(graph)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Course order:", order)
	}
	// Output:
	// Error: cycle detected
}

// validateTopoOrder checks that order is a valid topological sort of the
// graph. A prerequisite {a, b} requires b to appear before a.
func validateTopoOrder(numCourses int, prerequisites [][]int, order []int) error {
	if len(order) != numCourses {
		return fmt.Errorf("got %d courses, want %d", len(order), numCourses)
	}
	pos := make(map[int]int, numCourses)
	for i, c := range order {
		if c < 0 || c >= numCourses {
			return fmt.Errorf("course %d out of range [0,%d)", c, numCourses)
		}
		if _, dup := pos[c]; dup {
			return fmt.Errorf("course %d appears twice", c)
		}
		pos[c] = i
	}
	for _, p := range prerequisites {
		a, b := p[0], p[1]
		if pos[b] >= pos[a] {
			return fmt.Errorf("prerequisite violated: %d must precede %d (positions %d, %d)", b, a, pos[b], pos[a])
		}
	}
	return nil
}

func TestTopologicalSort(t *testing.T) {
	tests := []struct {
		name          string
		numCourses    int
		prerequisites [][]int
		wantErr       error
	}{
		{
			name:       "single node",
			numCourses: 1,
		},
		{
			name:       "no dependencies",
			numCourses: 4,
		},
		{
			name:          "linear chain",
			numCourses:    5,
			prerequisites: [][]int{{1, 0}, {2, 1}, {3, 2}, {4, 3}},
		},
		{
			name:          "diamond",
			numCourses:    4,
			prerequisites: [][]int{{1, 0}, {2, 0}, {3, 1}, {3, 2}},
		},
		{
			name:       "disconnected components",
			numCourses: 6,
			// Chain A: 0 -> 1 -> 2; Chain B: 3 -> 4 -> 5.
			prerequisites: [][]int{{1, 0}, {2, 1}, {4, 3}, {5, 4}},
		},
		{
			name:       "multi-source multi-sink",
			numCourses: 6,
			// Sources 0,1 feed into 2,3; both feed into sinks 4,5.
			prerequisites: [][]int{{2, 0}, {2, 1}, {3, 1}, {4, 2}, {5, 2}, {5, 3}},
		},
		{
			name:       "binary tree fan-out",
			numCourses: 7,
			// 0 is the root; 1,2 are inner; 3,4,5,6 are leaves.
			prerequisites: [][]int{{1, 0}, {2, 0}, {3, 1}, {4, 1}, {5, 2}, {6, 2}},
		},
		{
			name:          "self-loop",
			numCourses:    3,
			prerequisites: [][]int{{1, 0}, {2, 2}},
			wantErr:       ErrCycleDetected,
		},
		{
			name:          "three-node cycle",
			numCourses:    3,
			prerequisites: [][]int{{1, 0}, {2, 1}, {0, 2}},
			wantErr:       ErrCycleDetected,
		},
		{
			name:       "cycle inside larger graph",
			numCourses: 6,
			// Clean chain 0->1->2, plus cycle 3->4->5->3.
			prerequisites: [][]int{{1, 0}, {2, 1}, {4, 3}, {5, 4}, {3, 5}},
			wantErr:       ErrCycleDetected,
		},
	}

	algorithms := []struct {
		name string
		fn   func(*Graph) ([]int, error)
	}{
		{"Kahn", KahnAlgorithm},
		{"DFS", DFSTopologicalSort},
	}

	for _, alg := range algorithms {
		for _, tt := range tests {
			t.Run(alg.name+"/"+tt.name, func(t *testing.T) {
				g := BuildGraph(tt.numCourses, tt.prerequisites)
				order, err := alg.fn(g)
				if tt.wantErr != nil {
					if !errors.Is(err, tt.wantErr) {
						t.Fatalf("got err=%v, want %v", err, tt.wantErr)
					}
					return
				}
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				if vErr := validateTopoOrder(tt.numCourses, tt.prerequisites, order); vErr != nil {
					t.Fatalf("invalid topo order %v: %v", order, vErr)
				}
			})
		}
	}
}

func ExampleDFSTopologicalSort() {
	numCourses := 4
	// Must take [1] before [0]: [0] <- [1]
	prerequisites := [][]int{{1, 0}, {2, 0}, {3, 1}, {3, 2}}
	graph := BuildGraph(numCourses, prerequisites)
	order, err := DFSTopologicalSort(graph)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Course order:", order)
	}
	// Output:
	// Course order: [0 1 2 3]
}

func ExampleDFSTopologicalSort_cycle() {
	numCourses := 2
	// Must take [1] before [0]: [0] <- [1]
	// Must take [0] before [1]: [1] <- [0]
	prerequisites := [][]int{{1, 0}, {0, 1}}
	graph := BuildGraph(numCourses, prerequisites)
	order, err := DFSTopologicalSort(graph)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Course order:", order)
	}
	// Output:
	// Error: cycle detected
}
