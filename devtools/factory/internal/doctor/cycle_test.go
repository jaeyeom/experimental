package doctor

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestCycles(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		items map[string]source.Item
		want  []Finding
	}{
		{
			name: "2-cycle",
			items: map[string]source.Item{
				"A": epicItem("A", "B", "B"),
				"B": epicItem("B", "A"),
			},
			want: []Finding{cycleWant("A", "B", "A")},
		},
		{
			name: "3-cycle",
			items: map[string]source.Item{
				"A": epicItem("A", "C"),
				"B": epicItem("B", "A"),
				"C": epicItem("C", "B"),
			},
			want: []Finding{cycleWant("A", "B", "C", "A")},
		},
		{
			name: "two-cycles",
			items: map[string]source.Item{
				"A": epicItem("A", "B"),
				"B": epicItem("B", "A"),
				"C": epicItem("C", "D"),
				"D": epicItem("D", "C"),
			},
			want: []Finding{
				cycleWant("A", "B", "A"),
				cycleWant("C", "D", "C"),
			},
		},
		{
			name: "self-loop",
			items: map[string]source.Item{
				"A": epicItem("A", "A"),
			},
			want: []Finding{cycleWant("A", "A")},
		},
		{
			name: "self-loop-and-cycle",
			items: map[string]source.Item{
				"A": epicItem("A", "A", "B"),
				"B": epicItem("B", "A"),
			},
			want: []Finding{
				cycleWant("A", "A"),
				cycleWant("A", "B", "A"),
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			equalFindings(t, mustCheck(t, baseInput(tc.items)), tc.want)
		})
	}
}

func epicItem(id string, blockers ...string) source.Item {
	return jiraItem(id, "Epic", "To Do", "", false, nil, blockers...)
}
