package scan

import (
	"encoding/json"
	"testing"
)

func TestSummarize(t *testing.T) {
	t.Parallel()

	id := strPtr("PROJ-1")
	tab := &Tab{TabID: "w9:tD"}

	tests := []struct {
		name string
		prs  []PR
		want Summary
	}{
		{
			name: "empty",
			prs:  nil,
			want: Summary{},
		},
		{
			name: "tab here",
			prs:  []PR{{Identifier: id, Tab: tab}},
			want: Summary{Total: 1, TabHere: 1},
		},
		{
			name: "off machine has identifier but no tab",
			prs:  []PR{{Identifier: id}},
			want: Summary{Total: 1, OffMachine: 1},
		},
		{
			name: "no identifier and no tab is not off machine",
			prs:  []PR{{}},
			want: Summary{Total: 1, NoIdentifier: 1},
		},
		{
			name: "no identifier with a tab still counts tab here",
			prs:  []PR{{Tab: tab}},
			want: Summary{Total: 1, TabHere: 1, NoIdentifier: 1},
		},
		{
			name: "draft is independent of tab buckets",
			prs: []PR{
				{Identifier: id, Tab: tab, IsDraft: true},
				{Identifier: id, IsDraft: true},
			},
			want: Summary{Total: 2, TabHere: 1, OffMachine: 1, Drafts: 2},
		},
		{
			name: "mixed rollup",
			prs: []PR{
				{Identifier: id, Tab: tab},
				{Identifier: id, Tab: tab},
				{Identifier: id},
				{Identifier: id},
				{},
				{IsDraft: true, Identifier: id, Tab: tab},
			},
			want: Summary{Total: 6, TabHere: 3, OffMachine: 2, NoIdentifier: 1, Drafts: 1},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := summarize(tc.prs)
			if got != tc.want {
				t.Fatalf("summarize() = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestSummaryJSONKeys(t *testing.T) {
	t.Parallel()

	got, err := json.Marshal(Summary{
		Total:        17,
		TabHere:      6,
		OffMachine:   9,
		NoIdentifier: 2,
		Drafts:       4,
	})
	if err != nil {
		t.Fatal(err)
	}
	want := `{"total":17,"tab_here":6,"off_machine":9,"no_identifier":2,"drafts":4}`
	if string(got) != want {
		t.Fatalf("json = %s, want %s", got, want)
	}
}
