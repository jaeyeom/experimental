package git

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestParseCurrentDiff(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name     string
		input    string
		expected []models.LineChange
	}{
		{
			name:     "empty diff",
			input:    "",
			expected: []models.LineChange{},
		},
		{
			name: "simple addition",
			input: `@@ -10,3 +10,4 @@
 context line 1
 context line 2
+added line
 context line 3`,
			expected: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 12, NewLine: 12},
			},
		},
		{
			name: "simple deletion",
			input: `@@ -10,4 +10,3 @@
 context line 1
 context line 2
-deleted line
 context line 3`,
			expected: []models.LineChange{
				{Type: models.LineDeleted, OriginalLine: 12, NewLine: -1},
			},
		},
		{
			name: "mixed changes",
			input: `@@ -10,5 +10,5 @@
 context line 1
-deleted line
+added line
 context line 2
 context line 3`,
			expected: []models.LineChange{
				{Type: models.LineDeleted, OriginalLine: 11, NewLine: -1},
				{Type: models.LineAdded, OriginalLine: 12, NewLine: 11},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := gc.parseCurrentDiff(tt.input)
			if err != nil {
				t.Fatalf("parseCurrentDiff() error = %v", err)
			}

			if len(result) != len(tt.expected) {
				t.Fatalf("parseCurrentDiff() returned %d changes, want %d", len(result), len(tt.expected))
			}

			for i, change := range result {
				expected := tt.expected[i]
				if change.Type != expected.Type {
					t.Errorf("Change %d: Type = %v, want %v", i, change.Type, expected.Type)
				}
				if change.OriginalLine != expected.OriginalLine {
					t.Errorf("Change %d: OriginalLine = %d, want %d", i, change.OriginalLine, expected.OriginalLine)
				}
				if change.NewLine != expected.NewLine {
					t.Errorf("Change %d: NewLine = %d, want %d", i, change.NewLine, expected.NewLine)
				}
			}
		})
	}
}

func TestGroupConsecutiveChanges(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name     string
		changes  []models.LineChange
		expected int // number of groups expected
	}{
		{
			name:     "empty changes",
			changes:  []models.LineChange{},
			expected: 0,
		},
		{
			name: "single change",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
			},
			expected: 1,
		},
		{
			name: "consecutive changes",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
				{Type: models.LineDeleted, OriginalLine: 11},
				{Type: models.LineAdded, OriginalLine: 12},
			},
			expected: 1,
		},
		{
			name: "non-consecutive changes",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
				{Type: models.LineDeleted, OriginalLine: 20},
				{Type: models.LineAdded, OriginalLine: 30},
			},
			expected: 3,
		},
		{
			name: "mixed consecutive and non-consecutive",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
				{Type: models.LineDeleted, OriginalLine: 11},
				{Type: models.LineAdded, OriginalLine: 25}, // gap here
				{Type: models.LineDeleted, OriginalLine: 26},
			},
			expected: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			groups := gc.groupConsecutiveChanges(tt.changes)
			if len(groups) != tt.expected {
				t.Errorf("groupConsecutiveChanges() returned %d groups, want %d", len(groups), tt.expected)
			}
		})
	}
}

func TestCalculateNetOffset(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name     string
		changes  []models.LineChange
		expected int
	}{
		{
			name:     "empty changes",
			changes:  []models.LineChange{},
			expected: 0,
		},
		{
			name: "only additions",
			changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineAdded},
				{Type: models.LineAdded},
			},
			expected: 3,
		},
		{
			name: "only deletions",
			changes: []models.LineChange{
				{Type: models.LineDeleted},
				{Type: models.LineDeleted},
			},
			expected: -2,
		},
		{
			name: "mixed additions and deletions",
			changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
				{Type: models.LineAdded},
			},
			expected: 1,
		},
		{
			name: "equal additions and deletions",
			changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
			},
			expected: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := gc.calculateNetOffset(tt.changes)
			if result != tt.expected {
				t.Errorf("calculateNetOffset() = %d, want %d", result, tt.expected)
			}
		})
	}
}

func TestCalculateOverallConfidence(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name        string
		suggestions []models.MappingSuggestion
		expected    models.ConfidenceLevel
	}{
		{
			name:        "empty suggestions",
			suggestions: []models.MappingSuggestion{},
			expected:    models.ConfidenceLow,
		},
		{
			name: "all high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
			},
			expected: models.ConfidenceHigh,
		},
		{
			name: "majority high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceMedium},
			},
			expected: models.ConfidenceHigh,
		},
		{
			name: "minority high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceMedium},
				{Confidence: models.ConfidenceLow},
			},
			expected: models.ConfidenceMedium,
		},
		{
			name: "no high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceMedium},
				{Confidence: models.ConfidenceLow},
			},
			expected: models.ConfidenceMedium,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := gc.calculateOverallConfidence(tt.suggestions)
			if result != tt.expected {
				t.Errorf("calculateOverallConfidence() = %v, want %v", result, tt.expected)
			}
		})
	}
}
