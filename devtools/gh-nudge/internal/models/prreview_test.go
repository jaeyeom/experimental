package models

import (
	"encoding/json"
	"strings"
	"testing"
)

// TestParseOperationType tests the ParseOperationType function.
func TestParseOperationType(t *testing.T) {
	t.Run("Valid operations", func(t *testing.T) {
		// Test that all valid operations parse successfully and round-trip correctly
		validOps := []OperationType{OperationDelete, OperationInsert, OperationChange}
		for _, op := range validOps {
			parsed, err := ParseOperationType(op.String())
			if err != nil {
				t.Errorf("ParseOperationType(%q) unexpected error: %v", op.String(), err)
			}
			if parsed != op {
				t.Errorf("ParseOperationType(%q) = %v, want %v", op.String(), parsed, op)
			}
		}
	})

	t.Run("Invalid operations return error", func(t *testing.T) {
		// Test various invalid inputs to ensure they all return errors
		invalidInputs := []string{
			"x",      // wrong letter
			"D",      // uppercase
			"",       // empty
			"delete", // full word
			"ab",     // multiple chars
			"1",      // number
			" d ",    // with spaces
			"d\n",    // with newline
		}
		for _, input := range invalidInputs {
			_, err := ParseOperationType(input)
			if err == nil {
				t.Errorf("ParseOperationType(%q) expected error, got nil", input)
			}
		}
	})

	t.Run("Error messages are informative", func(t *testing.T) {
		// Verify error messages contain the invalid input for debugging
		_, err := ParseOperationType("x")
		if err == nil {
			t.Fatal("expected error for invalid operation")
		}
		if !strings.Contains(err.Error(), "x") {
			t.Errorf("error message should contain the invalid input 'x', got: %v", err)
		}
		if !strings.Contains(err.Error(), "unknown") || !strings.Contains(err.Error(), "operation") {
			t.Errorf("error message should indicate unknown operation, got: %v", err)
		}
	})
}

// TestParseLineSpec tests the ParseLineSpec function.
func TestParseLineSpec(t *testing.T) {
	tests := []struct {
		name      string
		spec      string
		want      *LineRange
		wantError bool
	}{
		{
			name: "Single line",
			spec: "42",
			want: &LineRange{StartLine: 42, EndLine: 42},
		},
		{
			name: "Line range",
			spec: "10-20",
			want: &LineRange{StartLine: 10, EndLine: 20},
		},
		{
			name: "Range with spaces",
			spec: "5 - 15",
			want: &LineRange{StartLine: 5, EndLine: 15},
		},
		{
			name:      "Invalid start line",
			spec:      "abc-10",
			wantError: true,
		},
		{
			name:      "Invalid end line",
			spec:      "10-xyz",
			wantError: true,
		},
		{
			name:      "Invalid single line",
			spec:      "notanumber",
			wantError: true,
		},
		{
			name:      "Start greater than end",
			spec:      "20-10",
			wantError: true,
		},
		{
			name:      "Too many dashes",
			spec:      "10-20-30",
			wantError: true,
		},
		{
			name:      "Empty spec",
			spec:      "",
			wantError: true,
		},
		{
			name: "Single digit",
			spec: "1",
			want: &LineRange{StartLine: 1, EndLine: 1},
		},
		{
			name: "Large line numbers",
			spec: "1000-2000",
			want: &LineRange{StartLine: 1000, EndLine: 2000},
		},
		{
			name: "Same start and end",
			spec: "100-100",
			want: &LineRange{StartLine: 100, EndLine: 100},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseLineSpec(tt.spec)
			if (err != nil) != tt.wantError {
				t.Errorf("ParseLineSpec(%q) error = %v, wantError %v", tt.spec, err, tt.wantError)
				return
			}
			if tt.wantError {
				return
			}
			if got == nil {
				t.Errorf("ParseLineSpec(%q) returned nil, want %+v", tt.spec, tt.want)
				return
			}
			if got.StartLine != tt.want.StartLine || got.EndLine != tt.want.EndLine {
				t.Errorf("ParseLineSpec(%q) = %+v, want %+v", tt.spec, got, tt.want)
			}
		})
	}
}

// TestGenerateCommentID tests that GenerateCommentID produces unique IDs.
func TestGenerateCommentID(t *testing.T) {
	ids := make(map[string]bool)
	numIDs := 1000

	for i := 0; i < numIDs; i++ {
		id := GenerateCommentID()

		// Check length
		if len(id) != 40 {
			t.Errorf("Generated ID has wrong length: got %d, want 40", len(id))
		}

		// Check uniqueness
		if ids[id] {
			t.Errorf("Duplicate ID generated: %s", id)
		}
		ids[id] = true

		// Check format (should be hex)
		for _, char := range id {
			if (char < '0' || char > '9') && (char < 'a' || char > 'f') {
				t.Errorf("Invalid character in ID: %c", char)
			}
		}
	}
}

func TestCommentMatchesIDPrefix(t *testing.T) {
	tests := []struct {
		name     string
		comment  Comment
		prefix   string
		expected bool
	}{
		{
			name:     "Full match",
			comment:  Comment{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12"},
			prefix:   "a1b2c3d4e5f67890abcdef1234567890abcdef12",
			expected: true,
		},
		{
			name:     "Prefix match",
			comment:  Comment{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12"},
			prefix:   "a1b2c3d4",
			expected: true,
		},
		{
			name:     "Short prefix match",
			comment:  Comment{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12"},
			prefix:   "a1",
			expected: true,
		},
		{
			name:     "No match",
			comment:  Comment{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12"},
			prefix:   "b1",
			expected: false,
		},
		{
			name:     "Empty prefix",
			comment:  Comment{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12"},
			prefix:   "",
			expected: false,
		},
		{
			name:     "Case sensitive",
			comment:  Comment{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12"},
			prefix:   "A1B2",
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.comment.MatchesIDPrefix(tt.prefix)
			if result != tt.expected {
				t.Errorf("MatchesIDPrefix(%q) = %v, want %v", tt.prefix, result, tt.expected)
			}
		})
	}
}

func TestCommentFormatIDShort(t *testing.T) {
	tests := []struct {
		name     string
		comment  Comment
		expected string
	}{
		{
			name:     "Full length ID",
			comment:  Comment{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12"},
			expected: "a1b2c3d4",
		},
		{
			name:     "Short ID",
			comment:  Comment{ID: "abc123"},
			expected: "abc123",
		},
		{
			name:     "Empty ID",
			comment:  Comment{ID: ""},
			expected: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.comment.FormatIDShort()
			if result != tt.expected {
				t.Errorf("FormatIDShort() = %q, want %q", result, tt.expected)
			}
		})
	}
}

// TestConcurrentCommentIDGeneration tests that concurrent ID generation works correctly.
func TestConcurrentCommentIDGeneration(t *testing.T) {
	// Test concurrent generation
	numGoroutines := 10
	numIDsPerGoroutine := 100
	idsChan := make(chan string, numGoroutines*numIDsPerGoroutine)

	for i := 0; i < numGoroutines; i++ {
		go func() {
			for j := 0; j < numIDsPerGoroutine; j++ {
				idsChan <- GenerateCommentID()
			}
		}()
	}

	// Collect all IDs
	allIDs := make(map[string]bool)
	for i := 0; i < numGoroutines*numIDsPerGoroutine; i++ {
		id := <-idsChan
		if allIDs[id] {
			t.Errorf("Duplicate ID generated in concurrent test: %s", id)
		}
		allIDs[id] = true
	}
}

// TestPrefixMatchingAmbiguity tests handling of ambiguous prefixes.
func TestPrefixMatchingAmbiguity(t *testing.T) {
	comments := []Comment{
		{ID: "a1b2c3d4e5f67890abcdef1234567890abcdef12", Body: "First comment"},
		{ID: "a1b2c3d4ffffff90abcdef1234567890abcdef12", Body: "Second comment"},
		{ID: "a1b7890123456790abcdef1234567890abcdef12", Body: "Third comment"},
		{ID: "b1234567890abcde1234567890abcdef12345678", Body: "Fourth comment"},
	}

	tests := []struct {
		name            string
		prefix          string
		expectedMatches int
	}{
		{
			name:            "Unique long prefix",
			prefix:          "a1b2c3d4e5",
			expectedMatches: 1,
		},
		{
			name:            "Ambiguous short prefix",
			prefix:          "a1b2c3d4",
			expectedMatches: 2,
		},
		{
			name:            "Very ambiguous prefix",
			prefix:          "a1b",
			expectedMatches: 3,
		},
		{
			name:            "Different prefix",
			prefix:          "b12",
			expectedMatches: 1,
		},
		{
			name:            "No match",
			prefix:          "c12",
			expectedMatches: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			matches := 0
			for _, comment := range comments {
				if comment.MatchesIDPrefix(tt.prefix) {
					matches++
				}
			}
			if matches != tt.expectedMatches {
				t.Errorf("Expected %d matches for prefix %q, got %d", tt.expectedMatches, tt.prefix, matches)
			}
		})
	}
}

// TestCommentIDInJSON tests that comment IDs are properly generated and serialized/deserialized.
func TestCommentIDInJSON(t *testing.T) {
	original := Comment{
		ID:   GenerateCommentID(),
		Path: "test.go",
		Line: NewSingleLine(42),
		Body: "Test comment",
		Side: "RIGHT",
	}

	// Test that the ID is preserved through JSON marshaling
	// This is important for storage persistence
	if original.ID == "" {
		t.Error("Comment ID should not be empty")
	}

	if len(original.ID) != 40 {
		t.Errorf("Comment ID should be 40 characters, got %d", len(original.ID))
	}

	// Verify it's a valid hex string
	for _, char := range strings.ToLower(original.ID) {
		if (char < '0' || char > '9') && (char < 'a' || char > 'f') {
			t.Errorf("Invalid character in comment ID: %c", char)
		}
	}

	// Test JSON serialization/deserialization
	jsonData, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("Failed to marshal comment to JSON: %v", err)
	}

	var deserialized Comment
	err = json.Unmarshal(jsonData, &deserialized)
	if err != nil {
		t.Fatalf("Failed to unmarshal comment from JSON: %v", err)
	}

	// Verify all fields are preserved
	if deserialized.ID != original.ID {
		t.Errorf("ID not preserved: expected %q, got %q", original.ID, deserialized.ID)
	}
	if deserialized.Path != original.Path {
		t.Errorf("Path not preserved: expected %q, got %q", original.Path, deserialized.Path)
	}
	if deserialized.Line != original.Line {
		t.Errorf("Line not preserved: expected %v, got %v", original.Line, deserialized.Line)
	}
	if deserialized.Body != original.Body {
		t.Errorf("Body not preserved: expected %q, got %q", original.Body, deserialized.Body)
	}
	if deserialized.Side != original.Side {
		t.Errorf("Side not preserved: expected %q, got %q", original.Side, deserialized.Side)
	}
}

// TestCommentGetLineRange tests the GetLineRange method.
func TestCommentGetLineRange(t *testing.T) {
	t.Run("Single line comment", func(t *testing.T) {
		comment := Comment{
			Line: NewSingleLine(42),
			Path: "test.go",
		}
		lineRange := comment.GetLineRange()
		if lineRange.StartLine != 42 || lineRange.EndLine != 42 {
			t.Errorf("GetLineRange() for single line = {%d, %d}, want {42, 42}",
				lineRange.StartLine, lineRange.EndLine)
		}
	})

	t.Run("Multi-line comment", func(t *testing.T) {
		comment := Comment{
			Line: NewLineRange(10, 20),
			Path: "test.go",
		}
		lineRange := comment.GetLineRange()
		if lineRange.StartLine != 10 || lineRange.EndLine != 20 {
			t.Errorf("GetLineRange() for multi-line = {%d, %d}, want {10, 20}",
				lineRange.StartLine, lineRange.EndLine)
		}
	})

	t.Run("Multi-line comment with same start and end", func(t *testing.T) {
		comment := Comment{
			Line: NewLineRange(15, 15),
			Path: "test.go",
		}
		// Even though StartLine is set, if it equals Line, IsMultiLine() returns false
		lineRange := comment.GetLineRange()
		if lineRange.StartLine != 15 || lineRange.EndLine != 15 {
			t.Errorf("GetLineRange() for same start/end = {%d, %d}, want {15, 15}",
				lineRange.StartLine, lineRange.EndLine)
		}
	})

	t.Run("Consistency with IsMultiLine", func(t *testing.T) {
		// Verify that GetLineRange behavior is consistent with IsMultiLine
		testCases := []struct {
			name      string
			comment   Comment
			wantStart int
			wantEnd   int
		}{
			{
				name:      "Single line",
				comment:   Comment{Line: NewSingleLine(5)},
				wantStart: 5,
				wantEnd:   5,
			},
			{
				name:      "Multi-line",
				comment:   Comment{Line: NewLineRange(1, 10)},
				wantStart: 1,
				wantEnd:   10,
			},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				lineRange := tc.comment.GetLineRange()
				if lineRange.StartLine != tc.wantStart || lineRange.EndLine != tc.wantEnd {
					t.Errorf("GetLineRange() = {%d, %d}, want {%d, %d}",
						lineRange.StartLine, lineRange.EndLine, tc.wantStart, tc.wantEnd)
				}
			})
		}
	})
}

// TestCommentMatchesFilter tests the MatchesFilter method.
func TestCommentMatchesFilter(t *testing.T) {
	t.Run("Empty filter matches all", func(t *testing.T) {
		comments := []Comment{
			{Path: "foo.go", Line: NewSingleLine(10), Side: "RIGHT"},
			{Path: "bar.go", Line: NewSingleLine(20), Side: "LEFT"},
			{Path: "baz.go", Line: NewSingleLine(30), Side: "RIGHT"},
		}
		emptyFilter := CommentFilter{}
		for i, comment := range comments {
			if !comment.MatchesFilter(emptyFilter) {
				t.Errorf("Comment %d should match empty filter", i)
			}
		}
	})

	t.Run("File filter", func(t *testing.T) {
		comments := []Comment{
			{Path: "foo.go", Line: NewSingleLine(10)},
			{Path: "bar.go", Line: NewSingleLine(20)},
			{Path: "foo.go", Line: NewSingleLine(30)},
		}
		filter := CommentFilter{File: "foo.go"}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				if comment.Path != "foo.go" {
					t.Errorf("Comment with path %q should not match filter for 'foo.go'", comment.Path)
				}
			}
		}
		if matched != 2 {
			t.Errorf("Expected 2 matches for file filter, got %d", matched)
		}
	})

	t.Run("Side filter", func(t *testing.T) {
		comments := []Comment{
			{Path: "test.go", Line: NewSingleLine(10), Side: "LEFT"},
			{Path: "test.go", Line: NewSingleLine(20), Side: "RIGHT"},
			{Path: "test.go", Line: NewSingleLine(30), Side: "LEFT"},
		}
		filter := CommentFilter{Side: "LEFT"}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				if comment.Side != "LEFT" {
					t.Errorf("Comment with side %q should not match filter for 'LEFT'", comment.Side)
				}
			}
		}
		if matched != 2 {
			t.Errorf("Expected 2 matches for side filter, got %d", matched)
		}
	})

	t.Run("LineRange filter - single line", func(t *testing.T) {
		targetLine := 42
		comments := []Comment{
			{Path: "test.go", Line: NewSingleLine(42)},
			{Path: "test.go", Line: NewSingleLine(43)},
			{Path: "test.go", Line: NewSingleLine(42)},
		}
		filter := CommentFilter{LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine}}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				if comment.Line.EndLine != targetLine {
					t.Errorf("Comment with line %d should not match filter for line %d", comment.Line.EndLine, targetLine)
				}
			}
		}
		if matched != 2 {
			t.Errorf("Expected 2 matches for line filter, got %d", matched)
		}
	})

	t.Run("LineRange filter - range overlap", func(t *testing.T) {
		comments := []Comment{
			{Path: "test.go", Line: NewLineRange(10, 20)}, // overlaps: 10-20 overlaps with 15-25
			{Path: "test.go", Line: NewLineRange(15, 25)}, // overlaps: 15-25 overlaps with 15-25
			{Path: "test.go", Line: NewLineRange(26, 30)}, // no overlap: 26-30 does not overlap with 15-25
			{Path: "test.go", Line: NewSingleLine(18)},    // overlaps: single line 18 overlaps with 15-25
			{Path: "test.go", Line: NewSingleLine(10)},    // no overlap: single line 10 does not overlap with 15-25
		}
		filter := CommentFilter{LineRange: &LineRange{StartLine: 15, EndLine: 25}}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
			}
		}
		// Should match: first 3 comments overlap with range 15-25
		if matched != 3 {
			t.Errorf("Expected 3 matches for lineRange filter, got %d", matched)
		}
	})

	t.Run("Combined filters", func(t *testing.T) {
		targetLine := 42
		comments := []Comment{
			{Path: "foo.go", Line: NewSingleLine(42), Side: "RIGHT"},
			{Path: "foo.go", Line: NewSingleLine(43), Side: "RIGHT"},
			{Path: "bar.go", Line: NewSingleLine(42), Side: "RIGHT"},
			{Path: "foo.go", Line: NewSingleLine(42), Side: "LEFT"},
		}
		filter := CommentFilter{
			File:      "foo.go",
			LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine},
			Side:      "RIGHT",
		}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				if comment.Path != "foo.go" || comment.Line.EndLine != targetLine || comment.Side != "RIGHT" {
					t.Errorf("Comment should match all filter criteria")
				}
			}
		}
		if matched != 1 {
			t.Errorf("Expected 1 match for combined filter, got %d", matched)
		}
	})

	t.Run("Filter specificity increases", func(t *testing.T) {
		// Test that adding more filter criteria reduces matches
		targetLine := 100
		comments := []Comment{
			{Path: "test.go", Line: NewSingleLine(100), Side: "RIGHT"},
			{Path: "test.go", Line: NewSingleLine(100), Side: "LEFT"},
			{Path: "other.go", Line: NewSingleLine(100), Side: "RIGHT"},
		}

		filter1 := CommentFilter{LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine}}
		filter2 := CommentFilter{LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine}, Side: "RIGHT"}
		filter3 := CommentFilter{LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine}, Side: "RIGHT", File: "test.go"}

		count1, count2, count3 := 0, 0, 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter1) {
				count1++
			}
			if comment.MatchesFilter(filter2) {
				count2++
			}
			if comment.MatchesFilter(filter3) {
				count3++
			}
		}

		// More specific filters should match fewer or equal comments
		if count1 < count2 || count2 < count3 {
			t.Errorf("Filter specificity not increasing: count1=%d, count2=%d, count3=%d",
				count1, count2, count3)
		}
		if count3 != 1 {
			t.Errorf("Most specific filter should match exactly 1 comment, got %d", count3)
		}
	})
}
