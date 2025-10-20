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
		Line: 42,
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
		t.Errorf("Line not preserved: expected %d, got %d", original.Line, deserialized.Line)
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
			Line: 42,
			Path: "test.go",
		}
		lineRange := comment.GetLineRange()
		if lineRange.StartLine != 42 || lineRange.EndLine != 42 {
			t.Errorf("GetLineRange() for single line = {%d, %d}, want {42, 42}",
				lineRange.StartLine, lineRange.EndLine)
		}
	})

	t.Run("Multi-line comment", func(t *testing.T) {
		startLine := 10
		comment := Comment{
			StartLine: &startLine,
			Line:      20,
			Path:      "test.go",
		}
		lineRange := comment.GetLineRange()
		if lineRange.StartLine != 10 || lineRange.EndLine != 20 {
			t.Errorf("GetLineRange() for multi-line = {%d, %d}, want {10, 20}",
				lineRange.StartLine, lineRange.EndLine)
		}
	})

	t.Run("Multi-line comment with same start and end", func(t *testing.T) {
		startLine := 15
		comment := Comment{
			StartLine: &startLine,
			Line:      15,
			Path:      "test.go",
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
				comment:   Comment{Line: 5},
				wantStart: 5,
				wantEnd:   5,
			},
			{
				name:      "Multi-line",
				comment:   Comment{StartLine: intPtr(1), Line: 10},
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
			{Path: "foo.go", Line: 10, Side: "RIGHT"},
			{Path: "bar.go", Line: 20, Side: "LEFT"},
			{Path: "baz.go", Line: 30, Side: "RIGHT"},
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
			{Path: "foo.go", Line: 10},
			{Path: "bar.go", Line: 20},
			{Path: "foo.go", Line: 30},
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
			{Path: "test.go", Line: 10, Side: "LEFT"},
			{Path: "test.go", Line: 20, Side: "RIGHT"},
			{Path: "test.go", Line: 30, Side: "LEFT"},
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

	t.Run("Line filter", func(t *testing.T) {
		targetLine := 42
		comments := []Comment{
			{Path: "test.go", Line: 42},
			{Path: "test.go", Line: 43},
			{Path: "test.go", Line: 42},
		}
		filter := CommentFilter{Line: &targetLine}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				if comment.Line != targetLine {
					t.Errorf("Comment with line %d should not match filter for line %d", comment.Line, targetLine)
				}
			}
		}
		if matched != 2 {
			t.Errorf("Expected 2 matches for line filter, got %d", matched)
		}
	})

	t.Run("StartLine filter", func(t *testing.T) {
		filterStartLine := 10
		commentStartLine := 10
		otherStartLine := 15

		comments := []Comment{
			{Path: "test.go", StartLine: &commentStartLine, Line: 20},
			{Path: "test.go", StartLine: &otherStartLine, Line: 25},
			{Path: "test.go", Line: 30}, // No StartLine - this will also match!
		}
		filter := CommentFilter{StartLine: &filterStartLine}

		// The filter only checks StartLine if BOTH filter.StartLine AND c.StartLine are non-nil
		// So comments without StartLine will also match
		matched := 0
		matchedWithStartLine := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				// Only count those that actually have matching StartLine
				if comment.StartLine != nil && *comment.StartLine == filterStartLine {
					matchedWithStartLine++
				}
			}
		}
		// Should match: the comment with StartLine=10 AND the single-line comment (no StartLine)
		if matched != 2 {
			t.Errorf("Expected 2 matches for startLine filter (including single-line comments), got %d", matched)
		}
		if matchedWithStartLine != 1 {
			t.Errorf("Expected 1 multi-line comment with matching startLine, got %d", matchedWithStartLine)
		}
	})

	t.Run("EndLine filter", func(t *testing.T) {
		filterEndLine := 20
		startLine1 := 10
		startLine2 := 15

		comments := []Comment{
			{Path: "test.go", StartLine: &startLine1, Line: 20}, // matches: has StartLine and Line=20
			{Path: "test.go", StartLine: &startLine2, Line: 25}, // doesn't match: Line=25
			{Path: "test.go", Line: 20},                         // matches: no StartLine (filter skipped)
		}
		filter := CommentFilter{EndLine: &filterEndLine}

		// EndLine filter only applies when BOTH filter.EndLine AND c.StartLine are non-nil
		// It checks if c.Line == filter.EndLine
		matched := 0
		matchedWithEndLine := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				// Count multi-line comments with matching end line
				if comment.StartLine != nil && comment.Line == filterEndLine {
					matchedWithEndLine++
				}
			}
		}
		// Should match: multi-line comment with Line=20 AND single-line comment (no StartLine)
		if matched != 2 {
			t.Errorf("Expected 2 matches for endLine filter (including single-line comments), got %d", matched)
		}
		if matchedWithEndLine != 1 {
			t.Errorf("Expected 1 multi-line comment with matching endLine, got %d", matchedWithEndLine)
		}
	})

	t.Run("StartLine and EndLine filters together", func(t *testing.T) {
		filterStartLine := 10
		filterEndLine := 20
		startLine := 10
		otherStartLine := 15

		comments := []Comment{
			{Path: "test.go", StartLine: &startLine, Line: 20},      // matches both
			{Path: "test.go", StartLine: &otherStartLine, Line: 20}, // matches EndLine only
			{Path: "test.go", StartLine: &startLine, Line: 25},      // matches StartLine only
			{Path: "test.go", Line: 20},                             // single-line, matches both (filters skipped)
		}
		filter := CommentFilter{
			StartLine: &filterStartLine,
			EndLine:   &filterEndLine,
		}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
			}
		}
		// Should match: multi-line with StartLine=10 AND Line=20, plus single-line comment
		if matched != 2 {
			t.Errorf("Expected 2 matches for combined startLine+endLine filter, got %d", matched)
		}
	})

	t.Run("Combined filters", func(t *testing.T) {
		targetLine := 42
		comments := []Comment{
			{Path: "foo.go", Line: 42, Side: "RIGHT"},
			{Path: "foo.go", Line: 43, Side: "RIGHT"},
			{Path: "bar.go", Line: 42, Side: "RIGHT"},
			{Path: "foo.go", Line: 42, Side: "LEFT"},
		}
		filter := CommentFilter{
			File: "foo.go",
			Line: &targetLine,
			Side: "RIGHT",
		}

		matched := 0
		for _, comment := range comments {
			if comment.MatchesFilter(filter) {
				matched++
				if comment.Path != "foo.go" || comment.Line != targetLine || comment.Side != "RIGHT" {
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
			{Path: "test.go", Line: 100, Side: "RIGHT"},
			{Path: "test.go", Line: 100, Side: "LEFT"},
			{Path: "other.go", Line: 100, Side: "RIGHT"},
		}

		filter1 := CommentFilter{Line: &targetLine}
		filter2 := CommentFilter{Line: &targetLine, Side: "RIGHT"}
		filter3 := CommentFilter{Line: &targetLine, Side: "RIGHT", File: "test.go"}

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
