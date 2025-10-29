package models

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"
	"time"
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
		Side: SideRight,
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
			{Path: "foo.go", Line: NewSingleLine(10), Side: SideRight},
			{Path: "bar.go", Line: NewSingleLine(20), Side: SideLeft},
			{Path: "baz.go", Line: NewSingleLine(30), Side: SideRight},
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
			{Path: "test.go", Line: NewSingleLine(10), Side: SideLeft},
			{Path: "test.go", Line: NewSingleLine(20), Side: SideRight},
			{Path: "test.go", Line: NewSingleLine(30), Side: SideLeft},
		}
		filter := CommentFilter{Side: SideLeft}

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
			{Path: "foo.go", Line: NewSingleLine(42), Side: SideRight},
			{Path: "foo.go", Line: NewSingleLine(43), Side: SideRight},
			{Path: "bar.go", Line: NewSingleLine(42), Side: SideRight},
			{Path: "foo.go", Line: NewSingleLine(42), Side: SideLeft},
		}
		filter := CommentFilter{
			File:      "foo.go",
			LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine},
			Side:      SideRight,
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
			{Path: "test.go", Line: NewSingleLine(100), Side: SideRight},
			{Path: "test.go", Line: NewSingleLine(100), Side: SideLeft},
			{Path: "other.go", Line: NewSingleLine(100), Side: SideRight},
		}

		filter1 := CommentFilter{LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine}}
		filter2 := CommentFilter{LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine}, Side: SideRight}
		filter3 := CommentFilter{LineRange: &LineRange{StartLine: targetLine, EndLine: targetLine}, Side: SideRight, File: "test.go"}

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

// mockCommentClearer implements CommentClearer for testing.
type mockCommentClearer struct {
	clearCommentsCalled        bool
	clearCommentsError         error
	clearCommentsForFileCalled bool
	clearCommentsForFileError  error
	lastRepository             Repository
	lastPRNumber               int
	lastFile                   string
}

func (m *mockCommentClearer) ClearComments(repository Repository, prNumber int) error {
	m.clearCommentsCalled = true
	m.lastRepository = repository
	m.lastPRNumber = prNumber
	return m.clearCommentsError
}

func (m *mockCommentClearer) ClearCommentsForFile(repository Repository, prNumber int, file string) error {
	m.clearCommentsForFileCalled = true
	m.lastRepository = repository
	m.lastPRNumber = prNumber
	m.lastFile = file
	return m.clearCommentsForFileError
}

// mockCommentArchiver implements CommentArchiver for testing.
type mockCommentArchiver struct {
	mockCommentClearer
	archiveCommentsCalled bool
	archiveCommentsError  error
	archivedSubmission    *ArchivedSubmission
}

func (m *mockCommentArchiver) ArchiveComments(repository Repository, prNumber int, _, _ string) (*ArchivedSubmission, error) {
	m.archiveCommentsCalled = true
	m.lastRepository = repository
	m.lastPRNumber = prNumber
	if m.archiveCommentsError != nil {
		return nil, m.archiveCommentsError
	}
	return m.archivedSubmission, nil
}

func (m *mockCommentArchiver) ListArchivedSubmissions(_ Repository, _ int) (*ArchiveMetadata, error) {
	return nil, nil
}

func (m *mockCommentArchiver) GetArchivedSubmission(_ Repository, _ int, _ string) (*ArchivedSubmission, error) {
	return nil, nil
}

func (m *mockCommentArchiver) CleanupOldArchives(_ Repository, _ int, _ time.Duration) error {
	return nil
}

// TestCommentResolve tests the Comment.Resolve method behavior.
func TestCommentResolve(t *testing.T) {
	t.Run("Resolves comment and sets timestamp", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Path:   "test.go",
			Line:   NewSingleLine(42),
			Body:   "Test comment",
			Status: StatusUnresolved,
		}

		beforeResolve := time.Now()
		comment.Resolve("Fixed the issue")
		afterResolve := time.Now()

		if comment.Status != StatusResolved {
			t.Errorf("Status = %s, want %s", comment.Status, StatusResolved)
		}
		if comment.ResolvedAt == nil {
			t.Fatal("ResolvedAt should not be nil")
		}
		if comment.ResolvedAt.Before(beforeResolve) || comment.ResolvedAt.After(afterResolve) {
			t.Errorf("ResolvedAt timestamp out of expected range")
		}
		if comment.ResolutionReason != "Fixed the issue" {
			t.Errorf("ResolutionReason = %q, want %q", comment.ResolutionReason, "Fixed the issue")
		}
	})

	t.Run("Resolves without reason", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Status: StatusUnresolved,
		}

		comment.Resolve("")

		if comment.Status != StatusResolved {
			t.Errorf("Status = %s, want %s", comment.Status, StatusResolved)
		}
		if comment.ResolvedAt == nil {
			t.Fatal("ResolvedAt should not be nil")
		}
		if comment.ResolutionReason != "" {
			t.Errorf("ResolutionReason = %q, want empty string", comment.ResolutionReason)
		}
	})

	t.Run("Can resolve already resolved comment", func(t *testing.T) {
		oldTime := time.Now().Add(-1 * time.Hour)
		comment := Comment{
			ID:               GenerateCommentID(),
			Status:           StatusResolved,
			ResolvedAt:       &oldTime,
			ResolutionReason: "Old reason",
		}

		comment.Resolve("New reason")

		if comment.Status != StatusResolved {
			t.Errorf("Status = %s, want %s", comment.Status, StatusResolved)
		}
		if comment.ResolutionReason != "New reason" {
			t.Errorf("ResolutionReason = %q, want %q", comment.ResolutionReason, "New reason")
		}
		if comment.ResolvedAt.Before(oldTime) {
			t.Error("ResolvedAt should be updated to newer timestamp")
		}
	})

	t.Run("Preserves other comment fields", func(t *testing.T) {
		comment := Comment{
			ID:       GenerateCommentID(),
			Path:     "important.go",
			Line:     NewLineRange(10, 20),
			Body:     "Important comment",
			Side:     SideRight,
			Status:   StatusUnresolved,
			Priority: PriorityHigh,
		}

		comment.Resolve("Done")

		if comment.Path != "important.go" {
			t.Errorf("Path changed unexpectedly")
		}
		if comment.Line.StartLine != 10 || comment.Line.EndLine != 20 {
			t.Errorf("Line range changed unexpectedly")
		}
		if comment.Body != "Important comment" {
			t.Errorf("Body changed unexpectedly")
		}
		if comment.Priority != PriorityHigh {
			t.Errorf("Priority changed unexpectedly")
		}
	})
}

// TestCommentArchive tests the Comment.Archive method behavior.
func TestCommentArchive(t *testing.T) {
	t.Run("Archives comment and sets timestamp", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Path:   "test.go",
			Line:   NewSingleLine(42),
			Body:   "Test comment",
			Status: StatusUnresolved,
		}

		beforeArchive := time.Now()
		comment.Archive("No longer relevant")
		afterArchive := time.Now()

		if comment.Status != StatusArchived {
			t.Errorf("Status = %s, want %s", comment.Status, StatusArchived)
		}
		if comment.ResolvedAt == nil {
			t.Fatal("ResolvedAt should not be nil")
		}
		if comment.ResolvedAt.Before(beforeArchive) || comment.ResolvedAt.After(afterArchive) {
			t.Errorf("ResolvedAt timestamp out of expected range")
		}
		if comment.ResolutionReason != "No longer relevant" {
			t.Errorf("ResolutionReason = %q, want %q", comment.ResolutionReason, "No longer relevant")
		}
	})

	t.Run("Archives without reason", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Status: StatusUnresolved,
		}

		comment.Archive("")

		if comment.Status != StatusArchived {
			t.Errorf("Status = %s, want %s", comment.Status, StatusArchived)
		}
		if comment.ResolvedAt == nil {
			t.Fatal("ResolvedAt should not be nil")
		}
		if comment.ResolutionReason != "" {
			t.Errorf("ResolutionReason = %q, want empty string", comment.ResolutionReason)
		}
	})

	t.Run("Can archive resolved comment", func(t *testing.T) {
		oldTime := time.Now().Add(-1 * time.Hour)
		comment := Comment{
			ID:               GenerateCommentID(),
			Status:           StatusResolved,
			ResolvedAt:       &oldTime,
			ResolutionReason: "Was resolved",
		}

		comment.Archive("Moving to archive")

		if comment.Status != StatusArchived {
			t.Errorf("Status = %s, want %s", comment.Status, StatusArchived)
		}
		if comment.ResolutionReason != "Moving to archive" {
			t.Errorf("ResolutionReason = %q, want %q", comment.ResolutionReason, "Moving to archive")
		}
		if comment.ResolvedAt.Before(oldTime) {
			t.Error("ResolvedAt should be updated to newer timestamp")
		}
	})

	t.Run("Preserves other comment fields", func(t *testing.T) {
		comment := Comment{
			ID:       GenerateCommentID(),
			Path:     "legacy.go",
			Line:     NewLineRange(100, 150),
			Body:     "Legacy comment",
			Side:     SideLeft,
			Status:   StatusUnresolved,
			Priority: PriorityLow,
		}

		comment.Archive("Archiving old comments")

		if comment.Path != "legacy.go" {
			t.Errorf("Path changed unexpectedly")
		}
		if comment.Line.StartLine != 100 || comment.Line.EndLine != 150 {
			t.Errorf("Line range changed unexpectedly")
		}
		if comment.Body != "Legacy comment" {
			t.Errorf("Body changed unexpectedly")
		}
		if comment.Priority != PriorityLow {
			t.Errorf("Priority changed unexpectedly")
		}
	})
}

// TestCommentReopen tests the Comment.Reopen method behavior.
func TestCommentReopen(t *testing.T) {
	t.Run("Reopens resolved comment", func(t *testing.T) {
		resolvedTime := time.Now()
		comment := Comment{
			ID:               GenerateCommentID(),
			Path:             "test.go",
			Line:             NewSingleLine(42),
			Body:             "Test comment",
			Status:           StatusResolved,
			ResolvedAt:       &resolvedTime,
			ResolutionReason: "Was fixed",
		}

		comment.Reopen()

		if comment.Status != StatusUnresolved {
			t.Errorf("Status = %s, want %s", comment.Status, StatusUnresolved)
		}
		if comment.ResolvedAt != nil {
			t.Errorf("ResolvedAt should be nil, got %v", comment.ResolvedAt)
		}
		if comment.ResolutionReason != "" {
			t.Errorf("ResolutionReason = %q, want empty string", comment.ResolutionReason)
		}
	})

	t.Run("Reopens archived comment", func(t *testing.T) {
		archivedTime := time.Now()
		comment := Comment{
			ID:               GenerateCommentID(),
			Status:           StatusArchived,
			ResolvedAt:       &archivedTime,
			ResolutionReason: "Archived",
		}

		comment.Reopen()

		if comment.Status != StatusUnresolved {
			t.Errorf("Status = %s, want %s", comment.Status, StatusUnresolved)
		}
		if comment.ResolvedAt != nil {
			t.Errorf("ResolvedAt should be nil, got %v", comment.ResolvedAt)
		}
		if comment.ResolutionReason != "" {
			t.Errorf("ResolutionReason = %q, want empty string", comment.ResolutionReason)
		}
	})

	t.Run("Reopening unresolved comment is idempotent", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Status: StatusUnresolved,
		}

		comment.Reopen()

		if comment.Status != StatusUnresolved {
			t.Errorf("Status = %s, want %s", comment.Status, StatusUnresolved)
		}
		if comment.ResolvedAt != nil {
			t.Errorf("ResolvedAt should be nil")
		}
		if comment.ResolutionReason != "" {
			t.Errorf("ResolutionReason should be empty")
		}
	})

	t.Run("Preserves other comment fields", func(t *testing.T) {
		resolvedTime := time.Now()
		comment := Comment{
			ID:               GenerateCommentID(),
			Path:             "important.go",
			Line:             NewLineRange(5, 10),
			Body:             "Still important",
			Side:             SideRight,
			Status:           StatusResolved,
			ResolvedAt:       &resolvedTime,
			ResolutionReason: "Fixed",
			Priority:         PriorityHigh,
		}

		comment.Reopen()

		if comment.Path != "important.go" {
			t.Errorf("Path changed unexpectedly")
		}
		if comment.Line.StartLine != 5 || comment.Line.EndLine != 10 {
			t.Errorf("Line range changed unexpectedly")
		}
		if comment.Body != "Still important" {
			t.Errorf("Body changed unexpectedly")
		}
		if comment.Priority != PriorityHigh {
			t.Errorf("Priority changed unexpectedly")
		}
	})
}

// TestCommentActionSequence tests sequences of Resolve, Archive, and Reopen.
func TestCommentActionSequence(t *testing.T) {
	t.Run("Resolve then Reopen", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Status: StatusUnresolved,
		}

		comment.Resolve("Fixed")
		if !comment.IsResolved() {
			t.Error("Comment should be resolved")
		}

		comment.Reopen()
		if !comment.IsUnresolved() {
			t.Error("Comment should be unresolved")
		}
	})

	t.Run("Archive then Reopen", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Status: StatusUnresolved,
		}

		comment.Archive("Old")
		if !comment.IsArchived() {
			t.Error("Comment should be archived")
		}

		comment.Reopen()
		if !comment.IsUnresolved() {
			t.Error("Comment should be unresolved")
		}
	})

	t.Run("Resolve then Archive", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Status: StatusUnresolved,
		}

		comment.Resolve("Fixed")
		firstResolvedAt := *comment.ResolvedAt

		time.Sleep(10 * time.Millisecond) // Ensure timestamp difference

		comment.Archive("Moving to archive")
		if !comment.IsArchived() {
			t.Error("Comment should be archived")
		}
		if !comment.ResolvedAt.After(firstResolvedAt) {
			t.Error("Archive should update timestamp")
		}
	})

	t.Run("Multiple resolve calls update timestamp", func(t *testing.T) {
		comment := Comment{
			ID:     GenerateCommentID(),
			Status: StatusUnresolved,
		}

		comment.Resolve("First fix")
		firstTime := *comment.ResolvedAt

		time.Sleep(10 * time.Millisecond)

		comment.Resolve("Second fix")
		secondTime := *comment.ResolvedAt

		if !secondTime.After(firstTime) {
			t.Error("Second resolve should have later timestamp")
		}
		if comment.ResolutionReason != "Second fix" {
			t.Errorf("ResolutionReason = %q, want %q", comment.ResolutionReason, "Second fix")
		}
	})
}

// TestCreatePostSubmitExecutor tests the CreatePostSubmitExecutor function.
func TestCreatePostSubmitExecutor(t *testing.T) {
	t.Run("Creates ClearAction for 'clear'", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("clear")
		if err != nil {
			t.Errorf("CreatePostSubmitExecutor('clear') returned error: %v", err)
		}
		if executor == nil {
			t.Fatal("CreatePostSubmitExecutor('clear') returned nil executor")
		}
		if _, ok := executor.(ClearAction); !ok {
			t.Errorf("CreatePostSubmitExecutor('clear') returned %T, want ClearAction", executor)
		}
		if executor.Name() != "clear" {
			t.Errorf("executor.Name() = %q, want %q", executor.Name(), "clear")
		}
	})

	t.Run("Creates ClearAction for empty string", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("")
		if err != nil {
			t.Errorf("CreatePostSubmitExecutor('') returned error: %v", err)
		}
		if executor == nil {
			t.Fatal("CreatePostSubmitExecutor('') returned nil executor")
		}
		if _, ok := executor.(ClearAction); !ok {
			t.Errorf("CreatePostSubmitExecutor('') returned %T, want ClearAction", executor)
		}
		if executor.Name() != "clear" {
			t.Errorf("executor.Name() = %q, want %q", executor.Name(), "clear")
		}
	})

	t.Run("Creates KeepAction for 'keep'", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("keep")
		if err != nil {
			t.Errorf("CreatePostSubmitExecutor('keep') returned error: %v", err)
		}
		if executor == nil {
			t.Fatal("CreatePostSubmitExecutor('keep') returned nil executor")
		}
		if _, ok := executor.(KeepAction); !ok {
			t.Errorf("CreatePostSubmitExecutor('keep') returned %T, want KeepAction", executor)
		}
		if executor.Name() != "keep" {
			t.Errorf("executor.Name() = %q, want %q", executor.Name(), "keep")
		}
	})

	t.Run("Creates ArchiveAction for 'archive'", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("archive")
		if err != nil {
			t.Errorf("CreatePostSubmitExecutor('archive') returned error: %v", err)
		}
		if executor == nil {
			t.Fatal("CreatePostSubmitExecutor('archive') returned nil executor")
		}
		if _, ok := executor.(ArchiveAction); !ok {
			t.Errorf("CreatePostSubmitExecutor('archive') returned %T, want ArchiveAction", executor)
		}
		if executor.Name() != "archive" {
			t.Errorf("executor.Name() = %q, want %q", executor.Name(), "archive")
		}
	})

	t.Run("Case insensitive - uppercase", func(t *testing.T) {
		testCases := []struct {
			input        string
			expectedType interface{}
			expectedName string
		}{
			{"CLEAR", ClearAction{}, "clear"},
			{"KEEP", KeepAction{}, "keep"},
			{"ARCHIVE", ArchiveAction{}, "archive"},
		}

		for _, tc := range testCases {
			t.Run(tc.input, func(t *testing.T) {
				executor, err := CreatePostSubmitExecutor(tc.input)
				if err != nil {
					t.Errorf("CreatePostSubmitExecutor(%q) returned error: %v", tc.input, err)
				}
				if executor == nil {
					t.Fatalf("CreatePostSubmitExecutor(%q) returned nil executor", tc.input)
				}
				if executor.Name() != tc.expectedName {
					t.Errorf("executor.Name() = %q, want %q", executor.Name(), tc.expectedName)
				}
			})
		}
	})

	t.Run("Case insensitive - mixed case", func(t *testing.T) {
		testCases := []struct {
			input        string
			expectedName string
		}{
			{"Clear", "clear"},
			{"KeEp", "keep"},
			{"ArChIvE", "archive"},
		}

		for _, tc := range testCases {
			t.Run(tc.input, func(t *testing.T) {
				executor, err := CreatePostSubmitExecutor(tc.input)
				if err != nil {
					t.Errorf("CreatePostSubmitExecutor(%q) returned error: %v", tc.input, err)
				}
				if executor == nil {
					t.Fatalf("CreatePostSubmitExecutor(%q) returned nil executor", tc.input)
				}
				if executor.Name() != tc.expectedName {
					t.Errorf("executor.Name() = %q, want %q", executor.Name(), tc.expectedName)
				}
			})
		}
	})

	t.Run("Returns error for invalid action", func(t *testing.T) {
		invalidInputs := []string{
			"invalid",
			"delete",
			"remove",
			"unknown",
			"clearx",
			"keepx",
			"archivex",
			"clear ",
			" keep",
		}

		for _, input := range invalidInputs {
			t.Run(input, func(t *testing.T) {
				executor, err := CreatePostSubmitExecutor(input)
				if err == nil {
					t.Errorf("CreatePostSubmitExecutor(%q) expected error, got nil", input)
				}
				if executor != nil {
					t.Errorf("CreatePostSubmitExecutor(%q) expected nil executor, got %T", input, executor)
				}
			})
		}
	})

	t.Run("Error message contains invalid input", func(t *testing.T) {
		invalidInput := "badaction"
		_, err := CreatePostSubmitExecutor(invalidInput)
		if err == nil {
			t.Fatal("Expected error for invalid input")
		}
		if !strings.Contains(err.Error(), invalidInput) {
			t.Errorf("Error message should contain %q, got: %v", invalidInput, err)
		}
		if !strings.Contains(err.Error(), "invalid") {
			t.Errorf("Error message should contain 'invalid', got: %v", err)
		}
	})

	t.Run("Error message lists valid options", func(t *testing.T) {
		_, err := CreatePostSubmitExecutor("badaction")
		if err == nil {
			t.Fatal("Expected error for invalid input")
		}
		errorMsg := err.Error()
		validOptions := []string{"clear", "keep", "archive"}
		for _, option := range validOptions {
			if !strings.Contains(errorMsg, option) {
				t.Errorf("Error message should mention valid option %q, got: %v", option, err)
			}
		}
	})
}

// TestKeepAction tests the KeepAction Execute method behavior.
func TestKeepAction(t *testing.T) {
	testRepo := Repository{Owner: "test-owner", Name: "test-repo"}
	testPRNumber := 123

	t.Run("Name returns correct string", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("keep")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('keep') failed: %v", err)
		}
		if got := executor.Name(); got != "keep" {
			t.Errorf("executor.Name() = %q, want %q", got, "keep")
		}
	})

	t.Run("Preserves all comments when file is empty", func(t *testing.T) {
		mock := &mockCommentClearer{}
		executor, err := CreatePostSubmitExecutor("keep")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('keep') failed: %v", err)
		}

		err = executor.Execute(mock, testRepo, testPRNumber, "")
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		// Should not call any storage methods since we're keeping comments
		if mock.clearCommentsCalled {
			t.Error("ClearComments() should not be called when keeping comments")
		}
		if mock.clearCommentsForFileCalled {
			t.Error("ClearCommentsForFile() should not be called when keeping comments")
		}
	})

	t.Run("Preserves file-specific comments when file is specified", func(t *testing.T) {
		mock := &mockCommentClearer{}
		executor, err := CreatePostSubmitExecutor("keep")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('keep') failed: %v", err)
		}
		testFile := "test.go"

		err = executor.Execute(mock, testRepo, testPRNumber, testFile)
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		// Should not call any storage methods since we're keeping comments
		if mock.clearCommentsCalled {
			t.Error("ClearComments() should not be called when keeping comments")
		}
		if mock.clearCommentsForFileCalled {
			t.Error("ClearCommentsForFile() should not be called when keeping comments")
		}
	})

	t.Run("Always returns nil regardless of inputs", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("keep")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('keep') failed: %v", err)
		}
		testCases := []struct {
			name     string
			repo     Repository
			prNumber int
			file     string
		}{
			{"empty file", testRepo, testPRNumber, ""},
			{"with file", testRepo, testPRNumber, "main.go"},
			{"different repo", Repository{Owner: "other", Name: "repo"}, 456, ""},
			{"long path", testRepo, testPRNumber, "internal/models/prreview.go"},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				mock := &mockCommentClearer{}
				err := executor.Execute(mock, tc.repo, tc.prNumber, tc.file)
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
			})
		}
	})

	t.Run("Handles different repository configurations", func(t *testing.T) {
		repos := []Repository{
			{Owner: "owner1", Name: "repo1"},
			{Owner: "owner-with-dash", Name: "repo-with-dash"},
			{Owner: "org", Name: "very-long-repository-name"},
		}

		for _, repo := range repos {
			t.Run(repo.Owner+"/"+repo.Name, func(t *testing.T) {
				mock := &mockCommentClearer{}
				action := KeepAction{}

				err := action.Execute(mock, repo, testPRNumber, "")
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				// Verify no storage operations were called
				if mock.clearCommentsCalled || mock.clearCommentsForFileCalled {
					t.Error("No storage methods should be called when keeping comments")
				}
			})
		}
	})

	t.Run("Handles different PR numbers", func(t *testing.T) {
		prNumbers := []int{1, 42, 999, 10000}

		for _, prNum := range prNumbers {
			t.Run(fmt.Sprintf("PR-%d", prNum), func(t *testing.T) {
				mock := &mockCommentClearer{}
				action := KeepAction{}

				err := action.Execute(mock, testRepo, prNum, "")
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				// Verify no storage operations were called
				if mock.clearCommentsCalled || mock.clearCommentsForFileCalled {
					t.Error("No storage methods should be called when keeping comments")
				}
			})
		}
	})

	t.Run("Handles various file paths", func(t *testing.T) {
		files := []string{
			"main.go",
			"pkg/server/handler.go",
			"internal/models/prreview.go",
			"docs/README.md",
		}

		for _, file := range files {
			t.Run(file, func(t *testing.T) {
				mock := &mockCommentClearer{}
				action := KeepAction{}

				err := action.Execute(mock, testRepo, testPRNumber, file)
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				// Verify no storage operations were called
				if mock.clearCommentsCalled || mock.clearCommentsForFileCalled {
					t.Error("No storage methods should be called when keeping comments")
				}
			})
		}
	})

	t.Run("Storage errors do not affect behavior", func(t *testing.T) {
		// Even if storage has errors configured, KeepAction should still succeed
		// since it doesn't call any storage methods
		mock := &mockCommentClearer{
			clearCommentsError:        fmt.Errorf("hypothetical storage error"),
			clearCommentsForFileError: fmt.Errorf("hypothetical file error"),
		}
		action := KeepAction{}

		err := action.Execute(mock, testRepo, testPRNumber, "test.go")
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		// Verify no storage operations were attempted
		if mock.clearCommentsCalled || mock.clearCommentsForFileCalled {
			t.Error("No storage methods should be called when keeping comments")
		}
	})
}

// TestClearAction tests the ClearAction Execute method behavior.
func TestClearAction(t *testing.T) {
	testRepo := Repository{Owner: "test-owner", Name: "test-repo"}
	testPRNumber := 123

	t.Run("Name returns correct string", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("clear")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('clear') failed: %v", err)
		}
		if got := executor.Name(); got != "clear" {
			t.Errorf("executor.Name() = %q, want %q", got, "clear")
		}
	})

	t.Run("Clears all comments when file is empty", func(t *testing.T) {
		mock := &mockCommentClearer{}
		executor, err := CreatePostSubmitExecutor("clear")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('clear') failed: %v", err)
		}

		err = executor.Execute(mock, testRepo, testPRNumber, "")
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		if !mock.clearCommentsCalled {
			t.Error("ClearComments() was not called")
		}
		if mock.clearCommentsForFileCalled {
			t.Error("ClearCommentsForFile() should not be called when file is empty")
		}
		if mock.lastRepository != testRepo {
			t.Errorf("Repository = %+v, want %+v", mock.lastRepository, testRepo)
		}
		if mock.lastPRNumber != testPRNumber {
			t.Errorf("PR number = %d, want %d", mock.lastPRNumber, testPRNumber)
		}
	})

	t.Run("Clears specific file comments when file is specified", func(t *testing.T) {
		mock := &mockCommentClearer{}
		executor, err := CreatePostSubmitExecutor("clear")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('clear') failed: %v", err)
		}
		testFile := "test.go"

		err = executor.Execute(mock, testRepo, testPRNumber, testFile)
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		if mock.clearCommentsCalled {
			t.Error("ClearComments() should not be called when file is specified")
		}
		if !mock.clearCommentsForFileCalled {
			t.Error("ClearCommentsForFile() was not called")
		}
		if mock.lastFile != testFile {
			t.Errorf("File = %q, want %q", mock.lastFile, testFile)
		}
		if mock.lastRepository != testRepo {
			t.Errorf("Repository = %+v, want %+v", mock.lastRepository, testRepo)
		}
		if mock.lastPRNumber != testPRNumber {
			t.Errorf("PR number = %d, want %d", mock.lastPRNumber, testPRNumber)
		}
	})

	t.Run("Returns nil even when ClearComments fails", func(t *testing.T) {
		mock := &mockCommentClearer{
			clearCommentsError: fmt.Errorf("storage error"),
		}
		executor, err := CreatePostSubmitExecutor("clear")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('clear') failed: %v", err)
		}

		err = executor.Execute(mock, testRepo, testPRNumber, "")
		// Should not fail the submission even if clearing fails
		if err != nil {
			t.Errorf("Execute() should return nil when ClearComments fails, got: %v", err)
		}
		if !mock.clearCommentsCalled {
			t.Error("ClearComments() should still be called despite expected error")
		}
	})

	t.Run("Returns nil even when ClearCommentsForFile fails", func(t *testing.T) {
		mock := &mockCommentClearer{
			clearCommentsForFileError: fmt.Errorf("file storage error"),
		}
		action := ClearAction{}

		err := action.Execute(mock, testRepo, testPRNumber, "test.go")
		// Should not fail the submission even if clearing fails
		if err != nil {
			t.Errorf("Execute() should return nil when ClearCommentsForFile fails, got: %v", err)
		}
		if !mock.clearCommentsForFileCalled {
			t.Error("ClearCommentsForFile() should still be called despite expected error")
		}
	})

	t.Run("Handles different repository configurations", func(t *testing.T) {
		repos := []Repository{
			{Owner: "owner1", Name: "repo1"},
			{Owner: "owner-with-dash", Name: "repo-with-dash"},
			{Owner: "org", Name: "very-long-repository-name"},
		}

		for _, repo := range repos {
			t.Run(repo.Owner+"/"+repo.Name, func(t *testing.T) {
				mock := &mockCommentClearer{}
				action := ClearAction{}

				err := action.Execute(mock, repo, testPRNumber, "")
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				if mock.lastRepository != repo {
					t.Errorf("Repository = %+v, want %+v", mock.lastRepository, repo)
				}
			})
		}
	})

	t.Run("Handles different PR numbers", func(t *testing.T) {
		prNumbers := []int{1, 42, 999, 10000}

		for _, prNum := range prNumbers {
			t.Run(fmt.Sprintf("PR-%d", prNum), func(t *testing.T) {
				mock := &mockCommentClearer{}
				action := ClearAction{}

				err := action.Execute(mock, testRepo, prNum, "")
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				if mock.lastPRNumber != prNum {
					t.Errorf("PR number = %d, want %d", mock.lastPRNumber, prNum)
				}
			})
		}
	})

	t.Run("Handles various file paths", func(t *testing.T) {
		files := []string{
			"main.go",
			"pkg/server/handler.go",
			"internal/models/prreview.go",
			"docs/README.md",
		}

		for _, file := range files {
			t.Run(file, func(t *testing.T) {
				mock := &mockCommentClearer{}
				action := ClearAction{}

				err := action.Execute(mock, testRepo, testPRNumber, file)
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				if !mock.clearCommentsForFileCalled {
					t.Error("ClearCommentsForFile() was not called")
				}
				if mock.lastFile != file {
					t.Errorf("File = %q, want %q", mock.lastFile, file)
				}
			})
		}
	})
}

// TestArchiveAction tests the ArchiveAction Execute method behavior.
func TestArchiveAction(t *testing.T) {
	testRepo := Repository{Owner: "test-owner", Name: "test-repo"}
	testPRNumber := 123

	t.Run("Name returns correct string", func(t *testing.T) {
		executor, err := CreatePostSubmitExecutor("archive")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('archive') failed: %v", err)
		}
		if got := executor.Name(); got != "archive" {
			t.Errorf("executor.Name() = %q, want %q", got, "archive")
		}
	})

	t.Run("Archives comments when storage supports archiving", func(t *testing.T) {
		expectedSubmission := &ArchivedSubmission{
			SubmissionID: "test-id-123",
			CommentCount: 5,
			PRNumber:     testPRNumber,
			Owner:        testRepo.Owner,
			Repo:         testRepo.Name,
		}
		mock := &mockCommentArchiver{
			archivedSubmission: expectedSubmission,
		}
		executor, err := CreatePostSubmitExecutor("archive")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('archive') failed: %v", err)
		}

		err = executor.Execute(mock, testRepo, testPRNumber, "")
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		if !mock.archiveCommentsCalled {
			t.Error("ArchiveComments() was not called")
		}
		if mock.lastRepository != testRepo {
			t.Errorf("Repository = %+v, want %+v", mock.lastRepository, testRepo)
		}
		if mock.lastPRNumber != testPRNumber {
			t.Errorf("PR number = %d, want %d", mock.lastPRNumber, testPRNumber)
		}
	})

	t.Run("Gracefully handles storage without archiving support", func(t *testing.T) {
		// Use mockCommentClearer which doesn't implement CommentArchiver
		mock := &mockCommentClearer{}
		executor, err := CreatePostSubmitExecutor("archive")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('archive') failed: %v", err)
		}

		err = executor.Execute(mock, testRepo, testPRNumber, "")
		// Should succeed without error even when archiving is not supported
		if err != nil {
			t.Errorf("Execute() should return nil when storage doesn't support archiving, got: %v", err)
		}
		// Should not attempt to clear comments as fallback
		if mock.clearCommentsCalled {
			t.Error("ClearComments() should not be called when archiving is not supported")
		}
	})

	t.Run("Returns nil when archiving fails", func(t *testing.T) {
		mock := &mockCommentArchiver{
			archiveCommentsError: fmt.Errorf("archive storage error"),
		}
		executor, err := CreatePostSubmitExecutor("archive")
		if err != nil {
			t.Fatalf("CreatePostSubmitExecutor('archive') failed: %v", err)
		}

		err = executor.Execute(mock, testRepo, testPRNumber, "")
		// Should not fail the submission even if archiving fails
		if err != nil {
			t.Errorf("Execute() should return nil when ArchiveComments fails, got: %v", err)
		}
		if !mock.archiveCommentsCalled {
			t.Error("ArchiveComments() should still be called despite expected error")
		}
	})

	t.Run("Preserves comments when file-specific archiving is requested", func(t *testing.T) {
		mock := &mockCommentArchiver{
			archivedSubmission: &ArchivedSubmission{SubmissionID: "test-id"},
		}
		action := ArchiveAction{}

		err := action.Execute(mock, testRepo, testPRNumber, "test.go")
		// Should succeed but not archive when file is specified
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		if mock.archiveCommentsCalled {
			t.Error("ArchiveComments() should not be called for file-specific archiving")
		}
	})

	t.Run("Handles different file paths for file-specific requests", func(t *testing.T) {
		files := []string{
			"main.go",
			"pkg/server/handler.go",
			"internal/models/prreview.go",
		}

		for _, file := range files {
			t.Run(file, func(t *testing.T) {
				mock := &mockCommentArchiver{
					archivedSubmission: &ArchivedSubmission{SubmissionID: "test-id"},
				}
				action := ArchiveAction{}

				err := action.Execute(mock, testRepo, testPRNumber, file)
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				// File-specific archiving is not supported, so should not call ArchiveComments
				if mock.archiveCommentsCalled {
					t.Error("ArchiveComments() should not be called for file-specific requests")
				}
			})
		}
	})

	t.Run("Handles different repository configurations", func(t *testing.T) {
		repos := []Repository{
			{Owner: "owner1", Name: "repo1"},
			{Owner: "owner-with-dash", Name: "repo-with-dash"},
			{Owner: "org", Name: "very-long-repository-name"},
		}

		for _, repo := range repos {
			t.Run(repo.Owner+"/"+repo.Name, func(t *testing.T) {
				mock := &mockCommentArchiver{
					archivedSubmission: &ArchivedSubmission{SubmissionID: "test-id"},
				}
				action := ArchiveAction{}

				err := action.Execute(mock, repo, testPRNumber, "")
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				if mock.lastRepository != repo {
					t.Errorf("Repository = %+v, want %+v", mock.lastRepository, repo)
				}
			})
		}
	})

	t.Run("Handles different PR numbers", func(t *testing.T) {
		prNumbers := []int{1, 42, 999, 10000}

		for _, prNum := range prNumbers {
			t.Run(fmt.Sprintf("PR-%d", prNum), func(t *testing.T) {
				mock := &mockCommentArchiver{
					archivedSubmission: &ArchivedSubmission{SubmissionID: "test-id"},
				}
				action := ArchiveAction{}

				err := action.Execute(mock, testRepo, prNum, "")
				if err != nil {
					t.Errorf("Execute() returned error: %v", err)
				}
				if mock.lastPRNumber != prNum {
					t.Errorf("PR number = %d, want %d", mock.lastPRNumber, prNum)
				}
			})
		}
	})

	t.Run("Verifies archived submission data", func(t *testing.T) {
		expectedSubmission := &ArchivedSubmission{
			SubmissionID: "submission-abc123",
			CommentCount: 10,
			PRNumber:     testPRNumber,
			Owner:        testRepo.Owner,
			Repo:         testRepo.Name,
			ReviewBody:   "LGTM",
			ReviewEvent:  "COMMENT",
		}
		mock := &mockCommentArchiver{
			archivedSubmission: expectedSubmission,
		}
		action := ArchiveAction{}

		err := action.Execute(mock, testRepo, testPRNumber, "")
		if err != nil {
			t.Errorf("Execute() returned error: %v", err)
		}
		// The action should successfully complete regardless of submission details
		if !mock.archiveCommentsCalled {
			t.Error("ArchiveComments() was not called")
		}
	})
}

// TestFileLocation_Key tests the Key() method behavior for FileLocation.
func TestFileLocation_Key(t *testing.T) {
	t.Run("Single line location creates correct key", func(t *testing.T) {
		loc := NewFileLocationSingleLine("main.go", 42)
		want := "main.go:42"
		if got := loc.Key(); got != want {
			t.Errorf("Key() = %q, want %q", got, want)
		}
	})

	t.Run("Multi-line location creates range key", func(t *testing.T) {
		loc := NewFileLocation("main.go", NewLineRange(10, 20))
		want := "main.go:10-20"
		if got := loc.Key(); got != want {
			t.Errorf("Key() = %q, want %q", got, want)
		}
	})

	t.Run("Paths with special characters are preserved", func(t *testing.T) {
		testCases := []struct {
			name string
			path string
			line int
			want string
		}{
			{"Path with spaces", "my file.go", 5, "my file.go:5"},
			{"Path with colons", "C:\\path\\to\\file.go", 10, "C:\\path\\to\\file.go:10"},
			{"Nested path", "internal/models/prreview.go", 100, "internal/models/prreview.go:100"},
			{"Path with dots", "../parent/file.go", 1, "../parent/file.go:1"},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				loc := NewFileLocationSingleLine(tc.path, tc.line)
				if got := loc.Key(); got != tc.want {
					t.Errorf("Key() = %q, want %q", got, tc.want)
				}
			})
		}
	})

	t.Run("Key matches String output", func(t *testing.T) {
		loc := NewFileLocationSingleLine("test.go", 15)
		if loc.Key() != loc.String() {
			t.Errorf("Key() = %q, String() = %q, should be equal", loc.Key(), loc.String())
		}
	})
}

// TestParseFileLocation tests the ParseFileLocation function behavior.
func TestParseFileLocation(t *testing.T) {
	t.Run("Parse single line location", func(t *testing.T) {
		key := "main.go:42"
		loc, err := ParseFileLocation(key)
		if err != nil {
			t.Fatalf("ParseFileLocation(%q) unexpected error: %v", key, err)
		}

		if loc.Path != "main.go" {
			t.Errorf("Path = %q, want %q", loc.Path, "main.go")
		}
		if loc.Lines.StartLine != 42 || loc.Lines.EndLine != 42 {
			t.Errorf("Lines = %+v, want StartLine=42, EndLine=42", loc.Lines)
		}
	})

	t.Run("Parse multi-line location", func(t *testing.T) {
		key := "main.go:10-20"
		loc, err := ParseFileLocation(key)
		if err != nil {
			t.Fatalf("ParseFileLocation(%q) unexpected error: %v", key, err)
		}

		if loc.Path != "main.go" {
			t.Errorf("Path = %q, want %q", loc.Path, "main.go")
		}
		if loc.Lines.StartLine != 10 || loc.Lines.EndLine != 20 {
			t.Errorf("Lines = %+v, want StartLine=10, EndLine=20", loc.Lines)
		}
	})

	t.Run("Parse paths with special characters", func(t *testing.T) {
		testCases := []struct {
			name      string
			key       string
			wantPath  string
			wantStart int
			wantEnd   int
		}{
			{"Path with spaces", "my file.go:5", "my file.go", 5, 5},
			{"Path with multiple colons", "C:\\path\\to\\file.go:10", "C:\\path\\to\\file.go", 10, 10},
			{"Nested path", "internal/models/prreview.go:100-200", "internal/models/prreview.go", 100, 200},
			{"Parent relative path", "../parent/file.go:1", "../parent/file.go", 1, 1},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				loc, err := ParseFileLocation(tc.key)
				if err != nil {
					t.Fatalf("ParseFileLocation(%q) unexpected error: %v", tc.key, err)
				}

				if loc.Path != tc.wantPath {
					t.Errorf("Path = %q, want %q", loc.Path, tc.wantPath)
				}
				if loc.Lines.StartLine != tc.wantStart {
					t.Errorf("StartLine = %d, want %d", loc.Lines.StartLine, tc.wantStart)
				}
				if loc.Lines.EndLine != tc.wantEnd {
					t.Errorf("EndLine = %d, want %d", loc.Lines.EndLine, tc.wantEnd)
				}
			})
		}
	})

	t.Run("Round-trip: Key and ParseFileLocation are inverses", func(t *testing.T) {
		testCases := []FileLocation{
			NewFileLocationSingleLine("test.go", 1),
			NewFileLocationSingleLine("main.go", 999),
			NewFileLocation("file.go", NewLineRange(5, 10)),
			NewFileLocation("internal/models/prreview.go", NewLineRange(100, 200)),
		}

		for _, original := range testCases {
			t.Run(original.Key(), func(t *testing.T) {
				key := original.Key()
				parsed, err := ParseFileLocation(key)
				if err != nil {
					t.Fatalf("ParseFileLocation(%q) unexpected error: %v", key, err)
				}

				if !parsed.Equals(original) {
					t.Errorf("Round-trip failed: original=%+v, parsed=%+v", original, parsed)
				}
			})
		}
	})

	t.Run("Invalid formats return errors", func(t *testing.T) {
		testCases := []struct {
			name      string
			key       string
			wantError string
		}{
			{"Missing colon", "main.go", "missing colon"},
			{"Missing line number", "main.go:", "invalid line"},
			{"Invalid line number", "main.go:abc", "invalid line"},
			{"Empty string", "", "missing colon"},
			{"Only colon", ":", "invalid line"},
			{"Negative line", "main.go:-5", "invalid line"},
			{"Invalid range", "main.go:10-abc", "invalid end line"},
			{"Reverse range", "main.go:20-10", ""}, // This might be valid depending on implementation
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				_, err := ParseFileLocation(tc.key)
				if err == nil {
					t.Errorf("ParseFileLocation(%q) expected error containing %q, got nil", tc.key, tc.wantError)
				}
			})
		}
	})
}

// TestFileLocation_Equals tests the Equals() method behavior.
func TestFileLocation_Equals(t *testing.T) {
	t.Run("Identical locations are equal", func(t *testing.T) {
		loc1 := NewFileLocationSingleLine("main.go", 42)
		loc2 := NewFileLocationSingleLine("main.go", 42)

		if !loc1.Equals(loc2) {
			t.Errorf("Expected identical locations to be equal: %+v vs %+v", loc1, loc2)
		}
		if !loc2.Equals(loc1) {
			t.Errorf("Equals should be symmetric")
		}
	})

	t.Run("Different paths are not equal", func(t *testing.T) {
		loc1 := NewFileLocationSingleLine("main.go", 42)
		loc2 := NewFileLocationSingleLine("test.go", 42)

		if loc1.Equals(loc2) {
			t.Errorf("Expected different paths to be unequal: %+v vs %+v", loc1, loc2)
		}
	})

	t.Run("Different line numbers are not equal", func(t *testing.T) {
		loc1 := NewFileLocationSingleLine("main.go", 42)
		loc2 := NewFileLocationSingleLine("main.go", 43)

		if loc1.Equals(loc2) {
			t.Errorf("Expected different lines to be unequal: %+v vs %+v", loc1, loc2)
		}
	})

	t.Run("Different ranges are not equal", func(t *testing.T) {
		loc1 := NewFileLocation("main.go", NewLineRange(10, 20))
		loc2 := NewFileLocation("main.go", NewLineRange(10, 21))

		if loc1.Equals(loc2) {
			t.Errorf("Expected different ranges to be unequal: %+v vs %+v", loc1, loc2)
		}
	})

	t.Run("Single line vs range not equal even if end lines match", func(t *testing.T) {
		loc1 := NewFileLocationSingleLine("main.go", 20)
		loc2 := NewFileLocation("main.go", NewLineRange(10, 20))

		if loc1.Equals(loc2) {
			t.Errorf("Expected single line to be unequal to range: %+v vs %+v", loc1, loc2)
		}
	})

	t.Run("Equals is reflexive", func(t *testing.T) {
		// Create a location and verify it equals a copy of itself
		loc1 := NewFileLocationSingleLine("main.go", 42)
		loc2 := loc1 // Make a copy
		if !loc1.Equals(loc2) {
			t.Error("Location should equal a copy of itself")
		}
	})
}

// TestComment_GetLocation tests Comment.GetLocation() behavior.
func TestComment_GetLocation(t *testing.T) {
	t.Run("Single line comment returns correct location", func(t *testing.T) {
		comment := Comment{
			Path: "main.go",
			Line: NewSingleLine(42),
		}

		loc := comment.GetLocation()
		if loc.Path != "main.go" {
			t.Errorf("Path = %q, want %q", loc.Path, "main.go")
		}
		if loc.Lines.StartLine != 42 || loc.Lines.EndLine != 42 {
			t.Errorf("Lines = %+v, want single line 42", loc.Lines)
		}
	})

	t.Run("Multi-line comment returns correct location", func(t *testing.T) {
		comment := Comment{
			Path: "test.go",
			Line: NewLineRange(10, 20),
		}

		loc := comment.GetLocation()
		if loc.Path != "test.go" {
			t.Errorf("Path = %q, want %q", loc.Path, "test.go")
		}
		if loc.Lines.StartLine != 10 || loc.Lines.EndLine != 20 {
			t.Errorf("Lines = %+v, want range 10-20", loc.Lines)
		}
	})
}

// TestComment_GetLocationKey tests Comment.GetLocationKey() behavior.
func TestComment_GetLocationKey(t *testing.T) {
	t.Run("Single line comment key", func(t *testing.T) {
		comment := Comment{
			Path: "main.go",
			Line: NewSingleLine(42),
		}

		want := "main.go:42"
		if got := comment.GetLocationKey(); got != want {
			t.Errorf("GetLocationKey() = %q, want %q", got, want)
		}
	})

	t.Run("Multi-line comment key includes range", func(t *testing.T) {
		comment := Comment{
			Path: "test.go",
			Line: NewLineRange(10, 20),
		}

		want := "test.go:10-20"
		if got := comment.GetLocationKey(); got != want {
			t.Errorf("GetLocationKey() = %q, want %q", got, want)
		}
	})
}

// TestComment_GetEndLineLocationKey tests Comment.GetEndLineLocationKey() behavior.
func TestComment_GetEndLineLocationKey(t *testing.T) {
	t.Run("Single line comment end line key", func(t *testing.T) {
		comment := Comment{
			Path: "main.go",
			Line: NewSingleLine(42),
		}

		want := "main.go:42"
		if got := comment.GetEndLineLocationKey(); got != want {
			t.Errorf("GetEndLineLocationKey() = %q, want %q", got, want)
		}
	})

	t.Run("Multi-line comment uses only end line", func(t *testing.T) {
		comment := Comment{
			Path: "test.go",
			Line: NewLineRange(10, 20),
		}

		// Should use end line (20) only, not the full range
		want := "test.go:20"
		if got := comment.GetEndLineLocationKey(); got != want {
			t.Errorf("GetEndLineLocationKey() = %q, want %q", got, want)
		}
	})

	t.Run("Comments with different ranges but same end line have same key", func(t *testing.T) {
		comment1 := Comment{
			Path: "test.go",
			Line: NewLineRange(5, 10),
		}
		comment2 := Comment{
			Path: "test.go",
			Line: NewLineRange(8, 10),
		}
		comment3 := Comment{
			Path: "test.go",
			Line: NewSingleLine(10),
		}

		key1 := comment1.GetEndLineLocationKey()
		key2 := comment2.GetEndLineLocationKey()
		key3 := comment3.GetEndLineLocationKey()

		if key1 != key2 || key1 != key3 {
			t.Errorf("Expected all keys to be equal: %q, %q, %q", key1, key2, key3)
		}
	})
}

// TestDiffHunk_GetLocation tests DiffHunk.GetLocation() behavior.
func TestDiffHunk_GetLocation(t *testing.T) {
	t.Run("DiffHunk returns correct location", func(t *testing.T) {
		hunk := DiffHunk{
			Location: NewFileLocation("main.go", NewLineRange(10, 20)),
		}

		loc := hunk.Location
		if loc.Path != "main.go" {
			t.Errorf("Path = %q, want %q", loc.Path, "main.go")
		}
		if loc.Lines.StartLine != 10 || loc.Lines.EndLine != 20 {
			t.Errorf("Lines = %+v, want range 10-20", loc.Lines)
		}
	})
}

// TestDiffHunk_GetLocationKey tests DiffHunk location key behavior.
func TestDiffHunk_GetLocationKey(t *testing.T) {
	t.Run("DiffHunk key includes full range", func(t *testing.T) {
		hunk := DiffHunk{
			Location: NewFileLocation("test.go", NewLineRange(5, 15)),
		}

		want := "test.go:5-15"
		if got := hunk.Location.Key(); got != want {
			t.Errorf("Location.Key() = %q, want %q", got, want)
		}
	})
}

// TestMergeConflict_Location tests MergeConflict.Location field behavior.
func TestMergeConflict_Location(t *testing.T) {
	t.Run("Single line conflict returns correct location", func(t *testing.T) {
		conflict := MergeConflict{
			Location: NewFileLocationSingleLine("main.go", 42),
		}

		if conflict.Location.Path != "main.go" {
			t.Errorf("Path = %q, want %q", conflict.Location.Path, "main.go")
		}
		if conflict.Location.Lines.StartLine != 42 || conflict.Location.Lines.EndLine != 42 {
			t.Errorf("Lines = %+v, want single line 42", conflict.Location.Lines)
		}
	})

	t.Run("Multi-line conflict with range", func(t *testing.T) {
		conflict := MergeConflict{
			Location: NewFileLocation("test.go", NewLineRange(10, 20)),
		}

		if conflict.Location.Path != "test.go" {
			t.Errorf("Path = %q, want %q", conflict.Location.Path, "test.go")
		}
		if conflict.Location.Lines.StartLine != 10 || conflict.Location.Lines.EndLine != 20 {
			t.Errorf("Lines = %+v, want range 10-20", conflict.Location.Lines)
		}
	})

	t.Run("Location key for single line conflict", func(t *testing.T) {
		conflict := MergeConflict{
			Location: NewFileLocationSingleLine("main.go", 42),
		}

		want := "main.go:42"
		if got := conflict.Location.Key(); got != want {
			t.Errorf("Location.Key() = %q, want %q", got, want)
		}
	})

	t.Run("Location key for multi-line conflict includes range", func(t *testing.T) {
		conflict := MergeConflict{
			Location: NewFileLocation("test.go", NewLineRange(10, 20)),
		}

		want := "test.go:10-20"
		if got := conflict.Location.Key(); got != want {
			t.Errorf("Location.Key() = %q, want %q", got, want)
		}
	})
}
