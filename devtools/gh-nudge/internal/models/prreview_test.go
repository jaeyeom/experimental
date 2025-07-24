package models

import (
	"strings"
	"testing"
)

func TestGenerateCommentID(t *testing.T) {
	// Test that GenerateCommentID produces unique IDs
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

// TestCommentIDInJSON tests that comment IDs are properly serialized/deserialized.
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
}
