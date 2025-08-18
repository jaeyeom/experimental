package prreview

import (
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

// TestFilterCommentsAgainstDiffHunks tests the comment filtering logic.
func TestFilterCommentsAgainstDiffHunks(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "test-filter-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	// Set up test data
	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123

	// Create storage and handler
	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	tests := []struct {
		name                  string
		setupDiffHunks        bool
		diffHunks             []models.DiffHunk
		inputComments         []models.Comment
		expectedValidCount    int
		expectedFilteredCount int
	}{
		{
			name:           "no diff hunks - skip validation",
			setupDiffHunks: false,
			inputComments: []models.Comment{
				{Path: "test.go", Line: 10, Side: "RIGHT"},
				{Path: "test.go", Line: 100, Side: "RIGHT"},
			},
			expectedValidCount:    2,
			expectedFilteredCount: 0,
		},
		{
			name:           "all comments within diff hunks",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
				{File: "test.go", Side: "RIGHT", StartLine: 50, EndLine: 60},
			},
			inputComments: []models.Comment{
				{Path: "test.go", Line: 15, Side: "RIGHT"},
				{Path: "test.go", Line: 55, Side: "RIGHT"},
			},
			expectedValidCount:    2,
			expectedFilteredCount: 0,
		},
		{
			name:           "some comments outside diff hunks",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
			},
			inputComments: []models.Comment{
				{Path: "test.go", Line: 15, Side: "RIGHT"},  // valid
				{Path: "test.go", Line: 25, Side: "RIGHT"},  // invalid
				{Path: "test.go", Line: 100, Side: "RIGHT"}, // invalid
			},
			expectedValidCount:    1,
			expectedFilteredCount: 2,
		},
		{
			name:           "all comments outside diff hunks",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
			},
			inputComments: []models.Comment{
				{Path: "test.go", Line: 5, Side: "RIGHT"},
				{Path: "test.go", Line: 25, Side: "RIGHT"},
			},
			expectedValidCount:    0,
			expectedFilteredCount: 2,
		},
		{
			name:           "comments for different files",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
			},
			inputComments: []models.Comment{
				{Path: "test.go", Line: 15, Side: "RIGHT"},  // valid
				{Path: "other.go", Line: 15, Side: "RIGHT"}, // invalid (different file)
			},
			expectedValidCount:    1,
			expectedFilteredCount: 1,
		},
		{
			name:           "comments for different sides",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
			},
			inputComments: []models.Comment{
				{Path: "test.go", Line: 15, Side: "RIGHT"}, // valid
				{Path: "test.go", Line: 15, Side: "LEFT"},  // invalid (different side)
			},
			expectedValidCount:    1,
			expectedFilteredCount: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Clean up storage for each test
			os.RemoveAll(tmpDir)
			tmpDir, err = os.MkdirTemp("", "test-filter-*")
			if err != nil {
				t.Fatal(err)
			}

			store, err = storage.NewGitHubStorage(tmpDir)
			if err != nil {
				t.Fatal(err)
			}
			handler.storage = store
			handler.storageHome = tmpDir

			// Set up diff hunks if needed
			if tt.setupDiffHunks {
				diffHunks := models.PRDiffHunks{
					PRNumber:   prNumber,
					Repository: repository,
					CapturedAt: time.Now(),
					DiffHunks:  tt.diffHunks,
				}
				if err := store.CaptureDiffHunks(repository, prNumber, diffHunks); err != nil {
					t.Fatalf("failed to capture diff hunks: %v", err)
				}
			}

			// Test the filtering
			validComments, filteredComments := handler.filterCommentsAgainstDiffHunks(repository, prNumber, tt.inputComments)

			// Verify counts
			if len(validComments) != tt.expectedValidCount {
				t.Errorf("expected %d valid comments, got %d", tt.expectedValidCount, len(validComments))
			}
			if len(filteredComments) != tt.expectedFilteredCount {
				t.Errorf("expected %d filtered comments, got %d", tt.expectedFilteredCount, len(filteredComments))
			}

			// Verify total count is preserved
			if len(validComments)+len(filteredComments) != len(tt.inputComments) {
				t.Errorf("total comments count mismatch: input=%d, valid=%d, filtered=%d",
					len(tt.inputComments), len(validComments), len(filteredComments))
			}
		})
	}
}

// TestFilteringIntegration tests the filtering logic isolated from external dependencies.
func TestFilteringIntegration(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "test-filtering-integration-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	// Set up test data
	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123

	// Create storage
	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	// Create handler
	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	// Set up diff hunks
	diffHunks := models.PRDiffHunks{
		PRNumber:   prNumber,
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
		},
	}
	if err := store.CaptureDiffHunks(repository, prNumber, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Test case 1: Mixed valid and invalid comments
	t.Run("mixed valid and invalid comments", func(t *testing.T) {
		comments := []models.Comment{
			{Path: "test.go", Line: 15, Side: "RIGHT", Body: "Valid comment"},      // valid
			{Path: "test.go", Line: 25, Side: "RIGHT", Body: "Invalid comment 1"},  // invalid
			{Path: "test.go", Line: 100, Side: "RIGHT", Body: "Invalid comment 2"}, // invalid
		}

		validComments, filteredComments := handler.filterCommentsAgainstDiffHunks(repository, prNumber, comments)

		if len(validComments) != 1 {
			t.Errorf("expected 1 valid comment, got %d", len(validComments))
		}
		if len(filteredComments) != 2 {
			t.Errorf("expected 2 filtered comments, got %d", len(filteredComments))
		}
		if validComments[0].Body != "Valid comment" {
			t.Errorf("expected valid comment body 'Valid comment', got '%s'", validComments[0].Body)
		}
	})

	// Test case 2: All comments invalid
	t.Run("all comments invalid", func(t *testing.T) {
		comments := []models.Comment{
			{Path: "test.go", Line: 25, Side: "RIGHT", Body: "Invalid comment 1"},
			{Path: "test.go", Line: 100, Side: "RIGHT", Body: "Invalid comment 2"},
		}

		validComments, filteredComments := handler.filterCommentsAgainstDiffHunks(repository, prNumber, comments)

		if len(validComments) != 0 {
			t.Errorf("expected 0 valid comments, got %d", len(validComments))
		}
		if len(filteredComments) != 2 {
			t.Errorf("expected 2 filtered comments, got %d", len(filteredComments))
		}
	})
}

// SimpleFormatter for testing.
type SimpleFormatter struct{}

func (f *SimpleFormatter) FormatComments(comments []models.Comment) (string, error) {
	return fmt.Sprintf("Formatted %d comments", len(comments)), nil
}
