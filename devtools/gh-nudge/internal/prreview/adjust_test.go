package prreview

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

func TestAdjustCommand(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "test-adjust-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	// Set up test data
	owner := "testowner"
	repo := "testrepo"
	prNumber := 123
	file := "src/main.go"

	// Create storage and handler
	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	// Set up diff hunks
	diffHunks := models.PRDiffHunks{
		PRNumber:   prNumber,
		Owner:      owner,
		Repo:       repo,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				File:      file,
				Side:      "RIGHT",
				StartLine: 10,
				EndLine:   50,
			},
		},
	}

	if err := store.CaptureDiffHunks(owner, repo, prNumber, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add test comments
	comments := []models.Comment{
		{
			ID:        "comment1",
			Path:      file,
			Line:      20,
			Body:      "Comment on line 20",
			Side:      "RIGHT",
			CreatedAt: time.Now(),
		},
		{
			ID:        "comment2",
			Path:      file,
			Line:      30,
			StartLine: intPtr(25),
			Body:      "Multi-line comment 25-30",
			Side:      "RIGHT",
			CreatedAt: time.Now(),
		},
		{
			ID:        "comment3",
			Path:      file,
			Line:      45,
			Body:      "Comment on line 45",
			Side:      "RIGHT",
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(owner, repo, prNumber, comment); err != nil {
			t.Fatal(err)
		}
	}

	tests := []struct {
		name         string
		diffSpec     string
		dryRun       bool
		force        bool
		wantErr      bool
		checkResults func(t *testing.T)
	}{
		{
			name:     "dry run simple deletion",
			diffSpec: "15,17d14",
			dryRun:   true,
			wantErr:  false,
			checkResults: func(t *testing.T) {
				// Comments should not be modified in dry run
				stored, err := store.GetComments(owner, repo, prNumber)
				if err != nil {
					t.Fatal(err)
				}
				if stored.Comments[0].Line != 20 {
					t.Errorf("Comment line should not change in dry run, got %d", stored.Comments[0].Line)
				}
			},
		},
		{
			name:     "apply simple deletion",
			diffSpec: "15,17d14",
			dryRun:   false,
			wantErr:  false,
			checkResults: func(t *testing.T) {
				stored, err := store.GetComments(owner, repo, prNumber)
				if err != nil {
					t.Fatal(err)
				}
				// Comment at line 20 should move to 17 (20 - 3)
				if stored.Comments[0].Line != 17 {
					t.Errorf("Comment line = %d, want 17", stored.Comments[0].Line)
				}
				// Multi-line comment 25-30 should move to 22-27
				if stored.Comments[1].Line != 27 || *stored.Comments[1].StartLine != 22 {
					t.Errorf("Multi-line comment = %d-%d, want 22-27",
						*stored.Comments[1].StartLine, stored.Comments[1].Line)
				}
			},
		},
		{
			name:     "multiple adjustments",
			diffSpec: "12d11;18a19,21;40,42c41,42",
			dryRun:   false,
			wantErr:  false,
			checkResults: func(t *testing.T) {
				stored, err := store.GetComments(owner, repo, prNumber)
				if err != nil {
					t.Fatal(err)
				}
				// Check adjustment history
				for _, comment := range stored.Comments {
					if len(comment.AdjustmentHistory) != 3 {
						t.Errorf("Comment %s should have 3 adjustments, got %d",
							comment.ID, len(comment.AdjustmentHistory))
					}
				}
			},
		},
		{
			name:     "comment on deleted line",
			diffSpec: "20d19",
			dryRun:   true,
			wantErr:  false, // Dry run should succeed but show warning
		},
		{
			name:     "comment outside diff hunks after adjustment",
			diffSpec: "10,40d9", // This would move line 45 to 14, outside the hunk
			dryRun:   true,
			force:    false,
			wantErr:  false, // Dry run shows warning
		},
		{
			name:     "invalid diff spec",
			diffSpec: "invalid",
			dryRun:   true,
			wantErr:  true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Reset comments between tests
			if !tt.dryRun {
				// Re-add original comments
				if err := store.ClearComments(owner, repo, prNumber); err != nil {
					t.Fatal(err)
				}
				for _, comment := range comments {
					if err := store.AddComment(owner, repo, prNumber, comment); err != nil {
						t.Fatal(err)
					}
				}
			}

			err := handler.AdjustCommand(owner, repo, fmt.Sprintf("%d", prNumber),
				file, tt.diffSpec, tt.dryRun, tt.force, "table")

			if (err != nil) != tt.wantErr {
				t.Errorf("AdjustCommand() error = %v, wantErr %v", err, tt.wantErr)
			}

			if tt.checkResults != nil {
				tt.checkResults(t)
			}
		})
	}
}

func TestAdjustCommandOutput(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "test-adjust-output-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	owner := "testowner"
	repo := "testrepo"
	prNumber := 123
	file := "src/main.go"

	// Create storage
	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	// Set up test data
	diffHunks := models.PRDiffHunks{
		PRNumber:   prNumber,
		Owner:      owner,
		Repo:       repo,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				File:      file,
				Side:      "RIGHT",
				StartLine: 1,
				EndLine:   100,
			},
		},
	}

	if err := store.CaptureDiffHunks(owner, repo, prNumber, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments
	comments := []models.Comment{
		{
			ID:        "abc123",
			Path:      file,
			Line:      20,
			Body:      "Fix this function",
			Side:      "RIGHT",
			CreatedAt: time.Now(),
		},
		{
			ID:        "def456",
			Path:      file,
			Line:      30,
			Body:      "Add error handling",
			Side:      "RIGHT",
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(owner, repo, prNumber, comment); err != nil {
			t.Fatal(err)
		}
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	// Test table format output
	t.Run("table format", func(t *testing.T) {
		result, err := handler.getAdjustmentPreview(owner, repo, prNumber, file, "15,17d14", "table")
		if err != nil {
			t.Fatal(err)
		}

		// Check that output contains expected elements
		if !contains(result, "abc123") || !contains(result, "def456") {
			t.Error("Table output should contain comment IDs")
		}
		if !contains(result, "20") || !contains(result, "30") {
			t.Error("Table output should contain original line numbers")
		}
		if !contains(result, "17") || !contains(result, "27") {
			t.Error("Table output should contain adjusted line numbers")
		}
	})

	// Test JSON format output
	t.Run("json format", func(t *testing.T) {
		result, err := handler.getAdjustmentPreview(owner, repo, prNumber, file, "15,17d14", "json")
		if err != nil {
			t.Fatal(err)
		}

		// Parse JSON to verify structure
		var preview AdjustmentPreview
		if err := json.Unmarshal([]byte(result), &preview); err != nil {
			t.Fatalf("Failed to parse JSON output: %v", err)
		}

		if len(preview.Adjustments) != 1 {
			t.Errorf("Expected 1 adjustment, got %d", len(preview.Adjustments))
		}

		if len(preview.CommentChanges) != 2 {
			t.Errorf("Expected 2 comment changes, got %d", len(preview.CommentChanges))
		}

		// Check first comment change
		if preview.CommentChanges[0].CommentID != "abc123" {
			t.Errorf("Expected comment ID abc123, got %s", preview.CommentChanges[0].CommentID)
		}
		if preview.CommentChanges[0].NewLine != 17 {
			t.Errorf("Expected new line 17, got %d", preview.CommentChanges[0].NewLine)
		}
	})
}

func TestAdjustBranchComments(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "test-adjust-branch-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	owner := "testowner"
	repo := "testrepo"
	branch := "feature/test"
	file := "src/main.go"

	// Create storage
	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	// Set up branch diff hunks
	branchDiffHunks := models.BranchDiffHunks{
		BranchName: branch,
		Owner:      owner,
		Repo:       repo,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				File:      file,
				Side:      "RIGHT",
				StartLine: 10,
				EndLine:   50,
			},
		},
	}

	if err := store.CaptureBranchDiffHunks(owner, repo, branch, branchDiffHunks); err != nil {
		t.Fatal(err)
	}

	// Add branch comment
	comment := models.Comment{
		ID:        "branch1",
		Path:      file,
		Line:      25,
		Body:      "Branch comment",
		Side:      "RIGHT",
		CreatedAt: time.Now(),
	}

	if err := store.AddBranchComment(owner, repo, branch, comment); err != nil {
		t.Fatal(err)
	}

	// Test branch adjustment
	err = handler.AdjustCommand(owner, repo, branch, file, "20,22d19", false, false, "table")
	if err != nil {
		t.Fatal(err)
	}

	// Verify adjustment
	stored, err := store.GetBranchComments(owner, repo, branch)
	if err != nil {
		t.Fatal(err)
	}

	if len(stored.Comments) != 1 {
		t.Fatalf("Expected 1 comment, got %d", len(stored.Comments))
	}

	// Line 25 should move to 22 (25 - 3 deleted lines)
	if stored.Comments[0].Line != 22 {
		t.Errorf("Comment line = %d, want 22", stored.Comments[0].Line)
	}

	if stored.Comments[0].OriginalLine != 25 {
		t.Errorf("Original line = %d, want 25", stored.Comments[0].OriginalLine)
	}
}

// Helper function.
func intPtr(i int) *int {
	return &i
}

func contains(s, substr string) bool {
	return strings.Contains(s, substr)
}
