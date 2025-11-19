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
	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)
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
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(10, 50)),
				Side:     models.SideRight,
			},
		},
	}

	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add test comments
	comments := []models.Comment{
		{
			ID:        "comment1",
			Path:      file,
			Line:      models.NewSingleLine(20),
			Body:      "Comment on line 20",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "comment2",
			Path:      file,
			Line:      models.NewLineRange(25, 30),
			Body:      "Multi-line comment 25-30",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "comment3",
			Path:      file,
			Line:      models.NewSingleLine(45),
			Body:      "Comment on line 45",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
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
				stored, err := store.GetComments(repository, target)
				if err != nil {
					t.Fatal(err)
				}
				if stored.Comments[0].Line != models.NewSingleLine(20) {
					t.Errorf("Comment line should not change in dry run, got %v", stored.Comments[0].Line)
				}
			},
		},
		{
			name:     "apply simple deletion",
			diffSpec: "15,17d14",
			dryRun:   false,
			wantErr:  false,
			checkResults: func(t *testing.T) {
				stored, err := store.GetComments(repository, target)
				if err != nil {
					t.Fatal(err)
				}
				// Comment at line 20 should move to 17 (20 - 3)
				if stored.Comments[0].Line != models.NewSingleLine(17) {
					t.Errorf("Comment line = %v, want 17", stored.Comments[0].Line)
				}
				// Multi-line comment 25-30 should move to 22-27
				if stored.Comments[1].Line != models.NewLineRange(22, 27) {
					t.Errorf("Multi-line comment = %v, want 22-27",
						stored.Comments[1].Line)
				}
			},
		},
		{
			name:     "multiple adjustments",
			diffSpec: "12d11;18a19,21;40,42c41,42",
			dryRun:   false,
			wantErr:  false,
			checkResults: func(t *testing.T) {
				stored, err := store.GetComments(repository, target)
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
				if err := store.ClearComments(repository, target, nil); err != nil {
					t.Fatal(err)
				}
				for _, comment := range comments {
					if err := store.AddComment(repository, target, comment); err != nil {
						t.Fatal(err)
					}
				}
			}

			err := handler.AdjustCommand(repository, fmt.Sprintf("%d", prNumber),
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

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)
	file := "src/main.go"

	// Create storage
	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	// Set up test data
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}

	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments
	comments := []models.Comment{
		{
			ID:        "abc123",
			Path:      file,
			Line:      models.NewSingleLine(20),
			Body:      "Fix this function",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "def456",
			Path:      file,
			Line:      models.NewSingleLine(30),
			Body:      "Add error handling",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	// Test table format output
	t.Run("table format", func(t *testing.T) {
		target := models.NewPRTarget(prNumber)
		result, err := handler.getAdjustmentPreview(repository, target, file, "15,17d14", "table")
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
		target := models.NewPRTarget(prNumber)
		result, err := handler.getAdjustmentPreview(repository, target, file, "15,17d14", "json")
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
		if preview.CommentChanges[0].NewLine.EndLine != 17 {
			t.Errorf("Expected new line 17, got %d", preview.CommentChanges[0].NewLine.EndLine)
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

	repository := models.NewRepository("testowner", "testrepo")
	branch := "feature/test"
	branchTarget := models.NewBranchTarget(branch)
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
	branchDiffHunks := models.ReviewDiffHunks{
		Target:     branchTarget.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(10, 50)),
				Side:     models.SideRight,
			},
		},
	}

	if err := store.CaptureDiffHunks(repository, branchTarget, branchDiffHunks); err != nil {
		t.Fatal(err)
	}

	// Add branch comment
	comment := models.Comment{
		ID:        "branch1",
		Path:      file,
		Line:      models.NewSingleLine(25),
		Body:      "Branch comment",
		Side:      models.SideRight,
		CreatedAt: time.Now(),
	}

	if err := store.AddComment(repository, branchTarget, comment); err != nil {
		t.Fatal(err)
	}

	// Test branch adjustment
	err = handler.AdjustCommand(repository, branch, file, "20,22d19", false, false, "table")
	if err != nil {
		t.Fatal(err)
	}

	// Verify adjustment
	stored, err := store.GetComments(repository, branchTarget)
	if err != nil {
		t.Fatal(err)
	}

	if len(stored.Comments) != 1 {
		t.Fatalf("Expected 1 comment, got %d", len(stored.Comments))
	}

	// Line 25 should move to 22 (25 - 3 deleted lines)
	if stored.Comments[0].Line != models.NewSingleLine(22) {
		t.Errorf("Comment line = %v, want 22", stored.Comments[0].Line)
	}

	if stored.Comments[0].OriginalRange == nil || *stored.Comments[0].OriginalRange != models.NewSingleLine(25) {
		if stored.Comments[0].OriginalRange == nil {
			t.Error("Original range should be set")
		} else {
			t.Errorf("Original line = %v, want 25", *stored.Comments[0].OriginalRange)
		}
	}
}

func contains(s, substr string) bool {
	return strings.Contains(s, substr)
}
