package storage

import (
	"os"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestDeleteCommentByID(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create storage instance
	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 42

	// Add test comments with IDs
	comment1 := models.Comment{
		ID:   "a1b2c3d4e5f67890abcdef1234567890abcdef12",
		Path: "test.go",
		Line: 10,
		Body: "First comment",
		Side: "RIGHT",
	}

	comment2 := models.Comment{
		ID:   "a1b2c3d4ffffff90abcdef1234567890abcdef12",
		Path: "test.go",
		Line: 10,
		Body: "Second comment with similar prefix",
		Side: "RIGHT",
	}

	comment3 := models.Comment{
		ID:   "b1234567890abcde1234567890abcdef12345678",
		Path: "test.go",
		Line: 20,
		Body: "Third comment",
		Side: "RIGHT",
	}

	// Add comments
	if err := storage.AddComment(repository, prNumber, comment1); err != nil {
		t.Fatalf("Failed to add comment1: %v", err)
	}
	if err := storage.AddComment(repository, prNumber, comment2); err != nil {
		t.Fatalf("Failed to add comment2: %v", err)
	}
	if err := storage.AddComment(repository, prNumber, comment3); err != nil {
		t.Fatalf("Failed to add comment3: %v", err)
	}

	// Test deletion with unique prefix
	if err := storage.DeleteCommentByID(repository, prNumber, "b123"); err != nil {
		t.Errorf("Failed to delete comment by unique prefix: %v", err)
	}

	// Verify comment3 was deleted
	prComments, err := storage.GetComments(repository, prNumber)
	if err != nil {
		t.Fatalf("Failed to get comments: %v", err)
	}
	if len(prComments.Comments) != 2 {
		t.Errorf("Expected 2 comments after deletion, got %d", len(prComments.Comments))
	}

	// Test deletion with ambiguous prefix (should fail)
	err = storage.DeleteCommentByID(repository, prNumber, "a1b2c3d4")
	if err == nil {
		t.Error("Expected error for ambiguous prefix, got nil")
	}
	if err != nil && !contains(err.Error(), "ambiguous") {
		t.Errorf("Expected ambiguous error, got: %v", err)
	}

	// Test deletion with non-existent prefix
	err = storage.DeleteCommentByID(repository, prNumber, "nonexistent")
	if err == nil {
		t.Error("Expected error for non-existent prefix, got nil")
	}
	if err != nil && !contains(err.Error(), "no comment found") {
		t.Errorf("Expected 'no comment found' error, got: %v", err)
	}

	// Test deletion with long unique prefix
	if err := storage.DeleteCommentByID(repository, prNumber, "a1b2c3d4e5f67890"); err != nil {
		t.Errorf("Failed to delete comment by long unique prefix: %v", err)
	}

	// Verify only one comment remains
	prComments, err = storage.GetComments(repository, prNumber)
	if err != nil {
		t.Fatalf("Failed to get comments: %v", err)
	}
	if len(prComments.Comments) != 1 {
		t.Errorf("Expected 1 comment after second deletion, got %d", len(prComments.Comments))
	}
	if prComments.Comments[0].ID != comment2.ID {
		t.Errorf("Wrong comment remained: expected ID %s, got %s", comment2.ID, prComments.Comments[0].ID)
	}
}

func TestFindCommentByIDPrefix(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create storage instance
	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 42

	// Add test comment
	comment := models.Comment{
		ID:   "a1b2c3d4e5f67890abcdef1234567890abcdef12",
		Path: "test.go",
		Line: 10,
		Body: "Test comment",
		Side: "RIGHT",
	}

	if err := storage.AddComment(repository, prNumber, comment); err != nil {
		t.Fatalf("Failed to add comment: %v", err)
	}

	// Test finding with various prefixes
	tests := []struct {
		name      string
		prefix    string
		shouldErr bool
		errMsg    string
	}{
		{
			name:      "Full ID",
			prefix:    "a1b2c3d4e5f67890abcdef1234567890abcdef12",
			shouldErr: false,
		},
		{
			name:      "Short prefix",
			prefix:    "a1b2",
			shouldErr: false,
		},
		{
			name:      "Non-existent prefix",
			prefix:    "zzzz",
			shouldErr: true,
			errMsg:    "no comment found",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			found, err := storage.FindCommentByIDPrefix(repository, prNumber, tt.prefix)
			if tt.shouldErr {
				if err == nil {
					t.Error("Expected error, got nil")
				} else if tt.errMsg != "" && !contains(err.Error(), tt.errMsg) {
					t.Errorf("Expected error containing %q, got: %v", tt.errMsg, err)
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
				if found == nil {
					t.Error("Expected comment, got nil")
				} else if found.ID != comment.ID {
					t.Errorf("Wrong comment found: expected ID %s, got %s", comment.ID, found.ID)
				}
			}
		})
	}
}

func TestCommentIDGeneration(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create storage instance
	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 42

	// Add comment without ID (should generate one)
	comment := models.Comment{
		Path: "test.go",
		Line: 10,
		Body: "Test comment",
		Side: "RIGHT",
	}

	if err := storage.AddComment(repository, prNumber, comment); err != nil {
		t.Fatalf("Failed to add comment: %v", err)
	}

	// Retrieve and check that ID was generated
	prComments, err := storage.GetComments(repository, prNumber)
	if err != nil {
		t.Fatalf("Failed to get comments: %v", err)
	}

	if len(prComments.Comments) != 1 {
		t.Fatalf("Expected 1 comment, got %d", len(prComments.Comments))
	}

	generatedComment := prComments.Comments[0]
	if generatedComment.ID == "" {
		t.Error("Comment ID was not generated")
	}
	if len(generatedComment.ID) != 40 {
		t.Errorf("Generated ID has wrong length: got %d, want 40", len(generatedComment.ID))
	}
}

func TestDeleteBranchCommentByID(t *testing.T) {
	// Create temporary storage directory
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create storage instance
	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repository := models.NewRepository("testowner", "testrepo")
	branchName := "feature-branch"

	// Add test comments with IDs
	comment1 := models.Comment{
		ID:   "a1b2c3d4e5f67890abcdef1234567890abcdef12",
		Path: "test.go",
		Line: 10,
		Body: "First comment",
		Side: "RIGHT",
	}

	comment2 := models.Comment{
		ID:   "a1b2c3d4ffffff90abcdef1234567890abcdef12",
		Path: "test.go",
		Line: 10,
		Body: "Second comment with similar prefix",
		Side: "RIGHT",
	}

	comment3 := models.Comment{
		ID:   "b1234567890abcde1234567890abcdef12345678",
		Path: "test.go",
		Line: 20,
		Body: "Third comment",
		Side: "RIGHT",
	}

	// Add comments
	if err := storage.AddBranchComment(repository, branchName, comment1); err != nil {
		t.Fatalf("Failed to add comment1: %v", err)
	}
	if err := storage.AddBranchComment(repository, branchName, comment2); err != nil {
		t.Fatalf("Failed to add comment2: %v", err)
	}
	if err := storage.AddBranchComment(repository, branchName, comment3); err != nil {
		t.Fatalf("Failed to add comment3: %v", err)
	}

	// Test deletion with unique prefix
	if err := storage.DeleteBranchCommentByID(repository, branchName, "b123"); err != nil {
		t.Errorf("Failed to delete branch comment by unique prefix: %v", err)
	}

	// Verify comment3 was deleted
	branchComments, err := storage.GetBranchComments(repository, branchName)
	if err != nil {
		t.Fatalf("Failed to get branch comments: %v", err)
	}
	if len(branchComments.Comments) != 2 {
		t.Errorf("Expected 2 comments after deletion, got %d", len(branchComments.Comments))
	}

	// Test deletion with ambiguous prefix (should fail)
	err = storage.DeleteBranchCommentByID(repository, branchName, "a1b2c3d4")
	if err == nil {
		t.Error("Expected error for ambiguous prefix, got nil")
	}
	if err != nil && !contains(err.Error(), "ambiguous") {
		t.Errorf("Expected ambiguous error, got: %v", err)
	}

	// Test deletion with non-existent prefix
	err = storage.DeleteBranchCommentByID(repository, branchName, "nonexistent")
	if err == nil {
		t.Error("Expected error for non-existent prefix, got nil")
	}
	if err != nil && !contains(err.Error(), "no comment found") {
		t.Errorf("Expected 'no comment found' error, got: %v", err)
	}

	// Test deletion with long unique prefix
	if err := storage.DeleteBranchCommentByID(repository, branchName, "a1b2c3d4e5f67890"); err != nil {
		t.Errorf("Failed to delete branch comment by long unique prefix: %v", err)
	}

	// Verify only one comment remains
	branchComments, err = storage.GetBranchComments(repository, branchName)
	if err != nil {
		t.Fatalf("Failed to get branch comments: %v", err)
	}
	if len(branchComments.Comments) != 1 {
		t.Errorf("Expected 1 comment after second deletion, got %d", len(branchComments.Comments))
	}
	if branchComments.Comments[0].ID != comment2.ID {
		t.Errorf("Wrong comment remained: expected ID %s, got %s", comment2.ID, branchComments.Comments[0].ID)
	}
}

// Helper function.
func contains(s, substr string) bool {
	return strings.Contains(s, substr)
}
