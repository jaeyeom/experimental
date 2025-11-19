package storage

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestGetComments(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repo := models.NewRepository("owner", "repo")

	tests := []struct {
		name   string
		target models.ReviewTarget
	}{
		{
			name:   "PR target",
			target: models.NewPRTarget(123),
		},
		{
			name:   "Branch target",
			target: models.NewBranchTarget("feature/test"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// First get should return empty comments
			comments, err := storage.GetComments(repo, tt.target)
			if err != nil {
				t.Fatalf("GetComments() error = %v", err)
			}

			if len(comments.Comments) != 0 {
				t.Errorf("Expected empty comments, got %d", len(comments.Comments))
			}

			if comments.Target != tt.target.String() {
				t.Errorf("Target = %q, want %q", comments.Target, tt.target.String())
			}
		})
	}
}

func TestAddComment(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repo := models.NewRepository("owner", "repo")

	tests := []struct {
		name   string
		target models.ReviewTarget
	}{
		{
			name:   "PR target",
			target: models.NewPRTarget(123),
		},
		{
			name:   "Branch target",
			target: models.NewBranchTarget("main"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment := models.Comment{
				Path: "test.go",
				Line: models.NewSingleLine(10),
				Body: "Test comment",
				Side: models.SideRight,
			}

			// Add comment
			err := storage.AddComment(repo, tt.target, comment)
			if err != nil {
				t.Fatalf("AddComment() error = %v", err)
			}

			// Verify comment was added
			comments, err := storage.GetComments(repo, tt.target)
			if err != nil {
				t.Fatalf("GetComments() error = %v", err)
			}

			if len(comments.Comments) != 1 {
				t.Fatalf("Expected 1 comment, got %d", len(comments.Comments))
			}

			if comments.Comments[0].Body != "Test comment" {
				t.Errorf("Comment body = %q, want %q", comments.Comments[0].Body, "Test comment")
			}

			// Try to add duplicate
			err = storage.AddComment(repo, tt.target, comment)
			if err == nil {
				t.Error("Expected error for duplicate comment, got nil")
			}
		})
	}
}

func TestDeleteCommentByID(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repo := models.NewRepository("owner", "repo")
	target := models.NewPRTarget(123)

	// Add a comment
	comment := models.Comment{
		Path: "test.go",
		Line: models.NewSingleLine(10),
		Body: "Test comment",
		Side: models.SideRight,
	}

	err = storage.AddComment(repo, target, comment)
	if err != nil {
		t.Fatalf("AddComment() error = %v", err)
	}

	// Get the comment to find its ID
	comments, err := storage.GetComments(repo, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}

	if len(comments.Comments) != 1 {
		t.Fatalf("Expected 1 comment, got %d", len(comments.Comments))
	}

	commentID := comments.Comments[0].ID

	// Delete the comment
	err = storage.DeleteCommentByID(repo, target, commentID[:8])
	if err != nil {
		t.Fatalf("DeleteCommentByID() error = %v", err)
	}

	// Verify deletion
	comments, err = storage.GetComments(repo, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}

	if len(comments.Comments) != 0 {
		t.Errorf("Expected 0 comments after deletion, got %d", len(comments.Comments))
	}
}

func TestClearComments(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repo := models.NewRepository("owner", "repo")
	target := models.NewPRTarget(123)

	// Add multiple comments
	for i := 0; i < 3; i++ {
		comment := models.Comment{
			Path: "test.go",
			Line: models.NewSingleLine(10 + i),
			Body: "Test comment",
			Side: models.SideRight,
		}
		err = storage.AddComment(repo, target, comment)
		if err != nil {
			t.Fatalf("AddComment() error = %v", err)
		}
	}

	// Clear all comments
	err = storage.ClearComments(repo, target, nil)
	if err != nil {
		t.Fatalf("ClearComments() error = %v", err)
	}

	// Verify all comments were cleared
	comments, err := storage.GetComments(repo, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}

	if len(comments.Comments) != 0 {
		t.Errorf("Expected 0 comments after clear, got %d", len(comments.Comments))
	}
}

func TestDiffHunksUnified(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repo := models.NewRepository("owner", "repo")
	target := models.NewPRTarget(123)

	// Check that diff hunks don't exist initially
	if storage.DiffHunksExist(repo, target) {
		t.Error("DiffHunksExist() = true, want false")
	}

	// Create diff hunks
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repo,
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation("test.go", models.NewLineRange(1, 10)),
				Side:     models.SideRight,
				Content:  "test content",
				SHA:      "abc123",
			},
		},
		CommitSHA: "abc123",
		BaseSHA:   "def456",
	}

	// Store diff hunks
	err = storage.CaptureDiffHunks(repo, target, diffHunks)
	if err != nil {
		t.Fatalf("CaptureDiffHunks() error = %v", err)
	}

	// Check that diff hunks exist now
	if !storage.DiffHunksExist(repo, target) {
		t.Error("DiffHunksExist() = false, want true")
	}

	// Retrieve diff hunks
	retrieved, err := storage.GetDiffHunks(repo, target)
	if err != nil {
		t.Fatalf("GetDiffHunks() error = %v", err)
	}

	if len(retrieved.DiffHunks) != 1 {
		t.Errorf("Expected 1 diff hunk, got %d", len(retrieved.DiffHunks))
	}

	if retrieved.CommitSHA != "abc123" {
		t.Errorf("CommitSHA = %q, want %q", retrieved.CommitSHA, "abc123")
	}
}

func TestValidateCommentAgainstDiff(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repo := models.NewRepository("owner", "repo")
	target := models.NewPRTarget(123)

	// Create and store diff hunks
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repo,
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation("test.go", models.NewLineRange(1, 10)),
				Side:     models.SideRight,
				Content:  "test content",
				SHA:      "abc123",
			},
		},
		CommitSHA: "abc123",
		BaseSHA:   "def456",
	}

	err = storage.CaptureDiffHunks(repo, target, diffHunks)
	if err != nil {
		t.Fatalf("CaptureDiffHunks() error = %v", err)
	}

	// Test valid comment
	validComment := models.Comment{
		Path: "test.go",
		Line: models.NewSingleLine(5),
		Body: "Valid comment",
		Side: models.SideRight,
	}

	err = storage.ValidateCommentAgainstDiff(repo, target, validComment)
	if err != nil {
		t.Errorf("ValidateCommentAgainstDiff() for valid comment error = %v", err)
	}

	// Test invalid comment (outside diff range)
	invalidComment := models.Comment{
		Path: "test.go",
		Line: models.NewSingleLine(20),
		Body: "Invalid comment",
		Side: models.SideRight,
	}

	err = storage.ValidateCommentAgainstDiff(repo, target, invalidComment)
	if err == nil {
		t.Error("Expected error for invalid comment, got nil")
	}
}

func TestUnifiedStorageWithBothTargetTypes(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "gh-storage-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	repo := models.NewRepository("owner", "repo")
	prTarget := models.NewPRTarget(123)
	branchTarget := models.NewBranchTarget("feature/test")

	// Add comment to PR
	prComment := models.Comment{
		Path: "test.go",
		Line: models.NewSingleLine(10),
		Body: "PR comment",
		Side: models.SideRight,
	}
	err = storage.AddComment(repo, prTarget, prComment)
	if err != nil {
		t.Fatalf("AddComment(PR) error = %v", err)
	}

	// Add comment to branch
	branchComment := models.Comment{
		Path: "test.go",
		Line: models.NewSingleLine(20),
		Body: "Branch comment",
		Side: models.SideRight,
	}
	err = storage.AddComment(repo, branchTarget, branchComment)
	if err != nil {
		t.Fatalf("AddComment(Branch) error = %v", err)
	}

	// Verify PR comments
	prComments, err := storage.GetComments(repo, prTarget)
	if err != nil {
		t.Fatalf("GetComments(PR) error = %v", err)
	}
	if len(prComments.Comments) != 1 {
		t.Errorf("Expected 1 PR comment, got %d", len(prComments.Comments))
	}
	if prComments.Comments[0].Body != "PR comment" {
		t.Errorf("PR comment body = %q, want %q", prComments.Comments[0].Body, "PR comment")
	}

	// Verify branch comments
	branchComments, err := storage.GetComments(repo, branchTarget)
	if err != nil {
		t.Fatalf("GetComments(Branch) error = %v", err)
	}
	if len(branchComments.Comments) != 1 {
		t.Errorf("Expected 1 branch comment, got %d", len(branchComments.Comments))
	}
	if branchComments.Comments[0].Body != "Branch comment" {
		t.Errorf("Branch comment body = %q, want %q", branchComments.Comments[0].Body, "Branch comment")
	}

	// Verify they are stored separately
	storePath := filepath.Join(tmpDir, "repos", "owner", "repo")
	prPath := filepath.Join(storePath, "pull", "123", "comments.json")
	branchPath := filepath.Join(storePath, "branch", "feature_test", "comments.json")

	if _, err := os.Stat(prPath); errors.Is(err, fs.ErrNotExist) {
		t.Error("PR comments file doesn't exist")
	}
	if _, err := os.Stat(branchPath); errors.Is(err, fs.ErrNotExist) {
		t.Error("Branch comments file doesn't exist")
	}
}
