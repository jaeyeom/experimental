package storage

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestGitHubStorage_BranchOperations(t *testing.T) {
	tmpDir := t.TempDir()
	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	owner := "testowner"
	repo := "testrepo"
	branchName := "feature/test-branch"

	t.Run("CaptureBranchDiffHunks", func(t *testing.T) {
		diffHunks := models.BranchDiffHunks{
			BranchName: branchName,
			Owner:      owner,
			Repo:       repo,
			CapturedAt: time.Now(),
			DiffHunks: []models.DiffHunk{
				{
					File:      "test.go",
					Side:      "RIGHT",
					StartLine: 10,
					EndLine:   20,
					Content:   "@@ -10,11 +10,11 @@\n+added line",
					SHA:       "abc123",
				},
			},
			CommitSHA:  "abc123",
			BaseSHA:    "def456",
			BaseBranch: "main",
		}

		err := storage.CaptureBranchDiffHunks(owner, repo, branchName, diffHunks)
		if err != nil {
			t.Errorf("CaptureBranchDiffHunks() error = %v", err)
		}

		// Verify the file was created
		sanitizedBranch := "feature_test-branch" // slashes replaced with underscores
		expectedPath := filepath.Join(tmpDir, "repos", owner, repo, "branch", sanitizedBranch, "diff-hunks.json")
		if _, err := os.Stat(expectedPath); errors.Is(err, fs.ErrNotExist) {
			t.Errorf("Expected diff hunks file not created at %s", expectedPath)
		}
	})

	t.Run("GetBranchDiffHunks", func(t *testing.T) {
		retrieved, err := storage.GetBranchDiffHunks(owner, repo, branchName)
		if err != nil {
			t.Fatalf("GetBranchDiffHunks() error = %v", err)
		}

		if retrieved.BranchName != branchName {
			t.Errorf("Retrieved branch name = %v, want %v", retrieved.BranchName, branchName)
		}
		if len(retrieved.DiffHunks) != 1 {
			t.Errorf("Retrieved diff hunks count = %v, want 1", len(retrieved.DiffHunks))
		}
	})

	t.Run("AddBranchComment", func(t *testing.T) {
		comment := models.Comment{
			Path: "test.go",
			Line: 15,
			Body: "Test comment on branch",
			Side: "RIGHT",
		}

		err := storage.AddBranchComment(owner, repo, branchName, comment)
		if err != nil {
			t.Errorf("AddBranchComment() error = %v", err)
		}
	})

	t.Run("GetBranchComments", func(t *testing.T) {
		comments, err := storage.GetBranchComments(owner, repo, branchName)
		if err != nil {
			t.Fatalf("GetBranchComments() error = %v", err)
		}

		if len(comments.Comments) != 1 {
			t.Errorf("Retrieved comments count = %v, want 1", len(comments.Comments))
		}
		if comments.Comments[0].Body != "Test comment on branch" {
			t.Errorf("Retrieved comment body = %v, want 'Test comment on branch'", comments.Comments[0].Body)
		}
		if comments.Comments[0].ID == "" {
			t.Error("Comment ID should be generated")
		}
	})

	t.Run("ValidateBranchCommentAgainstDiff", func(t *testing.T) {
		// Valid comment within diff range
		validComment := models.Comment{
			Path: "test.go",
			Line: 15,
			Side: "RIGHT",
		}
		err := storage.ValidateBranchCommentAgainstDiff(owner, repo, branchName, validComment)
		if err != nil {
			t.Errorf("ValidateBranchCommentAgainstDiff() error = %v for valid comment", err)
		}

		// Invalid comment outside diff range
		invalidComment := models.Comment{
			Path: "test.go",
			Line: 100,
			Side: "RIGHT",
		}
		err = storage.ValidateBranchCommentAgainstDiff(owner, repo, branchName, invalidComment)
		if err == nil {
			t.Error("ValidateBranchCommentAgainstDiff() should error for comment outside diff range")
		}
	})

	t.Run("ClearBranchCommentsForFile", func(t *testing.T) {
		// Clear any existing comments first
		_ = storage.ClearBranchComments(owner, repo, branchName)

		// Add multiple comments for different files
		comment1 := models.Comment{
			Path: "file1.go",
			Line: 10,
			Body: "Comment on file1",
			Side: "RIGHT",
		}
		comment2 := models.Comment{
			Path: "file2.go",
			Line: 20,
			Body: "Comment on file2",
			Side: "RIGHT",
		}
		comment3 := models.Comment{
			Path: "file1.go",
			Line: 30,
			Body: "Another comment on file1",
			Side: "RIGHT",
		}

		_ = storage.AddBranchComment(owner, repo, branchName, comment1)
		_ = storage.AddBranchComment(owner, repo, branchName, comment2)
		_ = storage.AddBranchComment(owner, repo, branchName, comment3)

		// Clear comments for file1.go only
		err := storage.ClearBranchCommentsForFile(owner, repo, branchName, "file1.go")
		if err != nil {
			t.Errorf("ClearBranchCommentsForFile() error = %v", err)
		}

		// Verify only file2.go comments remain
		comments, err := storage.GetBranchComments(owner, repo, branchName)
		if err != nil {
			t.Fatalf("GetBranchComments() after file-specific clear error = %v", err)
		}
		if len(comments.Comments) != 1 {
			t.Errorf("Comments count after file-specific clear = %v, want 1", len(comments.Comments))
		}
		if comments.Comments[0].Path != "file2.go" {
			t.Errorf("Remaining comment path = %v, want 'file2.go'", comments.Comments[0].Path)
		}
	})

	t.Run("ClearBranchComments", func(t *testing.T) {
		err := storage.ClearBranchComments(owner, repo, branchName)
		if err != nil {
			t.Errorf("ClearBranchComments() error = %v", err)
		}

		// Verify comments are cleared
		comments, err := storage.GetBranchComments(owner, repo, branchName)
		if err != nil {
			t.Fatalf("GetBranchComments() after clear error = %v", err)
		}
		if len(comments.Comments) != 0 {
			t.Errorf("Comments count after clear = %v, want 0", len(comments.Comments))
		}
	})

	t.Run("SetAndGetBranchMetadata", func(t *testing.T) {
		metadata := map[string]interface{}{
			"lastReviewed": time.Now().Format(time.RFC3339),
			"reviewer":     "testuser",
		}

		err := storage.SetBranchMetadata(owner, repo, branchName, metadata)
		if err != nil {
			t.Errorf("SetBranchMetadata() error = %v", err)
		}

		retrieved, err := storage.GetBranchMetadata(owner, repo, branchName)
		if err != nil {
			t.Fatalf("GetBranchMetadata() error = %v", err)
		}

		if retrieved["reviewer"] != "testuser" {
			t.Errorf("Retrieved metadata reviewer = %v, want 'testuser'", retrieved["reviewer"])
		}
	})
}

func TestGitHubStorage_BranchPathSanitization(t *testing.T) {
	tmpDir := t.TempDir()
	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	tests := []struct {
		branchName     string
		expectedSuffix string
	}{
		{"feature/auth", "feature_auth"},
		{"feature/sub/deep", "feature_sub_deep"},
		{"simple-branch", "simple-branch"},
		{"branch_with_underscore", "branch_with_underscore"},
	}

	for _, tt := range tests {
		t.Run(tt.branchName, func(t *testing.T) {
			path := storage.buildBranchPath("owner", "repo", tt.branchName)
			expectedPath := filepath.Join("repos", "owner", "repo", "branch", tt.expectedSuffix)
			if path != expectedPath {
				t.Errorf("buildBranchPath(%q) = %v, want %v", tt.branchName, path, expectedPath)
			}
		})
	}
}
