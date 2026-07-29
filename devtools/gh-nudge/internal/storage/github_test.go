package storage

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

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

func setupGitHubStorage(t *testing.T) (*GitHubStorage, models.Repository) {
	t.Helper()

	tmpDir, err := os.MkdirTemp("", "gh-storage-archive-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(tmpDir) })

	storage, err := NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}

	return storage, models.NewRepository("owner", "repo")
}

func addTestComment(t *testing.T, storage *GitHubStorage, repo models.Repository, target models.ReviewTarget, path string, line int, body string) {
	t.Helper()

	err := storage.AddComment(repo, target, models.Comment{
		Path: path,
		Line: models.NewSingleLine(line),
		Body: body,
		Side: models.SideRight,
	})
	if err != nil {
		t.Fatalf("AddComment() error = %v", err)
	}
}

func TestArchiveComments_HappyPathPR(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewPRTarget(123)

	addTestComment(t, storage, repo, target, "main.go", 10, "first comment")
	addTestComment(t, storage, repo, target, "main.go", 20, "second comment")

	archived, err := storage.ArchiveComments(repo, target, "LGTM", "APPROVE")
	if err != nil {
		t.Fatalf("ArchiveComments() error = %v", err)
	}

	if archived == nil {
		t.Fatal("ArchiveComments() returned nil submission")
		return
	}
	if archived.SubmissionID == "" {
		t.Error("SubmissionID is empty")
	}
	if archived.PRNumber != 123 {
		t.Errorf("PRNumber = %d, want 123", archived.PRNumber)
	}
	if archived.Owner != "owner" || archived.Repo != "repo" {
		t.Errorf("Owner/Repo = %s/%s, want owner/repo", archived.Owner, archived.Repo)
	}
	if archived.ReviewBody != "LGTM" {
		t.Errorf("ReviewBody = %q, want %q", archived.ReviewBody, "LGTM")
	}
	if archived.ReviewEvent != "APPROVE" {
		t.Errorf("ReviewEvent = %q, want %q", archived.ReviewEvent, "APPROVE")
	}
	if archived.CommentCount != 2 {
		t.Errorf("CommentCount = %d, want 2", archived.CommentCount)
	}
	if len(archived.Comments) != 2 {
		t.Errorf("len(Comments) = %d, want 2", len(archived.Comments))
	}
	if archived.ArchivedAt.IsZero() || archived.SubmittedAt.IsZero() {
		t.Error("ArchivedAt/SubmittedAt should be set")
	}

	// Active comments should be cleared.
	comments, err := storage.GetComments(repo, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}
	if len(comments.Comments) != 0 {
		t.Errorf("Expected 0 active comments after archive, got %d", len(comments.Comments))
	}

	// Archive file should exist on disk via GetArchivedSubmission.
	got, err := storage.GetArchivedSubmission(repo, target, archived.SubmissionID)
	if err != nil {
		t.Fatalf("GetArchivedSubmission() error = %v", err)
	}
	if got.SubmissionID != archived.SubmissionID {
		t.Errorf("GetArchivedSubmission ID = %q, want %q", got.SubmissionID, archived.SubmissionID)
	}
	if got.CommentCount != 2 {
		t.Errorf("GetArchivedSubmission CommentCount = %d, want 2", got.CommentCount)
	}
}

func TestArchiveComments_NoComments(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewPRTarget(456)

	_, err := storage.ArchiveComments(repo, target, "body", "COMMENT")
	if err == nil {
		t.Fatal("ArchiveComments() with no comments expected error, got nil")
	}
	if !strings.Contains(err.Error(), "no comments to archive") {
		t.Errorf("ArchiveComments() error = %q, want it to mention no comments", err.Error())
	}
}

func TestListArchivedSubmissions_Empty(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewPRTarget(789)

	metadata, err := storage.ListArchivedSubmissions(repo, target)
	if err != nil {
		t.Fatalf("ListArchivedSubmissions() error = %v", err)
	}
	if metadata.PRNumber != 789 {
		t.Errorf("PRNumber = %d, want 789", metadata.PRNumber)
	}
	if metadata.Owner != "owner" || metadata.Repo != "repo" {
		t.Errorf("Owner/Repo = %s/%s, want owner/repo", metadata.Owner, metadata.Repo)
	}
	if metadata.TotalArchives != 0 {
		t.Errorf("TotalArchives = %d, want 0", metadata.TotalArchives)
	}
	if len(metadata.Archives) != 0 {
		t.Errorf("len(Archives) = %d, want 0", len(metadata.Archives))
	}
}

func TestListArchivedSubmissions_AfterArchive(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewPRTarget(100)

	addTestComment(t, storage, repo, target, "a.go", 1, "comment one")
	first, err := storage.ArchiveComments(repo, target, "first review", "COMMENT")
	if err != nil {
		t.Fatalf("ArchiveComments(first) error = %v", err)
	}

	addTestComment(t, storage, repo, target, "b.go", 2, "comment two")
	second, err := storage.ArchiveComments(repo, target, "second review", "APPROVE")
	if err != nil {
		t.Fatalf("ArchiveComments(second) error = %v", err)
	}

	if first.SubmissionID == second.SubmissionID {
		t.Error("Expected unique submission IDs for separate archives")
	}

	metadata, err := storage.ListArchivedSubmissions(repo, target)
	if err != nil {
		t.Fatalf("ListArchivedSubmissions() error = %v", err)
	}
	if metadata.TotalArchives != 2 {
		t.Errorf("TotalArchives = %d, want 2", metadata.TotalArchives)
	}
	if len(metadata.Archives) != 2 {
		t.Fatalf("len(Archives) = %d, want 2", len(metadata.Archives))
	}

	ids := map[string]bool{
		metadata.Archives[0].SubmissionID: true,
		metadata.Archives[1].SubmissionID: true,
	}
	if !ids[first.SubmissionID] || !ids[second.SubmissionID] {
		t.Errorf("List archives IDs = %v, want both %q and %q", ids, first.SubmissionID, second.SubmissionID)
	}
}

func TestGetArchivedSubmission(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewPRTarget(200)

	addTestComment(t, storage, repo, target, "file.go", 5, "to archive")
	archived, err := storage.ArchiveComments(repo, target, "review body", "REQUEST_CHANGES")
	if err != nil {
		t.Fatalf("ArchiveComments() error = %v", err)
	}

	t.Run("found", func(t *testing.T) {
		got, err := storage.GetArchivedSubmission(repo, target, archived.SubmissionID)
		if err != nil {
			t.Fatalf("GetArchivedSubmission() error = %v", err)
		}
		if got.ReviewEvent != "REQUEST_CHANGES" {
			t.Errorf("ReviewEvent = %q, want REQUEST_CHANGES", got.ReviewEvent)
		}
		if got.CommentCount != 1 {
			t.Errorf("CommentCount = %d, want 1", got.CommentCount)
		}
		if len(got.Comments) != 1 || got.Comments[0].Body != "to archive" {
			t.Errorf("Comments = %+v, want body %q", got.Comments, "to archive")
		}
	})

	t.Run("not found", func(t *testing.T) {
		_, err := storage.GetArchivedSubmission(repo, target, "does-not-exist")
		if err == nil {
			t.Fatal("GetArchivedSubmission() expected error for unknown ID, got nil")
		}
		if !strings.Contains(err.Error(), "not found") {
			t.Errorf("GetArchivedSubmission() error = %q, want not found", err.Error())
		}
	})
}

func TestCleanupOldArchives(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewPRTarget(300)

	// Seed two archives with controlled ArchivedAt timestamps via the store.
	archivePath := storage.buildArchivePath(repo, target)
	metadataPath := filepath.Join(archivePath, "metadata.json")

	oldTime := time.Now().Add(-48 * time.Hour)
	recentTime := time.Now().Add(-30 * time.Minute)

	oldSubmission := models.ArchivedSubmission{
		SubmissionID: "oldsub01",
		ArchivedAt:   oldTime,
		SubmittedAt:  oldTime,
		PRNumber:     300,
		Owner:        "owner",
		Repo:         "repo",
		ReviewBody:   "old",
		ReviewEvent:  "COMMENT",
		Comments: []models.Comment{{
			Path: "old.go",
			Line: models.NewSingleLine(1),
			Body: "old comment",
			Side: models.SideRight,
		}},
		CommentCount: 1,
		Metadata:     map[string]interface{}{},
	}
	recentSubmission := models.ArchivedSubmission{
		SubmissionID: "newsub02",
		ArchivedAt:   recentTime,
		SubmittedAt:  recentTime,
		PRNumber:     300,
		Owner:        "owner",
		Repo:         "repo",
		ReviewBody:   "new",
		ReviewEvent:  "APPROVE",
		Comments: []models.Comment{{
			Path: "new.go",
			Line: models.NewSingleLine(2),
			Body: "new comment",
			Side: models.SideRight,
		}},
		CommentCount: 1,
		Metadata:     map[string]interface{}{},
	}

	if err := storage.store.Set(filepath.Join(archivePath, "oldsub01.json"), oldSubmission); err != nil {
		t.Fatalf("seed old archive: %v", err)
	}
	if err := storage.store.Set(filepath.Join(archivePath, "newsub02.json"), recentSubmission); err != nil {
		t.Fatalf("seed recent archive: %v", err)
	}
	metadata := models.ArchiveMetadata{
		PRNumber:      300,
		Owner:         "owner",
		Repo:          "repo",
		Archives:      []models.ArchivedSubmission{oldSubmission, recentSubmission},
		TotalArchives: 2,
		LastUpdated:   time.Now(),
	}
	if err := storage.store.Set(metadataPath, metadata); err != nil {
		t.Fatalf("seed archive metadata: %v", err)
	}

	// Keep archives newer than 24h; old (48h) should be removed.
	if err := storage.CleanupOldArchives(repo, target, 24*time.Hour); err != nil {
		t.Fatalf("CleanupOldArchives() error = %v", err)
	}

	listed, err := storage.ListArchivedSubmissions(repo, target)
	if err != nil {
		t.Fatalf("ListArchivedSubmissions() error = %v", err)
	}
	if listed.TotalArchives != 1 {
		t.Errorf("TotalArchives = %d, want 1 after cleanup", listed.TotalArchives)
	}
	if len(listed.Archives) != 1 || listed.Archives[0].SubmissionID != "newsub02" {
		t.Errorf("remaining archives = %+v, want only newsub02", listed.Archives)
	}

	if _, err := storage.GetArchivedSubmission(repo, target, "oldsub01"); err == nil {
		t.Error("expected old archive file to be deleted")
	}
	if _, err := storage.GetArchivedSubmission(repo, target, "newsub02"); err != nil {
		t.Errorf("expected recent archive to remain: %v", err)
	}
}

func TestCleanupOldArchives_NoArchives(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewPRTarget(301)

	if err := storage.CleanupOldArchives(repo, target, time.Hour); err != nil {
		t.Errorf("CleanupOldArchives() with no archives error = %v, want nil", err)
	}
}

func TestArchiveComments_BranchTarget(t *testing.T) {
	storage, repo := setupGitHubStorage(t)
	target := models.NewBranchTarget("feature/archive-test")

	addTestComment(t, storage, repo, target, "branch.go", 7, "branch comment")

	archived, err := storage.ArchiveComments(repo, target, "branch review", "COMMENT")
	if err != nil {
		t.Fatalf("ArchiveComments(branch) error = %v", err)
	}

	// Branch targets use PRNumber 0 as a placeholder.
	if archived.PRNumber != 0 {
		t.Errorf("PRNumber = %d, want 0 for branch target", archived.PRNumber)
	}
	if archived.CommentCount != 1 {
		t.Errorf("CommentCount = %d, want 1", archived.CommentCount)
	}

	comments, err := storage.GetComments(repo, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}
	if len(comments.Comments) != 0 {
		t.Errorf("Expected active comments cleared, got %d", len(comments.Comments))
	}

	metadata, err := storage.ListArchivedSubmissions(repo, target)
	if err != nil {
		t.Fatalf("ListArchivedSubmissions() error = %v", err)
	}
	if metadata.TotalArchives != 1 {
		t.Errorf("TotalArchives = %d, want 1", metadata.TotalArchives)
	}
	if metadata.PRNumber != 0 {
		t.Errorf("metadata.PRNumber = %d, want 0 for branch target", metadata.PRNumber)
	}

	// Path should sanitize slashes in branch name (feature/archive-test -> feature_archive-test).
	got, err := storage.GetArchivedSubmission(repo, target, archived.SubmissionID)
	if err != nil {
		t.Fatalf("GetArchivedSubmission(branch) error = %v", err)
	}
	if got.SubmissionID != archived.SubmissionID {
		t.Errorf("SubmissionID = %q, want %q", got.SubmissionID, archived.SubmissionID)
	}
}
