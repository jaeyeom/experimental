package storage

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// GitHubStorage provides GitHub-specific storage operations.
type GitHubStorage struct {
	store      *FileSystemStore
	locker     *FileLockManager
	lister     *FileSystemLister
	lockConfig FileLockConfig
}

// NewGitHubStorage creates a new GitHub storage instance.
func NewGitHubStorage(rootPath string) (*GitHubStorage, error) {
	store, err := NewFileSystemStore(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to create store: %w", err)
	}

	locker := NewFileLockManager(store)

	lister, err := NewFileSystemLister(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to create lister: %w", err)
	}

	return &GitHubStorage{
		store:      store,
		locker:     locker,
		lister:     lister,
		lockConfig: GetFileLockConfig(),
	}, nil
}

// buildPRPath constructs the storage path for a pull request.
func (gs *GitHubStorage) buildPRPath(repository models.Repository, prNumber int) string {
	return filepath.Join("repos", repository.Owner, repository.Name, "pull", strconv.Itoa(prNumber))
}

// buildBranchPath constructs the storage path for a branch.
func (gs *GitHubStorage) buildBranchPath(repository models.Repository, branchName string) string {
	// Sanitize branch name for filesystem storage
	sanitizedBranch := strings.ReplaceAll(branchName, "/", "_")
	return filepath.Join("repos", repository.Owner, repository.Name, "branch", sanitizedBranch)
}

// SetPRMetadata stores metadata for a pull request.
func (gs *GitHubStorage) SetPRMetadata(repository models.Repository, prNumber int, metadata map[string]interface{}) error {
	prPath := gs.buildPRPath(repository, prNumber)
	metadataPath := filepath.Join(prPath, "metadata.json")

	if err := gs.locker.WithLock(metadataPath, func() error {
		return gs.store.Set(metadataPath, metadata)
	}); err != nil {
		return fmt.Errorf("failed to set PR metadata with lock: %w", err)
	}
	return nil
}

// GetPRMetadata retrieves metadata for a pull request.
func (gs *GitHubStorage) GetPRMetadata(repository models.Repository, prNumber int) (map[string]interface{}, error) {
	prPath := gs.buildPRPath(repository, prNumber)
	metadataPath := filepath.Join(prPath, "metadata.json")

	if !gs.store.Exists(metadataPath) {
		return make(map[string]interface{}), nil
	}

	var metadata map[string]interface{}
	err := gs.store.Get(metadataPath, &metadata)
	if err != nil {
		return nil, fmt.Errorf("failed to get metadata: %w", err)
	}

	return metadata, nil
}

// RecordNotification records that a notification was sent for a PR reviewer.
func (gs *GitHubStorage) RecordNotification(repository models.Repository, prNumber int, reviewerLogin string, timestamp time.Time) error {
	prPath := gs.buildPRPath(repository, prNumber)
	notificationsPath := filepath.Join(prPath, "notifications.json")

	if err := gs.locker.WithLock(notificationsPath, func() error {
		var notifications map[string]time.Time
		if gs.store.Exists(notificationsPath) {
			if err := gs.store.Get(notificationsPath, &notifications); err != nil {
				return fmt.Errorf("failed to get existing notifications: %w", err)
			}
		} else {
			notifications = make(map[string]time.Time)
		}

		notifications[reviewerLogin] = timestamp
		return gs.store.Set(notificationsPath, notifications)
	}); err != nil {
		return fmt.Errorf("failed to record notification with lock: %w", err)
	}
	return nil
}

// GetLastNotification retrieves the last notification time for a PR reviewer.
func (gs *GitHubStorage) GetLastNotification(repository models.Repository, prNumber int, reviewerLogin string) (*time.Time, error) {
	prPath := gs.buildPRPath(repository, prNumber)
	notificationsPath := filepath.Join(prPath, "notifications.json")

	if !gs.store.Exists(notificationsPath) {
		return nil, nil
	}

	var notifications map[string]time.Time
	err := gs.store.Get(notificationsPath, &notifications)
	if err != nil {
		return nil, fmt.Errorf("failed to get notifications: %w", err)
	}

	if timestamp, exists := notifications[reviewerLogin]; exists {
		return &timestamp, nil
	}

	return nil, nil
}

// CleanupOldNotifications removes notification records older than the specified duration.
func (gs *GitHubStorage) CleanupOldNotifications(olderThan time.Duration) error {
	_ = time.Now().Add(-olderThan) // cutoff time, not used in current implementation

	// This would need to iterate through all PR notification files
	// For now, we'll return not implemented
	return fmt.Errorf("cleanup old notifications not implemented")
}

// ListPRs lists all pull requests in storage for a repository.
func (gs *GitHubStorage) ListPRs(repository models.Repository) ([]int, error) {
	repoPath := filepath.Join("repos", repository.Owner, repository.Name, "pull")

	if !gs.store.Exists(repoPath) {
		return []int{}, nil
	}

	children, err := gs.lister.GetChildren(repoPath)
	if err != nil {
		return nil, fmt.Errorf("failed to list PRs: %w", err)
	}

	var prNumbers []int
	for _, child := range children {
		if prNumber, err := strconv.Atoi(child); err == nil {
			prNumbers = append(prNumbers, prNumber)
		}
	}

	return prNumbers, nil
}

// ParseRepoAndPR parses owner/repo format and extracts components.
func ParseRepoAndPR(repoSpec string) (models.Repository, error) {
	parts := strings.Split(repoSpec, "/")
	if len(parts) != 2 {
		return models.Repository{}, fmt.Errorf("invalid repository format, expected 'owner/repo'")
	}
	return models.NewRepository(parts[0], parts[1]), nil
}

// PR-specific convenience wrappers for post-submit actions.
// These methods are required by the models.CommentClearer interface used by
// post-submit actions (ClearAction, ArchiveAction). They wrap the unified
// ReviewTarget-based methods for PR-specific operations.

// ClearPRComments clears all comments for a PR.
// This is a convenience wrapper for post-submit actions that require the
// models.CommentClearer interface.
//
// TODO: Consider removing this method and just use ClearComments directly.
func (gs *GitHubStorage) ClearPRComments(repository models.Repository, prNumber int) error {
	target := models.NewPRTarget(prNumber)
	return gs.ClearComments(repository, target)
}

// ClearPRCommentsForFile clears comments for a specific file in a PR.
// This is a convenience wrapper for post-submit actions that require the
// models.CommentClearer interface.
func (gs *GitHubStorage) ClearPRCommentsForFile(repository models.Repository, prNumber int, file string) error {
	target := models.NewPRTarget(prNumber)
	return gs.ClearCommentsForFile(repository, target, file)
}

// SetBranchMetadata stores metadata for a branch.
func (gs *GitHubStorage) SetBranchMetadata(repository models.Repository, branchName string, metadata map[string]interface{}) error {
	branchPath := gs.buildBranchPath(repository, branchName)
	metadataPath := filepath.Join(branchPath, "metadata.json")

	if err := gs.locker.WithLock(metadataPath, func() error {
		return gs.store.Set(metadataPath, metadata)
	}); err != nil {
		return fmt.Errorf("failed to set branch metadata with lock: %w", err)
	}
	return nil
}

// GetBranchMetadata retrieves metadata for a branch.
func (gs *GitHubStorage) GetBranchMetadata(repository models.Repository, branchName string) (map[string]interface{}, error) {
	branchPath := gs.buildBranchPath(repository, branchName)
	metadataPath := filepath.Join(branchPath, "metadata.json")

	if !gs.store.Exists(metadataPath) {
		return make(map[string]interface{}), nil
	}

	var metadata map[string]interface{}
	err := gs.store.Get(metadataPath, &metadata)
	if err != nil {
		return nil, fmt.Errorf("failed to get branch metadata: %w", err)
	}

	return metadata, nil
}

// generateSubmissionID generates a unique submission ID.
func (gs *GitHubStorage) generateSubmissionID() string {
	bytes := make([]byte, 8)
	if _, err := rand.Read(bytes); err != nil {
		// Fallback to timestamp-based ID if crypto rand fails
		return fmt.Sprintf("%x", time.Now().UnixNano())
	}
	return hex.EncodeToString(bytes)
}

// buildArchivePath constructs the storage path for archived submissions.
func (gs *GitHubStorage) buildArchivePath(repository models.Repository, prNumber int) string {
	prPath := gs.buildPRPath(repository, prNumber)
	return filepath.Join(prPath, "archives")
}

// ArchiveComments moves comments to archive storage and clears active comments.
func (gs *GitHubStorage) ArchiveComments(repository models.Repository, prNumber int, reviewBody, reviewEvent string) (*models.ArchivedSubmission, error) {
	target := models.NewPRTarget(prNumber)
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")
	archivePath := gs.buildArchivePath(repository, prNumber)
	metadataPath := filepath.Join(archivePath, "metadata.json")

	var archivedSubmission *models.ArchivedSubmission

	if err := gs.locker.WithLock(commentsPath, func() error {
		// Get current comments using unified method
		comments, err := gs.GetComments(repository, target)
		if err != nil {
			return fmt.Errorf("failed to get comments for archiving: %w", err)
		}

		if len(comments.Comments) == 0 {
			return fmt.Errorf("no comments to archive")
		}

		// Create archived submission
		now := time.Now()
		submissionID := gs.generateSubmissionID()

		archivedSubmission = &models.ArchivedSubmission{
			SubmissionID: submissionID,
			ArchivedAt:   now,
			SubmittedAt:  now, // Using now since we don't track actual submission time
			PRNumber:     prNumber,
			Owner:        repository.Owner,
			Repo:         repository.Name,
			ReviewBody:   reviewBody,
			ReviewEvent:  reviewEvent,
			Comments:     comments.Comments,
			CommentCount: len(comments.Comments),
			Metadata:     make(map[string]interface{}),
		}

		// Store the archived submission
		archiveFilePath := filepath.Join(archivePath, fmt.Sprintf("%s.json", submissionID))
		if err := gs.store.Set(archiveFilePath, archivedSubmission); err != nil {
			return fmt.Errorf("failed to store archived submission: %w", err)
		}

		// Update archive metadata
		if err := gs.updateArchiveMetadata(repository, prNumber, *archivedSubmission, metadataPath); err != nil {
			return fmt.Errorf("failed to update archive metadata: %w", err)
		}

		// Clear active comments
		return gs.store.Delete(commentsPath)
	}); err != nil {
		return nil, fmt.Errorf("failed to archive comments with lock: %w", err)
	}

	return archivedSubmission, nil
}

// updateArchiveMetadata updates the archive metadata index.
func (gs *GitHubStorage) updateArchiveMetadata(repository models.Repository, prNumber int, submission models.ArchivedSubmission, metadataPath string) error {
	var metadata models.ArchiveMetadata

	if gs.store.Exists(metadataPath) {
		if err := gs.store.Get(metadataPath, &metadata); err != nil {
			return fmt.Errorf("failed to get existing archive metadata: %w", err)
		}
	} else {
		metadata = models.ArchiveMetadata{
			PRNumber:      prNumber,
			Owner:         repository.Owner,
			Repo:          repository.Name,
			Archives:      []models.ArchivedSubmission{},
			TotalArchives: 0,
		}
	}

	// Add the new submission to the metadata
	metadata.Archives = append(metadata.Archives, submission)
	metadata.TotalArchives = len(metadata.Archives)
	metadata.LastUpdated = time.Now()

	return gs.store.Set(metadataPath, metadata)
}

// ListArchivedSubmissions returns all archived submissions for a PR.
func (gs *GitHubStorage) ListArchivedSubmissions(repository models.Repository, prNumber int) (*models.ArchiveMetadata, error) {
	archivePath := gs.buildArchivePath(repository, prNumber)
	metadataPath := filepath.Join(archivePath, "metadata.json")

	if !gs.store.Exists(metadataPath) {
		return &models.ArchiveMetadata{
			PRNumber:      prNumber,
			Owner:         repository.Owner,
			Repo:          repository.Name,
			Archives:      []models.ArchivedSubmission{},
			TotalArchives: 0,
			LastUpdated:   time.Now(),
		}, nil
	}

	var metadata models.ArchiveMetadata
	err := gs.store.Get(metadataPath, &metadata)
	if err != nil {
		return nil, fmt.Errorf("failed to get archive metadata: %w", err)
	}

	return &metadata, nil
}

// GetArchivedSubmission retrieves a specific archived submission.
func (gs *GitHubStorage) GetArchivedSubmission(repository models.Repository, prNumber int, submissionID string) (*models.ArchivedSubmission, error) {
	archivePath := gs.buildArchivePath(repository, prNumber)
	archiveFilePath := filepath.Join(archivePath, fmt.Sprintf("%s.json", submissionID))

	if !gs.store.Exists(archiveFilePath) {
		return nil, fmt.Errorf("archived submission with ID '%s' not found", submissionID)
	}

	var submission models.ArchivedSubmission
	err := gs.store.Get(archiveFilePath, &submission)
	if err != nil {
		return nil, fmt.Errorf("failed to get archived submission: %w", err)
	}

	return &submission, nil
}

// CleanupOldArchives removes archives older than the specified duration.
func (gs *GitHubStorage) CleanupOldArchives(repository models.Repository, prNumber int, olderThan time.Duration) error {
	archivePath := gs.buildArchivePath(repository, prNumber)
	metadataPath := filepath.Join(archivePath, "metadata.json")

	if !gs.store.Exists(metadataPath) {
		return nil // No archives to clean up
	}

	if err := gs.locker.WithLock(metadataPath, func() error {
		metadata, err := gs.ListArchivedSubmissions(repository, prNumber)
		if err != nil {
			return err
		}

		cutoffTime := time.Now().Add(-olderThan)
		var remainingArchives []models.ArchivedSubmission

		// Filter archives and delete old ones
		for _, submission := range metadata.Archives {
			if submission.ArchivedAt.After(cutoffTime) {
				remainingArchives = append(remainingArchives, submission)
			} else {
				// Delete the archived submission file
				archiveFilePath := filepath.Join(archivePath, fmt.Sprintf("%s.json", submission.SubmissionID))
				if err := gs.store.Delete(archiveFilePath); err != nil {
					// Log error but continue with cleanup
					continue
				}
			}
		}

		// Update metadata with remaining archives
		metadata.Archives = remainingArchives
		metadata.TotalArchives = len(remainingArchives)
		metadata.LastUpdated = time.Now()

		return gs.store.Set(metadataPath, metadata)
	}); err != nil {
		return fmt.Errorf("failed to cleanup old archives with lock: %w", err)
	}

	return nil
}
