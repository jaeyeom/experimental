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

// CaptureDiffHunks stores the diff hunks for a pull request.
func (gs *GitHubStorage) CaptureDiffHunks(repository models.Repository, prNumber int, diffHunks models.PRDiffHunks) error {
	prPath := gs.buildPRPath(repository, prNumber)
	diffPath := filepath.Join(prPath, "diff-hunks.json")

	if err := gs.locker.WithLock(diffPath, func() error {
		return gs.store.Set(diffPath, diffHunks)
	}); err != nil {
		return fmt.Errorf("failed to store diff hunks with lock: %w", err)
	}
	return nil
}

// GetDiffHunks retrieves the diff hunks for a pull request.
func (gs *GitHubStorage) GetDiffHunks(repository models.Repository, prNumber int) (*models.PRDiffHunks, error) {
	prPath := gs.buildPRPath(repository, prNumber)
	diffPath := filepath.Join(prPath, "diff-hunks.json")

	var diffHunks models.PRDiffHunks
	err := gs.store.Get(diffPath, &diffHunks)
	if err != nil {
		return nil, fmt.Errorf("failed to get diff hunks: %w", err)
	}

	return &diffHunks, nil
}

// AddComment adds a comment to a pull request.
func (gs *GitHubStorage) AddComment(repository models.Repository, prNumber int, comment models.Comment) error {
	prPath := gs.buildPRPath(repository, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLockRetry(commentsPath, gs.lockConfig, func() error {
		var prComments models.PRComments
		if gs.store.Exists(commentsPath) {
			if err := gs.store.Get(commentsPath, &prComments); err != nil {
				return fmt.Errorf("failed to get existing comments: %w", err)
			}
		} else {
			prComments = models.PRComments{
				PRNumber:   prNumber,
				Repository: repository,
				Comments:   []models.Comment{},
			}
		}

		for _, existingComment := range prComments.Comments {
			if comment.IsDuplicate(existingComment) {
				return fmt.Errorf("duplicate comment detected")
			}
		}

		// Generate ID if not provided
		if comment.ID == "" {
			comment.ID = models.GenerateCommentID()
		}
		comment.CreatedAt = time.Now()
		prComments.Comments = append(prComments.Comments, comment)
		prComments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, prComments)
	}); err != nil {
		return fmt.Errorf("failed to add comment with lock: %w", err)
	}
	return nil
}

// GetComments retrieves all comments for a pull request.
func (gs *GitHubStorage) GetComments(repository models.Repository, prNumber int) (*models.PRComments, error) {
	prPath := gs.buildPRPath(repository, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if !gs.store.Exists(commentsPath) {
		return &models.PRComments{
			PRNumber:   prNumber,
			Repository: repository,
			Comments:   []models.Comment{},
		}, nil
	}

	var prComments models.PRComments
	err := gs.store.Get(commentsPath, &prComments)
	if err != nil {
		return nil, fmt.Errorf("failed to get comments: %w", err)
	}

	return &prComments, nil
}

// FindCommentByIDPrefix finds a comment by ID prefix.
func (gs *GitHubStorage) FindCommentByIDPrefix(repository models.Repository, prNumber int, idPrefix string) (*models.Comment, error) {
	prComments, err := gs.GetComments(repository, prNumber)
	if err != nil {
		return nil, err
	}

	var matches []models.Comment
	for _, comment := range prComments.Comments {
		if comment.MatchesIDPrefix(idPrefix) {
			matches = append(matches, comment)
		}
	}

	if len(matches) == 0 {
		return nil, fmt.Errorf("no comment found with ID prefix '%s'", idPrefix)
	}

	if len(matches) > 1 {
		// Build error message with matching comments
		var matchDetails []string
		for _, c := range matches {
			matchDetails = append(matchDetails, fmt.Sprintf("  %s | %s:%v | %.50s...", c.FormatIDShort(), c.Path, c.Line, c.Body))
		}
		return nil, fmt.Errorf("ambiguous comment ID prefix '%s' matches %d comments:\n%s", idPrefix, len(matches), strings.Join(matchDetails, "\n"))
	}

	return &matches[0], nil
}

// DeleteCommentByID deletes a comment by ID prefix.
func (gs *GitHubStorage) DeleteCommentByID(repository models.Repository, prNumber int, idPrefix string) error {
	prPath := gs.buildPRPath(repository, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		prComments, err := gs.GetComments(repository, prNumber)
		if err != nil {
			return err
		}

		matchIndex := -1
		matchCount := 0
		for i, comment := range prComments.Comments {
			if comment.MatchesIDPrefix(idPrefix) {
				matchIndex = i
				matchCount++
			}
		}

		if matchCount == 0 {
			return fmt.Errorf("no comment found with ID prefix '%s'", idPrefix)
		}

		if matchCount > 1 {
			return fmt.Errorf("ambiguous comment ID prefix '%s' matches %d comments", idPrefix, matchCount)
		}

		prComments.Comments = append(prComments.Comments[:matchIndex], prComments.Comments[matchIndex+1:]...)
		prComments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, prComments)
	}); err != nil {
		return fmt.Errorf("failed to delete comment by ID with lock: %w", err)
	}
	return nil
}

// deleteComments is a helper function for deleting comments based on a filter.
func (gs *GitHubStorage) deleteComments(repository models.Repository, prNumber int, shouldDelete func(models.Comment) bool, allowMultiple bool) error {
	prPath := gs.buildPRPath(repository, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		prComments, err := gs.GetComments(repository, prNumber)
		if err != nil {
			return err
		}

		var toDelete []int
		for i, comment := range prComments.Comments {
			if shouldDelete(comment) {
				toDelete = append(toDelete, i)
			}
		}

		if len(toDelete) == 0 {
			return fmt.Errorf("no matching comments found")
		}

		if len(toDelete) > 1 && !allowMultiple {
			return fmt.Errorf("multiple comments found (%d), use --all flag or specify --index", len(toDelete))
		}

		// Remove comments in reverse order to maintain indices
		for i := len(toDelete) - 1; i >= 0; i-- {
			index := toDelete[i]
			prComments.Comments = append(prComments.Comments[:index], prComments.Comments[index+1:]...)
		}

		prComments.UpdatedAt = time.Now()
		return gs.store.Set(commentsPath, prComments)
	}); err != nil {
		return fmt.Errorf("failed to delete comments with lock: %w", err)
	}
	return nil
}

// ClearComments removes all comments for a pull request.
func (gs *GitHubStorage) ClearComments(repository models.Repository, prNumber int) error {
	prPath := gs.buildPRPath(repository, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		return gs.store.Delete(commentsPath)
	}); err != nil {
		return fmt.Errorf("failed to clear comments with lock: %w", err)
	}
	return nil
}

// ClearCommentsForFile removes all comments for a specific file.
func (gs *GitHubStorage) ClearCommentsForFile(repository models.Repository, prNumber int, file string) error {
	return gs.deleteComments(repository, prNumber, func(comment models.Comment) bool {
		return comment.Path == file
	}, true)
}

// ValidateCommentAgainstDiff validates that a comment line exists in the diff hunks.
func (gs *GitHubStorage) ValidateCommentAgainstDiff(repository models.Repository, prNumber int, comment models.Comment) error {
	diffHunks, err := gs.GetDiffHunks(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get diff hunks for validation: %w", err)
	}

	for _, hunk := range diffHunks.DiffHunks {
		if hunk.File == comment.Path && hunk.Side == comment.Side {
			if comment.Line.Overlaps(hunk.Range) {
				if comment.SHA != "" && comment.SHA != hunk.SHA {
					return fmt.Errorf("SHA mismatch: comment SHA %s does not match hunk SHA %s", comment.SHA, hunk.SHA)
				}
				return nil
			}
		}
	}

	return fmt.Errorf("line %v in file %s (side %s) is not within any diff hunk", comment.Line, comment.Path, comment.Side)
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

// Branch-specific storage methods

// CaptureBranchDiffHunks stores the diff hunks for a branch.
func (gs *GitHubStorage) CaptureBranchDiffHunks(repository models.Repository, branchName string, diffHunks models.BranchDiffHunks) error {
	branchPath := gs.buildBranchPath(repository, branchName)
	diffPath := filepath.Join(branchPath, "diff-hunks.json")

	if err := gs.locker.WithLock(diffPath, func() error {
		return gs.store.Set(diffPath, diffHunks)
	}); err != nil {
		return fmt.Errorf("failed to store branch diff hunks with lock: %w", err)
	}
	return nil
}

// GetBranchDiffHunks retrieves the diff hunks for a branch.
func (gs *GitHubStorage) GetBranchDiffHunks(repository models.Repository, branchName string) (*models.BranchDiffHunks, error) {
	branchPath := gs.buildBranchPath(repository, branchName)
	diffPath := filepath.Join(branchPath, "diff-hunks.json")

	var diffHunks models.BranchDiffHunks
	err := gs.store.Get(diffPath, &diffHunks)
	if err != nil {
		return nil, fmt.Errorf("failed to get branch diff hunks: %w", err)
	}

	return &diffHunks, nil
}

// AddBranchComment adds a comment to a branch.
func (gs *GitHubStorage) AddBranchComment(repository models.Repository, branchName string, comment models.Comment) error {
	branchPath := gs.buildBranchPath(repository, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if err := gs.locker.WithLockRetry(commentsPath, gs.lockConfig, func() error {
		var branchComments models.BranchComments
		if gs.store.Exists(commentsPath) {
			if err := gs.store.Get(commentsPath, &branchComments); err != nil {
				return fmt.Errorf("failed to get existing branch comments: %w", err)
			}
		} else {
			branchComments = models.BranchComments{
				BranchName: branchName,
				Repository: repository,
				Comments:   []models.Comment{},
			}
		}

		for _, existingComment := range branchComments.Comments {
			if comment.IsDuplicate(existingComment) {
				return fmt.Errorf("duplicate comment detected")
			}
		}

		// Generate ID if not provided
		if comment.ID == "" {
			comment.ID = models.GenerateCommentID()
		}
		comment.CreatedAt = time.Now()
		branchComments.Comments = append(branchComments.Comments, comment)
		branchComments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, branchComments)
	}); err != nil {
		return fmt.Errorf("failed to add branch comment with lock: %w", err)
	}
	return nil
}

// GetBranchComments retrieves all comments for a branch.
func (gs *GitHubStorage) GetBranchComments(repository models.Repository, branchName string) (*models.BranchComments, error) {
	branchPath := gs.buildBranchPath(repository, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if !gs.store.Exists(commentsPath) {
		return &models.BranchComments{
			BranchName: branchName,
			Repository: repository,
			Comments:   []models.Comment{},
		}, nil
	}

	var branchComments models.BranchComments
	err := gs.store.Get(commentsPath, &branchComments)
	if err != nil {
		return nil, fmt.Errorf("failed to get branch comments: %w", err)
	}

	return &branchComments, nil
}

// ClearBranchComments removes all comments for a branch.
func (gs *GitHubStorage) ClearBranchComments(repository models.Repository, branchName string) error {
	branchPath := gs.buildBranchPath(repository, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		return gs.store.Delete(commentsPath)
	}); err != nil {
		return fmt.Errorf("failed to clear branch comments with lock: %w", err)
	}
	return nil
}

// ClearBranchCommentsForFile removes all comments for a specific file in a branch.
func (gs *GitHubStorage) ClearBranchCommentsForFile(repository models.Repository, branchName string, file string) error {
	branchPath := gs.buildBranchPath(repository, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	return gs.locker.WithLock(commentsPath, func() error {
		branchComments, err := gs.GetBranchComments(repository, branchName)
		if err != nil {
			return err
		}

		// Filter out comments for the specified file
		var filteredComments []models.Comment
		for _, comment := range branchComments.Comments {
			if comment.Path != file {
				filteredComments = append(filteredComments, comment)
			}
		}

		branchComments.Comments = filteredComments
		branchComments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, branchComments)
	})
}

// DeleteBranchCommentByID deletes a specific branch comment by ID prefix.
func (gs *GitHubStorage) DeleteBranchCommentByID(repository models.Repository, branchName string, idPrefix string) error {
	branchPath := gs.buildBranchPath(repository, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		branchComments, err := gs.GetBranchComments(repository, branchName)
		if err != nil {
			return err
		}

		matchIndex := -1
		matchCount := 0
		for i, comment := range branchComments.Comments {
			if comment.MatchesIDPrefix(idPrefix) {
				matchIndex = i
				matchCount++
			}
		}

		if matchCount == 0 {
			return fmt.Errorf("no comment found with ID prefix '%s'", idPrefix)
		}

		if matchCount > 1 {
			return fmt.Errorf("ambiguous comment ID prefix '%s' matches %d comments", idPrefix, matchCount)
		}

		branchComments.Comments = append(branchComments.Comments[:matchIndex], branchComments.Comments[matchIndex+1:]...)
		branchComments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, branchComments)
	}); err != nil {
		return fmt.Errorf("failed to delete branch comment by ID with lock: %w", err)
	}
	return nil
}

// ValidateBranchCommentAgainstDiff validates that a comment line exists in the branch diff hunks.
func (gs *GitHubStorage) ValidateBranchCommentAgainstDiff(repository models.Repository, branchName string, comment models.Comment) error {
	diffHunks, err := gs.GetBranchDiffHunks(repository, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch diff hunks for validation: %w", err)
	}

	for _, hunk := range diffHunks.DiffHunks {
		if hunk.File == comment.Path && hunk.Side == comment.Side {
			if comment.Line.Overlaps(hunk.Range) {
				if comment.SHA != "" && comment.SHA != hunk.SHA {
					return fmt.Errorf("SHA mismatch: comment SHA %s does not match hunk SHA %s", comment.SHA, hunk.SHA)
				}
				return nil
			}
		}
	}

	return fmt.Errorf("line %v in file %s (side %s) is not within any diff hunk", comment.Line, comment.Path, comment.Side)
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

// UpdateComments updates the entire comment list for a PR.
func (gs *GitHubStorage) UpdateComments(repository models.Repository, prNumber int, comments *models.PRComments) error {
	prPath := gs.buildPRPath(repository, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		comments.UpdatedAt = time.Now()
		return gs.store.Set(commentsPath, comments)
	}); err != nil {
		return fmt.Errorf("failed to update comments with lock: %w", err)
	}
	return nil
}

// UpdateBranchComments updates the entire comment list for a branch.
func (gs *GitHubStorage) UpdateBranchComments(repository models.Repository, branchName string, comments *models.BranchComments) error {
	branchPath := gs.buildBranchPath(repository, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		comments.UpdatedAt = time.Now()
		return gs.store.Set(commentsPath, comments)
	}); err != nil {
		return fmt.Errorf("failed to update branch comments with lock: %w", err)
	}
	return nil
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
	prPath := gs.buildPRPath(repository, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")
	archivePath := gs.buildArchivePath(repository, prNumber)
	metadataPath := filepath.Join(archivePath, "metadata.json")

	var archivedSubmission *models.ArchivedSubmission

	if err := gs.locker.WithLock(commentsPath, func() error {
		// Get current comments
		prComments, err := gs.GetComments(repository, prNumber)
		if err != nil {
			return fmt.Errorf("failed to get comments for archiving: %w", err)
		}

		if len(prComments.Comments) == 0 {
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
			Comments:     prComments.Comments,
			CommentCount: len(prComments.Comments),
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
