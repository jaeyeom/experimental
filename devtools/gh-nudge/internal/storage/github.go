package storage

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// GitHubStorage provides GitHub-specific storage operations.
type GitHubStorage struct {
	store  *FileSystemStore
	locker *FileLockManager
	lister *FileSystemLister
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
		store:  store,
		locker: locker,
		lister: lister,
	}, nil
}

// buildPRPath constructs the storage path for a pull request.
func (gs *GitHubStorage) buildPRPath(owner, repo string, prNumber int) string {
	return filepath.Join("repos", owner, repo, "pull", strconv.Itoa(prNumber))
}

// buildBranchPath constructs the storage path for a branch.
func (gs *GitHubStorage) buildBranchPath(owner, repo, branchName string) string {
	// Sanitize branch name for filesystem storage
	sanitizedBranch := strings.ReplaceAll(branchName, "/", "_")
	return filepath.Join("repos", owner, repo, "branch", sanitizedBranch)
}

// CaptureDiffHunks stores the diff hunks for a pull request.
func (gs *GitHubStorage) CaptureDiffHunks(owner, repo string, prNumber int, diffHunks models.PRDiffHunks) error {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	diffPath := filepath.Join(prPath, "diff-hunks.json")

	if err := gs.locker.WithLock(diffPath, func() error {
		return gs.store.Set(diffPath, diffHunks)
	}); err != nil {
		return fmt.Errorf("failed to store diff hunks with lock: %w", err)
	}
	return nil
}

// GetDiffHunks retrieves the diff hunks for a pull request.
func (gs *GitHubStorage) GetDiffHunks(owner, repo string, prNumber int) (*models.PRDiffHunks, error) {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	diffPath := filepath.Join(prPath, "diff-hunks.json")

	var diffHunks models.PRDiffHunks
	err := gs.store.Get(diffPath, &diffHunks)
	if err != nil {
		return nil, fmt.Errorf("failed to get diff hunks: %w", err)
	}

	return &diffHunks, nil
}

// AddComment adds a comment to a pull request.
func (gs *GitHubStorage) AddComment(owner, repo string, prNumber int, comment models.Comment) error {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		var prComments models.PRComments
		if gs.store.Exists(commentsPath) {
			if err := gs.store.Get(commentsPath, &prComments); err != nil {
				return fmt.Errorf("failed to get existing comments: %w", err)
			}
		} else {
			prComments = models.PRComments{
				PRNumber: prNumber,
				Owner:    owner,
				Repo:     repo,
				Comments: []models.Comment{},
			}
		}

		for _, existingComment := range prComments.Comments {
			if comment.IsDuplicate(existingComment) {
				return fmt.Errorf("duplicate comment detected")
			}
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
func (gs *GitHubStorage) GetComments(owner, repo string, prNumber int) (*models.PRComments, error) {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if !gs.store.Exists(commentsPath) {
		return &models.PRComments{
			PRNumber: prNumber,
			Owner:    owner,
			Repo:     repo,
			Comments: []models.Comment{},
		}, nil
	}

	var prComments models.PRComments
	err := gs.store.Get(commentsPath, &prComments)
	if err != nil {
		return nil, fmt.Errorf("failed to get comments: %w", err)
	}

	return &prComments, nil
}

// DeleteComment deletes a specific comment.
func (gs *GitHubStorage) DeleteComment(owner, repo string, prNumber int, file string, line int, side string) error {
	return gs.deleteComments(owner, repo, prNumber, func(comment models.Comment) bool {
		return comment.Path == file && comment.Line == line && comment.Side == side
	}, false)
}

// DeleteCommentsInRange deletes comments within a line range.
func (gs *GitHubStorage) DeleteCommentsInRange(owner, repo string, prNumber int, file string, startLine, endLine int, side string) error {
	return gs.deleteComments(owner, repo, prNumber, func(comment models.Comment) bool {
		return comment.Path == file &&
			comment.Side == side &&
			comment.Line >= startLine &&
			comment.Line <= endLine
	}, true)
}

// DeleteAllCommentsOnLine deletes all comments on a specific line.
func (gs *GitHubStorage) DeleteAllCommentsOnLine(owner, repo string, prNumber int, file string, line int, side string) error {
	return gs.deleteComments(owner, repo, prNumber, func(comment models.Comment) bool {
		return comment.Path == file && comment.Line == line && comment.Side == side
	}, true)
}

// DeleteCommentByIndex deletes a comment at a specific index.
func (gs *GitHubStorage) DeleteCommentByIndex(owner, repo string, prNumber int, file string, line int, side string, index int) error {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		prComments, err := gs.GetComments(owner, repo, prNumber)
		if err != nil {
			return err
		}

		var matchingIndices []int
		for i, comment := range prComments.Comments {
			if comment.Path == file && comment.Line == line && comment.Side == side {
				matchingIndices = append(matchingIndices, i)
			}
		}

		if len(matchingIndices) == 0 {
			return fmt.Errorf("no comments found")
		}

		if index < 0 || index >= len(matchingIndices) {
			return fmt.Errorf("invalid index %d, valid range: 0-%d", index, len(matchingIndices)-1)
		}

		actualIndex := matchingIndices[index]
		prComments.Comments = append(prComments.Comments[:actualIndex], prComments.Comments[actualIndex+1:]...)
		prComments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, prComments)
	}); err != nil {
		return fmt.Errorf("failed to delete comment by index with lock: %w", err)
	}
	return nil
}

// FindCommentsOnLine finds all comments on a specific line.
func (gs *GitHubStorage) FindCommentsOnLine(owner, repo string, prNumber int, file string, line int, side string) ([]models.CommentMatch, error) {
	prComments, err := gs.GetComments(owner, repo, prNumber)
	if err != nil {
		return nil, err
	}

	var matches []models.CommentMatch
	index := 0
	for _, comment := range prComments.Comments {
		if comment.Path == file && comment.Line == line && comment.Side == side {
			matches = append(matches, models.CommentMatch{
				Index:   index,
				Comment: comment,
			})
			index++
		}
	}

	return matches, nil
}

// deleteComments is a helper function for deleting comments based on a filter.
func (gs *GitHubStorage) deleteComments(owner, repo string, prNumber int, shouldDelete func(models.Comment) bool, allowMultiple bool) error {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		prComments, err := gs.GetComments(owner, repo, prNumber)
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
func (gs *GitHubStorage) ClearComments(owner, repo string, prNumber int) error {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	commentsPath := filepath.Join(prPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		return gs.store.Delete(commentsPath)
	}); err != nil {
		return fmt.Errorf("failed to clear comments with lock: %w", err)
	}
	return nil
}

// ClearCommentsForFile removes all comments for a specific file.
func (gs *GitHubStorage) ClearCommentsForFile(owner, repo string, prNumber int, file string) error {
	return gs.deleteComments(owner, repo, prNumber, func(comment models.Comment) bool {
		return comment.Path == file
	}, true)
}

// ValidateCommentAgainstDiff validates that a comment line exists in the diff hunks.
func (gs *GitHubStorage) ValidateCommentAgainstDiff(owner, repo string, prNumber int, comment models.Comment) error {
	diffHunks, err := gs.GetDiffHunks(owner, repo, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get diff hunks for validation: %w", err)
	}

	for _, hunk := range diffHunks.DiffHunks {
		if hunk.File == comment.Path && hunk.Side == comment.Side {
			if hunk.IsInRange(comment.Line) {
				if comment.SHA != "" && comment.SHA != hunk.SHA {
					return fmt.Errorf("SHA mismatch: comment SHA %s does not match hunk SHA %s", comment.SHA, hunk.SHA)
				}
				return nil
			}
		}
	}

	return fmt.Errorf("line %d in file %s (side %s) is not within any diff hunk", comment.Line, comment.Path, comment.Side)
}

// SetPRMetadata stores metadata for a pull request.
func (gs *GitHubStorage) SetPRMetadata(owner, repo string, prNumber int, metadata map[string]interface{}) error {
	prPath := gs.buildPRPath(owner, repo, prNumber)
	metadataPath := filepath.Join(prPath, "metadata.json")

	if err := gs.locker.WithLock(metadataPath, func() error {
		return gs.store.Set(metadataPath, metadata)
	}); err != nil {
		return fmt.Errorf("failed to set PR metadata with lock: %w", err)
	}
	return nil
}

// GetPRMetadata retrieves metadata for a pull request.
func (gs *GitHubStorage) GetPRMetadata(owner, repo string, prNumber int) (map[string]interface{}, error) {
	prPath := gs.buildPRPath(owner, repo, prNumber)
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
func (gs *GitHubStorage) RecordNotification(owner, repo string, prNumber int, reviewerLogin string, timestamp time.Time) error {
	prPath := gs.buildPRPath(owner, repo, prNumber)
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
func (gs *GitHubStorage) GetLastNotification(owner, repo string, prNumber int, reviewerLogin string) (*time.Time, error) {
	prPath := gs.buildPRPath(owner, repo, prNumber)
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
func (gs *GitHubStorage) ListPRs(owner, repo string) ([]int, error) {
	repoPath := filepath.Join("repos", owner, repo, "pull")

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
func ParseRepoAndPR(repoSpec string) (owner, repo string, err error) {
	parts := strings.Split(repoSpec, "/")
	if len(parts) != 2 {
		return "", "", fmt.Errorf("invalid repository format, expected 'owner/repo'")
	}
	return parts[0], parts[1], nil
}

// Branch-specific storage methods

// CaptureBranchDiffHunks stores the diff hunks for a branch.
func (gs *GitHubStorage) CaptureBranchDiffHunks(owner, repo string, branchName string, diffHunks models.BranchDiffHunks) error {
	branchPath := gs.buildBranchPath(owner, repo, branchName)
	diffPath := filepath.Join(branchPath, "diff-hunks.json")

	if err := gs.locker.WithLock(diffPath, func() error {
		return gs.store.Set(diffPath, diffHunks)
	}); err != nil {
		return fmt.Errorf("failed to store branch diff hunks with lock: %w", err)
	}
	return nil
}

// GetBranchDiffHunks retrieves the diff hunks for a branch.
func (gs *GitHubStorage) GetBranchDiffHunks(owner, repo string, branchName string) (*models.BranchDiffHunks, error) {
	branchPath := gs.buildBranchPath(owner, repo, branchName)
	diffPath := filepath.Join(branchPath, "diff-hunks.json")

	var diffHunks models.BranchDiffHunks
	err := gs.store.Get(diffPath, &diffHunks)
	if err != nil {
		return nil, fmt.Errorf("failed to get branch diff hunks: %w", err)
	}

	return &diffHunks, nil
}

// AddBranchComment adds a comment to a branch.
func (gs *GitHubStorage) AddBranchComment(owner, repo string, branchName string, comment models.Comment) error {
	branchPath := gs.buildBranchPath(owner, repo, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		var branchComments models.BranchComments
		if gs.store.Exists(commentsPath) {
			if err := gs.store.Get(commentsPath, &branchComments); err != nil {
				return fmt.Errorf("failed to get existing branch comments: %w", err)
			}
		} else {
			branchComments = models.BranchComments{
				BranchName: branchName,
				Owner:      owner,
				Repo:       repo,
				Comments:   []models.Comment{},
			}
		}

		for _, existingComment := range branchComments.Comments {
			if comment.IsDuplicate(existingComment) {
				return fmt.Errorf("duplicate comment detected")
			}
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
func (gs *GitHubStorage) GetBranchComments(owner, repo string, branchName string) (*models.BranchComments, error) {
	branchPath := gs.buildBranchPath(owner, repo, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if !gs.store.Exists(commentsPath) {
		return &models.BranchComments{
			BranchName: branchName,
			Owner:      owner,
			Repo:       repo,
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
func (gs *GitHubStorage) ClearBranchComments(owner, repo string, branchName string) error {
	branchPath := gs.buildBranchPath(owner, repo, branchName)
	commentsPath := filepath.Join(branchPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		return gs.store.Delete(commentsPath)
	}); err != nil {
		return fmt.Errorf("failed to clear branch comments with lock: %w", err)
	}
	return nil
}

// ValidateBranchCommentAgainstDiff validates that a comment line exists in the branch diff hunks.
func (gs *GitHubStorage) ValidateBranchCommentAgainstDiff(owner, repo string, branchName string, comment models.Comment) error {
	diffHunks, err := gs.GetBranchDiffHunks(owner, repo, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch diff hunks for validation: %w", err)
	}

	for _, hunk := range diffHunks.DiffHunks {
		if hunk.File == comment.Path && hunk.Side == comment.Side {
			if hunk.IsInRange(comment.Line) {
				if comment.SHA != "" && comment.SHA != hunk.SHA {
					return fmt.Errorf("SHA mismatch: comment SHA %s does not match hunk SHA %s", comment.SHA, hunk.SHA)
				}
				return nil
			}
		}
	}

	return fmt.Errorf("line %d in file %s (side %s) is not within any diff hunk", comment.Line, comment.Path, comment.Side)
}

// SetBranchMetadata stores metadata for a branch.
func (gs *GitHubStorage) SetBranchMetadata(owner, repo string, branchName string, metadata map[string]interface{}) error {
	branchPath := gs.buildBranchPath(owner, repo, branchName)
	metadataPath := filepath.Join(branchPath, "metadata.json")

	if err := gs.locker.WithLock(metadataPath, func() error {
		return gs.store.Set(metadataPath, metadata)
	}); err != nil {
		return fmt.Errorf("failed to set branch metadata with lock: %w", err)
	}
	return nil
}

// GetBranchMetadata retrieves metadata for a branch.
func (gs *GitHubStorage) GetBranchMetadata(owner, repo string, branchName string) (map[string]interface{}, error) {
	branchPath := gs.buildBranchPath(owner, repo, branchName)
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
