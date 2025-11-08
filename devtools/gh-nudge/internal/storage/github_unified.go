package storage

import (
	"fmt"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// Unified storage methods that work with ReviewTarget interface.
// These methods replace the separate PR and Branch methods for new code.

// GetCommentsUnified retrieves comments for any review target (PR or branch).
func (gs *GitHubStorage) GetCommentsUnified(repository models.Repository, target models.ReviewTarget) (*models.ReviewComments, error) {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	if !gs.store.Exists(commentsPath) {
		return &models.ReviewComments{
			Target:     target.String(),
			Repository: repository,
			Comments:   []models.Comment{},
		}, nil
	}

	var comments models.ReviewComments
	err := gs.store.Get(commentsPath, &comments)
	if err != nil {
		return nil, fmt.Errorf("failed to get comments: %w", err)
	}

	return &comments, nil
}

// AddCommentUnified adds a comment to any review target (PR or branch).
func (gs *GitHubStorage) AddCommentUnified(repository models.Repository, target models.ReviewTarget, comment models.Comment) error {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	if err := gs.locker.WithLockRetry(commentsPath, gs.lockConfig, func() error {
		var comments models.ReviewComments
		if gs.store.Exists(commentsPath) {
			if err := gs.store.Get(commentsPath, &comments); err != nil {
				return fmt.Errorf("failed to get existing comments: %w", err)
			}
		} else {
			comments = models.ReviewComments{
				Target:     target.String(),
				Repository: repository,
				Comments:   []models.Comment{},
			}
		}

		for _, existingComment := range comments.Comments {
			if comment.IsDuplicate(existingComment) {
				return fmt.Errorf("duplicate comment detected")
			}
		}

		if comment.ID == "" {
			comment.ID = models.GenerateCommentID()
		}
		comment.CreatedAt = time.Now()
		comments.Comments = append(comments.Comments, comment)
		comments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, comments)
	}); err != nil {
		return fmt.Errorf("failed to add comment with lock: %w", err)
	}
	return nil
}

// DeleteCommentByIDUnified deletes a comment by ID prefix for any review target.
func (gs *GitHubStorage) DeleteCommentByIDUnified(repository models.Repository, target models.ReviewTarget, idPrefix string) error {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		comments, err := gs.GetCommentsUnified(repository, target)
		if err != nil {
			return err
		}

		matchIndex := -1
		matchCount := 0
		for i, comment := range comments.Comments {
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

		comments.Comments = append(comments.Comments[:matchIndex], comments.Comments[matchIndex+1:]...)
		comments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, comments)
	}); err != nil {
		return fmt.Errorf("failed to delete comment by ID with lock: %w", err)
	}
	return nil
}

// ClearCommentsUnified removes all comments for any review target.
func (gs *GitHubStorage) ClearCommentsUnified(repository models.Repository, target models.ReviewTarget) error {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		return gs.store.Delete(commentsPath)
	}); err != nil {
		return fmt.Errorf("failed to clear comments with lock: %w", err)
	}
	return nil
}

// ClearCommentsForFileUnified removes all comments for a specific file in any review target.
func (gs *GitHubStorage) ClearCommentsForFileUnified(repository models.Repository, target models.ReviewTarget, file string) error {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	return gs.locker.WithLock(commentsPath, func() error {
		comments, err := gs.GetCommentsUnified(repository, target)
		if err != nil {
			return err
		}

		// Filter out comments for the specified file
		var filteredComments []models.Comment
		for _, comment := range comments.Comments {
			if comment.Path != file {
				filteredComments = append(filteredComments, comment)
			}
		}

		comments.Comments = filteredComments
		comments.UpdatedAt = time.Now()

		return gs.store.Set(commentsPath, comments)
	})
}

// UpdateCommentsUnified updates the entire comment list for any review target.
func (gs *GitHubStorage) UpdateCommentsUnified(repository models.Repository, target models.ReviewTarget, comments *models.ReviewComments) error {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		comments.UpdatedAt = time.Now()
		return gs.store.Set(commentsPath, comments)
	}); err != nil {
		return fmt.Errorf("failed to update comments with lock: %w", err)
	}
	return nil
}

// GetDiffHunksUnified retrieves diff hunks for any review target.
func (gs *GitHubStorage) GetDiffHunksUnified(repository models.Repository, target models.ReviewTarget) (*models.ReviewDiffHunks, error) {
	targetPath := target.BuildPath(repository)
	diffPath := filepath.Join(targetPath, "diff-hunks.json")

	var diffHunks models.ReviewDiffHunks
	err := gs.store.Get(diffPath, &diffHunks)
	if err != nil {
		return nil, fmt.Errorf("failed to get diff hunks: %w", err)
	}

	return &diffHunks, nil
}

// CaptureDiffHunksUnified stores diff hunks for any review target.
func (gs *GitHubStorage) CaptureDiffHunksUnified(repository models.Repository, target models.ReviewTarget, diffHunks models.ReviewDiffHunks) error {
	targetPath := target.BuildPath(repository)
	diffPath := filepath.Join(targetPath, "diff-hunks.json")

	if err := gs.locker.WithLock(diffPath, func() error {
		return gs.store.Set(diffPath, diffHunks)
	}); err != nil {
		return fmt.Errorf("failed to store diff hunks with lock: %w", err)
	}
	return nil
}

// ValidateCommentAgainstDiffUnified validates that a comment line exists in the diff hunks for any review target.
func (gs *GitHubStorage) ValidateCommentAgainstDiffUnified(repository models.Repository, target models.ReviewTarget, comment models.Comment) error {
	diffHunks, err := gs.GetDiffHunksUnified(repository, target)
	if err != nil {
		return fmt.Errorf("failed to get diff hunks for validation: %w", err)
	}

	for _, hunk := range diffHunks.DiffHunks {
		if hunk.Location.Path == comment.Path && hunk.Side == comment.Side {
			if comment.Line.Overlaps(hunk.Location.Lines) {
				if comment.SHA != "" && comment.SHA != hunk.SHA {
					return fmt.Errorf("SHA mismatch: comment SHA %s does not match hunk SHA %s", comment.SHA, hunk.SHA)
				}
				return nil
			}
		}
	}

	return fmt.Errorf("line %v in file %s (side %s) is not within any diff hunk", comment.Line, comment.Path, comment.Side)
}

// DiffHunksExistUnified checks if diff hunks exist for any review target.
func (gs *GitHubStorage) DiffHunksExistUnified(repository models.Repository, target models.ReviewTarget) bool {
	targetPath := target.BuildPath(repository)
	diffPath := filepath.Join(targetPath, "diff-hunks.json")
	return gs.store.Exists(diffPath)
}
