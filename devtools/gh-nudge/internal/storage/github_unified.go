package storage

import (
	"fmt"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// Unified storage methods that work with ReviewTarget interface.
// These methods replace the separate PR and Branch methods for new code.

// GetComments retrieves comments for any review target (PR or branch).
func (gs *GitHubStorage) GetComments(repository models.Repository, target models.ReviewTarget) (*models.ReviewComments, error) {
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

// AddComment adds a comment to any review target (PR or branch).
func (gs *GitHubStorage) AddComment(repository models.Repository, target models.ReviewTarget, comment models.Comment) error {
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

// DeleteCommentByID deletes a comment by ID prefix for any review target.
func (gs *GitHubStorage) DeleteCommentByID(repository models.Repository, target models.ReviewTarget, idPrefix string) error {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	if err := gs.locker.WithLock(commentsPath, func() error {
		comments, err := gs.GetComments(repository, target)
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

// ClearCommentsForFile removes all comments for a specific file in any review target.
func (gs *GitHubStorage) ClearCommentsForFile(repository models.Repository, target models.ReviewTarget, file string) error {
	targetPath := target.BuildPath(repository)
	commentsPath := filepath.Join(targetPath, "comments.json")

	return gs.locker.WithLock(commentsPath, func() error {
		comments, err := gs.GetComments(repository, target)
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

// UpdateComments updates the entire comment list for any review target.
func (gs *GitHubStorage) UpdateComments(repository models.Repository, target models.ReviewTarget, comments *models.ReviewComments) error {
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

// GetDiffHunks retrieves diff hunks for any review target.
func (gs *GitHubStorage) GetDiffHunks(repository models.Repository, target models.ReviewTarget) (*models.ReviewDiffHunks, error) {
	targetPath := target.BuildPath(repository)
	diffPath := filepath.Join(targetPath, "diff-hunks.json")

	var diffHunks models.ReviewDiffHunks
	err := gs.store.Get(diffPath, &diffHunks)
	if err != nil {
		return nil, fmt.Errorf("failed to get diff hunks: %w", err)
	}

	return &diffHunks, nil
}

// CaptureDiffHunks stores diff hunks for any review target.
func (gs *GitHubStorage) CaptureDiffHunks(repository models.Repository, target models.ReviewTarget, diffHunks models.ReviewDiffHunks) error {
	targetPath := target.BuildPath(repository)
	diffPath := filepath.Join(targetPath, "diff-hunks.json")

	if err := gs.locker.WithLock(diffPath, func() error {
		return gs.store.Set(diffPath, diffHunks)
	}); err != nil {
		return fmt.Errorf("failed to store diff hunks with lock: %w", err)
	}
	return nil
}

// ValidateCommentAgainstDiff validates that a comment line exists in the diff hunks for any review target.
func (gs *GitHubStorage) ValidateCommentAgainstDiff(repository models.Repository, target models.ReviewTarget, comment models.Comment) error {
	diffHunks, err := gs.GetDiffHunks(repository, target)
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

// DiffHunksExist checks if diff hunks exist for any review target.
func (gs *GitHubStorage) DiffHunksExist(repository models.Repository, target models.ReviewTarget) bool {
	targetPath := target.BuildPath(repository)
	diffPath := filepath.Join(targetPath, "diff-hunks.json")
	return gs.store.Exists(diffPath)
}
