package prreview

import (
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// Unified command methods that work with ReviewTarget interface.
// These methods replace duplicate PR and Branch methods.

// addCommentUnified adds a line-specific comment to any review target.
func (ch *CommandHandler) addCommentUnified(repository models.Repository, target models.ReviewTarget, file string, lineSpec, commentBody, side string, force bool) error {
	// Parse line specification
	lineRange, err := models.ParseLineSpec(lineSpec)
	if err != nil {
		return fmt.Errorf("invalid line specification %q: %w", lineSpec, err)
	}

	// Parse side
	parsedSide, err := models.ParseSide(side)
	if err != nil {
		return fmt.Errorf("invalid side %q: %w", side, err)
	}

	// Create comment
	comment := models.Comment{
		Path: file,
		Line: *lineRange,
		Body: commentBody,
		Side: parsedSide,
	}

	// Validate comment against diff hunks if they exist
	if ch.storage.DiffHunksExistUnified(repository, target) {
		if err := ch.storage.ValidateCommentAgainstDiffUnified(repository, target, comment); err != nil {
			if !force {
				return fmt.Errorf("validation failed: %w (use --force to override)", err)
			}
			fmt.Printf("Warning: %v\n", err)
		}
	} else {
		fmt.Printf("Warning: No diff hunks found, comment validation skipped\n")
	}

	// Add comment
	if err := ch.storage.AddCommentUnified(repository, target, comment); err != nil {
		if strings.Contains(err.Error(), "duplicate") && !force {
			return fmt.Errorf("duplicate comment detected (use --force to override): %w", err)
		}
		return fmt.Errorf("failed to add comment: %w", err)
	}

	fmt.Printf("Added comment to %s:%s in %s %s\n",
		file, lineSpec, target.String(), repository)
	return nil
}

// listCommentsUnified lists comments for any review target.
func (ch *CommandHandler) listCommentsUnified(repository models.Repository, target models.ReviewTarget, formatter OutputFormatter, filter models.CommentFilter, showContext bool, contextLines int) error {
	comments, err := ch.storage.GetCommentsUnified(repository, target)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Apply filters
	filteredComments := filter.Apply(comments.Comments)

	// Output results
	var output string
	if showContext {
		// Create comments with context
		commentsWithContext := make([]models.CommentWithLineContext, 0, len(filteredComments))
		for _, comment := range filteredComments {
			cwc := models.CommentWithLineContext{Comment: comment}

			// Try to get line context
			context, err := models.GetLineContextForComment(comment.Path, comment, contextLines)
			if err != nil {
				// If we can't get context (file not found, etc.), just skip it
				fmt.Printf("Warning: Could not get context for %s:%v - %v\n", comment.Path, comment.Line, err)
			} else {
				cwc.Context = context
			}

			commentsWithContext = append(commentsWithContext, cwc)
		}

		output, err = formatter.FormatCommentsWithContext(commentsWithContext)
		if err != nil {
			return fmt.Errorf("failed to format comments with context: %w", err)
		}
	} else {
		output, err = formatter.FormatComments(filteredComments)
		if err != nil {
			return fmt.Errorf("failed to format comments: %w", err)
		}
	}

	fmt.Println(output)
	return nil
}

// clearCommentsUnified clears comments for any review target.
func (ch *CommandHandler) clearCommentsUnified(repository models.Repository, target models.ReviewTarget, file string, confirm bool) error {
	if !confirm {
		if file != "" {
			fmt.Printf("This will delete all comments for file '%s' in %s %s. Continue? (y/N): ",
				file, target.String(), repository)
		} else {
			fmt.Printf("This will delete ALL comments for %s %s. Continue? (y/N): ",
				target.String(), repository)
		}

		var response string
		_, _ = fmt.Scanln(&response)
		if strings.ToLower(response) != "y" && strings.ToLower(response) != "yes" {
			fmt.Println("Operation cancelled")
			return nil
		}
	}

	var err error
	if file != "" {
		err = ch.storage.ClearCommentsForFileUnified(repository, target, file)
	} else {
		err = ch.storage.ClearCommentsUnified(repository, target)
	}

	if err != nil {
		return fmt.Errorf("failed to clear comments: %w", err)
	}

	if file != "" {
		fmt.Printf("Cleared all comments for file '%s' in %s %s\n",
			file, target.String(), repository)
	} else {
		fmt.Printf("Cleared all comments for %s %s\n",
			target.String(), repository)
	}

	return nil
}

// nextCommentUnified gets the next unresolved comment for any review target.
func (ch *CommandHandler) nextCommentUnified(repository models.Repository, target models.ReviewTarget, formatter OutputFormatter, file string, priority models.CommentPriority) error {
	comments, err := ch.storage.GetCommentsUnified(repository, target)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Filter and sort comments
	var unresolvedComments []models.Comment
	for _, comment := range comments.Comments {
		if !comment.IsUnresolved() {
			continue
		}
		if file != "" && comment.Path != file {
			continue
		}
		if priority != "" && comment.Priority != priority {
			continue
		}
		unresolvedComments = append(unresolvedComments, comment)
	}

	if len(unresolvedComments) == 0 {
		fmt.Println("No unresolved comments found.")
		return nil
	}

	// Sort by file path, then line number, then creation time
	nextComment := sortAndGetNextComment(unresolvedComments)

	// Output next comment using the single comment formatter
	output, err := formatter.FormatSingleComment(nextComment)
	if err != nil {
		return fmt.Errorf("failed to format comment: %w", err)
	}
	fmt.Print(output)

	return nil
}

// resolveCommentUnified marks a comment as resolved for any review target.
func (ch *CommandHandler) resolveCommentUnified(repository models.Repository, target models.ReviewTarget, commentID string, archive bool, reason string) error {
	// Get current comments
	comments, err := ch.storage.GetCommentsUnified(repository, target)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Find and update the comment
	found := false
	for i, comment := range comments.Comments {
		if comment.MatchesIDPrefix(commentID) {
			if archive {
				comments.Comments[i].Archive(reason)
			} else {
				comments.Comments[i].Resolve(reason)
			}
			found = true
			break
		}
	}

	if !found {
		return fmt.Errorf("comment with ID prefix '%s' not found", commentID)
	}

	// Save updated comments
	if err := ch.storage.UpdateCommentsUnified(repository, target, comments); err != nil {
		return fmt.Errorf("failed to update comments: %w", err)
	}

	if archive {
		fmt.Printf("Archived comment with ID prefix '%s' in %s %s\n", commentID, target.String(), repository)
	} else {
		fmt.Printf("Resolved comment with ID prefix '%s' in %s %s\n", commentID, target.String(), repository)
	}
	if reason != "" {
		fmt.Printf("Resolution reason: %s\n", reason)
	}

	return nil
}
