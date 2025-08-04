package prreview

import (
	"encoding/json"
	"fmt"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// AdjustmentPreview represents the preview of line adjustments.
type AdjustmentPreview struct {
	Owner          string                  `json:"owner"`
	Repo           string                  `json:"repo"`
	Identifier     string                  `json:"identifier"`
	File           string                  `json:"file"`
	DiffSpec       string                  `json:"diff_spec"`
	Adjustments    []models.LineAdjustment `json:"adjustments"`
	CommentChanges []CommentChange         `json:"comment_changes"`
	Warnings       []string                `json:"warnings,omitempty"`
}

// CommentChange represents how a comment will be adjusted.
type CommentChange struct {
	CommentID      string `json:"comment_id"`
	CommentIDShort string `json:"comment_id_short"`
	OriginalLine   int    `json:"original_line"`
	NewLine        int    `json:"new_line"`
	StartLine      *int   `json:"start_line,omitempty"`
	NewStartLine   *int   `json:"new_start_line,omitempty"`
	Body           string `json:"body"`
	Status         string `json:"status"` // "adjusted", "deleted", "warning"
	Warning        string `json:"warning,omitempty"`
}

// AdjustCommand adjusts comment line numbers based on diff specifications.
func (ch *CommandHandler) AdjustCommand(owner, repo, identifier, file, diffSpec string, dryRun, force bool, format string) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	// Parse diff spec
	adjustments, err := models.ParseDiffSpec(diffSpec)
	if err != nil {
		return fmt.Errorf("invalid diff spec: %w", err)
	}

	// Get preview
	var preview string
	if parsed.IsPR() {
		preview, err = ch.getAdjustmentPreview(owner, repo, parsed.PRNumber, file, diffSpec, format)
	} else {
		preview, err = ch.getBranchAdjustmentPreview(owner, repo, parsed.BranchName, file, diffSpec, format)
	}
	if err != nil {
		return err
	}

	// Display preview
	fmt.Println(preview)

	// If dry run, stop here
	if dryRun {
		fmt.Println("\n[DRY RUN] No changes were made.")
		return nil
	}

	// Apply adjustments
	if parsed.IsPR() {
		return ch.applyPRAdjustments(owner, repo, parsed.PRNumber, file, adjustments, force)
	}
	return ch.applyBranchAdjustments(owner, repo, parsed.BranchName, file, adjustments, force)
}

// getBranchAdjustmentPreview generates a preview of the adjustments for a branch.
func (ch *CommandHandler) getBranchAdjustmentPreview(owner, repo, branchName, file, diffSpec, format string) (string, error) {
	// Parse adjustments
	adjustments, err := models.ParseDiffSpec(diffSpec)
	if err != nil {
		return "", fmt.Errorf("failed to parse diff spec: %w", err)
	}

	// Get comments for the file
	branchComments, err := ch.storage.GetBranchComments(owner, repo, branchName)
	if err != nil {
		return "", fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Get diff hunks
	branchDiffHunks, err := ch.storage.GetBranchDiffHunks(owner, repo, branchName)
	if err != nil {
		return "", fmt.Errorf("failed to get branch diff hunks: %w", err)
	}

	preview := AdjustmentPreview{
		Owner:       owner,
		Repo:        repo,
		Identifier:  branchName,
		File:        file,
		DiffSpec:    diffSpec,
		Adjustments: adjustments,
	}

	// Process each comment in the file
	for _, comment := range branchComments.Comments {
		if comment.Path != file {
			continue
		}

		change := CommentChange{
			CommentID:      comment.ID,
			CommentIDShort: comment.FormatIDShort(),
			OriginalLine:   comment.Line,
			Body:           truncateString(comment.Body, 50),
		}

		if comment.StartLine != nil {
			change.StartLine = comment.StartLine
		}

		// Test adjustment
		testComment := comment
		if models.AdjustComment(&testComment, adjustments) {
			change.NewLine = testComment.Line
			change.NewStartLine = testComment.StartLine
			change.Status = "adjusted"

			// Validate against diff hunks
			if err := models.ValidateAdjustmentAgainstDiff(testComment, adjustments, branchDiffHunks.DiffHunks); err != nil {
				change.Status = "warning"
				change.Warning = err.Error()
				preview.Warnings = append(preview.Warnings, fmt.Sprintf("Comment %s: %s", change.CommentIDShort, err.Error()))
			}
		} else {
			change.Status = "deleted"
			change.NewLine = -1
			change.Warning = "Comment on deleted line"
			preview.Warnings = append(preview.Warnings, fmt.Sprintf("Comment %s will be marked as orphaned (on deleted line)", change.CommentIDShort))
		}

		preview.CommentChanges = append(preview.CommentChanges, change)
	}

	// Format output
	if format == "json" {
		return formatJSONPreview(preview)
	}
	return formatTablePreview(preview)
}

// getAdjustmentPreview generates a preview of the adjustments.
func (ch *CommandHandler) getAdjustmentPreview(owner, repo string, prNumber int, file, diffSpec, format string) (string, error) {
	// Parse adjustments
	adjustments, err := models.ParseDiffSpec(diffSpec)
	if err != nil {
		return "", fmt.Errorf("failed to parse diff spec: %w", err)
	}

	// Get comments for the file
	prComments, err := ch.storage.GetComments(owner, repo, prNumber)
	if err != nil {
		return "", fmt.Errorf("failed to get comments: %w", err)
	}

	// Get diff hunks
	prDiffHunks, err := ch.storage.GetDiffHunks(owner, repo, prNumber)
	if err != nil {
		return "", fmt.Errorf("failed to get diff hunks: %w", err)
	}

	preview := AdjustmentPreview{
		Owner:       owner,
		Repo:        repo,
		Identifier:  fmt.Sprintf("%d", prNumber),
		File:        file,
		DiffSpec:    diffSpec,
		Adjustments: adjustments,
	}

	// Process each comment in the file
	for _, comment := range prComments.Comments {
		if comment.Path != file {
			continue
		}

		change := CommentChange{
			CommentID:      comment.ID,
			CommentIDShort: comment.FormatIDShort(),
			OriginalLine:   comment.Line,
			Body:           truncateString(comment.Body, 50),
		}

		if comment.StartLine != nil {
			change.StartLine = comment.StartLine
		}

		// Test adjustment
		testComment := comment
		if models.AdjustComment(&testComment, adjustments) {
			change.NewLine = testComment.Line
			change.NewStartLine = testComment.StartLine
			change.Status = "adjusted"

			// Validate against diff hunks
			if err := models.ValidateAdjustmentAgainstDiff(testComment, adjustments, prDiffHunks.DiffHunks); err != nil {
				change.Status = "warning"
				change.Warning = err.Error()
				preview.Warnings = append(preview.Warnings, fmt.Sprintf("Comment %s: %s", change.CommentIDShort, err.Error()))
			}
		} else {
			change.Status = "deleted"
			change.NewLine = -1
			change.Warning = "Comment on deleted line"
			preview.Warnings = append(preview.Warnings, fmt.Sprintf("Comment %s will be marked as orphaned (on deleted line)", change.CommentIDShort))
		}

		preview.CommentChanges = append(preview.CommentChanges, change)
	}

	// Format output
	if format == "json" {
		return formatJSONPreview(preview)
	}
	return formatTablePreview(preview)
}

// applyPRAdjustments applies adjustments to PR comments.
func (ch *CommandHandler) applyPRAdjustments(owner, repo string, prNumber int, file string, adjustments []models.LineAdjustment, force bool) error {
	// Get all comments
	prComments, err := ch.storage.GetComments(owner, repo, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Get diff hunks for validation
	prDiffHunks, err := ch.storage.GetDiffHunks(owner, repo, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get diff hunks: %w", err)
	}

	adjustedCount := 0
	orphanedCount := 0
	warningCount := 0

	// Process each comment
	updatedComments := make([]models.Comment, 0, len(prComments.Comments))
	for _, comment := range prComments.Comments {
		if comment.Path != file {
			// Keep comments from other files unchanged
			updatedComments = append(updatedComments, comment)
			continue
		}

		// Apply adjustments
		if models.AdjustComment(&comment, adjustments) {
			// Validate against diff hunks
			if err := models.ValidateAdjustmentAgainstDiff(comment, adjustments, prDiffHunks.DiffHunks); err != nil {
				if !force {
					return fmt.Errorf("comment %s: %w (use --force to override)", comment.FormatIDShort(), err)
				}
				warningCount++
				fmt.Printf("Warning: Comment %s: %s\n", comment.FormatIDShort(), err.Error())
			}
			adjustedCount++
			updatedComments = append(updatedComments, comment)
		} else {
			// Comment is on deleted line
			orphanedCount++
			fmt.Printf("Comment %s orphaned (on deleted line)\n", comment.FormatIDShort())
			// Optionally keep orphaned comments with a special marker
			if force {
				comment.Body = fmt.Sprintf("[ORPHANED - Original line %d deleted]\n%s", comment.OriginalLine, comment.Body)
				updatedComments = append(updatedComments, comment)
			}
		}
	}

	// Update storage
	prComments.Comments = updatedComments
	prComments.UpdatedAt = time.Now()

	if err := ch.storage.UpdateComments(owner, repo, prNumber, *prComments); err != nil {
		return fmt.Errorf("failed to update comments: %w", err)
	}

	fmt.Printf("\nAdjustment complete:\n")
	fmt.Printf("- %d comments adjusted\n", adjustedCount)
	if orphanedCount > 0 {
		fmt.Printf("- %d comments orphaned\n", orphanedCount)
	}
	if warningCount > 0 {
		fmt.Printf("- %d warnings\n", warningCount)
	}

	return nil
}

// applyBranchAdjustments applies adjustments to branch comments.
func (ch *CommandHandler) applyBranchAdjustments(owner, repo, branchName, file string, adjustments []models.LineAdjustment, force bool) error {
	// Get all comments
	branchComments, err := ch.storage.GetBranchComments(owner, repo, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Get diff hunks for validation
	branchDiffHunks, err := ch.storage.GetBranchDiffHunks(owner, repo, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch diff hunks: %w", err)
	}

	adjustedCount := 0
	orphanedCount := 0
	warningCount := 0

	// Process each comment
	updatedComments := make([]models.Comment, 0, len(branchComments.Comments))
	for _, comment := range branchComments.Comments {
		if comment.Path != file {
			// Keep comments from other files unchanged
			updatedComments = append(updatedComments, comment)
			continue
		}

		// Apply adjustments
		if models.AdjustComment(&comment, adjustments) {
			// Validate against diff hunks
			if err := models.ValidateAdjustmentAgainstDiff(comment, adjustments, branchDiffHunks.DiffHunks); err != nil {
				if !force {
					return fmt.Errorf("comment %s: %w (use --force to override)", comment.FormatIDShort(), err)
				}
				warningCount++
				fmt.Printf("Warning: Comment %s: %s\n", comment.FormatIDShort(), err.Error())
			}
			adjustedCount++
			updatedComments = append(updatedComments, comment)
		} else {
			// Comment is on deleted line
			orphanedCount++
			fmt.Printf("Comment %s orphaned (on deleted line)\n", comment.FormatIDShort())
			// Optionally keep orphaned comments with a special marker
			if force {
				comment.Body = fmt.Sprintf("[ORPHANED - Original line %d deleted]\n%s", comment.OriginalLine, comment.Body)
				updatedComments = append(updatedComments, comment)
			}
		}
	}

	// Update storage
	branchComments.Comments = updatedComments
	branchComments.UpdatedAt = time.Now()

	if err := ch.storage.UpdateBranchComments(owner, repo, branchName, *branchComments); err != nil {
		return fmt.Errorf("failed to update branch comments: %w", err)
	}

	fmt.Printf("\nAdjustment complete:\n")
	fmt.Printf("- %d comments adjusted\n", adjustedCount)
	if orphanedCount > 0 {
		fmt.Printf("- %d comments orphaned\n", orphanedCount)
	}
	if warningCount > 0 {
		fmt.Printf("- %d warnings\n", warningCount)
	}

	return nil
}

// formatTablePreview formats the preview as a table.
func formatTablePreview(preview AdjustmentPreview) (string, error) {
	var sb strings.Builder

	// Header
	fmt.Fprintf(&sb, "Adjustment Preview for %s/%s#%s %s\n", preview.Owner, preview.Repo, preview.Identifier, preview.File)
	fmt.Fprintf(&sb, "Diff spec: %s\n\n", preview.DiffSpec)

	// Adjustments
	fmt.Fprintln(&sb, "Adjustments to apply:")
	for _, adj := range preview.Adjustments {
		fmt.Fprintf(&sb, "  - %s\n", adj.FormatDescription())
	}
	fmt.Fprintln(&sb)

	// Comment changes
	if len(preview.CommentChanges) == 0 {
		fmt.Fprintln(&sb, "No comments in this file.")
		return sb.String(), nil
	}

	fmt.Fprintln(&sb, "Comment changes:")
	w := tabwriter.NewWriter(&sb, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tOriginal\tNew\tStatus\tPreview")
	fmt.Fprintln(w, "--\t--------\t---\t------\t-------")

	for _, change := range preview.CommentChanges {
		lineStr := formatLineChange(change.OriginalLine, change.StartLine, change.NewLine, change.NewStartLine)
		status := change.Status
		if change.Warning != "" {
			status = fmt.Sprintf("%s ⚠", status)
		}
		fmt.Fprintf(w, "%s\t%s\t%s\t%s\n", change.CommentIDShort, lineStr, status, change.Body)
	}
	w.Flush()

	// Warnings
	if len(preview.Warnings) > 0 {
		fmt.Fprintln(&sb, "\nWarnings:")
		for _, warning := range preview.Warnings {
			fmt.Fprintf(&sb, "  ⚠ %s\n", warning)
		}
	}

	return sb.String(), nil
}

// formatJSONPreview formats the preview as JSON.
func formatJSONPreview(preview AdjustmentPreview) (string, error) {
	data, err := json.MarshalIndent(preview, "", "  ")
	if err != nil {
		return "", fmt.Errorf("failed to marshal JSON: %w", err)
	}
	return string(data), nil
}

// formatLineChange formats a line change for display.
func formatLineChange(origLine int, origStart *int, newLine int, newStart *int) string {
	var orig, adjusted string

	if origStart != nil && *origStart != origLine {
		orig = fmt.Sprintf("%d-%d", *origStart, origLine)
	} else {
		orig = fmt.Sprintf("%d", origLine)
	}

	switch {
	case newLine == -1:
		adjusted = "deleted"
	case newStart != nil && *newStart != newLine:
		adjusted = fmt.Sprintf("%d-%d", *newStart, newLine)
	default:
		adjusted = fmt.Sprintf("%d", newLine)
	}

	return fmt.Sprintf("%s → %s", orig, adjusted)
}

// truncateString truncates a string to the specified length.
func truncateString(s string, maxLen int) string {
	s = strings.ReplaceAll(s, "\n", " ")
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
