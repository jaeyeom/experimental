package prreview

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// AdjustmentPreview represents the preview of line adjustments.
type AdjustmentPreview struct {
	Repository     models.Repository       `json:"repository"`
	Identifier     string                  `json:"identifier"`
	File           string                  `json:"file"`
	DiffSpec       string                  `json:"diffSpec"`
	Adjustments    []models.LineAdjustment `json:"adjustments"`
	CommentChanges []CommentChange         `json:"commentChanges"`
	Warnings       []string                `json:"warnings,omitempty"`
}

// CommentChange represents how a comment will be adjusted.
type CommentChange struct {
	CommentID      string           `json:"commentId"`
	CommentIDShort string           `json:"commentIdShort"`
	OriginalLine   models.LineRange `json:"originalLine"`
	NewLine        models.LineRange `json:"newLine"`
	Body           string           `json:"body"`
	Status         string           `json:"status"` // "adjusted", "deleted", "warning"
	Warning        string           `json:"warning,omitempty"`
}

// MappingFileEntry represents an entry in a mapping file.
type MappingFileEntry struct {
	FilePath string
	Line     int
	Offset   int
}

// AdjustOptionsExtended holds extended options for the adjust command.
type AdjustOptionsExtended struct {
	DryRun      bool
	Force       bool
	Format      string
	Interactive bool
	AllFiles    bool
	MappingFile string
	AutoDetect  bool
}

// AdjustCommand adjusts comment line numbers based on diff specifications.
// Supports both single-file and multi-file processing modes.
func (ch *CommandHandler) AdjustCommand(repository models.Repository, identifier, file, diffSpec string, dryRun, force bool, format string) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	// Check if this is a unified diff that might contain multiple files
	if models.DetectFormat(diffSpec) == models.FormatUnifiedDiff {
		return ch.adjustCommandUnifiedDiff(repository, *parsed, file, diffSpec, dryRun, force, format)
	}

	// Traditional single-file processing
	return ch.adjustCommandSingleFile(repository, *parsed, file, diffSpec, dryRun, force, format)
}

// adjustCommandSingleFile handles traditional single-file adjustment.
func (ch *CommandHandler) adjustCommandSingleFile(repository models.Repository, parsed models.ParsedIdentifier, file, diffSpec string, dryRun, force bool, format string) error {
	// Parse diff spec with auto-detection
	adjustments, err := models.ParseDiffSpecWithAutoDetection(diffSpec)
	if err != nil {
		return fmt.Errorf("invalid diff spec: %w", err)
	}

	// Get preview
	var preview string
	if parsed.IsPR() {
		preview, err = ch.getAdjustmentPreview(repository, parsed.PRNumber, file, diffSpec, format)
	} else {
		preview, err = ch.getBranchAdjustmentPreview(repository, parsed.BranchName, file, diffSpec, format)
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
		return ch.applyPRAdjustments(repository, parsed.PRNumber, file, adjustments, force)
	}
	return ch.applyBranchAdjustments(repository, parsed.BranchName, file, adjustments, force)
}

// adjustCommandUnifiedDiff handles unified diff processing (single-file or multi-file).
func (ch *CommandHandler) adjustCommandUnifiedDiff(repository models.Repository, parsed models.ParsedIdentifier, file, diffSpec string, dryRun, force bool, format string) error {
	if file != "" {
		// Single-file mode: filter unified diff for specific file
		filteredDiff, err := models.FilterUnifiedDiffForFile(diffSpec, file)
		if err != nil {
			return fmt.Errorf("failed to filter unified diff for file %s: %w", file, err)
		}
		return ch.adjustCommandSingleFile(repository, parsed, file, filteredDiff, dryRun, force, format)
	}

	// Multi-file mode: process all files in the unified diff
	fileSpecs, err := models.ConvertUnifiedDiffToMultiFileClassicDiff(diffSpec)
	if err != nil {
		return fmt.Errorf("failed to parse unified diff: %w", err)
	}

	fmt.Printf("Processing %d file(s) from unified diff:\n", len(fileSpecs))
	for _, spec := range fileSpecs {
		fmt.Printf("- %s\n", spec.FilePath)
	}
	fmt.Println()

	totalAdjusted := 0
	totalOrphaned := 0
	totalWarnings := 0

	for i, spec := range fileSpecs {
		fmt.Printf("=== Processing file %d/%d: %s ===\n", i+1, len(fileSpecs), spec.FilePath)

		// Get preview for this file
		var preview string
		if parsed.IsPR() {
			preview, err = ch.getAdjustmentPreview(repository, parsed.PRNumber, spec.FilePath, spec.ClassicDiff, format)
		} else {
			preview, err = ch.getBranchAdjustmentPreview(repository, parsed.BranchName, spec.FilePath, spec.ClassicDiff, format)
		}

		if err != nil {
			fmt.Printf("Warning: Failed to get preview for %s: %v\n", spec.FilePath, err)
			continue
		}

		// Display preview
		fmt.Println(preview)

		if !dryRun {
			// Parse adjustments for this file
			adjustments, err := models.ParseDiffSpecWithAutoDetection(spec.ClassicDiff)
			if err != nil {
				fmt.Printf("Warning: Failed to parse diff spec for %s: %v\n", spec.FilePath, err)
				continue
			}

			// Apply adjustments for this file
			var fileAdjusted, fileOrphaned, fileWarnings int
			if parsed.IsPR() {
				fileAdjusted, fileOrphaned, fileWarnings, err = ch.applyPRAdjustmentsWithCounts(repository, parsed.PRNumber, spec.FilePath, adjustments, force)
			} else {
				fileAdjusted, fileOrphaned, fileWarnings, err = ch.applyBranchAdjustmentsWithCounts(repository, parsed.BranchName, spec.FilePath, adjustments, force)
			}

			if err != nil {
				fmt.Printf("Warning: Failed to apply adjustments for %s: %v\n", spec.FilePath, err)
				continue
			}

			totalAdjusted += fileAdjusted
			totalOrphaned += fileOrphaned
			totalWarnings += fileWarnings
		}

		fmt.Println()
	}

	if dryRun {
		fmt.Println("\n[DRY RUN] No changes were made.")
	} else {
		fmt.Printf("\nOverall adjustment summary:\n")
		fmt.Printf("- %d comments adjusted across %d files\n", totalAdjusted, len(fileSpecs))
		if totalOrphaned > 0 {
			fmt.Printf("- %d comments orphaned\n", totalOrphaned)
		}
		if totalWarnings > 0 {
			fmt.Printf("- %d warnings\n", totalWarnings)
		}
	}

	return nil
}

// getBranchAdjustmentPreview generates a preview of the adjustments for a branch.
func (ch *CommandHandler) getBranchAdjustmentPreview(repository models.Repository, branchName, file, diffSpec, format string) (string, error) {
	// Parse adjustments with auto-detection
	adjustments, err := models.ParseDiffSpecWithAutoDetection(diffSpec)
	if err != nil {
		return "", fmt.Errorf("failed to parse diff spec: %w", err)
	}

	// Get comments for the file
	branchComments, err := ch.storage.GetBranchComments(repository, branchName)
	if err != nil {
		return "", fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Get diff hunks
	branchDiffHunks, err := ch.storage.GetBranchDiffHunks(repository, branchName)
	if err != nil {
		return "", fmt.Errorf("failed to get branch diff hunks: %w", err)
	}

	preview := AdjustmentPreview{
		Repository:  repository,
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

		// Test adjustment
		testComment := comment
		if models.AdjustComment(&testComment, adjustments) {
			change.NewLine = testComment.Line
			change.Status = "adjusted"

			// Validate against diff hunks
			if err := models.ValidateAdjustmentAgainstDiff(testComment, adjustments, branchDiffHunks.DiffHunks); err != nil {
				change.Status = "warning"
				change.Warning = err.Error()
				preview.Warnings = append(preview.Warnings, fmt.Sprintf("Comment %s: %s", change.CommentIDShort, err.Error()))
			}
		} else {
			change.Status = "deleted"
			change.NewLine = models.LineRange{StartLine: -1, EndLine: -1}
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
func (ch *CommandHandler) getAdjustmentPreview(repository models.Repository, prNumber int, file, diffSpec, format string) (string, error) {
	// Parse adjustments with auto-detection
	adjustments, err := models.ParseDiffSpecWithAutoDetection(diffSpec)
	if err != nil {
		return "", fmt.Errorf("failed to parse diff spec: %w", err)
	}

	// Get comments for the file
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return "", fmt.Errorf("failed to get comments: %w", err)
	}

	// Get diff hunks
	prDiffHunks, err := ch.storage.GetDiffHunks(repository, prNumber)
	if err != nil {
		return "", fmt.Errorf("failed to get diff hunks: %w", err)
	}

	preview := AdjustmentPreview{
		Repository:  repository,
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

		// Test adjustment
		testComment := comment
		if models.AdjustComment(&testComment, adjustments) {
			change.NewLine = testComment.Line
			change.Status = "adjusted"

			// Validate against diff hunks
			if err := models.ValidateAdjustmentAgainstDiff(testComment, adjustments, prDiffHunks.DiffHunks); err != nil {
				change.Status = "warning"
				change.Warning = err.Error()
				preview.Warnings = append(preview.Warnings, fmt.Sprintf("Comment %s: %s", change.CommentIDShort, err.Error()))
			}
		} else {
			change.Status = "deleted"
			change.NewLine = models.LineRange{StartLine: -1, EndLine: -1}
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
func (ch *CommandHandler) applyPRAdjustments(repository models.Repository, prNumber int, file string, adjustments []models.LineAdjustment, force bool) error {
	// Get all comments
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Get diff hunks for validation
	prDiffHunks, err := ch.storage.GetDiffHunks(repository, prNumber)
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
				originalLine := comment.Line.EndLine
				if comment.OriginalRange != nil {
					originalLine = comment.OriginalRange.EndLine
				}
				comment.Body = fmt.Sprintf("[ORPHANED - Original line %d deleted]\n%s", originalLine, comment.Body)
				updatedComments = append(updatedComments, comment)
			}
		}
	}

	// Update storage
	prComments.Comments = updatedComments
	prComments.UpdatedAt = time.Now()

	if err := ch.storage.UpdateComments(repository, prNumber, prComments); err != nil {
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
func (ch *CommandHandler) applyBranchAdjustments(repository models.Repository, branchName, file string, adjustments []models.LineAdjustment, force bool) error {
	// Get all comments
	branchComments, err := ch.storage.GetBranchComments(repository, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Get diff hunks for validation
	branchDiffHunks, err := ch.storage.GetBranchDiffHunks(repository, branchName)
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
				originalLine := comment.Line.EndLine
				if comment.OriginalRange != nil {
					originalLine = comment.OriginalRange.EndLine
				}
				comment.Body = fmt.Sprintf("[ORPHANED - Original line %d deleted]\n%s", originalLine, comment.Body)
				updatedComments = append(updatedComments, comment)
			}
		}
	}

	// Update storage
	branchComments.Comments = updatedComments
	branchComments.UpdatedAt = time.Now()

	if err := ch.storage.UpdateBranchComments(repository, branchName, branchComments); err != nil {
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
	fmt.Fprintf(&sb, "Adjustment Preview for %s#%s %s\n", preview.Repository, preview.Identifier, preview.File)
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
		lineStr := formatLineChange(change.OriginalLine, change.NewLine)
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
func formatLineChange(originalLine, newLine models.LineRange) string {
	orig := originalLine.String()

	var adjusted string
	if newLine.EndLine == -1 {
		adjusted = "deleted"
	} else {
		adjusted = newLine.String()
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

// applyPRAdjustmentsWithCounts applies adjustments to PR comments and returns counts.
func (ch *CommandHandler) applyPRAdjustmentsWithCounts(repository models.Repository, prNumber int, file string, adjustments []models.LineAdjustment, force bool) (int, int, int, error) {
	// Get all comments
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to get comments: %w", err)
	}

	// Get diff hunks for validation
	prDiffHunks, err := ch.storage.GetDiffHunks(repository, prNumber)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to get diff hunks: %w", err)
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
					return 0, 0, 0, fmt.Errorf("comment %s: %w (use --force to override)", comment.FormatIDShort(), err)
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
				originalLine := comment.Line.EndLine
				if comment.OriginalRange != nil {
					originalLine = comment.OriginalRange.EndLine
				}
				comment.Body = fmt.Sprintf("[ORPHANED - Original line %d deleted]\n%s", originalLine, comment.Body)
				updatedComments = append(updatedComments, comment)
			}
		}
	}

	// Update storage
	prComments.Comments = updatedComments
	prComments.UpdatedAt = time.Now()

	if err := ch.storage.UpdateComments(repository, prNumber, prComments); err != nil {
		return 0, 0, 0, fmt.Errorf("failed to update comments: %w", err)
	}

	return adjustedCount, orphanedCount, warningCount, nil
}

// applyBranchAdjustmentsWithCounts applies adjustments to branch comments and returns counts.
func (ch *CommandHandler) applyBranchAdjustmentsWithCounts(repository models.Repository, branchName, file string, adjustments []models.LineAdjustment, force bool) (int, int, int, error) {
	// Get all comments
	branchComments, err := ch.storage.GetBranchComments(repository, branchName)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Get diff hunks for validation
	branchDiffHunks, err := ch.storage.GetBranchDiffHunks(repository, branchName)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to get branch diff hunks: %w", err)
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
					return 0, 0, 0, fmt.Errorf("comment %s: %w (use --force to override)", comment.FormatIDShort(), err)
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
				originalLine := comment.Line.EndLine
				if comment.OriginalRange != nil {
					originalLine = comment.OriginalRange.EndLine
				}
				comment.Body = fmt.Sprintf("[ORPHANED - Original line %d deleted]\n%s", originalLine, comment.Body)
				updatedComments = append(updatedComments, comment)
			}
		}
	}

	// Update storage
	branchComments.Comments = updatedComments
	branchComments.UpdatedAt = time.Now()

	if err := ch.storage.UpdateBranchComments(repository, branchName, branchComments); err != nil {
		return 0, 0, 0, fmt.Errorf("failed to update branch comments: %w", err)
	}

	return adjustedCount, orphanedCount, warningCount, nil
}

// parseMappingFile parses a mapping file and returns entries grouped by file.
func parseMappingFile(filePath string) (map[string][]MappingFileEntry, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to open mapping file: %w", err)
	}
	defer file.Close()

	// Regular expression to match mapping file entries: file:line:offset
	mappingRegex := regexp.MustCompile(`^([^:]+):(\d+):([-+]?\d+)\s*$`)

	entries := make(map[string][]MappingFileEntry)
	scanner := bufio.NewScanner(file)
	lineNum := 0

	for scanner.Scan() {
		lineNum++
		line := strings.TrimSpace(scanner.Text())

		// Skip empty lines and comments
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		matches := mappingRegex.FindStringSubmatch(line)
		if matches == nil {
			return nil, fmt.Errorf("invalid mapping format at line %d: %s (expected: file:line:offset)", lineNum, line)
		}

		filePath := matches[1]
		lineInt, err := strconv.Atoi(matches[2])
		if err != nil {
			return nil, fmt.Errorf("invalid line number at line %d: %s", lineNum, matches[2])
		}

		offset, err := strconv.Atoi(matches[3])
		if err != nil {
			return nil, fmt.Errorf("invalid offset at line %d: %s", lineNum, matches[3])
		}

		entry := MappingFileEntry{
			FilePath: filePath,
			Line:     lineInt,
			Offset:   offset,
		}

		entries[filePath] = append(entries[filePath], entry)
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading mapping file: %w", err)
	}

	if len(entries) == 0 {
		return nil, fmt.Errorf("no valid entries found in mapping file")
	}

	return entries, nil
}

// convertMappingEntriesToDiffSpec converts mapping entries for a file to a simple mapping diff spec.
func convertMappingEntriesToDiffSpec(entries []MappingFileEntry) string {
	var parts []string
	for _, entry := range entries {
		if entry.Offset >= 0 {
			parts = append(parts, fmt.Sprintf("%d:+%d", entry.Line, entry.Offset))
		} else {
			parts = append(parts, fmt.Sprintf("%d:%d", entry.Line, entry.Offset))
		}
	}
	return strings.Join(parts, ";")
}

// promptUser prompts the user for confirmation and returns their response.
func promptUser(prompt string) (string, error) {
	fmt.Print(prompt)
	scanner := bufio.NewScanner(os.Stdin)
	if scanner.Scan() {
		return strings.TrimSpace(strings.ToLower(scanner.Text())), nil
	}
	if err := scanner.Err(); err != nil {
		return "", fmt.Errorf("failed to read user input: %w", err)
	}
	return "", nil
}

// AdjustCommandExtended provides extended adjust functionality with Phase 2 features.
func (ch *CommandHandler) AdjustCommandExtended(repository models.Repository, identifier, file, diffSpec string, opts AdjustOptionsExtended) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	// Handle auto-detect mode
	if opts.AutoDetect {
		return ch.adjustCommandAutoDetect(repository, *parsed, file, opts)
	}

	// Handle mapping file mode
	if opts.MappingFile != "" {
		return ch.adjustCommandMappingFile(repository, *parsed, opts)
	}

	// Handle all-files mode
	if opts.AllFiles {
		return ch.adjustCommandAllFiles(repository, *parsed, diffSpec, opts)
	}

	// Handle unified diff mode
	if models.DetectFormat(diffSpec) == models.FormatUnifiedDiff {
		return ch.adjustCommandUnifiedDiffExtended(repository, *parsed, file, diffSpec, opts)
	}

	// Traditional single-file processing with possible interactive mode
	return ch.adjustCommandSingleFileExtended(repository, *parsed, file, diffSpec, opts)
}

// adjustCommandMappingFile handles mapping file processing.
func (ch *CommandHandler) adjustCommandMappingFile(repository models.Repository, parsed models.ParsedIdentifier, opts AdjustOptionsExtended) error {
	// Parse mapping file
	mappingEntries, err := parseMappingFile(opts.MappingFile)
	if err != nil {
		return fmt.Errorf("failed to parse mapping file: %w", err)
	}

	fmt.Printf("Processing %d file(s) from mapping file %s:\n", len(mappingEntries), opts.MappingFile)
	for filePath := range mappingEntries {
		fmt.Printf("- %s\n", filePath)
	}
	fmt.Println()

	totalAdjusted := 0
	totalOrphaned := 0
	totalWarnings := 0

	for filePath, entries := range mappingEntries {
		fmt.Printf("=== Processing file: %s ===\n", filePath)

		// Convert mapping entries to diff spec
		diffSpec := convertMappingEntriesToDiffSpec(entries)

		// Process this file
		var fileAdjusted, fileOrphaned, fileWarnings int
		if parsed.IsPR() {
			fileAdjusted, fileOrphaned, fileWarnings, err = ch.processFileWithInteractive(repository, parsed.PRNumber, filePath, diffSpec, opts, true)
		} else {
			fileAdjusted, fileOrphaned, fileWarnings, err = ch.processFileWithInteractive(repository, 0, filePath, diffSpec, opts, false)
		}

		if err != nil {
			fmt.Printf("Warning: Failed to process file %s: %v\n", filePath, err)
			continue
		}

		totalAdjusted += fileAdjusted
		totalOrphaned += fileOrphaned
		totalWarnings += fileWarnings
		fmt.Println()
	}

	if opts.DryRun {
		fmt.Println("\n[DRY RUN] No changes were made.")
	} else {
		fmt.Printf("\nOverall adjustment summary:\n")
		fmt.Printf("- %d comments adjusted across %d files\n", totalAdjusted, len(mappingEntries))
		if totalOrphaned > 0 {
			fmt.Printf("- %d comments orphaned\n", totalOrphaned)
		}
		if totalWarnings > 0 {
			fmt.Printf("- %d warnings\n", totalWarnings)
		}
	}

	return nil
}

// adjustCommandAllFiles handles all-files batch processing.
func (ch *CommandHandler) adjustCommandAllFiles(repository models.Repository, parsed models.ParsedIdentifier, diffSpec string, opts AdjustOptionsExtended) error {
	files, err := ch.getAllFilesWithComments(repository, parsed)
	if err != nil {
		return err
	}

	if len(files) == 0 {
		fmt.Println("No files with comments found.")
		return nil
	}

	ch.printFileProcessingHeader(files)
	return ch.processAllFiles(repository, parsed, files, diffSpec, opts)
}

// getAllFilesWithComments retrieves all files that contain comments.
func (ch *CommandHandler) getAllFilesWithComments(repository models.Repository, parsed models.ParsedIdentifier) ([]string, error) {
	if parsed.IsPR() {
		prComments, err := ch.storage.GetComments(repository, parsed.PRNumber)
		if err != nil {
			return nil, fmt.Errorf("failed to get PR comments: %w", err)
		}
		return extractUniqueFilePaths(prComments.Comments), nil
	}

	branchComments, err := ch.storage.GetBranchComments(repository, parsed.BranchName)
	if err != nil {
		return nil, fmt.Errorf("failed to get branch comments: %w", err)
	}
	return extractUniqueFilePaths(branchComments.Comments), nil
}

// extractUniqueFilePaths extracts unique file paths from a list of comments.
func extractUniqueFilePaths(comments []models.Comment) []string {
	fileMap := make(map[string]bool)
	for _, comment := range comments {
		fileMap[comment.Path] = true
	}

	files := make([]string, 0, len(fileMap))
	for filePath := range fileMap {
		files = append(files, filePath)
	}
	return files
}

// printFileProcessingHeader prints the header for file processing.
func (ch *CommandHandler) printFileProcessingHeader(files []string) {
	fmt.Printf("Processing %d file(s) with comments:\n", len(files))
	for _, filePath := range files {
		fmt.Printf("- %s\n", filePath)
	}
	fmt.Println()
}

// processAllFiles processes all files and returns summary statistics.
func (ch *CommandHandler) processAllFiles(repository models.Repository, parsed models.ParsedIdentifier, files []string, diffSpec string, opts AdjustOptionsExtended) error {
	totalAdjusted := 0
	totalOrphaned := 0
	totalWarnings := 0

	for i, filePath := range files {
		fmt.Printf("=== Processing file %d/%d: %s ===\n", i+1, len(files), filePath)

		fileAdjusted, fileOrphaned, fileWarnings, err := ch.processSingleFileInBatch(repository, parsed, filePath, diffSpec, opts)
		if err != nil {
			fmt.Printf("Warning: Failed to process file %s: %v\n", filePath, err)
			continue
		}

		totalAdjusted += fileAdjusted
		totalOrphaned += fileOrphaned
		totalWarnings += fileWarnings
		fmt.Println()
	}

	ch.printBatchSummary(opts.DryRun, totalAdjusted, totalOrphaned, totalWarnings, len(files))
	return nil
}

// processSingleFileInBatch processes a single file as part of batch processing.
func (ch *CommandHandler) processSingleFileInBatch(repository models.Repository, parsed models.ParsedIdentifier, filePath, diffSpec string, opts AdjustOptionsExtended) (int, int, int, error) {
	if parsed.IsPR() {
		return ch.processFileWithInteractive(repository, parsed.PRNumber, filePath, diffSpec, opts, true)
	}
	return ch.processFileWithInteractive(repository, 0, filePath, diffSpec, opts, false)
}

// printBatchSummary prints the summary of batch processing results.
func (ch *CommandHandler) printBatchSummary(dryRun bool, totalAdjusted, totalOrphaned, totalWarnings, fileCount int) {
	if dryRun {
		fmt.Println("\n[DRY RUN] No changes were made.")
		return
	}

	fmt.Printf("\nOverall adjustment summary:\n")
	fmt.Printf("- %d comments adjusted across %d files\n", totalAdjusted, fileCount)
	if totalOrphaned > 0 {
		fmt.Printf("- %d comments orphaned\n", totalOrphaned)
	}
	if totalWarnings > 0 {
		fmt.Printf("- %d warnings\n", totalWarnings)
	}
}

// adjustCommandSingleFileExtended handles single-file processing with extended options.
func (ch *CommandHandler) adjustCommandSingleFileExtended(repository models.Repository, parsed models.ParsedIdentifier, file, diffSpec string, opts AdjustOptionsExtended) error {
	// Parse diff spec with auto-detection
	adjustments, err := models.ParseDiffSpecWithAutoDetection(diffSpec)
	if err != nil {
		return fmt.Errorf("invalid diff spec: %w", err)
	}

	// Get preview
	var preview string
	if parsed.IsPR() {
		preview, err = ch.getAdjustmentPreview(repository, parsed.PRNumber, file, diffSpec, opts.Format)
	} else {
		preview, err = ch.getBranchAdjustmentPreview(repository, parsed.BranchName, file, diffSpec, opts.Format)
	}
	if err != nil {
		return err
	}

	// Display preview
	fmt.Println(preview)

	// If dry run, stop here
	if opts.DryRun {
		fmt.Println("\n[DRY RUN] No changes were made.")
		return nil
	}

	// Interactive confirmation if requested
	if opts.Interactive {
		response, err := promptUser("Apply these adjustments? (y/n): ")
		if err != nil {
			return err
		}
		if response != "y" && response != "yes" {
			fmt.Println("Adjustments cancelled.")
			return nil
		}
	}

	// Apply adjustments
	if parsed.IsPR() {
		return ch.applyPRAdjustments(repository, parsed.PRNumber, file, adjustments, opts.Force)
	}
	return ch.applyBranchAdjustments(repository, parsed.BranchName, file, adjustments, opts.Force)
}

// adjustCommandUnifiedDiffExtended handles unified diff processing with extended options.
func (ch *CommandHandler) adjustCommandUnifiedDiffExtended(repository models.Repository, parsed models.ParsedIdentifier, file, diffSpec string, opts AdjustOptionsExtended) error {
	if file != "" {
		// Single-file mode: filter unified diff for specific file
		filteredDiff, err := models.FilterUnifiedDiffForFile(diffSpec, file)
		if err != nil {
			return fmt.Errorf("failed to filter unified diff for file %s: %w", file, err)
		}
		return ch.adjustCommandSingleFileExtended(repository, parsed, file, filteredDiff, opts)
	}

	// Multi-file mode: process all files in the unified diff
	fileSpecs, err := models.ConvertUnifiedDiffToMultiFileClassicDiff(diffSpec)
	if err != nil {
		return fmt.Errorf("failed to parse unified diff: %w", err)
	}

	fmt.Printf("Processing %d file(s) from unified diff:\n", len(fileSpecs))
	for _, spec := range fileSpecs {
		fmt.Printf("- %s\n", spec.FilePath)
	}
	fmt.Println()

	totalAdjusted := 0
	totalOrphaned := 0
	totalWarnings := 0

	for i, spec := range fileSpecs {
		fmt.Printf("=== Processing file %d/%d: %s ===\n", i+1, len(fileSpecs), spec.FilePath)

		// Process this file
		var fileAdjusted, fileOrphaned, fileWarnings int
		if parsed.IsPR() {
			fileAdjusted, fileOrphaned, fileWarnings, err = ch.processFileWithInteractive(repository, parsed.PRNumber, spec.FilePath, spec.ClassicDiff, opts, true)
		} else {
			fileAdjusted, fileOrphaned, fileWarnings, err = ch.processFileWithInteractive(repository, 0, spec.FilePath, spec.ClassicDiff, opts, false)
		}

		if err != nil {
			fmt.Printf("Warning: Failed to process file %s: %v\n", spec.FilePath, err)
			continue
		}

		totalAdjusted += fileAdjusted
		totalOrphaned += fileOrphaned
		totalWarnings += fileWarnings
		fmt.Println()
	}

	if opts.DryRun {
		fmt.Println("\n[DRY RUN] No changes were made.")
	} else {
		fmt.Printf("\nOverall adjustment summary:\n")
		fmt.Printf("- %d comments adjusted across %d files\n", totalAdjusted, len(fileSpecs))
		if totalOrphaned > 0 {
			fmt.Printf("- %d comments orphaned\n", totalOrphaned)
		}
		if totalWarnings > 0 {
			fmt.Printf("- %d warnings\n", totalWarnings)
		}
	}

	return nil
}

// processFileWithInteractive processes a single file with optional interactive confirmation.
func (ch *CommandHandler) processFileWithInteractive(repository models.Repository, prNumber int, file, diffSpec string, opts AdjustOptionsExtended, isPR bool) (int, int, int, error) {
	// Parse adjustments
	adjustments, err := models.ParseDiffSpecWithAutoDetection(diffSpec)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to parse diff spec: %w", err)
	}

	// Get and display preview
	var preview string
	if isPR {
		preview, err = ch.getAdjustmentPreview(repository, prNumber, file, diffSpec, opts.Format)
	} else {
		// For branch, we need to pass the branch name instead of prNumber
		// This is a bit of a hack - we should refactor this
		return 0, 0, 0, fmt.Errorf("branch processing not fully implemented in interactive mode")
	}

	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to get preview: %w", err)
	}

	fmt.Println(preview)

	if opts.DryRun {
		return 0, 0, 0, nil
	}

	// Interactive confirmation if requested
	if opts.Interactive {
		response, err := promptUser("Apply adjustments for this file? (y/n/s/q): ")
		if err != nil {
			return 0, 0, 0, err
		}
		switch response {
		case "q", "quit":
			return 0, 0, 0, fmt.Errorf("user quit")
		case "s", "skip":
			fmt.Println("Skipping this file.")
			return 0, 0, 0, nil
		case "y", "yes":
			// Continue with processing
		default:
			fmt.Println("Skipping this file.")
			return 0, 0, 0, nil
		}
	}

	// Apply adjustments
	if isPR {
		return ch.applyPRAdjustmentsWithCounts(repository, prNumber, file, adjustments, opts.Force)
	}
	// Branch processing would go here
	return 0, 0, 0, fmt.Errorf("branch processing not implemented")
}

// adjustCommandAutoDetect handles auto-detection mode.
func (ch *CommandHandler) adjustCommandAutoDetect(repository models.Repository, parsed models.ParsedIdentifier, file string, opts AdjustOptionsExtended) error {
	if file == "" {
		return fmt.Errorf("file path is required for auto-detection mode")
	}

	// Get stored diff hunks to compare against
	storedDiffHunks, err := ch.getStoredDiffHunks(repository, parsed)
	if err != nil {
		return err
	}

	if len(storedDiffHunks) == 0 {
		return fmt.Errorf("no stored diff hunks found. Please run 'gh-pr-review capture' first")
	}

	// Perform auto-detection
	result, err := ch.gitClient.AutoDetectChanges(file, storedDiffHunks)
	if err != nil {
		return fmt.Errorf("failed to auto-detect changes: %w", err)
	}

	// Display auto-detection results
	ch.displayAutoDetectionResults(file, result)

	// Check confidence and handle low confidence cases
	if err := ch.handleLowConfidenceDetection(result, opts); err != nil {
		return err
	}

	// Convert suggestions to diff spec and apply
	return ch.applyAutoDetectedChanges(repository, parsed, file, result, opts)
}

// getStoredDiffHunks retrieves stored diff hunks for either PR or branch.
func (ch *CommandHandler) getStoredDiffHunks(repository models.Repository, parsed models.ParsedIdentifier) ([]models.DiffHunk, error) {
	if parsed.IsPR() {
		prDiffHunks, err := ch.storage.GetDiffHunks(repository, parsed.PRNumber)
		if err != nil {
			return nil, fmt.Errorf("failed to get stored diff hunks for PR %d: %w", parsed.PRNumber, err)
		}
		return prDiffHunks.DiffHunks, nil
	}

	branchDiffHunks, err := ch.storage.GetBranchDiffHunks(repository, parsed.BranchName)
	if err != nil {
		return nil, fmt.Errorf("failed to get stored diff hunks for branch %s: %w", parsed.BranchName, err)
	}
	return branchDiffHunks.DiffHunks, nil
}

// displayAutoDetectionResults shows the auto-detection results to the user.
func (ch *CommandHandler) displayAutoDetectionResults(file string, result *models.AutoDetectResult) {
	fmt.Printf("Auto-detected changes in %s:\n", file)
	fmt.Printf("Overall confidence: %s\n", result.Confidence)
	fmt.Printf("Detected %d line changes\n", len(result.Changes))
	fmt.Printf("Generated %d mapping suggestions\n\n", len(result.Suggestions))

	ch.displayDetectedChanges(result.Changes)
	ch.displayMappingSuggestions(result.Suggestions)
}

// displayDetectedChanges shows the detected line changes.
func (ch *CommandHandler) displayDetectedChanges(changes []models.LineChange) {
	if len(changes) == 0 {
		return
	}

	fmt.Println("Detected changes:")
	for _, change := range changes {
		switch change.Type {
		case models.LineAdded:
			fmt.Printf("  + Line added at %d\n", change.NewLine)
		case models.LineDeleted:
			fmt.Printf("  - Line deleted from %d\n", change.OriginalLine)
		case models.LineChanged:
			fmt.Printf("  ~ Line %d changed to %d\n", change.OriginalLine, change.NewLine)
		}
	}
	fmt.Println()
}

// displayMappingSuggestions shows the mapping suggestions.
func (ch *CommandHandler) displayMappingSuggestions(suggestions []models.MappingSuggestion) {
	if len(suggestions) == 0 {
		return
	}

	fmt.Println("Suggested adjustments:")
	for _, suggestion := range suggestions {
		var offsetStr string
		if suggestion.Offset >= 0 {
			offsetStr = fmt.Sprintf("+%d", suggestion.Offset)
		} else {
			offsetStr = fmt.Sprintf("%d", suggestion.Offset)
		}
		fmt.Printf("  Line %d: %s offset (%s confidence) - %s\n",
			suggestion.OriginalLine, offsetStr, suggestion.Confidence, suggestion.Reason)
	}
	fmt.Println()
}

// handleLowConfidenceDetection handles cases where auto-detection has low confidence.
func (ch *CommandHandler) handleLowConfidenceDetection(result *models.AutoDetectResult, opts AdjustOptionsExtended) error {
	if result.Confidence != models.ConfidenceLow {
		return nil
	}

	fmt.Println("⚠ Low confidence in auto-detection. Manual review recommended.")
	if opts.DryRun {
		fmt.Println("[DRY RUN] No changes were made.")
		return fmt.Errorf("dry run completed")
	}
	if !opts.Force {
		return fmt.Errorf("low confidence auto-detection (use --force to apply anyway)")
	}
	return nil
}

// applyAutoDetectedChanges converts suggestions to diff spec and applies them.
func (ch *CommandHandler) applyAutoDetectedChanges(repository models.Repository, parsed models.ParsedIdentifier, file string, result *models.AutoDetectResult, opts AdjustOptionsExtended) error {
	diffSpec := result.ConvertToSimpleMappingSpec()
	if diffSpec == "" {
		fmt.Println("No adjustments needed - file appears unchanged.")
		return nil
	}

	fmt.Printf("Generated diff spec: %s\n\n", diffSpec)

	// Use existing adjustment logic with the auto-generated diff spec
	return ch.adjustCommandSingleFileExtended(repository, parsed, file, diffSpec, opts)
}
