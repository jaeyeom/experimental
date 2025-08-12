// Package git provides functionality for interacting with Git repositories,
// specifically focused on capturing and analyzing branch diffs.
//
// The package offers secure Git operations with branch name validation to prevent
// command injection, diff parsing capabilities, and automatic change detection
// between stored diff hunks and current file states. It supports both manual
// and automatic line adjustment mapping for code review comments.
//
// Key features:
//   - Secure branch name validation to prevent command injection
//   - Branch diff capture with hunk parsing
//   - Automatic change detection and line mapping suggestions
//   - Support for detecting additions, deletions, and modifications
//   - Confidence scoring for automatic mapping suggestions
package git

import (
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// Client provides git operations for branch diff capture.
type Client struct {
	repoPath string
}

// NewClient creates a new git client for the specified repository path.
func NewClient(repoPath string) *Client {
	return &Client{
		repoPath: repoPath,
	}
}

// CaptureBranchDiff captures the diff hunks for a branch compared to its base branch.
func (gc *Client) CaptureBranchDiff(repository models.Repository, branch, baseBranch Branch) (*models.BranchDiffHunks, error) {
	// Get current commit SHA
	commitSHA, err := gc.getCommitSHA(branch)
	if err != nil {
		return nil, fmt.Errorf("failed to get commit SHA for branch %s: %w", branch, err)
	}

	// Get base commit SHA
	baseSHA, err := gc.getCommitSHA(baseBranch)
	if err != nil {
		return nil, fmt.Errorf("failed to get commit SHA for base branch %s: %w", baseBranch, err)
	}

	// Get diff output with unified context
	diffOutput, err := gc.getDiffOutput(baseBranch, branch)
	if err != nil {
		return nil, fmt.Errorf("failed to get diff output: %w", err)
	}

	// Parse diff into hunks
	diffHunks, err := gc.parseDiffOutput(diffOutput, commitSHA, baseSHA)
	if err != nil {
		return nil, fmt.Errorf("failed to parse diff output: %w", err)
	}

	return &models.BranchDiffHunks{
		BranchName:  branch.String(),
		Repository:  repository,
		CapturedAt:  time.Now(),
		DiffHunks:   diffHunks,
		CommitSHA:   commitSHA,
		BaseSHA:     baseSHA,
		BaseBranch:  baseBranch.String(),
		Description: fmt.Sprintf("Diff between %s and %s", baseBranch, branch),
	}, nil
}

// getCommitSHA gets the commit SHA for a given branch.
func (gc *Client) getCommitSHA(branch Branch) (string, error) {
	// Branch is already validated during construction
	cmd := exec.Command("git", "rev-parse", branch.String()) // #nosec G204
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get commit SHA: %w", err)
	}

	return strings.TrimSpace(string(output)), nil
}

// GetStagedDiff gets the diff of staged changes.
func (gc *Client) GetStagedDiff() (string, error) {
	cmd := exec.Command("git", "diff", "--cached", "--unified=3")
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get staged diff: %w", err)
	}

	return string(output), nil
}

// GetDiffSince gets the diff since a specific commit.
func (gc *Client) GetDiffSince(since string) (string, error) {
	cmd := exec.Command("git", "diff", "--unified=3", since)
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get diff since %s: %w", since, err)
	}

	return string(output), nil
}

// getDiffOutput gets the raw diff output between two branches.
func (gc *Client) getDiffOutput(baseBranch, targetBranch Branch) (string, error) {
	// Branches are already validated during construction - safe to use in git command.
	cmd := exec.Command("git", "diff", "--unified=3", fmt.Sprintf("%s...%s", baseBranch, targetBranch)) // #nosec G204
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get diff output: %w", err)
	}

	return string(output), nil
}

// parseDiffOutput parses git diff output into DiffHunk structures.
func (gc *Client) parseDiffOutput(diffOutput, commitSHA, baseSHA string) ([]models.DiffHunk, error) {
	lines := strings.Split(diffOutput, "\n")
	var hunks []models.DiffHunk

	var currentFile string
	var currentHunk *models.DiffHunk
	var hunkLines []string

	for _, line := range lines {
		// File header: diff --git a/file b/file
		if strings.HasPrefix(line, "diff --git") {
			// Save previous hunk if exists
			if currentHunk != nil {
				currentHunk.Content = strings.Join(hunkLines, "\n")
				hunks = append(hunks, *currentHunk)
			}

			// Extract filename
			parts := strings.Fields(line)
			if len(parts) >= 4 {
				currentFile = strings.TrimPrefix(parts[3], "b/")
			}
			currentHunk = nil
			hunkLines = []string{}
			continue
		}

		// Hunk header: @@ -old_start,old_count +new_start,new_count @@
		if strings.HasPrefix(line, "@@") && currentFile != "" {
			// Save previous hunk if exists
			if currentHunk != nil {
				currentHunk.Content = strings.Join(hunkLines, "\n")
				hunks = append(hunks, *currentHunk)
			}

			// Parse hunk header
			leftHunk, rightHunk, err := gc.parseHunkHeader(line, currentFile, commitSHA, baseSHA)
			if err != nil {
				continue // Skip malformed hunk headers
			}

			hunkLines = []string{line}

			// Create separate hunks for LEFT (deletions) and RIGHT (additions)
			if leftHunk != nil {
				hunks = append(hunks, *leftHunk)
			}
			if rightHunk != nil {
				currentHunk = rightHunk
			} else {
				currentHunk = nil
			}
			continue
		}

		// Content lines
		if currentHunk != nil {
			hunkLines = append(hunkLines, line)
		}
	}

	// Save final hunk
	if currentHunk != nil && len(hunkLines) > 0 {
		currentHunk.Content = strings.Join(hunkLines, "\n")
		hunks = append(hunks, *currentHunk)
	}

	return hunks, nil
}

// parseHunkHeader parses a hunk header line and returns LEFT and RIGHT hunks.
func (gc *Client) parseHunkHeader(header, file, commitSHA, baseSHA string) (*models.DiffHunk, *models.DiffHunk, error) {
	// Extract line range information: @@ -old_start,old_count +new_start,new_count @@
	parts := strings.Fields(header)
	if len(parts) < 3 {
		return nil, nil, fmt.Errorf("malformed hunk header: %s", header)
	}

	var leftHunk, rightHunk *models.DiffHunk

	// Parse old range (LEFT side - deletions)
	if strings.HasPrefix(parts[1], "-") {
		oldRange := strings.TrimPrefix(parts[1], "-")
		startLine, count, err := gc.parseRange(oldRange)
		if err == nil && count > 0 {
			leftHunk = &models.DiffHunk{
				File:      file,
				Side:      "LEFT",
				StartLine: startLine,
				EndLine:   startLine + count - 1,
				Content:   header,
				SHA:       baseSHA,
			}
		}
	}

	// Parse new range (RIGHT side - additions)
	if len(parts) >= 3 && strings.HasPrefix(parts[2], "+") {
		newRange := strings.TrimPrefix(parts[2], "+")
		startLine, count, err := gc.parseRange(newRange)
		if err == nil && count > 0 {
			rightHunk = &models.DiffHunk{
				File:      file,
				Side:      "RIGHT",
				StartLine: startLine,
				EndLine:   startLine + count - 1,
				Content:   header,
				SHA:       commitSHA,
			}
		}
	}

	return leftHunk, rightHunk, nil
}

// parseRange parses a range specification like "15,10" or "15" and returns start line and count.
func (gc *Client) parseRange(rangeSpec string) (int, int, error) {
	if strings.Contains(rangeSpec, ",") {
		parts := strings.Split(rangeSpec, ",")
		if len(parts) != 2 {
			return 0, 0, fmt.Errorf("invalid range format: %q", rangeSpec)
		}

		startLine, err := strconv.Atoi(parts[0])
		if err != nil {
			return 0, 0, fmt.Errorf("invalid start line: %w", err)
		}

		count, err := strconv.Atoi(parts[1])
		if err != nil {
			return 0, 0, fmt.Errorf("invalid count: %w", err)
		}

		return startLine, count, nil
	}

	// Single line number
	startLine, err := strconv.Atoi(rangeSpec)
	if err != nil {
		return 0, 0, fmt.Errorf("invalid line number: %w", err)
	}

	return startLine, 1, nil
}

// GetDefaultBaseBranch attempts to determine the default base branch (main or master).
func (gc *Client) GetDefaultBaseBranch() (Branch, error) {
	// Try to get the default branch from git
	cmd := exec.Command("git", "symbolic-ref", "refs/remotes/origin/HEAD")
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err == nil {
		// Parse output like "refs/remotes/origin/main"
		defaultRef := strings.TrimSpace(string(output))
		parts := strings.Split(defaultRef, "/")
		if len(parts) >= 3 {
			return NewBranch(parts[len(parts)-1])
		}
	}

	// Fallback: check if main or master exists
	for _, branchName := range []string{"main", "master"} {
		// #nosec G204 - git command with hardcoded branch names
		cmd := exec.Command("git", "rev-parse", "--verify", fmt.Sprintf("origin/%s", branchName))
		cmd.Dir = gc.repoPath
		if err := cmd.Run(); err == nil {
			return NewBranch(branchName)
		}
	}

	return Branch{}, fmt.Errorf("could not determine default base branch")
}

// BranchExists checks if a branch exists in the repository.
func (gc *Client) BranchExists(branch Branch) (bool, error) {
	// Branch is already validated during construction
	cmd := exec.Command("git", "rev-parse", "--verify", branch.String()) // #nosec G204
	cmd.Dir = gc.repoPath

	err := cmd.Run()
	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok && exitError.ExitCode() == 1 {
			return false, nil // Branch doesn't exist
		}
		return false, fmt.Errorf("failed to check if branch exists: %w", err)
	}

	return true, nil
}

// AutoDetectChanges analyzes differences between stored diff hunks and current file state
// to automatically generate line adjustment mappings.
func (gc *Client) AutoDetectChanges(file string, storedDiffHunks []models.DiffHunk) (*models.AutoDetectResult, error) {
	// Get current file diff compared to the original captured state
	currentDiff, err := gc.getFileDiffFromCapture(file, storedDiffHunks)
	if err != nil {
		return nil, fmt.Errorf("failed to get current file diff: %w", err)
	}

	// Parse the current diff to detect line changes
	changes, err := gc.parseCurrentDiff(currentDiff)
	if err != nil {
		return nil, fmt.Errorf("failed to parse current diff: %w", err)
	}

	// Generate automatic mapping suggestions with confidence scoring
	suggestions := gc.generateMappingSuggestions(changes, storedDiffHunks)

	return &models.AutoDetectResult{
		File:        file,
		Changes:     changes,
		Suggestions: suggestions,
		Confidence:  gc.calculateOverallConfidence(suggestions),
	}, nil
}

// getFileDiffFromCapture gets the diff for a specific file since the diff hunks were captured.
func (gc *Client) getFileDiffFromCapture(file string, storedDiffHunks []models.DiffHunk) (string, error) {
	// Find the commit SHA from stored diff hunks
	if len(storedDiffHunks) == 0 {
		return "", fmt.Errorf("no stored diff hunks available for comparison")
	}

	// Use the SHA from the first hunk as the baseline
	baseSHA := storedDiffHunks[0].SHA

	// Validate that the commit SHA still exists
	if err := gc.validateCommitExists(baseSHA); err != nil {
		return "", fmt.Errorf("stored commit %s no longer exists: %w", baseSHA, err)
	}

	// Check if the file exists in the working directory
	if err := gc.validateFileExists(file); err != nil {
		return "", fmt.Errorf("file validation failed: %w", err)
	}

	// Get diff from the stored commit to the current working directory
	cmd := exec.Command("git", "diff", baseSHA, "--", file)
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err != nil {
		// If the file doesn't exist or there are no changes, that's valid
		if exitError, ok := err.(*exec.ExitError); ok {
			// Exit code 1 typically means no changes or file not found
			if exitError.ExitCode() == 1 {
				return "", nil // No changes
			}
			return "", fmt.Errorf("git diff failed with exit code %d: %s",
				exitError.ExitCode(), string(exitError.Stderr))
		}
		return "", fmt.Errorf("failed to get file diff: %w", err)
	}

	return string(output), nil
}

// validateCommitExists checks if a commit SHA exists in the repository.
func (gc *Client) validateCommitExists(sha string) error {
	cmd := exec.Command("git", "cat-file", "-e", sha)
	cmd.Dir = gc.repoPath

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("commit does not exist or is not accessible")
	}
	return nil
}

// validateFileExists checks if a file exists in the current working directory.
func (gc *Client) validateFileExists(file string) error {
	cmd := exec.Command("git", "ls-files", "--", file)
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to check if file is tracked by git: %w", err)
	}

	if strings.TrimSpace(string(output)) == "" {
		return fmt.Errorf("file %s is not tracked by git", file)
	}

	return nil
}

// parseCurrentDiff parses the current diff output to detect specific line changes.
func (gc *Client) parseCurrentDiff(diffOutput string) ([]models.LineChange, error) {
	if diffOutput == "" {
		return []models.LineChange{}, nil // No changes
	}

	lines := strings.Split(diffOutput, "\n")
	var changes []models.LineChange

	var inHunk bool
	var oldLine, newLine int

	for _, line := range lines {
		// Hunk header: @@ -old_start,old_count +new_start,new_count @@
		if strings.HasPrefix(line, "@@") {
			inHunk = true

			// Parse hunk header to get starting line numbers
			parts := strings.Fields(line)
			if len(parts) >= 3 {
				if strings.HasPrefix(parts[1], "-") {
					oldRange := strings.TrimPrefix(parts[1], "-")
					if startLine, _, err := gc.parseRange(oldRange); err == nil {
						oldLine = startLine
					}
				}
				if strings.HasPrefix(parts[2], "+") {
					newRange := strings.TrimPrefix(parts[2], "+")
					if startLine, _, err := gc.parseRange(newRange); err == nil {
						newLine = startLine
					}
				}
			}
			continue
		}

		if !inHunk {
			continue
		}

		// Process diff content lines
		switch {
		case strings.HasPrefix(line, "-"):
			// Line deleted
			changes = append(changes, models.LineChange{
				Type:         models.LineDeleted,
				OriginalLine: oldLine,
				NewLine:      -1, // Deleted
			})
			oldLine++

		case strings.HasPrefix(line, "+"):
			// Line added
			changes = append(changes, models.LineChange{
				Type:         models.LineAdded,
				OriginalLine: oldLine,
				NewLine:      newLine,
			})
			newLine++

		case strings.HasPrefix(line, " "):
			// Context line (unchanged)
			oldLine++
			newLine++
		}
	}

	return changes, nil
}

// generateMappingSuggestions creates automatic mapping suggestions based on detected changes.
func (gc *Client) generateMappingSuggestions(changes []models.LineChange, storedDiffHunks []models.DiffHunk) []models.MappingSuggestion {
	if len(changes) == 0 {
		return []models.MappingSuggestion{}
	}

	var suggestions []models.MappingSuggestion

	// Group changes by line ranges to create more accurate suggestions
	changeGroups := gc.groupConsecutiveChanges(changes)

	for _, group := range changeGroups {
		suggestion := gc.createSuggestionFromGroup(group, storedDiffHunks)
		if suggestion != nil {
			suggestions = append(suggestions, *suggestion)
		}
	}

	// Validate suggestions don't conflict with stored diff hunks
	return gc.validateSuggestionsAgainstDiffHunks(suggestions, storedDiffHunks)
}

// ChangeGroup represents a group of consecutive line changes.
type ChangeGroup struct {
	StartLine int
	EndLine   int
	Changes   []models.LineChange
	NetOffset int // Net change in line count
}

// groupConsecutiveChanges groups consecutive line changes together.
func (gc *Client) groupConsecutiveChanges(changes []models.LineChange) []ChangeGroup {
	if len(changes) == 0 {
		return []ChangeGroup{}
	}

	var groups []ChangeGroup
	currentGroup := ChangeGroup{
		StartLine: changes[0].OriginalLine,
		Changes:   []models.LineChange{changes[0]},
	}

	for i := 1; i < len(changes); i++ {
		change := changes[i]

		// Calculate current group's end line based on existing changes
		currentEndLine := currentGroup.StartLine + len(currentGroup.Changes) - 1

		// If this change is consecutive or close to the previous group, add to current group
		if change.OriginalLine <= currentEndLine+2 {
			currentGroup.Changes = append(currentGroup.Changes, change)
		} else {
			// Finalize current group and start new one
			currentGroup.EndLine = currentEndLine
			currentGroup.NetOffset = gc.calculateNetOffset(currentGroup.Changes)
			groups = append(groups, currentGroup)

			currentGroup = ChangeGroup{
				StartLine: change.OriginalLine,
				Changes:   []models.LineChange{change},
			}
		}
	}

	// Finalize the last group
	currentGroup.EndLine = currentGroup.StartLine + len(currentGroup.Changes) - 1
	currentGroup.NetOffset = gc.calculateNetOffset(currentGroup.Changes)
	groups = append(groups, currentGroup)

	return groups
}

// calculateNetOffset calculates the net change in line count for a group of changes.
func (gc *Client) calculateNetOffset(changes []models.LineChange) int {
	netOffset := 0
	for _, change := range changes {
		switch change.Type {
		case models.LineAdded:
			netOffset++
		case models.LineDeleted:
			netOffset--
		}
	}
	return netOffset
}

// createSuggestionFromGroup creates a mapping suggestion from a change group.
func (gc *Client) createSuggestionFromGroup(group ChangeGroup, storedDiffHunks []models.DiffHunk) *models.MappingSuggestion {
	if group.NetOffset == 0 {
		// No net change, no adjustment needed
		return nil
	}

	confidence := models.ConfidenceHigh
	reason := fmt.Sprintf("Net %d line change detected in range %d-%d",
		group.NetOffset, group.StartLine, group.EndLine)

	// Lower confidence for complex change patterns
	if len(group.Changes) > 5 {
		confidence = models.ConfidenceMedium
		reason += " (complex change pattern)"
	}

	// Check if this change conflicts with existing diff hunks
	if gc.conflictsWithDiffHunks(group, storedDiffHunks) {
		confidence = models.ConfidenceLow
		reason += " (potential conflict with stored diff hunks)"
	}

	return &models.MappingSuggestion{
		OriginalLine: group.StartLine,
		Offset:       group.NetOffset,
		Confidence:   confidence,
		Reason:       reason,
	}
}

// conflictsWithDiffHunks checks if a change group conflicts with stored diff hunks.
func (gc *Client) conflictsWithDiffHunks(group ChangeGroup, storedDiffHunks []models.DiffHunk) bool {
	for _, hunk := range storedDiffHunks {
		// Check if the change group overlaps with any stored hunk
		if group.StartLine <= hunk.EndLine && group.EndLine >= hunk.StartLine {
			return true
		}
	}
	return false
}

// validateSuggestionsAgainstDiffHunks validates suggestions against stored diff hunks.
func (gc *Client) validateSuggestionsAgainstDiffHunks(suggestions []models.MappingSuggestion, _ []models.DiffHunk) []models.MappingSuggestion {
	var validSuggestions []models.MappingSuggestion

	for _, suggestion := range suggestions {
		// Check if suggestion line range is reasonable
		if suggestion.OriginalLine < 1 {
			continue // Invalid line number
		}

		// Check for unreasonably large offsets (likely indicates parsing error)
		if suggestion.Offset > 1000 || suggestion.Offset < -1000 {
			suggestion.Confidence = models.ConfidenceLow
			suggestion.Reason += " (unusually large offset - may be incorrect)"
		}

		validSuggestions = append(validSuggestions, suggestion)
	}

	return validSuggestions
}

// calculateOverallConfidence determines overall confidence based on individual suggestion confidence.
func (gc *Client) calculateOverallConfidence(suggestions []models.MappingSuggestion) models.ConfidenceLevel {
	if len(suggestions) == 0 {
		return models.ConfidenceLow
	}

	highCount := 0
	for _, suggestion := range suggestions {
		if suggestion.Confidence == models.ConfidenceHigh {
			highCount++
		}
	}

	// If majority are high confidence, overall is high
	if float64(highCount)/float64(len(suggestions)) >= 0.7 {
		return models.ConfidenceHigh
	}

	return models.ConfidenceMedium
}
