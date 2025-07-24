package git

import (
	"fmt"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// Client provides git operations for branch diff capture.
type Client struct {
	repoPath string
}

// validBranchNamePattern defines what constitutes a valid git branch name.
// This follows git's branch naming rules to prevent command injection.
var validBranchNamePattern = regexp.MustCompile(`^[a-zA-Z0-9][a-zA-Z0-9._/-]*[a-zA-Z0-9]$|^[a-zA-Z0-9]$`)

// validateBranchName ensures a branch name is safe to use in git commands.
func validateBranchName(branchName string) error {
	if branchName == "" {
		return fmt.Errorf("branch name cannot be empty")
	}

	// Check for dangerous characters and patterns
	if strings.Contains(branchName, "..") {
		return fmt.Errorf("branch name cannot contain '..'")
	}
	if strings.HasPrefix(branchName, "-") {
		return fmt.Errorf("branch name cannot start with '-'")
	}
	if strings.HasSuffix(branchName, ".") {
		return fmt.Errorf("branch name cannot end with '.'")
	}
	if strings.Contains(branchName, " ") {
		return fmt.Errorf("branch name cannot contain spaces")
	}
	if strings.Contains(branchName, "\t") || strings.Contains(branchName, "\n") || strings.Contains(branchName, "\r") {
		return fmt.Errorf("branch name cannot contain whitespace characters")
	}

	// Validate against the pattern
	if !validBranchNamePattern.MatchString(branchName) {
		return fmt.Errorf("invalid branch name: %q", branchName)
	}

	return nil
}

// NewClient creates a new git client for the specified repository path.
func NewClient(repoPath string) *Client {
	return &Client{
		repoPath: repoPath,
	}
}

// CaptureBranchDiff captures the diff hunks for a branch compared to its base branch.
func (gc *Client) CaptureBranchDiff(owner, repo, branchName, baseBranch string) (*models.BranchDiffHunks, error) {
	// Validate branch names for security
	if err := validateBranchName(branchName); err != nil {
		return nil, fmt.Errorf("invalid branch name %q: %w", branchName, err)
	}
	if err := validateBranchName(baseBranch); err != nil {
		return nil, fmt.Errorf("invalid base branch name %q: %w", baseBranch, err)
	}

	// Get current commit SHA
	commitSHA, err := gc.getCommitSHA(branchName)
	if err != nil {
		return nil, fmt.Errorf("failed to get commit SHA for branch %s: %w", branchName, err)
	}

	// Get base commit SHA
	baseSHA, err := gc.getCommitSHA(baseBranch)
	if err != nil {
		return nil, fmt.Errorf("failed to get commit SHA for base branch %s: %w", baseBranch, err)
	}

	// Get diff output with unified context
	diffOutput, err := gc.getDiffOutput(baseBranch, branchName)
	if err != nil {
		return nil, fmt.Errorf("failed to get diff output: %w", err)
	}

	// Parse diff into hunks
	diffHunks, err := gc.parseDiffOutput(diffOutput, commitSHA, baseSHA)
	if err != nil {
		return nil, fmt.Errorf("failed to parse diff output: %w", err)
	}

	return &models.BranchDiffHunks{
		BranchName:  branchName,
		Owner:       owner,
		Repo:        repo,
		CapturedAt:  time.Now(),
		DiffHunks:   diffHunks,
		CommitSHA:   commitSHA,
		BaseSHA:     baseSHA,
		BaseBranch:  baseBranch,
		Description: fmt.Sprintf("Diff between %s and %s", baseBranch, branchName),
	}, nil
}

// getCommitSHA gets the commit SHA for a given branch.
func (gc *Client) getCommitSHA(branch string) (string, error) {
	// Branch name is already validated by the caller
	cmd := exec.Command("git", "rev-parse", branch)
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get commit SHA: %w", err)
	}

	return strings.TrimSpace(string(output)), nil
}

// getDiffOutput gets the raw diff output between two branches.
func (gc *Client) getDiffOutput(baseBranch, targetBranch string) (string, error) {
	// Branch names are validated by the caller via validateBranchName() - safe to use in git command.
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
func (gc *Client) GetDefaultBaseBranch() (string, error) {
	// Try to get the default branch from git
	cmd := exec.Command("git", "symbolic-ref", "refs/remotes/origin/HEAD")
	cmd.Dir = gc.repoPath

	output, err := cmd.Output()
	if err == nil {
		// Parse output like "refs/remotes/origin/main"
		defaultRef := strings.TrimSpace(string(output))
		parts := strings.Split(defaultRef, "/")
		if len(parts) >= 3 {
			return parts[len(parts)-1], nil
		}
	}

	// Fallback: check if main or master exists
	for _, branch := range []string{"main", "master"} {
		// #nosec G204 - git command with hardcoded branch names
		cmd := exec.Command("git", "rev-parse", "--verify", fmt.Sprintf("origin/%s", branch))
		cmd.Dir = gc.repoPath
		if err := cmd.Run(); err == nil {
			return branch, nil
		}
	}

	return "", fmt.Errorf("could not determine default base branch")
}

// BranchExists checks if a branch exists in the repository.
func (gc *Client) BranchExists(branchName string) (bool, error) {
	// Validate branch name for security
	if err := validateBranchName(branchName); err != nil {
		return false, fmt.Errorf("invalid branch name %q: %w", branchName, err)
	}

	cmd := exec.Command("git", "rev-parse", "--verify", branchName)
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
