package prreview

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/git"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

// OutputFormatter defines the interface for formatting output.
type OutputFormatter interface {
	FormatSubmitResult(result models.SubmitResult) (string, error)
	FormatComments(comments []models.Comment) (string, error)
}

// CommandHandler handles gh-pr-review commands.
type CommandHandler struct {
	storage     *storage.GitHubStorage
	ghClient    *github.PRReviewClient
	gitClient   *git.Client
	storageHome string
}

// NewCommandHandler creates a new command handler.
func NewCommandHandler(storageHome string) (*CommandHandler, error) {
	// Initialize storage
	ghStorage, err := storage.NewGitHubStorage(storageHome)
	if err != nil {
		return nil, fmt.Errorf("failed to initialize storage: %w", err)
	}

	// Initialize GitHub client
	baseClient := github.NewClient(nil)
	prClient := github.NewPRReviewClient(baseClient)

	// Initialize git client (current working directory)
	wd, err := os.Getwd()
	if err != nil {
		return nil, fmt.Errorf("failed to get working directory: %w", err)
	}
	gitClient := git.NewClient(wd)

	return &CommandHandler{
		storage:     ghStorage,
		ghClient:    prClient,
		gitClient:   gitClient,
		storageHome: storageHome,
	}, nil
}

// CaptureCommand captures and stores diff hunks for either PR or branch.
func (ch *CommandHandler) CaptureCommand(owner, repo, identifier string, force bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.capturePRDiff(owner, repo, parsed.PRNumber, force)
	}
	return ch.captureBranchDiff(owner, repo, parsed.BranchName, force)
}

// capturePRDiff captures and stores PR diff hunks.
func (ch *CommandHandler) capturePRDiff(owner, repo string, prNumber int, force bool) error {
	// Check if diff hunks already exist
	if !force && ch.diffHunksExist(owner, repo, prNumber) {
		return fmt.Errorf("diff hunks already exist for PR %d, use --force to overwrite", prNumber)
	}

	// Validate PR access
	if err := ch.ghClient.ValidatePRAccess(owner, repo, prNumber); err != nil {
		return fmt.Errorf("failed to access PR %s/%s#%d: %w", owner, repo, prNumber, err)
	}

	// Fetch diff hunks from GitHub
	diffHunks, err := ch.ghClient.GetPRDiff(owner, repo, prNumber)
	if err != nil {
		return fmt.Errorf("failed to fetch diff hunks: %w", err)
	}

	// Store diff hunks
	if err := ch.storage.CaptureDiffHunks(owner, repo, prNumber, *diffHunks); err != nil {
		return fmt.Errorf("failed to store diff hunks: %w", err)
	}

	fmt.Printf("Captured diff hunks for PR %s/%s#%d (%d hunks)\n",
		owner, repo, prNumber, len(diffHunks.DiffHunks))
	return nil
}

// captureBranchDiff captures and stores branch diff hunks.
func (ch *CommandHandler) captureBranchDiff(owner, repo, branchName string, force bool) error {
	// Check if diff hunks already exist
	if !force && ch.branchDiffHunksExist(owner, repo, branchName) {
		return fmt.Errorf("diff hunks already exist for branch %q, use --force to overwrite", branchName)
	}

	// Check if branch exists
	exists, err := ch.gitClient.BranchExists(branchName)
	if err != nil {
		return fmt.Errorf("failed to check if branch exists: %w", err)
	}
	if !exists {
		return fmt.Errorf("branch %q does not exist", branchName)
	}

	// Get default base branch
	baseBranch, err := ch.gitClient.GetDefaultBaseBranch()
	if err != nil {
		return fmt.Errorf("failed to determine base branch: %w", err)
	}

	// Capture diff hunks from git
	diffHunks, err := ch.gitClient.CaptureBranchDiff(owner, repo, branchName, baseBranch)
	if err != nil {
		return fmt.Errorf("failed to capture branch diff: %w", err)
	}

	// Store diff hunks
	if err := ch.storage.CaptureBranchDiffHunks(owner, repo, branchName, *diffHunks); err != nil {
		return fmt.Errorf("failed to store branch diff hunks: %w", err)
	}

	fmt.Printf("Captured diff hunks for branch %s/%s:%s (%d hunks)\n",
		owner, repo, branchName, len(diffHunks.DiffHunks))
	return nil
}

// CommentCommand adds a line-specific comment for either PR or branch.
func (ch *CommandHandler) CommentCommand(owner, repo, identifier string, file string, lineSpec, commentBody, side string, force bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.addPRComment(owner, repo, parsed.PRNumber, file, lineSpec, commentBody, side, force)
	}
	return ch.addBranchComment(owner, repo, parsed.BranchName, file, lineSpec, commentBody, side, force)
}

// addPRComment adds a line-specific comment to a PR.
func (ch *CommandHandler) addPRComment(owner, repo string, prNumber int, file string, lineSpec, commentBody, side string, force bool) error {
	// Parse line specification
	lineRange, err := models.ParseLineSpec(lineSpec)
	if err != nil {
		return fmt.Errorf("invalid line specification %q: %w", lineSpec, err)
	}

	// Create comment
	comment := models.Comment{
		Path: file,
		Line: lineRange.EndLine,
		Body: commentBody,
		Side: side,
	}

	// Set start line for multi-line comments
	if lineRange.StartLine != lineRange.EndLine {
		comment.StartLine = &lineRange.StartLine
	}

	// Validate comment against diff hunks if they exist
	if ch.diffHunksExist(owner, repo, prNumber) {
		if err := ch.storage.ValidateCommentAgainstDiff(owner, repo, prNumber, comment); err != nil {
			if !force {
				return fmt.Errorf("validation failed: %w (use --force to override)", err)
			}
			fmt.Printf("Warning: %v\n", err)
		}
	} else {
		fmt.Printf("Warning: No diff hunks found, comment validation skipped\n")
	}

	// Add comment
	if err := ch.storage.AddComment(owner, repo, prNumber, comment); err != nil {
		if strings.Contains(err.Error(), "duplicate") && !force {
			return fmt.Errorf("duplicate comment detected (use --force to override): %w", err)
		}
		return fmt.Errorf("failed to add comment: %w", err)
	}

	fmt.Printf("Added comment to %s:%s in PR %s/%s#%d\n",
		file, lineSpec, owner, repo, prNumber)
	return nil
}

// addBranchComment adds a line-specific comment to a branch.
func (ch *CommandHandler) addBranchComment(owner, repo, branchName string, file string, lineSpec, commentBody, side string, force bool) error {
	// Parse line specification
	lineRange, err := models.ParseLineSpec(lineSpec)
	if err != nil {
		return fmt.Errorf("invalid line specification %q: %w", lineSpec, err)
	}

	// Create comment
	comment := models.Comment{
		Path: file,
		Line: lineRange.EndLine,
		Body: commentBody,
		Side: side,
	}

	// Set start line for multi-line comments
	if lineRange.StartLine != lineRange.EndLine {
		comment.StartLine = &lineRange.StartLine
	}

	// Validate comment against diff hunks if they exist
	if ch.branchDiffHunksExist(owner, repo, branchName) {
		if err := ch.storage.ValidateBranchCommentAgainstDiff(owner, repo, branchName, comment); err != nil {
			if !force {
				return fmt.Errorf("validation failed: %w (use --force to override)", err)
			}
			fmt.Printf("Warning: %v\n", err)
		}
	} else {
		fmt.Printf("Warning: No diff hunks found, comment validation skipped\n")
	}

	// Add comment
	if err := ch.storage.AddBranchComment(owner, repo, branchName, comment); err != nil {
		if strings.Contains(err.Error(), "duplicate") && !force {
			return fmt.Errorf("duplicate comment detected (use --force to override): %w", err)
		}
		return fmt.Errorf("failed to add comment: %w", err)
	}

	fmt.Printf("Added comment to %s:%s in branch %s/%s:%s\n",
		file, lineSpec, owner, repo, branchName)
	return nil
}

// SubmitCommand submits a review to GitHub.
func (ch *CommandHandler) SubmitCommand(owner, repo string, prNumber int, body, event string, formatter OutputFormatter, postSubmitAction models.Executor) error {
	// Get comments
	prComments, err := ch.storage.GetComments(owner, repo, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Convert to GitHub API format
	review := models.PRReview{
		Body:     body,
		Event:    event,
		Comments: prComments.Comments,
	}

	// Submit review
	if err := ch.ghClient.SubmitReview(owner, repo, prNumber, review); err != nil {
		return fmt.Errorf("failed to submit review: %w", err)
	}

	// Format result
	result := models.SubmitResult{
		Status:           "success",
		PRNumber:         prNumber,
		Owner:            owner,
		Repo:             repo,
		Comments:         len(prComments.Comments),
		SubmittedAt:      time.Now(),
		PostSubmitAction: postSubmitAction.Name(),
	}

	output, err := formatter.FormatSubmitResult(result)
	if err != nil {
		return fmt.Errorf("failed to format result: %w", err)
	}
	fmt.Println(output)

	// Execute post-submit action
	if err := postSubmitAction.Execute(ch.storage, owner, repo, prNumber); err != nil {
		return fmt.Errorf("post-submit action failed: %w", err)
	}

	return nil
}

// ListCommand lists comments for either PR or branch.
func (ch *CommandHandler) ListCommand(owner, repo, identifier string, formatter OutputFormatter, file, line, side string) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.listPRComments(owner, repo, parsed.PRNumber, formatter, file, line, side)
	}
	return ch.listBranchComments(owner, repo, parsed.BranchName, formatter, file, line, side)
}

// listPRComments lists comments for a PR.
func (ch *CommandHandler) listPRComments(owner, repo string, prNumber int, formatter OutputFormatter, file, line, side string) error {
	prComments, err := ch.storage.GetComments(owner, repo, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Apply filters
	var filteredComments []models.Comment
	for _, comment := range prComments.Comments {
		if file != "" && comment.Path != file {
			continue
		}
		if side != "" && comment.Side != side {
			continue
		}
		if line != "" {
			lineRange, err := models.ParseLineSpec(line)
			if err != nil {
				return fmt.Errorf("invalid line filter %q: %w", line, err)
			}
			if !commentInRange(comment, lineRange) {
				continue
			}
		}
		filteredComments = append(filteredComments, comment)
	}

	// Output results
	output, err := formatter.FormatComments(filteredComments)
	if err != nil {
		return fmt.Errorf("failed to format comments: %w", err)
	}
	fmt.Println(output)

	return nil
}

// listBranchComments lists comments for a branch.
func (ch *CommandHandler) listBranchComments(owner, repo, branchName string, formatter OutputFormatter, file, line, side string) error {
	branchComments, err := ch.storage.GetBranchComments(owner, repo, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Apply filters
	var filteredComments []models.Comment
	for _, comment := range branchComments.Comments {
		if file != "" && comment.Path != file {
			continue
		}
		if side != "" && comment.Side != side {
			continue
		}
		if line != "" {
			lineRange, err := models.ParseLineSpec(line)
			if err != nil {
				return fmt.Errorf("invalid line filter %q: %w", line, err)
			}
			if !commentInRange(comment, lineRange) {
				continue
			}
		}
		filteredComments = append(filteredComments, comment)
	}

	// Output results
	output, err := formatter.FormatComments(filteredComments)
	if err != nil {
		return fmt.Errorf("failed to format comments: %w", err)
	}
	fmt.Println(output)

	return nil
}

// DeleteCommand deletes specific comments for either PR or branch.
func (ch *CommandHandler) DeleteCommand(owner, repo, identifier string, commentID string, _ bool, _ OutputFormatter) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		if err := ch.storage.DeleteCommentByID(owner, repo, parsed.PRNumber, commentID); err != nil {
			return fmt.Errorf("failed to delete comment by ID: %w", err)
		}
		fmt.Printf("Deleted comment with ID prefix '%s' from PR %s/%s#%d\n", commentID, owner, repo, parsed.PRNumber)
		return nil
	}

	// For branches, deletion by ID is not yet implemented
	return fmt.Errorf("comment deletion by ID is not yet supported for branches")
}

// ClearCommand clears comments for either PR or branch.
func (ch *CommandHandler) ClearCommand(owner, repo, identifier string, file string, confirm bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.clearPRComments(owner, repo, parsed.PRNumber, file, confirm)
	}
	return ch.clearBranchComments(owner, repo, parsed.BranchName, file, confirm)
}

// clearPRComments clears comments for a PR or file.
func (ch *CommandHandler) clearPRComments(owner, repo string, prNumber int, file string, confirm bool) error {
	if !confirm {
		if file != "" {
			fmt.Printf("This will delete all comments for file '%s' in PR %s/%s#%d. Continue? (y/N): ",
				file, owner, repo, prNumber)
		} else {
			fmt.Printf("This will delete ALL comments for PR %s/%s#%d. Continue? (y/N): ",
				owner, repo, prNumber)
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
		err = ch.storage.ClearCommentsForFile(owner, repo, prNumber, file)
	} else {
		err = ch.storage.ClearComments(owner, repo, prNumber)
	}

	if err != nil {
		return fmt.Errorf("failed to clear comments: %w", err)
	}

	if file != "" {
		fmt.Printf("Cleared all comments for file '%s' in PR %s/%s#%d\n",
			file, owner, repo, prNumber)
	} else {
		fmt.Printf("Cleared all comments for PR %s/%s#%d\n",
			owner, repo, prNumber)
	}

	return nil
}

// clearBranchComments clears comments for a branch.
func (ch *CommandHandler) clearBranchComments(owner, repo, branchName string, file string, confirm bool) error {
	if !confirm {
		if file != "" {
			fmt.Printf("This will delete all comments for file '%s' in branch %s/%s:%s. Continue? (y/N): ",
				file, owner, repo, branchName)
		} else {
			fmt.Printf("This will delete ALL comments for branch %s/%s:%s. Continue? (y/N): ",
				owner, repo, branchName)
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
		// For now, we'll clear all branch comments (file-specific clearing not implemented)
		err = ch.storage.ClearBranchComments(owner, repo, branchName)
		fmt.Printf("Note: Cleared ALL branch comments (file-specific clearing not yet implemented)\n")
	} else {
		err = ch.storage.ClearBranchComments(owner, repo, branchName)
	}

	if err != nil {
		return fmt.Errorf("failed to clear comments: %w", err)
	}

	if file != "" {
		fmt.Printf("Cleared all comments for file '%s' in branch %s/%s:%s\n", file, owner, repo, branchName)
	} else {
		fmt.Printf("Cleared all comments for branch %s/%s:%s\n", owner, repo, branchName)
	}

	return nil
}

// Helper functions.

func (ch *CommandHandler) diffHunksExist(owner, repo string, prNumber int) bool {
	prPath := filepath.Join("repos", owner, repo, "pull", strconv.Itoa(prNumber))
	diffPath := filepath.Join(prPath, "diff-hunks.json")

	// Create a basic storage instance to check existence
	fs, err := storage.NewFileSystemStore(ch.storageHome)
	if err != nil {
		return false
	}

	return fs.Exists(diffPath)
}

func (ch *CommandHandler) branchDiffHunksExist(owner, repo, branchName string) bool {
	sanitizedBranch := strings.ReplaceAll(branchName, "/", "_")
	branchPath := filepath.Join("repos", owner, repo, "branch", sanitizedBranch)
	diffPath := filepath.Join(branchPath, "diff-hunks.json")

	// Create a basic storage instance to check existence
	fs, err := storage.NewFileSystemStore(ch.storageHome)
	if err != nil {
		return false
	}

	return fs.Exists(diffPath)
}

func commentInRange(comment models.Comment, lineRange *models.LineRange) bool {
	commentRange := comment.GetLineRange()
	return commentRange.StartLine <= lineRange.EndLine && commentRange.EndLine >= lineRange.StartLine
}

// getStorageHome returns the storage home directory.
func GetStorageHome() string {
	if home := os.Getenv("GH_STORAGE_HOME"); home != "" {
		return home
	}

	userHome, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting home directory: %v\n", err)
		os.Exit(1)
	}

	return filepath.Join(userHome, ".config", "gh-nudge", "storage")
}
