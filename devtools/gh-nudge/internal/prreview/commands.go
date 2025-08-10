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
func (ch *CommandHandler) CaptureCommand(repository models.Repository, identifier string, force bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.capturePRDiff(repository, parsed.PRNumber, force)
	}
	return ch.captureBranchDiff(repository, parsed.BranchName, force)
}

// capturePRDiff captures and stores PR diff hunks.
func (ch *CommandHandler) capturePRDiff(repository models.Repository, prNumber int, force bool) error {
	// Check if diff hunks already exist
	if !force && ch.diffHunksExist(repository, prNumber) {
		return fmt.Errorf("diff hunks already exist for PR %d, use --force to overwrite", prNumber)
	}

	// Validate PR access
	if err := ch.ghClient.ValidatePRAccess(repository, prNumber); err != nil {
		return fmt.Errorf("failed to access PR %s#%d: %w", repository, prNumber, err)
	}

	// Fetch diff hunks from GitHub
	diffHunks, err := ch.ghClient.GetPRDiff(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to fetch diff hunks: %w", err)
	}

	// Store diff hunks
	if err := ch.storage.CaptureDiffHunks(repository, prNumber, *diffHunks); err != nil {
		return fmt.Errorf("failed to store diff hunks: %w", err)
	}

	fmt.Printf("Captured diff hunks for PR %s#%d (%d hunks)\n",
		repository, prNumber, len(diffHunks.DiffHunks))
	return nil
}

// captureBranchDiff captures and stores branch diff hunks.
func (ch *CommandHandler) captureBranchDiff(repository models.Repository, branchName string, force bool) error {
	// Check if diff hunks already exist
	if !force && ch.branchDiffHunksExist(repository, branchName) {
		return fmt.Errorf("diff hunks already exist for branch %q, use --force to overwrite", branchName)
	}

	// Create validated branch
	branch, err := git.NewBranch(branchName)
	if err != nil {
		return fmt.Errorf("invalid branch name %q: %w", branchName, err)
	}

	// Check if branch exists
	exists, err := ch.gitClient.BranchExists(branch)
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
	diffHunks, err := ch.gitClient.CaptureBranchDiff(repository, branch, baseBranch)
	if err != nil {
		return fmt.Errorf("failed to capture branch diff: %w", err)
	}

	// Store diff hunks
	if err := ch.storage.CaptureBranchDiffHunks(repository, branchName, *diffHunks); err != nil {
		return fmt.Errorf("failed to store branch diff hunks: %w", err)
	}

	fmt.Printf("Captured diff hunks for branch %s:%s (%d hunks)\n",
		repository, branchName, len(diffHunks.DiffHunks))
	return nil
}

// CommentCommand adds a line-specific comment for either PR or branch.
func (ch *CommandHandler) CommentCommand(repository models.Repository, identifier string, file string, lineSpec, commentBody, side string, force bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.addPRComment(repository, parsed.PRNumber, file, lineSpec, commentBody, side, force)
	}
	return ch.addBranchComment(repository, parsed.BranchName, file, lineSpec, commentBody, side, force)
}

// addPRComment adds a line-specific comment to a PR.
func (ch *CommandHandler) addPRComment(repository models.Repository, prNumber int, file string, lineSpec, commentBody, side string, force bool) error {
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
	if ch.diffHunksExist(repository, prNumber) {
		if err := ch.storage.ValidateCommentAgainstDiff(repository, prNumber, comment); err != nil {
			if !force {
				return fmt.Errorf("validation failed: %w (use --force to override)", err)
			}
			fmt.Printf("Warning: %v\n", err)
		}
	} else {
		fmt.Printf("Warning: No diff hunks found, comment validation skipped\n")
	}

	// Add comment
	if err := ch.storage.AddComment(repository, prNumber, comment); err != nil {
		if strings.Contains(err.Error(), "duplicate") && !force {
			return fmt.Errorf("duplicate comment detected (use --force to override): %w", err)
		}
		return fmt.Errorf("failed to add comment: %w", err)
	}

	fmt.Printf("Added comment to %s:%s in PR %s#%d\n",
		file, lineSpec, repository, prNumber)
	return nil
}

// addBranchComment adds a line-specific comment to a branch.
func (ch *CommandHandler) addBranchComment(repository models.Repository, branchName string, file string, lineSpec, commentBody, side string, force bool) error {
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
	if ch.branchDiffHunksExist(repository, branchName) {
		if err := ch.storage.ValidateBranchCommentAgainstDiff(repository, branchName, comment); err != nil {
			if !force {
				return fmt.Errorf("validation failed: %w (use --force to override)", err)
			}
			fmt.Printf("Warning: %v\n", err)
		}
	} else {
		fmt.Printf("Warning: No diff hunks found, comment validation skipped\n")
	}

	// Add comment
	if err := ch.storage.AddBranchComment(repository, branchName, comment); err != nil {
		if strings.Contains(err.Error(), "duplicate") && !force {
			return fmt.Errorf("duplicate comment detected (use --force to override): %w", err)
		}
		return fmt.Errorf("failed to add comment: %w", err)
	}

	fmt.Printf("Added comment to %s:%s in branch %s:%s\n",
		file, lineSpec, repository, branchName)
	return nil
}

// SubmitCommand submits a review to GitHub.
func (ch *CommandHandler) SubmitCommand(repository models.Repository, prNumber int, body, event, file string, formatter OutputFormatter, postSubmitAction models.Executor) error {
	// Get comments
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Filter comments by file if specified
	var commentsToSubmit []models.Comment
	if file != "" {
		for _, comment := range prComments.Comments {
			if comment.Path == file {
				commentsToSubmit = append(commentsToSubmit, comment)
			}
		}
		if len(commentsToSubmit) == 0 {
			return fmt.Errorf("no comments found for file: %s", file)
		}
	} else {
		commentsToSubmit = prComments.Comments
	}

	// Convert to GitHub API format
	review := models.PRReview{
		Body:     body,
		Event:    event,
		Comments: commentsToSubmit,
	}

	// Submit review
	if err := ch.ghClient.SubmitReview(repository, prNumber, review); err != nil {
		return fmt.Errorf("failed to submit review: %w", err)
	}

	// Format result
	result := models.SubmitResult{
		Status:           "success",
		PRNumber:         prNumber,
		Repository:       repository,
		Comments:         len(commentsToSubmit),
		SubmittedAt:      time.Now(),
		PostSubmitAction: postSubmitAction.Name(),
		File:             file,
	}

	output, err := formatter.FormatSubmitResult(result)
	if err != nil {
		return fmt.Errorf("failed to format result: %w", err)
	}
	fmt.Println(output)

	// Execute post-submit action
	if err := postSubmitAction.Execute(ch.storage, repository, prNumber, file); err != nil {
		return fmt.Errorf("post-submit action failed: %w", err)
	}

	return nil
}

// ListCommand lists comments for either PR or branch.
func (ch *CommandHandler) ListCommand(repository models.Repository, identifier string, formatter OutputFormatter, file, line, side string) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.listPRComments(repository, parsed.PRNumber, formatter, file, line, side)
	}
	return ch.listBranchComments(repository, parsed.BranchName, formatter, file, line, side)
}

// listPRComments lists comments for a PR.
func (ch *CommandHandler) listPRComments(repository models.Repository, prNumber int, formatter OutputFormatter, file, line, side string) error {
	prComments, err := ch.storage.GetComments(repository, prNumber)
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
func (ch *CommandHandler) listBranchComments(repository models.Repository, branchName string, formatter OutputFormatter, file, line, side string) error {
	branchComments, err := ch.storage.GetBranchComments(repository, branchName)
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
func (ch *CommandHandler) DeleteCommand(repository models.Repository, identifier string, commentID string, _ bool, _ OutputFormatter) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		if err := ch.storage.DeleteCommentByID(repository, parsed.PRNumber, commentID); err != nil {
			return fmt.Errorf("failed to delete comment by ID: %w", err)
		}
		fmt.Printf("Deleted comment with ID prefix '%s' from PR %s#%d\n", commentID, repository, parsed.PRNumber)
		return nil
	}

	// Delete branch comment by ID
	if err := ch.storage.DeleteBranchCommentByID(repository, parsed.BranchName, commentID); err != nil {
		return fmt.Errorf("failed to delete branch comment by ID: %w", err)
	}
	fmt.Printf("Deleted comment with ID prefix '%s' from branch %s:%s\n", commentID, repository, parsed.BranchName)
	return nil
}

// ClearCommand clears comments for either PR or branch.
func (ch *CommandHandler) ClearCommand(repository models.Repository, identifier string, file string, confirm bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.clearPRComments(repository, parsed.PRNumber, file, confirm)
	}
	return ch.clearBranchComments(repository, parsed.BranchName, file, confirm)
}

// clearPRComments clears comments for a PR or file.
func (ch *CommandHandler) clearPRComments(repository models.Repository, prNumber int, file string, confirm bool) error {
	if !confirm {
		if file != "" {
			fmt.Printf("This will delete all comments for file '%s' in PR %s#%d. Continue? (y/N): ",
				file, repository, prNumber)
		} else {
			fmt.Printf("This will delete ALL comments for PR %s#%d. Continue? (y/N): ",
				repository, prNumber)
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
		err = ch.storage.ClearCommentsForFile(repository, prNumber, file)
	} else {
		err = ch.storage.ClearComments(repository, prNumber)
	}

	if err != nil {
		return fmt.Errorf("failed to clear comments: %w", err)
	}

	if file != "" {
		fmt.Printf("Cleared all comments for file '%s' in PR %s#%d\n",
			file, repository, prNumber)
	} else {
		fmt.Printf("Cleared all comments for PR %s#%d\n",
			repository, prNumber)
	}

	return nil
}

// clearBranchComments clears comments for a branch.
func (ch *CommandHandler) clearBranchComments(repository models.Repository, branchName string, file string, confirm bool) error {
	if !confirm {
		if file != "" {
			fmt.Printf("This will delete all comments for file '%s' in branch %s:%s. Continue? (y/N): ",
				file, repository, branchName)
		} else {
			fmt.Printf("This will delete ALL comments for branch %s:%s. Continue? (y/N): ",
				repository, branchName)
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
		err = ch.storage.ClearBranchCommentsForFile(repository, branchName, file)
	} else {
		err = ch.storage.ClearBranchComments(repository, branchName)
	}

	if err != nil {
		return fmt.Errorf("failed to clear comments: %w", err)
	}

	if file != "" {
		fmt.Printf("Cleared all comments for file '%s' in branch %s:%s\n", file, repository, branchName)
	} else {
		fmt.Printf("Cleared all comments for branch %s:%s\n", repository, branchName)
	}

	return nil
}

// Helper functions.

func (ch *CommandHandler) diffHunksExist(repository models.Repository, prNumber int) bool {
	// TODO: Consider providing PR path creation.
	prPath := filepath.Join("repos", repository.Owner, repository.Name, "pull", strconv.Itoa(prNumber))
	diffPath := filepath.Join(prPath, "diff-hunks.json")

	// Create a basic storage instance to check existence
	fs, err := storage.NewFileSystemStore(ch.storageHome)
	if err != nil {
		return false
	}

	return fs.Exists(diffPath)
}

func (ch *CommandHandler) branchDiffHunksExist(repository models.Repository, branchName string) bool {
	sanitizedBranch := strings.ReplaceAll(branchName, "/", "_")
	// TODO: Consider providing branch path creation.
	branchPath := filepath.Join("repos", repository.Owner, repository.Name, "branch", sanitizedBranch)
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
