package prreview

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/git"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

// OutputFormatter defines the interface for formatting output.
type OutputFormatter interface {
	FormatSubmitResult(result models.SubmitResult) (string, error)
	FormatComments(comments []models.Comment) (string, error)
	FormatCommentsWithContext(comments []models.CommentWithLineContext) (string, error)
	FormatSingleComment(comment models.Comment) (string, error)
}

// GitHubClient defines the interface for GitHub PR operations.
// This interface allows for easy mocking in tests.
type GitHubClient interface {
	// ValidatePRAccess validates that the PR exists and is accessible.
	ValidatePRAccess(repository models.Repository, prNumber int) error

	// GetPRDiff fetches the diff hunks for a pull request.
	GetPRDiff(repository models.Repository, prNumber int) (*models.PRDiffHunks, error)

	// SubmitReview submits a review to GitHub.
	SubmitReview(repository models.Repository, prNumber int, review models.PRReview) error

	// GetPRComments fetches comments from GitHub for a pull request.
	GetPRComments(repository models.Repository, prNumber int) ([]models.Comment, error)
}

// GitClient defines the interface for git operations.
// This interface allows for easy mocking in tests.
type GitClient interface {
	// BranchExists checks if a branch exists in the repository.
	BranchExists(branch git.Branch) (bool, error)

	// GetDefaultBaseBranch returns the default base branch (e.g., main, master).
	GetDefaultBaseBranch() (git.Branch, error)

	// CaptureBranchDiff captures the diff hunks for a branch compared to its base branch.
	CaptureBranchDiff(repository models.Repository, branch, baseBranch git.Branch) (*models.BranchDiffHunks, error)

	// GetStagedDiff gets the diff of staged changes.
	GetStagedDiff() (string, error)

	// GetUnstagedDiff gets the diff of unstaged working directory changes.
	GetUnstagedDiff() (string, error)

	// GetDiffSince gets the diff since a specific commit.
	GetDiffSince(since string) (string, error)

	// AutoDetectChanges analyzes differences between stored diff hunks and current file state.
	AutoDetectChanges(file string, storedDiffHunks []models.DiffHunk) (*models.AutoDetectResult, error)
}

// CommandHandler handles gh-pr-review commands.
type CommandHandler struct {
	storage     *storage.GitHubStorage
	ghClient    GitHubClient
	gitClient   GitClient
	storageHome string
}

// NewCommandHandler creates a new command handler with injected dependencies.
// This constructor is primarily intended for testing purposes.
func NewCommandHandler(
	storage *storage.GitHubStorage,
	ghClient GitHubClient,
	gitClient GitClient,
	storageHome string,
) *CommandHandler {
	return &CommandHandler{
		storage:     storage,
		ghClient:    ghClient,
		gitClient:   gitClient,
		storageHome: storageHome,
	}
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
		Line: *lineRange,
		Body: commentBody,
		Side: side,
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
		Line: *lineRange,
		Body: commentBody,
		Side: side,
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
	return ch.SubmitCommandWithOptions(repository, prNumber, body, event, file, formatter, postSubmitAction, SubmitOptions{})
}

// SubmitOptions contains options for the submit command.
type SubmitOptions struct {
	AutoAdjust          bool
	ValidateAdjustments bool
	SmartMerge          bool
	MergeOptions        models.MergeOptions
}

// SubmitCommandWithOptions submits a review to GitHub with advanced options.
func (ch *CommandHandler) SubmitCommandWithOptions(repository models.Repository, prNumber int, body, event, file string, formatter OutputFormatter, postSubmitAction models.Executor, options SubmitOptions) error {
	// Get comments
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Process comments for submission
	commentsToSubmit, err := ch.processCommentsForSubmission(prComments.Comments, file, options)
	if err != nil {
		return fmt.Errorf("failed to process comments: %w", err)
	}

	// Validate adjustments if requested
	if err := ch.validateAdjustmentsIfRequested(commentsToSubmit, repository, prNumber, options); err != nil {
		return err
	}

	// Filter out comments that are not within valid diff hunks
	validComments, filteredComments := ch.filterCommentsAgainstDiffHunks(repository, prNumber, commentsToSubmit)

	// Report filtering results
	if len(filteredComments) > 0 {
		fmt.Printf("Warning: Filtered out %d comment(s) not within diff hunks:\n", len(filteredComments))
		for _, comment := range filteredComments {
			fmt.Printf("  - %s:%v (side %s) - %s\n", comment.Path, comment.Line, comment.Side, comment.FormatIDShort())
		}
	}

	if len(validComments) == 0 {
		return fmt.Errorf("no valid comments to submit after filtering")
	}

	fmt.Printf("Submitting %d valid comment(s) to GitHub\n", len(validComments))

	// Submit review to GitHub with only valid comments
	if err := ch.submitReviewToGitHub(repository, prNumber, body, event, validComments); err != nil {
		return err
	}

	// Update local storage if needed (use validComments instead of commentsToSubmit)
	if err := ch.updateLocalStorageAfterSubmit(repository, prNumber, prComments, validComments, options); err != nil {
		fmt.Printf("Warning: Failed to update local comments after auto-adjustment: %v\n", err)
	}

	// Format and display result (use validComments instead of commentsToSubmit)
	if err := ch.formatAndDisplayResult(repository, prNumber, validComments, postSubmitAction, file, formatter); err != nil {
		return err
	}

	// Execute post-submit action (this will now only affect comments that were actually submitted)
	if err := postSubmitAction.Execute(ch.storage, repository, prNumber, file); err != nil {
		return fmt.Errorf("post-submit action failed: %w", err)
	}

	return nil
}

// ListCommand lists comments for either PR or branch.
func (ch *CommandHandler) ListCommand(repository models.Repository, identifier string, formatter OutputFormatter, file, line, side string, showContext bool, showArchived bool, contextLines int) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	// Build filter
	filter := models.CommentFilter{
		File:         file,
		Side:         side,
		ShowArchived: showArchived,
	}
	if line != "" {
		lineRange, err := models.ParseLineSpec(line)
		if err != nil {
			return fmt.Errorf("invalid line filter %q: %w", line, err)
		}
		filter.LineRange = lineRange
	}

	if parsed.IsPR() {
		return ch.listPRComments(repository, parsed.PRNumber, formatter, filter, showContext, contextLines)
	}
	return ch.listBranchComments(repository, parsed.BranchName, formatter, filter, showContext, contextLines)
}

// listPRComments lists comments for a PR.
func (ch *CommandHandler) listPRComments(repository models.Repository, prNumber int, formatter OutputFormatter, filter models.CommentFilter, showContext bool, contextLines int) error {
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Apply filters
	filteredComments := filter.Apply(prComments.Comments)

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
				fmt.Fprintf(os.Stderr, "Warning: Could not get context for %s:%v - %v\n", comment.Path, comment.Line, err)
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

// listBranchComments lists comments for a branch.
func (ch *CommandHandler) listBranchComments(repository models.Repository, branchName string, formatter OutputFormatter, filter models.CommentFilter, showContext bool, contextLines int) error {
	branchComments, err := ch.storage.GetBranchComments(repository, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Apply filters
	filteredComments := filter.Apply(branchComments.Comments)

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
				fmt.Fprintf(os.Stderr, "Warning: Could not get context for %s:%v - %v\n", comment.Path, comment.Line, err)
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

// NextCommand gets the next unresolved comment for either PR or branch.
func (ch *CommandHandler) NextCommand(repository models.Repository, identifier string, formatter OutputFormatter, file string, priority models.CommentPriority) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.nextPRComment(repository, parsed.PRNumber, formatter, file, priority)
	}
	return ch.nextBranchComment(repository, parsed.BranchName, formatter, file, priority)
}

// ResolveCommand marks a comment as resolved for either PR or branch.
func (ch *CommandHandler) ResolveCommand(repository models.Repository, identifier string, commentID string, archive bool, reason string) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	if parsed.IsPR() {
		return ch.resolvePRComment(repository, parsed.PRNumber, commentID, archive, reason)
	}
	return ch.resolveBranchComment(repository, parsed.BranchName, commentID, archive, reason)
}

// AutoAdjustCommand automatically adjusts line numbers based on git diff.
func (ch *CommandHandler) AutoAdjustCommand(repository models.Repository, identifier string, since string, staged bool, unstaged bool, gitDiffSpec string, ifNeeded bool) error {
	_, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	// Get current diff
	var diffOutput string
	switch {
	case gitDiffSpec != "":
		diffOutput = gitDiffSpec
	case staged:
		// Get staged changes
		diffOutput, err = ch.gitClient.GetStagedDiff()
		if err != nil {
			return fmt.Errorf("failed to get staged diff: %w", err)
		}
	case unstaged:
		// Get unstaged working directory changes
		diffOutput, err = ch.gitClient.GetUnstagedDiff()
		if err != nil {
			return fmt.Errorf("failed to get unstaged diff: %w", err)
		}
	case since != "":
		// Get diff since specific commit
		diffOutput, err = ch.gitClient.GetDiffSince(since)
		if err != nil {
			return fmt.Errorf("failed to get diff since %s: %w", since, err)
		}
	default:
		// Default to diff since last adjustment or HEAD~1
		diffOutput, err = ch.gitClient.GetDiffSince("HEAD~1")
		if err != nil {
			return fmt.Errorf("failed to get diff: %w", err)
		}
	}

	// If --if-needed flag is set, check if adjustment is needed
	if ifNeeded && diffOutput == "" {
		fmt.Println("No changes detected, adjustment not needed.")
		return nil
	}

	// Use the existing adjust command with unified diff
	options := AdjustOptionsExtended{
		DryRun: false,
		Force:  false,
		Format: "table",
	}

	return ch.AdjustCommandExtended(repository, identifier, "", diffOutput, options)
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

// filterCommentsAgainstDiffHunks filters comments to only include those within valid diff hunks.
// Returns validComments (those within diff hunks) and filteredComments (those outside diff hunks).
func (ch *CommandHandler) filterCommentsAgainstDiffHunks(repository models.Repository, prNumber int, comments []models.Comment) ([]models.Comment, []models.Comment) {
	// If no diff hunks exist, return all comments as valid (skip validation)
	if !ch.diffHunksExist(repository, prNumber) {
		fmt.Printf("Warning: No diff hunks found, comment validation skipped\n")
		return comments, nil
	}

	var validComments []models.Comment
	var filteredComments []models.Comment

	for _, comment := range comments {
		if err := ch.storage.ValidateCommentAgainstDiff(repository, prNumber, comment); err != nil {
			filteredComments = append(filteredComments, comment)
		} else {
			validComments = append(validComments, comment)
		}
	}

	return validComments, filteredComments
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

// nextPRComment gets the next unresolved comment for a PR.
func (ch *CommandHandler) nextPRComment(repository models.Repository, prNumber int, formatter OutputFormatter, file string, priority models.CommentPriority) error {
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Filter and sort comments
	var unresolvedComments []models.Comment
	for _, comment := range prComments.Comments {
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

// nextBranchComment gets the next unresolved comment for a branch.
func (ch *CommandHandler) nextBranchComment(repository models.Repository, branchName string, formatter OutputFormatter, file string, priority models.CommentPriority) error {
	branchComments, err := ch.storage.GetBranchComments(repository, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Filter and sort comments
	var unresolvedComments []models.Comment
	for _, comment := range branchComments.Comments {
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

// resolvePRComment marks a PR comment as resolved.
func (ch *CommandHandler) resolvePRComment(repository models.Repository, prNumber int, commentID string, archive bool, reason string) error {
	// Get current comments
	prComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Find and update the comment
	found := false
	for i, comment := range prComments.Comments {
		if comment.MatchesIDPrefix(commentID) {
			now := time.Now()
			if archive {
				prComments.Comments[i].Status = models.StatusArchived
			} else {
				prComments.Comments[i].Status = models.StatusResolved
			}
			prComments.Comments[i].ResolvedAt = &now
			prComments.Comments[i].ResolutionReason = reason
			found = true
			break
		}
	}

	if !found {
		return fmt.Errorf("comment with ID prefix '%s' not found", commentID)
	}

	// Save updated comments
	if err := ch.storage.UpdateComments(repository, prNumber, prComments); err != nil {
		return fmt.Errorf("failed to update comments: %w", err)
	}

	if archive {
		fmt.Printf("Archived comment with ID prefix '%s' in PR %s#%d\n", commentID, repository, prNumber)
	} else {
		fmt.Printf("Resolved comment with ID prefix '%s' in PR %s#%d\n", commentID, repository, prNumber)
	}
	if reason != "" {
		fmt.Printf("Resolution reason: %s\n", reason)
	}

	return nil
}

// resolveBranchComment marks a branch comment as resolved.
func (ch *CommandHandler) resolveBranchComment(repository models.Repository, branchName string, commentID string, archive bool, reason string) error {
	// Get current comments
	branchComments, err := ch.storage.GetBranchComments(repository, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch comments: %w", err)
	}

	// Find and update the comment
	found := false
	for i, comment := range branchComments.Comments {
		if comment.MatchesIDPrefix(commentID) {
			now := time.Now()
			if archive {
				branchComments.Comments[i].Status = models.StatusArchived
			} else {
				branchComments.Comments[i].Status = models.StatusResolved
			}
			branchComments.Comments[i].ResolvedAt = &now
			branchComments.Comments[i].ResolutionReason = reason
			found = true
			break
		}
	}

	if !found {
		return fmt.Errorf("comment with ID prefix '%s' not found", commentID)
	}

	// Save updated comments
	if err := ch.storage.UpdateBranchComments(repository, branchName, branchComments); err != nil {
		return fmt.Errorf("failed to update comments: %w", err)
	}

	if archive {
		fmt.Printf("Archived comment with ID prefix '%s' in branch %s:%s\n", commentID, repository, branchName)
	} else {
		fmt.Printf("Resolved comment with ID prefix '%s' in branch %s:%s\n", commentID, repository, branchName)
	}
	if reason != "" {
		fmt.Printf("Resolution reason: %s\n", reason)
	}

	return nil
}

// sortAndGetNextComment sorts comments and returns the next one to work on.
func sortAndGetNextComment(comments []models.Comment) models.Comment {
	// Sort by: file path (alphabetical), line number (ascending), creation time (oldest first)
	// This is a simple implementation - you may want to use sort.Slice for more complex sorting
	if len(comments) == 0 {
		return models.Comment{}
	}

	// For now, return the first comment
	// TODO: Implement proper sorting logic
	return comments[0]
}

// getStorageHome returns the storage home directory.
// processCommentsForSubmission processes comments based on the options provided.
func (ch *CommandHandler) processCommentsForSubmission(prComments []models.Comment, file string, options SubmitOptions) ([]models.Comment, error) {
	if options.AutoAdjust {
		return ch.autoAdjustComments(prComments, file, options)
	}
	return ch.filterCommentsByFile(prComments, file)
}

// autoAdjustComments automatically adjusts comments before submission.
func (ch *CommandHandler) autoAdjustComments(prComments []models.Comment, file string, options SubmitOptions) ([]models.Comment, error) {
	fmt.Println("Auto-adjusting comments before submission...")

	// Get latest diff to auto-adjust against
	diffOutput, err := ch.gitClient.GetDiffSince("HEAD~1")
	if err != nil {
		return nil, fmt.Errorf("failed to get git diff for auto-adjustment: %w", err)
	}

	if diffOutput == "" {
		fmt.Println("No changes detected, using comments as-is.")
		return prComments, nil
	}

	return ch.processFilesForAutoAdjustment(prComments, file, diffOutput, options)
}

// processFilesForAutoAdjustment processes each file for auto-adjustment.
func (ch *CommandHandler) processFilesForAutoAdjustment(prComments []models.Comment, file string, diffOutput string, options SubmitOptions) ([]models.Comment, error) {
	// Process each unique file
	fileComments := ch.groupCommentsByFile(prComments, file)

	var commentsToSubmit []models.Comment
	for filePath, fileCommentList := range fileComments {
		fmt.Printf("Auto-adjusting comments in %s...\n", filePath)

		adjustedComments, err := ch.adjustCommentsForFile(filePath, fileCommentList, diffOutput, options)
		if err != nil {
			fmt.Printf("Warning: %v, using comments as-is\n", err)
			commentsToSubmit = append(commentsToSubmit, fileCommentList...)
			continue
		}

		commentsToSubmit = append(commentsToSubmit, adjustedComments...)
	}

	fmt.Printf("Auto-adjustment completed. %d comments ready for submission.\n", len(commentsToSubmit))
	return commentsToSubmit, nil
}

// groupCommentsByFile groups comments by file path.
func (ch *CommandHandler) groupCommentsByFile(prComments []models.Comment, file string) map[string][]models.Comment {
	fileComments := make(map[string][]models.Comment)
	for _, comment := range prComments {
		if file == "" || comment.Path == file {
			fileComments[comment.Path] = append(fileComments[comment.Path], comment)
		}
	}
	return fileComments
}

// adjustCommentsForFile adjusts comments for a specific file.
func (ch *CommandHandler) adjustCommentsForFile(filePath string, fileCommentList []models.Comment, diffOutput string, options SubmitOptions) ([]models.Comment, error) {
	// Filter unified diff for this file
	filteredDiff, err := models.FilterUnifiedDiffForFile(diffOutput, filePath)
	if err != nil {
		return nil, fmt.Errorf("could not filter diff for %s: %w", filePath, err)
	}

	if filteredDiff == "" {
		// No changes for this file
		return fileCommentList, nil
	}

	// Parse adjustments
	adjustments, err := models.ParseDiffSpecWithAutoDetection(filteredDiff)
	if err != nil {
		return nil, fmt.Errorf("could not parse diff for %s: %w", filePath, err)
	}

	return ch.applyAdjustmentsToComments(fileCommentList, adjustments, filePath, options)
}

// applyAdjustmentsToComments applies adjustments to comments with optional smart merging.
func (ch *CommandHandler) applyAdjustmentsToComments(fileCommentList []models.Comment, adjustments []models.LineAdjustment, filePath string, options SubmitOptions) ([]models.Comment, error) {
	if options.SmartMerge {
		return ch.applySmartMerging(fileCommentList, adjustments, filePath, options)
	}
	return ch.applyRegularAdjustments(fileCommentList, adjustments)
}

// applySmartMerging applies smart merging to comments.
func (ch *CommandHandler) applySmartMerging(fileCommentList []models.Comment, adjustments []models.LineAdjustment, filePath string, options SubmitOptions) ([]models.Comment, error) {
	adjustedComments, conflicts, err := models.ApplySmartMerging(fileCommentList, adjustments, options.MergeOptions)
	if err != nil {
		fmt.Printf("Warning: Smart merging failed for %s: %v\n", filePath, err)
		// Fall back to regular adjustment
		return ch.applyRegularAdjustmentsFallback(fileCommentList, adjustments)
	}

	if len(conflicts) > 0 {
		ch.reportMergeConflicts(conflicts, filePath)
	}

	return adjustedComments, nil
}

// applyRegularAdjustmentsFallback applies regular adjustments as fallback.
func (ch *CommandHandler) applyRegularAdjustmentsFallback(fileCommentList []models.Comment, adjustments []models.LineAdjustment) ([]models.Comment, error) {
	adjustedComments := fileCommentList
	for i := range adjustedComments {
		models.AdjustComment(&adjustedComments[i], adjustments)
	}
	return adjustedComments, nil
}

// applyRegularAdjustments applies regular adjustments to comments.
func (ch *CommandHandler) applyRegularAdjustments(fileCommentList []models.Comment, adjustments []models.LineAdjustment) ([]models.Comment, error) {
	var adjustedComments []models.Comment
	for _, comment := range fileCommentList {
		testComment := comment
		if models.AdjustComment(&testComment, adjustments) {
			adjustedComments = append(adjustedComments, testComment)
		} else {
			fmt.Printf("Warning: Comment %s became orphaned during auto-adjustment\n", comment.FormatIDShort())
		}
	}
	return adjustedComments, nil
}

// reportMergeConflicts reports merge conflicts to the user.
func (ch *CommandHandler) reportMergeConflicts(conflicts []models.MergeConflict, filePath string) {
	fmt.Printf("Warning: %d unresolved merge conflicts in %s\n", len(conflicts), filePath)
	for _, conflict := range conflicts {
		fmt.Printf("  - Conflict at line %d: %d comments\n", conflict.Line, len(conflict.ConflictingComments))
	}
}

// filterCommentsByFile filters comments by file if specified.
func (ch *CommandHandler) filterCommentsByFile(prComments []models.Comment, file string) ([]models.Comment, error) {
	if file == "" {
		return prComments, nil
	}

	var commentsToSubmit []models.Comment
	for _, comment := range prComments {
		if comment.Path == file {
			commentsToSubmit = append(commentsToSubmit, comment)
		}
	}

	if len(commentsToSubmit) == 0 {
		return nil, fmt.Errorf("no comments found for file: %s", file)
	}

	return commentsToSubmit, nil
}

// validateAdjustmentsIfRequested validates adjustments if requested in options.
func (ch *CommandHandler) validateAdjustmentsIfRequested(commentsToSubmit []models.Comment, repository models.Repository, prNumber int, options SubmitOptions) error {
	if !options.ValidateAdjustments {
		return nil
	}

	fmt.Println("Validating adjusted comments against diff hunks...")

	prDiffHunks, err := ch.storage.GetDiffHunks(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get diff hunks for validation: %w", err)
	}

	var validationErrors []string
	for _, comment := range commentsToSubmit {
		if err := models.ValidateAdjustmentAgainstDiff(comment, nil, prDiffHunks.DiffHunks); err != nil {
			validationErrors = append(validationErrors, fmt.Sprintf("Comment %s: %s", comment.FormatIDShort(), err.Error()))
		}
	}

	if len(validationErrors) > 0 {
		return fmt.Errorf("validation failed:\n%s", strings.Join(validationErrors, "\n"))
	}

	fmt.Println("All comments validated successfully.")
	return nil
}

// submitReviewToGitHub submits the review to GitHub.
func (ch *CommandHandler) submitReviewToGitHub(repository models.Repository, prNumber int, body, event string, commentsToSubmit []models.Comment) error {
	review := models.PRReview{
		Body:     body,
		Event:    event,
		Comments: commentsToSubmit,
	}

	if err := ch.ghClient.SubmitReview(repository, prNumber, review); err != nil {
		return fmt.Errorf("failed to submit review: %w", err)
	}

	return nil
}

// updateLocalStorageAfterSubmit updates local storage after successful submission.
func (ch *CommandHandler) updateLocalStorageAfterSubmit(repository models.Repository, prNumber int, prComments *models.PRComments, commentsToSubmit []models.Comment, options SubmitOptions) error {
	if !options.AutoAdjust {
		return nil
	}

	prComments.Comments = commentsToSubmit
	prComments.UpdatedAt = time.Now()
	if err := ch.storage.UpdateComments(repository, prNumber, prComments); err != nil {
		return fmt.Errorf("failed to update comments: %w", err)
	}
	return nil
}

// formatAndDisplayResult formats and displays the submission result.
func (ch *CommandHandler) formatAndDisplayResult(repository models.Repository, prNumber int, commentsToSubmit []models.Comment, postSubmitAction models.Executor, file string, formatter OutputFormatter) error {
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

	return nil
}

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

// ArchiveListCommand lists all archived submissions for a PR.
func (ch *CommandHandler) ArchiveListCommand(repository models.Repository, prNumber int, _ OutputFormatter) error {
	metadata, err := ch.storage.ListArchivedSubmissions(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to list archived submissions: %w", err)
	}

	if len(metadata.Archives) == 0 {
		fmt.Printf("No archived submissions found for PR %s#%d\n", repository, prNumber)
		return nil
	}

	fmt.Printf("Archives for PR %s#%d:\n", repository, prNumber)
	for _, archive := range metadata.Archives {
		fmt.Printf("  %s  %s  %d comments  %s\n",
			archive.SubmissionID[:8],
			archive.ArchivedAt.Format("2006-01-02T15:04:05Z"),
			archive.CommentCount,
			archive.ReviewEvent)
	}

	return nil
}

// ArchiveShowCommand shows a specific archived submission.
func (ch *CommandHandler) ArchiveShowCommand(repository models.Repository, prNumber int, submissionID string, formatter OutputFormatter) error {
	submission, err := ch.storage.GetArchivedSubmission(repository, prNumber, submissionID)
	if err != nil {
		return fmt.Errorf("failed to get archived submission: %w", err)
	}

	fmt.Printf("Archived Submission: %s\n", submission.SubmissionID)
	fmt.Printf("Archived At: %s\n", submission.ArchivedAt.Format("2006-01-02T15:04:05Z"))
	fmt.Printf("Submitted At: %s\n", submission.SubmittedAt.Format("2006-01-02T15:04:05Z"))
	fmt.Printf("PR: %s/%s#%d\n", submission.Owner, submission.Repo, submission.PRNumber)
	fmt.Printf("Review Event: %s\n", submission.ReviewEvent)
	fmt.Printf("Review Body: %s\n", submission.ReviewBody)
	fmt.Printf("Comment Count: %d\n", submission.CommentCount)
	fmt.Println()

	if len(submission.Comments) > 0 {
		fmt.Println("Comments:")
		output, err := formatter.FormatComments(submission.Comments)
		if err != nil {
			return fmt.Errorf("failed to format comments: %w", err)
		}
		fmt.Print(output)
	}

	return nil
}

// ArchiveCleanupCommand removes archives older than the specified duration.
func (ch *CommandHandler) ArchiveCleanupCommand(repository models.Repository, prNumber int, olderThanStr string) error {
	olderThan, err := time.ParseDuration(olderThanStr)
	if err != nil {
		// Try to parse as days if duration parsing fails
		if strings.HasSuffix(olderThanStr, "d") {
			daysStr := strings.TrimSuffix(olderThanStr, "d")
			days, parseErr := strconv.Atoi(daysStr)
			if parseErr != nil {
				return fmt.Errorf("invalid duration '%s': %w", olderThanStr, err)
			}
			olderThan = time.Duration(days) * 24 * time.Hour
		} else {
			return fmt.Errorf("invalid duration '%s': %w", olderThanStr, err)
		}
	}

	if err := ch.storage.CleanupOldArchives(repository, prNumber, olderThan); err != nil {
		return fmt.Errorf("failed to cleanup old archives: %w", err)
	}

	fmt.Printf("Cleaned up archives older than %s for PR %s#%d\n", olderThanStr, repository, prNumber)
	return nil
}

// PullCommand fetches comments from GitHub and stores them in local storage with merge strategies.
func (ch *CommandHandler) PullCommand(repository models.Repository, prNumber int, options models.PullOptions) (*models.MergeResult, error) {
	// Validate PR access
	if err := ch.ghClient.ValidatePRAccess(repository, prNumber); err != nil {
		return nil, fmt.Errorf("failed to access PR %s#%d: %w", repository, prNumber, err)
	}

	// Fetch comments from GitHub
	githubComments, err := ch.ghClient.GetPRComments(repository, prNumber)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch comments from GitHub: %w", err)
	}

	// Apply filters if specified
	filteredComments := ch.filterGitHubComments(githubComments, options)

	if options.DryRun {
		return &models.MergeResult{
			PulledComments: filteredComments,
			TotalProcessed: len(filteredComments),
		}, nil
	}

	// Get existing local comments
	existingComments, err := ch.storage.GetComments(repository, prNumber)
	if err != nil && !strings.Contains(err.Error(), "not found") {
		return nil, fmt.Errorf("failed to get existing comments: %w", err)
	}

	// Merge with existing comments using specified strategy
	mergeResult := ch.mergeGitHubComments(filteredComments, existingComments.Comments, options.MergeStrategy)

	// Update local storage with merged results
	updatedComments := models.PRComments{
		PRNumber:   prNumber,
		Repository: repository,
		Comments:   mergeResult.PulledComments,
		UpdatedAt:  time.Now(),
	}

	if err := ch.storage.UpdateComments(repository, prNumber, &updatedComments); err != nil {
		return nil, fmt.Errorf("failed to update local comments: %w", err)
	}

	return mergeResult, nil
}

// filterGitHubComments applies filters to GitHub comments based on options.
func (ch *CommandHandler) filterGitHubComments(comments []models.Comment, options models.PullOptions) []models.Comment {
	if options.File == "" && options.Author == "" {
		return comments
	}

	var filtered []models.Comment
	for _, comment := range comments {
		// Filter by file if specified
		if options.File != "" && comment.Path != options.File {
			continue
		}

		// Filter by author if specified (GitHub comments don't have author info in our current model)
		// This would need to be implemented if we store author information

		filtered = append(filtered, comment)
	}

	return filtered
}

// mergeGitHubComments merges GitHub comments with existing local comments using the specified strategy.
func (ch *CommandHandler) mergeGitHubComments(githubComments, localComments []models.Comment, strategy models.MergeStrategy) *models.MergeResult {
	result := &models.MergeResult{
		TotalProcessed: len(githubComments),
	}

	// Default strategy is overwrite if not specified
	if strategy == "" {
		strategy = models.MergeStrategyOverwrite
	}

	switch strategy {
	case models.MergeStrategyOverwrite:
		result.PulledComments = githubComments
	case models.MergeStrategyMerge:
		result = ch.mergeCommentsStrategy(githubComments, localComments)
	case models.MergeStrategySkip:
		result = ch.skipConflictsStrategy(githubComments, localComments)
	}

	return result
}

// mergeCommentsStrategy implements the merge strategy: keep both local and GitHub comments.
func (ch *CommandHandler) mergeCommentsStrategy(githubComments, localComments []models.Comment) *models.MergeResult {
	result := &models.MergeResult{
		TotalProcessed: len(githubComments),
	}

	// Start with all local comments
	mergedComments := make([]models.Comment, len(localComments))
	copy(mergedComments, localComments)

	// Add GitHub comments, checking for conflicts
	for _, ghComment := range githubComments {
		conflict := ch.findConflictingComment(ghComment, localComments)
		if conflict != nil {
			result.ConflictedComments = append(result.ConflictedComments, ghComment)
			// Keep both - add GitHub comment with modified ID to avoid conflicts
			ghComment.ID = models.GenerateCommentID()
			mergedComments = append(mergedComments, ghComment)
		} else {
			mergedComments = append(mergedComments, ghComment)
		}
	}

	result.PulledComments = mergedComments
	return result
}

// skipConflictsStrategy implements the skip strategy: skip conflicting comments, only pull non-conflicting ones.
func (ch *CommandHandler) skipConflictsStrategy(githubComments, localComments []models.Comment) *models.MergeResult {
	result := &models.MergeResult{
		TotalProcessed: len(githubComments),
	}

	// Start with all local comments
	mergedComments := make([]models.Comment, len(localComments))
	copy(mergedComments, localComments)

	// Add only non-conflicting GitHub comments
	for _, ghComment := range githubComments {
		conflict := ch.findConflictingComment(ghComment, localComments)
		if conflict != nil {
			result.SkippedComments = append(result.SkippedComments, ghComment)
		} else {
			mergedComments = append(mergedComments, ghComment)
		}
	}

	result.PulledComments = mergedComments
	return result
}

// findConflictingComment checks if a GitHub comment conflicts with any existing local comment.
func (ch *CommandHandler) findConflictingComment(ghComment models.Comment, localComments []models.Comment) *models.Comment {
	for i := range localComments {
		if ch.isConflictingComment(ghComment, localComments[i]) {
			return &localComments[i]
		}
	}
	return nil
}

// isConflictingComment determines if two comments are conflicting.
// Comments conflict if they are on the same file, line, side, and have different content.
func (ch *CommandHandler) isConflictingComment(comment1, comment2 models.Comment) bool {
	return comment1.Path == comment2.Path &&
		comment1.Line == comment2.Line &&
		comment1.Side == comment2.Side &&
		comment1.Body != comment2.Body
}
