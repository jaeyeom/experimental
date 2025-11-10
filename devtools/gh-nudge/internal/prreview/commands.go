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
	target := models.NewPRTarget(prNumber)

	// Check if diff hunks already exist
	if !force && ch.diffHunksExistUnified(repository, target) {
		return fmt.Errorf("diff hunks already exist for PR %d, use --force to overwrite", prNumber)
	}

	// Validate PR access
	if err := ch.ghClient.ValidatePRAccess(repository, prNumber); err != nil {
		return fmt.Errorf("failed to access PR %s#%d: %w", repository, prNumber, err)
	}

	// Fetch diff hunks from GitHub
	prDiffHunks, err := ch.ghClient.GetPRDiff(repository, prNumber)
	if err != nil {
		return fmt.Errorf("failed to fetch diff hunks: %w", err)
	}

	// Convert to unified format
	reviewDiffHunks := models.ReviewDiffHunks{
		Target:      target.String(),
		Repository:  prDiffHunks.Repository,
		CapturedAt:  prDiffHunks.CapturedAt,
		DiffHunks:   prDiffHunks.DiffHunks,
		CommitSHA:   prDiffHunks.CommitSHA,
		BaseSHA:     prDiffHunks.BaseSHA,
		Description: prDiffHunks.Description,
	}

	// Store diff hunks using unified API
	if err := ch.storage.CaptureDiffHunksUnified(repository, target, reviewDiffHunks); err != nil {
		return fmt.Errorf("failed to store diff hunks: %w", err)
	}

	fmt.Printf("Captured diff hunks for PR %s#%d (%d hunks)\n",
		repository, prNumber, len(reviewDiffHunks.DiffHunks))
	return nil
}

// captureBranchDiff captures and stores branch diff hunks.
func (ch *CommandHandler) captureBranchDiff(repository models.Repository, branchName string, force bool) error {
	target := models.NewBranchTarget(branchName)

	// Check if diff hunks already exist
	if !force && ch.diffHunksExistUnified(repository, target) {
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
	branchDiffHunks, err := ch.gitClient.CaptureBranchDiff(repository, branch, baseBranch)
	if err != nil {
		return fmt.Errorf("failed to capture branch diff: %w", err)
	}

	// Convert to unified format
	reviewDiffHunks := models.ReviewDiffHunks{
		Target:      target.String(),
		Repository:  branchDiffHunks.Repository,
		CapturedAt:  branchDiffHunks.CapturedAt,
		DiffHunks:   branchDiffHunks.DiffHunks,
		CommitSHA:   branchDiffHunks.CommitSHA,
		BaseSHA:     branchDiffHunks.BaseSHA,
		BaseBranch:  branchDiffHunks.BaseBranch,
		Description: branchDiffHunks.Description,
	}

	// Store diff hunks using unified API
	if err := ch.storage.CaptureDiffHunksUnified(repository, target, reviewDiffHunks); err != nil {
		return fmt.Errorf("failed to store branch diff hunks: %w", err)
	}

	fmt.Printf("Captured diff hunks for branch %s:%s (%d hunks)\n",
		repository, branchName, len(reviewDiffHunks.DiffHunks))
	return nil
}

// CommentCommand adds a line-specific comment for either PR or branch.
func (ch *CommandHandler) CommentCommand(repository models.Repository, identifier string, file string, lineSpec, commentBody, side string, force bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	target := models.NewReviewTarget(parsed)
	return ch.addCommentUnified(repository, target, file, lineSpec, commentBody, side, force)
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
	target := models.NewPRTarget(prNumber)

	// Get comments
	reviewComments, err := ch.storage.GetCommentsUnified(repository, target)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	// Process comments for submission
	commentsToSubmit, err := ch.processCommentsForSubmission(reviewComments.Comments, file, options)
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
	if err := ch.updateLocalStorageAfterSubmit(repository, target, reviewComments, validComments, options); err != nil {
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
	var parsedSide models.Side
	if side != "" {
		var err error
		parsedSide, err = models.ParseSide(side)
		if err != nil {
			return fmt.Errorf("invalid side %q: %w", side, err)
		}
	}
	filter := models.CommentFilter{
		File:         file,
		Side:         parsedSide,
		ShowArchived: showArchived,
	}
	if line != "" {
		lineRange, err := models.ParseLineSpec(line)
		if err != nil {
			return fmt.Errorf("invalid line filter %q: %w", line, err)
		}
		filter.LineRange = lineRange
	}

	target := models.NewReviewTarget(parsed)
	return ch.listCommentsUnified(repository, target, formatter, filter, showContext, contextLines)
}

// DeleteCommand deletes specific comments for either PR or branch.
func (ch *CommandHandler) DeleteCommand(repository models.Repository, identifier string, commentID string, _ bool, _ OutputFormatter) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	target := models.NewReviewTarget(parsed)
	if err := ch.storage.DeleteCommentByIDUnified(repository, target, commentID); err != nil {
		return fmt.Errorf("failed to delete comment by ID: %w", err)
	}
	fmt.Printf("Deleted comment with ID prefix '%s' from %s %s\n", commentID, target.String(), repository)
	return nil
}

// ClearCommand clears comments for either PR or branch.
func (ch *CommandHandler) ClearCommand(repository models.Repository, identifier string, file string, confirm bool) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	target := models.NewReviewTarget(parsed)
	return ch.clearCommentsUnified(repository, target, file, confirm)
}

// NextCommand gets the next unresolved comment for either PR or branch.
func (ch *CommandHandler) NextCommand(repository models.Repository, identifier string, formatter OutputFormatter, file string, priority models.CommentPriority) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	target := models.NewReviewTarget(parsed)
	return ch.nextCommentUnified(repository, target, formatter, file, priority)
}

// ResolveCommand marks a comment as resolved for either PR or branch.
func (ch *CommandHandler) ResolveCommand(repository models.Repository, identifier string, commentID string, archive bool, reason string) error {
	parsed, err := models.ParseIdentifier(identifier)
	if err != nil {
		return fmt.Errorf("invalid identifier %q: %w", identifier, err)
	}

	target := models.NewReviewTarget(parsed)
	return ch.resolveCommentUnified(repository, target, commentID, archive, reason)
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

// Helper functions.

// diffHunksExistUnified checks if diff hunks exist for any review target.
func (ch *CommandHandler) diffHunksExistUnified(repository models.Repository, target models.ReviewTarget) bool {
	return ch.storage.DiffHunksExistUnified(repository, target)
}

func (ch *CommandHandler) diffHunksExist(repository models.Repository, prNumber int) bool {
	target := models.NewPRTarget(prNumber)
	return ch.diffHunksExistUnified(repository, target)
}

// filterCommentsAgainstDiffHunks filters comments to only include those within valid diff hunks.
// Returns validComments (those within diff hunks) and filteredComments (those outside diff hunks).
func (ch *CommandHandler) filterCommentsAgainstDiffHunks(repository models.Repository, prNumber int, comments []models.Comment) ([]models.Comment, []models.Comment) {
	target := models.NewPRTarget(prNumber)

	// If no diff hunks exist, return all comments as valid (skip validation)
	if !ch.diffHunksExist(repository, prNumber) {
		fmt.Printf("Warning: No diff hunks found, comment validation skipped\n")
		return comments, nil
	}

	var validComments []models.Comment
	var filteredComments []models.Comment

	for _, comment := range comments {
		if err := ch.storage.ValidateCommentAgainstDiffUnified(repository, target, comment); err != nil {
			filteredComments = append(filteredComments, comment)
		} else {
			validComments = append(validComments, comment)
		}
	}

	return validComments, filteredComments
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
		fmt.Printf("  - Conflict at %s: %d comments\n", conflict.Location, len(conflict.ConflictingComments))
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

	target := models.NewPRTarget(prNumber)
	fmt.Println("Validating adjusted comments against diff hunks...")

	reviewDiffHunks, err := ch.storage.GetDiffHunksUnified(repository, target)
	if err != nil {
		return fmt.Errorf("failed to get diff hunks for validation: %w", err)
	}

	var validationErrors []string
	for _, comment := range commentsToSubmit {
		if err := models.ValidateAdjustmentAgainstDiff(comment, nil, reviewDiffHunks.DiffHunks); err != nil {
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
func (ch *CommandHandler) updateLocalStorageAfterSubmit(repository models.Repository, target models.ReviewTarget, reviewComments *models.ReviewComments, commentsToSubmit []models.Comment, options SubmitOptions) error {
	if !options.AutoAdjust {
		return nil
	}

	reviewComments.Comments = commentsToSubmit
	reviewComments.UpdatedAt = time.Now()
	if err := ch.storage.UpdateCommentsUnified(repository, target, reviewComments); err != nil {
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
	target := models.NewPRTarget(prNumber)

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
	existingComments, err := ch.storage.GetCommentsUnified(repository, target)
	if err != nil && !strings.Contains(err.Error(), "not found") {
		return nil, fmt.Errorf("failed to get existing comments: %w", err)
	}

	// Merge with existing comments using specified strategy
	mergeResult := ch.mergeGitHubComments(filteredComments, existingComments.Comments, options.MergeStrategy)

	// Update local storage with merged results
	updatedComments := models.ReviewComments{
		Target:     target.String(),
		Repository: repository,
		Comments:   mergeResult.PulledComments,
		UpdatedAt:  time.Now(),
	}

	if err := ch.storage.UpdateCommentsUnified(repository, target, &updatedComments); err != nil {
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
