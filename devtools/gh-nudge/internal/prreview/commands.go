package prreview

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

// OutputFormatter defines the interface for formatting output.
type OutputFormatter interface {
	FormatSubmitResult(result models.SubmitResult) (string, error)
	FormatComments(comments []models.Comment) (string, error)
	FormatCommentMatches(matches []models.CommentMatch, line int) (string, error)
}

// CommandHandler handles gh-pr-review commands.
type CommandHandler struct {
	storage     *storage.GitHubStorage
	ghClient    *github.PRReviewClient
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

	return &CommandHandler{
		storage:     ghStorage,
		ghClient:    prClient,
		storageHome: storageHome,
	}, nil
}

// CaptureCommand captures and stores PR diff hunks.
func (ch *CommandHandler) CaptureCommand(owner, repo string, prNumber int, force bool) error {
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

// CommentCommand adds a line-specific comment.
func (ch *CommandHandler) CommentCommand(owner, repo string, prNumber int, file string, lineSpec, commentBody, side string, force bool) error {
	// Parse line specification
	lineRange, err := models.ParseLineSpec(lineSpec)
	if err != nil {
		return fmt.Errorf("invalid line specification '%s': %w", lineSpec, err)
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

// SubmitCommand submits a review to GitHub.
func (ch *CommandHandler) SubmitCommand(owner, repo string, prNumber int, body, event string, formatter OutputFormatter, postSubmitAction models.Executor) error {
	// Get comments
	prComments, err := ch.storage.GetComments(owner, repo, prNumber)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	if len(prComments.Comments) == 0 {
		return fmt.Errorf("no comments found for PR %s/%s#%d", owner, repo, prNumber)
	}

	review := models.PRReview{
		Body:     body,
		Event:    event,
		Comments: prComments.Comments,
	}

	if err := ch.ghClient.SubmitReview(owner, repo, prNumber, review); err != nil {
		return fmt.Errorf("failed to submit review: %w", err)
	}

	result := models.SubmitResult{
		Status:      "success",
		PRNumber:    prNumber,
		Owner:       owner,
		Repo:        repo,
		Comments:    len(review.Comments),
		SubmittedAt: time.Now(),
	}

	// Handle post-submit action
	if err := postSubmitAction.Execute(ch.storage, owner, repo, prNumber); err != nil {
		return fmt.Errorf("post-submit action failed: %w", err)
	}

	// Add action info to result
	result.PostSubmitAction = postSubmitAction.Name()

	// Format and output result
	output, err := formatter.FormatSubmitResult(result)
	if err != nil {
		return fmt.Errorf("failed to format submit result: %w", err)
	}
	fmt.Println(output)

	return nil
}

// ListCommand lists stored comments.
func (ch *CommandHandler) ListCommand(owner, repo string, prNumber int, formatter OutputFormatter, file, line, side string) error {
	// Get comments
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
				return fmt.Errorf("invalid line filter '%s': %w", line, err)
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

// DeleteCommand deletes specific comments.
func (ch *CommandHandler) DeleteCommand(owner, repo string, prNumber int, file, lineSpec, side string, all bool, index *int, confirm bool, formatter OutputFormatter) error {
	lineRange, err := models.ParseLineSpec(lineSpec)
	if err != nil {
		return fmt.Errorf("invalid line specification '%s': %w", lineSpec, err)
	}

	// Handle single line deletion
	if lineRange.StartLine == lineRange.EndLine {
		return ch.deleteSingleLineComments(owner, repo, prNumber, file, lineRange.StartLine, side, all, index, confirm, formatter)
	}

	// Handle range deletion
	return ch.deleteRangeComments(owner, repo, prNumber, file, lineRange.StartLine, lineRange.EndLine, side, confirm, formatter)
}

// ClearCommand clears comments for a PR or file.
func (ch *CommandHandler) ClearCommand(owner, repo string, prNumber int, file string, confirm bool) error {
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

func (ch *CommandHandler) deleteSingleLineComments(owner, repo string, prNumber int, file string, line int, side string, all bool, index *int, confirm bool, formatter OutputFormatter) error {
	// Find comments on the line
	matches, err := ch.storage.FindCommentsOnLine(owner, repo, prNumber, file, line, side)
	if err != nil {
		return fmt.Errorf("failed to find comments: %w", err)
	}

	if len(matches) == 0 {
		return fmt.Errorf("no comments found on line %d", line)
	}

	// Handle different deletion modes
	switch {
	case index != nil:
		// Delete specific index
		if err := ch.storage.DeleteCommentByIndex(owner, repo, prNumber, file, line, side, *index); err != nil {
			return fmt.Errorf("failed to delete comment at index %d: %w", *index, err)
		}
		fmt.Printf("Deleted comment at index %d on line %d\n", *index, line)
	case all:
		// Delete all comments on line
		if err := ch.storage.DeleteAllCommentsOnLine(owner, repo, prNumber, file, line, side); err != nil {
			return fmt.Errorf("failed to delete all comments: %w", err)
		}
		fmt.Printf("Deleted %d comments on line %d\n", len(matches), line)
	case len(matches) == 1:
		// Delete single comment
		if err := ch.storage.DeleteComment(owner, repo, prNumber, file, line, side); err != nil {
			return fmt.Errorf("failed to delete comment: %w", err)
		}
		fmt.Printf("Deleted comment on line %d\n", line)
	default:
		// Multiple comments found, show options
		output, err := formatter.FormatCommentMatches(matches, line)
		if err != nil {
			return fmt.Errorf("failed to format matches: %w", err)
		}
		fmt.Println(output)
		return fmt.Errorf("multiple comments found, specify --index or --all")
	}

	return nil
}

func (ch *CommandHandler) deleteRangeComments(owner, repo string, prNumber int, file string, startLine, endLine int, side string, confirm bool, formatter OutputFormatter) error {
	if !confirm {
		fmt.Printf("This will delete all comments in range %d-%d. Continue? (y/N): ", startLine, endLine)
		var response string
		_, _ = fmt.Scanln(&response)
		if strings.ToLower(response) != "y" && strings.ToLower(response) != "yes" {
			fmt.Println("Operation cancelled")
			return nil
		}
	}

	if err := ch.storage.DeleteCommentsInRange(owner, repo, prNumber, file, startLine, endLine, side); err != nil {
		return fmt.Errorf("failed to delete comments in range: %w", err)
	}

	fmt.Printf("Deleted comments in range %d-%d\n", startLine, endLine)
	return nil
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
