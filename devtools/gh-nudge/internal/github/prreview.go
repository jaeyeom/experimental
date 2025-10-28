package github

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// PRReviewClient handles GitHub PR review operations via the gh CLI.
type PRReviewClient struct {
	client *Client
}

// NewPRReviewClient creates a new PR review client.
func NewPRReviewClient(client *Client) *PRReviewClient {
	return &PRReviewClient{
		client: client,
	}
}

// File represents a file in a GitHub PR diff.
type File struct {
	Filename         string `json:"filename"`
	Patch            string `json:"patch"`
	SHA              string `json:"sha"`
	Status           string `json:"status"`
	Additions        int    `json:"additions"`
	Deletions        int    `json:"deletions"`
	Changes          int    `json:"changes"`
	BlobURL          string `json:"blobUrl"`
	ContentsURL      string `json:"contentsUrl"`
	RawURL           string `json:"rawUrl"`
	PreviousFilename string `json:"previousFilename,omitempty"`
}

// PR represents basic PR information.
type PR struct {
	Number int    `json:"number"`
	Title  string `json:"title"`
	Head   struct {
		SHA string `json:"sha"`
	} `json:"head"`
	Base struct {
		SHA string `json:"sha"`
	} `json:"base"`
}

// GetPRDiff fetches the diff for a pull request.
func (prc *PRReviewClient) GetPRDiff(repository models.Repository, prNumber int) (*models.PRDiffHunks, error) {
	// Get PR files via GitHub API
	cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/pulls/%d/files", repository, prNumber)) //nolint:gosec // Intentional subprocess execution with gh CLI
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch PR files: %w", err)
	}

	var files []File
	if err := json.Unmarshal(output, &files); err != nil {
		return nil, fmt.Errorf("failed to parse PR files response: %w", err)
	}

	// Get basic PR info for SHA values
	prInfo, err := prc.GetPRInfo(repository, prNumber)
	if err != nil {
		return nil, fmt.Errorf("failed to get PR info: %w", err)
	}

	// Convert GitHub files to diff hunks
	var diffHunks []models.DiffHunk
	for _, file := range files {
		hunks := prc.parsePatchToDiffHunks(file.Filename, file.Patch, file.SHA)
		diffHunks = append(diffHunks, hunks...)
	}

	return &models.PRDiffHunks{
		PRNumber:   prNumber,
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks:  diffHunks,
		CommitSHA:  prInfo.Head.SHA,
		BaseSHA:    prInfo.Base.SHA,
	}, nil
}

// GetPRInfo fetches basic information about a pull request.
func (prc *PRReviewClient) GetPRInfo(repository models.Repository, prNumber int) (*PR, error) {
	cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/pulls/%d", repository, prNumber)) //nolint:gosec // Intentional subprocess execution with gh CLI
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch PR info: %w", err)
	}

	var pr PR
	if err := json.Unmarshal(output, &pr); err != nil {
		return nil, fmt.Errorf("failed to parse PR info response: %w", err)
	}

	return &pr, nil
}

// ValidatePRAccess checks if the PR is accessible.
func (prc *PRReviewClient) ValidatePRAccess(repository models.Repository, prNumber int) error {
	_, err := prc.GetPRInfo(repository, prNumber)
	return err
}

// SubmitReview submits a review to GitHub.
func (prc *PRReviewClient) SubmitReview(repository models.Repository, prNumber int, review models.PRReview) error {
	// Convert our comment format to GitHub's format
	//
	// TODO: Investigate why this does not use GitHubComment struct.
	githubComments := make([]map[string]interface{}, len(review.Comments))
	for i, comment := range review.Comments {
		githubComment := map[string]interface{}{
			"path": comment.Path,
			"body": comment.Body,
			"side": comment.Side,
		}

		if comment.IsMultiLine() {
			githubComment["start_line"] = comment.Line.StartLine
			githubComment["line"] = comment.Line.EndLine
		} else {
			githubComment["line"] = comment.Line.EndLine
		}

		if comment.SHA != "" {
			githubComment["commit_id"] = comment.SHA
		}

		githubComments[i] = githubComment
	}

	// Prepare the review payload
	reviewPayload := map[string]interface{}{
		"comments": githubComments,
	}

	if review.Body != "" {
		reviewPayload["body"] = review.Body
	}

	if review.Event != "" {
		reviewPayload["event"] = review.Event
	}

	// Convert to JSON
	payloadBytes, err := json.Marshal(reviewPayload)
	if err != nil {
		return fmt.Errorf("failed to marshal review payload: %w", err)
	}

	// Submit via gh CLI
	cmd := exec.Command("gh", "api", "-X", "POST", //nolint:gosec // Intentional subprocess execution with gh CLI
		fmt.Sprintf("/repos/%s/pulls/%d/reviews", repository, prNumber),
		"--input", "-")
	cmd.Stdin = strings.NewReader(string(payloadBytes))

	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to submit review: %w (output: %s)", err, string(output))
	}

	return nil
}

// parsePatchToDiffHunks converts a GitHub patch string to diff hunks.
func (prc *PRReviewClient) parsePatchToDiffHunks(filename, patch, sha string) []models.DiffHunk {
	if patch == "" {
		return []models.DiffHunk{}
	}

	lines := strings.Split(patch, "\n")
	hunks := prc.parseHunksFromLines(lines, filename, sha)
	return prc.createBidirectionalHunks(hunks)
}

func (prc *PRReviewClient) parseHunksFromLines(lines []string, filename, sha string) []models.DiffHunk {
	var hunks []models.DiffHunk
	var currentHunk *models.DiffHunk
	var rightLineNum, leftLineNum int

	for _, line := range lines {
		if strings.HasPrefix(line, "@@") {
			if currentHunk != nil {
				hunks = append(hunks, *currentHunk)
			}
			leftLineNum, rightLineNum = prc.parseHunkHeader(line)
			currentHunk = prc.createNewHunk(filename, sha, line, rightLineNum)
			continue
		}

		if currentHunk == nil {
			continue
		}

		currentHunk.Content += line + "\n"
		rightLineNum, leftLineNum = prc.updateLineNumbers(line, rightLineNum, leftLineNum, currentHunk)
	}

	if currentHunk != nil {
		hunks = append(hunks, *currentHunk)
	}

	return hunks
}

func (prc *PRReviewClient) parseHunkHeader(line string) (int, int) {
	parts := strings.Fields(line)
	if len(parts) < 3 {
		return 0, 0
	}

	leftLineNum := prc.parseLineNumber(parts[1], "-")
	rightLineNum := prc.parseLineNumber(parts[2], "+")

	return leftLineNum, rightLineNum
}

func (prc *PRReviewClient) parseLineNumber(part, prefix string) int {
	if !strings.HasPrefix(part, prefix) {
		return 0
	}

	numbers := strings.Split(part[1:], ",")
	if len(numbers) == 0 {
		return 0
	}

	if num, err := strconv.Atoi(numbers[0]); err == nil {
		return num
	}
	return 0
}

func (prc *PRReviewClient) createNewHunk(filename, sha, line string, rightLineNum int) *models.DiffHunk {
	return &models.DiffHunk{
		File:    filename,
		SHA:     sha,
		Range:   models.NewSingleLine(rightLineNum),
		Content: line + "\n",
		Side:    models.SideRight,
	}
}

func (prc *PRReviewClient) updateLineNumbers(line string, rightLineNum, leftLineNum int, currentHunk *models.DiffHunk) (int, int) {
	switch {
	case strings.HasPrefix(line, "+"):
		currentHunk.Range.EndLine = rightLineNum
		rightLineNum++
	case strings.HasPrefix(line, "-"):
		leftLineNum++
	case strings.HasPrefix(line, " "):
		currentHunk.Range.EndLine = rightLineNum
		rightLineNum++
		leftLineNum++
	}
	return rightLineNum, leftLineNum
}

func (prc *PRReviewClient) createBidirectionalHunks(hunks []models.DiffHunk) []models.DiffHunk {
	var allHunks []models.DiffHunk
	for _, hunk := range hunks {
		allHunks = append(allHunks, hunk)

		if strings.Contains(hunk.Content, "\n-") {
			leftHunk := hunk
			leftHunk.Side = models.SideLeft
			allHunks = append(allHunks, leftHunk)
		}
	}
	return allHunks
}

// CreatePendingReview creates a pending review (draft).
func (prc *PRReviewClient) CreatePendingReview(repository models.Repository, prNumber int, comments []models.Comment, body string) error {
	review := models.PRReview{
		Body:     body,
		Comments: comments,
		// Event is omitted to create a pending review
	}

	return prc.SubmitReview(repository, prNumber, review)
}

// GetExistingReviews fetches existing reviews for a PR.
func (prc *PRReviewClient) GetExistingReviews(repository models.Repository, prNumber int) ([]map[string]interface{}, error) {
	cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/pulls/%d/reviews", repository, prNumber)) //nolint:gosec // Intentional subprocess execution with gh CLI
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch existing reviews: %w", err)
	}

	var reviews []map[string]interface{}
	if err := json.Unmarshal(output, &reviews); err != nil {
		return nil, fmt.Errorf("failed to parse reviews response: %w", err)
	}

	return reviews, nil
}

//nolint:revive // GitHubComment is appropriate naming for this context
type GitHubComment struct {
	ID                int64       `json:"id"`
	Path              string      `json:"path"`
	Position          *int        `json:"position"`
	OriginalPosition  *int        `json:"original_position"` //nolint:tagliatelle // GitHub API uses snake_case
	Line              *int        `json:"line"`
	OriginalLine      *int        `json:"original_line"`       //nolint:tagliatelle // GitHub API uses snake_case
	StartLine         *int        `json:"start_line"`          //nolint:tagliatelle // GitHub API uses snake_case
	OriginalStartLine *int        `json:"original_start_line"` //nolint:tagliatelle // GitHub API uses snake_case
	Body              string      `json:"body"`
	Side              models.Side `json:"side"`
	CommitID          string      `json:"commit_id"`  //nolint:tagliatelle // GitHub API uses snake_case
	CreatedAt         time.Time   `json:"created_at"` //nolint:tagliatelle // GitHub API uses snake_case
	UpdatedAt         time.Time   `json:"updated_at"` //nolint:tagliatelle // GitHub API uses snake_case
	User              struct {
		Login string `json:"login"`
	} `json:"user"`
}

// GetPRComments fetches all line comments for a PR from GitHub.
func (prc *PRReviewClient) GetPRComments(repository models.Repository, prNumber int) ([]models.Comment, error) {
	cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/pulls/%d/comments", repository, prNumber)) //nolint:gosec // Intentional subprocess execution with gh CLI
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch PR comments: %w", err)
	}

	var githubComments []GitHubComment
	if err := json.Unmarshal(output, &githubComments); err != nil {
		return nil, fmt.Errorf("failed to parse comments response: %w", err)
	}

	// Convert GitHub comments to local comment format
	var comments []models.Comment
	for _, ghComment := range githubComments {
		localComment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			fmt.Printf("Warning: Failed to convert comment %d: %v\n", ghComment.ID, err)
			continue
		}
		comments = append(comments, localComment)
	}

	return comments, nil
}

// GetPRReviewComments fetches comments from all reviews for a PR.
func (prc *PRReviewClient) GetPRReviewComments(repository models.Repository, prNumber int) ([]models.Comment, error) {
	// Get all reviews
	reviews, err := prc.GetExistingReviews(repository, prNumber)
	if err != nil {
		return nil, fmt.Errorf("failed to get reviews: %w", err)
	}

	var allComments []models.Comment

	// Get comments from each review
	for _, review := range reviews {
		reviewID, ok := review["id"].(float64)
		if !ok {
			continue
		}

		cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/pulls/%d/reviews/%d/comments", repository, prNumber, int64(reviewID))) //nolint:gosec // Intentional subprocess execution with gh CLI
		output, err := cmd.Output()
		if err != nil {
			fmt.Printf("Warning: Failed to fetch comments for review %d: %v\n", int64(reviewID), err)
			continue
		}

		var githubComments []GitHubComment
		if err := json.Unmarshal(output, &githubComments); err != nil {
			fmt.Printf("Warning: Failed to parse comments for review %d: %v\n", int64(reviewID), err)
			continue
		}

		// Convert and add comments
		for _, ghComment := range githubComments {
			localComment, err := prc.convertGitHubComment(ghComment)
			if err != nil {
				fmt.Printf("Warning: Failed to convert comment %d: %v\n", ghComment.ID, err)
				continue
			}
			allComments = append(allComments, localComment)
		}
	}

	return allComments, nil
}

// convertGitHubComment converts GitHub API comment to local Comment model.
func (prc *PRReviewClient) convertGitHubComment(ghComment GitHubComment) (models.Comment, error) {
	now := time.Now()

	// Parse and validate side - default to RIGHT if unspecified
	side := ghComment.Side
	if side == models.SideUnspecified {
		side = models.SideRight
	}

	comment := models.Comment{
		ID:         models.GenerateCommentID(), // Generate new local ID
		Path:       ghComment.Path,
		Body:       ghComment.Body,
		Side:       side,
		SHA:        ghComment.CommitID,
		CreatedAt:  ghComment.CreatedAt,
		Status:     models.StatusUnresolved,
		Source:     "github",
		GitHubID:   &ghComment.ID,
		LastSynced: &now,
		SyncStatus: "synced",
	}

	// Handle line numbers - GitHub API can use different fields
	//
	// TODO: Consider implement ghComment.GetLineRange() and maybe ghComment.IsMultiLine().
	var endLine int
	switch {
	case ghComment.Line != nil:
		endLine = *ghComment.Line
	case ghComment.OriginalLine != nil:
		endLine = *ghComment.OriginalLine
	default:
		return models.Comment{}, fmt.Errorf("comment has no line number")
	}

	// Handle multi-line comments
	if ghComment.StartLine != nil && *ghComment.StartLine != endLine {
		comment.Line = models.NewLineRange(*ghComment.StartLine, endLine)
	} else {
		comment.Line = models.NewSingleLine(endLine)
	}

	// Handle original position for multi-line comments
	if ghComment.OriginalStartLine != nil && ghComment.OriginalLine != nil && *ghComment.OriginalStartLine != *ghComment.OriginalLine {
		origRange := models.NewLineRange(*ghComment.OriginalStartLine, *ghComment.OriginalLine)
		comment.OriginalRange = &origRange
	} else if ghComment.OriginalLine != nil {
		origRange := models.NewSingleLine(*ghComment.OriginalLine)
		comment.OriginalRange = &origRange
	}

	return comment, nil
}
