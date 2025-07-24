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
	BlobURL          string `json:"blob_url"`
	ContentsURL      string `json:"contents_url"`
	RawURL           string `json:"raw_url"`
	PreviousFilename string `json:"previous_filename,omitempty"`
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
func (prc *PRReviewClient) GetPRDiff(owner, repo string, prNumber int) (*models.PRDiffHunks, error) {
	// Get PR files via GitHub API
	cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/%s/pulls/%d/files", owner, repo, prNumber)) //nolint:gosec // Intentional subprocess execution with gh CLI
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch PR files: %w", err)
	}

	var files []File
	if err := json.Unmarshal(output, &files); err != nil {
		return nil, fmt.Errorf("failed to parse PR files response: %w", err)
	}

	// Get basic PR info for SHA values
	prInfo, err := prc.GetPRInfo(owner, repo, prNumber)
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
		Owner:      owner,
		Repo:       repo,
		CapturedAt: time.Now(),
		DiffHunks:  diffHunks,
		CommitSHA:  prInfo.Head.SHA,
		BaseSHA:    prInfo.Base.SHA,
	}, nil
}

// GetPRInfo fetches basic information about a pull request.
func (prc *PRReviewClient) GetPRInfo(owner, repo string, prNumber int) (*PR, error) {
	cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/%s/pulls/%d", owner, repo, prNumber)) //nolint:gosec // Intentional subprocess execution with gh CLI
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
func (prc *PRReviewClient) ValidatePRAccess(owner, repo string, prNumber int) error {
	_, err := prc.GetPRInfo(owner, repo, prNumber)
	return err
}

// SubmitReview submits a review to GitHub.
func (prc *PRReviewClient) SubmitReview(owner, repo string, prNumber int, review models.PRReview) error {
	// Convert our comment format to GitHub's format
	githubComments := make([]map[string]interface{}, len(review.Comments))
	for i, comment := range review.Comments {
		githubComment := map[string]interface{}{
			"path": comment.Path,
			"body": comment.Body,
			"side": comment.Side,
		}

		if comment.IsMultiLine() {
			githubComment["start_line"] = *comment.StartLine
			githubComment["line"] = comment.Line
		} else {
			githubComment["line"] = comment.Line
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
		fmt.Sprintf("/repos/%s/%s/pulls/%d/reviews", owner, repo, prNumber),
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

	var hunks []models.DiffHunk
	lines := strings.Split(patch, "\n")

	var currentHunk *models.DiffHunk
	var rightLineNum, leftLineNum int

	for _, line := range lines {
		// Parse hunk header (e.g., "@@ -1,4 +1,6 @@")
		if strings.HasPrefix(line, "@@") {
			if currentHunk != nil {
				hunks = append(hunks, *currentHunk)
			}

			// Parse line numbers from hunk header
			parts := strings.Fields(line)
			if len(parts) >= 3 {
				// Parse left side (old file)
				if leftPart := parts[1]; strings.HasPrefix(leftPart, "-") {
					leftNumbers := strings.Split(leftPart[1:], ",")
					if len(leftNumbers) > 0 {
						if num, err := strconv.Atoi(leftNumbers[0]); err == nil {
							leftLineNum = num
						}
					}
				}

				// Parse right side (new file)
				if rightPart := parts[2]; strings.HasPrefix(rightPart, "+") {
					rightNumbers := strings.Split(rightPart[1:], ",")
					if len(rightNumbers) > 0 {
						if num, err := strconv.Atoi(rightNumbers[0]); err == nil {
							rightLineNum = num
						}
					}
				}
			}

			// Start new hunk
			currentHunk = &models.DiffHunk{
				File:      filename,
				SHA:       sha,
				StartLine: rightLineNum,
				EndLine:   rightLineNum,
				Content:   line + "\n",
				Side:      "RIGHT", // Default to RIGHT side
			}
			continue
		}

		if currentHunk == nil {
			continue
		}

		// Add line to current hunk content
		currentHunk.Content += line + "\n"

		// Track line numbers
		switch {
		case strings.HasPrefix(line, "+"):
			currentHunk.EndLine = rightLineNum
			rightLineNum++
		case strings.HasPrefix(line, "-"):
			leftLineNum++
		case strings.HasPrefix(line, " "):
			// Context line, present in both sides
			currentHunk.EndLine = rightLineNum
			rightLineNum++
			leftLineNum++
		}
	}

	// Add the last hunk
	if currentHunk != nil {
		hunks = append(hunks, *currentHunk)
	}

	// Create separate hunks for LEFT side (deletions)
	var allHunks []models.DiffHunk
	for _, hunk := range hunks {
		// Add the RIGHT side hunk
		allHunks = append(allHunks, hunk)

		// Check if there are deletions to create a LEFT side hunk
		if strings.Contains(hunk.Content, "\n-") {
			leftHunk := hunk
			leftHunk.Side = "LEFT"
			// Note: We'd need more sophisticated parsing to get accurate LEFT side line numbers
			// For now, we'll use the same range which is not entirely accurate
			allHunks = append(allHunks, leftHunk)
		}
	}

	return allHunks
}

// CreatePendingReview creates a pending review (draft).
func (prc *PRReviewClient) CreatePendingReview(owner, repo string, prNumber int, comments []models.Comment, body string) error {
	review := models.PRReview{
		Body:     body,
		Comments: comments,
		// Event is omitted to create a pending review
	}

	return prc.SubmitReview(owner, repo, prNumber, review)
}

// GetExistingReviews fetches existing reviews for a PR.
func (prc *PRReviewClient) GetExistingReviews(owner, repo string, prNumber int) ([]map[string]interface{}, error) {
	cmd := exec.Command("gh", "api", fmt.Sprintf("/repos/%s/%s/pulls/%d/reviews", owner, repo, prNumber)) //nolint:gosec // Intentional subprocess execution with gh CLI
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
