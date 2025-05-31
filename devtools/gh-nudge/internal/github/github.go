// Package github provides functionality to interact with GitHub using the gh CLI.
package github

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// CommandExecutor defines an interface for executing shell commands.
// This allows for easier testing by mocking command execution.
type CommandExecutor interface {
	Execute(cmd string, args ...string) (string, error)
}

// DefaultExecutor is the default implementation of CommandExecutor.
type DefaultExecutor struct{}

// Execute runs a command with the given arguments and returns its output.
func (e *DefaultExecutor) Execute(cmd string, args ...string) (string, error) {
	command := exec.Command(cmd, args...)
	output, err := command.CombinedOutput()
	return string(output), err
}

// Client provides methods to interact with GitHub.
type Client struct {
	executor CommandExecutor
}

// NewClient creates a new GitHub client with the given executor.
// If nil is provided, it uses the DefaultExecutor.
func NewClient(executor CommandExecutor) *Client {
	if executor == nil {
		executor = &DefaultExecutor{}
	}
	return &Client{executor: executor}
}

// GetPendingPullRequests fetches pending pull requests using the gh CLI.
// It fetches PRs created by the current user.
func (c *Client) GetPendingPullRequests() ([]models.PullRequest, error) {
	// Construct the gh command to get PR information
	// This command fetches PRs with their title, URL, review requests, files, and baseRefName
	output, err := c.executor.Execute("gh", "pr", "status", "--json", "url,title,reviewRequests,files,mergeable,headRefName,baseRefName", "-q", ".createdBy")
	if err != nil {
		return nil, fmt.Errorf("failed to execute gh command: %w", err)
	}

	// Parse the JSON output
	var prs []models.PullRequest
	if err := json.Unmarshal([]byte(output), &prs); err != nil {
		return nil, fmt.Errorf("failed to parse gh command output: %w", err)
	}

	return prs, nil
}

// GetMergeablePullRequests fetches pull requests with no review requests.
func (c *Client) GetMergeablePullRequests() ([]models.PullRequest, error) {
	prs, err := c.GetPendingPullRequests()
	if err != nil {
		return nil, err
	}

	var mergeablePRs []models.PullRequest
	for _, pr := range prs {
		if len(pr.ReviewRequests) == 0 && pr.Mergeable == "MERGEABLE" {
			mergeablePRs = append(mergeablePRs, pr)
		}
	}

	return mergeablePRs, nil
}

// FilterPullRequestsByAge filters pull requests by their age.
// This is a placeholder for future implementation.
func (c *Client) FilterPullRequestsByAge(prs []models.PullRequest, hours int) []models.PullRequest {
	// In a real implementation, we would fetch PR creation time and filter by age
	// For now, this is just a placeholder
	return prs
}

// GetPullRequestDetails fetches additional details for a pull request.
// This is a placeholder for future implementation.
func (c *Client) GetPullRequestDetails(pr models.PullRequest) (models.PullRequest, error) {
	// In a real implementation, we would fetch additional details if needed
	return pr, nil
}

// MergePullRequest merges a specific pull request.
// If deleteBranch is true, it will delete the source branch after merging.
func (c *Client) MergePullRequest(prURL string, deleteBranch bool) error {
	args := []string{"pr", "merge", prURL}

	if deleteBranch {
		args = append(args, "--delete-branch")
	}

	output, err := c.executor.Execute("gh", args...)
	if err != nil {
		return fmt.Errorf("failed to merge PR %s: %w\nOutput: %s",
			prURL, err, output)
	}

	return nil
}

// GetFileContent fetches the content of a file from a GitHub repository.
// It uses the `gh api` command to retrieve the file content.
func (c *Client) GetFileContent(owner, repo, path, ref string) (string, error) {
	// Construct the API endpoint path
	apiPath := fmt.Sprintf("repos/%s/%s/contents/%s", owner, repo, path)
	args := []string{"api", apiPath}
	if ref != "" {
		args = append(args, "-q", fmt.Sprintf("ref=%s", ref))
	}

	// Execute the gh api command
	// Example: gh api repos/owner/repo/contents/path/to/file.txt?ref=main
	output, err := c.executor.Execute("gh", args...)
	if err != nil {
		// Check if the error is due to file not found (404)
		// gh api returns non-zero exit code for HTTP errors, and output might contain error message
		if strings.Contains(output, "\"Not Found\"") {
			return "", fmt.Errorf("file not found: %s/%s/%s at ref %s (gh api output: %s): %w", owner, repo, path, ref, output, err)
		}
		return "", fmt.Errorf("failed to execute gh api command for file content: %s (output: %s): %w", apiPath, output, err)
	}

	// Define a struct to parse the JSON response from GitHub API
	var fileData struct {
		Content  string `json:"content"`
		Encoding string `json:"encoding"`
		Message  string `json:"message"` // For error messages like "Not Found"
	}

	// Parse the JSON output
	if err := json.Unmarshal([]byte(output), &fileData); err != nil {
		return "", fmt.Errorf("failed to parse gh api JSON output for file content: %s (output: %s): %w", apiPath, output, err)
	}

	// Check if the API returned an error message (e.g. file not found, though usually caught by exit code)
	if fileData.Message != "" && fileData.Content == "" {
		return "", fmt.Errorf("failed to get file content: %s from %s/%s/%s at ref %s: %s", fileData.Message, owner, repo, path, ref)
	}


	// Ensure the content is base64 encoded as expected
	if fileData.Encoding != "base64" {
		return "", fmt.Errorf("unexpected content encoding for %s: expected 'base64', got '%s'", path, fileData.Encoding)
	}

	// Decode the base64 content
	decodedContent, err := base64.StdEncoding.DecodeString(fileData.Content)
	if err != nil {
		return "", fmt.Errorf("failed to decode base64 content for %s: %w", path, err)
	}

	return string(decodedContent), nil
}
