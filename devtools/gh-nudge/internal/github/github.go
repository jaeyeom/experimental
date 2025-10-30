// Package github provides functionality to interact with GitHub using the gh CLI.
package github

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

// CommandExecutor defines an interface for executing shell commands.
// This allows for easier testing by mocking command execution.
type CommandExecutor interface {
	Execute(cmd string, args ...string) (string, error)
	ExecuteWithStdin(stdin, cmd string, args ...string) (string, error)
}

// executorAdapter adapts our shared executor.Executor to CommandExecutor interface.
type executorAdapter struct {
	exec executor.Executor
	ctx  context.Context
}

// Execute runs a command with the given arguments and returns its combined output.
func (e *executorAdapter) Execute(cmd string, args ...string) (string, error) {
	output, err := executor.CombinedOutput(e.ctx, e.exec, cmd, args...)
	return string(output), err
}

// ExecuteWithStdin runs a command with stdin input and returns combined output.
func (e *executorAdapter) ExecuteWithStdin(stdin, cmd string, args ...string) (string, error) {
	output, err := executor.CombinedOutputWithStdin(e.ctx, e.exec, stdin, cmd, args...)
	return string(output), err
}

// Client provides methods to interact with GitHub.
type Client struct {
	executor CommandExecutor
}

// NewClient creates a new GitHub client with the shared executor.
func NewClient(ctx context.Context, exec executor.Executor) *Client {
	return &Client{
		executor: &executorAdapter{
			exec: exec,
			ctx:  ctx,
		},
	}
}

// NewClientWithExecutor creates a client with a custom CommandExecutor (for backward compatibility).
func NewClientWithExecutor(executor CommandExecutor) *Client {
	return &Client{executor: executor}
}

// GetPendingPullRequests fetches pending pull requests using the gh CLI.
// It fetches PRs created by the current user.
func (c *Client) GetPendingPullRequests() ([]models.PullRequest, error) {
	// Construct the gh command to get PR information
	// This command fetches PRs with their title, URL, review requests, and files
	output, err := c.executor.Execute("gh", "pr", "status", "--json", "url,title,reviewRequests,files,mergeable,headRefName", "-q", ".createdBy")
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
	_ = hours // TODO: implement age filtering
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
