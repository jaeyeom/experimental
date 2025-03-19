// Package github provides functionality to interact with GitHub using the gh CLI.
package github

import (
	"encoding/json"
	"fmt"
	"os/exec"

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
func (c *Client) GetPendingPullRequests() ([]models.PullRequest, error) {
	// Construct the gh command to get PR information
	// This command fetches PRs with their title, URL, review requests, and files
	output, err := c.executor.Execute("gh", "pr", "status", "--json", "url,title,reviewRequests,files", "-q", ".createdBy")
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
