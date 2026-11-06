// Package github provides functionality to interact with GitHub using the gh CLI.
package github

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	executor "github.com/jaeyeom/go-cmdexec"
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

// ExecuteSeparated runs a command and returns stdout and stderr separately.
// A non-zero exit returns an error. Stderr is not appended to stdout.
func (e *executorAdapter) ExecuteSeparated(cmd string, args ...string) (string, string, error) {
	result, err := e.exec.Execute(e.ctx, executor.ToolConfig{
		Command: cmd,
		Args:    args,
	})
	if err != nil {
		return "", "", fmt.Errorf("failed to execute %s: %w", cmd, err)
	}
	if result == nil {
		return "", "", fmt.Errorf("failed to execute %s: empty result", cmd)
	}
	if result.ExitCode != 0 {
		return result.Output, result.Stderr, &executor.ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}
	return result.Output, result.Stderr, nil
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

// pullRequestListLimit caps how many pull requests are fetched. The gh CLI's
// `pr list` defaults to 30; we raise it well past that so users with many open
// PRs are not silently truncated (the previous `pr status` command was capped
// at 10 with no way to override).
const pullRequestListLimit = 100

// GetPendingPullRequests fetches open pull requests authored by the current user, excluding drafts.
// A non-zero gh exit, or stdout that is not a JSON array, is retried once.
// The pull request list is parsed from stdout only.
func (c *Client) GetPendingPullRequests() ([]models.PullRequest, error) {
	prs, _, err := c.pendingPullRequestsOnce()
	if err == nil {
		return prs, nil
	}
	prs, stderr, err := c.pendingPullRequestsOnce()
	if err == nil {
		return prs, nil
	}
	if stderr != "" {
		if _, printErr := fmt.Fprintln(os.Stderr, stderr); printErr != nil {
			err = errors.Join(err, printErr)
		}
		err = fmt.Errorf("%w\n%s", err, stderr)
	}
	return nil, err
}

func (c *Client) pendingPullRequestsOnce() ([]models.PullRequest, string, error) {
	// `pr list --author @me` replaces `pr status`, which is capped at 10 PRs.
	stdout, stderr, err := c.executeSeparated("gh", "pr", "list",
		"--author", "@me",
		"--limit", fmt.Sprintf("%d", pullRequestListLimit),
		"--json", "url,title,reviewRequests,latestReviews,files,mergeable,headRefName,statusCheckRollup,isDraft,labels")
	if err != nil {
		return nil, stderr, fmt.Errorf("failed to execute gh command: %w", err)
	}
	prs, err := parsePullRequestList(stdout)
	if err != nil {
		return nil, stderr, err
	}
	return withoutDrafts(prs), stderr, nil
}

func (c *Client) executeSeparated(cmd string, args ...string) (string, string, error) {
	type separated interface {
		ExecuteSeparated(cmd string, args ...string) (string, string, error)
	}
	if sep, ok := c.executor.(separated); ok {
		stdout, stderr, err := sep.ExecuteSeparated(cmd, args...)
		if err != nil {
			return stdout, stderr, fmt.Errorf("failed to execute %s: %w", cmd, err)
		}
		return stdout, stderr, nil
	}
	out, err := c.executor.Execute(cmd, args...)
	if err != nil {
		return "", out, fmt.Errorf("failed to execute %s: %w", cmd, err)
	}
	return out, "", nil
}

func parsePullRequestList(stdout string) ([]models.PullRequest, error) {
	trimmed := strings.TrimSpace(stdout)
	if !strings.HasPrefix(trimmed, "[") {
		return nil, fmt.Errorf("failed to parse gh command output: stdout is not a JSON array")
	}
	var prs []models.PullRequest
	if err := json.Unmarshal([]byte(trimmed), &prs); err != nil {
		return nil, fmt.Errorf("failed to parse gh command output: %w", err)
	}
	return prs, nil
}

func withoutDrafts(prs []models.PullRequest) []models.PullRequest {
	pending := make([]models.PullRequest, 0, len(prs))
	for _, pr := range prs {
		if pr.IsDraft {
			continue
		}
		pending = append(pending, pr)
	}
	return pending
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
