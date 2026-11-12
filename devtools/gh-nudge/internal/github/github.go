// Package github provides functionality to interact with GitHub using the gh CLI.
package github

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"
	"time"

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
	// sleep and warn default to time.Sleep and slog.Warn when nil.
	sleep func(time.Duration)
	warn  func(msg string, args ...any)

	// mu guards the per-run detail caches, keyed by pull request URL.
	mu     sync.Mutex
	checks map[string][]models.StatusCheck
	files  map[string][]models.File
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

// GetAuthoredPullRequests fetches open pull requests authored by the current user, including drafts.
// A non-zero gh exit is retried once, and so is an exit 0 with empty stdout.
// The pull request list is parsed from stdout only.
// It requests gh-merge's list fields only. Status checks come from StatusChecks.
func (c *Client) GetAuthoredPullRequests() ([]models.PullRequest, error) {
	return c.fetchPullRequests(mergeListFields, false)
}

// GetPendingPullRequests fetches open pull requests authored by the current user, excluding drafts.
// A non-zero gh exit is retried once, and so is an exit 0 with empty stdout.
// The pull request list is parsed from stdout only.
// It requests gh-nudge's list fields only. Changed files come from PullRequestFiles.
func (c *Client) GetPendingPullRequests() ([]models.PullRequest, error) {
	return c.fetchPullRequests(nudgeListFields, true)
}

// The list fields are cheap scalars. files and statusCheckRollup cost
// GraphQL time for every open pull request, so they are fetched per pull
// request, and only for the ones a command acts on.
const (
	// mergeListFields are what gh-merge's list and merge filters and output read.
	mergeListFields = "url,title,isDraft,labels,mergeable,headRefName,reviewRequests"
	// nudgeListFields are what gh-nudge's filters and reviewer logic read.
	nudgeListFields = "url,title,isDraft,labels,reviewRequests,latestReviews"
)

func (c *Client) fetchPullRequests(fields string, skipDrafts bool) ([]models.PullRequest, error) {
	// `pr list --author @me` replaces `pr status`, which is capped at 10 PRs.
	args := []string{
		"pr", "list",
		"--author", "@me",
		"--limit", fmt.Sprintf("%d", pullRequestListLimit),
		"--json", fields,
	}
	out, err := c.runGHJSON(args...)
	if err != nil {
		out, err = c.runGHJSON(args...)
	}
	if err != nil {
		err = fmt.Errorf("failed to execute gh command: %w", err)
		if out.stderr != "" {
			if _, printErr := fmt.Fprintln(os.Stderr, out.stderr); printErr != nil {
				err = errors.Join(err, printErr)
			}
			err = fmt.Errorf("%w\n%s", err, out.stderr)
		}
		return nil, err
	}
	prs, err := parsePullRequestList(out)
	if err != nil {
		return nil, err
	}
	return filterDrafts(prs, skipDrafts), nil
}

func filterDrafts(prs []models.PullRequest, skip bool) []models.PullRequest {
	if !skip {
		return prs
	}
	return withoutDrafts(prs)
}

// MergeableState reads one pull request's mergeable field from gh.
// Stdout is parsed alone. Stderr is not part of the value.
func (c *Client) MergeableState(prURL string) (string, error) {
	out, err := c.runGHJSON("pr", "view", prURL, "--json", "mergeable")
	if err != nil {
		return "", fmt.Errorf("failed to read mergeable state: %w", err)
	}
	var body struct {
		Mergeable string `json:"mergeable"`
	}
	if err := out.decode(&body); err != nil {
		return "", fmt.Errorf("failed to parse mergeable state: %w", err)
	}
	return body.Mergeable, nil
}

// StatusChecks reads one pull request's statusCheckRollup from gh.
// A successful result is cached on the client, so each pull request is
// fetched at most once per run. Errors are not cached.
func (c *Client) StatusChecks(prURL string) ([]models.StatusCheck, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if checks, ok := c.checks[prURL]; ok {
		return checks, nil
	}
	var body struct {
		StatusCheckRollup []models.StatusCheck `json:"statusCheckRollup"`
	}
	if err := c.viewPullRequest(prURL, "statusCheckRollup", &body); err != nil {
		return nil, fmt.Errorf("failed to read status checks: %w", err)
	}
	if c.checks == nil {
		c.checks = make(map[string][]models.StatusCheck)
	}
	c.checks[prURL] = body.StatusCheckRollup
	return body.StatusCheckRollup, nil
}

// PullRequestFiles reads one pull request's changed files from gh.
// A successful result is cached on the client, so each pull request is
// fetched at most once per run. Errors are not cached.
func (c *Client) PullRequestFiles(prURL string) ([]models.File, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if files, ok := c.files[prURL]; ok {
		return files, nil
	}
	var body struct {
		Files []models.File `json:"files"`
	}
	if err := c.viewPullRequest(prURL, "files", &body); err != nil {
		return nil, fmt.Errorf("failed to read changed files: %w", err)
	}
	if c.files == nil {
		c.files = make(map[string][]models.File)
	}
	c.files[prURL] = body.Files
	return body.Files, nil
}

// viewPullRequest runs `gh pr view <url> --json <field>` and decodes stdout into v.
func (c *Client) viewPullRequest(prURL, field string, v any) error {
	out, err := c.runGHJSON("pr", "view", prURL, "--json", field)
	if err != nil {
		return err
	}
	return out.decode(v)
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

func parsePullRequestList(out ghJSONOutput) ([]models.PullRequest, error) {
	var prs []models.PullRequest
	if err := out.decode(&prs); err != nil {
		return nil, err
	}
	if !strings.HasPrefix(strings.TrimSpace(out.stdout), "[") {
		return nil, out.parseError(errors.New("stdout is not a JSON array"))
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
// It requests gh-merge's list fields only. Status checks come from StatusChecks.
func (c *Client) GetMergeablePullRequests() ([]models.PullRequest, error) {
	prs, err := c.fetchPullRequests(mergeListFields, true)
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
