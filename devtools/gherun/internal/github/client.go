// Package github provides functionality to interact with GitHub using the gh CLI.
package github

import (
	"context"
	"encoding/json"
	"fmt"
	"net/url"
	"strconv"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"
)

// Issue represents a GitHub issue.
type Issue struct {
	Number int    `json:"number"`
	URL    string `json:"url"`
	Body   string `json:"body"`
}

// Client provides methods to interact with GitHub via gh CLI.
type Client struct {
	executor executor.Executor
}

// NewClient creates a new GitHub client with the shared executor.
// Note: context.Context should be passed to each method call, not stored.
func NewClient(_ context.Context, exec executor.Executor) *Client {
	return &Client{
		executor: exec,
	}
}

// CreateIssue creates a new GitHub issue and returns its details.
func (c *Client) CreateIssue(ctx context.Context, title, body string) (*Issue, error) {
	output, err := executor.Output(ctx, c.executor, "gh", "issue", "create",
		"--title", title,
		"--body", body)
	if err != nil {
		return nil, fmt.Errorf("creating issue: %w", err)
	}

	// gh issue create returns the issue URL
	issueURL := strings.TrimSpace(string(output))

	// Extract issue number from URL
	number, err := extractIssueNumber(issueURL)
	if err != nil {
		return nil, fmt.Errorf("extracting issue number from %s: %w", issueURL, err)
	}

	return &Issue{
		Number: number,
		URL:    issueURL,
		Body:   body,
	}, nil
}

// GetIssue fetches an issue by its number.
func (c *Client) GetIssue(ctx context.Context, number int) (*Issue, error) {
	output, err := executor.Output(ctx, c.executor, "gh", "issue", "view",
		fmt.Sprintf("%d", number),
		"--json", "number,url,body")
	if err != nil {
		return nil, fmt.Errorf("getting issue %d: %w", number, err)
	}

	var issue Issue
	if err := json.Unmarshal(output, &issue); err != nil {
		return nil, fmt.Errorf("parsing issue JSON: %w", err)
	}

	return &issue, nil
}

// extractIssueNumber extracts the issue number from a GitHub issue URL.
// Example: "https://github.com/owner/repo/issues/123" -> 123.
func extractIssueNumber(issueURL string) (int, error) {
	// Parse and validate the URL
	parsed, err := url.Parse(strings.TrimSpace(issueURL))
	if err != nil {
		return 0, fmt.Errorf("invalid URL: %w", err)
	}

	// Validate it's a GitHub URL with issues path
	if parsed.Host != "github.com" {
		return 0, fmt.Errorf("not a GitHub URL: %s", issueURL)
	}

	// URL format: https://github.com/owner/repo/issues/123
	parts := strings.Split(strings.Trim(parsed.Path, "/"), "/")
	// Expected: [owner, repo, "issues", number]
	if len(parts) < 4 || parts[2] != "issues" {
		return 0, fmt.Errorf("invalid GitHub issue URL format: %s", issueURL)
	}

	number, err := strconv.Atoi(parts[3])
	if err != nil {
		return 0, fmt.Errorf("parsing issue number from %q: %w", parts[3], err)
	}

	return number, nil
}
