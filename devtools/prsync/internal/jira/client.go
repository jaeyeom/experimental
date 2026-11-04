// Package jira reads Jira issues by shelling out to the jira CLI.
package jira

import (
	"context"
	"fmt"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const viewTimeout = 60 * time.Second

// Client shells out to jira_bin through an injected executor.
type Client struct {
	exec executor.Executor
	bin  string
}

// NewClient returns a jira adapter that invokes bin via exec.
func NewClient(exec executor.Executor, bin string) *Client {
	return &Client{exec: exec, bin: bin}
}

// ViewIssue runs `jira issue view KEY --raw` and decodes the issue.
func (c *Client) ViewIssue(ctx context.Context, key string) (Issue, error) {
	result, err := c.exec.Execute(ctx, executor.ToolConfig{
		Command: c.bin,
		Args:    []string{"issue", "view", key, "--raw"},
		Timeout: viewTimeout,
	})
	if err != nil {
		return Issue{}, fmt.Errorf("jira issue view %s: %w", key, err)
	}
	if result.ExitCode != 0 {
		msg := strings.TrimSpace(result.Stderr)
		if msg == "" {
			msg = strings.TrimSpace(result.Output)
		}
		if msg == "" {
			msg = fmt.Sprintf("exit %d", result.ExitCode)
		}
		return Issue{}, fmt.Errorf("jira issue view %s: %s", key, msg)
	}
	issue, err := ParseIssue(result.Output)
	if err != nil {
		return Issue{}, fmt.Errorf("jira issue view %s: %w", key, err)
	}
	if issue.Key == "" {
		issue.Key = key
	}
	return issue, nil
}
