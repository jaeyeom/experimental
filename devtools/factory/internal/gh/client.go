package gh

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	defaultCallTimeout = 60 * time.Second

	searchQuery = "query($q:String!,$after:String){ search(query:$q,type:ISSUE,first:100,after:$after){" +
		" pageInfo{hasNextPage endCursor}" +
		" nodes{ ... on PullRequest { number title url isDraft merged mergedAt createdAt closedAt headRefName" +
		" repository{nameWithOwner}" +
		" closingIssuesReferences(first:5){ nodes{ number repository{nameWithOwner} } } } } } }"

	openByMeQuery      = "is:pr is:open author:@me archived:false"
	mergedByMeQueryAll = "is:pr is:merged author:@me"
)

// Client runs gh through an injected executor.
type Client struct {
	exec executor.Executor
	bin  string
}

// NewClient returns a client that runs bin through exec.
func NewClient(exec executor.Executor, bin string) *Client {
	return &Client{exec: exec, bin: bin}
}

// OpenByMe returns the authenticated user's open pull requests.
func (c *Client) OpenByMe(ctx context.Context) ([]PR, error) {
	return c.search(ctx, openByMeQuery)
}

// MergedByMe returns the authenticated user's merged pull requests.
func (c *Client) MergedByMe(ctx context.Context) ([]PR, error) {
	return c.search(ctx, mergedByMeQueryAll)
}

// MergedByMeSince returns the authenticated user's pull requests merged on or after t.
// t is sent as an RFC3339 date, YYYY-MM-DD, in t's location.
func (c *Client) MergedByMeSince(ctx context.Context, t time.Time) ([]PR, error) {
	return c.search(ctx, mergedByMeQuery(t))
}

func mergedByMeQuery(t time.Time) string {
	return "is:pr is:merged author:@me merged:>=" + t.Format(time.DateOnly)
}

// ByTitle returns the current user's pull requests, in any state, whose title
// contains id. id is one quoted search term. The caller filters with TitleMatches.
func (c *Client) ByTitle(ctx context.Context, id string) ([]PR, error) {
	if err := validateSearchID(id); err != nil {
		return nil, err
	}
	return c.search(ctx, byTitleQuery(id))
}

// MergedByTitle returns the current user's merged pull requests whose title
// contains id. The search has no merged-after date. id is one quoted term.
func (c *Client) MergedByTitle(ctx context.Context, id string) ([]PR, error) {
	if err := validateSearchID(id); err != nil {
		return nil, err
	}
	return c.search(ctx, mergedByTitleQuery(id))
}

func validateSearchID(id string) error {
	if id == "" {
		return errors.New("gh search: expected a non-empty id, found empty")
	}
	if strings.Contains(id, `"`) || strings.ContainsAny(id, "\r\n") {
		return fmt.Errorf("gh search: expected an id without a quote or a newline, found %q", id)
	}
	return nil
}

func byTitleQuery(id string) string {
	return `is:pr author:@me in:title "` + id + `" archived:false`
}

func mergedByTitleQuery(id string) string {
	return `is:pr is:merged author:@me in:title "` + id + `" archived:false`
}

func (c *Client) search(ctx context.Context, query string) ([]PR, error) {
	all := []PR{}
	seen := map[string]struct{}{}
	after := ""
	for {
		raw, err := c.query(ctx, query, after)
		if err != nil {
			return nil, err
		}
		page, err := decodeSearch(raw)
		if err != nil {
			return nil, err
		}
		all = append(all, page.prs...)
		if page.next == "" {
			return all, nil
		}
		if _, ok := seen[page.next]; ok {
			return nil, fmt.Errorf("gh search: expected a new cursor, found %q again", page.next)
		}
		seen[page.next] = struct{}{}
		after = page.next
	}
}

func (c *Client) query(ctx context.Context, query, after string) ([]byte, error) {
	args := []string{"api", "graphql", "-f", "query=" + searchQuery, "-F", "q=" + query}
	if after != "" {
		args = append(args, "-F", "after="+after)
	}
	result, err := c.execute(ctx, args...)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, ghFailure(result)
	}
	return []byte(result.Output), nil
}

func (c *Client) execute(ctx context.Context, args ...string) (*executor.ExecutionResult, error) {
	result, err := c.exec.Execute(ctx, executor.ToolConfig{
		Command: c.bin,
		Args:    args,
		Timeout: defaultCallTimeout,
	})
	if err != nil {
		return nil, fmt.Errorf("run %s: %w", c.bin, err)
	}
	if result == nil {
		return nil, fmt.Errorf("run %s: expected an execution result, found nil", c.bin)
	}
	return result, nil
}

func ghFailure(result *executor.ExecutionResult) error {
	detail := strings.TrimSpace(result.Stderr)
	if detail == "" {
		detail = strings.TrimSpace(result.Output)
	}
	if detail == "" {
		return fmt.Errorf("gh: expected stderr, found exit %d", result.ExitCode)
	}
	return fmt.Errorf("gh: %s", detail)
}
