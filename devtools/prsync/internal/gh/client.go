package gh

import (
	"context"
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	defaultCallTimeout   = 60 * time.Second
	maxReviewThreadPages = 50
	prListJSONFields     = "number,title,url,baseRefName,headRefName,headRefOid,mergeable,isDraft,reviewDecision,reviewRequests,latestReviews,statusCheckRollup"
	reviewThreadsQuery   = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String){repository(owner:$owner,name:$repo){pullRequest(number:$num){reviewThreads(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{id isResolved comments(last:1){nodes{id author{login} path line url body}}}}}}}`
)

// Client shells out to gh_bin through an injected executor.
type Client struct {
	exec executor.Executor
	bin  string
}

// NewClient returns a gh adapter that invokes bin via exec.
func NewClient(exec executor.Executor, bin string) *Client {
	return &Client{exec: exec, bin: bin}
}

// AuthStatus runs `gh auth status`. A missing binary is an ExecutableNotFoundError.
func (c *Client) AuthStatus(ctx context.Context) error {
	result, err := c.execute(ctx, defaultCallTimeout, "auth", "status")
	if err != nil {
		return err
	}
	if result.ExitCode != 0 {
		return fmt.Errorf("%w: %s", ErrUnauthenticated, strings.TrimSpace(result.Stderr))
	}
	return nil
}

// UserLogin runs `gh api user --jq .login`.
func (c *Client) UserLogin(ctx context.Context) (string, error) {
	result, err := c.requireOK(ctx, defaultCallTimeout, "api", "user", "--jq", ".login")
	if err != nil {
		return "", err
	}
	login := strings.TrimSpace(result.Output)
	if login == "" {
		return "", fmt.Errorf("gh api user: empty login")
	}
	return login, nil
}

// SearchOpenPRRepos returns unique repository.nameWithOwner values.
// capped is true when gh returned exactly 1000 search hits.
func (c *Client) SearchOpenPRRepos(ctx context.Context, author string) (repos []string, capped bool, err error) {
	result, err := c.requireOK(ctx, defaultCallTimeout,
		"search", "prs", "--author", author, "--state", "open", "--limit", "1000", "--json", "repository")
	if err != nil {
		return nil, false, err
	}
	var items []struct {
		Repository struct {
			NameWithOwner string `json:"nameWithOwner"`
		} `json:"repository"`
	}
	if err := json.Unmarshal([]byte(result.Output), &items); err != nil {
		return nil, false, fmt.Errorf("decode search prs: %w", err)
	}
	seen := make(map[string]struct{}, len(items))
	for _, item := range items {
		name := item.Repository.NameWithOwner
		if name == "" {
			continue
		}
		if _, ok := seen[name]; ok {
			continue
		}
		seen[name] = struct{}{}
		repos = append(repos, name)
	}
	return repos, len(items) == 1000, nil
}

// ListOpenPRs runs `gh pr list` for one repo. 404/archived become ErrInaccessible.
func (c *Client) ListOpenPRs(ctx context.Context, repo, author string) ([]PRListItem, error) {
	result, err := c.execute(ctx, defaultCallTimeout,
		"pr", "list", "--repo", repo, "--author", author, "--state", "open",
		"--limit", "1000", "--json", prListJSONFields)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		if isInaccessible(result.Stderr) {
			return nil, fmt.Errorf("%w: %s", ErrInaccessible, strings.TrimSpace(result.Stderr))
		}
		return nil, &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
	}
	var prs []PRListItem
	if err := json.Unmarshal([]byte(result.Output), &prs); err != nil {
		return nil, fmt.Errorf("decode pr list: %w", err)
	}
	if prs == nil {
		prs = []PRListItem{}
	}
	return prs, nil
}

// ReviewThreads paginates GraphQL review threads. Hitting 50 pages is an error.
func (c *Client) ReviewThreads(ctx context.Context, owner, repo string, number int) ([]Thread, error) {
	var threads []Thread
	cursor := ""
	for page := 0; page < maxReviewThreadPages; page++ {
		nodes, next, err := c.reviewThreadsPage(ctx, owner, repo, number, cursor)
		if err != nil {
			return nil, err
		}
		threads = append(threads, nodes...)
		if next == "" {
			if threads == nil {
				threads = []Thread{}
			}
			return threads, nil
		}
		cursor = next
	}
	return nil, fmt.Errorf("review threads page cap exceeded for %s/%s#%d", owner, repo, number)
}

func (c *Client) reviewThreadsPage(ctx context.Context, owner, repo string, number int, cursor string) ([]Thread, string, error) {
	args := []string{
		"api", "graphql",
		"-f", "query=" + reviewThreadsQuery,
		"-F", "owner=" + owner,
		"-F", "repo=" + repo,
		"-F", "num=" + strconv.Itoa(number),
	}
	if cursor != "" {
		args = append(args, "-F", "cursor="+cursor)
	}
	result, err := c.requireOK(ctx, defaultCallTimeout, args...)
	if err != nil {
		return nil, "", err
	}
	var payload graphqlReviewThreads
	if err := json.Unmarshal([]byte(result.Output), &payload); err != nil {
		return nil, "", fmt.Errorf("decode review threads: %w", err)
	}
	if len(payload.Errors) > 0 {
		return nil, "", fmt.Errorf("review threads graphql: %s", payload.Errors[0].Message)
	}
	if payload.Data.Repository.PullRequest == nil {
		return nil, "", fmt.Errorf("review threads: pull request %s/%s#%d not found", owner, repo, number)
	}
	conn := payload.Data.Repository.PullRequest.ReviewThreads
	threads := make([]Thread, 0, len(conn.Nodes))
	for _, node := range conn.Nodes {
		threads = append(threads, node.toThread())
	}
	next := ""
	if conn.PageInfo.HasNextPage {
		next = conn.PageInfo.EndCursor
	}
	return threads, next, nil
}

func (c *Client) requireOK(ctx context.Context, timeout time.Duration, args ...string) (*executor.ExecutionResult, error) {
	result, err := c.execute(ctx, timeout, args...)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
	}
	return result, nil
}

func (c *Client) execute(ctx context.Context, timeout time.Duration, args ...string) (*executor.ExecutionResult, error) {
	result, err := c.exec.Execute(ctx, executor.ToolConfig{
		Command: c.bin,
		Args:    args,
		Timeout: timeout,
	})
	if err != nil {
		return nil, fmt.Errorf("run %s: %w", c.bin, err)
	}
	return result, nil
}

func isInaccessible(stderr string) bool {
	s := strings.ToLower(stderr)
	return strings.Contains(s, "http 404") ||
		strings.Contains(s, "could not resolve to a repository") ||
		strings.Contains(s, "not found") ||
		strings.Contains(s, "archived")
}

type graphqlReviewThreads struct {
	Data struct {
		Repository struct {
			PullRequest *struct {
				ReviewThreads graphqlThreadConn `json:"reviewThreads"`
			} `json:"pullRequest"`
		} `json:"repository"`
	} `json:"data"`
	Errors []struct {
		Message string `json:"message"`
	} `json:"errors"`
}

type graphqlThreadConn struct {
	PageInfo struct {
		HasNextPage bool   `json:"hasNextPage"`
		EndCursor   string `json:"endCursor"`
	} `json:"pageInfo"`
	Nodes []graphqlThread `json:"nodes"`
}

type graphqlThread struct {
	ID         string `json:"id"`
	IsResolved bool   `json:"isResolved"`
	Comments   struct {
		Nodes []graphqlComment `json:"nodes"`
	} `json:"comments"`
}

type graphqlComment struct {
	ID     string `json:"id"`
	Author *struct {
		Login string `json:"login"`
	} `json:"author"`
	Path string `json:"path"`
	Line *int   `json:"line"`
	URL  string `json:"url"`
	Body string `json:"body"`
}

func (n graphqlThread) toThread() Thread {
	comments := make([]ThreadComment, 0, len(n.Comments.Nodes))
	for _, node := range n.Comments.Nodes {
		var author *ThreadAuthor
		if node.Author != nil {
			author = &ThreadAuthor{Login: node.Author.Login}
		}
		comments = append(comments, ThreadComment{
			ID:     node.ID,
			Author: author,
			Path:   node.Path,
			Line:   node.Line,
			URL:    node.URL,
			Body:   node.Body,
		})
	}
	return Thread{ID: n.ID, IsResolved: n.IsResolved, Comments: comments}
}
