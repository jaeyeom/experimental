package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	defaultCallTimeout     = 60 * time.Second
	maxReviewThreadPages   = 50
	rollupFetchConcurrency = 4
	// prListJSONFields omits statusCheckRollup and latestReviews. One search
	// page that includes the rollup exceeds GitHub's GraphQL resource limit
	// once many open PRs each carry a large check rollup, and the gateway
	// answers 502 or 504. state is a scalar on each pull request already
	// loaded, so it adds no node and no second request.
	prListJSONFields      = "number,title,url,state,baseRefName,headRefName,headRefOid,mergeable,mergeStateStatus,isDraft,reviewDecision,reviewRequests"
	prViewJSONFields      = prListJSONFields + ",latestReviews,statusCheckRollup"
	statusCheckJSONFields = "statusCheckRollup"
	prSearchJSONFields    = "number,title,url,state,isDraft,closedAt,repository"
	prStateJSONFields     = "number,title,url,state,isDraft,closedAt"
	reviewThreadsQuery    = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String){repository(owner:$owner,name:$repo){pullRequest(number:$num){reviewThreads(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{id isResolved comments(last:1){nodes{id author{login} path line url body}}}}}}}`
)

// Client shells out to gh_bin through an injected executor.
type Client struct {
	exec   executor.Executor
	bin    string
	errOut io.Writer
	sleep  func(ctx context.Context, d time.Duration) error
	now    func() time.Time
}

// NewClient returns a gh adapter that invokes bin via exec.
func NewClient(exec executor.Executor, bin string) *Client {
	return &Client{
		exec:   exec,
		bin:    bin,
		errOut: os.Stderr,
		sleep:  sleepWithContext,
		now:    time.Now,
	}
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
	result, err := c.requireText(ctx, defaultCallTimeout, "api", "user", "--jq", ".login")
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
	result, err := c.requireJSON(ctx, defaultCallTimeout,
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

// SearchAuthoredPRs runs `gh search prs <query>` for the author across all
// states (open, merged, closed). It is the reverse of ListOpenPRs: given a
// ticket, find every PR whose text matches, so a tab can be correlated back to
// a merged or abandoned PR.
//
// Query tokens are passed as separate argv entries. gh phrase-quotes a single
// argv that contains spaces, so `(A OR B)` as one argument becomes
// q="(A OR B)" and matches nothing; `A OR B` as three arguments becomes
// q=( A OR B ).
func (c *Client) SearchAuthoredPRs(ctx context.Context, author, query string) ([]PRSearchItem, error) {
	args := append([]string{"search", "prs"}, strings.Fields(query)...)
	args = append(args, "--author", author, "--limit", "100", "--json", prSearchJSONFields)
	result, err := c.requireJSON(ctx, defaultCallTimeout, args...)
	if err != nil {
		return nil, err
	}
	var items []PRSearchItem
	if err := json.Unmarshal([]byte(result.Output), &items); err != nil {
		return nil, fmt.Errorf("decode search prs: %w", err)
	}
	if items == nil {
		items = []PRSearchItem{}
	}
	return items, nil
}

// ListOpenPRs runs `gh pr list` for one repo. 404/archived become ErrInaccessible.
func (c *Client) ListOpenPRs(ctx context.Context, repo, author string) ([]PRListItem, error) {
	result, err := c.executeChecked(ctx, defaultCallTimeout, validJSONOutput,
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
	for i := range prs {
		prs[i].State = NormalizePRState(prs[i].State)
	}
	if len(prs) == 0 {
		return prs, nil
	}
	if err := c.fillStatusChecks(ctx, repo, prs); err != nil {
		return nil, err
	}
	return prs, nil
}

// fillStatusChecks loads each pull request's rollup with its own view.
// A per-PR failure sets StatusCheckError and leaves the rollup nil.
// A primary rate limit or a canceled context fails the whole list.
func (c *Client) fillStatusChecks(ctx context.Context, repo string, items []PRListItem) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	var (
		wg      sync.WaitGroup
		fatalMu sync.Mutex
		fatal   error
		sem     = make(chan struct{}, rollupFetchConcurrency)
	)
	for i := range items {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			select {
			case sem <- struct{}{}:
			case <-ctx.Done():
				return
			}
			defer func() { <-sem }()
			if ctx.Err() != nil {
				return
			}
			checks, err := c.statusChecks(ctx, repo, items[i].Number)
			if err == nil {
				items[i].StatusCheckRollup = checks
				return
			}
			if rollupErrFatal(err) {
				fatalMu.Lock()
				if fatal == nil {
					fatal = err
					cancel()
				}
				fatalMu.Unlock()
				return
			}
			items[i].StatusCheckRollup = nil
			items[i].StatusCheckError = err.Error()
		}(i)
	}
	wg.Wait()
	if fatal != nil {
		return fatal
	}
	if err := ctx.Err(); err != nil {
		return fmt.Errorf("fill status checks: %w", err)
	}
	return nil
}

func rollupErrFatal(err error) bool {
	if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
		return true
	}
	lower := strings.ToLower(err.Error())
	return strings.Contains(lower, "rate limit") && !strings.Contains(lower, "secondary")
}

func (c *Client) statusChecks(ctx context.Context, repo string, number int) ([]StatusCheck, error) {
	raw, err := c.prViewJSON(ctx, repo, number, statusCheckJSONFields)
	if err != nil {
		return nil, err
	}
	var payload struct {
		StatusCheckRollup []StatusCheck `json:"statusCheckRollup"`
	}
	if err := json.Unmarshal([]byte(raw), &payload); err != nil {
		return nil, fmt.Errorf("decode status checks: %w", err)
	}
	return payload.StatusCheckRollup, nil
}

// ViewPR runs `gh pr view <number> --repo <repo> --json` for one pull request,
// including latestReviews and statusCheckRollup. A missing pull request is
// ErrNotFound. A missing or archived repository is ErrInaccessible.
func (c *Client) ViewPR(ctx context.Context, repo string, number int) (PRListItem, error) {
	raw, err := c.prViewJSON(ctx, repo, number, prViewJSONFields)
	if err != nil {
		return PRListItem{}, err
	}
	var item PRListItem
	if err := json.Unmarshal([]byte(raw), &item); err != nil {
		return PRListItem{}, fmt.Errorf("decode pr view: %w", err)
	}
	item.State = NormalizePRState(item.State)
	return item, nil
}

// NormalizePRState maps GitHub's OPEN, CLOSED, and MERGED values to
// open, closed, or merged. Any other value, including empty, is empty.
func NormalizePRState(state string) string {
	switch strings.ToLower(strings.TrimSpace(state)) {
	case "open", "closed", "merged":
		return strings.ToLower(strings.TrimSpace(state))
	default:
		return ""
	}
}

// ViewPRState runs `gh pr view` for one pull request in any state and by any
// author. State is normalized to "open", "merged", or "closed". Repository is
// the repo argument when the payload omits it. A missing pull request is
// ErrNotFound. A missing or archived repository is ErrInaccessible.
func (c *Client) ViewPRState(ctx context.Context, repo string, number int) (PRSearchItem, error) {
	raw, err := c.prViewJSON(ctx, repo, number, prStateJSONFields)
	if err != nil {
		return PRSearchItem{}, err
	}
	var item PRSearchItem
	if err := json.Unmarshal([]byte(raw), &item); err != nil {
		return PRSearchItem{}, fmt.Errorf("decode pr view: %w", err)
	}
	item.State = strings.ToLower(strings.TrimSpace(item.State))
	if item.Repository.NameWithOwner == "" {
		item.Repository.NameWithOwner = repo
	}
	if item.Number == 0 {
		item.Number = number
	}
	return item, nil
}

func (c *Client) prViewJSON(ctx context.Context, repo string, number int, fields string) (string, error) {
	result, err := c.executeChecked(ctx, defaultCallTimeout, validJSONOutput,
		"pr", "view", strconv.Itoa(number), "--repo", repo, "--json", fields)
	if err != nil {
		return "", err
	}
	if result.ExitCode != 0 {
		stderr := strings.TrimSpace(result.Stderr)
		if isPRNotFound(result.Stderr) {
			return "", fmt.Errorf("%w: %s", ErrNotFound, stderr)
		}
		if isInaccessible(result.Stderr) {
			return "", fmt.Errorf("%w: %s", ErrInaccessible, stderr)
		}
		return "", &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
	}
	return result.Output, nil
}

func isPRNotFound(stderr string) bool {
	s := strings.ToLower(stderr)
	if strings.Contains(s, "could not resolve to a pullrequest") {
		return true
	}
	if strings.Contains(s, "no pull requests found") {
		return true
	}
	return strings.Contains(s, "pull request") && strings.Contains(s, "not found")
}

// CommentPR runs `gh pr comment <number> --repo <repo> --body <body>`.
func (c *Client) CommentPR(ctx context.Context, repo string, number int, body string) error {
	_, err := c.requireOK(ctx, defaultCallTimeout,
		"pr", "comment", strconv.Itoa(number), "--repo", repo, "--body", body)
	return err
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
	result, err := c.requireJSON(ctx, defaultCallTimeout, args...)
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
	return finishOK(c.execute(ctx, timeout, args...))
}

func (c *Client) requireJSON(ctx context.Context, timeout time.Duration, args ...string) (*executor.ExecutionResult, error) {
	return finishOK(c.executeChecked(ctx, timeout, validJSONOutput, args...))
}

func (c *Client) requireText(ctx context.Context, timeout time.Duration, args ...string) (*executor.ExecutionResult, error) {
	return finishOK(c.executeChecked(ctx, timeout, nonEmptyOutput, args...))
}

func finishOK(result *executor.ExecutionResult, err error) (*executor.ExecutionResult, error) {
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
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
