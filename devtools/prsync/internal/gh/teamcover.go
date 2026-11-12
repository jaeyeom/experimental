package gh

import (
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/url"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

// prReviewsBatch is how many pull requests one reviews request asks for.
const prReviewsBatch = 50

// pathHistoryQuery asks for the history of every path in one request, one
// alias per path. Each connection stays small: GitHub's secondary limit caps
// GraphQL CPU time per user. Reviews are not nested here; prReviewsQuery
// loads them once per distinct pull request. A truncated connection is
// logged, not paged: history is a ranking signal, not a complete record.
func pathHistoryQuery(paths []string) string {
	var b strings.Builder
	b.WriteString(`query($owner:String!,$name:String!,$expression:String!,$since:GitTimestamp!){repository(owner:$owner,name:$name){object(expression:$expression){... on Commit{`)
	for i, path := range paths {
		fmt.Fprintf(&b, `p%d:history(first:30,path:%s,since:$since){pageInfo{hasNextPage}nodes{author{user{login}}associatedPullRequests(first:2){pageInfo{hasNextPage}nodes{number}}}}`, i, graphQLString(path))
	}
	b.WriteString(`}}}}`)
	return b.String()
}

// prReviewsQuery asks for the reviewers of each pull request, aliased prN.
func prReviewsQuery(numbers []int) string {
	var b strings.Builder
	b.WriteString(`query($owner:String!,$name:String!){repository(owner:$owner,name:$name){`)
	for _, n := range numbers {
		fmt.Fprintf(&b, `pr%d:pullRequest(number:%d){reviews(first:10){pageInfo{hasNextPage}nodes{author{login}}}}`, n, n)
	}
	b.WriteString(`}}`)
	return b.String()
}

// graphQLString quotes s as a GraphQL string literal. JSON string escapes
// are valid GraphQL escapes.
func graphQLString(s string) string {
	quoted, err := json.Marshal(s)
	if err != nil {
		return `""`
	}
	return string(quoted)
}

// ReviewSnapshot loads the pull request author and latestReviews.
// Used only when team_cover is on, so the open-PR list query stays unchanged.
func (c *Client) ReviewSnapshot(ctx context.Context, repo string, number int) (string, []LatestReview, error) {
	raw, err := c.prViewJSON(ctx, repo, number, "author,latestReviews")
	if err != nil {
		return "", nil, err
	}
	var payload struct {
		Author struct {
			Login string `json:"login"`
		} `json:"author"`
		LatestReviews []LatestReview `json:"latestReviews"`
	}
	if err := json.Unmarshal([]byte(raw), &payload); err != nil {
		return "", nil, fmt.Errorf("decode review snapshot: %w", err)
	}
	if payload.LatestReviews == nil {
		payload.LatestReviews = []LatestReview{}
	}
	return payload.Author.Login, payload.LatestReviews, nil
}

// TeamMembers lists GET /orgs/{org}/teams/{slug}/members.
// A missing read:org scope comes back as a non-nil error from gh.
func (c *Client) TeamMembers(ctx context.Context, org, slug string) ([]TeamMember, error) {
	endpoint := fmt.Sprintf("orgs/%s/teams/%s/members", url.PathEscape(org), url.PathEscape(slug))
	result, err := c.call(ctx, "api", "--paginate", endpoint)
	if err != nil {
		return nil, err
	}
	members, err := decodeJSONPages[TeamMember](result.Output)
	if err != nil {
		return nil, fmt.Errorf("decode team members: %w", err)
	}
	return members, nil
}

// ChangedFiles returns paths changed in the pull request, one per line from gh.
func (c *Client) ChangedFiles(ctx context.Context, repo string, number int) ([]string, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return nil, err
	}
	endpoint := fmt.Sprintf("repos/%s/%s/pulls/%d/files", url.PathEscape(owner), url.PathEscape(name), number)
	result, err := c.call(ctx, "api", "--paginate", endpoint, "--jq", ".[].filename")
	if err != nil {
		return nil, err
	}
	return nonEmptyLines(result.Output), nil
}

// codeownersPaths are where GitHub looks for CODEOWNERS, in its order.
var codeownersPaths = []string{".github/CODEOWNERS", "CODEOWNERS", "docs/CODEOWNERS"}

// Codeowners returns the base-branch CODEOWNERS file and the path it was
// found at. GitHub uses the first of .github/CODEOWNERS, CODEOWNERS, and
// docs/CODEOWNERS. first, when it is one of those, is tried before the
// others: the caller passes where the file was last found, so a location
// known to be missing is not requested. An empty body and path mean the
// repository has none. A non-404 failure is returned.
func (c *Client) Codeowners(ctx context.Context, repo, ref, first string) (string, string, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return "", "", err
	}
	paths := codeownersPaths
	if slices.Contains(codeownersPaths, first) {
		paths = append([]string{first}, slices.DeleteFunc(slices.Clone(codeownersPaths), func(p string) bool { return p == first })...)
	}
	for _, path := range paths {
		body, ok, err := c.contents(ctx, owner, name, path, ref)
		if err != nil {
			return "", "", err
		}
		if ok {
			return body, path, nil
		}
	}
	return "", "", nil
}

func (c *Client) contents(ctx context.Context, owner, name, path, ref string) (string, bool, error) {
	endpoint := fmt.Sprintf("repos/%s/%s/contents/%s?ref=%s",
		url.PathEscape(owner), url.PathEscape(name), path, url.QueryEscape(ref))
	result, err := c.call(ctx, "api", endpoint)
	if err != nil {
		if isMissingPath(err) {
			return "", false, nil
		}
		return "", false, err
	}
	body, err := decodeGitHubContent(result.Output)
	if err != nil {
		return "", false, err
	}
	return body, true, nil
}

// BranchHead returns the commit SHA at the tip of branch.
func (c *Client) BranchHead(ctx context.Context, repo, branch string) (string, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return "", err
	}
	segments := strings.Split(branch, "/")
	for i, segment := range segments {
		segments[i] = url.PathEscape(segment)
	}
	endpoint := fmt.Sprintf("repos/%s/%s/git/ref/heads/%s", url.PathEscape(owner), url.PathEscape(name), strings.Join(segments, "/"))
	result, err := c.call(ctx, "api", endpoint, "--jq", ".object.sha")
	if err != nil {
		if IsRateLimited(err) {
			return "", fmt.Errorf("%w: %s", ErrRateLimited, err.Error())
		}
		return "", err
	}
	head := strings.TrimSpace(result.Output)
	if head == "" {
		return "", fmt.Errorf("no head for %s %s", repo, branch)
	}
	return head, nil
}

// PathHistoryResult is what PathHistories fetched.
type PathHistoryResult struct {
	// Histories has an entry for every path except one skipped because
	// it failed on its own, or deferred.
	Histories map[string]PathHistory
	// Batch is the request size to start the next call at: at least 2
	// unless two paths failed together that each worked alone. Zero when
	// no request succeeded.
	Batch int
	// Deferred are the paths left for a later call because the call
	// budget ran out.
	Deferred []string
}

// PathHistories returns, for each path, the authors and reviewers of up to
// 30 commits on it since the cutoff, on ref (a branch or commit SHA).
// Paths go batch to a request. A request that times out or exceeds
// GitHub's resource limits is split in half and each half retried, down
// to single paths; a single path that still fails is skipped with a
// warning. Later requests use the size that worked, and after two
// requests in a row work whole the size doubles, up to maxBatch.
// Reviewers come from the commits' pull requests, each fetched once
// however many paths share it. A truncated connection logs a warning.
// Under a CallBudget, paths whose history or reviewers the budget cannot
// pay for are Deferred; a call is kept for the reviewers before each
// history request.
func (c *Client) PathHistories(ctx context.Context, repo, ref string, paths []string, since time.Time, batch, maxBatch int) (PathHistoryResult, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return PathHistoryResult{}, err
	}
	h := historyFetch{c: c, owner: owner, name: name, ref: ref, since: since, commits: map[string][]historyCommit{}, maxBatch: max(maxBatch, 1)}
	deferred, err := h.fetchAll(ctx, paths, batch)
	if err != nil {
		return PathHistoryResult{}, err
	}
	numbers := pullRequestNumbers(h.commits)
	reviewers, done, err := c.prReviewers(ctx, owner, name, numbers)
	if err != nil && !errors.Is(err, ErrCallCap) {
		return PathHistoryResult{}, err
	}
	out, unreviewed := h.histories(paths, reviewers, numbers[done:])
	return PathHistoryResult{Histories: out, Batch: h.nextBatch(), Deferred: slices.Concat(deferred, unreviewed)}, nil
}

// fetchAll fetches the commits of paths, starting at batch paths to a
// request, and returns the paths the call budget left unfetched.
func (h *historyFetch) fetchAll(ctx context.Context, paths []string, batch int) ([]string, error) {
	budget := CallBudgetFrom(ctx)
	h.size = min(max(batch, 1), h.maxBatch)
	for start := 0; start < len(paths); {
		if left := budget.Left(); left >= 0 && left <= reviewCalls(h.commits) {
			return slices.Clone(paths[start:]), nil
		}
		chunk := paths[start:min(start+h.size, len(paths))]
		start += len(chunk)
		best, err := h.fetch(ctx, chunk)
		if errors.Is(err, ErrCallCap) {
			return append(h.unfetched(chunk), paths[start:]...), nil
		}
		if err != nil {
			return nil, err
		}
		h.adapt(best, len(chunk))
	}
	return nil, nil
}

// adapt sets the next request size from one that asked for asked paths
// and best paths worked in one request: the size that worked after a
// split, or double after two whole successes in a row, up to maxBatch.
func (h *historyFetch) adapt(best, asked int) {
	switch {
	case best == asked:
		h.worked = true
		h.streak++
		if h.streak >= 2 && h.size < h.maxBatch {
			h.size = min(h.size*2, h.maxBatch)
			h.streak = 0
		}
	case best > 0:
		h.worked = true
		h.size = best
		h.streak = 0
	default:
		h.streak = 0
	}
}

// nextBatch is the size the next call starts at, or zero when no request
// worked.
func (h *historyFetch) nextBatch() int {
	if !h.worked {
		return 0
	}
	size := h.size
	if size < 2 && !h.singlesOnly {
		size = 2
	}
	return min(size, h.maxBatch)
}

// histories joins each fetched path's commits with their reviewers. A path
// with a commit in an unreviewed pull request is returned as deferred.
func (h *historyFetch) histories(paths []string, reviewers map[int][]string, unreviewed []int) (map[string]PathHistory, []string) {
	missing := map[int]bool{}
	for _, number := range unreviewed {
		missing[number] = true
	}
	out := make(map[string]PathHistory, len(paths))
	var deferred []string
	for _, path := range paths {
		list, ok := h.commits[path]
		if !ok {
			continue
		}
		if needsAny(list, missing) {
			deferred = append(deferred, path)
			continue
		}
		var history PathHistory
		for _, commit := range list {
			if commit.author != "" {
				history.Authors = append(history.Authors, commit.author)
			}
			for _, number := range commit.prs {
				history.Reviewers = append(history.Reviewers, reviewers[number]...)
			}
		}
		out[path] = history
	}
	return out, deferred
}

// reviewCalls is how many requests the reviewers of commits take.
func reviewCalls(commits map[string][]historyCommit) int {
	n := len(pullRequestNumbers(commits))
	return (n + prReviewsBatch - 1) / prReviewsBatch
}

func needsAny(list []historyCommit, missing map[int]bool) bool {
	if len(missing) == 0 {
		return false
	}
	for _, commit := range list {
		for _, number := range commit.prs {
			if missing[number] {
				return true
			}
		}
	}
	return false
}

// historyFetch collects the commits of each path across history requests.
type historyFetch struct {
	c                *Client
	owner, name, ref string
	since            time.Time
	commits          map[string][]historyCommit
	// size is the next request's path count, at most maxBatch; streak
	// counts whole successes in a row at it.
	size, maxBatch, streak int
	// worked is set once a request succeeds.
	worked bool
	// singlesOnly is set when two paths failed together but each worked
	// alone.
	singlesOnly bool
}

// unfetched is the paths with no commits fetched.
func (h *historyFetch) unfetched(paths []string) []string {
	var out []string
	for _, path := range paths {
		if _, ok := h.commits[path]; !ok {
			out = append(out, path)
		}
	}
	return out
}

// fetch asks for paths in one request and halves it on a splittable
// error. It returns the most paths a successful request held, or zero
// when none succeeded.
func (h *historyFetch) fetch(ctx context.Context, paths []string) (int, error) {
	got, err := h.c.historyBatch(ctx, h.owner, h.name, h.ref, paths, h.since)
	if err == nil {
		for _, path := range paths {
			h.commits[path] = got[path]
		}
		return len(paths), nil
	}
	if ctx.Err() != nil || !isSplittable(err) {
		return 0, err
	}
	stdout, stderr := firstErrorLines(err)
	attrs := []any{"repo", h.owner + "/" + h.name, "ref", h.ref, "stdout", stdout, "stderr", stderr}
	if len(paths) == 1 {
		runlog.FromContext(ctx).Warn(fmt.Sprintf("team-cover: skipping path %s: it %s on its own", paths[0], splitReason(err)), attrs...)
		return 0, nil
	}
	half := (len(paths) + 1) / 2
	parts := fmt.Sprintf("2×%d", half)
	if len(paths)%2 != 0 {
		parts = fmt.Sprintf("%d+%d", half, len(paths)-half)
	}
	runlog.FromContext(ctx).Warn(fmt.Sprintf("team-cover: batch of %d %s; retrying as %s", len(paths), splitReason(err), parts), attrs...)
	first, err := h.fetch(ctx, paths[:half])
	if err != nil {
		return 0, err
	}
	second, err := h.fetch(ctx, paths[half:])
	if err != nil {
		return 0, err
	}
	if len(paths) == 2 && first == 1 && second == 1 {
		h.singlesOnly = true
	}
	return max(first, second), nil
}

var splitHTTPRE = regexp.MustCompile(`\b50[24]\b`)

// isSplittable reports whether a history request failed for its size:
// GitHub's GraphQL timeout, its resource limits, or an HTTP 502 or 504.
func isSplittable(err error) bool {
	text := strings.ToLower(errorText(err))
	return isTimeoutText(text) ||
		strings.Contains(text, "resource_limits_exceeded") ||
		splitHTTPRE.MatchString(text)
}

func isTimeoutText(lower string) bool {
	return strings.Contains(lower, "timeout") || strings.Contains(lower, "timed out")
}

func splitReason(err error) string {
	if isTimeoutText(strings.ToLower(errorText(err))) {
		return "timed out"
	}
	return "failed"
}

// errorText is the error and, for a gh exit, its stdout and stderr.
func errorText(err error) string {
	text := err.Error()
	var proc *ProcError
	if errors.As(err, &proc) {
		text += "\n" + proc.Stdout + "\n" + proc.Stderr
	}
	return text
}

// firstErrorLines is the first non-empty line of a gh exit's stdout and
// stderr. Another error is reported as stderr.
func firstErrorLines(err error) (string, string) {
	var proc *ProcError
	if !errors.As(err, &proc) {
		return "", firstLine(err.Error())
	}
	return firstLine(proc.Stdout), firstLine(proc.Stderr)
}

func firstLine(text string) string {
	if lines := nonEmptyLines(text); len(lines) > 0 {
		return lines[0]
	}
	return ""
}

type historyCommit struct {
	author string
	prs    []int
}

func (c *Client) historyBatch(ctx context.Context, owner, name, ref string, paths []string, since time.Time) (map[string][]historyCommit, error) {
	raw, err := c.graphQL(ctx,
		"-f", "query="+pathHistoryQuery(paths),
		"-F", "owner="+owner,
		"-F", "name="+name,
		"-F", "expression="+ref,
		"-F", "since="+since.UTC().Format(time.RFC3339),
	)
	if err != nil {
		return nil, err
	}
	var payload struct {
		Data struct {
			Repository struct {
				Object map[string]*historyConn `json:"object"`
			} `json:"repository"`
		} `json:"data"`
		Errors []gqlError `json:"errors"`
	}
	if err := json.Unmarshal([]byte(raw), &payload); err != nil {
		return nil, fmt.Errorf("decode path history: %w", err)
	}
	if len(payload.Errors) > 0 {
		first := payload.Errors[0]
		return nil, fmt.Errorf("path history graphql: %s", strings.TrimSpace(first.Type+" "+first.Message))
	}
	out := map[string][]historyCommit{}
	for i, path := range paths {
		conn := payload.Data.Repository.Object[fmt.Sprintf("p%d", i)]
		if conn == nil {
			continue
		}
		var truncated truncation
		truncated.note("history", conn.PageInfo.HasNextPage)
		for _, node := range conn.Nodes {
			commit := historyCommit{}
			if node.Author.User != nil {
				commit.author = node.Author.User.Login
			}
			truncated.note("associatedPullRequests", node.AssociatedPullRequests.PageInfo.HasNextPage)
			for _, pr := range node.AssociatedPullRequests.Nodes {
				commit.prs = append(commit.prs, pr.Number)
			}
			out[path] = append(out[path], commit)
		}
		for _, connName := range truncated {
			warnTruncated(ctx, connName, "repo", owner+"/"+name, "ref", ref, "path", path)
		}
	}
	return out, nil
}

// prReviewers returns the review authors of each pull request, in
// prReviewsBatch-sized requests, and how many of numbers it looked up: on
// an error, those before it.
func (c *Client) prReviewers(ctx context.Context, owner, name string, numbers []int) (map[int][]string, int, error) {
	out := map[int][]string{}
	for start := 0; start < len(numbers); start += prReviewsBatch {
		batch := numbers[start:min(start+prReviewsBatch, len(numbers))]
		raw, err := c.graphQL(ctx,
			"-f", "query="+prReviewsQuery(batch),
			"-F", "owner="+owner,
			"-F", "name="+name,
		)
		if err != nil {
			return out, start, err
		}
		var payload struct {
			Data struct {
				Repository map[string]*prReviewsNode `json:"repository"`
			} `json:"data"`
			Errors []gqlError `json:"errors"`
		}
		if err := json.Unmarshal([]byte(raw), &payload); err != nil {
			return out, start, fmt.Errorf("decode pull request reviews: %w", err)
		}
		if len(payload.Errors) > 0 {
			return out, start, fmt.Errorf("pull request reviews graphql: %s", payload.Errors[0].Message)
		}
		for _, number := range batch {
			node := payload.Data.Repository[fmt.Sprintf("pr%d", number)]
			if node == nil {
				continue
			}
			if node.Reviews.PageInfo.HasNextPage {
				warnTruncated(ctx, "reviews", "repo", owner+"/"+name, "pr", number)
			}
			for _, review := range node.Reviews.Nodes {
				if review.Author != nil && review.Author.Login != "" {
					out[number] = append(out[number], review.Author.Login)
				}
			}
		}
	}
	return out, len(numbers), nil
}

// graphQL runs one gh api graphql request. A rate limit is ErrRateLimited.
func (c *Client) graphQL(ctx context.Context, args ...string) (string, error) {
	result, err := c.requireJSON(ctx, defaultCallTimeout, append([]string{"api", "graphql"}, args...)...)
	if err != nil {
		if IsRateLimited(err) {
			return "", fmt.Errorf("%w: %s", ErrRateLimited, err.Error())
		}
		return "", err
	}
	return result.Output, nil
}

// pullRequestNumbers is every distinct pull request in commits, ascending.
func pullRequestNumbers(commits map[string][]historyCommit) []int {
	seen := map[int]struct{}{}
	var out []int
	for _, list := range commits {
		for _, commit := range list {
			for _, number := range commit.prs {
				if _, ok := seen[number]; ok {
					continue
				}
				seen[number] = struct{}{}
				out = append(out, number)
			}
		}
	}
	slices.Sort(out)
	return out
}

// DirectReviewCount is the search total for user-review-requested:login
// scoped to org. It does not use review-requested:, which also counts team
// requests. A rate limit is ErrRateLimited. Search is GET only, and gh api
// sends -f fields as a POST unless --method GET is given.
func (c *Client) DirectReviewCount(ctx context.Context, org, login string) (int, error) {
	query := fmt.Sprintf("user-review-requested:%s org:%s is:pr is:open", login, org)
	result, err := c.call(ctx, "api", "search/issues", "--method", "GET", "-f", "q="+query, "-f", "per_page=1")
	if err != nil {
		if IsRateLimited(err) {
			return 0, fmt.Errorf("%w: %s", ErrRateLimited, err.Error())
		}
		return 0, err
	}
	var payload struct {
		TotalCount int `json:"total_count"` //nolint:tagliatelle // GitHub API
	}
	if err := json.Unmarshal([]byte(result.Output), &payload); err != nil {
		return 0, fmt.Errorf("decode review count: %w", err)
	}
	return payload.TotalCount, nil
}

// OrgActive reports whether login has an org event at or after since.
// No events, or a 404, is inactive. Other failures are returned.
func (c *Client) OrgActive(ctx context.Context, org, login string, since time.Time) (bool, error) {
	endpoint := fmt.Sprintf("users/%s/events/orgs/%s?per_page=100", url.PathEscape(login), url.PathEscape(org))
	result, err := c.call(ctx, "api", endpoint)
	if err != nil {
		if isMissingPath(err) {
			return false, nil
		}
		if IsRateLimited(err) {
			return false, fmt.Errorf("%w: %s", ErrRateLimited, err.Error())
		}
		return false, err
	}
	var events []struct {
		CreatedAt string `json:"created_at"` //nolint:tagliatelle // GitHub API
	}
	if err := json.Unmarshal([]byte(result.Output), &events); err != nil {
		return false, fmt.Errorf("decode org events: %w", err)
	}
	for _, event := range events {
		at, err := time.Parse(time.RFC3339, event.CreatedAt)
		if err != nil {
			continue
		}
		if !at.Before(since) {
			return true, nil
		}
	}
	return false, nil
}

// call runs gh and returns a non-zero exit as an error, including stderr.
func (c *Client) call(ctx context.Context, args ...string) (*executorResult, error) {
	result, err := c.execute(ctx, defaultCallTimeout, args...)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
	}
	return &executorResult{Output: result.Output}, nil
}

// executorResult is the stdout of a successful gh call.
type executorResult struct {
	Output string
}

func splitOwnerRepo(repo string) (string, string, error) {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") {
		return "", "", fmt.Errorf("invalid repo %q", repo)
	}
	return owner, name, nil
}

func nonEmptyLines(text string) []string {
	var out []string
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		out = append(out, line)
	}
	if out == nil {
		return []string{}
	}
	return out
}

func decodeJSONPages[T any](raw string) ([]T, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return []T{}, nil
	}
	dec := json.NewDecoder(strings.NewReader(raw))
	var out []T
	for {
		var page []T
		if err := dec.Decode(&page); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return nil, fmt.Errorf("decode json page: %w", err)
		}
		out = append(out, page...)
	}
	if out == nil {
		return []T{}, nil
	}
	return out, nil
}

func decodeGitHubContent(raw string) (string, error) {
	var payload struct {
		Encoding string `json:"encoding"`
		Content  string `json:"content"`
	}
	if err := json.Unmarshal([]byte(raw), &payload); err != nil {
		return "", fmt.Errorf("decode contents: %w", err)
	}
	if payload.Encoding != "" && !strings.EqualFold(payload.Encoding, "base64") {
		return "", fmt.Errorf("unsupported content encoding %q", payload.Encoding)
	}
	cleaned := strings.Map(dropBase64Space, payload.Content)
	decoded, err := base64.StdEncoding.DecodeString(cleaned)
	if err != nil {
		return "", fmt.Errorf("decode content: %w", err)
	}
	return string(decoded), nil
}

func dropBase64Space(r rune) rune {
	if r == '\n' || r == '\r' || r == ' ' {
		return -1
	}
	return r
}

func isMissingPath(err error) bool {
	if err == nil {
		return false
	}
	lower := strings.ToLower(err.Error())
	return strings.Contains(lower, "http 404") || strings.Contains(lower, "not found")
}

type historyConn struct {
	PageInfo gqlHasNextPage `json:"pageInfo"`
	Nodes    []struct {
		Author struct {
			User *struct {
				Login string `json:"login"`
			} `json:"user"`
		} `json:"author"`
		AssociatedPullRequests struct {
			PageInfo gqlHasNextPage `json:"pageInfo"`
			Nodes    []struct {
				Number int `json:"number"`
			} `json:"nodes"`
		} `json:"associatedPullRequests"`
	} `json:"nodes"`
}

type prReviewsNode struct {
	Reviews struct {
		PageInfo gqlHasNextPage `json:"pageInfo"`
		Nodes    []struct {
			Author *struct {
				Login string `json:"login"`
			} `json:"author"`
		} `json:"nodes"`
	} `json:"reviews"`
}

type gqlError struct {
	Type    string `json:"type"`
	Message string `json:"message"`
}
