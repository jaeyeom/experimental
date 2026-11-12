package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	executor "github.com/jaeyeom/go-cmdexec"
)

// errQueryResourceLimit is GitHub's per-query complexity error.
// It is not a rate-limit quota error and is not a transient HTTP failure.
var errQueryResourceLimit = errors.New("resource limits for this query exceeded")

// openPRSearchPage is the first search page size. It is half of GitHub's
// maximum of 100 nodes for one search connection, to keep each page's
// GraphQL CPU time down under the secondary limit; the cursor walk loads the
// rest. Raise it only if measured page times stay low. A resource-limit
// error or an HTTP 502/503/504 retries the same cursor with a smaller page,
// and the walk keeps that size for later pages.
const openPRSearchPage = 50

// openPRScalarFields are the pull-request scalars both queries load.
// statusCheckRollup stays off the search: one page that includes it exceeds
// GitHub's resource limit once many open PRs carry a large rollup.
// updatedAt with headRefOid tells a later round that a pull request is unchanged.
const openPRScalarFields = "number title url state baseRefName headRefName headRefOid updatedAt mergeable mergeStateStatus isDraft reviewDecision"

// reviewerSelection is the requestedReviewer fragment for the open-PR search
// and the single-PR view. User and Bot select login. Team selects name and
// slug; Team has no login field. reviewRequests(first:20) covers real pull
// requests at a fraction of the cost of the connection maximum; a truncated
// list logs a warning. The fragment does not select organization, which is
// what pushes the search over the limit.
const reviewerSelection = `reviewRequests(first:20){pageInfo{hasNextPage}nodes{requestedReviewer{__typename ... on User{login} ... on Bot{login} ... on Team{name slug}}}}`

const openPRSearchQuery = `query($q:String!,$first:Int!,$after:String){search(query:$q,type:ISSUE,first:$first,after:$after){issueCount pageInfo{hasNextPage endCursor} nodes{... on PullRequest{` +
	openPRScalarFields + ` ` + reviewerSelection + `}}}}`

const openPRViewQuery = `query($owner:String!,$repo:String!,$num:Int!){repository(owner:$owner,name:$repo){pullRequest(number:$num){` +
	openPRScalarFields + ` ` + reviewerSelection +
	` latestReviews(first:20){pageInfo{hasNextPage}nodes{author{login} state submittedAt}} statusCheckRollup{contexts(first:100){nodes{... on CheckRun{name status conclusion} ... on StatusContext{context state}}}}}}}`

func openPRSearchArgs(repo, author string, page int, after string) []string {
	args := []string{
		"api", "graphql",
		"-f", "query=" + openPRSearchQuery,
		"-F", "q=" + openPRSearchText(repo, author),
		"-F", "first=" + strconv.Itoa(page),
	}
	if after != "" {
		args = append(args, "-F", "after="+after)
	}
	return args
}

func openPRSearchText(repo, author string) string {
	return "repo:" + repo + " author:" + author + " is:pr is:open"
}

func openPRViewArgs(repo string, number int) ([]string, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return nil, err
	}
	return []string{
		"api", "graphql",
		"-f", "query=" + openPRViewQuery,
		"-F", "owner=" + owner,
		"-F", "repo=" + name,
		"-F", "num=" + strconv.Itoa(number),
	}, nil
}

func smallerOpenPRPage(page int) int {
	next := page / 2
	if next < 1 {
		return 1
	}
	return next
}

// currentOpenPRPage is the search page for the next list call. A page that
// was halved stays halved for the rest of the client's run.
func (c *Client) currentOpenPRPage() int {
	if page := int(c.openPRPage.Load()); page > 0 {
		return page
	}
	return openPRSearchPage
}

// halveOpenPRPage halves the search page, logs why, and returns the new page.
func (c *Client) halveOpenPRPage(ctx context.Context, reason string) int {
	page := c.currentOpenPRPage()
	next := smallerOpenPRPage(page)
	c.openPRPage.Store(int32(next)) //nolint:gosec // next is at most openPRSearchPage
	runlog.FromContext(ctx).Info("open pr search page halved",
		"failed_page", page,
		"next_page", next,
		"reason", reason,
	)
	return next
}

// listOpenPRsShrinking walks every search page with the cursor. Pull requests
// are deduplicated by number in case results shift during the walk. A walk
// that ends with fewer pull requests than issueCount logs a warning.
func (c *Client) listOpenPRsShrinking(ctx context.Context, repo, author string) ([]PRListItem, error) {
	items := []PRListItem{}
	seen := map[int]bool{}
	after := ""
	for {
		page, err := c.listOpenPRPageShrinking(ctx, repo, author, after)
		if err != nil {
			return nil, err
		}
		for _, item := range page.items {
			warnTruncatedPR(ctx, repo, item)
			if !seen[item.Number] {
				seen[item.Number] = true
				items = append(items, item)
			}
		}
		if !page.hasNextPage {
			if len(items) < page.issueCount {
				runlog.FromContext(ctx).Warn("open pr search incomplete",
					"repo", repo,
					"got", len(items),
					"issue_count", page.issueCount,
				)
			}
			return items, nil
		}
		if page.endCursor == "" || page.endCursor == after {
			return nil, fmt.Errorf("open pr search %s: next page has no new cursor", repo)
		}
		after = page.endCursor
	}
}

// listOpenPRPageShrinking loads the page after the cursor. A resource-limit
// error retries the same cursor with a halved page.
func (c *Client) listOpenPRPageShrinking(ctx context.Context, repo, author, after string) (openPRSearchResult, error) {
	for {
		page, err := c.listOpenPRSearch(ctx, repo, author, after)
		if err == nil || !isQueryResourceLimit(err) || c.currentOpenPRPage() <= 1 {
			return page, err
		}
		c.halveOpenPRPage(ctx, "resource limit")
	}
}

// listOpenPRSearch runs the search at the current page size. An HTTP 502,
// 503, or 504 halves the page before the next retry on the same cursor: a
// large search often times out on GitHub's side where a smaller page succeeds.
func (c *Client) listOpenPRSearch(ctx context.Context, repo, author, after string) (openPRSearchResult, error) {
	policy := ghRetryPolicy{
		httpAttempts: ghListHTTPRetryAttempts,
		httpBackoffs: ghListHTTPRetryBackoffs,
		nextHTTPArgs: func(ctx context.Context, failErr error) []string {
			reason := transientHTTPRE.FindString(strings.ToLower(failErr.Error()))
			return openPRSearchArgs(repo, author, c.halveOpenPRPage(ctx, reason), after)
		},
	}
	result, err := c.executeRetry(ctx, defaultCallTimeout, validJSONOutput, policy, openPRSearchArgs(repo, author, c.currentOpenPRPage(), after)...)
	if err != nil {
		return openPRSearchResult{}, err
	}
	if result.ExitCode != 0 {
		return openPRSearchResult{}, graphqlExecError(result)
	}
	return decodeOpenPRSearch(result.Output)
}

func (c *Client) viewOpenPR(ctx context.Context, repo string, number int) (PRListItem, error) {
	args, err := openPRViewArgs(repo, number)
	if err != nil {
		return PRListItem{}, err
	}
	result, err := c.executeChecked(ctx, defaultCallTimeout, validJSONOutput, args...)
	if err != nil {
		return PRListItem{}, err
	}
	if result.ExitCode != 0 {
		return PRListItem{}, graphqlExecError(result)
	}
	item, err := decodeOpenPRView(result.Output)
	if err != nil {
		return PRListItem{}, err
	}
	warnTruncatedPR(ctx, repo, item)
	return item, nil
}

func isQueryResourceLimit(err error) bool {
	if err == nil {
		return false
	}
	if errors.Is(err, errQueryResourceLimit) {
		return true
	}
	return strings.Contains(strings.ToLower(err.Error()), errQueryResourceLimit.Error())
}

func graphqlExecError(result *executor.ExecutionResult) error {
	text := result.Stderr + "\n" + result.Output
	if err := classifyGraphQLMessage(text); err != nil {
		return err
	}
	return &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
}

func classifyGraphQLMessage(message string) error {
	message = strings.TrimSpace(message)
	switch {
	case isPRNotFound(message):
		return fmt.Errorf("%w: %s", ErrNotFound, message)
	case isInaccessible(message):
		return fmt.Errorf("%w: %s", ErrInaccessible, message)
	case isQueryResourceLimitText(message):
		return fmt.Errorf("%w: %s", errQueryResourceLimit, message)
	default:
		return nil
	}
}

func isQueryResourceLimitText(text string) bool {
	return strings.Contains(strings.ToLower(text), errQueryResourceLimit.Error())
}

func graphqlErrorsError(prefix string, messages []string) error {
	if len(messages) == 0 {
		return nil
	}
	parts := make([]string, 0, len(messages))
	for _, msg := range messages {
		if msg != "" {
			parts = append(parts, msg)
		}
	}
	message := strings.Join(parts, "; ")
	if err := classifyGraphQLMessage(message); err != nil {
		return err
	}
	if message == "" {
		message = "graphql error"
	}
	return fmt.Errorf("%s: %s", prefix, message)
}

func decodeGraphQLData(raw, prefix string) (json.RawMessage, error) {
	data, messages, err := ghcli.ParseEnvelope(raw)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", prefix, err)
	}
	if err := graphqlErrorsError(prefix, messages); err != nil {
		return nil, err
	}
	if len(data) == 0 {
		return nil, fmt.Errorf("%s: missing data", prefix)
	}
	return data, nil
}

type gqlPR struct {
	Number            int                  `json:"number"`
	Title             string               `json:"title"`
	URL               string               `json:"url"`
	State             string               `json:"state"`
	BaseRefName       string               `json:"baseRefName"`
	HeadRefName       string               `json:"headRefName"`
	HeadRefOid        string               `json:"headRefOid"`
	UpdatedAt         nullableString       `json:"updatedAt"`
	Mergeable         string               `json:"mergeable"`
	MergeStateStatus  string               `json:"mergeStateStatus"`
	IsDraft           bool                 `json:"isDraft"`
	ReviewDecision    nullableString       `json:"reviewDecision"`
	ReviewRequests    gqlReviewRequests    `json:"reviewRequests"`
	LatestReviews     *gqlLatestReviews    `json:"latestReviews"`
	StatusCheckRollup gqlStatusCheckRollup `json:"statusCheckRollup"`
}

// gqlStatusCheckRollup is statusCheckRollup.contexts.nodes.
// CheckRun and StatusContext belong to that union.
type gqlStatusCheckRollup struct {
	Contexts struct {
		Nodes []gqlStatusCheck `json:"nodes"`
	} `json:"contexts"`
}

// gqlHasNextPage is pageInfo{hasNextPage} on a connection loaded with a
// small page, so a truncated list is logged rather than silent.
type gqlHasNextPage struct {
	HasNextPage bool `json:"hasNextPage"`
}

type gqlReviewRequests struct {
	PageInfo gqlHasNextPage `json:"pageInfo"`
	Nodes    []struct {
		RequestedReviewer *gqlRequestedReviewer `json:"requestedReviewer"`
	} `json:"nodes"`
}

type gqlRequestedReviewer struct {
	Type  string `json:"__typename"` //nolint:tagliatelle // GraphQL union discriminator
	Login string `json:"login"`
	Name  string `json:"name"`
	Slug  string `json:"slug"`
}

type gqlLatestReviews struct {
	PageInfo gqlHasNextPage    `json:"pageInfo"`
	Nodes    []gqlLatestReview `json:"nodes"`
}

type gqlLatestReview struct {
	Author *struct {
		Login string `json:"login"`
	} `json:"author"`
	State       string         `json:"state"`
	SubmittedAt nullableString `json:"submittedAt"`
}

type gqlStatusCheck struct {
	Name       string         `json:"name"`
	Status     string         `json:"status"`
	Conclusion nullableString `json:"conclusion"`
	Context    string         `json:"context"`
	State      string         `json:"state"`
}

type nullableString string

func (s *nullableString) UnmarshalJSON(data []byte) error {
	if string(data) == "null" {
		*s = ""
		return nil
	}
	var value string
	if err := json.Unmarshal(data, &value); err != nil {
		return fmt.Errorf("decode string: %w", err)
	}
	*s = nullableString(value)
	return nil
}

func (p *gqlPR) toPRListItem() PRListItem {
	item := PRListItem{
		Number:            p.Number,
		Title:             p.Title,
		URL:               p.URL,
		State:             NormalizePRState(p.State),
		BaseRefName:       p.BaseRefName,
		HeadRefName:       p.HeadRefName,
		HeadRefOid:        p.HeadRefOid,
		UpdatedAt:         string(p.UpdatedAt),
		Mergeable:         p.Mergeable,
		MergeStateStatus:  p.MergeStateStatus,
		IsDraft:           p.IsDraft,
		ReviewDecision:    string(p.ReviewDecision),
		ReviewRequests:    reviewReqsFrom(p.ReviewRequests),
		StatusCheckRollup: statusChecksFrom(p.StatusCheckRollup.Contexts.Nodes),
	}
	var truncated truncation
	truncated.note("reviewRequests", p.ReviewRequests.PageInfo.HasNextPage)
	if p.LatestReviews != nil {
		item.LatestReviews = latestReviewsFrom(p.LatestReviews.Nodes)
		truncated.note("latestReviews", p.LatestReviews.PageInfo.HasNextPage)
	}
	item.Truncated = truncated
	return item
}

// truncation lists connection names that had a next page, once each.
type truncation []string

func (t *truncation) note(connection string, hasNextPage bool) {
	if hasNextPage && !slices.Contains(*t, connection) {
		*t = append(*t, connection)
	}
}

// warnTruncated logs a connection cut short by its page size.
func warnTruncated(ctx context.Context, connection string, attrs ...any) {
	runlog.FromContext(ctx).Warn("github connection truncated",
		append([]any{"connection", connection}, attrs...)...)
}

func warnTruncatedPR(ctx context.Context, repo string, item PRListItem) {
	for _, conn := range item.Truncated {
		warnTruncated(ctx, conn, "repo", repo, "pr", item.Number)
	}
}

func reviewReqsFrom(conn gqlReviewRequests) []ReviewReq {
	if conn.Nodes == nil {
		return nil
	}
	out := make([]ReviewReq, 0, len(conn.Nodes))
	for _, node := range conn.Nodes {
		reviewer := node.RequestedReviewer
		if reviewer == nil {
			continue
		}
		out = append(out, ReviewReq{
			Type:  reviewer.Type,
			Login: reviewer.Login,
			Name:  reviewer.Name,
			Slug:  reviewer.Slug,
		})
	}
	return out
}

func latestReviewsFrom(nodes []gqlLatestReview) []LatestReview {
	out := make([]LatestReview, len(nodes))
	for i, node := range nodes {
		login := ""
		if node.Author != nil {
			login = node.Author.Login
		}
		out[i] = LatestReview{
			Author:      ReviewAuthor{Login: login},
			State:       node.State,
			SubmittedAt: string(node.SubmittedAt),
		}
	}
	return out
}

func statusChecksFrom(checks []gqlStatusCheck) []StatusCheck {
	if checks == nil {
		return nil
	}
	out := make([]StatusCheck, len(checks))
	for i, check := range checks {
		out[i] = StatusCheck{
			Name:       check.Name,
			Status:     check.Status,
			Conclusion: string(check.Conclusion),
			Context:    check.Context,
			State:      check.State,
		}
	}
	return out
}

// openPRSearchResult is one page of the open-PR search.
type openPRSearchResult struct {
	items       []PRListItem
	issueCount  int
	hasNextPage bool
	endCursor   string
}

func decodeOpenPRSearch(raw string) (openPRSearchResult, error) {
	const prefix = "decode pr list"
	data, err := decodeGraphQLData(raw, prefix)
	if err != nil {
		return openPRSearchResult{}, err
	}
	var payload struct {
		Search struct {
			IssueCount int `json:"issueCount"`
			PageInfo   struct {
				HasNextPage bool           `json:"hasNextPage"`
				EndCursor   nullableString `json:"endCursor"`
			} `json:"pageInfo"`
			Nodes []*gqlPR `json:"nodes"`
		} `json:"search"`
	}
	if err := json.Unmarshal(data, &payload); err != nil {
		return openPRSearchResult{}, fmt.Errorf("%s: %w", prefix, err)
	}
	items := make([]PRListItem, 0, len(payload.Search.Nodes))
	for _, node := range payload.Search.Nodes {
		if node == nil {
			continue
		}
		items = append(items, node.toPRListItem())
	}
	return openPRSearchResult{
		items:       items,
		issueCount:  payload.Search.IssueCount,
		hasNextPage: payload.Search.PageInfo.HasNextPage,
		endCursor:   string(payload.Search.PageInfo.EndCursor),
	}, nil
}

type gqlViewData struct {
	Repository *struct {
		PullRequest *gqlPR `json:"pullRequest"`
	} `json:"repository"`
}

func decodeOpenPRView(raw string) (PRListItem, error) {
	const prefix = "decode pr view"
	data, err := decodeGraphQLData(raw, prefix)
	if err != nil {
		return PRListItem{}, err
	}
	var payload gqlViewData
	if err := json.Unmarshal(data, &payload); err != nil {
		return PRListItem{}, fmt.Errorf("%s: %w", prefix, err)
	}
	if payload.Repository == nil {
		return PRListItem{}, fmt.Errorf("%w: repository not found", ErrInaccessible)
	}
	if payload.Repository.PullRequest == nil {
		return PRListItem{}, fmt.Errorf("%w: pull request not found", ErrNotFound)
	}
	return payload.Repository.PullRequest.toPRListItem(), nil
}
