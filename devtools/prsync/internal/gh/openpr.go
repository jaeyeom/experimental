package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	executor "github.com/jaeyeom/go-cmdexec"
)

// errQueryResourceLimit is GitHub's per-query complexity error.
// It is not a rate-limit quota error and is not a transient HTTP failure.
var errQueryResourceLimit = errors.New("resource limits for this query exceeded")

// openPRSearchPage is the first search page. It is below the old --limit 1000
// cap and at GitHub's maximum of 100 nodes for one search connection.
// A resource-limit error retries with a smaller page. Later pages are not walked.
const openPRSearchPage = 100

// openPRScalarFields are the pull-request scalars both queries load.
// statusCheckRollup stays off the search: one page that includes it exceeds
// GitHub's resource limit once many open PRs carry a large rollup.
const openPRScalarFields = "number title url state baseRefName headRefName headRefOid mergeable mergeStateStatus isDraft reviewDecision"

// reviewerSelection is the requestedReviewer fragment for the open-PR search
// and the single-PR view. User and Bot select login. Team selects name and
// slug; Team has no login field. reviewRequests(first:100) is the connection
// maximum, same as GitHub CLI, not a smaller complexity cap. The fragment does
// not select organization, which is what pushes the search over the limit.
const reviewerSelection = `reviewRequests(first:100){nodes{requestedReviewer{__typename ... on User{login} ... on Bot{login} ... on Team{name slug}}}}`

const openPRSearchQuery = `query($q:String!,$first:Int!){search(query:$q,type:ISSUE,first:$first){nodes{... on PullRequest{` +
	openPRScalarFields + ` ` + reviewerSelection + `}}}}`

const openPRViewQuery = `query($owner:String!,$repo:String!,$num:Int!){repository(owner:$owner,name:$repo){pullRequest(number:$num){` +
	openPRScalarFields + ` ` + reviewerSelection +
	` latestReviews(first:100){nodes{author{login} state submittedAt}} statusCheckRollup{... on CheckRun{name status conclusion} ... on StatusContext{context state}}}}}`

func openPRSearchArgs(repo, author string, page int) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + openPRSearchQuery,
		"-F", "q=" + openPRSearchText(repo, author),
		"-F", "first=" + strconv.Itoa(page),
	}
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

func (c *Client) listOpenPRsShrinking(ctx context.Context, repo, author string) ([]PRListItem, error) {
	page := openPRSearchPage
	for {
		items, err := c.listOpenPRSearch(ctx, repo, author, page)
		if err == nil || !isQueryResourceLimit(err) || page <= 1 {
			return items, err
		}
		next := smallerOpenPRPage(page)
		runlog.FromContext(ctx).Info("open pr search page halved",
			"failed_page", page,
			"next_page", next,
		)
		page = next
	}
}

func (c *Client) listOpenPRSearch(ctx context.Context, repo, author string, page int) ([]PRListItem, error) {
	policy := ghRetryPolicy{httpAttempts: ghListHTTPRetryAttempts, httpBackoffs: ghListHTTPRetryBackoffs}
	result, err := c.executeRetry(ctx, defaultCallTimeout, validJSONOutput, policy, openPRSearchArgs(repo, author, page)...)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, graphqlExecError(result)
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
	return decodeOpenPRView(result.Output)
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
	Number            int               `json:"number"`
	Title             string            `json:"title"`
	URL               string            `json:"url"`
	State             string            `json:"state"`
	BaseRefName       string            `json:"baseRefName"`
	HeadRefName       string            `json:"headRefName"`
	HeadRefOid        string            `json:"headRefOid"`
	Mergeable         string            `json:"mergeable"`
	MergeStateStatus  string            `json:"mergeStateStatus"`
	IsDraft           bool              `json:"isDraft"`
	ReviewDecision    nullableString    `json:"reviewDecision"`
	ReviewRequests    gqlReviewRequests `json:"reviewRequests"`
	LatestReviews     *gqlLatestReviews `json:"latestReviews"`
	StatusCheckRollup []gqlStatusCheck  `json:"statusCheckRollup"`
}

type gqlReviewRequests struct {
	Nodes []struct {
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
	Nodes []gqlLatestReview `json:"nodes"`
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
		Mergeable:         p.Mergeable,
		MergeStateStatus:  p.MergeStateStatus,
		IsDraft:           p.IsDraft,
		ReviewDecision:    string(p.ReviewDecision),
		ReviewRequests:    reviewReqsFrom(p.ReviewRequests),
		StatusCheckRollup: statusChecksFrom(p.StatusCheckRollup),
	}
	if p.LatestReviews != nil {
		item.LatestReviews = latestReviewsFrom(p.LatestReviews.Nodes)
	}
	return item
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

func decodeOpenPRSearch(raw string) ([]PRListItem, error) {
	const prefix = "decode pr list"
	data, err := decodeGraphQLData(raw, prefix)
	if err != nil {
		return nil, err
	}
	var payload struct {
		Search struct {
			Nodes []*gqlPR `json:"nodes"`
		} `json:"search"`
	}
	if err := json.Unmarshal(data, &payload); err != nil {
		return nil, fmt.Errorf("%s: %w", prefix, err)
	}
	items := make([]PRListItem, 0, len(payload.Search.Nodes))
	for _, node := range payload.Search.Nodes {
		if node == nil {
			continue
		}
		items = append(items, node.toPRListItem())
	}
	return items, nil
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
