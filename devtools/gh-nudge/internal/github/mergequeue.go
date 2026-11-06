package github

import (
	"encoding/json"
	"fmt"
	"net/url"
	"sort"
	"strconv"
	"strings"
	"time"
)

const (
	// QueueEventAdded is an ADDED_TO_MERGE_QUEUE timeline event.
	QueueEventAdded = "added"
	// QueueEventRemoved is a REMOVED_FROM_MERGE_QUEUE timeline event.
	QueueEventRemoved = "removed"
)

// PullRef identifies one pull request. Raw is the text the caller passed.
type PullRef struct {
	Owner  string
	Repo   string
	Number int
	Raw    string
}

// QueueCheck is one status check on the pull request head.
type QueueCheck struct {
	Name       string
	Green      bool
	Conclusion string
}

// QueueEvent is one merge-queue timeline event, oldest information included.
type QueueEvent struct {
	ID        string
	Kind      string
	CreatedAt time.Time
	Reason    string
	CommitSHA string
}

// QueuePullRequest is the pull request state gh-merge queue and watch need.
type QueuePullRequest struct {
	Ref               PullRef
	NodeID            string
	Draft             bool
	ReviewDecision    string
	Labels            []string
	HeadSHA           string
	Checks            []QueueCheck
	UnresolvedThreads int
	Comments          []string
	Events            []QueueEvent
	Merged            bool
	// InMergeQueue flickers while a merge is still in progress.
	// Callers must not treat false by itself as an ejection.
	InMergeQueue bool
	// BaseRefName is the pull request base branch.
	BaseRefName string
	// HeadRefName is the pull request head branch.
	HeadRefName string
	// DefaultBranch is the repository's default branch from defaultBranchRef.
	DefaultBranch string
}

// BranchPull is one pull request matched by head or base branch name.
type BranchPull struct {
	Number      int
	State       string
	HeadRefName string
	BaseRefName string
}

// StackPR is one pull request in a stack or a merge batch.
type StackPR struct {
	Number   int
	State    string
	URL      string
	MergedAt time.Time
}

// FailureEvidence is the highest-ranked failing check run or commit status.
// Name and TargetURL identify that evidence. CheckRunURL and JobURL are set
// when a check run wins. A status winner keeps CheckRunURL empty unless a
// check run link contains /workflow/ or /workflows/, which a legacy CircleCI target needs.
// Log is the Actions failing-step log when TargetURL is a GitHub Actions job URL.
type FailureEvidence struct {
	Name        string
	TargetURL   string
	CheckRunURL string
	JobURL      string
	StepName    string
	Log         string
}

// ParsePullRef accepts owner/repo#n or a github.com pull request URL.
func ParsePullRef(raw string) (PullRef, error) {
	raw = strings.TrimSpace(raw)
	if owner, repo, number, ok := parseHashRef(raw); ok {
		return PullRef{Owner: owner, Repo: repo, Number: number, Raw: raw}, nil
	}
	owner, repo, number, err := parsePullRequestURL(raw)
	if err != nil {
		return PullRef{}, fmt.Errorf("invalid pull request %q", raw)
	}
	return PullRef{Owner: owner, Repo: repo, Number: number, Raw: raw}, nil
}

func parseHashRef(raw string) (string, string, int, bool) {
	ownerRepo, numText, ok := strings.Cut(raw, "#")
	if !ok || strings.Contains(ownerRepo, "://") || !strings.Contains(ownerRepo, "/") {
		return "", "", 0, false
	}
	owner, repo, ok := strings.Cut(ownerRepo, "/")
	if !ok || owner == "" || repo == "" || strings.Contains(repo, "/") {
		return "", "", 0, false
	}
	number, err := strconv.Atoi(numText)
	if err != nil || number <= 0 {
		return "", "", 0, false
	}
	return owner, repo, number, true
}

const queuePullRequestQuery = `
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    defaultBranchRef { name }
    pullRequest(number: $number) {
      id
      isDraft
      reviewDecision
      state
      merged
      isInMergeQueue
      headRefOid
      baseRefName
      headRefName
      url
      labels(first: 100) { nodes { name } }
      reviewThreads(first: 100) { nodes { isResolved } }
      commits(last: 1) {
        nodes {
          commit {
            statusCheckRollup {
              contexts(first: 100) {
                nodes {
                  ... on CheckRun { name conclusion status }
                  ... on StatusContext { context state }
                }
              }
            }
          }
        }
      }
      comments(last: 100) { nodes { body } }
      timelineItems(last: 100, itemTypes: [ADDED_TO_MERGE_QUEUE_EVENT, REMOVED_FROM_MERGE_QUEUE_EVENT]) {
        nodes {
          __typename
          ... on AddedToMergeQueueEvent { id createdAt }
          ... on RemovedFromMergeQueueEvent { id createdAt reason beforeCommit { oid } }
        }
      }
    }
  }
}`

const enqueuePullRequestMutation = `
mutation($id: ID!) {
  enqueuePullRequest(input: {pullRequestId: $id}) {
    mergeQueueEntry { id }
  }
}`

const dequeuePullRequestMutation = `
mutation($id: ID!) {
  dequeuePullRequest(input: {id: $id}) {
    mergeQueueEntry { id }
  }
}`

// LoadPullRequest reads the pull request state used by queue and watch.
func (c *Client) LoadPullRequest(raw string) (QueuePullRequest, error) {
	ref, err := ParsePullRef(raw)
	if err != nil {
		return QueuePullRequest{}, err
	}
	body, callErr := c.graphqlQuery(queuePullRequestQuery, []string{
		"-f", "owner=" + ref.Owner,
		"-f", "name=" + ref.Repo,
		"-F", "number=" + strconv.Itoa(ref.Number),
	})
	if err := graphqlCallError("load pull request "+ref.Raw, body, callErr); err != nil {
		return QueuePullRequest{}, err
	}
	pr, err := parseQueuePullRequest(body, ref)
	if err != nil {
		return QueuePullRequest{}, fmt.Errorf("load pull request %s: %w", ref.Raw, err)
	}
	return pr, nil
}

// DequeuePullRequest calls the dequeuePullRequest GraphQL mutation.
func (c *Client) DequeuePullRequest(nodeID string) error {
	body, callErr := c.graphqlQuery(dequeuePullRequestMutation, []string{"-f", "id=" + nodeID})
	if err := graphqlCallError("dequeue pull request", body, callErr); err != nil {
		return err
	}
	return nil
}

// EnqueuePullRequest calls the enqueuePullRequest GraphQL mutation.
func (c *Client) EnqueuePullRequest(nodeID string) error {
	body, callErr := c.graphqlQuery(enqueuePullRequestMutation, []string{"-f", "id=" + nodeID})
	if err := graphqlCallError("enqueue pull request", body, callErr); err != nil {
		return err
	}
	return nil
}

// OpenDownstack returns open pull requests below number in its stack.
// The stack is ordered from the bottom to the top.
func (c *Client) OpenDownstack(owner, repo string, number int) ([]StackPR, error) {
	stacks, err := c.listStacks(owner, repo, number)
	if err != nil {
		return nil, err
	}
	return openBelow(stacks, number), nil
}

// OpenAbove returns open pull requests above number, nearest first.
func (c *Client) OpenAbove(owner, repo string, number int) ([]StackPR, error) {
	stacks, err := c.listStacks(owner, repo, number)
	if err != nil {
		return nil, err
	}
	return openAbove(stacks, number), nil
}

// InNativeStack reports whether the native stacks response contains number.
// An empty list is not an error and reports false.
func (c *Client) InNativeStack(owner, repo string, number int) (bool, error) {
	stacks, err := c.listStacks(owner, repo, number)
	if err != nil {
		return false, err
	}
	_, _, ok := stackAt(stacks, number)
	return ok, nil
}

// OpenByBase returns open pull requests whose base branch is base.
func (c *Client) OpenByBase(owner, repo, base string) ([]BranchPull, error) {
	return c.listPullsByRef(owner, repo, "base", base)
}

// OpenByHead returns open pull requests whose head branch is head.
func (c *Client) OpenByHead(owner, repo, head string) ([]BranchPull, error) {
	return c.listPullsByRef(owner, repo, "head", owner+":"+head)
}

func (c *Client) listPullsByRef(owner, repo, key, value string) ([]BranchPull, error) {
	query := url.Values{}
	query.Set("state", "open")
	query.Set("per_page", "100")
	query.Set(key, value)
	path := fmt.Sprintf("repos/%s/%s/pulls?%s", owner, repo, query.Encode())
	out, err := c.executor.Execute("gh", "api", path)
	if err != nil {
		return nil, fmt.Errorf("list pulls for %s/%s: %w", owner, repo, err)
	}
	var raw []branchPullJSON
	if err := json.Unmarshal([]byte(out), &raw); err != nil {
		return nil, fmt.Errorf("parse pulls for %s/%s: %w", owner, repo, err)
	}
	return convertBranchPulls(raw), nil
}

func convertBranchPulls(raw []branchPullJSON) []BranchPull {
	out := make([]BranchPull, 0, len(raw))
	for _, item := range raw {
		out = append(out, BranchPull{
			Number:      item.Number,
			State:       item.State,
			HeadRefName: item.Head.Ref,
			BaseRefName: item.Base.Ref,
		})
	}
	return out
}

func (c *Client) listStacks(owner, repo string, number int) ([]stackResponse, error) {
	path := fmt.Sprintf("repos/%s/%s/stacks?pull_request=%d&per_page=100", owner, repo, number)
	out, err := c.executor.Execute("gh", "api", path)
	if err != nil {
		return nil, fmt.Errorf("list stacks for %s/%s#%d: %w", owner, repo, number, err)
	}
	var stacks []stackResponse
	if err := json.Unmarshal([]byte(out), &stacks); err != nil {
		return nil, fmt.Errorf("parse stacks for %s/%s#%d: %w", owner, repo, number, err)
	}
	return stacks, nil
}

// FailureEvidence ranks check runs and commit statuses, then reads an Actions
// job log when the winner's target URL points at one.
func (c *Client) FailureEvidence(owner, repo, sha string) (FailureEvidence, error) {
	checks, err := c.listCheckRuns(owner, repo, sha)
	if err != nil {
		return FailureEvidence{}, err
	}
	statuses, err := c.listCommitStatuses(owner, repo, sha)
	if err != nil {
		return FailureEvidence{}, err
	}
	chosen, ok := pickFailure(checks, statuses)
	if !ok {
		return FailureEvidence{}, nil
	}
	evidence := FailureEvidence{
		Name:        chosen.name,
		TargetURL:   chosen.targetURL,
		CheckRunURL: chosen.checkHTML,
		JobURL:      chosen.jobURL,
	}
	jobID, ok := jobIDFromURL(chosen.targetURL)
	if !ok {
		return evidence, nil
	}
	step, log, err := c.failingStepLog(owner, repo, jobID)
	if err != nil {
		return FailureEvidence{}, err
	}
	evidence.StepName = step
	evidence.Log = log
	if evidence.JobURL == "" {
		evidence.JobURL = chosen.targetURL
	}
	return evidence, nil
}

// LandedFirst returns the other pull request on sha that merged earliest.
func (c *Client) LandedFirst(owner, repo, sha string, self int) (StackPR, bool, error) {
	path := fmt.Sprintf("repos/%s/%s/commits/%s/pulls?per_page=100", owner, repo, sha)
	out, err := c.executor.Execute("gh", "api", path)
	if err != nil {
		return StackPR{}, false, fmt.Errorf("list pulls for %s: %w", sha, err)
	}
	var pulls []commitPullJSON
	if err := json.Unmarshal([]byte(out), &pulls); err != nil {
		return StackPR{}, false, fmt.Errorf("parse pulls for %s: %w", sha, err)
	}
	return earliestMerged(pulls, self)
}

// CommentOnPullRequest posts one issue comment on the pull request.
func (c *Client) CommentOnPullRequest(owner, repo string, number int, body string) error {
	payload, err := json.Marshal(struct {
		Body string `json:"body"`
	}{Body: body})
	if err != nil {
		return fmt.Errorf("encode comment: %w", err)
	}
	path := fmt.Sprintf("repos/%s/%s/issues/%d/comments", owner, repo, number)
	out, err := c.executor.ExecuteWithStdin(string(payload), "gh", "api", "--method", "POST", path, "--input", "-")
	if err != nil {
		return fmt.Errorf("comment on %s/%s#%d: %w\n%s", owner, repo, number, err, out)
	}
	return nil
}

// MergeAsync enqueues with PUT merge-async, merge_action=merge_queue, and sha.
func (c *Client) MergeAsync(owner, repo string, number int, sha string) error {
	body := fmt.Sprintf("{\"merge_action\":\"merge_queue\",\"sha\":%q}", sha)
	path := fmt.Sprintf("repos/%s/%s/pulls/%d/merge-async", owner, repo, number)
	out, err := c.executor.ExecuteWithStdin(body, "gh", "api", "--method", "PUT", path, "--input", "-")
	if err != nil {
		return fmt.Errorf("merge-async %s/%s#%d: %w\n%s", owner, repo, number, err, out)
	}
	return nil
}

func (c *Client) graphqlQuery(query string, fields []string) ([]byte, error) {
	args := make([]string, 0, 3+len(fields))
	args = append(args, "api", "graphql", "-f", "query="+query)
	args = append(args, fields...)
	out, err := c.executor.Execute("gh", args...)
	if err != nil {
		return []byte(out), fmt.Errorf("gh api graphql: %w", err)
	}
	return []byte(out), nil
}

func graphqlCallError(action string, body []byte, callErr error) error {
	msg := graphqlMessages(body)
	if callErr != nil {
		if msg == "" {
			return fmt.Errorf("%s: %w", action, callErr)
		}
		return fmt.Errorf("%s: %s: %w", action, msg, callErr)
	}
	if msg != "" {
		return fmt.Errorf("%s: %s", action, msg)
	}
	return nil
}

func graphqlMessages(body []byte) string {
	if len(body) == 0 {
		return ""
	}
	var resp struct {
		Errors []struct {
			Message string `json:"message"`
		} `json:"errors"`
	}
	if err := json.Unmarshal(body, &resp); err != nil || len(resp.Errors) == 0 {
		return ""
	}
	parts := make([]string, 0, len(resp.Errors))
	for _, item := range resp.Errors {
		if item.Message != "" {
			parts = append(parts, item.Message)
		}
	}
	return strings.Join(parts, "; ")
}

type queueQueryResponse struct {
	Data struct {
		Repository *struct {
			DefaultBranchRef *struct {
				Name string `json:"name"`
			} `json:"defaultBranchRef"`
			PullRequest *queuePRJSON `json:"pullRequest"`
		} `json:"repository"`
	} `json:"data"`
}

type queuePRJSON struct {
	ID             string `json:"id"`
	IsDraft        bool   `json:"isDraft"`
	ReviewDecision string `json:"reviewDecision"`
	State          string `json:"state"`
	Merged         bool   `json:"merged"`
	IsInMergeQueue bool   `json:"isInMergeQueue"`
	HeadRefOid     string `json:"headRefOid"`
	BaseRefName    string `json:"baseRefName"`
	HeadRefName    string `json:"headRefName"`
	URL            string `json:"url"`
	Labels         struct {
		Nodes []struct {
			Name string `json:"name"`
		} `json:"nodes"`
	} `json:"labels"`
	ReviewThreads struct {
		Nodes []struct {
			IsResolved bool `json:"isResolved"`
		} `json:"nodes"`
	} `json:"reviewThreads"`
	Commits struct {
		Nodes []struct {
			Commit struct {
				StatusCheckRollup *struct {
					Contexts struct {
						Nodes []queueCheckJSON `json:"nodes"`
					} `json:"contexts"`
				} `json:"statusCheckRollup"`
			} `json:"commit"`
		} `json:"nodes"`
	} `json:"commits"`
	Comments struct {
		Nodes []struct {
			Body string `json:"body"`
		} `json:"nodes"`
	} `json:"comments"`
	TimelineItems struct {
		Nodes []queueEventJSON `json:"nodes"`
	} `json:"timelineItems"`
}

type queueCheckJSON struct {
	Name       string `json:"name"`
	Context    string `json:"context"`
	Conclusion string `json:"conclusion"`
	State      string `json:"state"`
}

type queueEventJSON struct {
	TypeName     string    `json:"__typename"` //nolint:tagliatelle // GraphQL field name
	ID           string    `json:"id"`
	CreatedAt    time.Time `json:"createdAt"`
	Reason       string    `json:"reason"`
	BeforeCommit *struct {
		OID string `json:"oid"`
	} `json:"beforeCommit"`
}

func parseQueuePullRequest(body []byte, ref PullRef) (QueuePullRequest, error) {
	var resp queueQueryResponse
	if err := json.Unmarshal(body, &resp); err != nil {
		return QueuePullRequest{}, fmt.Errorf("parse graphql: %w", err)
	}
	repo := resp.Data.Repository
	if repo == nil || repo.PullRequest == nil {
		return QueuePullRequest{}, fmt.Errorf("pull request not found")
	}
	pr := convertQueuePR(repo.PullRequest, ref)
	if repo.DefaultBranchRef != nil {
		pr.DefaultBranch = repo.DefaultBranchRef.Name
	}
	return pr, nil
}

func convertQueuePR(raw *queuePRJSON, ref PullRef) QueuePullRequest {
	pr := QueuePullRequest{
		Ref:            ref,
		NodeID:         raw.ID,
		Draft:          raw.IsDraft,
		ReviewDecision: raw.ReviewDecision,
		HeadSHA:        raw.HeadRefOid,
		BaseRefName:    raw.BaseRefName,
		HeadRefName:    raw.HeadRefName,
		Merged:         raw.Merged || raw.State == "MERGED",
		InMergeQueue:   raw.IsInMergeQueue,
	}
	for _, label := range raw.Labels.Nodes {
		pr.Labels = append(pr.Labels, label.Name)
	}
	for _, thread := range raw.ReviewThreads.Nodes {
		if !thread.IsResolved {
			pr.UnresolvedThreads++
		}
	}
	pr.Checks = checksFrom(raw)
	for _, comment := range raw.Comments.Nodes {
		pr.Comments = append(pr.Comments, comment.Body)
	}
	pr.Events = eventsFrom(raw.TimelineItems.Nodes)
	return pr
}

func checksFrom(raw *queuePRJSON) []QueueCheck {
	if len(raw.Commits.Nodes) == 0 || raw.Commits.Nodes[0].Commit.StatusCheckRollup == nil {
		return nil
	}
	nodes := raw.Commits.Nodes[0].Commit.StatusCheckRollup.Contexts.Nodes
	checks := make([]QueueCheck, 0, len(nodes))
	for _, node := range nodes {
		name := node.Name
		if name == "" {
			name = node.Context
		}
		if name == "" {
			continue
		}
		conclusion := node.Conclusion
		if conclusion == "" {
			conclusion = node.State
		}
		checks = append(checks, QueueCheck{
			Name:       name,
			Green:      strings.EqualFold(conclusion, "SUCCESS"),
			Conclusion: conclusion,
		})
	}
	return checks
}

func eventsFrom(nodes []queueEventJSON) []QueueEvent {
	events := make([]QueueEvent, 0, len(nodes))
	for _, node := range nodes {
		kind := queueEventKind(node.TypeName)
		if kind == "" {
			continue
		}
		event := QueueEvent{ID: node.ID, Kind: kind, CreatedAt: node.CreatedAt, Reason: node.Reason}
		if node.BeforeCommit != nil {
			event.CommitSHA = node.BeforeCommit.OID
		}
		events = append(events, event)
	}
	sort.SliceStable(events, func(i, j int) bool {
		return events[i].CreatedAt.Before(events[j].CreatedAt)
	})
	return events
}

func queueEventKind(typeName string) string {
	switch typeName {
	case "AddedToMergeQueueEvent":
		return QueueEventAdded
	case "RemovedFromMergeQueueEvent":
		return QueueEventRemoved
	default:
		return ""
	}
}

type stackResponse struct {
	PullRequests []stackPullJSON `json:"pull_requests"` //nolint:tagliatelle // GitHub API uses snake_case
}

type stackPullJSON struct {
	Number int    `json:"number"`
	State  string `json:"state"`
}

type branchPullJSON struct {
	Number int    `json:"number"`
	State  string `json:"state"`
	Head   struct {
		Ref string `json:"ref"`
	} `json:"head"`
	Base struct {
		Ref string `json:"ref"`
	} `json:"base"`
}

func openBelow(stacks []stackResponse, number int) []StackPR {
	pulls, index, ok := stackAt(stacks, number)
	if !ok {
		return nil
	}
	return openStack(pulls[:index])
}

func openAbove(stacks []stackResponse, number int) []StackPR {
	pulls, index, ok := stackAt(stacks, number)
	if !ok || index+1 >= len(pulls) {
		return nil
	}
	return openStack(pulls[index+1:])
}

func stackAt(stacks []stackResponse, number int) ([]stackPullJSON, int, bool) {
	for _, stack := range stacks {
		for i, pr := range stack.PullRequests {
			if pr.Number == number {
				return stack.PullRequests, i, true
			}
		}
	}
	return nil, 0, false
}

func openStack(pulls []stackPullJSON) []StackPR {
	out := make([]StackPR, 0)
	for _, pr := range pulls {
		if pr.State != "open" {
			continue
		}
		out = append(out, StackPR{Number: pr.Number, State: pr.State})
	}
	return out
}

type checkRunsResponse struct {
	CheckRuns []checkRunJSON `json:"check_runs"` //nolint:tagliatelle // GitHub API uses snake_case
}

type checkRunJSON struct {
	Name       string `json:"name"`
	Conclusion string `json:"conclusion"`
	HTMLURL    string `json:"html_url"`    //nolint:tagliatelle // GitHub API uses snake_case
	DetailsURL string `json:"details_url"` //nolint:tagliatelle // GitHub API uses snake_case
}

type statusJSON struct {
	Context   string `json:"context"`
	State     string `json:"state"`
	TargetURL string `json:"target_url"` //nolint:tagliatelle // GitHub API uses snake_case
}

type combinedStatusResponse struct {
	Statuses []statusJSON `json:"statuses"`
}

const (
	rankFailure = iota
	rankError
	rankCancelled
	rankIgnore
)

type failureChoice struct {
	name      string
	targetURL string
	checkHTML string
	jobURL    string
	rank      int
}

func (c *Client) listCheckRuns(owner, repo, sha string) ([]checkRunJSON, error) {
	path := fmt.Sprintf("repos/%s/%s/commits/%s/check-runs?per_page=100", owner, repo, sha)
	out, err := c.executor.Execute("gh", "api", path)
	if err != nil {
		return nil, fmt.Errorf("list check runs for %s: %w", sha, err)
	}
	var resp checkRunsResponse
	if err := json.Unmarshal([]byte(out), &resp); err != nil {
		return nil, fmt.Errorf("parse check runs for %s: %w", sha, err)
	}
	return resp.CheckRuns, nil
}

func (c *Client) listCommitStatuses(owner, repo, sha string) ([]statusJSON, error) {
	path := fmt.Sprintf("repos/%s/%s/commits/%s/status?per_page=100", owner, repo, sha)
	out, err := c.executor.Execute("gh", "api", path)
	if err != nil {
		return nil, fmt.Errorf("list commit statuses for %s: %w", sha, err)
	}
	var resp combinedStatusResponse
	if err := json.Unmarshal([]byte(out), &resp); err != nil {
		return nil, fmt.Errorf("parse commit statuses for %s: %w", sha, err)
	}
	return resp.Statuses, nil
}

func pickFailure(checks []checkRunJSON, statuses []statusJSON) (failureChoice, bool) {
	var best failureChoice
	found := false
	consider := func(rank int, choice failureChoice) {
		if rank >= rankIgnore {
			return
		}
		if found && rank >= best.rank {
			return
		}
		choice.rank = rank
		best = choice
		found = true
	}
	for _, run := range checks {
		target := run.DetailsURL
		if target == "" {
			target = run.HTMLURL
		}
		consider(rankCheck(run.Conclusion), failureChoice{
			name:      run.Name,
			targetURL: target,
			checkHTML: run.HTMLURL,
			jobURL:    run.DetailsURL,
		})
	}
	for _, status := range statuses {
		consider(rankCommitStatus(status.State), failureChoice{
			name:      status.Context,
			targetURL: status.TargetURL,
		})
	}
	if found && best.checkHTML == "" && !hasCircleWorkflowPath(best.targetURL) {
		best.checkHTML = relatedWorkflowLink(checks, best.name)
	}
	return best, found
}

func hasCircleWorkflowPath(raw string) bool {
	return strings.Contains(raw, "/workflow/") || strings.Contains(raw, "/workflows/")
}

func relatedWorkflowLink(checks []checkRunJSON, name string) string {
	fallback := ""
	for _, run := range checks {
		link := ""
		for _, candidate := range []string{run.DetailsURL, run.HTMLURL} {
			if hasCircleWorkflowPath(candidate) {
				link = candidate
				break
			}
		}
		if link == "" {
			continue
		}
		if run.Name == name {
			return link
		}
		if fallback == "" {
			fallback = link
		}
	}
	return fallback
}

func rankCheck(conclusion string) int {
	switch strings.ToLower(conclusion) {
	case "failure", "timed_out":
		return rankFailure
	case "cancelled":
		return rankCancelled
	default:
		return rankIgnore
	}
}

func rankCommitStatus(state string) int {
	switch strings.ToLower(state) {
	case "failure":
		return rankFailure
	case "error":
		return rankError
	default:
		return rankIgnore
	}
}

type actionsJobJSON struct {
	Steps []struct {
		Name       string `json:"name"`
		Conclusion string `json:"conclusion"`
	} `json:"steps"`
}

func (c *Client) failingStepLog(owner, repo string, jobID int64) (string, string, error) {
	jobPath := fmt.Sprintf("repos/%s/%s/actions/jobs/%d", owner, repo, jobID)
	jobOut, err := c.executor.Execute("gh", "api", jobPath)
	if err != nil {
		return "", "", fmt.Errorf("get job %d: %w", jobID, err)
	}
	var job actionsJobJSON
	if err := json.Unmarshal([]byte(jobOut), &job); err != nil {
		return "", "", fmt.Errorf("parse job %d: %w", jobID, err)
	}
	logPath := jobPath + "/logs"
	logOut, err := c.executor.Execute("gh", "api", logPath)
	if err != nil {
		return "", "", fmt.Errorf("get job %d logs: %w", jobID, err)
	}
	step := failingStepName(job)
	return step, stepLog(logOut, step), nil
}

func failingStepName(job actionsJobJSON) string {
	for _, want := range []string{"failure", "timed_out", "cancelled"} {
		for _, step := range job.Steps {
			if strings.EqualFold(step.Conclusion, want) {
				return step.Name
			}
		}
	}
	return ""
}

func stepLog(log, step string) string {
	if step == "" {
		return log
	}
	lines := strings.Split(log, "\n")
	start := -1
	for i, line := range lines {
		if strings.Contains(line, step) {
			start = i
			break
		}
	}
	if start < 0 {
		return log
	}
	end := len(lines)
	for i := start + 1; i < len(lines); i++ {
		if strings.Contains(lines[i], "##[group]") {
			end = i
			break
		}
	}
	return strings.Join(lines[start:end], "\n")
}

func jobIDFromURL(raw string) (int64, bool) {
	parsed, err := url.Parse(raw)
	if err != nil {
		return 0, false
	}
	parts := strings.Split(strings.Trim(parsed.Path, "/"), "/")
	for i, part := range parts {
		if part != "job" || i+1 >= len(parts) {
			continue
		}
		id, err := strconv.ParseInt(parts[i+1], 10, 64)
		if err != nil {
			return 0, false
		}
		return id, true
	}
	return 0, false
}

type commitPullJSON struct {
	Number   int        `json:"number"`
	HTMLURL  string     `json:"html_url"`  //nolint:tagliatelle // GitHub API uses snake_case
	MergedAt *time.Time `json:"merged_at"` //nolint:tagliatelle // GitHub API uses snake_case
}

func earliestMerged(pulls []commitPullJSON, self int) (StackPR, bool, error) {
	var best StackPR
	found := false
	for _, pr := range pulls {
		if pr.Number == self || pr.MergedAt == nil || pr.MergedAt.IsZero() {
			continue
		}
		if found && !pr.MergedAt.Before(best.MergedAt) {
			continue
		}
		found = true
		best = StackPR{Number: pr.Number, URL: pr.HTMLURL, MergedAt: *pr.MergedAt}
	}
	return best, found, nil
}
