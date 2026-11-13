package github

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// userReviewRequestedQuery finds open pull requests that request the viewer
// as an individual. A request to a team the viewer belongs to does not match.
const userReviewRequestedQuery = "is:pr is:open archived:false user-review-requested:@me"

// ApprovalPullRequest is the pull request state gh-approve evaluates.
// A non-empty *Err field means that data could not be read completely.
type ApprovalPullRequest struct {
	Ref          PullRef
	URL          string
	Title        string
	State        string
	Author       string
	HeadSHA      string
	Draft        bool
	Additions    int
	Deletions    int
	ChangedFiles int
	Files        []models.File
	FilesErr     string
	// Patches are the REST file patches. Nil means they were not fetched.
	Patches    []PullFile
	PatchesErr string
	Labels     []string
	LabelsErr  string
	// RequestedUsers are the logins requested as individuals.
	RequestedUsers []string
	// RequestedTeams are the slugs of requested teams.
	RequestedTeams    []string
	Reviews           []ApprovalReview
	UnresolvedThreads int
	ThreadsErr        string
	// Checks are the status checks on the head commit.
	Checks    []ApprovalCheck
	ChecksErr string
	// BaseRef is the base branch name.
	BaseRef string
	// Ownership is set by gh-approve when a rule sets codeowners_covered.
	// Nil means it was not read.
	Ownership *Ownership
}

// ApprovalReview is one submitted review, oldest first.
type ApprovalReview struct {
	Author    string
	State     string
	CommitSHA string
}

// ApprovalCheck is one status check on the head commit.
// Outcome is models.ChecksPass, models.ChecksFail, or models.ChecksPending.
type ApprovalCheck struct {
	Name     string
	Required bool
	Outcome  string
}

const viewerQuery = `query { viewer { login } }`

// ViewerLogin returns the login of the authenticated gh user.
func (c *Client) ViewerLogin() (string, error) {
	body, callErr := c.graphqlQuery(viewerQuery, nil)
	if err := graphqlCallError("read viewer", body, callErr); err != nil {
		return "", err
	}
	var resp struct {
		Data struct {
			Viewer struct {
				Login string `json:"login"`
			} `json:"viewer"`
		} `json:"data"`
	}
	if err := body.decode(&resp); err != nil {
		return "", fmt.Errorf("read viewer: %w", err)
	}
	if resp.Data.Viewer.Login == "" {
		return "", fmt.Errorf("read viewer: empty login")
	}
	return resp.Data.Viewer.Login, nil
}

const searchPullRequestsQuery = `
query($q: String!, $after: String) {
  search(query: $q, type: ISSUE, first: 100, after: $after) {
    pageInfo { hasNextPage endCursor }
    nodes {
      ... on PullRequest { number url repository { nameWithOwner } }
    }
  }
}`

// SearchUserReviewRequested lists open pull requests that request the viewer
// as an individual, across all pages.
func (c *Client) SearchUserReviewRequested() ([]PullRef, error) {
	var refs []PullRef
	after := ""
	for {
		fields := []string{"-f", "q=" + userReviewRequestedQuery}
		if after != "" {
			fields = append(fields, "-f", "after="+after)
		}
		body, callErr := c.graphqlQuery(searchPullRequestsQuery, fields)
		if err := graphqlCallError("search review requests", body, callErr); err != nil {
			return nil, err
		}
		var resp struct {
			Data struct {
				Search struct {
					PageInfo struct {
						HasNextPage bool   `json:"hasNextPage"`
						EndCursor   string `json:"endCursor"`
					} `json:"pageInfo"`
					Nodes []struct {
						Number     int    `json:"number"`
						URL        string `json:"url"`
						Repository struct {
							NameWithOwner string `json:"nameWithOwner"`
						} `json:"repository"`
					} `json:"nodes"`
				} `json:"search"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return nil, fmt.Errorf("search review requests: %w", err)
		}
		for _, node := range resp.Data.Search.Nodes {
			owner, repo, ok := strings.Cut(node.Repository.NameWithOwner, "/")
			if node.Number == 0 || !ok {
				continue
			}
			refs = append(refs, PullRef{Owner: owner, Repo: repo, Number: node.Number, Raw: node.URL})
		}
		page := resp.Data.Search.PageInfo
		if !page.HasNextPage || page.EndCursor == "" {
			return refs, nil
		}
		after = page.EndCursor
	}
}

const approvalPullRequestQuery = `
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    nameWithOwner
    pullRequest(number: $number) {
      number url title state isDraft headRefOid baseRefName
      author { login }
      additions deletions changedFiles
      files(first: 100) { pageInfo { hasNextPage } nodes { path additions deletions } }
      labels(first: 100) { pageInfo { hasNextPage } nodes { name } }
      reviewRequests(first: 100) {
        nodes { requestedReviewer { __typename ... on User { login } ... on Team { slug } } }
      }
      reviews(last: 100) { nodes { author { login } state commit { oid } } }
      reviewThreads(first: 100) { totalCount nodes { isResolved } }
      commits(last: 1) {
        nodes {
          commit {
            oid
            statusCheckRollup {
              contexts(first: 100) {
                pageInfo { hasNextPage }
                nodes {
                  __typename
                  ... on CheckRun { name status conclusion isRequired(pullRequestNumber: $number) }
                  ... on StatusContext { context state isRequired(pullRequestNumber: $number) }
                }
              }
            }
          }
        }
      }
    }
  }
}`

type pageInfoJSON struct {
	HasNextPage bool `json:"hasNextPage"`
}

type approvalPRJSON struct {
	Number      int    `json:"number"`
	URL         string `json:"url"`
	Title       string `json:"title"`
	State       string `json:"state"`
	IsDraft     bool   `json:"isDraft"`
	HeadRefOid  string `json:"headRefOid"`
	BaseRefName string `json:"baseRefName"`
	Author      *struct {
		Login string `json:"login"`
	} `json:"author"`
	Additions    int `json:"additions"`
	Deletions    int `json:"deletions"`
	ChangedFiles int `json:"changedFiles"`
	Files        *struct {
		PageInfo pageInfoJSON  `json:"pageInfo"`
		Nodes    []models.File `json:"nodes"`
	} `json:"files"`
	Labels *struct {
		PageInfo pageInfoJSON `json:"pageInfo"`
		Nodes    []struct {
			Name string `json:"name"`
		} `json:"nodes"`
	} `json:"labels"`
	ReviewRequests struct {
		Nodes []struct {
			RequestedReviewer *struct {
				TypeName string `json:"__typename"` //nolint:tagliatelle // GraphQL field name
				Login    string `json:"login"`
				Slug     string `json:"slug"`
			} `json:"requestedReviewer"`
		} `json:"nodes"`
	} `json:"reviewRequests"`
	Reviews struct {
		Nodes []struct {
			Author *struct {
				Login string `json:"login"`
			} `json:"author"`
			State  string `json:"state"`
			Commit *struct {
				OID string `json:"oid"`
			} `json:"commit"`
		} `json:"nodes"`
	} `json:"reviews"`
	ReviewThreads *struct {
		TotalCount int `json:"totalCount"`
		Nodes      []struct {
			IsResolved bool `json:"isResolved"`
		} `json:"nodes"`
	} `json:"reviewThreads"`
	Commits struct {
		Nodes []struct {
			Commit struct {
				OID               string `json:"oid"`
				StatusCheckRollup *struct {
					Contexts struct {
						PageInfo pageInfoJSON        `json:"pageInfo"`
						Nodes    []approvalCheckJSON `json:"nodes"`
					} `json:"contexts"`
				} `json:"statusCheckRollup"`
			} `json:"commit"`
		} `json:"nodes"`
	} `json:"commits"`
}

type approvalCheckJSON struct {
	TypeName   string `json:"__typename"` //nolint:tagliatelle // GraphQL field name
	Name       string `json:"name"`
	Status     string `json:"status"`
	Conclusion string `json:"conclusion"`
	Context    string `json:"context"`
	State      string `json:"state"`
	IsRequired bool   `json:"isRequired"`
}

// LoadApprovalPullRequest reads the pull request state gh-approve evaluates.
// Data that cannot be read completely is reported in the *Err fields.
func (c *Client) LoadApprovalPullRequest(ref PullRef) (ApprovalPullRequest, error) {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	body, callErr := c.graphqlQuery(approvalPullRequestQuery, []string{
		"-f", "owner=" + ref.Owner,
		"-f", "name=" + ref.Repo,
		"-F", "number=" + strconv.Itoa(ref.Number),
	})
	if err := graphqlCallError("load pull request "+name, body, callErr); err != nil {
		return ApprovalPullRequest{}, err
	}
	var resp struct {
		Data struct {
			Repository *struct {
				PullRequest *approvalPRJSON `json:"pullRequest"`
			} `json:"repository"`
		} `json:"data"`
	}
	if err := body.decode(&resp); err != nil {
		return ApprovalPullRequest{}, fmt.Errorf("load pull request %s: %w", name, err)
	}
	if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
		return ApprovalPullRequest{}, fmt.Errorf("load pull request %s: not found", name)
	}
	return convertApprovalPR(resp.Data.Repository.PullRequest, ref), nil
}

func convertApprovalPR(raw *approvalPRJSON, ref PullRef) ApprovalPullRequest {
	pr := ApprovalPullRequest{
		Ref:          ref,
		URL:          raw.URL,
		Title:        raw.Title,
		State:        raw.State,
		HeadSHA:      raw.HeadRefOid,
		BaseRef:      raw.BaseRefName,
		Draft:        raw.IsDraft,
		Additions:    raw.Additions,
		Deletions:    raw.Deletions,
		ChangedFiles: raw.ChangedFiles,
	}
	if raw.Author != nil {
		pr.Author = raw.Author.Login
	}
	pr.Files, pr.FilesErr = approvalFiles(raw)
	pr.Labels, pr.LabelsErr = approvalLabels(raw)
	pr.RequestedUsers, pr.RequestedTeams = requestedReviewers(raw)
	pr.Reviews = approvalReviews(raw)
	pr.UnresolvedThreads, pr.ThreadsErr = unresolvedThreads(raw)
	pr.Checks, pr.ChecksErr = approvalChecks(raw)
	return pr
}

func approvalFiles(raw *approvalPRJSON) ([]models.File, string) {
	switch {
	case raw.Files == nil:
		return nil, "file list missing"
	case raw.Files.PageInfo.HasNextPage:
		return nil, "more than 100 changed files; file list incomplete"
	default:
		return raw.Files.Nodes, ""
	}
}

func approvalLabels(raw *approvalPRJSON) ([]string, string) {
	switch {
	case raw.Labels == nil:
		return nil, "labels missing"
	case raw.Labels.PageInfo.HasNextPage:
		return nil, "more than 100 labels; label list incomplete"
	}
	labels := make([]string, 0, len(raw.Labels.Nodes))
	for _, label := range raw.Labels.Nodes {
		labels = append(labels, label.Name)
	}
	return labels, ""
}

func requestedReviewers(raw *approvalPRJSON) (users, teams []string) {
	for _, node := range raw.ReviewRequests.Nodes {
		reviewer := node.RequestedReviewer
		if reviewer == nil {
			continue
		}
		switch reviewer.TypeName {
		case "User":
			users = append(users, reviewer.Login)
		case "Team":
			teams = append(teams, reviewer.Slug)
		}
	}
	return users, teams
}

func approvalReviews(raw *approvalPRJSON) []ApprovalReview {
	reviews := make([]ApprovalReview, 0, len(raw.Reviews.Nodes))
	for _, node := range raw.Reviews.Nodes {
		review := ApprovalReview{State: node.State}
		if node.Author != nil {
			review.Author = node.Author.Login
		}
		if node.Commit != nil {
			review.CommitSHA = node.Commit.OID
		}
		reviews = append(reviews, review)
	}
	return reviews
}

func unresolvedThreads(raw *approvalPRJSON) (int, string) {
	threads := raw.ReviewThreads
	if threads == nil {
		return 0, "review thread data missing"
	}
	if threads.TotalCount > len(threads.Nodes) {
		return 0, fmt.Sprintf("read %d of %d review threads", len(threads.Nodes), threads.TotalCount)
	}
	unresolved := 0
	for _, thread := range threads.Nodes {
		if !thread.IsResolved {
			unresolved++
		}
	}
	return unresolved, ""
}

func approvalChecks(raw *approvalPRJSON) ([]ApprovalCheck, string) {
	if len(raw.Commits.Nodes) == 0 {
		return nil, "head commit missing"
	}
	commit := raw.Commits.Nodes[0].Commit
	if commit.OID != raw.HeadRefOid {
		return nil, fmt.Sprintf("check data is for %s, not head %s", commit.OID, raw.HeadRefOid)
	}
	if commit.StatusCheckRollup == nil {
		return nil, "check data missing on head commit"
	}
	contexts := commit.StatusCheckRollup.Contexts
	if contexts.PageInfo.HasNextPage {
		return nil, "more than 100 checks; check list incomplete"
	}
	checks := make([]ApprovalCheck, 0, len(contexts.Nodes))
	for _, node := range contexts.Nodes {
		status := models.StatusCheck{
			Name:       node.Name,
			Status:     node.Status,
			Conclusion: node.Conclusion,
			State:      node.State,
			Context:    node.Context,
		}
		name := node.Name
		if name == "" {
			name = node.Context
		}
		outcome := (&models.PullRequest{StatusCheckRollup: []models.StatusCheck{status}}).ChecksStatus()
		checks = append(checks, ApprovalCheck{Name: name, Required: node.IsRequired, Outcome: outcome})
	}
	return checks, ""
}

// PullFile is one changed file from the pulls/{n}/files REST list.
// PatchSet is false when GitHub omitted the patch, as it does for a
// binary file or a patch that is too large.
type PullFile struct {
	Path      string
	Previous  string
	Status    string
	Additions int
	Deletions int
	Patch     string
	PatchSet  bool
}

type pullFileJSON struct {
	Filename         string  `json:"filename"`
	PreviousFilename string  `json:"previous_filename"` //nolint:tagliatelle // GitHub API uses snake_case
	Status           string  `json:"status"`
	Additions        int     `json:"additions"`
	Deletions        int     `json:"deletions"`
	Patch            *string `json:"patch"`
}

// PullRequestFilePatches reads every changed file's patch. gh paginates
// the REST list. A file whose patch GitHub omitted has PatchSet false.
func (c *Client) PullRequestFilePatches(ref PullRef) ([]PullFile, error) {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	path := fmt.Sprintf("repos/%s/%s/pulls/%d/files?per_page=100", ref.Owner, ref.Repo, ref.Number)
	output, err := c.runGHJSON("api", "--paginate", path)
	if err != nil {
		return nil, fmt.Errorf("read patches of %s: %w", name, err)
	}
	var raw []pullFileJSON
	if err := output.decode(&raw); err != nil {
		return nil, fmt.Errorf("read patches of %s: %w", name, err)
	}
	files := make([]PullFile, len(raw))
	for i, file := range raw {
		got := PullFile{
			Path:      file.Filename,
			Previous:  file.PreviousFilename,
			Status:    file.Status,
			Additions: file.Additions,
			Deletions: file.Deletions,
			PatchSet:  file.Patch != nil,
		}
		if file.Patch != nil {
			got.Patch = *file.Patch
		}
		files[i] = got
	}
	return files, nil
}

// PullRequestHeadSHA reads the current head SHA through the REST API.
func (c *Client) PullRequestHeadSHA(ref PullRef) (string, error) {
	path := fmt.Sprintf("repos/%s/%s/pulls/%d", ref.Owner, ref.Repo, ref.Number)
	out, err := c.executor.Execute("gh", "api", path, "--jq", ".head.sha")
	if err != nil {
		return "", fmt.Errorf("read head of %s/%s#%d: %w\n%s", ref.Owner, ref.Repo, ref.Number, err, out)
	}
	sha := strings.TrimSpace(out)
	if sha == "" {
		return "", fmt.Errorf("read head of %s/%s#%d: empty head SHA", ref.Owner, ref.Repo, ref.Number)
	}
	return sha, nil
}

// PullRequestDiff reads the pull request's unified diff through the REST API.
// Only stdout is returned, so a gh warning cannot end up in the diff.
func (c *Client) PullRequestDiff(ref PullRef) (string, error) {
	path := fmt.Sprintf("repos/%s/%s/pulls/%d", ref.Owner, ref.Repo, ref.Number)
	stdout, stderr, err := c.executeSeparated("gh", "api", path, "-H", "Accept: application/vnd.github.diff")
	if err != nil {
		return "", fmt.Errorf("read diff of %s/%s#%d: %w\n%s", ref.Owner, ref.Repo, ref.Number, err, stderr)
	}
	return stdout, nil
}

// ApprovePullRequest posts an APPROVE review pinned to commitSHA.
// GitHub rejects the review when commitSHA is not part of the pull request.
func (c *Client) ApprovePullRequest(ref PullRef, commitSHA, body string) error {
	payload, err := json.Marshal(map[string]string{
		"commit_id": commitSHA,
		"event":     "APPROVE",
		"body":      body,
	})
	if err != nil {
		return fmt.Errorf("approve %s/%s#%d: %w", ref.Owner, ref.Repo, ref.Number, err)
	}
	path := fmt.Sprintf("repos/%s/%s/pulls/%d/reviews", ref.Owner, ref.Repo, ref.Number)
	out, err := c.executor.ExecuteWithStdin(string(payload), "gh", "api", "--method", "POST", path, "--input", "-")
	if err != nil {
		return fmt.Errorf("approve %s/%s#%d: %w\n%s", ref.Owner, ref.Repo, ref.Number, err, out)
	}
	return nil
}
