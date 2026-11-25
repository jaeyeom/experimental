package github

import (
	"fmt"
	"strconv"
	"strings"
	"time"
)

// RerequestPRState holds the pull request data needed to decide which
// reviewers to re-request.
type RerequestPRState struct {
	Number          int
	State           string
	IsDraft         bool
	Author          string
	RequestedUsers  []string
	RequestedTeams  []string
	Reviews         []RerequestReview
	Threads         []RerequestThread
	AuthorPushTimes []time.Time
}

// RerequestReview is one submitted review on a pull request.
type RerequestReview struct {
	Author      string
	State       string
	SubmittedAt time.Time
}

// RerequestThread is one review thread on a pull request.
type RerequestThread struct {
	IsResolved bool
	Comments   []RerequestThreadComment
}

// RerequestThreadComment is one comment in a review thread.
type RerequestThreadComment struct {
	Author    string
	CreatedAt time.Time
}

const rerequestPRStateQuery = `
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      number
      state
      isDraft
      author { login }
      reviewRequests(first: 100) {
        pageInfo { hasNextPage endCursor }
        nodes {
          requestedReviewer {
            __typename
            ... on User { login }
            ... on Team { slug }
          }
        }
      }
      reviews(first: 100) {
        pageInfo { hasNextPage endCursor }
        nodes {
          author { login }
          state
          submittedAt
        }
      }
      reviewThreads(first: 100) {
        pageInfo { hasNextPage endCursor }
        nodes {
          id
          isResolved
          comments(first: 100) {
            pageInfo { hasNextPage endCursor }
            nodes {
              author { login }
              createdAt
            }
          }
        }
      }
      commits(first: 100) {
        pageInfo { hasNextPage endCursor }
        nodes {
          commit {
            committedDate
            pushedDate
            author { user { login } }
            committer { user { login } }
          }
        }
      }
      timelineItems(itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT], first: 100) {
        pageInfo { hasNextPage endCursor }
        nodes {
          ... on HeadRefForcePushedEvent {
            createdAt
            actor { login }
          }
        }
      }
    }
  }
}`

const rerequestReviewRequestsPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $after: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviewRequests(first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes {
          requestedReviewer {
            __typename
            ... on User { login }
            ... on Team { slug }
          }
        }
      }
    }
  }
}`

const rerequestReviewsPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $after: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviews(first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes {
          author { login }
          state
          submittedAt
        }
      }
    }
  }
}`

const rerequestThreadsPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $after: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviewThreads(first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes {
          id
          isResolved
          comments(first: 100) {
            pageInfo { hasNextPage endCursor }
            nodes {
              author { login }
              createdAt
            }
          }
        }
      }
    }
  }
}`

const rerequestThreadCommentsPageQuery = `
query($id: ID!, $after: String!) {
  node(id: $id) {
    ... on PullRequestReviewThread {
      comments(first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes {
          author { login }
          createdAt
        }
      }
    }
  }
}`

const rerequestCommitsPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $after: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      commits(first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes {
          commit {
            committedDate
            pushedDate
            author { user { login } }
            committer { user { login } }
          }
        }
      }
    }
  }
}`

const rerequestForcePushesPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $after: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      timelineItems(itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT], first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes {
          ... on HeadRefForcePushedEvent {
            createdAt
            actor { login }
          }
        }
      }
    }
  }
}`

type cursorPageInfoJSON struct {
	HasNextPage bool   `json:"hasNextPage"`
	EndCursor   string `json:"endCursor"`
}

type rerequestReviewerNodeJSON struct {
	RequestedReviewer *struct {
		TypeName string `json:"__typename"` //nolint:tagliatelle // GraphQL field name
		Login    string `json:"login"`
		Slug     string `json:"slug"`
	} `json:"requestedReviewer"`
}

type rerequestReviewNodeJSON struct {
	Author *struct {
		Login string `json:"login"`
	} `json:"author"`
	State       string `json:"state"`
	SubmittedAt string `json:"submittedAt"`
}

type rerequestCommentNodeJSON struct {
	Author *struct {
		Login string `json:"login"`
	} `json:"author"`
	CreatedAt string `json:"createdAt"`
}

type rerequestThreadNodeJSON struct {
	ID         string `json:"id"`
	IsResolved bool   `json:"isResolved"`
	Comments   struct {
		PageInfo cursorPageInfoJSON         `json:"pageInfo"`
		Nodes    []rerequestCommentNodeJSON `json:"nodes"`
	} `json:"comments"`
}

type rerequestCommitNodeJSON struct {
	Commit struct {
		CommittedDate string `json:"committedDate"`
		PushedDate    string `json:"pushedDate"`
		Author        *struct {
			User *struct {
				Login string `json:"login"`
			} `json:"user"`
		} `json:"author"`
		Committer *struct {
			User *struct {
				Login string `json:"login"`
			} `json:"user"`
		} `json:"committer"`
	} `json:"commit"`
}

type rerequestForcePushNodeJSON struct {
	CreatedAt string `json:"createdAt"`
	Actor     *struct {
		Login string `json:"login"`
	} `json:"actor"`
}

type rerequestPRStateJSON struct {
	Number  int    `json:"number"`
	State   string `json:"state"`
	IsDraft bool   `json:"isDraft"`
	Author  *struct {
		Login string `json:"login"`
	} `json:"author"`
	ReviewRequests struct {
		PageInfo cursorPageInfoJSON          `json:"pageInfo"`
		Nodes    []rerequestReviewerNodeJSON `json:"nodes"`
	} `json:"reviewRequests"`
	Reviews struct {
		PageInfo cursorPageInfoJSON        `json:"pageInfo"`
		Nodes    []rerequestReviewNodeJSON `json:"nodes"`
	} `json:"reviews"`
	ReviewThreads struct {
		PageInfo cursorPageInfoJSON        `json:"pageInfo"`
		Nodes    []rerequestThreadNodeJSON `json:"nodes"`
	} `json:"reviewThreads"`
	Commits struct {
		PageInfo cursorPageInfoJSON        `json:"pageInfo"`
		Nodes    []rerequestCommitNodeJSON `json:"nodes"`
	} `json:"commits"`
	TimelineItems struct {
		PageInfo cursorPageInfoJSON           `json:"pageInfo"`
		Nodes    []rerequestForcePushNodeJSON `json:"nodes"`
	} `json:"timelineItems"`
}

// LoadRerequestPRState loads the pull request metadata, pending requests,
// submitted reviews, review threads, and author push timestamps for rerequest
// evaluation.
func (c *Client) LoadRerequestPRState(ref PullRef) (RerequestPRState, error) {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	fields := []string{
		"-f", "owner=" + ref.Owner,
		"-f", "name=" + ref.Repo,
		"-F", "number=" + strconv.Itoa(ref.Number),
	}
	body, callErr := c.graphqlQuery(rerequestPRStateQuery, fields)
	if err := graphqlCallError("read pull request "+name, body, callErr); err != nil {
		return RerequestPRState{}, err
	}

	var resp struct {
		Data struct {
			Repository *struct {
				PullRequest *rerequestPRStateJSON `json:"pullRequest"`
			} `json:"repository"`
		} `json:"data"`
	}
	if err := body.decode(&resp); err != nil {
		return RerequestPRState{}, fmt.Errorf("read pull request %s: %w", name, err)
	}
	if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
		return RerequestPRState{}, fmt.Errorf("read pull request %s: not found", name)
	}

	return c.populateRerequestPRState(ref, name, resp.Data.Repository.PullRequest)
}

func (c *Client) populateRerequestPRState(ref PullRef, name string, raw *rerequestPRStateJSON) (RerequestPRState, error) {
	state := RerequestPRState{
		Number:  raw.Number,
		State:   raw.State,
		IsDraft: raw.IsDraft,
	}
	if state.Number == 0 {
		state.Number = ref.Number
	}
	if raw.Author != nil {
		state.Author = raw.Author.Login
	}

	steps := []func() error{
		func() error { return c.loadRerequestReviewRequests(ref, raw, &state) },
		func() error { return c.loadRerequestReviews(ref, name, raw, &state) },
		func() error { return c.loadRerequestThreads(ref, name, raw, &state) },
		func() error { return c.loadRerequestCommits(ref, name, raw, &state) },
		func() error { return c.loadRerequestForcePushes(ref, name, raw, &state) },
	}
	for _, step := range steps {
		if err := step(); err != nil {
			return RerequestPRState{}, err
		}
	}
	return state, nil
}

func (c *Client) loadRerequestReviewRequests(ref PullRef, raw *rerequestPRStateJSON, state *RerequestPRState) error {
	appendRequestedReviewers(state, raw.ReviewRequests.Nodes)
	if raw.ReviewRequests.PageInfo.HasNextPage && raw.ReviewRequests.PageInfo.EndCursor != "" {
		return c.paginateRerequestReviewRequests(ref, raw.ReviewRequests.PageInfo.EndCursor, state)
	}
	return nil
}

func (c *Client) loadRerequestReviews(ref PullRef, name string, raw *rerequestPRStateJSON, state *RerequestPRState) error {
	if err := appendRerequestReviews(state, raw.Reviews.Nodes); err != nil {
		return fmt.Errorf("read pull request %s: %w", name, err)
	}
	if raw.Reviews.PageInfo.HasNextPage && raw.Reviews.PageInfo.EndCursor != "" {
		return c.paginateRerequestReviews(ref, raw.Reviews.PageInfo.EndCursor, state)
	}
	return nil
}

func (c *Client) loadRerequestThreads(ref PullRef, name string, raw *rerequestPRStateJSON, state *RerequestPRState) error {
	if err := c.appendRerequestThreads(name, raw.ReviewThreads.Nodes, state); err != nil {
		return err
	}
	if raw.ReviewThreads.PageInfo.HasNextPage && raw.ReviewThreads.PageInfo.EndCursor != "" {
		return c.paginateRerequestThreads(ref, raw.ReviewThreads.PageInfo.EndCursor, state)
	}
	return nil
}

func (c *Client) loadRerequestCommits(ref PullRef, name string, raw *rerequestPRStateJSON, state *RerequestPRState) error {
	if err := appendRerequestCommits(state, raw.Commits.Nodes); err != nil {
		return fmt.Errorf("read pull request %s: %w", name, err)
	}
	if raw.Commits.PageInfo.HasNextPage && raw.Commits.PageInfo.EndCursor != "" {
		return c.paginateRerequestCommits(ref, raw.Commits.PageInfo.EndCursor, state)
	}
	return nil
}

func (c *Client) loadRerequestForcePushes(ref PullRef, name string, raw *rerequestPRStateJSON, state *RerequestPRState) error {
	if err := appendRerequestForcePushes(state, raw.TimelineItems.Nodes); err != nil {
		return fmt.Errorf("read pull request %s: %w", name, err)
	}
	if raw.TimelineItems.PageInfo.HasNextPage && raw.TimelineItems.PageInfo.EndCursor != "" {
		return c.paginateRerequestForcePushes(ref, raw.TimelineItems.PageInfo.EndCursor, state)
	}
	return nil
}

func appendRequestedReviewers(state *RerequestPRState, nodes []rerequestReviewerNodeJSON) {
	for _, node := range nodes {
		if node.RequestedReviewer == nil {
			continue
		}
		switch node.RequestedReviewer.TypeName {
		case "User":
			if node.RequestedReviewer.Login != "" {
				state.RequestedUsers = append(state.RequestedUsers, node.RequestedReviewer.Login)
			}
		case "Team":
			if node.RequestedReviewer.Slug != "" {
				state.RequestedTeams = append(state.RequestedTeams, node.RequestedReviewer.Slug)
			}
		}
	}
}

func appendRerequestReviews(state *RerequestPRState, nodes []rerequestReviewNodeJSON) error {
	for _, node := range nodes {
		if node.Author == nil || node.Author.Login == "" || node.State == "PENDING" || node.SubmittedAt == "" {
			continue
		}
		ts, err := time.Parse(time.RFC3339, node.SubmittedAt)
		if err != nil {
			return fmt.Errorf("parse review submittedAt %q: %w", node.SubmittedAt, err)
		}
		state.Reviews = append(state.Reviews, RerequestReview{
			Author:      node.Author.Login,
			State:       node.State,
			SubmittedAt: ts.UTC(),
		})
	}
	return nil
}

func (c *Client) appendRerequestThreads(prName string, nodes []rerequestThreadNodeJSON, state *RerequestPRState) error {
	for _, node := range nodes {
		comments, err := convertRerequestComments(node.Comments.Nodes)
		if err != nil {
			return fmt.Errorf("read pull request %s: %w", prName, err)
		}
		if node.Comments.PageInfo.HasNextPage && node.Comments.PageInfo.EndCursor != "" && node.ID != "" {
			more, err := c.paginateRerequestThreadComments(prName, node.ID, node.Comments.PageInfo.EndCursor)
			if err != nil {
				return err
			}
			comments = append(comments, more...)
		}
		state.Threads = append(state.Threads, RerequestThread{
			IsResolved: node.IsResolved,
			Comments:   comments,
		})
	}
	return nil
}

func convertRerequestComments(nodes []rerequestCommentNodeJSON) ([]RerequestThreadComment, error) {
	comments := make([]RerequestThreadComment, 0, len(nodes))
	for _, node := range nodes {
		if node.CreatedAt == "" {
			continue
		}
		ts, err := time.Parse(time.RFC3339, node.CreatedAt)
		if err != nil {
			return nil, fmt.Errorf("parse review comment createdAt %q: %w", node.CreatedAt, err)
		}
		author := ""
		if node.Author != nil {
			author = node.Author.Login
		}
		comments = append(comments, RerequestThreadComment{
			Author:    author,
			CreatedAt: ts.UTC(),
		})
	}
	return comments, nil
}

func appendRerequestCommits(state *RerequestPRState, nodes []rerequestCommitNodeJSON) error {
	if state.Author == "" {
		return nil
	}
	for _, node := range nodes {
		authorMatch := node.Commit.Author != nil && node.Commit.Author.User != nil &&
			strings.EqualFold(node.Commit.Author.User.Login, state.Author)
		committerMatch := node.Commit.Committer != nil && node.Commit.Committer.User != nil &&
			strings.EqualFold(node.Commit.Committer.User.Login, state.Author)
		if !authorMatch && !committerMatch {
			continue
		}
		rawTime := node.Commit.PushedDate
		if rawTime == "" {
			rawTime = node.Commit.CommittedDate
		}
		if rawTime == "" {
			continue
		}
		ts, err := time.Parse(time.RFC3339, rawTime)
		if err != nil {
			return fmt.Errorf("parse commit timestamp %q: %w", rawTime, err)
		}
		state.AuthorPushTimes = append(state.AuthorPushTimes, ts.UTC())
	}
	return nil
}

func appendRerequestForcePushes(state *RerequestPRState, nodes []rerequestForcePushNodeJSON) error {
	if state.Author == "" {
		return nil
	}
	for _, node := range nodes {
		if node.Actor == nil || !strings.EqualFold(node.Actor.Login, state.Author) || node.CreatedAt == "" {
			continue
		}
		ts, err := time.Parse(time.RFC3339, node.CreatedAt)
		if err != nil {
			return fmt.Errorf("parse force-push createdAt %q: %w", node.CreatedAt, err)
		}
		state.AuthorPushTimes = append(state.AuthorPushTimes, ts.UTC())
	}
	return nil
}

func (c *Client) paginateRerequestReviewRequests(ref PullRef, after string, state *RerequestPRState) error {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	for after != "" {
		fields := []string{
			"-f", "owner=" + ref.Owner,
			"-f", "name=" + ref.Repo,
			"-F", "number=" + strconv.Itoa(ref.Number),
			"-f", "after=" + after,
		}
		body, callErr := c.graphqlQuery(rerequestReviewRequestsPageQuery, fields)
		if err := graphqlCallError("read review requests of "+name, body, callErr); err != nil {
			return err
		}
		var resp struct {
			Data struct {
				Repository *struct {
					PullRequest *struct {
						ReviewRequests struct {
							PageInfo cursorPageInfoJSON          `json:"pageInfo"`
							Nodes    []rerequestReviewerNodeJSON `json:"nodes"`
						} `json:"reviewRequests"`
					} `json:"pullRequest"`
				} `json:"repository"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return fmt.Errorf("read review requests of %s: %w", name, err)
		}
		if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
			return fmt.Errorf("read review requests of %s: not found", name)
		}
		page := resp.Data.Repository.PullRequest.ReviewRequests
		appendRequestedReviewers(state, page.Nodes)
		if !page.PageInfo.HasNextPage {
			return nil
		}
		after = page.PageInfo.EndCursor
	}
	return nil
}

func (c *Client) paginateRerequestReviews(ref PullRef, after string, state *RerequestPRState) error {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	for after != "" {
		fields := []string{
			"-f", "owner=" + ref.Owner,
			"-f", "name=" + ref.Repo,
			"-F", "number=" + strconv.Itoa(ref.Number),
			"-f", "after=" + after,
		}
		body, callErr := c.graphqlQuery(rerequestReviewsPageQuery, fields)
		if err := graphqlCallError("read reviews of "+name, body, callErr); err != nil {
			return err
		}
		var resp struct {
			Data struct {
				Repository *struct {
					PullRequest *struct {
						Reviews struct {
							PageInfo cursorPageInfoJSON        `json:"pageInfo"`
							Nodes    []rerequestReviewNodeJSON `json:"nodes"`
						} `json:"reviews"`
					} `json:"pullRequest"`
				} `json:"repository"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return fmt.Errorf("read reviews of %s: %w", name, err)
		}
		if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
			return fmt.Errorf("read reviews of %s: not found", name)
		}
		page := resp.Data.Repository.PullRequest.Reviews
		if err := appendRerequestReviews(state, page.Nodes); err != nil {
			return fmt.Errorf("read reviews of %s: %w", name, err)
		}
		if !page.PageInfo.HasNextPage {
			return nil
		}
		after = page.PageInfo.EndCursor
	}
	return nil
}

func (c *Client) paginateRerequestThreads(ref PullRef, after string, state *RerequestPRState) error {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	for after != "" {
		fields := []string{
			"-f", "owner=" + ref.Owner,
			"-f", "name=" + ref.Repo,
			"-F", "number=" + strconv.Itoa(ref.Number),
			"-f", "after=" + after,
		}
		body, callErr := c.graphqlQuery(rerequestThreadsPageQuery, fields)
		if err := graphqlCallError("read review threads of "+name, body, callErr); err != nil {
			return err
		}
		var resp struct {
			Data struct {
				Repository *struct {
					PullRequest *struct {
						ReviewThreads struct {
							PageInfo cursorPageInfoJSON        `json:"pageInfo"`
							Nodes    []rerequestThreadNodeJSON `json:"nodes"`
						} `json:"reviewThreads"`
					} `json:"pullRequest"`
				} `json:"repository"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return fmt.Errorf("read review threads of %s: %w", name, err)
		}
		if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
			return fmt.Errorf("read review threads of %s: not found", name)
		}
		page := resp.Data.Repository.PullRequest.ReviewThreads
		if err := c.appendRerequestThreads(name, page.Nodes, state); err != nil {
			return err
		}
		if !page.PageInfo.HasNextPage {
			return nil
		}
		after = page.PageInfo.EndCursor
	}
	return nil
}

func (c *Client) paginateRerequestThreadComments(prName, threadID, after string) ([]RerequestThreadComment, error) {
	var all []RerequestThreadComment
	for after != "" {
		fields := []string{
			"-f", "id=" + threadID,
			"-f", "after=" + after,
		}
		body, callErr := c.graphqlQuery(rerequestThreadCommentsPageQuery, fields)
		if err := graphqlCallError("read thread comments of "+prName, body, callErr); err != nil {
			return nil, err
		}
		var resp struct {
			Data struct {
				Node *struct {
					Comments struct {
						PageInfo cursorPageInfoJSON         `json:"pageInfo"`
						Nodes    []rerequestCommentNodeJSON `json:"nodes"`
					} `json:"comments"`
				} `json:"node"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return nil, fmt.Errorf("read thread comments of %s: %w", prName, err)
		}
		if resp.Data.Node == nil {
			return nil, fmt.Errorf("read thread comments of %s: thread %s not found", prName, threadID)
		}
		more, err := convertRerequestComments(resp.Data.Node.Comments.Nodes)
		if err != nil {
			return nil, fmt.Errorf("read thread comments of %s: %w", prName, err)
		}
		all = append(all, more...)
		if !resp.Data.Node.Comments.PageInfo.HasNextPage {
			return all, nil
		}
		after = resp.Data.Node.Comments.PageInfo.EndCursor
	}
	return all, nil
}

func (c *Client) paginateRerequestCommits(ref PullRef, after string, state *RerequestPRState) error {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	for after != "" {
		fields := []string{
			"-f", "owner=" + ref.Owner,
			"-f", "name=" + ref.Repo,
			"-F", "number=" + strconv.Itoa(ref.Number),
			"-f", "after=" + after,
		}
		body, callErr := c.graphqlQuery(rerequestCommitsPageQuery, fields)
		if err := graphqlCallError("read commits of "+name, body, callErr); err != nil {
			return err
		}
		var resp struct {
			Data struct {
				Repository *struct {
					PullRequest *struct {
						Commits struct {
							PageInfo cursorPageInfoJSON        `json:"pageInfo"`
							Nodes    []rerequestCommitNodeJSON `json:"nodes"`
						} `json:"commits"`
					} `json:"pullRequest"`
				} `json:"repository"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return fmt.Errorf("read commits of %s: %w", name, err)
		}
		if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
			return fmt.Errorf("read commits of %s: not found", name)
		}
		page := resp.Data.Repository.PullRequest.Commits
		if err := appendRerequestCommits(state, page.Nodes); err != nil {
			return fmt.Errorf("read commits of %s: %w", name, err)
		}
		if !page.PageInfo.HasNextPage {
			return nil
		}
		after = page.PageInfo.EndCursor
	}
	return nil
}

func (c *Client) paginateRerequestForcePushes(ref PullRef, after string, state *RerequestPRState) error {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	for after != "" {
		fields := []string{
			"-f", "owner=" + ref.Owner,
			"-f", "name=" + ref.Repo,
			"-F", "number=" + strconv.Itoa(ref.Number),
			"-f", "after=" + after,
		}
		body, callErr := c.graphqlQuery(rerequestForcePushesPageQuery, fields)
		if err := graphqlCallError("read force pushes of "+name, body, callErr); err != nil {
			return err
		}
		var resp struct {
			Data struct {
				Repository *struct {
					PullRequest *struct {
						TimelineItems struct {
							PageInfo cursorPageInfoJSON           `json:"pageInfo"`
							Nodes    []rerequestForcePushNodeJSON `json:"nodes"`
						} `json:"timelineItems"`
					} `json:"pullRequest"`
				} `json:"repository"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return fmt.Errorf("read force pushes of %s: %w", name, err)
		}
		if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
			return fmt.Errorf("read force pushes of %s: not found", name)
		}
		page := resp.Data.Repository.PullRequest.TimelineItems
		if err := appendRerequestForcePushes(state, page.Nodes); err != nil {
			return fmt.Errorf("read force pushes of %s: %w", name, err)
		}
		if !page.PageInfo.HasNextPage {
			return nil
		}
		after = page.PageInfo.EndCursor
	}
	return nil
}
