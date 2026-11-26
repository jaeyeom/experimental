package gh

import (
	"context"
	"encoding/json"
	"fmt"
	"net/url"
	"strconv"
	"strings"
	"time"
)

const (
	waiveTimelineQuery = `query($owner:String!,$repo:String!,$num:Int!){repository(owner:$owner,name:$repo){pullRequest(number:$num){` +
		`comments(first:100){pageInfo{hasNextPage endCursor}nodes{id databaseId author{login} body createdAt url}}` +
		`commits(first:100){pageInfo{hasNextPage endCursor}nodes{commit{oid committedDate pushedDate parents(first:5){nodes{oid}}}}}` +
		`timelineItems(itemTypes:[HEAD_REF_FORCE_PUSHED_EVENT],first:100){pageInfo{hasNextPage endCursor}nodes{... on HeadRefForcePushedEvent{createdAt beforeCommit{oid} afterCommit{oid}}}}` +
		`}}}`

	waiveCommentsPageQuery    = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String!){repository(owner:$owner,name:$repo){pullRequest(number:$num){comments(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{id databaseId author{login} body createdAt url}}}}}`
	waiveCommitsPageQuery     = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String!){repository(owner:$owner,name:$repo){pullRequest(number:$num){commits(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{commit{oid committedDate pushedDate parents(first:5){nodes{oid}}}}}}}}`
	waiveForcePushesPageQuery = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String!){repository(owner:$owner,name:$repo){pullRequest(number:$num){timelineItems(itemTypes:[HEAD_REF_FORCE_PUSHED_EVENT],first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{... on HeadRefForcePushedEvent{createdAt beforeCommit{oid} afterCommit{oid}}}}}}}`
)

// WaiveTimeline loads all issue comments, PR commits, and force-push timeline
// events for owner/repo#number.
func (c *Client) WaiveTimeline(ctx context.Context, owner, repo string, number int) (WaiveTimeline, error) {
	result, err := c.requireJSON(ctx, defaultCallTimeout,
		"api", "graphql",
		"-f", "query="+waiveTimelineQuery,
		"-F", "owner="+owner,
		"-F", "repo="+repo,
		"-F", "num="+strconv.Itoa(number),
	)
	if err != nil {
		return WaiveTimeline{}, err
	}
	pr, err := decodeWaivePR(result.Output, owner, repo, number)
	if err != nil {
		return WaiveTimeline{}, err
	}
	out := WaiveTimeline{
		Comments:    toWaiveComments(pr.Comments.Nodes),
		Commits:     toPRCommits(pr.Commits.Nodes),
		ForcePushes: toForcePushes(pr.TimelineItems.Nodes),
	}
	if err := c.paginateWaiveComments(ctx, owner, repo, number, pr.Comments.PageInfo, &out.Comments); err != nil {
		return WaiveTimeline{}, err
	}
	if err := c.paginateWaiveCommits(ctx, owner, repo, number, pr.Commits.PageInfo, &out.Commits); err != nil {
		return WaiveTimeline{}, err
	}
	if err := c.paginateWaiveForcePushes(ctx, owner, repo, number, pr.TimelineItems.PageInfo, &out.ForcePushes); err != nil {
		return WaiveTimeline{}, err
	}
	return out, nil
}

func (c *Client) paginateWaiveComments(ctx context.Context, owner, repo string, number int, info gqlPageInfo, dst *[]WaiveComment) error {
	for page := 1; info.HasNextPage; page++ {
		if page >= maxReviewThreadPages {
			return fmt.Errorf("waive comments page cap exceeded for %s/%s#%d", owner, repo, number)
		}
		pr, err := c.waivePage(ctx, waiveCommentsPageQuery, owner, repo, number, info.EndCursor)
		if err != nil {
			return err
		}
		*dst = append(*dst, toWaiveComments(pr.Comments.Nodes)...)
		info = pr.Comments.PageInfo
	}
	return nil
}

func (c *Client) paginateWaiveCommits(ctx context.Context, owner, repo string, number int, info gqlPageInfo, dst *[]PRCommit) error {
	for page := 1; info.HasNextPage; page++ {
		if page >= maxReviewThreadPages {
			return fmt.Errorf("waive commits page cap exceeded for %s/%s#%d", owner, repo, number)
		}
		pr, err := c.waivePage(ctx, waiveCommitsPageQuery, owner, repo, number, info.EndCursor)
		if err != nil {
			return err
		}
		*dst = append(*dst, toPRCommits(pr.Commits.Nodes)...)
		info = pr.Commits.PageInfo
	}
	return nil
}

func (c *Client) paginateWaiveForcePushes(ctx context.Context, owner, repo string, number int, info gqlPageInfo, dst *[]ForcePushEvent) error {
	for page := 1; info.HasNextPage; page++ {
		if page >= maxReviewThreadPages {
			return fmt.Errorf("waive force-pushes page cap exceeded for %s/%s#%d", owner, repo, number)
		}
		pr, err := c.waivePage(ctx, waiveForcePushesPageQuery, owner, repo, number, info.EndCursor)
		if err != nil {
			return err
		}
		*dst = append(*dst, toForcePushes(pr.TimelineItems.Nodes)...)
		info = pr.TimelineItems.PageInfo
	}
	return nil
}

func (c *Client) waivePage(ctx context.Context, query, owner, repo string, number int, cursor string) (*gqlWaivePR, error) {
	result, err := c.requireJSON(ctx, defaultCallTimeout,
		"api", "graphql",
		"-f", "query="+query,
		"-F", "owner="+owner,
		"-F", "repo="+repo,
		"-F", "num="+strconv.Itoa(number),
		"-F", "cursor="+cursor,
	)
	if err != nil {
		return nil, err
	}
	return decodeWaivePR(result.Output, owner, repo, number)
}

// CompareCommits returns GitHub's comparison status ("ahead", "behind",
// "identical", "diverged") and merge-base commit SHA for base...head.
func (c *Client) CompareCommits(ctx context.Context, owner, repo, base, head string) (status, mergeBaseSHA string, err error) {
	endpoint := fmt.Sprintf("repos/%s/%s/compare/%s...%s",
		url.PathEscape(owner), url.PathEscape(repo), escapeGitRef(base), escapeGitRef(head))
	result, err := c.requireJSON(ctx, defaultCallTimeout, "api", endpoint)
	if err != nil {
		return "", "", err
	}
	var payload struct {
		Status          string `json:"status"`
		MergeBaseCommit struct {
			SHA string `json:"sha"`
		} `json:"merge_base_commit"` //nolint:tagliatelle // GitHub REST API wire format
	}
	if err := json.Unmarshal([]byte(result.Output), &payload); err != nil {
		return "", "", fmt.Errorf("decode compare commits: %w", err)
	}
	return payload.Status, payload.MergeBaseCommit.SHA, nil
}

// CompareDiff returns the raw unified diff bytes for base..head from GitHub.
func (c *Client) CompareDiff(ctx context.Context, owner, repo, base, head string) ([]byte, error) {
	endpoint := fmt.Sprintf("repos/%s/%s/compare/%s..%s",
		url.PathEscape(owner), url.PathEscape(repo), escapeGitRef(base), escapeGitRef(head))
	result, err := c.requireOK(ctx, defaultCallTimeout,
		"api", "-H", "Accept: application/vnd.github.v3.diff", endpoint)
	if err != nil {
		return nil, err
	}
	return []byte(result.Output), nil
}

func escapeGitRef(ref string) string {
	segments := strings.Split(ref, "/")
	for i, seg := range segments {
		segments[i] = url.PathEscape(seg)
	}
	return strings.Join(segments, "/")
}

func decodeWaivePR(raw, owner, repo string, number int) (*gqlWaivePR, error) {
	data, err := decodeGraphQLData(raw, "waive timeline")
	if err != nil {
		return nil, err
	}
	var payload struct {
		Repository *struct {
			PullRequest *gqlWaivePR `json:"pullRequest"`
		} `json:"repository"`
	}
	if err := json.Unmarshal(data, &payload); err != nil {
		return nil, fmt.Errorf("decode waive timeline: %w", err)
	}
	if payload.Repository == nil {
		return nil, fmt.Errorf("%w: %s/%s", ErrInaccessible, owner, repo)
	}
	if payload.Repository.PullRequest == nil {
		return nil, fmt.Errorf("%w: %s/%s#%d", ErrNotFound, owner, repo, number)
	}
	return payload.Repository.PullRequest, nil
}

func toWaiveComments(nodes []gqlWaiveComment) []WaiveComment {
	out := make([]WaiveComment, 0, len(nodes))
	for _, n := range nodes {
		id := n.ID
		if n.DatabaseID != nil && *n.DatabaseID > 0 {
			id = strconv.FormatInt(*n.DatabaseID, 10)
		}
		author := ""
		if n.Author != nil {
			author = n.Author.Login
		}
		out = append(out, WaiveComment{
			ID:        id,
			Author:    author,
			Body:      n.Body,
			URL:       n.URL,
			CreatedAt: n.CreatedAt,
		})
	}
	return out
}

func toPRCommits(nodes []gqlWaiveCommitNode) []PRCommit {
	out := make([]PRCommit, 0, len(nodes))
	for _, n := range nodes {
		var pushed time.Time
		if n.Commit.PushedDate != nil {
			pushed = *n.Commit.PushedDate
		}
		parents := make([]string, 0, len(n.Commit.Parents.Nodes))
		for _, p := range n.Commit.Parents.Nodes {
			if p.OID != "" {
				parents = append(parents, p.OID)
			}
		}
		out = append(out, PRCommit{
			OID:         n.Commit.OID,
			CommittedAt: n.Commit.CommittedDate,
			PushedAt:    pushed,
			Parents:     parents,
		})
	}
	return out
}

func toForcePushes(nodes []gqlForcePushNode) []ForcePushEvent {
	out := make([]ForcePushEvent, 0, len(nodes))
	for _, n := range nodes {
		if n.CreatedAt.IsZero() {
			continue
		}
		var before, after string
		if n.BeforeCommit != nil {
			before = n.BeforeCommit.OID
		}
		if n.AfterCommit != nil {
			after = n.AfterCommit.OID
		}
		out = append(out, ForcePushEvent{
			CreatedAt: n.CreatedAt,
			BeforeOID: before,
			AfterOID:  after,
		})
	}
	return out
}

type gqlPageInfo struct {
	HasNextPage bool   `json:"hasNextPage"`
	EndCursor   string `json:"endCursor"`
}

type gqlWaivePR struct {
	Comments struct {
		PageInfo gqlPageInfo       `json:"pageInfo"`
		Nodes    []gqlWaiveComment `json:"nodes"`
	} `json:"comments"`
	Commits struct {
		PageInfo gqlPageInfo          `json:"pageInfo"`
		Nodes    []gqlWaiveCommitNode `json:"nodes"`
	} `json:"commits"`
	TimelineItems struct {
		PageInfo gqlPageInfo        `json:"pageInfo"`
		Nodes    []gqlForcePushNode `json:"nodes"`
	} `json:"timelineItems"`
}

type gqlWaiveComment struct {
	ID         string `json:"id"`
	DatabaseID *int64 `json:"databaseId"`
	Author     *struct {
		Login string `json:"login"`
	} `json:"author"`
	Body      string    `json:"body"`
	CreatedAt time.Time `json:"createdAt"`
	URL       string    `json:"url"`
}

type gqlWaiveCommitNode struct {
	Commit struct {
		OID           string     `json:"oid"`
		CommittedDate time.Time  `json:"committedDate"`
		PushedDate    *time.Time `json:"pushedDate"`
		Parents       struct {
			Nodes []struct {
				OID string `json:"oid"`
			} `json:"nodes"`
		} `json:"parents"`
	} `json:"commit"`
}

type gqlForcePushNode struct {
	CreatedAt    time.Time `json:"createdAt"`
	BeforeCommit *struct {
		OID string `json:"oid"`
	} `json:"beforeCommit"`
	AfterCommit *struct {
		OID string `json:"oid"`
	} `json:"afterCommit"`
}
