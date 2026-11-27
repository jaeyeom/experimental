package gh

import (
	"context"
	"encoding/json"
	"fmt"
	"strconv"
	"time"
)

const (
	botRereviewQuery = `query($owner:String!,$repo:String!,$num:Int!){repository(owner:$owner,name:$repo){pullRequest(number:$num){` +
		`commits(last:1){nodes{commit{oid committedDate pushedDate}}}` +
		`reviews(first:100){pageInfo{hasNextPage endCursor}nodes{author{login} state submittedAt}}` +
		`reviewThreads(first:100){pageInfo{hasNextPage endCursor}nodes{id isResolved comments(first:1){nodes{author{login}}}}}` +
		`comments(first:100){pageInfo{hasNextPage endCursor}nodes{body createdAt}}` +
		`}}}`

	botRereviewReviewsPageQuery  = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String!){repository(owner:$owner,name:$repo){pullRequest(number:$num){reviews(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{author{login} state submittedAt}}}}}`
	botRereviewThreadsPageQuery  = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String!){repository(owner:$owner,name:$repo){pullRequest(number:$num){reviewThreads(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{id isResolved comments(first:1){nodes{author{login}}}}}}}}`
	botRereviewCommentsPageQuery = `query($owner:String!,$repo:String!,$num:Int!,$cursor:String!){repository(owner:$owner,name:$repo){pullRequest(number:$num){comments(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{body createdAt}}}}}`
)

// BotRereviewSnapshot loads the PR's last commit timestamp, all reviews, all
// review threads (with each thread's starter author), and all issue comments.
func (c *Client) BotRereviewSnapshot(ctx context.Context, owner, repo string, number int) (BotRereviewSnapshot, error) {
	result, err := c.requireJSON(ctx, defaultCallTimeout,
		"api", "graphql",
		"-f", "query="+botRereviewQuery,
		"-F", "owner="+owner,
		"-F", "repo="+repo,
		"-F", "num="+strconv.Itoa(number),
	)
	if err != nil {
		return BotRereviewSnapshot{}, err
	}
	pr, err := decodeBotRereviewPR(result.Output, owner, repo, number)
	if err != nil {
		return BotRereviewSnapshot{}, err
	}
	headSHA, lastCommitAt := extractLastCommit(pr.Commits.Nodes)
	out := BotRereviewSnapshot{
		HeadSHA:      headSHA,
		LastCommitAt: lastCommitAt,
		Reviews:      toReviews(pr.Reviews.Nodes),
		Threads:      toThreadSummaries(pr.ReviewThreads.Nodes),
		Comments:     toIssueComments(pr.Comments.Nodes),
	}
	if err := c.paginateBotRereviewReviews(ctx, owner, repo, number, pr.Reviews.PageInfo, &out.Reviews); err != nil {
		return BotRereviewSnapshot{}, err
	}
	if err := c.paginateBotRereviewThreads(ctx, owner, repo, number, pr.ReviewThreads.PageInfo, &out.Threads); err != nil {
		return BotRereviewSnapshot{}, err
	}
	if err := c.paginateBotRereviewComments(ctx, owner, repo, number, pr.Comments.PageInfo, &out.Comments); err != nil {
		return BotRereviewSnapshot{}, err
	}
	return out, nil
}

func (c *Client) paginateBotRereviewReviews(ctx context.Context, owner, repo string, number int, info gqlPageInfo, dst *[]Review) error {
	for page := 1; info.HasNextPage; page++ {
		if page >= maxReviewThreadPages {
			return fmt.Errorf("bot-rereview reviews page cap exceeded for %s/%s#%d", owner, repo, number)
		}
		pr, err := c.botRereviewPage(ctx, botRereviewReviewsPageQuery, owner, repo, number, info.EndCursor)
		if err != nil {
			return err
		}
		*dst = append(*dst, toReviews(pr.Reviews.Nodes)...)
		info = pr.Reviews.PageInfo
	}
	return nil
}

func (c *Client) paginateBotRereviewThreads(ctx context.Context, owner, repo string, number int, info gqlPageInfo, dst *[]ReviewThreadSummary) error {
	for page := 1; info.HasNextPage; page++ {
		if page >= maxReviewThreadPages {
			return fmt.Errorf("bot-rereview reviewThreads page cap exceeded for %s/%s#%d", owner, repo, number)
		}
		pr, err := c.botRereviewPage(ctx, botRereviewThreadsPageQuery, owner, repo, number, info.EndCursor)
		if err != nil {
			return err
		}
		*dst = append(*dst, toThreadSummaries(pr.ReviewThreads.Nodes)...)
		info = pr.ReviewThreads.PageInfo
	}
	return nil
}

func (c *Client) paginateBotRereviewComments(ctx context.Context, owner, repo string, number int, info gqlPageInfo, dst *[]IssueComment) error {
	for page := 1; info.HasNextPage; page++ {
		if page >= maxReviewThreadPages {
			return fmt.Errorf("bot-rereview comments page cap exceeded for %s/%s#%d", owner, repo, number)
		}
		pr, err := c.botRereviewPage(ctx, botRereviewCommentsPageQuery, owner, repo, number, info.EndCursor)
		if err != nil {
			return err
		}
		*dst = append(*dst, toIssueComments(pr.Comments.Nodes)...)
		info = pr.Comments.PageInfo
	}
	return nil
}

func (c *Client) botRereviewPage(ctx context.Context, query, owner, repo string, number int, cursor string) (*gqlBotRereviewPR, error) {
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
	return decodeBotRereviewPR(result.Output, owner, repo, number)
}

func decodeBotRereviewPR(raw, owner, repo string, number int) (*gqlBotRereviewPR, error) {
	data, err := decodeGraphQLData(raw, "bot-rereview snapshot")
	if err != nil {
		return nil, err
	}
	var payload struct {
		Repository *struct {
			PullRequest *gqlBotRereviewPR `json:"pullRequest"`
		} `json:"repository"`
	}
	if err := json.Unmarshal(data, &payload); err != nil {
		return nil, fmt.Errorf("decode bot-rereview snapshot: %w", err)
	}
	if payload.Repository == nil {
		return nil, fmt.Errorf("%w: %s/%s", ErrInaccessible, owner, repo)
	}
	if payload.Repository.PullRequest == nil {
		return nil, fmt.Errorf("%w: %s/%s#%d", ErrNotFound, owner, repo, number)
	}
	return payload.Repository.PullRequest, nil
}

func extractLastCommit(nodes []gqlBotRereviewCommitNode) (string, time.Time) {
	if len(nodes) == 0 {
		return "", time.Time{}
	}
	c := nodes[len(nodes)-1].Commit
	var t time.Time
	if c.PushedDate != nil && !c.PushedDate.IsZero() {
		t = *c.PushedDate
	} else if c.CommittedDate != nil {
		t = *c.CommittedDate
	}
	return c.OID, t
}

func toReviews(nodes []gqlBotRereviewReviewNode) []Review {
	out := make([]Review, 0, len(nodes))
	for _, n := range nodes {
		author := ""
		if n.Author != nil {
			author = n.Author.Login
		}
		var submitted time.Time
		if n.SubmittedAt != nil {
			submitted = *n.SubmittedAt
		}
		out = append(out, Review{
			Author:      author,
			State:       n.State,
			SubmittedAt: submitted,
		})
	}
	return out
}

func toThreadSummaries(nodes []gqlBotRereviewThreadNode) []ReviewThreadSummary {
	out := make([]ReviewThreadSummary, 0, len(nodes))
	for _, n := range nodes {
		starter := ""
		if len(n.Comments.Nodes) > 0 && n.Comments.Nodes[0].Author != nil {
			starter = n.Comments.Nodes[0].Author.Login
		}
		out = append(out, ReviewThreadSummary{
			ID:            n.ID,
			IsResolved:    n.IsResolved,
			StarterAuthor: starter,
		})
	}
	return out
}

func toIssueComments(nodes []gqlBotRereviewCommentNode) []IssueComment {
	out := make([]IssueComment, 0, len(nodes))
	for _, n := range nodes {
		out = append(out, IssueComment(n))
	}
	return out
}

type gqlBotRereviewPR struct {
	Commits struct {
		Nodes []gqlBotRereviewCommitNode `json:"nodes"`
	} `json:"commits"`
	Reviews struct {
		PageInfo gqlPageInfo                `json:"pageInfo"`
		Nodes    []gqlBotRereviewReviewNode `json:"nodes"`
	} `json:"reviews"`
	ReviewThreads struct {
		PageInfo gqlPageInfo                `json:"pageInfo"`
		Nodes    []gqlBotRereviewThreadNode `json:"nodes"`
	} `json:"reviewThreads"`
	Comments struct {
		PageInfo gqlPageInfo                 `json:"pageInfo"`
		Nodes    []gqlBotRereviewCommentNode `json:"nodes"`
	} `json:"comments"`
}

type gqlBotRereviewCommitNode struct {
	Commit struct {
		OID           string     `json:"oid"`
		CommittedDate *time.Time `json:"committedDate"`
		PushedDate    *time.Time `json:"pushedDate"`
	} `json:"commit"`
}

type gqlBotRereviewReviewNode struct {
	Author *struct {
		Login string `json:"login"`
	} `json:"author"`
	State       string     `json:"state"`
	SubmittedAt *time.Time `json:"submittedAt"`
}

type gqlBotRereviewThreadNode struct {
	ID         string `json:"id"`
	IsResolved bool   `json:"isResolved"`
	Comments   struct {
		Nodes []struct {
			Author *struct {
				Login string `json:"login"`
			} `json:"author"`
		} `json:"nodes"`
	} `json:"comments"`
}

type gqlBotRereviewCommentNode struct {
	Body      string    `json:"body"`
	CreatedAt time.Time `json:"createdAt"`
}
