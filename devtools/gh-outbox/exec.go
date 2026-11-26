package ghoutbox

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"slices"
	"strconv"
	"strings"
	"time"
)

// Runner runs a command with stdin and returns its stdout.
type Runner func(ctx context.Context, stdin []byte, args ...string) ([]byte, error)

// commandTimeout bounds one external command.
const commandTimeout = 2 * time.Minute

// Exec returns a Runner for argv, such as Config.GHCommand, followed by
// the call's args. stderr is part of the error.
func Exec(argv []string) Runner {
	return func(ctx context.Context, stdin []byte, args ...string) ([]byte, error) {
		ctx, cancel := context.WithTimeout(ctx, commandTimeout)
		defer cancel()
		// argv comes from the user's own config.
		// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
		cmd := exec.CommandContext(ctx, argv[0], append(slices.Clip(argv[1:]), args...)...) //nolint:gosec // G204: user config.
		if stdin != nil {
			cmd.Stdin = bytes.NewReader(stdin)
		}
		var stdout, stderr bytes.Buffer
		cmd.Stdout, cmd.Stderr = &stdout, &stderr
		cmd.WaitDelay = time.Second
		if err := cmd.Run(); err != nil {
			return nil, fmt.Errorf("%s %s: %w: %s", argv[0], firstArg(args), err, strings.TrimSpace(stderr.String()))
		}
		return stdout.Bytes(), nil
	}
}

func firstArg(args []string) string {
	if len(args) == 0 {
		return ""
	}
	return args[0]
}

func runJSON(ctx context.Context, run Runner, stdin []byte, v any, args ...string) error {
	out, err := run(ctx, stdin, args...)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(out, v); err != nil {
		return fmt.Errorf("%s: parse output: %w", firstArg(args), err)
	}
	return nil
}

// longLinesFlag lets decide ask take the question gh-outbox builds,
// with its quoted draft, past decide's line limit for agents. An older
// decide without the flag gets the same ask again without it.
const longLinesFlag = "--long-lines"

// DecideCLI is the Decider that runs the decide command.
type DecideCLI struct{ Run Runner }

// Ask files a relay item with the post, edit and cancel options and free
// text for edit notes.
func (d DecideCLI) Ask(ctx context.Context, owner, notifyPane, question string) (string, error) {
	args := []string{"ask", longLinesFlag, "--owner", owner}
	if notifyPane != "" {
		args = append(args, "--notify-pane", notifyPane)
	}
	args = append(args, "--kind", "relay",
		"--option", OptionPost+":Post exactly this",
		"--option", OptionEdit+":Edit (write notes for a redraft)",
		"--option", OptionCancel+":Cancel",
		"--free-text", "--json", question)
	var it DecideItem
	err := runJSON(ctx, d.Run, nil, &it, args...)
	if err != nil && strings.Contains(err.Error(), "flag provided but not defined: -long-lines") {
		args = slices.DeleteFunc(args, func(a string) bool { return a == longLinesFlag })
		err = runJSON(ctx, d.Run, nil, &it, args...)
	}
	if err != nil {
		return "", err
	}
	if it.ID == "" {
		return "", errors.New("decide ask printed no id")
	}
	return it.ID, nil
}

// Show reads an item.
func (d DecideCLI) Show(ctx context.Context, id string) (DecideItem, error) {
	var it DecideItem
	err := runJSON(ctx, d.Run, nil, &it, "show", "--json", id)
	return it, err
}

// Done closes an item with outcome.
func (d DecideCLI) Done(ctx context.Context, id, outcome string) error {
	var it DecideItem
	return runJSON(ctx, d.Run, nil, &it, "done", "--json", id, outcome)
}

// AgentbusCLI is the Bus that runs the agentbus command.
type AgentbusCLI struct{ Run Runner }

// Send sends text to the agent named to. The text goes on stdin.
func (a AgentbusCLI) Send(ctx context.Context, to, text string) error {
	_, err := a.Run(ctx, []byte(text), "send", to)
	return err
}

// Names lists the registered agentbus names.
func (a AgentbusCLI) Names(ctx context.Context) ([]string, error) {
	var entries []struct {
		Name string `json:"name"`
	}
	if err := runJSON(ctx, a.Run, nil, &entries, "list", "--json"); err != nil {
		return nil, err
	}
	names := make([]string, 0, len(entries))
	for _, e := range entries {
		names = append(names, e.Name)
	}
	return names, nil
}

// ErrReplyNotFound is returned when a review comment or thread is not on the PR.
var ErrReplyNotFound = errors.New("review comment or thread not found on pull request")

// GHCLI is the GitHub that runs gh api as the user's account.
type GHCLI struct{ Run Runner }

const (
	maxGraphQLPages  = 50
	replyTargetQuery = `query($owner:String!,$repo:String!,$pr:Int!,$cursor:String){repository(owner:$owner,name:$repo){pullRequest(number:$pr){reviewThreads(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{id isResolved path line originalLine comments(first:100){pageInfo{hasNextPage endCursor}nodes{databaseId author{login} body}}}}}}}`
	threadMoreQuery  = `query($id:ID!,$cursor:String){node(id:$id){... on PullRequestReviewThread{comments(first:100,after:$cursor){pageInfo{hasNextPage endCursor}nodes{databaseId author{login} body}}}}}`
)

type graphqlComment struct {
	DatabaseID int64 `json:"databaseId"`
	Author     *struct {
		Login string `json:"login"`
	} `json:"author"`
	Body string `json:"body"`
}

type graphqlCommentConn struct {
	PageInfo struct {
		HasNextPage bool   `json:"hasNextPage"`
		EndCursor   string `json:"endCursor"`
	} `json:"pageInfo"`
	Nodes []graphqlComment `json:"nodes"`
}

type graphqlThread struct {
	ID           string             `json:"id"`
	IsResolved   bool               `json:"isResolved"`
	Path         string             `json:"path"`
	Line         *int               `json:"line"`
	OriginalLine *int               `json:"originalLine"`
	Comments     graphqlCommentConn `json:"comments"`
}

type ghComment struct {
	Body     string `json:"body,omitempty"`
	CommitID string `json:"commit_id,omitempty"` //nolint:tagliatelle // GitHub API field
	Path     string `json:"path,omitempty"`
	Line     int    `json:"line,omitempty"`
	Side     string `json:"side,omitempty"`
	Event    string `json:"event,omitempty"`
	// Comments is the line comments of a review.
	Comments []ghComment `json:"comments,omitempty"`
}

func pullPath(repo string, pr int) string {
	return "repos/" + repo + "/pulls/" + strconv.Itoa(pr)
}

func (g GHCLI) get(ctx context.Context, v any, path string) error {
	return runJSON(ctx, g.Run, nil, v, "api", path)
}

// write sends payload as JSON with method to path and returns html_url.
func (g GHCLI) write(ctx context.Context, method, path string, payload ghComment) (string, error) {
	in, err := json.Marshal(payload)
	if err != nil {
		return "", fmt.Errorf("encode %s: %w", path, err)
	}
	var out struct {
		HTMLURL string `json:"html_url"` //nolint:tagliatelle // GitHub API field
	}
	if err := runJSON(ctx, g.Run, in, &out, "api", "--method", method, path, "--input", "-"); err != nil {
		return "", err
	}
	return out.HTMLURL, nil
}

// HeadSHA is the pull request's head commit.
func (g GHCLI) HeadSHA(ctx context.Context, repo string, pr int) (string, error) {
	var out struct {
		Head struct {
			SHA string `json:"sha"`
		} `json:"head"`
	}
	if err := g.get(ctx, &out, pullPath(repo, pr)); err != nil {
		return "", err
	}
	if out.Head.SHA == "" {
		return "", errors.New("gh api printed no head sha")
	}
	return out.Head.SHA, nil
}

// CommentBody is the text of a review comment.
func (g GHCLI) CommentBody(ctx context.Context, repo, id string) (string, error) {
	var out ghComment
	err := g.get(ctx, &out, "repos/"+repo+"/pulls/comments/"+id)
	return out.Body, err
}

// ReviewBody is the body of a review.
func (g GHCLI) ReviewBody(ctx context.Context, repo string, pr int, id string) (string, error) {
	var out ghComment
	err := g.get(ctx, &out, pullPath(repo, pr)+"/reviews/"+id)
	return out.Body, err
}

// CreateThread starts a discussion thread on c's line of the head diff.
func (g GHCLI) CreateThread(ctx context.Context, repo string, pr int, sha string, c LineComment) (string, error) {
	return g.write(ctx, "POST", pullPath(repo, pr)+"/comments", ghComment{
		Body: c.Text, CommitID: sha, Path: c.Path, Line: c.Line, Side: "RIGHT",
	})
}

// CreateReview submits a review with event and its line comments.
func (g GHCLI) CreateReview(ctx context.Context, repo string, pr int, sha, event, body string, cs []LineComment) (string, error) {
	payload := ghComment{Body: body, CommitID: sha, Event: event}
	for _, c := range cs {
		payload.Comments = append(payload.Comments, ghComment{Body: c.Text, Path: c.Path, Line: c.Line, Side: "RIGHT"})
	}
	return g.write(ctx, "POST", pullPath(repo, pr)+"/reviews", payload)
}

// EditComment replaces a review comment's text.
func (g GHCLI) EditComment(ctx context.Context, repo, id, body string) (string, error) {
	return g.write(ctx, "PATCH", "repos/"+repo+"/pulls/comments/"+id, ghComment{Body: body})
}

// EditReview replaces a review's body.
func (g GHCLI) EditReview(ctx context.Context, repo string, pr int, id, body string) (string, error) {
	return g.write(ctx, "PUT", pullPath(repo, pr)+"/reviews/"+id, ghComment{Body: body})
}

// CreateReply posts a reply inside an existing review comment thread.
func (g GHCLI) CreateReply(ctx context.Context, repo string, pr int, commentID, body string) (string, error) {
	url, err := g.write(ctx, "POST", pullPath(repo, pr)+"/comments/"+commentID+"/replies", ghComment{Body: body})
	if err != nil && strings.Contains(err.Error(), "HTTP 404") {
		return "", fmt.Errorf("%w: %w", ErrReplyNotFound, err)
	}
	return url, err
}

// ReplyTarget finds commentID in the pull request's review threads and
// returns its thread metadata and the text of the comment being answered.
func (g GHCLI) ReplyTarget(ctx context.Context, repo string, pr int, commentID string) (ReplyTarget, error) {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" {
		return ReplyTarget{}, fmt.Errorf("invalid repo %q", repo)
	}
	targetID, err := strconv.ParseInt(commentID, 10, 64)
	if err != nil || targetID <= 0 {
		return ReplyTarget{}, fmt.Errorf("invalid comment id %q", commentID)
	}
	cursor := ""
	for range maxGraphQLPages {
		threads, hasNext, nextCursor, err := g.replyThreadsPage(ctx, owner, name, pr, cursor)
		if err != nil {
			return ReplyTarget{}, err
		}
		for _, th := range threads {
			if target, found, err := g.matchThreadComment(ctx, th, targetID); err != nil {
				return ReplyTarget{}, err
			} else if found {
				return target, nil
			}
		}
		if !hasNext || nextCursor == "" {
			return ReplyTarget{}, fmt.Errorf("comment %s on %s#%d: %w", commentID, repo, pr, ErrReplyNotFound)
		}
		cursor = nextCursor
	}
	return ReplyTarget{}, fmt.Errorf("review threads page cap exceeded for %s#%d", repo, pr)
}

func (g GHCLI) replyThreadsPage(ctx context.Context, owner, name string, pr int, cursor string) ([]graphqlThread, bool, string, error) {
	args := []string{
		"api", "graphql",
		"-f", "query=" + replyTargetQuery,
		"-F", "owner=" + owner,
		"-F", "repo=" + name,
		"-F", "pr=" + strconv.Itoa(pr),
	}
	if cursor != "" {
		args = append(args, "-F", "cursor="+cursor)
	}
	var resp struct {
		Data struct {
			Repository *struct {
				PullRequest *struct {
					ReviewThreads struct {
						PageInfo struct {
							HasNextPage bool   `json:"hasNextPage"`
							EndCursor   string `json:"endCursor"`
						} `json:"pageInfo"`
						Nodes []graphqlThread `json:"nodes"`
					} `json:"reviewThreads"`
				} `json:"pullRequest"`
			} `json:"repository"`
		} `json:"data"`
		Errors []struct {
			Message string `json:"message"`
		} `json:"errors"`
	}
	if err := runJSON(ctx, g.Run, nil, &resp, args...); err != nil {
		return nil, false, "", err
	}
	if len(resp.Errors) > 0 {
		return nil, false, "", fmt.Errorf("gh api graphql: %s", resp.Errors[0].Message)
	}
	if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
		return nil, false, "", fmt.Errorf("pull request %s/%s#%d not found", owner, name, pr)
	}
	rt := resp.Data.Repository.PullRequest.ReviewThreads
	return rt.Nodes, rt.PageInfo.HasNextPage, rt.PageInfo.EndCursor, nil
}

func (g GHCLI) matchThreadComment(ctx context.Context, th graphqlThread, targetID int64) (ReplyTarget, bool, error) {
	if len(th.Comments.Nodes) == 0 {
		return ReplyTarget{}, false, nil
	}
	first := th.Comments.Nodes[0]
	conn := th.Comments
	for page := 0; page < maxGraphQLPages; page++ {
		for _, c := range conn.Nodes {
			if c.DatabaseID == targetID {
				return buildReplyTarget(th, first, c), true, nil
			}
		}
		if !conn.PageInfo.HasNextPage || conn.PageInfo.EndCursor == "" {
			return ReplyTarget{}, false, nil
		}
		next, err := g.threadCommentsPage(ctx, th.ID, conn.PageInfo.EndCursor)
		if err != nil {
			return ReplyTarget{}, false, err
		}
		conn = next
	}
	return ReplyTarget{}, false, fmt.Errorf("thread %s comment page cap exceeded", th.ID)
}

func (g GHCLI) threadCommentsPage(ctx context.Context, threadID, cursor string) (graphqlCommentConn, error) {
	var resp struct {
		Data struct {
			Node *struct {
				Comments graphqlCommentConn `json:"comments"`
			} `json:"node"`
		} `json:"data"`
		Errors []struct {
			Message string `json:"message"`
		} `json:"errors"`
	}
	if err := runJSON(ctx, g.Run, nil, &resp, "api", "graphql", "-f", "query="+threadMoreQuery, "-F", "id="+threadID, "-F", "cursor="+cursor); err != nil {
		return graphqlCommentConn{}, err
	}
	if len(resp.Errors) > 0 {
		return graphqlCommentConn{}, fmt.Errorf("gh api graphql: %s", resp.Errors[0].Message)
	}
	if resp.Data.Node == nil {
		return graphqlCommentConn{}, ErrReplyNotFound
	}
	return resp.Data.Node.Comments, nil
}

func buildReplyTarget(th graphqlThread, first, target graphqlComment) ReplyTarget {
	line := 0
	if th.Line != nil && *th.Line > 0 {
		line = *th.Line
	} else if th.OriginalLine != nil && *th.OriginalLine > 0 {
		line = *th.OriginalLine
	}
	starter := "unknown"
	if first.Author != nil && first.Author.Login != "" {
		starter = first.Author.Login
	}
	return ReplyTarget{
		RootID:      strconv.FormatInt(first.DatabaseID, 10),
		Path:        th.Path,
		Line:        line,
		Starter:     starter,
		CommentBody: target.Body,
		Resolved:    th.IsResolved,
	}
}
