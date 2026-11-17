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

// DecideCLI is the Decider that runs the decide command.
type DecideCLI struct{ Run Runner }

// Ask files a relay item with the post, edit and cancel options and free
// text for edit notes.
func (d DecideCLI) Ask(ctx context.Context, owner, notifyPane, question string) (string, error) {
	args := []string{"ask", "--owner", owner}
	if notifyPane != "" {
		args = append(args, "--notify-pane", notifyPane)
	}
	args = append(args, "--kind", "relay",
		"--option", OptionPost+":Post exactly this",
		"--option", OptionEdit+":Edit (write notes for a redraft)",
		"--option", OptionCancel+":Cancel",
		"--free-text", "--json", question)
	var it DecideItem
	if err := runJSON(ctx, d.Run, nil, &it, args...); err != nil {
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

// GHCLI is the GitHub that runs gh api as the user's account.
type GHCLI struct{ Run Runner }

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
