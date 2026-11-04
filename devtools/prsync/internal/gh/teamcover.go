package gh

import (
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/url"
	"strings"
	"time"
)

const pathHistoryQuery = `query($owner:String!,$name:String!,$expression:String!,$path:String!,$since:GitTimestamp!){repository(owner:$owner,name:$name){object(expression:$expression){... on Commit{history(first:100,path:$path,since:$since){nodes{author{user{login}}associatedPullRequests(first:10){nodes{reviews(first:20){nodes{author{login}}}}}}}}}}}`

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

// Codeowners returns the base-branch CODEOWNERS file.
// GitHub uses the first of .github/CODEOWNERS, CODEOWNERS, and docs/CODEOWNERS.
// An empty string means the repository has none. A non-404 failure is returned.
func (c *Client) Codeowners(ctx context.Context, repo, ref string) (string, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return "", err
	}
	paths := []string{".github/CODEOWNERS", "CODEOWNERS", "docs/CODEOWNERS"}
	for _, path := range paths {
		body, ok, err := c.contents(ctx, owner, name, path, ref)
		if err != nil {
			return "", err
		}
		if ok {
			return body, nil
		}
	}
	return "", nil
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

// PathHistory returns authors and reviewers of up to 100 commits on path
// since the cutoff, on ref. Reviewers come from those commits' pull requests.
func (c *Client) PathHistory(ctx context.Context, repo, ref, path string, since time.Time) (PathHistory, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return PathHistory{}, err
	}
	result, err := c.requireJSON(ctx, defaultCallTimeout,
		"api", "graphql",
		"-f", "query="+pathHistoryQuery,
		"-F", "owner="+owner,
		"-F", "name="+name,
		"-F", "expression="+ref,
		"-F", "path="+path,
		"-F", "since="+since.UTC().Format(time.RFC3339),
	)
	if err != nil {
		if IsRateLimited(err) {
			return PathHistory{}, fmt.Errorf("%w: %s", ErrRateLimited, err.Error())
		}
		return PathHistory{}, err
	}
	history, err := parsePathHistory(result.Output)
	if err != nil {
		return PathHistory{}, err
	}
	return history, nil
}

// DirectReviewCount is the search total for user-review-requested:login
// scoped to org. It does not use review-requested:, which also counts team
// requests. A rate limit is ErrRateLimited.
func (c *Client) DirectReviewCount(ctx context.Context, org, login string) (int, error) {
	query := fmt.Sprintf("user-review-requested:%s org:%s is:pr is:open", login, org)
	result, err := c.call(ctx, "api", "search/issues", "-f", "q="+query, "-f", "per_page=1")
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

func parsePathHistory(raw string) (PathHistory, error) {
	var payload pathHistoryPayload
	if err := json.Unmarshal([]byte(raw), &payload); err != nil {
		return PathHistory{}, fmt.Errorf("decode path history: %w", err)
	}
	if len(payload.Errors) > 0 {
		return PathHistory{}, fmt.Errorf("path history graphql: %s", payload.Errors[0].Message)
	}
	var history PathHistory
	if payload.Data.Repository.Object == nil {
		return history, nil
	}
	for _, node := range payload.Data.Repository.Object.History.Nodes {
		if node.Author.User != nil && node.Author.User.Login != "" {
			history.Authors = append(history.Authors, node.Author.User.Login)
		}
		for _, pr := range node.AssociatedPullRequests.Nodes {
			for _, review := range pr.Reviews.Nodes {
				if review.Author != nil && review.Author.Login != "" {
					history.Reviewers = append(history.Reviewers, review.Author.Login)
				}
			}
		}
	}
	return history, nil
}

type pathHistoryPayload struct {
	Data struct {
		Repository struct {
			Object *struct {
				History struct {
					Nodes []struct {
						Author struct {
							User *struct {
								Login string `json:"login"`
							} `json:"user"`
						} `json:"author"`
						AssociatedPullRequests struct {
							Nodes []struct {
								Reviews struct {
									Nodes []struct {
										Author *struct {
											Login string `json:"login"`
										} `json:"author"`
									} `json:"nodes"`
								} `json:"reviews"`
							} `json:"nodes"`
						} `json:"associatedPullRequests"`
					} `json:"nodes"`
				} `json:"history"`
			} `json:"object"`
		} `json:"repository"`
	} `json:"data"`
	Errors []struct {
		Message string `json:"message"`
	} `json:"errors"`
}
