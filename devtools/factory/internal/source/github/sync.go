package github

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// Sync returns issues updated at or after since, oldest first.
// The zero time is a full sync: $since is omitted and every open and closed
// issue in the repository is returned.
func (s *Source) Sync(ctx context.Context, since time.Time) ([]source.Item, error) {
	if ctx == nil {
		return nil, errors.New("github sync: expected a context, found nil")
	}
	owner, name, err := splitRepo(s.cfg.Repo)
	if err != nil {
		return nil, err
	}
	return s.syncAll(ctx, owner, name, since)
}

func (s *Source) syncAll(ctx context.Context, owner, name string, since time.Time) ([]source.Item, error) {
	items := []source.Item{}
	seen := map[string]struct{}{}
	after := ""
	for {
		raw, err := s.run(ctx, syncArgs(owner, name, after, since)...)
		if err != nil {
			return nil, fmt.Errorf("github sync: %w", err)
		}
		page, err := decodeIssues(raw)
		if err != nil {
			return nil, fmt.Errorf("github sync: %w", err)
		}
		if err := pageProgress(page); err != nil {
			return nil, err
		}
		mapped, err := mapNodes(&s.cfg, page.nodes)
		if err != nil {
			return nil, fmt.Errorf("github sync: %w", err)
		}
		items = append(items, mapped...)
		if page.next == "" {
			return items, nil
		}
		if _, ok := seen[page.next]; ok {
			return nil, fmt.Errorf("github sync: expected a new cursor, found %q again", page.next)
		}
		seen[page.next] = struct{}{}
		after = page.next
	}
}

func pageProgress(page issuePage) error {
	if page.next != "" && len(page.nodes) == 0 {
		return errors.New("github sync: expected issues on a page that has another page, found none")
	}
	return nil
}

// Get reads one issue by owner/repo#number.
// A missing issue returns source.ErrNotFound.
// An id outside this source's repository is an error.
func (s *Source) Get(ctx context.Context, id string) (source.Item, error) {
	if ctx == nil {
		return source.Item{}, errors.New("github get: expected a context, found nil")
	}
	item, _, err := s.fetchIssue(ctx, id)
	return item, err
}

// fetchIssue reads one issue and its GraphQL node id.
// The node id is the value Link and Unlink send as issueId or blockingIssueId.
func (s *Source) fetchIssue(ctx context.Context, id string) (source.Item, string, error) {
	owner, name, number, err := s.issueNumber(id)
	if err != nil {
		return source.Item{}, "", err
	}
	raw, err := s.run(ctx, getArgs(owner, name, number)...)
	if err != nil {
		return source.Item{}, "", fmt.Errorf("github get %s: %w", id, err)
	}
	node, err := decodeGetIssue(raw)
	if err != nil {
		return source.Item{}, "", fmt.Errorf("github get %s: %w", id, err)
	}
	item, err := mapNode(&s.cfg, node)
	if err != nil {
		return source.Item{}, "", fmt.Errorf("github get %s: %w", id, err)
	}
	nodeID, err := issueNodeID(node)
	if err != nil {
		return source.Item{}, "", fmt.Errorf("github get %s: %w", id, err)
	}
	return item, nodeID, nil
}

func (s *Source) issueNumber(id string) (owner, name string, number int, err error) {
	owner, name, number, err = s.issueRef(id)
	if err != nil {
		if !validRepo(s.cfg.Repo) {
			return "", "", 0, err
		}
		return "", "", 0, fmt.Errorf("github get: %w", err)
	}
	return owner, name, number, nil
}

// issueRef parses id as an issue in this source's repository.
// The error is not prefixed with "github get", so writers can wrap it.
func (s *Source) issueRef(id string) (owner, name string, number int, err error) {
	owner, name, err = splitRepo(s.cfg.Repo)
	if err != nil {
		return "", "", 0, err
	}
	var repo string
	repo, number, err = source.ParseGitHubID(id)
	if err != nil {
		return "", "", 0, err //nolint:wrapcheck // internal factory error already carries context
	}
	if repo != s.cfg.Repo {
		return "", "", 0, fmt.Errorf("expected an issue in %s, found %q", s.cfg.Repo, id)
	}
	return owner, name, number, nil
}

func splitRepo(repo string) (owner, name string, err error) {
	if !validRepo(repo) {
		return "", "", fmt.Errorf("github: expected repo owner/name, found %q", repo)
	}
	owner, name, _ = strings.Cut(repo, "/")
	return owner, name, nil
}

// syncArgs builds `gh api graphql` arguments for one issues page.
// A zero since omits $since, so a full sync does not filter by date.
// A non-zero since is sent as UTC RFC3339, GitHub's DateTime shape.
func syncArgs(owner, name, after string, since time.Time) []string {
	args := []string{
		"api", "graphql",
		"-f", "query=" + syncQuery,
		"-f", "owner=" + owner,
		"-f", "name=" + name,
	}
	if after != "" {
		args = append(args, "-f", "after="+after)
	}
	if !since.IsZero() {
		args = append(args, "-f", "since="+since.UTC().Format(time.RFC3339))
	}
	return args
}

// getArgs builds `gh api graphql` arguments for repository.issue.
// number is a typed field because the variable is Int!.
func getArgs(owner, name string, number int) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + getQuery,
		"-f", "owner=" + owner,
		"-f", "name=" + name,
		"-F", "number=" + strconv.Itoa(number),
	}
}
