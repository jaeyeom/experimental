package github

import (
	"context"
	"errors"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// idsQuery lists issue numbers with the same repository and states as
// syncQuery and no since filter.
const idsQuery = "query($owner:String!,$name:String!,$after:String){" +
	"repository(owner:$owner,name:$name){" +
	"issues(first:100,after:$after,states:[OPEN,CLOSED]){" +
	"pageInfo{hasNextPage endCursor}" +
	"nodes{number}}}}"

// IDs returns owner/repo#N for every open and closed issue in the repository,
// the same set a full Sync returns. It reads issue numbers only.
func (s *Source) IDs(ctx context.Context) ([]string, error) {
	if ctx == nil {
		return nil, errors.New("github ids: expected a context, found nil")
	}
	owner, name, err := splitRepo(s.cfg.Repo)
	if err != nil {
		return nil, err
	}
	ids := []string{}
	seenID := map[string]struct{}{}
	seenCursor := map[string]struct{}{}
	after := ""
	for {
		raw, err := s.run(ctx, idsArgs(owner, name, after)...)
		if err != nil {
			return nil, fmt.Errorf("github ids: %w", err)
		}
		page, err := decodeIssues(raw)
		if err != nil {
			return nil, fmt.Errorf("github ids: %w", err)
		}
		if page.next != "" && len(page.nodes) == 0 {
			return nil, errors.New("github ids: expected issues on a page that has another page, found none")
		}
		for _, node := range page.nodes {
			obj, err := objectOf(node, "issue")
			if err != nil {
				return nil, fmt.Errorf("github ids: %w", err)
			}
			number, err := requiredPositiveInt(obj, "number")
			if err != nil {
				return nil, fmt.Errorf("github ids: issue: %w", err)
			}
			id := source.GitHubID(s.cfg.Repo, number)
			if _, ok := seenID[id]; ok {
				continue
			}
			seenID[id] = struct{}{}
			ids = append(ids, id)
		}
		if page.next == "" {
			return ids, nil
		}
		if _, ok := seenCursor[page.next]; ok {
			return nil, fmt.Errorf("github ids: expected a new cursor, found %q again", page.next)
		}
		seenCursor[page.next] = struct{}{}
		after = page.next
	}
}

// idsArgs builds `gh api graphql` arguments for one page of issue numbers.
func idsArgs(owner, name, after string) []string {
	args := []string{
		"api", "graphql",
		"-f", "query=" + idsQuery,
		"-f", "owner=" + owner,
		"-f", "name=" + name,
	}
	if after != "" {
		args = append(args, "-f", "after="+after)
	}
	return args
}
