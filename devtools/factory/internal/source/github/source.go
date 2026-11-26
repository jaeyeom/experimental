// Package github is a GitHub Issues source.
// Sync and Get read issues through gh api graphql. Claim, Release, Link,
// Unlink, Create, and SetPriority write through the same gh binary. Finish is a no-op:
// Fixes #N in the pull request closes the issue. No token is read here.
package github

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const (
	// callTimeout matches the gh client's call timeout.
	callTimeout = 60 * time.Second

	// maxLabels is the labels(first:50) page size.
	// Fifty labels is treated as truncated: the query has no labels pageInfo.
	maxLabels = 50

	// maxBlocked is the blockedBy(first:50) page size.
	maxBlocked = 50

	// defaultPriority is the rank when no label is in PriorityLabels.
	// It sorts with Lowest, after Low.
	defaultPriority = 4

	// nodeFields is the issue selection shared by Sync and Get.
	nodeFields = "id number title url state " +
		"author{login} authorAssociation locked " +
		"status{name} " +
		"createdAt updatedAt " +
		"issueType{name} " +
		"labels(first:50){nodes{name}} " +
		"assignees(first:10){nodes{login}} " +
		"milestone{dueOn} " +
		"issueFieldValues(first:50){nodes{" +
		"__typename " +
		"... on IssueFieldSingleSelectValue{name field{... on IssueFieldSingleSelect{name}}} " +
		"... on IssueFieldDateValue{value field{... on IssueFieldDate{name}}}" +
		"}} " +
		"parent{number repository{nameWithOwner}} " +
		"blockedBy(first:50){pageInfo{hasNextPage} nodes{id number repository{nameWithOwner}}}"

	// syncQuery is the issues connection from issue #20.
	// A full sync omits the $since variable; filterBy.since is then null.
	syncQuery = "query($owner:String!,$name:String!,$after:String,$since:DateTime){" +
		"repository(owner:$owner,name:$name){" +
		"issues(first:50,after:$after,orderBy:{field:UPDATED_AT,direction:ASC}," +
		"filterBy:{since:$since},states:[OPEN,CLOSED]){" +
		"pageInfo{hasNextPage endCursor}" +
		"nodes{" + nodeFields + "}}}}"

	// getQuery reads one issue with the same node fields as syncQuery.
	getQuery = "query($owner:String!,$name:String!,$number:Int!){" +
		"repository(owner:$owner,name:$name){" +
		"issue(number:$number){" + nodeFields + "}}}"
)

// Source reads issues from one GitHub repository through gh.
type Source struct {
	cfg  config.Source
	exec executor.Executor
	bin  string

	mu           sync.Mutex
	cached       bool
	login        string
	statusCached bool
	statusIDs    map[string]string
	statusNames  []string
	fieldsCached bool
	issueFields  map[string]issueFieldDef
}

// New returns a GitHub source that runs bin through exec.
// Sync and Get call `gh api graphql`. Me calls `gh api user --jq .login`.
func New(cfg config.Source, exec executor.Executor, bin string) *Source {
	return &Source{cfg: cfg, exec: exec, bin: bin}
}

// Compile-time check that Source implements source.Source.
var _ source.Source = (*Source)(nil)

// Name returns the configured source name.
func (s *Source) Name() string {
	return s.cfg.Name
}

// Me returns the authenticated user's login.
// The first successful call is cached.
func (s *Source) Me(ctx context.Context) (string, error) {
	if ctx == nil {
		return "", errors.New("github me: expected a context, found nil")
	}
	s.mu.Lock()
	if s.cached {
		login := s.login
		s.mu.Unlock()
		return login, nil
	}
	s.mu.Unlock()

	login, err := s.loadLogin(ctx)
	if err != nil {
		return "", err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if !s.cached {
		s.login = login
		s.cached = true
	}
	return s.login, nil
}

func (s *Source) loadLogin(ctx context.Context) (string, error) {
	raw, err := s.run(ctx, "api", "user", "--jq", ".login")
	if err != nil {
		return "", fmt.Errorf("github me: %w", err)
	}
	login, err := parseLogin(string(raw))
	if err != nil {
		return "", fmt.Errorf("github me: %w", err)
	}
	return login, nil
}

func parseLogin(output string) (string, error) {
	login := strings.TrimSpace(output)
	if login == "" {
		return "", errors.New("expected a login, found empty")
	}
	if strings.ContainsAny(login, " \t\r\n\"'{}[]/") {
		return "", fmt.Errorf("expected a login, found %q", login)
	}
	return login, nil
}

func (s *Source) run(ctx context.Context, args ...string) ([]byte, error) {
	if s.exec == nil {
		return nil, errors.New("github: expected an executor, found nil")
	}
	if s.bin == "" {
		return nil, errors.New("github: expected a gh binary, found empty")
	}
	result, err := s.exec.Execute(ctx, executor.ToolConfig{
		Command: s.bin,
		Args:    args,
		Timeout: callTimeout,
	})
	if err != nil {
		return nil, fmt.Errorf("run %s: %w", s.bin, err)
	}
	if result == nil {
		return nil, fmt.Errorf("run %s: expected an execution result, found nil", s.bin)
	}
	if result.ExitCode != 0 {
		return nil, commandFailure(result)
	}
	return []byte(result.Output), nil
}

func commandFailure(result *executor.ExecutionResult) error {
	detail := strings.TrimSpace(result.Stderr)
	if detail == "" {
		detail = strings.TrimSpace(result.Output)
	}
	if detail == "" {
		return fmt.Errorf("github: expected stderr, found exit %d", result.ExitCode)
	}
	return fmt.Errorf("github: %s", detail)
}
