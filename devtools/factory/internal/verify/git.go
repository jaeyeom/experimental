package verify

import (
	"context"
	"errors"
	"fmt"
	"regexp"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
)

// ErrNotAncestor means the merge-base commit is not on origin/<base>.
var ErrNotAncestor = errors.New("merge-base is not an ancestor of the base ref")

const (
	gitCommandTimeout = 60 * time.Second
	mergeLogLimit     = "100"
)

type logCommit struct {
	sha     string
	parents []string
	subject string
	body    string
}

// LandedCommit is a linear commit on origin/<base> matching a work item.
type LandedCommit struct {
	SHA     string
	Subject string
}

// GitMerge checks that origin/<base> contains a commit referencing id (via
// pattern, item ID, or #<number> for GitHub owner/repo#N) and that no merge
// commits appear at or after the oldest matching commit in recent history.
// It does not fetch.
func GitMerge(exec executor.Executor) func(context.Context, string, string, string, string) ([]string, error) {
	return func(ctx context.Context, dir, base, id, pattern string) ([]string, error) {
		_, reason, err := findLinearCommit(ctx, exec, dir, base, id, pattern)
		if err != nil {
			return nil, err
		}
		if reason != "" {
			return []string{reason}, nil
		}
		return nil, nil
	}
}

// FindLinearCommit checks origin/<base> in dir for a commit referencing id (via
// pattern, item ID, or #<number> for GitHub owner/repo#N) and verifies that no
// merge commits appear at or after the oldest matching commit in recent history.
// It returns the newest matching commit, a failure reason when none landed or
// history is not linear, and any git execution or parse error. It does not fetch.
func FindLinearCommit(exec executor.Executor) func(context.Context, string, string, string, string) (LandedCommit, string, error) {
	return func(ctx context.Context, dir, base, id, pattern string) (LandedCommit, string, error) {
		return findLinearCommit(ctx, exec, dir, base, id, pattern)
	}
}

func findLinearCommit(ctx context.Context, exec executor.Executor, dir, base, id, pattern string) (LandedCommit, string, error) {
	if base == "" {
		return LandedCommit{}, "", errors.New("git log: expected a branch, found empty")
	}
	if id == "" {
		return LandedCommit{}, "", errors.New("git log: expected a non-empty id, found empty")
	}
	ref := "origin/" + base
	out, err := runGit(ctx, exec, "git log", dir, "log", ref, "-n", mergeLogLimit, "--format=%H%x1f%P%x1f%B%x1e")
	if err != nil {
		return LandedCommit{}, "", err
	}
	commits, err := parseLogCommits(out)
	if err != nil {
		return LandedCommit{}, "", err
	}
	firstMatch := -1
	lastMatch := -1
	for i, c := range commits {
		ok, err := matchesItemCommit(c.body, id, pattern)
		if err != nil {
			return LandedCommit{}, "", err
		}
		if ok {
			if firstMatch < 0 {
				firstMatch = i
			}
			lastMatch = i
		}
	}
	if lastMatch < 0 {
		return LandedCommit{}, fmt.Sprintf("merge: expected a commit referencing %s on %s, found none", id, ref), nil
	}
	for i := 0; i <= lastMatch; i++ {
		c := commits[i]
		if len(c.parents) > 1 {
			return LandedCommit{}, fmt.Sprintf("merge: expected linear history on %s, found merge commit %s (%s)", ref, shortSHA(c.sha), c.subject), nil
		}
	}
	return LandedCommit{SHA: commits[firstMatch].sha, Subject: commits[firstMatch].subject}, "", nil
}

func shortSHA(sha string) string {
	if len(sha) > 7 {
		return sha[:7]
	}
	return sha
}

func parseLogCommits(out string) ([]logCommit, error) {
	records := strings.Split(out, "\x1e")
	commits := make([]logCommit, 0, len(records))
	for _, rec := range records {
		rec = strings.TrimLeft(rec, "\r\n")
		if rec == "" {
			continue
		}
		parts := strings.Split(rec, "\x1f")
		if len(parts) != 3 {
			return nil, fmt.Errorf("git log: expected 3 fields per commit, found %d", len(parts))
		}
		sha := strings.TrimSpace(parts[0])
		if sha == "" {
			return nil, errors.New("git log: expected a commit hash, found empty")
		}
		body := parts[2]
		subject, _, _ := strings.Cut(strings.TrimSpace(body), "\n")
		commits = append(commits, logCommit{
			sha:     sha,
			parents: strings.Fields(parts[1]),
			subject: subject,
			body:    body,
		})
	}
	return commits, nil
}

func matchesItemCommit(body, id, pattern string) (bool, error) {
	if pattern != "" {
		ok, err := gh.TitleMatches(pattern, id, body)
		if err != nil {
			return false, err //nolint:wrapcheck // internal factory error already carries context
		}
		if ok {
			return true, nil
		}
	}
	if _, num, ok := strings.Cut(id, "#"); ok && num != "" {
		re := regexp.MustCompile(`(?:^|[^0-9A-Za-z_])#` + regexp.QuoteMeta(num) + `(?:[^0-9]|$)`)
		if re.MatchString(body) {
			return true, nil
		}
	}
	re := regexp.MustCompile(`(?:^|[^0-9A-Za-z_])` + regexp.QuoteMeta(id) + `(?:[^0-9A-Za-z_]|$)`)
	return re.MatchString(body), nil
}

// GitBase returns the committer time of the merge-base of HEAD and origin/base.
// It does not fetch. Exit 1 from `merge-base --is-ancestor` is ErrNotAncestor.
func GitBase(exec executor.Executor) func(context.Context, string, string) (time.Time, error) {
	return func(ctx context.Context, dir, base string) (time.Time, error) {
		if base == "" {
			return time.Time{}, errors.New("git merge-base: expected a branch, found empty")
		}
		ref := "origin/" + base
		shaOut, err := runGit(ctx, exec, "git merge-base", dir, "merge-base", "HEAD", ref)
		if err != nil {
			return time.Time{}, err
		}
		sha := strings.TrimSpace(shaOut)
		if sha == "" || strings.ContainsAny(sha, " \t\r\n") {
			return time.Time{}, fmt.Errorf("git merge-base: expected one commit, found %q", shaOut)
		}
		if err := ancestor(ctx, exec, dir, sha, ref); err != nil {
			return time.Time{}, err
		}
		whenOut, err := runGit(ctx, exec, "git log", dir, "log", "-1", "--format=%cI", sha)
		if err != nil {
			return time.Time{}, err
		}
		text := strings.TrimSpace(whenOut)
		when, err := time.Parse(time.RFC3339, text)
		if err != nil {
			return time.Time{}, fmt.Errorf("git log: expected an RFC3339 committer time, found %q", whenOut)
		}
		return when, nil
	}
}

func ancestor(ctx context.Context, exec executor.Executor, dir, sha, ref string) error {
	result, err := executeGit(ctx, exec, "git merge-base", dir, "merge-base", "--is-ancestor", sha, ref)
	if err != nil {
		return err
	}
	if result.ExitCode == 1 {
		return ErrNotAncestor
	}
	if result.ExitCode != 0 {
		return gitFailure("git merge-base", result)
	}
	return nil
}

func runGit(ctx context.Context, exec executor.Executor, label, dir string, args ...string) (string, error) {
	result, err := executeGit(ctx, exec, label, dir, args...)
	if err != nil {
		return "", err
	}
	if result.ExitCode != 0 {
		return "", gitFailure(label, result)
	}
	return result.Output, nil
}

func executeGit(ctx context.Context, exec executor.Executor, label, dir string, args ...string) (*executor.ExecutionResult, error) {
	if exec == nil {
		return nil, fmt.Errorf("%s: expected an executor, found nil", label)
	}
	if dir == "" {
		return nil, fmt.Errorf("%s: expected a directory, found empty", label)
	}
	full := make([]string, 0, len(args)+2)
	full = append(full, "-C", dir)
	full = append(full, args...)
	result, err := exec.Execute(ctx, executor.ToolConfig{
		Command: "git",
		Args:    full,
		Timeout: gitCommandTimeout,
	})
	if err != nil {
		return nil, fmt.Errorf("%s: %w", label, err)
	}
	if result == nil {
		return nil, fmt.Errorf("%s: expected an execution result, found nil", label)
	}
	return result, nil
}

func gitFailure(label string, result *executor.ExecutionResult) error {
	detail := strings.TrimSpace(result.Stderr)
	if detail == "" {
		detail = strings.TrimSpace(result.Output)
	}
	if detail == "" {
		return fmt.Errorf("%s: expected success, found exit %d", label, result.ExitCode)
	}
	return fmt.Errorf("%s: %s", label, detail)
}
