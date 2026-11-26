package gate

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

// gitCommandTimeout matches the herdr and gh call timeout.
// A stuck git command becomes a reason instead of hanging the command.
const gitCommandTimeout = 60 * time.Second

// GitStatus returns the function Inputs.GitStatus expects.
// It runs `git -C dir status --porcelain` through exec.
// A non-zero exit or an executor error is returned as an error.
// Successful porcelain text is returned unchanged.
func GitStatus(exec executor.Executor) func(context.Context, string) (string, error) {
	return func(ctx context.Context, dir string) (string, error) {
		return gitOutput(ctx, exec, "git status", dir, "status", "--porcelain")
	}
}

// GitBranch returns the function Inputs.GitBranch expects.
// It runs `git -C dir rev-parse --abbrev-ref HEAD`.
// A detached HEAD is the name HEAD. A non-zero exit is an error.
func GitBranch(exec executor.Executor) func(context.Context, string) (string, error) {
	return func(ctx context.Context, dir string) (string, error) {
		out, err := gitOutput(ctx, exec, "git rev-parse", dir, "rev-parse", "--abbrev-ref", "HEAD")
		if err != nil {
			return "", err
		}
		name := strings.TrimSpace(out)
		if name == "" || strings.ContainsAny(name, "\r\n") {
			return "", fmt.Errorf("git rev-parse: expected one branch name, found %q", out)
		}
		return name, nil
	}
}

// GitAhead returns the function Inputs.GitAhead expects.
// It runs `git -C dir rev-list --count origin/<base>..HEAD` and does not fetch.
// base is the branch name, not a remote-tracking ref.
// A count of zero includes a checkout that is behind origin/base:
// the check is commits on HEAD that are not on that ref, not equality with it.
// An empty base is an error and does not run git.
func GitAhead(exec executor.Executor) func(context.Context, string, string) (int, error) {
	return func(ctx context.Context, dir, base string) (int, error) {
		if base == "" {
			return 0, errors.New("git rev-list: expected a branch, found empty")
		}
		rev := "origin/" + base + "..HEAD"
		out, err := gitOutput(ctx, exec, "git rev-list", dir, "rev-list", "--count", rev)
		if err != nil {
			return 0, err
		}
		text := strings.TrimSpace(out)
		n, convErr := strconv.Atoi(text)
		if convErr != nil {
			return 0, fmt.Errorf("git rev-list: expected an integer, found %q", out)
		}
		if n < 0 {
			return 0, fmt.Errorf("git rev-list: expected a non-negative count, found %d", n)
		}
		return n, nil
	}
}

// GitStashCount returns the number of lines from `git -C dir stash list`.
// An empty list is zero. A non-zero exit is an error.
func GitStashCount(exec executor.Executor) func(context.Context, string) (int, error) {
	return func(ctx context.Context, dir string) (int, error) {
		out, err := gitOutput(ctx, exec, "git stash list", dir, "stash", "list")
		if err != nil {
			return 0, err
		}
		return stashLines(out), nil
	}
}

// GitWorktreeAdd creates a git worktree at worktreeDir on a new branch from
// base (or HEAD when base is empty).
func GitWorktreeAdd(exec executor.Executor) func(ctx context.Context, repoDir, worktreeDir, branch, base string) error {
	return func(ctx context.Context, repoDir, worktreeDir, branch, base string) error {
		if worktreeDir == "" {
			return errors.New("git worktree add: expected a worktree directory, found empty")
		}
		if branch == "" {
			return errors.New("git worktree add: expected a branch name, found empty")
		}
		args := []string{"worktree", "add", "-b", branch, worktreeDir}
		if base != "" {
			args = append(args, base)
		}
		_, err := gitOutput(ctx, exec, "git worktree add", repoDir, args...)
		return err
	}
}

// GitWorktreeRemove removes a git worktree at worktreeDir.
func GitWorktreeRemove(exec executor.Executor) func(ctx context.Context, repoDir, worktreeDir string) error {
	return func(ctx context.Context, repoDir, worktreeDir string) error {
		if worktreeDir == "" {
			return errors.New("git worktree remove: expected a worktree directory, found empty")
		}
		_, err := gitOutput(ctx, exec, "git worktree remove", repoDir, "worktree", "remove", worktreeDir)
		return err
	}
}

func stashLines(out string) int {
	n := 0
	for _, line := range strings.Split(out, "\n") {
		if strings.TrimSpace(line) != "" {
			n++
		}
	}
	return n
}

func gitOutput(ctx context.Context, exec executor.Executor, label, dir string, args ...string) (string, error) {
	if exec == nil {
		return "", fmt.Errorf("%s: expected an executor, found nil", label)
	}
	if dir == "" {
		return "", fmt.Errorf("%s: expected a directory, found empty", label)
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
		return "", fmt.Errorf("%s: %w", label, err)
	}
	if result == nil {
		return "", fmt.Errorf("%s: expected an execution result, found nil", label)
	}
	if result.ExitCode != 0 {
		return "", gitFailure(label, result)
	}
	return result.Output, nil
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
