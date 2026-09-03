// Package gitchanges lists files changed against a git base ref.
package gitchanges

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const gitTimeout = 60 * time.Second

// ErrNotRepo means the workDir is not inside a git repository.
var ErrNotRepo = errors.New("not a git repository")

// Changed returns repo-root-relative paths from `git diff --name-only <base>...HEAD`.
func Changed(ctx context.Context, exec executor.Executor, base, workDir string) ([]string, error) {
	result, err := runGit(ctx, exec, workDir, "diff", "--name-only", base+"...HEAD")
	if err != nil {
		return nil, err
	}
	var files []string
	for _, line := range strings.Split(result.Output, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		files = append(files, line)
	}
	if files == nil {
		files = []string{}
	}
	return files, nil
}

// RepoRoot returns `git rev-parse --show-toplevel`.
func RepoRoot(ctx context.Context, exec executor.Executor, workDir string) (string, error) {
	result, err := runGit(ctx, exec, workDir, "rev-parse", "--show-toplevel")
	if err != nil {
		return "", err
	}
	root := strings.TrimSpace(result.Output)
	if root == "" {
		return "", fmt.Errorf("git rev-parse --show-toplevel: empty output")
	}
	return root, nil
}

func runGit(ctx context.Context, exec executor.Executor, workDir string, args ...string) (*executor.ExecutionResult, error) {
	result, err := exec.Execute(ctx, executor.ToolConfig{
		Command:    "git",
		Args:       args,
		WorkingDir: workDir,
		Timeout:    gitTimeout,
	})
	if err != nil {
		return nil, fmt.Errorf("run git: %w", err)
	}
	if result.ExitCode != 0 {
		msg := strings.TrimSpace(result.Stderr)
		if msg == "" {
			msg = strings.TrimSpace(result.Output)
		}
		if strings.Contains(strings.ToLower(msg), "not a git repository") {
			return nil, fmt.Errorf("%w: %s", ErrNotRepo, msg)
		}
		return nil, fmt.Errorf("git %s: %s", strings.Join(args, " "), msg)
	}
	return result, nil
}
