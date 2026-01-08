// Package git provides Git operations for bazel-affected-tests.
package git

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

// GetStagedFiles returns the list of staged files (Added, Copied, Modified - not Deleted).
func GetStagedFiles(ctx context.Context, exec executor.Executor) ([]string, error) {
	output, err := executor.Output(ctx, exec, "git", "diff", "--cached", "--name-only", "--diff-filter=ACM")
	if err != nil {
		return nil, fmt.Errorf("failed to get staged files: %w", err)
	}

	if len(output) == 0 {
		return []string{}, nil
	}

	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	var files []string
	for _, line := range lines {
		if line != "" {
			files = append(files, line)
		}
	}
	return files, nil
}
