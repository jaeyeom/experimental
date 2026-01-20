package ghauth

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/executor"
)

// GHCLITokenSource retrieves GitHub tokens from the gh CLI.
type GHCLITokenSource struct {
	exec     executor.Executor
	hostname string
}

// NewGHCLITokenSource creates a token source that uses the gh CLI.
// If hostname is empty, it defaults to "github.com".
func NewGHCLITokenSource(exec executor.Executor, hostname string) *GHCLITokenSource {
	if hostname == "" {
		hostname = "github.com"
	}
	return &GHCLITokenSource{
		exec:     exec,
		hostname: hostname,
	}
}

// Token retrieves the token from gh auth token command.
func (g *GHCLITokenSource) Token(ctx context.Context) (string, error) {
	output, err := executor.Output(ctx, g.exec, "gh", "auth", "token", "--hostname", g.hostname)
	if err != nil {
		return "", fmt.Errorf("failed to get token from gh CLI: %w", err)
	}

	token := strings.TrimSpace(string(output))
	if token == "" {
		return "", fmt.Errorf("gh CLI returned empty token")
	}

	return token, nil
}

// Hostname returns the hostname this token source is configured for.
func (g *GHCLITokenSource) Hostname() string {
	return g.hostname
}

// IsGHCLIAvailable checks if the gh CLI is available and authenticated.
func IsGHCLIAvailable(ctx context.Context, exec executor.Executor) bool {
	// Check if gh is available
	if !exec.IsAvailable("gh") {
		return false
	}

	// Check if gh is authenticated by trying to get a token
	source := NewGHCLITokenSource(exec, "")
	_, err := source.Token(ctx)
	return err == nil
}
