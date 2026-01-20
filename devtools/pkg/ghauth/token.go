// Package ghauth provides GitHub authentication utilities.
// It supports multiple token sources including the gh CLI and environment variables.
package ghauth

import (
	"context"
	"errors"
	"fmt"
)

// ErrNoToken is returned when no token can be found from any source.
var ErrNoToken = errors.New("no GitHub token found")

// TokenSource provides GitHub tokens from various sources.
type TokenSource interface {
	// Token returns a GitHub token or an error if unavailable.
	Token(ctx context.Context) (string, error)
}

// ChainTokenSource tries multiple token sources in order until one succeeds.
type ChainTokenSource struct {
	Sources []TokenSource
}

// NewChainTokenSource creates a token source that tries multiple sources in order.
func NewChainTokenSource(sources ...TokenSource) *ChainTokenSource {
	return &ChainTokenSource{Sources: sources}
}

// Token tries each source in order and returns the first successful token.
func (c *ChainTokenSource) Token(ctx context.Context) (string, error) {
	var lastErr error

	for _, source := range c.Sources {
		token, err := source.Token(ctx)
		if err == nil && token != "" {
			return token, nil
		}
		if err != nil {
			lastErr = err
		}
	}

	if lastErr != nil {
		return "", fmt.Errorf("all token sources failed: %w", lastErr)
	}
	return "", ErrNoToken
}

// DefaultTokenSource returns the standard token source chain:
// 1. gh CLI token
// 2. GH_TOKEN environment variable
// 3. GITHUB_TOKEN environment variable.
func DefaultTokenSource(ghCLI TokenSource) *ChainTokenSource {
	return NewChainTokenSource(
		ghCLI,
		NewEnvTokenSource("GH_TOKEN"),
		NewEnvTokenSource("GITHUB_TOKEN"),
	)
}
