// Package browser provides functionality to launch browsers with Perplexity URLs.
package browser

import (
	"context"
	"fmt"
	"net/url"
	"strings"

	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

// allowedSchemes defines the URL schemes that are safe to open in a browser.
var allowedSchemes = map[string]bool{
	"http":  true,
	"https": true,
}

// Launcher opens URLs in the browser.
type Launcher interface {
	Launch(ctx context.Context, url string) error
}

// OpenCommandLauncher uses the "open" command on macOS to launch URLs.
type OpenCommandLauncher struct {
	executor executor.Executor
}

// Compile-time check that OpenCommandLauncher implements Launcher.
var _ Launcher = (*OpenCommandLauncher)(nil)

// NewLauncher creates a new OpenCommandLauncher.
func NewLauncher(exec executor.Executor) *OpenCommandLauncher {
	return &OpenCommandLauncher{executor: exec}
}

// openCommand is the full path to the macOS open command.
// Using the full path prevents PATH manipulation attacks.
const openCommand = "/usr/bin/open"

// Launch opens the given URL in the default browser.
// It validates that the URL uses a safe scheme (http or https) to prevent
// command injection via malicious URL schemes like file://.
func (l *OpenCommandLauncher) Launch(ctx context.Context, targetURL string) error {
	if err := validateURLScheme(targetURL); err != nil {
		return err
	}
	if err := executor.Run(ctx, l.executor, openCommand, targetURL); err != nil {
		return fmt.Errorf("opening URL %s: %w", targetURL, err)
	}
	return nil
}

// validateURLScheme checks that a URL uses a safe scheme.
func validateURLScheme(targetURL string) error {
	parsed, err := url.Parse(targetURL)
	if err != nil {
		return fmt.Errorf("invalid URL %q: %w", targetURL, err)
	}
	scheme := strings.ToLower(parsed.Scheme)
	if !allowedSchemes[scheme] {
		return fmt.Errorf("unsupported URL scheme %q: only http and https are allowed", scheme)
	}
	return nil
}

// BuildPerplexityURL constructs the Perplexity search URL with proper escaping.
func BuildPerplexityURL(prompt string) string {
	return "https://www.perplexity.ai/search?q=" + url.QueryEscape(prompt)
}
