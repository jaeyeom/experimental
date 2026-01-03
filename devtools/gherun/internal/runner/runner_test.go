package runner

import (
	"context"
	"log/slog"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/browser"
	"github.com/jaeyeom/experimental/devtools/gherun/internal/gherkin"
	"github.com/jaeyeom/experimental/devtools/gherun/internal/github"
)

// mockLauncher implements browser.Launcher for testing.
type mockLauncher struct {
	mu           sync.Mutex
	launchedURLs []string
	launchErr    error
}

func (m *mockLauncher) Launch(_ context.Context, url string) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.launchErr != nil {
		return m.launchErr
	}
	m.launchedURLs = append(m.launchedURLs, url)
	return nil
}

func (m *mockLauncher) count() int {
	m.mu.Lock()
	defer m.mu.Unlock()
	return len(m.launchedURLs)
}

func TestRunner_launchBrowsers(t *testing.T) {
	features := []*gherkin.Feature{
		{ID: "login", Name: "User Login", Content: "Feature: Login"},
		{ID: "checkout", Name: "Checkout", Content: "Feature: Checkout"},
	}

	launcher := &mockLauncher{}
	logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.LevelError}))

	runner := &Runner{
		launcher:      launcher,
		promptBuilder: browser.NewPromptBuilder(),
		config:        Config{MaxParallel: 2},
		logger:        logger,
	}

	ctx := context.Background()
	err := runner.launchBrowsers(ctx, features, "https://github.com/test/repo/issues/1")
	if err != nil {
		t.Errorf("launchBrowsers() error = %v", err)
	}

	if launcher.count() != 2 {
		t.Errorf("launchBrowsers() launched %d URLs, want 2", launcher.count())
	}
}

func TestRunner_launchBrowsers_withParallelism(t *testing.T) {
	// Create many features to test parallelism
	features := make([]*gherkin.Feature, 10)
	for i := 0; i < 10; i++ {
		features[i] = &gherkin.Feature{
			ID:      "feature-" + string(rune('a'+i)),
			Name:    "Feature " + string(rune('A'+i)),
			Content: "Feature: Test",
		}
	}

	launcher := &mockLauncher{}
	logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.LevelError}))

	runner := &Runner{
		launcher:      launcher,
		promptBuilder: browser.NewPromptBuilder(),
		config:        Config{MaxParallel: 3}, // Limit to 3 concurrent
		logger:        logger,
	}

	ctx := context.Background()
	err := runner.launchBrowsers(ctx, features, "https://github.com/test/repo/issues/1")
	if err != nil {
		t.Errorf("launchBrowsers() error = %v", err)
	}

	if launcher.count() != 10 {
		t.Errorf("launchBrowsers() launched %d URLs, want 10", launcher.count())
	}
}

func TestRunner_launchBrowsers_contextCancellation(_ *testing.T) {
	features := []*gherkin.Feature{
		{ID: "login", Name: "User Login", Content: "Feature: Login"},
	}

	launcher := &mockLauncher{}
	logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.LevelError}))

	runner := &Runner{
		launcher:      launcher,
		promptBuilder: browser.NewPromptBuilder(),
		config:        Config{MaxParallel: 1},
		logger:        logger,
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Millisecond)
	defer cancel()

	// This should complete quickly since we only have one feature
	err := runner.launchBrowsers(ctx, features, "https://github.com/test/repo/issues/1")

	// Error may or may not occur depending on timing
	_ = err
}

func TestProgressMonitor_PrintProgress(_ *testing.T) {
	logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.LevelError}))
	monitor := NewProgressMonitor(nil, time.Second, logger)

	progress := &github.TestProgress{
		Total:     5,
		Completed: 3,
		Passed:    2,
		Failed:    1,
	}

	// Just ensure it doesn't panic
	monitor.PrintProgress(progress)
}
