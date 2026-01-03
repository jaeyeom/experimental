// Package runner provides the main orchestration for gherun test execution.
package runner

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/browser"
	"github.com/jaeyeom/experimental/devtools/gherun/internal/gherkin"
	"github.com/jaeyeom/experimental/devtools/gherun/internal/github"
)

// Config holds runner configuration.
type Config struct {
	MaxParallel  int           // Maximum concurrent browser tabs
	PollInterval time.Duration // Interval for checking GitHub issue
	IssueTitle   string        // Title for the GitHub issue
	Verbose      bool          // Enable verbose logging
}

// Summary contains the final test results.
type Summary struct {
	Total    int
	Passed   int
	Failed   int
	Duration time.Duration
	IssueURL string
}

// Runner orchestrates the test execution.
type Runner struct {
	parser        gherkin.Parser
	issueManager  *github.IssueManager
	launcher      browser.Launcher
	promptBuilder *browser.PromptBuilder
	config        Config
	logger        *slog.Logger
}

// NewRunner creates a runner with all dependencies.
func NewRunner(
	parser gherkin.Parser,
	issueManager *github.IssueManager,
	launcher browser.Launcher,
	promptBuilder *browser.PromptBuilder,
	config Config,
	logger *slog.Logger,
) *Runner {
	return &Runner{
		parser:        parser,
		issueManager:  issueManager,
		launcher:      launcher,
		promptBuilder: promptBuilder,
		config:        config,
		logger:        logger,
	}
}

// Run executes the full test workflow.
func (r *Runner) Run(ctx context.Context, featureFiles []string) (*Summary, error) {
	startTime := time.Now()

	// 1. Parse features
	r.logger.Info("Parsing feature files", "count", len(featureFiles))
	features, err := r.parser.ParseAll(featureFiles)
	if err != nil {
		return nil, fmt.Errorf("parsing features: %w", err)
	}
	r.logger.Info("Parsed features successfully", "count", len(features))

	// 2. Create GitHub issue
	r.logger.Info("Creating GitHub issue", "title", r.config.IssueTitle)
	issue, err := r.issueManager.CreateSuiteIssue(features, r.config.IssueTitle)
	if err != nil {
		return nil, fmt.Errorf("creating issue: %w", err)
	}
	r.logger.Info("Created GitHub issue", "url", issue.URL, "number", issue.Number)
	fmt.Printf("GitHub Issue created: %s\n", issue.URL)

	// 3. Launch browsers with parallelism
	r.logger.Info("Launching browsers", "maxParallel", r.config.MaxParallel)
	if err := r.launchBrowsers(ctx, features, issue.URL); err != nil {
		return nil, fmt.Errorf("launching browsers: %w", err)
	}

	// 4. Monitor progress
	r.logger.Info("Starting progress monitoring", "pollInterval", r.config.PollInterval)
	monitor := NewProgressMonitor(r.issueManager, r.config.PollInterval, r.logger)
	progress, err := monitor.Monitor(ctx, issue.Number, len(features))
	if err != nil {
		return nil, fmt.Errorf("monitoring progress: %w", err)
	}

	// 5. Return summary
	return &Summary{
		Total:    progress.Total,
		Passed:   progress.Passed,
		Failed:   progress.Failed,
		Duration: time.Since(startTime),
		IssueURL: issue.URL,
	}, nil
}

func (r *Runner) launchBrowsers(ctx context.Context, features []*gherkin.Feature, issueURL string) error {
	sem := make(chan struct{}, r.config.MaxParallel)
	var wg sync.WaitGroup
	var errs []error
	var mu sync.Mutex

	for _, f := range features {
		wg.Add(1)
		go func(feature *gherkin.Feature) {
			defer wg.Done()

			// Acquire semaphore slot
			select {
			case sem <- struct{}{}:
				defer func() { <-sem }()
			case <-ctx.Done():
				mu.Lock()
				errs = append(errs, ctx.Err())
				mu.Unlock()
				return
			}

			prompt := r.promptBuilder.Build(feature, issueURL)
			url := browser.BuildPerplexityURL(prompt)

			r.logger.Debug("Launching browser", "featureID", feature.ID, "url", url)
			if err := r.launcher.Launch(ctx, url); err != nil {
				mu.Lock()
				errs = append(errs, fmt.Errorf("launching %s: %w", feature.ID, err))
				mu.Unlock()
				return
			}

			r.logger.Info("Launched browser for feature", "id", feature.ID)
			fmt.Printf("Launched: %s - %s\n", feature.ID, feature.Name)
		}(f)
	}

	wg.Wait()

	if len(errs) > 0 {
		return errors.Join(errs...)
	}
	return nil
}
