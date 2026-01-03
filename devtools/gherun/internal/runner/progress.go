package runner

import (
	"context"
	"fmt"
	"log/slog"
	"time"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/github"
)

// ProgressMonitor polls GitHub issue for completion status.
type ProgressMonitor struct {
	issueManager *github.IssueManager
	interval     time.Duration
	logger       *slog.Logger
}

// NewProgressMonitor creates a new ProgressMonitor.
func NewProgressMonitor(issueManager *github.IssueManager, interval time.Duration, logger *slog.Logger) *ProgressMonitor {
	return &ProgressMonitor{
		issueManager: issueManager,
		interval:     interval,
		logger:       logger,
	}
}

// Monitor polls the issue until all tests complete or context is cancelled.
func (m *ProgressMonitor) Monitor(ctx context.Context, issueNumber int, totalTests int) (*github.TestProgress, error) {
	ticker := time.NewTicker(m.interval)
	defer ticker.Stop()

	fmt.Printf("\nWaiting for tests to complete (polling every %s)...\n", m.interval)

	for {
		select {
		case <-ctx.Done():
			return nil, fmt.Errorf("monitoring cancelled: %w", ctx.Err())
		case <-ticker.C:
			progress, err := m.issueManager.GetTestProgress(issueNumber)
			if err != nil {
				m.logger.Warn("Failed to get progress", "error", err)
				continue
			}

			m.PrintProgress(progress)

			if progress.Completed >= totalTests {
				fmt.Println() // New line after progress
				return progress, nil
			}
		}
	}
}

// PrintProgress outputs current progress to console.
func (m *ProgressMonitor) PrintProgress(progress *github.TestProgress) {
	fmt.Printf("\r[%d/%d] Completed: %d passed, %d failed",
		progress.Completed, progress.Total, progress.Passed, progress.Failed)
}
