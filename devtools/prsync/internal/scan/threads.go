package scan

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

// threadLookup fetches review threads for one round, reusing those stored
// for a pull request whose head and updatedAt are unchanged.
type threadLookup struct {
	// stored is nil when threads are fetched every round.
	stored ThreadCache
	count  lookupCount
}

func newThreadLookup(deps Deps, cfg config.Config) *threadLookup {
	l := &threadLookup{}
	if cfg.ReviewThreadsTTL > 0 {
		l.stored = deps.Threads
	}
	return l
}

// fetch returns item's review threads. A fetched result is stored; a
// failed fetch is not.
func (l *threadLookup) fetch(ctx context.Context, client GH, owner, name, repo string, item gh.PRListItem) ([]gh.Thread, error) {
	if l.stored != nil {
		if threads, ok := l.stored.ReviewThreads(repo, item.Number, item.HeadRefOid, item.UpdatedAt); ok {
			l.count.hits++
			return threads, nil
		}
	}
	l.count.calls++
	threads, err := client.ReviewThreads(ctx, owner, name, item.Number)
	if err != nil {
		return nil, fmt.Errorf("review threads: %w", err)
	}
	if l.stored != nil {
		l.stored.PutReviewThreads(repo, item.Number, item.HeadRefOid, item.UpdatedAt, threads)
	}
	return threads, nil
}

// prune drops the stored threads of repo's pull requests not in open.
func (l *threadLookup) prune(repo string, open []gh.PRListItem) {
	if l.stored == nil {
		return
	}
	numbers := make([]int, len(open))
	for i, item := range open {
		numbers[i] = item.Number
	}
	l.stored.PruneReviewThreads(repo, numbers)
}

// report logs the round's review-thread cache hits and calls.
func (l *threadLookup) report(ctx context.Context) {
	runlog.FromContext(ctx).Info(fmt.Sprintf("scan: review threads %d hits/%d calls", l.count.hits, l.count.calls))
}
