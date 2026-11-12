package scan

import (
	"context"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

// PRViewCache stores merged and closed pull-request views between rounds
// and owns the closed-view expiry (pr_view_closed_ttl).
type PRViewCache interface {
	PRView(repo string, number int) (gh.PRSearchItem, bool)
	PutPRView(repo string, number int, item gh.PRSearchItem)
	// PrunePRViews drops the views of pull requests keep rejects.
	PrunePRViews(keep func(repo string, number int) bool)
}

// PRViewLookup is an OrphanGH whose ViewPRState reuses stored views of
// merged pull requests, and of closed ones while closedTTL > 0. Open
// results, errors and gh.ErrNotFound are never stored.
type PRViewLookup struct {
	OrphanGH
	// stored is nil when every view is fetched.
	stored    PRViewCache
	closedTTL time.Duration
	seen      map[string]map[int]struct{}
	count     lookupCount
}

// NewPRViewLookup wraps client. A nil stored fetches every view.
func NewPRViewLookup(client OrphanGH, stored PRViewCache, closedTTL time.Duration) *PRViewLookup {
	return &PRViewLookup{
		OrphanGH:  client,
		stored:    stored,
		closedTTL: closedTTL,
		seen:      map[string]map[int]struct{}{},
	}
}

// ViewPRState returns the stored view of repo#number when it may be
// reused, and otherwise fetches and stores it.
func (l *PRViewLookup) ViewPRState(ctx context.Context, repo string, number int) (gh.PRSearchItem, error) {
	if l.seen[repo] == nil {
		l.seen[repo] = map[int]struct{}{}
	}
	l.seen[repo][number] = struct{}{}
	if l.stored != nil {
		if item, ok := l.stored.PRView(repo, number); ok && l.reusable(item) {
			l.count.hits++
			return item, nil
		}
	}
	l.count.calls++
	item, err := l.OrphanGH.ViewPRState(ctx, repo, number)
	if err != nil {
		return gh.PRSearchItem{}, fmt.Errorf("view pr state: %w", err)
	}
	if l.stored != nil && l.reusable(item) {
		l.stored.PutPRView(repo, number, item)
	}
	return item, nil
}

func (l *PRViewLookup) reusable(item gh.PRSearchItem) bool {
	switch item.State {
	case "merged":
		return true
	case "closed":
		return l.closedTTL > 0
	default:
		return false
	}
}

// Prune drops the stored views of pull requests this round did not look
// up, so the cache only holds numbers a tab still references. Call it
// only after a complete round.
func (l *PRViewLookup) Prune() {
	if l.stored == nil {
		return
	}
	l.stored.PrunePRViews(func(repo string, number int) bool {
		_, ok := l.seen[repo][number]
		return ok
	})
}

// Report logs the round's pull-request view cache hits and calls.
func (l *PRViewLookup) Report(ctx context.Context) {
	runlog.FromContext(ctx).Info(fmt.Sprintf("tabs: pr view %d hits/%d calls", l.count.hits, l.count.calls))
}
