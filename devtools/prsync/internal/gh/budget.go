package gh

import (
	"context"
	"errors"
	"fmt"
	"sync"
)

// ErrCallCap is a call refused because a CallBudget is spent.
var ErrCallCap = errors.New("call cap reached")

// CallBudget caps the gh invocations made under a context, retries
// included. Under a budget a rate limit is not retried: the budget stops
// and refuses every later call. A nil budget is unlimited. It is safe for
// concurrent use.
type CallBudget struct {
	mu      sync.Mutex
	max     int
	used    int
	limited bool
}

// NewCallBudget allows max calls; max <= 0 allows any number.
func NewCallBudget(maxCalls int) *CallBudget {
	return &CallBudget{max: maxCalls}
}

type budgetKey struct{}

// WithCallBudget makes every gh call under ctx take one call from b.
func WithCallBudget(ctx context.Context, b *CallBudget) context.Context {
	return context.WithValue(ctx, budgetKey{}, b)
}

// CallBudgetFrom is the budget of ctx, or nil.
func CallBudgetFrom(ctx context.Context) *CallBudget {
	b, _ := ctx.Value(budgetKey{}).(*CallBudget)
	return b
}

// Take counts one call. It returns ErrCallCap when the budget is spent and
// ErrRateLimited once a call hit a rate limit.
func (b *CallBudget) Take() error {
	if b == nil {
		return nil
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.limited {
		return fmt.Errorf("%w: stopped after an earlier call", ErrRateLimited)
	}
	if b.max > 0 && b.used >= b.max {
		return fmt.Errorf("%w (%d)", ErrCallCap, b.max)
	}
	b.used++
	return nil
}

// Used is how many calls were taken.
func (b *CallBudget) Used() int {
	if b == nil {
		return 0
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.used
}

// rollupStats counts status-check cache hits and fetches for one scan round.
// It is safe for concurrent use.
type rollupStats struct {
	mu    sync.Mutex
	hits  int
	calls int
}

type rollupStatsKey struct{}

// WithRollupStats counts status-check cache hits and fetches under ctx.
// A context that already counts is returned unchanged.
func WithRollupStats(ctx context.Context) context.Context {
	if _, ok := ctx.Value(rollupStatsKey{}).(*rollupStats); ok {
		return ctx
	}
	return context.WithValue(ctx, rollupStatsKey{}, &rollupStats{})
}

// RollupStats is the status-check hits and fetches counted under ctx.
func RollupStats(ctx context.Context) (hits, calls int) {
	stats, _ := ctx.Value(rollupStatsKey{}).(*rollupStats)
	if stats == nil {
		return 0, 0
	}
	stats.mu.Lock()
	defer stats.mu.Unlock()
	return stats.hits, stats.calls
}

func noteRollup(ctx context.Context, hit bool) {
	stats, _ := ctx.Value(rollupStatsKey{}).(*rollupStats)
	if stats == nil {
		return
	}
	stats.mu.Lock()
	defer stats.mu.Unlock()
	if hit {
		stats.hits++
		return
	}
	stats.calls++
}

func noteRollupHit(ctx context.Context)  { noteRollup(ctx, true) }
func noteRollupCall(ctx context.Context) { noteRollup(ctx, false) }

// Narrow lowers the cap to n further calls until restore runs. Those
// calls still count toward the cap restore puts back. n <= 0 allows no
// further call until restore. A nil budget returns a no-op restore.
func (b *CallBudget) Narrow(n int) func() {
	if b == nil {
		return func() {}
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	old := b.max
	if n < 0 {
		n = 0
	}
	next := b.used + n
	if b.max <= 0 || next < b.max {
		b.max = next
	}
	return func() {
		b.mu.Lock()
		b.max = old
		b.mu.Unlock()
	}
}

// Left is how many calls remain, or -1 without a cap.
func (b *CallBudget) Left() int {
	if b == nil {
		return -1
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.max <= 0 {
		return -1
	}
	return b.max - b.used
}

// RateLimited reports whether a call under the budget hit a rate limit.
func (b *CallBudget) RateLimited() bool {
	if b == nil {
		return false
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.limited
}

func (b *CallBudget) markLimited() {
	if b == nil {
		return
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	b.limited = true
}
