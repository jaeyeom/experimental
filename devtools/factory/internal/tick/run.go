package tick

import (
	"context"
	"fmt"
	"log/slog"
	"time"
)

const maxConsecutiveErrors = 5

// Run calls Tick immediately and then every interval until ctx is cancelled.
// The current tick finishes after cancellation. The caller wraps ctx with
// signal.NotifyContext for SIGINT and SIGTERM. Five consecutive tick errors
// are returned. A clean stop returns nil.
func (e *Engine) Run(ctx context.Context, interval time.Duration) error {
	if err := e.validate(ctx); err != nil {
		return err
	}
	if interval <= 0 {
		return fmt.Errorf("run: expected a positive interval, found %s", interval)
	}
	consecutive := 0
	for {
		if ctx.Err() != nil {
			return nil
		}
		// WithoutCancel lets the current tick finish after the parent is cancelled.
		err := e.Tick(context.WithoutCancel(ctx))
		if err != nil {
			consecutive++
			slog.Error("tick failed", "err", err, "consecutive", consecutive)
			if consecutive >= maxConsecutiveErrors {
				return fmt.Errorf("run: %d consecutive tick errors: %w", consecutive, err)
			}
		} else {
			consecutive = 0
		}
		if err := waitInterval(ctx, interval); err != nil {
			return nil
		}
	}
}

func waitInterval(ctx context.Context, interval time.Duration) error {
	timer := time.NewTimer(interval)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err() //nolint:wrapcheck // propagate context cancellation
	case <-timer.C:
		return nil
	}
}
