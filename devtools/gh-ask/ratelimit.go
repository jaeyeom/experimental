package ghask

import (
	"sync"
	"time"
)

// limiter allows at most limit events in any sliding window.
type limiter struct {
	mu     sync.Mutex
	limit  int
	window time.Duration
	now    func() time.Time
	events []time.Time
}

func newLimiter(limit int, window time.Duration, now func() time.Time) *limiter {
	return &limiter{limit: limit, window: window, now: now}
}

// Allow records an event and reports true when the window has room.
func (l *limiter) Allow() bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	now := l.now()
	keep := l.events[:0]
	for _, at := range l.events {
		if now.Sub(at) < l.window {
			keep = append(keep, at)
		}
	}
	l.events = keep
	if len(l.events) >= l.limit {
		return false
	}
	l.events = append(l.events, now)
	return true
}
