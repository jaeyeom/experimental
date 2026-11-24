package approve

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net"
	"strings"
	"sync"
	"time"
)

const (
	defaultBackoffBase  = time.Second
	maxBackoffDuration  = 60 * time.Second
	maxRateLimitRetries = 4
)

// isSecondaryRateLimit reports whether err looks like a GitHub secondary
// rate-limit or abuse-detection response.
func isSecondaryRateLimit(err error) bool {
	if err == nil {
		return false
	}
	msg := strings.ToLower(err.Error())
	if strings.Contains(msg, "secondary rate limit") || strings.Contains(msg, "abuse detection") {
		return true
	}
	return (strings.Contains(msg, "403") || strings.Contains(msg, "429")) && strings.Contains(msg, "rate limit")
}

// isTransientBatchError reports whether err looks like a transient gateway
// failure (HTTP 502/503/504), timeout, or truncated/garbled response that
// warrants retrying or splitting a batched GraphQL chunk.
func isTransientBatchError(err error) bool {
	if err == nil || isSecondaryRateLimit(err) {
		return false
	}
	if errors.Is(err, context.DeadlineExceeded) ||
		errors.Is(err, io.ErrUnexpectedEOF) ||
		errors.Is(err, net.ErrClosed) {
		return true
	}
	var netErr net.Error
	if errors.As(err, &netErr) && netErr.Timeout() {
		return true
	}
	var syntaxErr *json.SyntaxError
	if errors.As(err, &syntaxErr) {
		return true
	}
	msg := strings.ToLower(err.Error())
	if strings.Contains(msg, "invalid character ") && strings.Contains(msg, "looking for beginning of value") {
		return true
	}
	for _, token := range []string{
		"http 502",
		"http 503",
		"http 504",
		"502 bad gateway",
		"503 service unavailable",
		"504 gateway timeout",
		"bad gateway",
		"service unavailable",
		"gateway timeout",
		"timed out",
		"timeout",
		"deadline exceeded",
		"unexpected end of json input",
		"unexpected eof",
		"connection reset",
		"connection closed",
		"connection force closed",
		"use of closed network connection",
		"response body closed",
	} {
		if strings.Contains(msg, token) {
			return true
		}
	}
	return false
}

type syncCacheEntry[V any] struct {
	done chan struct{}
	val  V
}

// syncCache deduplicates concurrent loads for the same key within a single run.
type syncCache[V any] struct {
	mu      sync.Mutex
	entries map[string]*syncCacheEntry[V]
}

// getOrLoad returns the cached value for key, or calls load once if absent.
// loaded is true only for the caller that executed load.
func (c *syncCache[V]) getOrLoad(key string, load func() V) (V, bool) {
	c.mu.Lock()
	if c.entries == nil {
		c.entries = make(map[string]*syncCacheEntry[V])
	}
	if entry, ok := c.entries[key]; ok {
		c.mu.Unlock()
		<-entry.done
		return entry.val, false
	}
	entry := &syncCacheEntry[V]{done: make(chan struct{})}
	c.entries[key] = entry
	c.mu.Unlock()

	entry.val = load()
	close(entry.done)
	return entry.val, true
}

// snapshot returns a copy of all completed entries.
func (c *syncCache[V]) snapshot() map[string]V {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make(map[string]V, len(c.entries))
	for k, e := range c.entries {
		select {
		case <-e.done:
			out[k] = e.val
		default:
		}
	}
	return out
}

// loadPool bounds concurrent PR loads and backs off on GitHub secondary
// rate-limit responses by reducing the worker limit and sleeping.
type loadPool struct {
	mu              sync.Mutex
	cond            *sync.Cond
	limit           int
	active          int
	backoffUntil    time.Time
	backoffDuration time.Duration
	backoffBase     time.Duration
	now             func() time.Time
	sleep           func(time.Duration)
}

func newLoadPool(limit int, backoffBase time.Duration, now func() time.Time, sleep func(time.Duration)) *loadPool {
	if limit <= 0 {
		limit = 1
	}
	if backoffBase <= 0 {
		backoffBase = defaultBackoffBase
	}
	if now == nil {
		now = time.Now
	}
	if sleep == nil {
		sleep = time.Sleep
	}
	p := &loadPool{
		limit:       limit,
		backoffBase: backoffBase,
		now:         now,
		sleep:       sleep,
	}
	p.cond = sync.NewCond(&p.mu)
	return p
}

// acquire waits for an available worker slot and any active rate-limit backoff.
// canStart is checked right before taking a slot; if it returns false, acquire
// returns false without incrementing active.
func (p *loadPool) acquire(canStart func() bool) bool {
	for {
		p.mu.Lock()
		for p.active >= p.limit {
			if canStart != nil && !canStart() {
				p.mu.Unlock()
				return false
			}
			p.cond.Wait()
		}
		wait := p.backoffUntil.Sub(p.now())
		if wait > 0 {
			p.mu.Unlock()
			p.sleep(wait)
			continue
		}
		if canStart != nil && !canStart() {
			p.mu.Unlock()
			return false
		}
		p.active++
		p.mu.Unlock()
		return true
	}
}

func (p *loadPool) release() {
	p.mu.Lock()
	p.active--
	p.cond.Broadcast()
	p.mu.Unlock()
}

func (p *loadPool) onSecondaryRateLimit() time.Duration {
	p.mu.Lock()
	if p.limit > 1 {
		p.limit--
	}
	if p.backoffDuration == 0 {
		p.backoffDuration = p.backoffBase
	} else {
		p.backoffDuration *= 2
		if p.backoffDuration > maxBackoffDuration {
			p.backoffDuration = maxBackoffDuration
		}
	}
	wait := p.backoffDuration
	until := p.now().Add(wait)
	if until.After(p.backoffUntil) {
		p.backoffUntil = until
	}
	p.cond.Broadcast()
	p.mu.Unlock()

	p.sleep(wait)
	return wait
}

// callWithRetry runs fn and retries on secondary rate-limit errors after
// backing off and reducing concurrency.
func (p *loadPool) callWithRetry(fn func() error) error {
	for attempt := 0; ; attempt++ {
		err := fn()
		if err == nil || p == nil || !isSecondaryRateLimit(err) || attempt >= maxRateLimitRetries {
			return err
		}
		p.onSecondaryRateLimit()
	}
}

// pauseTransient waits briefly before retrying a chunk that hit a transient
// gateway or timeout error.
func (p *loadPool) pauseTransient() {
	if p == nil {
		time.Sleep(defaultBackoffBase)
		return
	}
	p.sleep(p.backoffBase)
}
