package ghlimit

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// ReasonCooldown is the wait reason logged when a caller waits on an
// active account-wide cooldown before taking a slot.
const ReasonCooldown = "cooldown"

const (
	cooldownFile = "cooldown.json"
	cooldownLock = "cooldown.lock"
)

type cooldownState struct {
	Until    time.Time     `json:"until"`
	Duration time.Duration `json:"duration"`
	Updated  time.Time     `json:"updated"`
	Cause    string        `json:"cause,omitempty"`
	Hits     int           `json:"hits,omitempty"`
}

type cooldownHit struct {
	wait     time.Duration
	duration time.Duration
	cause    string
}

func (l *Limiter) readCooldown() (cooldownState, bool) {
	raw, err := os.ReadFile(filepath.Join(l.Dir, cooldownFile))
	if err != nil {
		return cooldownState{}, false
	}
	var s cooldownState
	if json.Unmarshal(raw, &s) != nil {
		return cooldownState{}, false
	}
	return s, true
}

// waitCooldown waits until any active account-wide cooldown expires
// before taking a slot. It re-checks after sleeping in case another
// caller extended the cooldown while it slept.
func (l *Limiter) waitCooldown(ctx context.Context) (wait, bool, error) {
	var total time.Duration
	waited := false
	for {
		s, ok := l.readCooldown()
		if !ok {
			return wait{ReasonCooldown, total}, waited, nil
		}
		d := s.Until.Sub(l.now())
		if d <= 0 {
			return wait{ReasonCooldown, total}, waited, nil
		}
		if err := l.sleepCtx(ctx, d); err != nil {
			return wait{ReasonCooldown, total}, waited, err
		}
		total += d
		waited = true
	}
}

// preflightCooldownCheck fails fast with ExitRateLimited and
// RateLimitMarker when a preflight call runs during an active cooldown.
func (l *Limiter) preflightCooldownCheck(args []string, info CallInfo) (Result, bool, error) {
	s, ok := l.readCooldown()
	if !ok {
		return Result{}, false, nil
	}
	left := s.Until.Sub(l.now())
	if left <= 0 {
		return Result{}, false, nil
	}
	rem := left.Round(time.Second)
	if left < time.Second {
		rem = left.Round(time.Millisecond)
	}
	msg := fmt.Sprintf("gh-throttle: rate limit cooldown active (%s remaining; not an auth failure) [%s]\n", rem, RateLimitMarker)
	res := Result{ExitCode: ExitRateLimited, Stderr: []byte(msg)}
	l.logCall(args, info, l.now(), 0, res, 0, 0, 0, "cooldown_active")
	return res, true, nil
}

// recordCooldown records or extends the shared account-wide cooldown
// after a rate-limited response. The first hit uses Retry-After or
// x-ratelimit-reset else Backoff; further hits during the cooldown
// double the duration up to MaxBackoff.
func (l *Limiter) recordCooldown(ctx context.Context, args []string, res Result, wasInCooldown bool) (time.Duration, string, error) {
	cause := RateLimitCause(args, string(res.Stderr), string(res.StdoutHead))
	if cause == "" {
		cause = "rate_limit"
	}
	var (
		waitDur time.Duration
		setDur  time.Duration
	)
	err := withLockedJSON(ctx, l, cooldownLock, cooldownFile, func(s *cooldownState, now time.Time) bool {
		retryDur, hasRetry := RetryAfter(string(res.Stderr)+"\n"+string(res.StdoutHead), now)
		if hasRetry {
			retryDur = clampDuration(retryDur, l.Config.MaxBackoff)
		}
		d, hits := nextCooldownDuration(*s, now, l.Config, retryDur, hasRetry, wasInCooldown)
		until := now.Add(d)
		if until.Before(s.Until) {
			until = s.Until
		}
		s.Duration = d
		s.Until = until
		s.Updated = now
		s.Cause = cause
		s.Hits = hits
		setDur = d
		waitDur = max(until.Sub(now), 0)
		return true
	})
	if err != nil {
		// Fall back to local backoff if locking fails.
		d := l.backoff(res)
		return d, cause, err
	}
	_ = setDur
	return waitDur, cause, nil
}

func nextCooldownDuration(s cooldownState, now time.Time, cfg Config, retryDur time.Duration, hasRetry, wasInCooldown bool) (time.Duration, int) {
	during := s.Hits > 0 && (wasInCooldown || now.Before(s.Until))
	if !during {
		d := cfg.Backoff
		if hasRetry {
			d = retryDur
		}
		return clampDuration(d, cfg.MaxBackoff), 1
	}
	base := s.Duration
	if base <= 0 {
		base = cfg.Backoff
	}
	d := base * 2
	if hasRetry && retryDur > d {
		d = retryDur
	}
	return clampDuration(d, cfg.MaxBackoff), s.Hits + 1
}

func clampDuration(d, maxDur time.Duration) time.Duration {
	return max(min(d, maxDur), 0)
}

func (l *Limiter) clearCooldownIfExpired(ctx context.Context) {
	path := filepath.Join(l.Dir, cooldownFile)
	if _, err := os.Stat(path); errors.Is(err, fs.ErrNotExist) {
		return
	}
	unlock, _, err := l.acquire(ctx, []string{cooldownLock})
	if err != nil {
		return
	}
	defer unlock()
	s, ok := l.readCooldown()
	if !ok || !l.now().Before(s.Until) {
		_ = os.Remove(path)
	}
}

func ensurePreflightMarker(res *Result) {
	res.ExitCode = ExitRateLimited
	if strings.Contains(string(res.Stderr), RateLimitMarker) {
		return
	}
	suffix := fmt.Sprintf("gh-throttle: rate limit hit (not an auth failure) [%s]\n", RateLimitMarker)
	if len(res.Stderr) > 0 && res.Stderr[len(res.Stderr)-1] != '\n' {
		res.Stderr = append(res.Stderr, '\n')
	}
	res.Stderr = append(res.Stderr, suffix...)
}
