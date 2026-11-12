// Package ghlimit is a per-user limiter for gh calls. Every process
// that runs gh through it shares lock files in one directory, so calls
// across tools run at most MaxConcurrent at once, mutations keep
// MutationGap apart, and a rate-limited call backs off and retries once.
//
// Limiter.Run takes the call as a Runner, so a layer such as a read
// cache can wrap or replace the Runner without touching the limiter.
package ghlimit

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"time"
)

// Environment variables that configure the limiter.
const (
	EnvMaxConcurrent = "GH_THROTTLE_MAX_CONCURRENT"
	EnvMutationGap   = "GH_THROTTLE_MUTATION_GAP"
	EnvBackoff       = "GH_THROTTLE_BACKOFF"
	EnvMaxBackoff    = "GH_THROTTLE_MAX_BACKOFF"
)

// Config holds the limiter knobs.
type Config struct {
	// MaxConcurrent is how many gh calls may run at once across all of
	// the user's processes.
	MaxConcurrent int
	// MutationGap is the least time between the end of one mutation and
	// the start of the next.
	MutationGap time.Duration
	// Backoff is the wait before retrying a rate-limited call when gh
	// output names no wait.
	Backoff time.Duration
	// MaxBackoff caps every backoff.
	MaxBackoff time.Duration
	// MaxWait is the longest wait for a lock. After it, the call runs
	// anyway, so a stuck holder cannot block everyone.
	MaxWait time.Duration
}

// DefaultConfig is one call at a time, mutations 1s apart, a 60s
// backoff capped at 5m, and a 10m lock wait.
func DefaultConfig() Config {
	return Config{
		MaxConcurrent: 1,
		MutationGap:   time.Second,
		Backoff:       time.Minute,
		MaxBackoff:    5 * time.Minute,
		MaxWait:       10 * time.Minute,
	}
}

// ConfigFromEnv is DefaultConfig with the GH_THROTTLE_* variables that
// getenv returns applied. A duration is a Go duration ("1500ms") or a
// number of seconds.
func ConfigFromEnv(getenv func(string) string) (Config, error) {
	cfg := DefaultConfig()
	if v := getenv(EnvMaxConcurrent); v != "" {
		n, err := strconv.Atoi(v)
		if err != nil || n < 1 {
			return cfg, fmt.Errorf("%s=%q: want a whole number of at least 1", EnvMaxConcurrent, v)
		}
		cfg.MaxConcurrent = n
	}
	for _, d := range []struct {
		name string
		dst  *time.Duration
	}{
		{EnvMutationGap, &cfg.MutationGap},
		{EnvBackoff, &cfg.Backoff},
		{EnvMaxBackoff, &cfg.MaxBackoff},
	} {
		v := getenv(d.name)
		if v == "" {
			continue
		}
		parsed, err := parseDuration(v)
		if err != nil {
			return cfg, fmt.Errorf("%s=%q: %w", d.name, v, err)
		}
		*d.dst = parsed
	}
	return cfg, nil
}

func parseDuration(v string) (time.Duration, error) {
	if secs, err := strconv.ParseFloat(v, 64); err == nil {
		if secs < 0 {
			return 0, fmt.Errorf("negative duration")
		}
		return time.Duration(secs * float64(time.Second)), nil
	}
	d, err := time.ParseDuration(v)
	if err != nil {
		return 0, fmt.Errorf("want a duration such as 1s or 500ms: %w", err)
	}
	if d < 0 {
		return 0, fmt.Errorf("negative duration")
	}
	return d, nil
}

// StateDir is the lock directory: $XDG_RUNTIME_DIR/gh-throttle, or
// gh-throttle under $TMPDIR when XDG_RUNTIME_DIR is unset.
func StateDir(getenv func(string) string) string {
	base := getenv("XDG_RUNTIME_DIR")
	if base == "" {
		base = os.TempDir()
	}
	return filepath.Join(base, "gh-throttle")
}

// LogPath is $XDG_STATE_HOME/gh-throttle/log.jsonl, with
// XDG_STATE_HOME defaulting to ~/.local/state.
func LogPath(getenv func(string) string) string {
	base := getenv("XDG_STATE_HOME")
	if base == "" {
		base = filepath.Join(getenv("HOME"), ".local", "state")
	}
	return filepath.Join(base, "gh-throttle", "log.jsonl")
}
