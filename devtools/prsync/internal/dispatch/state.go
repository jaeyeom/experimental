package dispatch

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"math/rand/v2" // nosemgrep: go.lang.security.audit.crypto.math_random.math-random-used
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/google/renameio/v2"
	"golang.org/x/sys/unix"
)

// ErrCorruptState is returned when state_file exists but is not valid JSON.
var ErrCorruptState = errors.New("corrupt state file")

// ErrLock is returned when the state_file lock cannot be acquired.
var ErrLock = errors.New("state file lock not acquired")

type lockRetryConfig struct {
	MaxRetries    int
	InitialDelay  time.Duration
	MaxDelay      time.Duration
	BackoffFactor float64
}

func defaultLockRetry() lockRetryConfig {
	return lockRetryConfig{
		MaxRetries:    10,
		InitialDelay:  50 * time.Millisecond,
		MaxDelay:      2 * time.Second,
		BackoffFactor: 1.5,
	}
}

// heldLocks tracks lock files owned by this process (re-homed LockManager).
var heldLocks sync.Map

// State is the on-disk dedupe map keyed by owner/repo#N.
type State map[string]Entry

// Entry is one PR's last successful dispatch.
type Entry struct {
	DispatchedCommentIDs []string `json:"dispatched_comment_ids"`        //nolint:tagliatelle // brief outbound contract
	DispatchedHeadSHA    string   `json:"dispatched_head_sha,omitempty"` //nolint:tagliatelle // brief outbound contract
	DispatchedAt         string   `json:"dispatched_at"`                 //nolint:tagliatelle // brief outbound contract
}

// FileStore loads dispatch state from a JSON file.
type FileStore struct {
	Path string
}

// Load implements StateStore.
func (s FileStore) Load() (State, error) {
	return LoadFile(s.Path)
}

// Save implements StateStore.
func (s FileStore) Save(st State) error {
	return SaveFile(s.Path, st)
}

// AcquireLock creates state_file.lock with O_EXCL + PID. The returned function
// closes the file and removes the lock. Retry shape matches gh-nudge's
// DefaultFileLockConfig (~5s). A live holder returns ErrLock.
func (s FileStore) AcquireLock() (func(), error) {
	return acquireLockRetry(s.Path+".lock", defaultLockRetry())
}

// WithLock runs fn while holding the exclusive state lock.
func (s FileStore) WithLock(fn func() error) error {
	unlock, err := s.AcquireLock()
	if err != nil {
		return err
	}
	defer unlock()
	return fn()
}

func acquireLockRetry(lockPath string, cfg lockRetryConfig) (func(), error) {
	if err := os.MkdirAll(filepath.Dir(lockPath), 0o700); err != nil {
		return nil, fmt.Errorf("%w: create lock dir: %v", ErrLock, err)
	}
	var lastErr error
	delay := cfg.InitialDelay
	for attempt := 0; attempt <= cfg.MaxRetries; attempt++ {
		unlock, err := tryAcquireLock(lockPath)
		if err == nil {
			return unlock, nil
		}
		lastErr = err
		if alreadyHeld(err) {
			return nil, fmt.Errorf("%w: %v", ErrLock, err)
		}
		if isStaleLock(lockPath) {
			_ = os.Remove(lockPath)
			unlock, err := tryAcquireLock(lockPath)
			if err == nil {
				return unlock, nil
			}
			lastErr = err
		}
		if attempt < cfg.MaxRetries {
			jitter := 0.75 + rand.Float64()*0.5 //nolint:gosec // G404: lock retry jitter
			time.Sleep(time.Duration(float64(delay) * jitter))
			delay = time.Duration(float64(delay) * cfg.BackoffFactor)
			if delay > cfg.MaxDelay {
				delay = cfg.MaxDelay
			}
		}
	}
	return nil, fmt.Errorf("%w: %v", ErrLock, lastErr)
}

func tryAcquireLock(lockPath string) (func(), error) {
	if _, loaded := heldLocks.LoadOrStore(lockPath, struct{}{}); loaded {
		return nil, fmt.Errorf("lock already held for path: %s", lockPath)
	}
	file, err := os.OpenFile(lockPath, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o644) //nolint:gosec // lock metadata is not secret
	if err != nil {
		heldLocks.Delete(lockPath)
		return nil, fmt.Errorf("create lock file: %w", err)
	}
	lockInfo := fmt.Sprintf("locked_at: %s\npid: %d\n", time.Now().Format(time.RFC3339), os.Getpid())
	if _, err := fmt.Fprintf(file, "%s", lockInfo); err != nil {
		_ = file.Close()
		_ = os.Remove(lockPath)
		heldLocks.Delete(lockPath)
		return nil, fmt.Errorf("write lock file: %w", err)
	}
	return func() {
		_ = file.Close()
		_ = os.Remove(lockPath)
		heldLocks.Delete(lockPath)
	}, nil
}

func alreadyHeld(err error) bool {
	return err != nil && strings.Contains(err.Error(), "lock already held")
}

func isStaleLock(lockPath string) bool {
	content, err := os.ReadFile(lockPath) //nolint:gosec // lock path is state_file + ".lock"
	if err != nil {
		return false
	}
	var pid int
	for _, line := range strings.Split(string(content), "\n") {
		if strings.HasPrefix(line, "pid: ") {
			parsed, convErr := strconv.Atoi(strings.TrimPrefix(line, "pid: "))
			if convErr == nil {
				pid = parsed
				break
			}
		}
	}
	if pid == 0 {
		return false
	}
	return unix.Kill(pid, 0) == unix.ESRCH
}

// Deduped reports whether current comment IDs are a subset of the stored set.
func (s State) Deduped(key string, commentIDs []string) bool {
	if s == nil {
		return false
	}
	entry, ok := s[key]
	if !ok {
		return false
	}
	stored := make(map[string]struct{}, len(entry.DispatchedCommentIDs))
	for _, id := range entry.DispatchedCommentIDs {
		stored[id] = struct{}{}
	}
	for _, id := range commentIDs {
		if _, ok := stored[id]; !ok {
			return false
		}
	}
	return true
}

// Record replaces the stored comment-id set for key. It does not union.
// An existing head SHA is preserved so comment and rebase modes share state.
func (s State) Record(key string, commentIDs []string, at time.Time) {
	ids := append([]string(nil), commentIDs...)
	prev := s[key]
	s[key] = Entry{
		DispatchedCommentIDs: ids,
		DispatchedHeadSHA:    prev.DispatchedHeadSHA,
		DispatchedAt:         at.UTC().Format(time.RFC3339),
	}
}

// DedupedHead reports whether the current head SHA matches the last rebase send.
// An empty SHA is never treated as a match.
func (s State) DedupedHead(key, sha string) bool {
	if s == nil || sha == "" {
		return false
	}
	entry, ok := s[key]
	return ok && entry.DispatchedHeadSHA == sha
}

// RecordHead stores the head SHA for rebase dedupe. Comment IDs are preserved.
func (s State) RecordHead(key, sha string, at time.Time) {
	prev := s[key]
	s[key] = Entry{
		DispatchedCommentIDs: prev.DispatchedCommentIDs,
		DispatchedHeadSHA:    sha,
		DispatchedAt:         at.UTC().Format(time.RFC3339),
	}
}

// LoadFile reads state from path. A missing file is empty state.
func LoadFile(path string) (State, error) {
	data, err := os.ReadFile(path) //nolint:gosec // path is the configured state_file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return State{}, nil
		}
		return nil, fmt.Errorf("read state file: %w", err)
	}
	var st State
	if err := json.Unmarshal(data, &st); err != nil {
		return nil, fmt.Errorf("%w: %v", ErrCorruptState, err)
	}
	if st == nil {
		st = State{}
	}
	return st, nil
}

// SaveFile writes state atomically. Parent dirs are created with 0700.
func SaveFile(path string, st State) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	data, err := json.MarshalIndent(st, "", "  ")
	if err != nil {
		return fmt.Errorf("encode state: %w", err)
	}
	data = append(data, '\n')
	if err := renameio.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write state file: %w", err)
	}
	return nil
}
