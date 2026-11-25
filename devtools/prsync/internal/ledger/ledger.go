// Package ledger implements a human-readable, queryable append-only dispatch
// ledger (JSON Lines) recording PR, head SHA, kind, time, and outcome.
package ledger

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

// Outcome constants recorded in the dispatch ledger.
const (
	OutcomeDispatched = "dispatched"
	OutcomeTimeout    = "timeout"
	OutcomeBlocked    = "blocked"
	OutcomeDryRun     = "dry-run"
	OutcomeSkipped    = "skipped"
	OutcomeFailed     = "failed"
)

// Entry is one dispatch ledger record.
type Entry struct {
	PR      string    `json:"pr"`
	HeadSHA string    `json:"head_sha"` //nolint:tagliatelle // ledger wire format
	Kind    string    `json:"kind"`
	Time    time.Time `json:"time"`
	Outcome string    `json:"outcome"`
	Detail  string    `json:"detail,omitempty"`
}

// Filter narrows ledger query results. Zero fields match everything.
type Filter struct {
	PR      string
	HeadSHA string
	Kind    string
	Outcome string
	Since   time.Time
	Limit   int
}

// Store is an append-only JSON Lines dispatch ledger on disk (or in-memory when
// Path is empty).
type Store struct {
	Path string
	mu   sync.Mutex
	mem  []Entry
}

// Append writes one entry to the ledger.
func (s *Store) Append(e Entry) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if e.Time.IsZero() {
		e.Time = time.Now().UTC()
	} else {
		e.Time = e.Time.UTC()
	}
	if s.Path == "" {
		s.mem = append(s.mem, e)
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(s.Path), 0o700); err != nil {
		return fmt.Errorf("create ledger dir: %w", err)
	}
	line, err := json.Marshal(e)
	if err != nil {
		return fmt.Errorf("encode ledger entry: %w", err)
	}
	f, err := os.OpenFile(s.Path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o600) //nolint:gosec // configured ledger path
	if err != nil {
		return fmt.Errorf("open ledger: %w", err)
	}
	defer f.Close() //nolint:errcheck
	if _, err := f.Write(append(line, '\n')); err != nil {
		return fmt.Errorf("write ledger entry: %w", err)
	}
	return nil
}

// Load returns all entries in chronological order. A missing file returns an empty slice.
func (s *Store) Load() ([]Entry, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.loadLocked()
}

func (s *Store) loadLocked() ([]Entry, error) {
	if s.Path == "" {
		out := make([]Entry, len(s.mem))
		copy(out, s.mem)
		return out, nil
	}
	f, err := os.Open(s.Path) //nolint:gosec // configured ledger path
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return []Entry{}, nil
		}
		return nil, fmt.Errorf("open ledger: %w", err)
	}
	defer f.Close() //nolint:errcheck
	var entries []Entry
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	lineNo := 0
	for sc.Scan() {
		lineNo++
		text := strings.TrimSpace(sc.Text())
		if text == "" {
			continue
		}
		var e Entry
		if err := json.Unmarshal([]byte(text), &e); err != nil {
			return nil, fmt.Errorf("decode ledger line %d: %w", lineNo, err)
		}
		entries = append(entries, e)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("scan ledger: %w", err)
	}
	return entries, nil
}

// Query returns entries matching filter. When Limit > 0, returns the last Limit
// matching entries.
func (s *Store) Query(f Filter) ([]Entry, error) {
	entries, err := s.Load()
	if err != nil {
		return nil, err
	}
	out := make([]Entry, 0, len(entries))
	for _, e := range entries {
		if f.PR != "" && e.PR != f.PR {
			continue
		}
		if f.HeadSHA != "" && e.HeadSHA != f.HeadSHA {
			continue
		}
		if f.Kind != "" && e.Kind != f.Kind {
			continue
		}
		if f.Outcome != "" && e.Outcome != f.Outcome {
			continue
		}
		if !f.Since.IsZero() && e.Time.Before(f.Since) {
			continue
		}
		out = append(out, e)
	}
	if f.Limit > 0 && len(out) > f.Limit {
		out = out[len(out)-f.Limit:]
	}
	return out, nil
}

// HasDispatched reports whether (pr, headSHA, kind) already has a dispatched
// entry in the ledger. An empty headSHA never matches.
func (s *Store) HasDispatched(pr, headSHA, kind string) (bool, error) {
	if headSHA == "" {
		return false, nil
	}
	entries, err := s.Load()
	if err != nil {
		return false, err
	}
	for _, e := range entries {
		if e.PR == pr && e.HeadSHA == headSHA && e.Kind == kind && countsAsDispatched(e.Outcome) {
			return true, nil
		}
	}
	return false, nil
}

// CountToday returns how many live dispatches occurred on the same UTC date as now.
func (s *Store) CountToday(now time.Time) (int, error) {
	entries, err := s.Load()
	if err != nil {
		return 0, err
	}
	y, m, d := now.UTC().Date()
	count := 0
	for _, e := range entries {
		if !countsTowardDailyCap(e.Outcome) {
			continue
		}
		ey, em, ed := e.Time.UTC().Date()
		if ey == y && em == m && ed == d {
			count++
		}
	}
	return count, nil
}

func countsAsDispatched(outcome string) bool {
	return outcome == OutcomeDispatched || outcome == OutcomeTimeout
}

func countsTowardDailyCap(outcome string) bool {
	return outcome == OutcomeDispatched || outcome == OutcomeTimeout || outcome == OutcomeBlocked
}

// FormatLines renders entries as one human-readable line per entry.
func FormatLines(entries []Entry) string {
	var b strings.Builder
	for _, e := range entries {
		sha := e.HeadSHA
		if len(sha) > 7 {
			sha = sha[:7]
		}
		line := fmt.Sprintf("%s  %-24s  %-10s  %-7s  %s",
			e.Time.UTC().Format(time.RFC3339),
			e.PR,
			e.Kind,
			sha,
			e.Outcome,
		)
		if e.Detail != "" {
			line += "  " + e.Detail
		}
		b.WriteString(line)
		b.WriteByte('\n')
	}
	return b.String()
}
