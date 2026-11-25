package secretwatch

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"time"

	"github.com/google/renameio/v2"
)

const maxSeenCursors = 10000

// State is the persisted cursor and deduplication state in state.json.
// It never contains secret values or journal messages.
type State struct {
	Journals    map[JournalKind]JournalState `json:"journals"`
	SeenCursors map[string]time.Time         `json:"seenCursors,omitempty"`
}

// JournalState records scan progress for one journal.
type JournalState struct {
	Cursor   string    `json:"cursor,omitempty"`
	LastScan time.Time `json:"lastScan,omitempty"`
}

func (s State) journal(kind JournalKind) JournalState {
	if s.Journals == nil {
		return JournalState{}
	}
	return s.Journals[kind]
}

func (s *State) setJournal(kind JournalKind, js JournalState) {
	if s.Journals == nil {
		s.Journals = make(map[JournalKind]JournalState)
	}
	s.Journals[kind] = js
}

// HasSeenCursor reports whether an alert for cursor has already been recorded.
func (s State) HasSeenCursor(cursor string) bool {
	if cursor == "" || s.SeenCursors == nil {
		return false
	}
	_, ok := s.SeenCursors[cursor]
	return ok
}

// MarkCursorSeen records cursor as alerted at timestamp at, capping the stored
// cursor map to prevent unbounded state growth.
func (s *State) MarkCursorSeen(cursor string, at time.Time) {
	if cursor == "" {
		return
	}
	if s.SeenCursors == nil {
		s.SeenCursors = make(map[string]time.Time)
	}
	s.SeenCursors[cursor] = at.UTC()
	pruneSeenCursors(s.SeenCursors, maxSeenCursors)
}

func pruneSeenCursors(seen map[string]time.Time, limit int) {
	if len(seen) <= limit {
		return
	}
	type entry struct {
		cursor string
		at     time.Time
	}
	items := make([]entry, 0, len(seen))
	for c, t := range seen {
		items = append(items, entry{cursor: c, at: t})
	}
	sort.Slice(items, func(i, j int) bool {
		if items[i].at.Equal(items[j].at) {
			return items[i].cursor < items[j].cursor
		}
		return items[i].at.Before(items[j].at)
	})
	excess := len(items) - limit
	for i := 0; i < excess; i++ {
		delete(seen, items[i].cursor)
	}
}

// LoadState reads state.json. A missing file returns an empty State.
func LoadState(path string) (State, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: user's own state path
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return State{}, nil
		}
		return State{}, fmt.Errorf("read state: %w", err)
	}
	var st State
	if err := json.Unmarshal(data, &st); err != nil {
		return State{}, fmt.Errorf("parse state: %w", err)
	}
	return st, nil
}

// SaveState atomically writes state.json with mode 0600.
func SaveState(path string, state State) error {
	if state.Journals == nil {
		state.Journals = map[JournalKind]JournalState{}
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	data, err := json.MarshalIndent(state, "", "  ")
	if err != nil {
		return fmt.Errorf("encode state: %w", err)
	}
	data = append(data, '\n')
	if err := renameio.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write state: %w", err)
	}
	return nil
}
