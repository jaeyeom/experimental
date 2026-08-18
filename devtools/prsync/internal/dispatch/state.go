package dispatch

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"github.com/google/renameio/v2"
)

// ErrCorruptState is returned when state_file exists but is not valid JSON.
var ErrCorruptState = errors.New("corrupt state file")

// State is the on-disk dedupe map keyed by owner/repo#N.
type State map[string]Entry

// Entry is one PR's last successful dispatch.
type Entry struct {
	DispatchedCommentIDs []string `json:"dispatched_comment_ids"` //nolint:tagliatelle // brief outbound contract
	DispatchedAt         string   `json:"dispatched_at"`          //nolint:tagliatelle // brief outbound contract
}

// FileStore loads dispatch state from a JSON file.
type FileStore struct {
	Path string
}

// Load implements StateStore.
func (s FileStore) Load() (State, error) {
	return LoadFile(s.Path)
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
func (s State) Record(key string, commentIDs []string, at time.Time) {
	ids := append([]string(nil), commentIDs...)
	s[key] = Entry{
		DispatchedCommentIDs: ids,
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
