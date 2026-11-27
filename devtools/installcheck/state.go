package installcheck

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

// State is what earlier runs saw and reported.
type State struct {
	LastRun        time.Time            `json:"lastRun,omitempty"`
	FailureNotedAt time.Time            `json:"failureNotedAt,omitempty"`
	Repos          map[string]RepoState `json:"repos,omitempty"`
}

// RepoState is one repository's last seen main head and the closed issues
// already reported (or deliberately baselined) for it.
type RepoState struct {
	Head     string `json:"head"`
	Reported []int  `json:"reported,omitempty"`
}

func (r RepoState) has(n int) bool {
	for _, x := range r.Reported {
		if x == n {
			return true
		}
	}
	return false
}

// LoadState reads the state file. A missing file is an empty state.
func LoadState(path string) (State, error) {
	if path == "" {
		return State{}, errors.New("state path is empty")
	}
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own state path
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

// SaveState writes the state atomically (temp file, fsync, rename) with mode 0600.
func SaveState(path string, st State) error {
	if path == "" {
		return errors.New("state path is empty")
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	data, err := json.MarshalIndent(st, "", "  ")
	if err != nil {
		return fmt.Errorf("encode state: %w", err)
	}
	if err := renameio.WriteFile(path, append(data, '\n'), 0o600); err != nil {
		return fmt.Errorf("write state: %w", err)
	}
	return nil
}
