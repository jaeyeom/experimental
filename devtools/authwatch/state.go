package authwatch

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

// State is the persisted per-profile state in state.json.
// Other tools may read it to inspect login health without touching credentials.
type State struct {
	Profiles map[string]ProfileState `json:"profiles"`
}

// ProfileState records one profile's current level, transition time, and expiries.
// It never contains token values.
type ProfileState struct {
	Level                Level     `json:"level"`
	Since                time.Time `json:"since,omitempty"`
	AccessExpiresAt      time.Time `json:"accessExpiresAt,omitempty"`
	RefreshExpiresAt     time.Time `json:"refreshExpiresAt,omitempty"`
	LastCheck            time.Time `json:"lastCheck,omitempty"`
	LastNotifiedLevel    Level     `json:"lastNotifiedLevel,omitempty"`
	LastNotifiedAt       time.Time `json:"lastNotifiedAt,omitempty"`
	LastNotifyError      string    `json:"lastNotifyError,omitempty"`
	FormatError          string    `json:"formatError,omitempty"`
	ReportedFailureAt    time.Time `json:"reportedFailureAt,omitempty"`
	ReportedAccessExpiry time.Time `json:"reportedAccessExpiry,omitempty"`
}

func (s State) profile(name string) ProfileState {
	if s.Profiles == nil {
		return ProfileState{}
	}
	return s.Profiles[name]
}

func (s *State) put(name string, st ProfileState) {
	if s.Profiles == nil {
		s.Profiles = make(map[string]ProfileState)
	}
	s.Profiles[name] = st
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

// SaveState atomically writes state.json.
func SaveState(path string, state State) error {
	if state.Profiles == nil {
		state.Profiles = map[string]ProfileState{}
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
