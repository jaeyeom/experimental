package diskwatch

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

// State is the hysteresis memory for every mount, plus the last hook time.
type State struct {
	Mounts map[string]MountState `json:"mounts,omitempty"`
	HookAt time.Time             `json:"hookAt,omitempty"`
}

// MountState is one mount's arm, cooldown, and last action.
type MountState struct {
	Armed           bool      `json:"armed"`
	TriggeredAt     time.Time `json:"triggeredAt,omitempty"`
	LastAction      string    `json:"lastAction,omitempty"`
	LastActionError string    `json:"lastActionError,omitempty"`
	LastFreeBytes   uint64    `json:"lastFreeBytes,omitempty"`
	LastTotalBytes  uint64    `json:"lastTotalBytes,omitempty"`
	LastCheck       time.Time `json:"lastCheck,omitempty"`
}

func (s State) mount(path string) MountState {
	if s.Mounts == nil {
		return MountState{Armed: true}
	}
	st, ok := s.Mounts[path]
	if !ok {
		return MountState{Armed: true}
	}
	return st
}

func (s *State) put(path string, st MountState) {
	if s.Mounts == nil {
		s.Mounts = make(map[string]MountState)
	}
	s.Mounts[path] = st
}

// LoadState reads state.json. A missing file is an empty state.
func LoadState(path string) (State, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own state path
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return State{}, nil
		}
		return State{}, fmt.Errorf("read state: %w", err)
	}
	var state State
	if err := json.Unmarshal(data, &state); err != nil {
		return State{}, fmt.Errorf("parse state: %w", err)
	}
	return state, nil
}

// SaveState writes state.json by writing a temp file and renaming it.
func SaveState(path string, state State) error {
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

// decide says whether this observation should run on_low.
// Below urgent, cooldown and the disarmed state do not suppress a trigger.
// Otherwise a trigger requires the mount to be armed, free space below low,
// and the cooldown to have elapsed.
func decide(free, total uint64, mount Mount, st MountState, now time.Time) (bool, string) {
	if mount.Urgent != nil && mount.Urgent.Below(free, total) {
		return true, "urgent"
	}
	if !st.Armed || !mount.Low.Below(free, total) {
		return false, ""
	}
	if !st.TriggeredAt.IsZero() && now.Sub(st.TriggeredAt) < mount.Cooldown {
		return false, ""
	}
	return true, "low"
}

func rearm(free, total uint64, mount Mount, st MountState) MountState {
	if mount.High.Above(free, total) {
		st.Armed = true
	}
	return st
}

func inCooldown(st MountState, cooldown time.Duration, now time.Time) bool {
	return !st.TriggeredAt.IsZero() && now.Sub(st.TriggeredAt) < cooldown
}
