package loop

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

// Heartbeat records the running loop's PID, round number, and timestamps.
type Heartbeat struct {
	PID            int       `json:"pid"`
	ConfigPath     string    `json:"config_path,omitempty"` //nolint:tagliatelle // state file schema
	Round          int       `json:"round"`
	State          string    `json:"state"`
	StartedAt      time.Time `json:"started_at"`                 //nolint:tagliatelle // state file schema
	UpdatedAt      time.Time `json:"updated_at"`                 //nolint:tagliatelle // state file schema
	LastRoundStart time.Time `json:"last_round_start,omitempty"` //nolint:tagliatelle // state file schema
	LastRoundEnd   time.Time `json:"last_round_end,omitempty"`   //nolint:tagliatelle // state file schema
	LastGateResult string    `json:"last_gate_result,omitempty"` //nolint:tagliatelle // state file schema
	LastGateReason string    `json:"last_gate_reason,omitempty"` //nolint:tagliatelle // state file schema
	PauseReason    string    `json:"pause_reason,omitempty"`     //nolint:tagliatelle // state file schema
	PausedUntil    time.Time `json:"paused_until,omitempty"`     //nolint:tagliatelle // state file schema
}

// WriteHeartbeat atomically persists hb to path.
func WriteHeartbeat(path string, hb Heartbeat) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create heartbeat dir: %w", err)
	}
	data, err := json.MarshalIndent(hb, "", "  ")
	if err != nil {
		return fmt.Errorf("encode heartbeat: %w", err)
	}
	data = append(data, '\n')
	if err := renameio.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write heartbeat: %w", err)
	}
	return nil
}

// ReadHeartbeat loads a heartbeat JSON file. A missing file returns (zero, false, nil).
func ReadHeartbeat(path string) (Heartbeat, bool, error) {
	data, err := os.ReadFile(path) //nolint:gosec // path is a loop heartbeat file in state dir
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return Heartbeat{}, false, nil
		}
		return Heartbeat{}, false, fmt.Errorf("read heartbeat: %w", err)
	}
	var hb Heartbeat
	if err := json.Unmarshal(data, &hb); err != nil {
		return Heartbeat{}, true, fmt.Errorf("decode heartbeat: %w", err)
	}
	return hb, true, nil
}

// RemoveHeartbeat removes path when the loop exits cleanly.
func RemoveHeartbeat(path string) error {
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("remove heartbeat: %w", err)
	}
	return nil
}

func (hb Heartbeat) freshTimestamp() time.Time {
	switch {
	case !hb.UpdatedAt.IsZero():
		return hb.UpdatedAt
	case !hb.LastRoundStart.IsZero():
		return hb.LastRoundStart
	default:
		return hb.StartedAt
	}
}
