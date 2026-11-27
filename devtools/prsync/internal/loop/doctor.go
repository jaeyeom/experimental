package loop

import (
	"fmt"
	"path/filepath"
	"time"
)

// Loop health states reported by Inspect.
const (
	StateRunning    = "running"
	StateStale      = "stale heartbeat"
	StateNotRunning = "not running"
)

// StatusReport summarizes the loop state for `prsync doctor`.
type StatusReport struct {
	State         string    `json:"state"`
	PID           int       `json:"pid,omitempty"`
	Round         int       `json:"round,omitempty"`
	HeartbeatPath string    `json:"heartbeat_path,omitempty"` //nolint:tagliatelle // state schema
	ConfigPath    string    `json:"config_path,omitempty"`    //nolint:tagliatelle // state schema
	UpdatedAt     time.Time `json:"updated_at,omitempty"`     //nolint:tagliatelle // state schema
	LastRoundEnd  time.Time `json:"last_round_end,omitempty"` //nolint:tagliatelle // state schema
	Detail        string    `json:"detail"`
}

// Inspect reads heartbeat state in stateDir for configPath and reports whether
// the loop is running, has a stale heartbeat, or is not running. When
// explicitConfig is false and the exact heartbeat path is absent, any
// loop*.heartbeat.json file in stateDir is inspected so default `prsync doctor`
// invocations detect custom-config loops sharing the default state directory.
func Inspect(stateDir, configPath string, explicitConfig bool, now time.Time, staleAfter time.Duration, isAlive func(int) bool) StatusReport {
	if staleAfter <= 0 {
		staleAfter = DefaultStaleAfter
	}
	if isAlive == nil {
		isAlive = ProcessAlive
	}
	primary := HeartbeatPath(stateDir, configPath)
	if rep, ok := inspectFile(primary, now, staleAfter, isAlive); ok {
		return rep
	}
	if !explicitConfig {
		matches, err := filepath.Glob(filepath.Join(stateDir, "loop*.heartbeat.json"))
		if err == nil {
			var stale *StatusReport
			for _, m := range matches {
				if m == primary {
					continue
				}
				rep, ok := inspectFile(m, now, staleAfter, isAlive)
				if !ok {
					continue
				}
				if rep.State == StateRunning {
					return rep
				}
				if stale == nil {
					r := rep
					stale = &r
				}
			}
			if stale != nil {
				return *stale
			}
		}
	}
	return StatusReport{
		State:  StateNotRunning,
		Detail: StateNotRunning,
	}
}

func inspectFile(path string, now time.Time, staleAfter time.Duration, isAlive func(int) bool) (StatusReport, bool) {
	hb, exists, err := ReadHeartbeat(path)
	if !exists {
		return StatusReport{}, false
	}
	if err != nil {
		return StatusReport{
			State:         StateStale,
			HeartbeatPath: path,
			Detail:        fmt.Sprintf("%s (unreadable %s: %v)", StateStale, path, err),
		}, true
	}
	alive := isAlive(hb.PID)
	ts := hb.freshTimestamp()
	age := now.UTC().Sub(ts).Round(time.Second)
	if age < 0 {
		age = 0
	}
	if isHealthy(hb, now.UTC(), staleAfter, isAlive) {
		return StatusReport{
			State:         StateRunning,
			PID:           hb.PID,
			Round:         hb.Round,
			HeartbeatPath: path,
			ConfigPath:    hb.ConfigPath,
			UpdatedAt:     hb.UpdatedAt,
			LastRoundEnd:  hb.LastRoundEnd,
			Detail:        fmt.Sprintf("%s (pid %d, round %d, updated %s ago)", StateRunning, hb.PID, hb.Round, age),
		}, true
	}
	reason := fmt.Sprintf("updated %s ago", age)
	if !alive {
		reason = fmt.Sprintf("pid %d dead, updated %s ago", hb.PID, age)
	}
	return StatusReport{
		State:         StateStale,
		PID:           hb.PID,
		Round:         hb.Round,
		HeartbeatPath: path,
		ConfigPath:    hb.ConfigPath,
		UpdatedAt:     hb.UpdatedAt,
		LastRoundEnd:  hb.LastRoundEnd,
		Detail:        fmt.Sprintf("%s (pid %d, round %d, %s)", StateStale, hb.PID, hb.Round, reason),
	}, true
}
