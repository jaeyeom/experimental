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
	State          string    `json:"state"`
	PID            int       `json:"pid,omitempty"`
	Round          int       `json:"round,omitempty"`
	HeartbeatPath  string    `json:"heartbeat_path,omitempty"`   //nolint:tagliatelle // state schema
	ConfigPath     string    `json:"config_path,omitempty"`      //nolint:tagliatelle // state schema
	UpdatedAt      time.Time `json:"updated_at,omitempty"`       //nolint:tagliatelle // state schema
	LastRoundEnd   time.Time `json:"last_round_end,omitempty"`   //nolint:tagliatelle // state schema
	LastGateResult string    `json:"last_gate_result,omitempty"` //nolint:tagliatelle // state schema
	LastGateReason string    `json:"last_gate_reason,omitempty"` //nolint:tagliatelle // state schema
	PauseReason    string    `json:"pause_reason,omitempty"`     //nolint:tagliatelle // state schema
	PausedUntil    time.Time `json:"paused_until,omitempty"`     //nolint:tagliatelle // state schema
	Detail         string    `json:"detail"`
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
		prefix := StateRunning
		detailParts := []string{
			fmt.Sprintf("pid %d", hb.PID),
			fmt.Sprintf("round %d", hb.Round),
			fmt.Sprintf("updated %s ago", age),
		}
		if hb.State == StatePaused {
			detailParts = append([]string{"paused"}, detailParts...)
		}
		detailParts = append(detailParts, gateStatusParts(hb)...)
		return StatusReport{
			State:          prefix,
			PID:            hb.PID,
			Round:          hb.Round,
			HeartbeatPath:  path,
			ConfigPath:     hb.ConfigPath,
			UpdatedAt:      hb.UpdatedAt,
			LastRoundEnd:   hb.LastRoundEnd,
			LastGateResult: hb.LastGateResult,
			LastGateReason: hb.LastGateReason,
			PauseReason:    hb.PauseReason,
			PausedUntil:    hb.PausedUntil,
			Detail:         fmt.Sprintf("%s (%s)", prefix, joinDetailParts(detailParts)),
		}, true
	}
	reason := fmt.Sprintf("updated %s ago", age)
	if !alive {
		reason = fmt.Sprintf("pid %d dead, updated %s ago", hb.PID, age)
	}
	staleParts := []string{
		fmt.Sprintf("pid %d", hb.PID),
		fmt.Sprintf("round %d", hb.Round),
		reason,
	}
	staleParts = append(staleParts, gateStatusParts(hb)...)
	return StatusReport{
		State:          StateStale,
		PID:            hb.PID,
		Round:          hb.Round,
		HeartbeatPath:  path,
		ConfigPath:     hb.ConfigPath,
		UpdatedAt:      hb.UpdatedAt,
		LastRoundEnd:   hb.LastRoundEnd,
		LastGateResult: hb.LastGateResult,
		LastGateReason: hb.LastGateReason,
		PauseReason:    hb.PauseReason,
		PausedUntil:    hb.PausedUntil,
		Detail:         fmt.Sprintf("%s (%s)", StateStale, joinDetailParts(staleParts)),
	}, true
}

func gateStatusParts(hb Heartbeat) []string {
	var parts []string
	if hb.LastGateResult != "" {
		parts = append(parts, "last_gate="+hb.LastGateResult)
	}
	if hb.PauseReason != "" {
		parts = append(parts, fmt.Sprintf("pause_reason=%q", hb.PauseReason))
	} else if hb.LastGateReason != "" {
		parts = append(parts, fmt.Sprintf("gate_reason=%q", hb.LastGateReason))
	}
	if !hb.PausedUntil.IsZero() {
		parts = append(parts, "paused_until="+hb.PausedUntil.UTC().Format(time.RFC3339))
	}
	return parts
}

func joinDetailParts(parts []string) string {
	out := ""
	for i, p := range parts {
		if i > 0 {
			out += ", "
		}
		out += p
	}
	return out
}
