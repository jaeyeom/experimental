package diskwatch

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// MountStatus is one mount as status prints it.
type MountStatus struct {
	Path            string    `json:"path"`
	FreeBytes       uint64    `json:"freeBytes"`
	FreePct         float64   `json:"freePct"`
	TotalBytes      uint64    `json:"totalBytes"`
	Armed           bool      `json:"armed"`
	Cooldown        bool      `json:"cooldown"`
	CooldownUntil   time.Time `json:"cooldownUntil,omitempty"`
	LastTrigger     time.Time `json:"lastTrigger,omitempty"`
	LastAction      string    `json:"lastAction,omitempty"`
	LastActionError string    `json:"lastActionError,omitempty"`
	Error           string    `json:"error,omitempty"`
}

// Status reads live free space and joins it with the saved hysteresis state.
func Status(cfg Config, state State, now time.Time, stat StatFS) []MountStatus {
	out := make([]MountStatus, 0, len(cfg.Mounts))
	for _, mount := range cfg.Mounts {
		st := state.mount(mount.Path)
		row := MountStatus{
			Path:            mount.Path,
			Armed:           st.Armed,
			LastTrigger:     st.TriggeredAt,
			LastAction:      st.LastAction,
			LastActionError: st.LastActionError,
		}
		if inCooldown(st, mount.Cooldown, now) {
			row.Cooldown = true
			row.CooldownUntil = st.TriggeredAt.Add(mount.Cooldown)
		}
		usage, err := stat(mount.Path)
		if err != nil {
			row.Error = err.Error()
			out = append(out, row)
			continue
		}
		row.FreeBytes = usage.Free
		row.TotalBytes = usage.Total
		row.FreePct = FreePct(usage.Free, usage.Total)
		out = append(out, row)
	}
	return out
}

// FormatStatus renders status as text or as one JSON object.
func FormatStatus(rows []MountStatus, asJSON bool) (string, error) {
	if asJSON {
		body, err := json.MarshalIndent(struct {
			Mounts []MountStatus `json:"mounts"`
		}{Mounts: rows}, "", "  ")
		if err != nil {
			return "", fmt.Errorf("encode status: %w", err)
		}
		return string(body) + "\n", nil
	}
	var b strings.Builder
	for _, row := range rows {
		arm := "disarmed"
		if row.Armed {
			arm = "armed"
		}
		cool := "ready"
		if row.Cooldown {
			cool = "cooldown"
		}
		trigger := "never"
		if !row.LastTrigger.IsZero() {
			trigger = row.LastTrigger.Format(time.RFC3339)
		}
		action := "none"
		if row.LastAction != "" {
			action = row.LastAction
		}
		if row.Error != "" {
			fmt.Fprintf(&b, "%s error %s\n", row.Path, row.Error)
			continue
		}
		fmt.Fprintf(&b, "%s free %.2f%% (%d/%d) %s %s last-trigger %s action %s\n",
			row.Path, row.FreePct, row.FreeBytes, row.TotalBytes, arm, cool, trigger, action)
		if row.LastActionError != "" {
			fmt.Fprintf(&b, "  action-error %s\n", row.LastActionError)
		}
	}
	return b.String(), nil
}
