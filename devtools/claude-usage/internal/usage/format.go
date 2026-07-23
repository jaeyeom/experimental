package usage

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"time"
)

// FormatHuman writes the stable human-readable usage table.
//
//	plan: Team
//	  5-hour    2%   resets 2026-07-24 03:29 UTC
//	  7-day    12%   resets 2026-07-27 22:59 UTC
func FormatHuman(w io.Writer, d *Data) error {
	if d == nil {
		return fmt.Errorf("usage data is nil")
	}
	plan := d.PlanName
	if plan == "" {
		plan = "unknown"
	}
	if err := writef(w, "plan: %s\n", plan); err != nil {
		return err
	}
	if err := writef(w, "  5-hour  %s   resets %s\n", pct(d.FiveHour), when(d.FiveHourResetAt)); err != nil {
		return err
	}
	return writef(w, "  7-day   %s   resets %s\n", pct(d.SevenDay), when(d.SevenDayResetAt))
}

// FormatJSON writes pretty-printed JSON matching the stable field contract.
func FormatJSON(w io.Writer, d *Data) error {
	if d == nil {
		return fmt.Errorf("usage data is nil")
	}
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	if err := enc.Encode(d); err != nil {
		return fmt.Errorf("encode usage json: %w", err)
	}
	return nil
}

func writef(w io.Writer, format string, args ...any) error {
	if _, err := fmt.Fprintf(w, format, args...); err != nil {
		return fmt.Errorf("write usage output: %w", err)
	}
	return nil
}

func pct(v *int) string {
	if v == nil {
		return "  ?"
	}
	return fmt.Sprintf("%3d%%", *v)
}

func when(t *time.Time) string {
	if t == nil {
		return "n/a"
	}
	// Match ~/.local/bin/claude-usage: "YYYY-MM-DD HH:MM UTC"
	return t.UTC().Format("2006-01-02 15:04 UTC")
}

// Int returns a pointer to v (helper for tests and callers).
func Int(v int) *int { return &v }

// Time returns a pointer to t.
func Time(t time.Time) *time.Time { return &t }

// String reports a multi-line human summary.
func (d *Data) String() string {
	if d == nil {
		return "<nil>"
	}
	var b strings.Builder
	_ = FormatHuman(&b, d)
	return strings.TrimSpace(b.String())
}
