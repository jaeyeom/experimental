package usage

import (
	"encoding/json"
	"fmt"
	"io"
	"math"
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

type quotapaceLine struct {
	Window   string     `json:"window"`
	Percent  float64    `json:"percent"`
	ResetsAt time.Time  `json:"resetsAt"`
	Source   string     `json:"source,omitempty"`
	At       *time.Time `json:"at,omitempty"`
}

// FormatQuotapace writes one JSON line per window in `quotapace record --stdin` format.
// Returns ErrNoData when neither window has both utilization and reset time.
func FormatQuotapace(w io.Writer, d *Data) error {
	if d == nil {
		return fmt.Errorf("usage data is nil")
	}
	var at *time.Time
	if d.FetchedAt != nil && !d.FetchedAt.IsZero() {
		t := d.FetchedAt.UTC()
		at = &t
	}
	windows := []struct {
		name    string
		percent *float64
		resets  *time.Time
	}{
		{name: "5h", percent: d.FiveHour, resets: d.FiveHourResetAt},
		{name: "7d", percent: d.SevenDay, resets: d.SevenDayResetAt},
	}
	enc := json.NewEncoder(w)
	wrote := 0
	for _, win := range windows {
		if win.percent == nil || win.resets == nil || win.resets.IsZero() {
			continue
		}
		line := quotapaceLine{
			Window:   win.name,
			Percent:  *win.percent,
			ResetsAt: win.resets.UTC(),
			Source:   d.Source,
			At:       at,
		}
		if err := enc.Encode(line); err != nil {
			return fmt.Errorf("encode quotapace line: %w", err)
		}
		wrote++
	}
	if wrote == 0 {
		return ErrNoData
	}
	return nil
}

func writef(w io.Writer, format string, args ...any) error {
	if _, err := fmt.Fprintf(w, format, args...); err != nil {
		return fmt.Errorf("write usage output: %w", err)
	}
	return nil
}

func pct(v *float64) string {
	if v == nil {
		return "  ?"
	}
	return fmt.Sprintf("%3d%%", int(math.Round(*v)))
}

func when(t *time.Time) string {
	if t == nil {
		return "n/a"
	}
	// Match ~/.local/bin/claude-usage: "YYYY-MM-DD HH:MM UTC"
	return t.UTC().Format("2006-01-02 15:04 UTC")
}

// Float returns a pointer to v (helper for tests and callers).
func Float(v float64) *float64 { return &v }

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
