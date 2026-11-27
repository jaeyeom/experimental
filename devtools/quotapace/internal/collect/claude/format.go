package claude

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

type windowSnapshot struct {
	name     string
	percent  float64
	resetsAt time.Time
}

func completeWindows(d *Data) []windowSnapshot {
	if d == nil {
		return nil
	}
	candidates := []struct {
		name    string
		percent *float64
		resets  *time.Time
	}{
		{name: "5h", percent: d.FiveHour, resets: d.FiveHourResetAt},
		{name: "7d", percent: d.SevenDay, resets: d.SevenDayResetAt},
	}
	var out []windowSnapshot
	for _, c := range candidates {
		if c.percent == nil || c.resets == nil || c.resets.IsZero() {
			continue
		}
		out = append(out, windowSnapshot{
			name:     c.name,
			percent:  *c.percent,
			resetsAt: c.resets.UTC(),
		})
	}
	return out
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
	wins := completeWindows(d)
	if len(wins) == 0 {
		return ErrNoData
	}
	var at *time.Time
	if d.FetchedAt != nil && !d.FetchedAt.IsZero() {
		t := d.FetchedAt.UTC()
		at = &t
	}
	enc := json.NewEncoder(w)
	for _, win := range wins {
		line := quotapaceLine{
			Window:   win.name,
			Percent:  win.percent,
			ResetsAt: win.resetsAt,
			Source:   d.Source,
			At:       at,
		}
		if err := enc.Encode(line); err != nil {
			return fmt.Errorf("encode quotapace line: %w", err)
		}
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
