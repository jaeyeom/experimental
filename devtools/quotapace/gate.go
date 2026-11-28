package quotapace

import (
	"fmt"
	"strings"
	"time"
)

// Gate target line choices for GateOptions.Against.
const (
	AgainstLine  = "line"
	AgainstAmber = "amber"
)

// GateOptions configures a fail-closed headroom gate check for one profile and window.
type GateOptions struct {
	Profile string
	Window  string
	MinGap  float64
	Stale   time.Duration
	Against string
}

// GateResult is the outcome of Gate, serialized by `quotapace gate --json`.
type GateResult struct {
	Gap        *float64   `json:"gap"`
	BudgetLine *float64   `json:"budgetLine"`
	AmberLine  *float64   `json:"amberLine,omitempty"`
	Against    string     `json:"against"`
	SevenDay   *float64   `json:"sevenDay"`
	FiveHour   *float64   `json:"fiveHour,omitempty"`
	Window     string     `json:"window"`
	Profile    string     `json:"profile"`
	Source     string     `json:"source"`
	ReadingAt  *time.Time `json:"readingAt"`
	AgeSeconds *int64     `json:"ageSeconds"`
	Exit       int        `json:"exit"`
	Reason     string     `json:"reason,omitempty"`

	used float64
}

// Gate evaluates whether the given profile's window has at least MinGap
// percentage points of headroom under its plan line (or amber line when
// Against is AgainstAmber). It reads only o.Profile and fails closed (Exit 3)
// when there is no reading, the reading is stale, there is no explicit plan,
// or the window reset since the last reading.
func (s Store) Gate(o GateOptions, now time.Time) (GateResult, error) {
	res := GateResult{
		Window:  o.Window,
		Profile: o.Profile,
		Exit:    3,
	}
	against, err := normalizeAgainst(o.Against)
	if err != nil {
		return res, err
	}
	res.Against = against
	if err := CheckProfileName(o.Profile); err != nil {
		return res, err
	}
	if _, err := WindowLength(o.Window); err != nil {
		return res, err
	}
	prof, err := s.LoadProfile(o.Profile)
	if err != nil {
		return res, err
	}
	readings, err := s.Readings(o.Profile)
	if err != nil {
		return res, err
	}
	populateGateWindows(&res, readings, prof.Offset)

	latest := latestOf(readings, o.Window)
	if latest == nil {
		res.Reason = "no reading"
		return res, nil
	}
	at := latest.At
	ageSec := int64(now.Sub(latest.At) / time.Second)
	res.Source = latest.Source
	res.ReadingAt = &at
	res.AgeSeconds = &ageSec
	res.used = latest.Percent + prof.Offset

	plan, ok, err := gatePlan(prof, o.Window, latest, now)
	if err != nil {
		return res, err
	}
	if !ok {
		res.Reason = "no plan"
		return res, nil
	}
	return evaluateGate(res, plan, *latest, o.MinGap, o.Stale, against, now), nil
}

func normalizeAgainst(against string) (string, error) {
	switch against {
	case "", AgainstLine:
		return AgainstLine, nil
	case AgainstAmber:
		return AgainstAmber, nil
	default:
		return "", fmt.Errorf("invalid against %q: want %s or %s", against, AgainstLine, AgainstAmber)
	}
}

func populateGateWindows(res *GateResult, readings []Reading, offset float64) {
	if r5h := latestOf(readings, "5h"); r5h != nil {
		res.FiveHour = ptr(r5h.Percent + offset)
	}
	if r7d := latestOf(readings, "7d"); r7d != nil {
		res.SevenDay = ptr(r7d.Percent + offset)
	}
}

func evaluateGate(res GateResult, plan Plan, latest Reading, minGap float64, stale time.Duration, against string, now time.Time) GateResult {
	if !now.Before(plan.ResetsAt) || !now.Before(latest.ResetsAt) || latest.At.Before(plan.BaselineAt) {
		res.Reason = "window reset since last reading"
		return res
	}
	if stale > 0 && now.Sub(latest.At) > stale {
		res.Reason = "reading stale"
		return res
	}
	line := plan.OnTrack(latest.At)
	amberLine := line + plan.Amber
	target := line
	if against == AgainstAmber {
		target = amberLine
	}
	gap := target - res.used
	res.BudgetLine = &line
	res.AmberLine = &amberLine
	res.Gap = &gap
	const eps = 1e-9
	if gap+eps >= minGap {
		res.Exit = 0
	} else {
		res.Exit = 1
	}
	return res
}

func gatePlan(prof Profile, window string, latest *Reading, now time.Time) (Plan, bool, error) {
	if p := prof.Active[window]; p != nil {
		return *p, true, nil
	}
	if sched := prof.Schedules[window]; sched != nil {
		return sched.PlanAt(now, latest)
	}
	return Plan{}, false, nil
}

// Line formats the single-line human output for quotapace gate.
func (r GateResult) Line(now time.Time) string {
	if r.Exit == 3 {
		if r.Reason == "reading stale" && r.ReadingAt != nil {
			return fmt.Sprintf("reading stale, %s old (%s, profile %s)", formatGateAge(now.Sub(*r.ReadingAt)), r.Window, r.Profile)
		}
		reason := r.Reason
		if reason == "" {
			reason = "unavailable"
		}
		return fmt.Sprintf("%s (%s, profile %s)", reason, r.Window, r.Profile)
	}
	age := time.Duration(0)
	if r.ReadingAt != nil {
		age = now.Sub(*r.ReadingAt)
	}
	label, ref := "line", *r.BudgetLine
	if r.Against == AgainstAmber && r.AmberLine != nil {
		label, ref = "amber", *r.AmberLine
	}
	return fmt.Sprintf("gap=%.1f %s=%.1f used=%.1f (%s, profile %s, reading %s old)",
		*r.Gap, label, ref, r.used, r.Window, r.Profile, formatGateAge(age))
}

func formatGateAge(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	if d < time.Minute {
		return fmt.Sprintf("%ds", int(d/time.Second))
	}
	if d < time.Hour {
		return fmt.Sprintf("%dm", int(d/time.Minute))
	}
	s := d.Truncate(time.Minute).String()
	s = strings.TrimSuffix(s, "0s")
	s = strings.TrimSuffix(s, "0m")
	return s
}

// GateWatcher tracks gate threshold crossings across consecutive checks.
type GateWatcher struct {
	hasLast  bool
	lastOpen bool
}

// Step records res and returns true once each time the gate crosses MinGap in
// either direction (between exit 0 and exit 1).
func (w *GateWatcher) Step(res GateResult) bool {
	if res.Exit != 0 && res.Exit != 1 {
		return false
	}
	open := res.Exit == 0
	if !w.hasLast {
		w.hasLast = true
		w.lastOpen = open
		return false
	}
	if open == w.lastOpen {
		return false
	}
	w.lastOpen = open
	return true
}
