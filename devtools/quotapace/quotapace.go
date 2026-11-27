// Package quotapace records usage readings of rolling quota windows,
// such as a coding agent's 5-hour and 7-day limits, and projects them
// against a plan: the pace so far, the percent at reset, and checkpoints.
package quotapace

import (
	"fmt"
	"slices"
	"strconv"
	"strings"
	"time"
)

// Reading is one observed usage percent of a window.
// Percent is what the source showed, before the profile offset.
type Reading struct {
	At       time.Time `json:"at"`
	Profile  string    `json:"profile"`
	Window   string    `json:"window"`
	Percent  float64   `json:"percent"`
	ResetsAt time.Time `json:"resetsAt"`
	Source   string    `json:"source,omitempty"`
}

// Plan is the line usage should follow from a baseline to the reset.
// The on-track percent goes linearly from Baseline at BaselineAt to
// Target at ResetsAt. The amber line is Amber points above it.
// Checkpoints fall on Clocks (HH:MM in TZ) each day, or every Every.
type Plan struct {
	Window     string    `json:"window"`
	StartedAt  time.Time `json:"startedAt"`
	BaselineAt time.Time `json:"baselineAt"`
	Baseline   float64   `json:"baseline"`
	ResetsAt   time.Time `json:"resetsAt"`
	Target     float64   `json:"target"`
	Amber      float64   `json:"amber"`
	Rate       *float64  `json:"rate,omitempty"`
	Every      string    `json:"every,omitempty"`
	Clocks     []string  `json:"clocks,omitempty"`
	TZ         string    `json:"tz,omitempty"`
	Closed     *Closure  `json:"closed,omitempty"`
}

// Closure records why and when a plan ended, with its last percent.
type Closure struct {
	At     time.Time `json:"at"`
	Final  float64   `json:"final"`
	Reason string    `json:"reason"`
}

// Profile is one account or machine. Profiles are never added together.
// Offset is added to every reading of the profile, in points.
type Profile struct {
	Offset    float64              `json:"offset"`
	Active    map[string]*Plan     `json:"active"`
	History   []Plan               `json:"history"`
	Schedules map[string]*Schedule `json:"schedules,omitempty"`
}

// Defaults for a plan that does not set them.
const (
	DefaultTarget = 100.0
	DefaultAmber  = 5.0
	// ResetDrop is how far the percent must fall between two readings
	// to count as a reset.
	ResetDrop = 20.0
	// Limit is the quota line.
	Limit = 100.0
)

// WindowLength parses a window such as "5h" or "7d". A trailing d is
// whole days; anything else is a Go duration.
func WindowLength(window string) (time.Duration, error) {
	return parseDuration(window)
}

func parseDuration(value string) (time.Duration, error) {
	if days, ok := strings.CutSuffix(value, "d"); ok {
		n, err := strconv.Atoi(days)
		if err != nil || n <= 0 {
			return 0, fmt.Errorf("bad duration %q", value)
		}
		return time.Duration(n) * 24 * time.Hour, nil
	}
	d, err := time.ParseDuration(value)
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("bad duration %q", value)
	}
	return d, nil
}

// ParseDuration is WindowLength for any duration flag, such as 6d5h.
// It accepts a leading whole-day part followed by a Go duration.
func ParseDuration(value string) (time.Duration, error) {
	if i := strings.Index(value, "d"); i > 0 && i < len(value)-1 {
		days, err := parseDuration(value[:i+1])
		if err != nil {
			return 0, err
		}
		rest, err := parseDuration(value[i+1:])
		if err != nil {
			return 0, err
		}
		return days + rest, nil
	}
	return parseDuration(value)
}

// ImplicitPlan is the plan used when none is active: the whole window
// from 0% to the default target, ending at the reading's reset.
func ImplicitPlan(r Reading) (Plan, error) {
	length, err := WindowLength(r.Window)
	if err != nil {
		return Plan{}, err
	}
	return Plan{
		Window:     r.Window,
		BaselineAt: r.ResetsAt.Add(-length),
		ResetsAt:   r.ResetsAt,
		Target:     DefaultTarget,
		Amber:      DefaultAmber,
	}, nil
}

// OnTrack is the plan's percent at t.
func (p Plan) OnTrack(t time.Time) float64 {
	if p.Rate != nil {
		h := t.Sub(p.BaselineAt).Hours()
		if h <= 0 {
			return min(Limit, p.Baseline)
		}
		return min(Limit, p.Baseline+*p.Rate*h)
	}
	span := p.ResetsAt.Sub(p.BaselineAt)
	switch {
	case !t.After(p.BaselineAt) || span <= 0:
		return p.Baseline
	case !t.Before(p.ResetsAt):
		return p.Target
	}
	frac := float64(t.Sub(p.BaselineAt)) / float64(span)
	return p.Baseline + (p.Target-p.Baseline)*frac
}

// InWindow reports whether r belongs to the plan's window.
func (p Plan) InWindow(r Reading) bool {
	return r.Window == p.Window && !r.At.Before(p.BaselineAt) && r.At.Before(p.ResetsAt)
}

// ResetReason says why r shows that plan's window has reset, given the
// last reading before r. Empty means no reset.
func ResetReason(p Plan, prev *Reading, r Reading) string {
	switch {
	case !r.At.Before(p.ResetsAt):
		return "reset time passed"
	case r.ResetsAt.Sub(p.ResetsAt) > time.Hour:
		return "new window"
	case prev != nil && prev.Percent-r.Percent >= ResetDrop:
		return "percent dropped"
	}
	return ""
}

// windowReadings returns the readings in the plan's window, by time.
func windowReadings(p Plan, readings []Reading) []Reading {
	var out []Reading
	for _, r := range readings {
		if p.InWindow(r) {
			out = append(out, r)
		}
	}
	slices.SortStableFunc(out, func(a, b Reading) int { return a.At.Compare(b.At) })
	return out
}

// windowOrder sorts windows by length, then by name.
func windowOrder(a, b string) int {
	la, errA := WindowLength(a)
	lb, errB := WindowLength(b)
	if errA == nil && errB == nil && la != lb {
		if la < lb {
			return -1
		}
		return 1
	}
	return strings.Compare(a, b)
}
