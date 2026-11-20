package quotapace

import (
	"fmt"
	"time"
)

// Checkpoint is one planned point. Actual is the last reading after the
// previous checkpoint and at or before this one, with the profile
// offset. State is empty until Actual is known.
type Checkpoint struct {
	At        time.Time  `json:"at"`
	OnTrack   float64    `json:"onTrack"`
	AmberLine float64    `json:"amberLine"`
	Limit     float64    `json:"limit"`
	Actual    *float64   `json:"actual"`
	ActualAt  *time.Time `json:"actualAt"`
	State     string     `json:"state,omitempty"`
}

// Location is the plan's time zone; empty is UTC.
func (p Plan) Location() (*time.Location, error) {
	if p.TZ == "" {
		return time.UTC, nil
	}
	loc, err := time.LoadLocation(p.TZ)
	if err != nil {
		return nil, fmt.Errorf("time zone %q: %w", p.TZ, err)
	}
	return loc, nil
}

// CheckpointTimes lists the plan's checkpoint times after the baseline,
// ending with the reset. Clocks (HH:MM) fall on each calendar day in
// the plan's zone, so a DST change keeps the wall-clock time. Without
// Clocks, Every steps from the baseline. Without either, the step is a
// day for windows of two days or more, otherwise an hour.
func (p Plan) CheckpointTimes() ([]time.Time, error) {
	loc, err := p.Location()
	if err != nil {
		return nil, err
	}
	var times []time.Time
	if len(p.Clocks) > 0 {
		times, err = clockTimes(p, loc)
	} else {
		times, err = stepTimes(p)
	}
	if err != nil {
		return nil, err
	}
	return append(times, p.ResetsAt), nil
}

func clockTimes(p Plan, loc *time.Location) ([]time.Time, error) {
	type hm struct{ h, m int }
	clocks := make([]hm, 0, len(p.Clocks))
	for _, c := range p.Clocks {
		t, err := time.Parse("15:04", c)
		if err != nil {
			return nil, fmt.Errorf("checkpoint clock %q: want HH:MM", c)
		}
		clocks = append(clocks, hm{t.Hour(), t.Minute()})
	}
	var out []time.Time
	start := p.BaselineAt.In(loc)
	y, mo, d := start.Date()
	for day := time.Date(y, mo, d, 0, 0, 0, 0, loc); day.Before(p.ResetsAt); day = day.AddDate(0, 0, 1) {
		for _, c := range clocks {
			t := time.Date(day.Year(), day.Month(), day.Day(), c.h, c.m, 0, 0, loc)
			if t.After(p.BaselineAt) && t.Before(p.ResetsAt) {
				out = append(out, t)
			}
		}
	}
	return out, nil
}

func stepTimes(p Plan) ([]time.Time, error) {
	step := time.Hour
	if p.Every != "" {
		d, err := ParseDuration(p.Every)
		if err != nil {
			return nil, err
		}
		step = d
	} else if p.ResetsAt.Sub(p.BaselineAt) >= 48*time.Hour {
		step = 24 * time.Hour
	}
	var out []time.Time
	for t := p.BaselineAt.Add(step); t.Before(p.ResetsAt); t = t.Add(step) {
		out = append(out, t)
	}
	return out, nil
}

// Checkpoints fills the plan's checkpoints from the readings. A
// checkpoint's state uses the pace so far at its reading.
func Checkpoints(p Plan, readings []Reading, offset float64) ([]Checkpoint, error) {
	times, err := p.CheckpointTimes()
	if err != nil {
		return nil, err
	}
	in := windowReadings(p, readings)
	out := make([]Checkpoint, 0, len(times))
	prev := p.BaselineAt
	for _, t := range times {
		cp := Checkpoint{At: t, OnTrack: p.OnTrack(t), AmberLine: p.OnTrack(t) + p.Amber, Limit: Limit}
		if r, ok := lastBetween(in, prev, t); ok {
			pct := r.Percent + offset
			at := r.At
			cp.Actual, cp.ActualAt = &pct, &at
			projected := pct + pace(p.Baseline, p.BaselineAt, pct, r.At)*max(hours(p.ResetsAt.Sub(r.At)), 0)
			cp.State = classify(pct, p.OnTrack(r.At), projected)
		}
		out = append(out, cp)
		prev = t
	}
	return out, nil
}

// lastBetween is the last reading in (from, to].
func lastBetween(in []Reading, from, to time.Time) (Reading, bool) {
	var found Reading
	ok := false
	for _, r := range in {
		if r.At.After(from) && !r.At.After(to) {
			found, ok = r, true
		}
	}
	return found, ok
}
