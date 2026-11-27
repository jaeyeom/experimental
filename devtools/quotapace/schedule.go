package quotapace

import (
	"fmt"
	"strings"
	"time"
	_ "time/tzdata"
)

// Schedule is a recurring pacing plan whose line is
// min(Limit, Offset + Rate * hoursSinceWindowStart), restarting at each reset.
// When Reset is set (e.g. "Thu 15:00" in TZ), it defines the weekly reset
// boundary and overrides the reading's reset time; otherwise the latest
// reading's reset time is used.
type Schedule struct {
	Window string  `json:"window"`
	Reset  string  `json:"reset,omitempty"`
	TZ     string  `json:"tz,omitempty"`
	Rate   float64 `json:"rate"`
	Offset float64 `json:"offset,omitempty"`
	Amber  float64 `json:"amber,omitempty"`
}

var weekdayNames = map[string]time.Weekday{
	"sun": time.Sunday, "sunday": time.Sunday,
	"mon": time.Monday, "monday": time.Monday,
	"tue": time.Tuesday, "tuesday": time.Tuesday,
	"wed": time.Wednesday, "wednesday": time.Wednesday,
	"thu": time.Thursday, "thursday": time.Thursday,
	"fri": time.Friday, "friday": time.Friday,
	"sat": time.Saturday, "saturday": time.Saturday,
}

// ParseWeeklyReset parses a weekly reset specification such as "Thu 15:00".
func ParseWeeklyReset(spec string) (time.Weekday, int, int, error) {
	fields := strings.Fields(spec)
	if len(fields) != 2 {
		return 0, 0, 0, fmt.Errorf("reset %q: want \"Weekday HH:MM\", such as \"Thu 15:00\"", spec)
	}
	wd, ok := weekdayNames[strings.ToLower(fields[0])]
	if !ok {
		return 0, 0, 0, fmt.Errorf("reset %q: unknown weekday %q", spec, fields[0])
	}
	tm, err := time.Parse("15:04", fields[1])
	if err != nil {
		return 0, 0, 0, fmt.Errorf("reset %q: want HH:MM: %w", spec, err)
	}
	return wd, tm.Hour(), tm.Minute(), nil
}

// Location returns the schedule's time zone; empty is UTC.
func (s Schedule) Location() (*time.Location, error) {
	if s.TZ == "" {
		return time.UTC, nil
	}
	loc, err := time.LoadLocation(s.TZ)
	if err != nil {
		return nil, fmt.Errorf("time zone %q: %w", s.TZ, err)
	}
	return loc, nil
}

// WindowBounds returns the active [start, reset) window containing t when a
// weekly Reset is configured. ok is false when Reset is empty.
func (s Schedule) WindowBounds(t time.Time) (start, reset time.Time, ok bool, err error) {
	if s.Reset == "" {
		return time.Time{}, time.Time{}, false, nil
	}
	wd, hour, minVal, err := ParseWeeklyReset(s.Reset)
	if err != nil {
		return time.Time{}, time.Time{}, false, err
	}
	loc, err := s.Location()
	if err != nil {
		return time.Time{}, time.Time{}, false, err
	}
	tLoc := t.In(loc)
	delta := (int(tLoc.Weekday()) - int(wd) + 7) % 7
	anchor := time.Date(tLoc.Year(), tLoc.Month(), tLoc.Day(), 0, 0, 0, 0, loc)
	day := anchor.AddDate(0, 0, -delta)
	cand := time.Date(day.Year(), day.Month(), day.Day(), hour, minVal, 0, 0, loc)
	if cand.After(t) {
		prevDay := day.AddDate(0, 0, -7)
		start = time.Date(prevDay.Year(), prevDay.Month(), prevDay.Day(), hour, minVal, 0, 0, loc)
		reset = cand
	} else {
		nextDay := day.AddDate(0, 0, 7)
		start = cand
		reset = time.Date(nextDay.Year(), nextDay.Month(), nextDay.Day(), hour, minVal, 0, 0, loc)
	}
	return start, reset, true, nil
}

// LineAt returns the schedule's plan line value at t for a window starting at
// windowStart, capped at Limit (100).
func (s Schedule) LineAt(windowStart, t time.Time) float64 {
	h := t.Sub(windowStart).Hours()
	if h <= 0 {
		return min(Limit, s.Offset)
	}
	return min(Limit, s.Offset+s.Rate*h)
}

// ResetDisagreement reports whether a reading's reset time disagrees with the
// configured weekly schedule reset by more than one hour.
func (s Schedule) ResetDisagreement(readingResetsAt time.Time) (bool, string, error) {
	if s.Reset == "" || readingResetsAt.IsZero() {
		return false, "", nil
	}
	prev, next, ok, err := s.WindowBounds(readingResetsAt)
	if err != nil || !ok {
		return false, "", err
	}
	nearest := prev
	diff := readingResetsAt.Sub(prev)
	if nextDiff := next.Sub(readingResetsAt); nextDiff < diff {
		nearest = next
		diff = nextDiff
	}
	if diff <= time.Hour {
		return false, "", nil
	}
	loc, err := s.Location()
	if err != nil {
		return false, "", err
	}
	return true, fmt.Sprintf(
		"reading reset %s disagrees with schedule %s by %s",
		readingResetsAt.In(loc).Format("Mon 15:04 MST"),
		nearest.In(loc).Format("Mon 15:04 MST"),
		roundAge(diff),
	), nil
}

// PlanAt builds the concrete Plan for the window at now, using the configured
// weekly reset or falling back to latest.ResetsAt.
func (s Schedule) PlanAt(now time.Time, latest *Reading) (Plan, bool, error) {
	length, err := WindowLength(s.Window)
	if err != nil {
		return Plan{}, false, err
	}
	var start, reset time.Time
	switch {
	case s.Reset != "":
		var ok bool
		start, reset, ok, err = s.WindowBounds(now)
		if err != nil || !ok {
			return Plan{}, false, err
		}
	case latest != nil:
		reset = latest.ResetsAt
		start = reset.Add(-length)
	default:
		return Plan{}, false, nil
	}
	amber := s.Amber
	if amber == 0 {
		amber = DefaultAmber
	}
	rate := s.Rate
	return Plan{
		Window:     s.Window,
		BaselineAt: start,
		Baseline:   s.Offset,
		ResetsAt:   reset,
		Target:     s.LineAt(start, reset),
		Amber:      amber,
		Rate:       &rate,
		TZ:         s.TZ,
	}, true, nil
}

// SetSchedule saves a recurring schedule for profile and window. Any active
// one-off plan for that window is closed as "rebaselined" in history.
func (s Store) SetSchedule(profile string, sched Schedule, now time.Time) (Schedule, error) {
	if err := CheckProfileName(profile); err != nil {
		return Schedule{}, err
	}
	length, err := WindowLength(sched.Window)
	if err != nil {
		return Schedule{}, err
	}
	if sched.Rate < 0 {
		return Schedule{}, fmt.Errorf("rate %.2f must be >= 0", sched.Rate)
	}
	if _, err := sched.Location(); err != nil {
		return Schedule{}, err
	}
	if sched.Reset != "" {
		if _, _, _, err := ParseWeeklyReset(sched.Reset); err != nil {
			return Schedule{}, err
		}
		if length != 7*24*time.Hour {
			return Schedule{}, fmt.Errorf("--reset weekly schedule requires a 7d window, got %q", sched.Window)
		}
	}
	prof, err := s.LoadProfile(profile)
	if err != nil {
		return Schedule{}, err
	}
	if old := prof.Active[sched.Window]; old != nil {
		readings, err := s.Readings(profile)
		if err != nil {
			return Schedule{}, err
		}
		final := old.Baseline
		if pr, ok := Project(*old, readings, prof.Offset, 0); ok {
			final = pr.Percent
		}
		closed := *old
		closed.Closed = &Closure{At: now, Final: final, Reason: "rebaselined"}
		prof.History = append(prof.History, closed)
		delete(prof.Active, sched.Window)
	}
	prof.Schedules[sched.Window] = &sched
	return sched, s.SaveProfile(profile, prof)
}
