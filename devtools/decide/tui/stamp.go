package tui

import (
	"fmt"
	"time"
)

// stamp is how the inbox shows t in loc, relative to now.
// Today is "13:57 · 3m ago" or "today 17:00 · in 3h". Yesterday and
// tomorrow name the day. A time within a week uses a weekday. Older
// times use a date. A zero t is empty.
func stamp(t, now time.Time, loc *time.Location) string {
	if t.IsZero() {
		return ""
	}
	if loc == nil {
		loc = time.Local
	}
	t = t.In(loc)
	now = now.In(loc)
	clock := t.Format("15:04")
	switch days := calendarDays(t, now); {
	case days == 0:
		return sameDay(clock, now.Sub(t))
	case days == -1:
		return "yesterday " + clock
	case days == 1:
		return "tomorrow " + clock
	case days <= -7 || days >= 7:
		return t.Format("2006-01-02") + " " + clock
	default:
		return t.Format("Mon") + " " + clock
	}
}

// calendarDays is the date of t minus the date of now, in those times' zone.
func calendarDays(t, now time.Time) int {
	ty, tm, td := t.Date()
	ny, nm, nd := now.Date()
	a := time.Date(ty, tm, td, 12, 0, 0, 0, time.UTC)
	b := time.Date(ny, nm, nd, 12, 0, 0, 0, time.UTC)
	return int(a.Sub(b) / (24 * time.Hour)) //nolint:gosec // G115: a date span fits in int
}

func sameDay(clock string, ago time.Duration) string {
	if ago >= 0 {
		return clock + " · " + pastPhrase(ago)
	}
	return "today " + clock + " · " + futurePhrase(-ago)
}

func pastPhrase(d time.Duration) string {
	unit := coarse(d)
	if unit == "now" {
		return unit
	}
	return unit + " ago"
}

func futurePhrase(d time.Duration) string {
	unit := coarse(d)
	if unit == "now" {
		return unit
	}
	return "in " + unit
}

func coarse(d time.Duration) string {
	if d < time.Minute {
		return "now"
	}
	if d < time.Hour {
		return fmt.Sprintf("%dm", int(d/time.Minute))
	}
	return fmt.Sprintf("%dh", int(d/time.Hour))
}
