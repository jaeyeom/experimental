package memstore

import (
	"fmt"
	"slices"
	"strconv"
	"strings"
	"time"
)

// Schedule represents a parsed deterministic routine schedule, either an
// @every duration or a concise RRULE subset.
type Schedule struct {
	every      time.Duration
	freq       string
	interval   int
	byDay      []time.Weekday
	byHour     []int
	byMinute   []int
	byMonthDay []int
}

// ParseSchedule parses and validates a routine schedule string.
// Supported formats:
//   - "@every <duration>" (e.g., "@every 15m", "@every 1h")
//   - RRULE subset: FREQ=HOURLY|DAILY|WEEKLY|MONTHLY, INTERVAL=N,
//     BYDAY=MO,TU,..., BYHOUR=H, BYMINUTE=M, BYMONTHDAY=D (with optional "RRULE:" prefix).
func ParseSchedule(raw string) (Schedule, error) {
	s := strings.TrimSpace(raw)
	if s == "" {
		return Schedule{}, fmt.Errorf("schedule is empty")
	}
	if strings.HasPrefix(s, "@every ") || s == "@every" {
		return parseEverySchedule(s)
	}
	return parseRRULESchedule(s)
}

func parseEverySchedule(s string) (Schedule, error) {
	durStr := strings.TrimSpace(strings.TrimPrefix(s, "@every"))
	if durStr == "" {
		return Schedule{}, fmt.Errorf("schedule %q missing duration", s)
	}
	d, err := time.ParseDuration(durStr)
	if err != nil {
		return Schedule{}, fmt.Errorf("invalid @every duration %q: %w", durStr, err)
	}
	if d <= 0 {
		return Schedule{}, fmt.Errorf("@every duration must be positive: %s", d)
	}
	return Schedule{every: d}, nil
}

func parseRRULESchedule(s string) (Schedule, error) {
	if len(s) >= 6 && strings.EqualFold(s[:6], "RRULE:") {
		s = s[6:]
	}
	s = strings.TrimSpace(s)
	if s == "" {
		return Schedule{}, fmt.Errorf("rrule schedule is empty")
	}

	sched := Schedule{interval: 1}
	seen := make(map[string]bool)
	for _, part := range strings.Split(s, ";") {
		part = strings.TrimSpace(part)
		if part == "" {
			return Schedule{}, fmt.Errorf("rrule %q contains empty clause", s)
		}
		k, v, ok := strings.Cut(part, "=")
		if !ok || k == "" || v == "" {
			return Schedule{}, fmt.Errorf("invalid rrule clause %q", part)
		}
		key := strings.ToUpper(strings.TrimSpace(k))
		val := strings.TrimSpace(v)
		if seen[key] {
			return Schedule{}, fmt.Errorf("duplicate rrule key %q", key)
		}
		seen[key] = true
		if err := applyRRULEField(&sched, key, val); err != nil {
			return Schedule{}, err
		}
	}

	if sched.freq == "" {
		return Schedule{}, fmt.Errorf("rrule %q missing required FREQ", s)
	}
	applyRRULEDefaults(&sched)
	return sched, nil
}

func applyRRULEField(sched *Schedule, key, val string) error {
	switch key {
	case "FREQ":
		return applyRRULEFreq(sched, val)
	case "INTERVAL":
		n, err := strconv.Atoi(val)
		if err != nil || n < 1 {
			return fmt.Errorf("invalid INTERVAL %q: must be >= 1", val)
		}
		sched.interval = n
	case "BYDAY":
		days, err := parseWeekdays(val)
		if err != nil {
			return err
		}
		sched.byDay = days
	case "BYHOUR":
		hours, err := parseIntList(val, 0, 23, "BYHOUR")
		if err != nil {
			return err
		}
		sched.byHour = hours
	case "BYMINUTE":
		mins, err := parseIntList(val, 0, 59, "BYMINUTE")
		if err != nil {
			return err
		}
		sched.byMinute = mins
	case "BYMONTHDAY":
		days, err := parseIntList(val, 1, 31, "BYMONTHDAY")
		if err != nil {
			return err
		}
		sched.byMonthDay = days
	default:
		return fmt.Errorf("unsupported rrule key %q", key)
	}
	return nil
}

func applyRRULEFreq(sched *Schedule, val string) error {
	freq := strings.ToUpper(val)
	switch freq {
	case "HOURLY", "DAILY", "WEEKLY", "MONTHLY":
		sched.freq = freq
		return nil
	default:
		return fmt.Errorf("unsupported FREQ %q", val)
	}
}

func applyRRULEDefaults(sched *Schedule) {
	if len(sched.byMinute) == 0 {
		sched.byMinute = []int{0}
	}
	if len(sched.byHour) == 0 && sched.freq != "HOURLY" {
		sched.byHour = []int{0}
	}
	if len(sched.byDay) == 0 && sched.freq == "WEEKLY" {
		sched.byDay = []time.Weekday{time.Monday}
	}
	if len(sched.byMonthDay) == 0 && sched.freq == "MONTHLY" {
		sched.byMonthDay = []int{1}
	}
}

func parseWeekdays(val string) ([]time.Weekday, error) {
	parts := strings.Split(val, ",")
	out := make([]time.Weekday, 0, len(parts))
	for _, p := range parts {
		wd, err := parseWeekday(strings.ToUpper(strings.TrimSpace(p)))
		if err != nil {
			return nil, err
		}
		if !slices.Contains(out, wd) {
			out = append(out, wd)
		}
	}
	slices.Sort(out)
	return out, nil
}

func parseWeekday(code string) (time.Weekday, error) {
	switch code {
	case "SU":
		return time.Sunday, nil
	case "MO":
		return time.Monday, nil
	case "TU":
		return time.Tuesday, nil
	case "WE":
		return time.Wednesday, nil
	case "TH":
		return time.Thursday, nil
	case "FR":
		return time.Friday, nil
	case "SA":
		return time.Saturday, nil
	default:
		return 0, fmt.Errorf("invalid BYDAY value %q", code)
	}
}

func parseIntList(val string, minVal, maxVal int, field string) ([]int, error) {
	parts := strings.Split(val, ",")
	out := make([]int, 0, len(parts))
	for _, p := range parts {
		n, err := strconv.Atoi(strings.TrimSpace(p))
		if err != nil || n < minVal || n > maxVal {
			return nil, fmt.Errorf("invalid %s value %q (want %d..%d)", field, p, minVal, maxVal)
		}
		if !slices.Contains(out, n) {
			out = append(out, n)
		}
	}
	slices.Sort(out)
	return out, nil
}

func loadScheduleLocation(tz string) (*time.Location, error) {
	if tz == "" {
		return time.UTC, nil
	}
	loc, err := time.LoadLocation(tz)
	if err != nil {
		return nil, fmt.Errorf("load timezone %q: %w", tz, err)
	}
	return loc, nil
}

// NextRun computes the next scheduled run time strictly after after in tz.
func NextRun(schedule, tz string, after time.Time) (time.Time, error) {
	sched, err := ParseSchedule(schedule)
	if err != nil {
		return time.Time{}, err
	}
	loc, err := loadScheduleLocation(tz)
	if err != nil {
		return time.Time{}, err
	}
	base := after.In(loc)
	if sched.every > 0 {
		return base.Add(sched.every), nil
	}
	switch sched.freq {
	case "HOURLY":
		return nextHourly(sched, loc, base)
	case "DAILY":
		return nextDaily(sched, loc, base)
	case "WEEKLY":
		return nextWeekly(sched, loc, base)
	case "MONTHLY":
		return nextMonthly(sched, loc, base)
	default:
		return time.Time{}, fmt.Errorf("unsupported FREQ %q", sched.freq)
	}
}

// IsDue reports whether a routine with schedule and tz is due at now given
// lastRun. If lastRun is nil, the routine is immediately due.
func IsDue(schedule, tz string, lastRun *time.Time, now time.Time) (bool, time.Time, error) {
	if _, err := ParseSchedule(schedule); err != nil {
		return false, time.Time{}, err
	}
	loc, err := loadScheduleLocation(tz)
	if err != nil {
		return false, time.Time{}, err
	}
	if lastRun == nil {
		return true, now.In(loc), nil
	}
	next, err := NextRun(schedule, tz, *lastRun)
	if err != nil {
		return false, time.Time{}, err
	}
	return !now.Before(next), next, nil
}

func nextHourly(sched Schedule, loc *time.Location, base time.Time) (time.Time, error) {
	hourStart := time.Date(base.Year(), base.Month(), base.Day(), base.Hour(), 0, 0, 0, loc)
	const maxSteps = 24 * 366 * 4
	for k := range maxSteps {
		cur := hourStart.Add(time.Duration(k*sched.interval) * time.Hour)
		if !matchesDayFilters(sched, cur) {
			continue
		}
		if len(sched.byHour) > 0 && !slices.Contains(sched.byHour, cur.Hour()) {
			continue
		}
		for _, m := range sched.byMinute {
			cand := cur.Add(time.Duration(m) * time.Minute)
			if cand.After(base) {
				return cand, nil
			}
		}
	}
	return time.Time{}, fmt.Errorf("no hourly occurrence found within search window")
}

func nextDaily(sched Schedule, loc *time.Location, base time.Time) (time.Time, error) {
	dayStart := time.Date(base.Year(), base.Month(), base.Day(), 0, 0, 0, 0, loc)
	const maxSteps = 366 * 4
	for k := range maxSteps {
		day := dayStart.AddDate(0, 0, k*sched.interval)
		if !matchesDayFilters(sched, day) {
			continue
		}
		if cand, ok := firstTimeOnDayAfter(sched, loc, day, base); ok {
			return cand, nil
		}
	}
	return time.Time{}, fmt.Errorf("no daily occurrence found within search window")
}

func nextWeekly(sched Schedule, loc *time.Location, base time.Time) (time.Time, error) {
	// Anchor to Monday of base's ISO week so INTERVAL>1 steps whole weeks cleanly.
	offset := (int(base.Weekday()) + 6) % 7
	weekStart := time.Date(base.Year(), base.Month(), base.Day()-offset, 0, 0, 0, 0, loc)
	const maxSteps = 53 * 4
	for k := range maxSteps {
		ws := weekStart.AddDate(0, 0, k*7*sched.interval)
		for d := range 7 {
			day := ws.AddDate(0, 0, d)
			if !matchesDayFilters(sched, day) {
				continue
			}
			if cand, ok := firstTimeOnDayAfter(sched, loc, day, base); ok {
				return cand, nil
			}
		}
	}
	return time.Time{}, fmt.Errorf("no weekly occurrence found within search window")
}

func nextMonthly(sched Schedule, loc *time.Location, base time.Time) (time.Time, error) {
	startYear, startMonth := base.Year(), base.Month()
	const maxSteps = 12 * 10
	for k := range maxSteps {
		first := time.Date(startYear, startMonth+time.Month(k*sched.interval), 1, 0, 0, 0, 0, loc)
		y, m := first.Year(), first.Month()
		for _, md := range sched.byMonthDay {
			day := time.Date(y, m, md, 0, 0, 0, 0, loc)
			if day.Month() != m {
				continue
			}
			if len(sched.byDay) > 0 && !slices.Contains(sched.byDay, day.Weekday()) {
				continue
			}
			if cand, ok := firstTimeOnDayAfter(sched, loc, day, base); ok {
				return cand, nil
			}
		}
	}
	return time.Time{}, fmt.Errorf("no monthly occurrence found within search window")
}

func matchesDayFilters(sched Schedule, day time.Time) bool {
	if len(sched.byDay) > 0 && !slices.Contains(sched.byDay, day.Weekday()) {
		return false
	}
	if len(sched.byMonthDay) > 0 && !slices.Contains(sched.byMonthDay, day.Day()) {
		return false
	}
	return true
}

func firstTimeOnDayAfter(sched Schedule, loc *time.Location, day, after time.Time) (time.Time, bool) {
	for _, h := range sched.byHour {
		for _, m := range sched.byMinute {
			cand := time.Date(day.Year(), day.Month(), day.Day(), h, m, 0, 0, loc)
			if cand.After(after) {
				return cand, true
			}
		}
	}
	return time.Time{}, false
}
