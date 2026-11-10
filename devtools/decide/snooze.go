package decide

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
	"time"
)

// SnoozePreset is one quick choice in the snooze popup.
type SnoozePreset struct {
	Label string
	At    time.Time
}

// Snoozed reports whether it is hidden until SnoozedUntil. The ledger
// clears the snooze on read once that time has come, so a set
// SnoozedUntil on an item read from the ledger is in the future.
func (it *Item) Snoozed() bool {
	return it.SnoozedUntil != nil
}

// Snooze hides id until until. The item keeps its state. A done or
// withdrawn item is refused, and until must be in the future.
func (l *Ledger) Snooze(id string, until time.Time) (Item, error) {
	return l.update(id, func(it *Item, now time.Time) error {
		if it.State == StateWithdrawn || it.State == StateDone {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		if !until.After(now) {
			return fmt.Errorf("decide snooze time %s is not in the future", until.Format(time.RFC3339))
		}
		u, at := until.UTC(), now.UTC()
		it.SnoozedUntil, it.SnoozedAt = &u, &at
		return nil
	})
}

// Unsnooze returns a snoozed id to its folder now.
func (l *Ledger) Unsnooze(id string) (Item, error) {
	return l.update(id, func(it *Item, _ time.Time) error {
		if !it.Snoozed() {
			return fmt.Errorf("decide item %s is not snoozed", id)
		}
		clearSnooze(it)
		return nil
	})
}

func (l *Ledger) update(id string, fn func(*Item, time.Time) error) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if err := fn(it, l.clock()); err != nil {
			return err
		}
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

func clearSnooze(it *Item) {
	it.SnoozedUntil = nil
	it.SnoozedAt = nil
}

// wakeOnMessage ends the snooze of an item that just got a message and
// marks it unread.
func wakeOnMessage(it *Item) {
	if !it.Snoozed() {
		return
	}
	clearSnooze(it)
	it.Unread = true
}

// wakeStore wakes every item whose snooze time has come. It reports
// whether the ledger changed.
func wakeStore(st *store, now time.Time) bool {
	changed := false
	for i := range st.Items {
		it := &st.Items[i]
		if it.SnoozedUntil == nil || now.Before(*it.SnoozedUntil) {
			continue
		}
		clearSnooze(it)
		it.Unread = true
		changed = true
	}
	return changed
}

// SelectSnoozed returns the snoozed items, soonest wake time first.
func SelectSnoozed(items []Item) []Item {
	out := make([]Item, 0)
	for _, it := range items {
		if it.Snoozed() {
			out = append(out, it)
		}
	}
	sort.SliceStable(out, func(i, j int) bool {
		a, b := out[i].SnoozedUntil, out[j].SnoozedUntil
		if !a.Equal(*b) {
			return a.Before(*b)
		}
		return before(&out[i], &out[j])
	})
	return out
}

// SnoozeLabel is the short wake time, such as "Tue 09:00", in t's zone.
func SnoozeLabel(t time.Time) string {
	return t.Format("Mon 15:04")
}

// SnoozePresets are 1h, 3h, tomorrow 9:00, and next Monday 9:00 from now,
// in now's zone. Next Monday is a week away when today is Monday.
func SnoozePresets(now time.Time) []SnoozePreset {
	nine := func(days int) time.Time {
		y, m, d := now.Date()
		return time.Date(y, m, d+days, 9, 0, 0, 0, now.Location())
	}
	toMonday := (int(time.Monday) - int(now.Weekday()) + 7) % 7
	if toMonday == 0 {
		toMonday = 7
	}
	return []SnoozePreset{
		{Label: "1h", At: now.Add(time.Hour)},
		{Label: "3h", At: now.Add(3 * time.Hour)},
		{Label: "tomorrow 9:00", At: nine(1)},
		{Label: "next Monday 9:00", At: nine(toMonday)},
	}
}

// ParseSnooze reads a wake time relative to now, in now's zone. It accepts
// a duration such as 2h30m, HH:MM (today, or tomorrow once that time has
// passed), YYYY-MM-DD HH:MM, or a weekday and HH:MM such as "mon 9:00"
// (the next such time). The result is always after now.
func ParseSnooze(raw string, now time.Time) (time.Time, error) {
	fields := strings.Fields(raw)
	bad := fmt.Errorf("decide snooze time %q is not a duration, HH:MM, YYYY-MM-DD HH:MM, or a weekday and HH:MM", strings.TrimSpace(raw))
	var at time.Time
	switch len(fields) {
	case 1:
		if d, err := time.ParseDuration(fields[0]); err == nil {
			if d <= 0 {
				return time.Time{}, fmt.Errorf("decide snooze duration %q is not positive", fields[0])
			}
			return now.Add(d), nil
		}
		h, m, ok := parseClock(fields[0])
		if !ok {
			return time.Time{}, bad
		}
		at = onDay(now, 0, h, m)
		if !at.After(now) {
			at = onDay(now, 1, h, m)
		}
	case 2:
		h, m, ok := parseClock(fields[1])
		if !ok {
			return time.Time{}, bad
		}
		if day, err := time.ParseInLocation("2006-01-02", fields[0], now.Location()); err == nil {
			at = time.Date(day.Year(), day.Month(), day.Day(), h, m, 0, 0, now.Location())
			break
		}
		wd, ok := parseWeekday(fields[0])
		if !ok {
			return time.Time{}, bad
		}
		days := (int(wd) - int(now.Weekday()) + 7) % 7
		at = onDay(now, days, h, m)
		if !at.After(now) {
			at = onDay(now, days+7, h, m)
		}
	default:
		return time.Time{}, bad
	}
	if !at.After(now) {
		return time.Time{}, fmt.Errorf("decide snooze time %q is in the past", strings.TrimSpace(raw))
	}
	return at, nil
}

func onDay(now time.Time, days, hour, minute int) time.Time {
	y, m, d := now.Date()
	return time.Date(y, m, d+days, hour, minute, 0, 0, now.Location())
}

func parseClock(s string) (int, int, bool) {
	hs, ms, ok := strings.Cut(s, ":")
	if !ok || len(ms) != 2 || hs == "" || len(hs) > 2 {
		return 0, 0, false
	}
	h, err := strconv.Atoi(hs)
	if err != nil || h < 0 || h > 23 {
		return 0, 0, false
	}
	m, err := strconv.Atoi(ms)
	if err != nil || m < 0 || m > 59 {
		return 0, 0, false
	}
	return h, m, true
}

func parseWeekday(s string) (time.Weekday, bool) {
	s = strings.ToLower(s)
	for d := time.Sunday; d <= time.Saturday; d++ {
		name := strings.ToLower(d.String())
		if s == name || s == name[:3] {
			return d, true
		}
	}
	return 0, false
}
