package decide

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

type fakeClock struct{ t time.Time }

func (c *fakeClock) now() time.Time { return c.t }

func snoozeLedger(t *testing.T) (*Ledger, *fakeClock, Item) {
	t.Helper()
	led, _ := openTemp(t)
	clock := &fakeClock{t: time.Date(2026, 11, 10, 14, 0, 0, 0, time.UTC)}
	led.SetClock(clock.now)
	item, err := led.Create(&Item{Question: "ship?", Owner: "ada", Kind: KindRelay})
	if err != nil {
		t.Fatal(err)
	}
	return led, clock, item
}

func TestSnoozeHidesTheItemUntilItsTime(t *testing.T) {
	led, clock, item := snoozeLedger(t)
	until := clock.t.Add(time.Hour)
	snoozed, err := led.Snooze(item.ID, until)
	if err != nil {
		t.Fatal(err)
	}
	if !snoozed.Snoozed() || !snoozed.SnoozedUntil.Equal(until) || !snoozed.SnoozedAt.Equal(clock.t) || snoozed.State != StateOpen {
		t.Fatalf("snoozed = %+v", snoozed)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if open, _ := Select(items, StateOpen); len(open) != 0 {
		t.Fatalf("open = %+v", open)
	}
	if got := SelectSnoozed(items); len(got) != 1 || got[0].ID != item.ID {
		t.Fatalf("snoozed folder = %+v", got)
	}

	clock.t = until
	items, err = led.List()
	if err != nil {
		t.Fatal(err)
	}
	open, _ := Select(items, StateOpen)
	if len(open) != 1 || open[0].Snoozed() || !open[0].Unread || open[0].SnoozedAt != nil {
		t.Fatalf("woken = %+v", open)
	}
	if got := SelectSnoozed(items); len(got) != 0 {
		t.Fatalf("snoozed after wake = %+v", got)
	}
}

func TestReplyWakesASnoozedItem(t *testing.T) {
	led, clock, item := snoozeLedger(t)
	if _, err := led.Snooze(item.ID, clock.t.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	clock.t = clock.t.Add(10 * time.Minute)
	got, err := led.Reply(item.ID, "update", nil, false)
	if err != nil {
		t.Fatal(err)
	}
	if got.Snoozed() || got.SnoozedAt != nil || !got.Unread {
		t.Fatalf("reply = %+v", got)
	}
	items, _ := led.List()
	if open, _ := Select(items, StateOpen); len(open) != 1 {
		t.Fatalf("open = %+v", open)
	}
}

func TestComposeAndAnswerWakeASnoozedItem(t *testing.T) {
	led, clock, item := snoozeLedger(t)
	if _, err := led.Snooze(item.ID, clock.t.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	got, err := led.Compose(item.ID, "hi")
	if err != nil {
		t.Fatal(err)
	}
	if got.Snoozed() || !got.Unread {
		t.Fatalf("compose = %+v", got)
	}

	q, err := led.Create(&Item{
		Question: "pick", Owner: "ada", Kind: KindRelay,
		Options: []Option{{Key: "a", Label: "A"}, {Key: "b", Label: "B"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Snooze(q.ID, clock.t.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	answered, err := led.Answer(q.ID, "a")
	if err != nil {
		t.Fatal(err)
	}
	if answered.Snoozed() {
		t.Fatalf("answer = %+v", answered)
	}
}

func TestUnsnoozeReturnsTheItem(t *testing.T) {
	led, clock, item := snoozeLedger(t)
	if _, err := led.Unsnooze(item.ID); err == nil {
		t.Fatal("unsnooze of an awake item succeeded")
	}
	if _, err := led.Snooze(item.ID, clock.t.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	got, err := led.Unsnooze(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Snoozed() || got.SnoozedAt != nil {
		t.Fatalf("unsnooze = %+v", got)
	}
}

func TestSnoozeRefusesFinishedItemsAndPastTimes(t *testing.T) {
	led, clock, item := snoozeLedger(t)
	if _, err := led.Snooze(item.ID, clock.t); err == nil {
		t.Fatal("snooze until now succeeded")
	}
	if _, err := led.Snooze(item.ID, clock.t.Add(-time.Minute)); err == nil {
		t.Fatal("snooze into the past succeeded")
	}
	if _, err := led.Done(item.ID, "ok"); err != nil {
		t.Fatal(err)
	}
	if _, err := led.Snooze(item.ID, clock.t.Add(time.Hour)); err == nil || !strings.Contains(err.Error(), "done") {
		t.Fatalf("snooze done err = %v", err)
	}
	other, err := led.Create(&Item{Question: "x", Owner: "ada", Kind: KindRelay})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Withdraw(other.ID, "gone"); err != nil {
		t.Fatal(err)
	}
	if _, err := led.Snooze(other.ID, clock.t.Add(time.Hour)); err == nil {
		t.Fatal("snooze withdrawn succeeded")
	}
}

func TestDoneClearsTheSnooze(t *testing.T) {
	led, clock, item := snoozeLedger(t)
	if _, err := led.Snooze(item.ID, clock.t.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	done, err := led.Done(item.ID, "ok")
	if err != nil {
		t.Fatal(err)
	}
	if done.Snoozed() {
		t.Fatalf("done = %+v", done)
	}
}

func TestSelectSnoozedSortsByWakeTime(t *testing.T) {
	led, clock, first := snoozeLedger(t)
	second, err := led.Create(&Item{Question: "two", Owner: "ada", Kind: KindRelay})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Snooze(first.ID, clock.t.Add(3*time.Hour)); err != nil {
		t.Fatal(err)
	}
	if _, err := led.Snooze(second.ID, clock.t.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	items, _ := led.List()
	got := SelectSnoozed(items)
	if len(got) != 2 || got[0].ID != second.ID || got[1].ID != first.ID {
		t.Fatalf("order = %+v", got)
	}
}

func TestOldLedgerLoadsUnsnoozed(t *testing.T) {
	dir := t.TempDir()
	raw := `{"next": 2, "items": [{"id": "D-001", "question": "old", "owner": "ada", "kind": "relay", "state": "open", "freeText": false, "unread": false,
  "messages": [{"author": "ada", "time": "2026-11-01T00:00:00Z", "text": "old"}]}]}`
	if err := os.WriteFile(filepath.Join(dir, ledgerName), []byte(raw), 0o600); err != nil {
		t.Fatal(err)
	}
	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 1 || items[0].Snoozed() || items[0].SnoozedAt != nil || items[0].Unread {
		t.Fatalf("items = %+v", items)
	}
	if open, _ := Select(items, StateOpen); len(open) != 1 {
		t.Fatalf("open = %+v", open)
	}
}

func TestParseSnooze(t *testing.T) {
	loc := time.FixedZone("KST", 9*3600)
	// Tuesday 2026-11-10 14:00 local.
	now := time.Date(2026, 11, 10, 14, 0, 0, 0, loc)
	at := func(month time.Month, day, hour, minute int) time.Time {
		return time.Date(2026, month, day, hour, minute, 0, 0, loc)
	}
	tests := []struct {
		in   string
		want time.Time
	}{
		{"2h30m", now.Add(150 * time.Minute)},
		{"1h", now.Add(time.Hour)},
		{"45m", now.Add(45 * time.Minute)},
		{"16:30", at(11, 10, 16, 30)},
		{"9:00", at(11, 11, 9, 0)},
		{"14:00", at(11, 11, 14, 0)},
		{"2026-11-12 08:15", at(11, 12, 8, 15)},
		{"mon 9:00", at(11, 16, 9, 0)},
		{"Tue 15:00", at(11, 10, 15, 0)},
		{"tue 13:00", at(11, 17, 13, 0)},
		{"friday 18:00", at(11, 13, 18, 0)},
		{"  wed   7:05 ", at(11, 11, 7, 5)},
	}
	for _, tt := range tests {
		got, err := ParseSnooze(tt.in, now)
		if err != nil {
			t.Errorf("ParseSnooze(%q) error = %v", tt.in, err)
			continue
		}
		if !got.Equal(tt.want) {
			t.Errorf("ParseSnooze(%q) = %s, want %s", tt.in, got, tt.want)
		}
	}
	for _, bad := range []string{"", "soon", "-1h", "0s", "25:00", "9:60", "2026-11-09 08:00", "2026-13-01 08:00", "xyz 9:00", "mon", "mon 9", "9:00 mon"} {
		if got, err := ParseSnooze(bad, now); err == nil {
			t.Errorf("ParseSnooze(%q) = %s, want an error", bad, got)
		}
	}
}

func TestSnoozePresets(t *testing.T) {
	loc := time.FixedZone("KST", 9*3600)
	tests := []struct {
		name string
		now  time.Time
		want []time.Time
	}{
		{
			name: "tuesday afternoon",
			now:  time.Date(2026, 11, 10, 14, 0, 0, 0, loc),
			want: []time.Time{
				time.Date(2026, 11, 10, 15, 0, 0, 0, loc),
				time.Date(2026, 11, 10, 17, 0, 0, 0, loc),
				time.Date(2026, 11, 11, 9, 0, 0, 0, loc),
				time.Date(2026, 11, 16, 9, 0, 0, 0, loc),
			},
		},
		{
			name: "just after midnight",
			now:  time.Date(2026, 11, 11, 0, 5, 0, 0, loc),
			want: []time.Time{
				time.Date(2026, 11, 11, 1, 5, 0, 0, loc),
				time.Date(2026, 11, 11, 3, 5, 0, 0, loc),
				time.Date(2026, 11, 12, 9, 0, 0, 0, loc),
				time.Date(2026, 11, 16, 9, 0, 0, 0, loc),
			},
		},
		{
			name: "monday morning",
			now:  time.Date(2026, 11, 16, 8, 0, 0, 0, loc),
			want: []time.Time{
				time.Date(2026, 11, 16, 9, 0, 0, 0, loc),
				time.Date(2026, 11, 16, 11, 0, 0, 0, loc),
				time.Date(2026, 11, 17, 9, 0, 0, 0, loc),
				time.Date(2026, 11, 23, 9, 0, 0, 0, loc),
			},
		},
	}
	labels := []string{"1h", "3h", "tomorrow 9:00", "next Monday 9:00"}
	for _, tt := range tests {
		got := SnoozePresets(tt.now)
		if len(got) != len(tt.want) {
			t.Fatalf("%s: presets = %+v", tt.name, got)
		}
		for i := range got {
			if got[i].Label != labels[i] || !got[i].At.Equal(tt.want[i]) {
				t.Errorf("%s: preset %d = %s %s, want %s %s", tt.name, i, got[i].Label, got[i].At, labels[i], tt.want[i])
			}
		}
	}
}

func TestSnoozeLabel(t *testing.T) {
	at := time.Date(2026, 11, 17, 9, 0, 0, 0, time.UTC)
	if got := SnoozeLabel(at); got != "Tue 09:00" {
		t.Fatalf("SnoozeLabel = %q", got)
	}
}
