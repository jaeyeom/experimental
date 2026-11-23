package memstore

import (
	"testing"
	"time"
)

func TestNextRunEvery(t *testing.T) {
	la, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}
	after := time.Date(2026, 5, 1, 10, 0, 0, 0, time.UTC)
	got, err := NextRun("@every 15m", "America/Los_Angeles", after)
	if err != nil {
		t.Fatalf("NextRun: %v", err)
	}
	want := time.Date(2026, 5, 1, 3, 15, 0, 0, la)
	if !got.Equal(want) || got.Location().String() != la.String() {
		t.Errorf("got = %v (%s), want %v (%s)", got, got.Location(), want, la)
	}
}

func TestNextRunRRULE(t *testing.T) {
	cases := []struct {
		name     string
		schedule string
		tz       string
		after    time.Time
		want     time.Time
	}{
		{
			name:     "hourly default minute",
			schedule: "FREQ=HOURLY",
			tz:       "UTC",
			after:    time.Date(2026, 5, 1, 10, 0, 0, 0, time.UTC),
			want:     time.Date(2026, 5, 1, 11, 0, 0, 0, time.UTC),
		},
		{
			name:     "hourly interval 2 with byminute",
			schedule: "RRULE:FREQ=HOURLY;INTERVAL=2;BYMINUTE=15,45",
			tz:       "UTC",
			after:    time.Date(2026, 5, 1, 10, 20, 0, 0, time.UTC),
			want:     time.Date(2026, 5, 1, 10, 45, 0, 0, time.UTC),
		},
		{
			name:     "hourly interval 2 wraps to +2h",
			schedule: "FREQ=HOURLY;INTERVAL=2;BYMINUTE=15",
			tz:       "UTC",
			after:    time.Date(2026, 5, 1, 10, 20, 0, 0, time.UTC),
			want:     time.Date(2026, 5, 1, 12, 15, 0, 0, time.UTC),
		},
		{
			name:     "daily same day",
			schedule: "FREQ=DAILY;BYHOUR=9;BYMINUTE=30",
			tz:       "UTC",
			after:    time.Date(2026, 5, 1, 8, 0, 0, 0, time.UTC),
			want:     time.Date(2026, 5, 1, 9, 30, 0, 0, time.UTC),
		},
		{
			name:     "daily interval 3 next day",
			schedule: "FREQ=DAILY;INTERVAL=3;BYHOUR=9;BYMINUTE=0",
			tz:       "UTC",
			after:    time.Date(2026, 5, 1, 9, 0, 0, 0, time.UTC),
			want:     time.Date(2026, 5, 4, 9, 0, 0, 0, time.UTC),
		},
		{
			name:     "weekly byday same week",
			schedule: "FREQ=WEEKLY;BYDAY=MO,WE,FR;BYHOUR=8;BYMINUTE=0",
			tz:       "UTC",
			// 2026-05-04 is Monday.
			after: time.Date(2026, 5, 4, 9, 0, 0, 0, time.UTC),
			want:  time.Date(2026, 5, 6, 8, 0, 0, 0, time.UTC),
		},
		{
			name:     "weekly interval 2 wraps to next cycle",
			schedule: "FREQ=WEEKLY;INTERVAL=2;BYDAY=MO;BYHOUR=8;BYMINUTE=0",
			tz:       "UTC",
			// 2026-05-04 is Monday.
			after: time.Date(2026, 5, 4, 9, 0, 0, 0, time.UTC),
			want:  time.Date(2026, 5, 18, 8, 0, 0, 0, time.UTC),
		},
		{
			name:     "monthly bymonthday skips short months",
			schedule: "FREQ=MONTHLY;BYMONTHDAY=31;BYHOUR=12;BYMINUTE=0",
			tz:       "UTC",
			after:    time.Date(2026, 1, 31, 13, 0, 0, 0, time.UTC),
			want:     time.Date(2026, 3, 31, 12, 0, 0, 0, time.UTC),
		},
		{
			name:     "monthly leap day",
			schedule: "FREQ=MONTHLY;BYMONTHDAY=29;BYHOUR=10;BYMINUTE=0",
			tz:       "UTC",
			after:    time.Date(2024, 1, 30, 0, 0, 0, 0, time.UTC),
			want:     time.Date(2024, 2, 29, 10, 0, 0, 0, time.UTC),
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got, err := NextRun(tc.schedule, tc.tz, tc.after)
			if err != nil {
				t.Fatalf("NextRun: %v", err)
			}
			if !got.Equal(tc.want) {
				t.Errorf("NextRun = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestNextRunDSTBoundaries(t *testing.T) {
	la, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}

	// Spring forward: 2026-03-08 02:00 PST -> 03:00 PDT.
	// A daily 09:00 job on 2026-03-07 10:00 PST should fire at 2026-03-08 09:00 PDT (23h later).
	beforeSpring := time.Date(2026, 3, 7, 10, 0, 0, 0, la)
	gotSpring, err := NextRun("FREQ=DAILY;BYHOUR=9;BYMINUTE=0", "America/Los_Angeles", beforeSpring)
	if err != nil {
		t.Fatalf("NextRun spring forward: %v", err)
	}
	wantSpring := time.Date(2026, 3, 8, 9, 0, 0, 0, la)
	if !gotSpring.Equal(wantSpring) {
		t.Errorf("spring forward = %v, want %v", gotSpring, wantSpring)
	}

	// Hourly across fall-back: 2026-11-01 01:30 PDT -> 1h later is 01:30 PST.
	beforeFall := time.Date(2026, 11, 1, 0, 30, 0, 0, la)
	firstFall, err := NextRun("FREQ=HOURLY;BYMINUTE=30", "America/Los_Angeles", beforeFall)
	if err != nil {
		t.Fatalf("NextRun fall-back 1: %v", err)
	}
	secondFall, err := NextRun("FREQ=HOURLY;BYMINUTE=30", "America/Los_Angeles", firstFall)
	if err != nil {
		t.Fatalf("NextRun fall-back 2: %v", err)
	}
	if diff := secondFall.Sub(firstFall); diff != time.Hour {
		t.Errorf("fall-back hourly interval = %v, want 1h (first=%v, second=%v)", diff, firstFall, secondFall)
	}
}

func TestIsDue(t *testing.T) {
	now := time.Date(2026, 5, 1, 9, 30, 0, 0, time.UTC)

	// Never run -> due immediately.
	due, next, err := IsDue("FREQ=DAILY;BYHOUR=9;BYMINUTE=0", "UTC", nil, now)
	if err != nil {
		t.Fatalf("IsDue nil lastRun: %v", err)
	}
	if !due || !next.Equal(now) {
		t.Errorf("nil lastRun: due=%v next=%v, want true, %v", due, next, now)
	}

	// Last ran yesterday -> due today at 09:00.
	yesterday := time.Date(2026, 4, 30, 9, 0, 0, 0, time.UTC)
	due, next, err = IsDue("FREQ=DAILY;BYHOUR=9;BYMINUTE=0", "UTC", &yesterday, now)
	if err != nil {
		t.Fatalf("IsDue yesterday: %v", err)
	}
	wantNext := time.Date(2026, 5, 1, 9, 0, 0, 0, time.UTC)
	if !due || !next.Equal(wantNext) {
		t.Errorf("yesterday lastRun: due=%v next=%v, want true, %v", due, next, wantNext)
	}

	// Last ran today at 09:00 -> not due yet at 09:30.
	today := time.Date(2026, 5, 1, 9, 0, 0, 0, time.UTC)
	due, next, err = IsDue("FREQ=DAILY;BYHOUR=9;BYMINUTE=0", "UTC", &today, now)
	if err != nil {
		t.Fatalf("IsDue today: %v", err)
	}
	wantTomorrow := time.Date(2026, 5, 2, 9, 0, 0, 0, time.UTC)
	if due || !next.Equal(wantTomorrow) {
		t.Errorf("today lastRun: due=%v next=%v, want false, %v", due, next, wantTomorrow)
	}

	// @every duration.
	last10mAgo := now.Add(-10 * time.Minute)
	due, _, err = IsDue("@every 15m", "UTC", &last10mAgo, now)
	if err != nil || due {
		t.Errorf("@every 15m after 10m: due=%v err=%v, want false nil", due, err)
	}
	last15mAgo := now.Add(-15 * time.Minute)
	due, _, err = IsDue("@every 15m", "UTC", &last15mAgo, now)
	if err != nil || !due {
		t.Errorf("@every 15m after 15m: due=%v err=%v, want true nil", due, err)
	}
}

func TestParseScheduleErrors(t *testing.T) {
	bad := []string{
		"",
		"@every",
		"@every -5m",
		"@every bogus",
		"RRULE:",
		"INTERVAL=2",
		"FREQ=YEARLY",
		"FREQ=DAILY;INTERVAL=0",
		"FREQ=DAILY;BOGUS=1",
		"FREQ=WEEKLY;BYDAY=XX",
		"FREQ=DAILY;BYHOUR=25",
		"FREQ=DAILY;BYMINUTE=60",
		"FREQ=MONTHLY;BYMONTHDAY=32",
		"FREQ=DAILY;FREQ=HOURLY",
	}
	for _, s := range bad {
		if _, err := ParseSchedule(s); err == nil {
			t.Errorf("ParseSchedule(%q) = nil, want error", s)
		}
	}
	if _, err := NextRun("FREQ=DAILY", "Mars/Olympus_Mons", time.Now()); err == nil {
		t.Error("NextRun invalid timezone = nil, want error")
	}
}
