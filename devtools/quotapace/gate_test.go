package quotapace

import (
	"encoding/json"
	"strings"
	"testing"
	"time"
	_ "time/tzdata"
)

func TestScheduleLineMathAndWeeklyRestart(t *testing.T) {
	loc, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}
	// Thu 2026-11-12 15:00 PST starts the week; Thu 2026-11-19 15:00 PST restarts it.
	sched := Schedule{
		Window: "7d",
		Reset:  "Thu 15:00",
		TZ:     "America/Los_Angeles",
		Rate:   0.6,
		Offset: 10,
	}

	cases := []struct {
		name      string
		at        time.Time
		wantStart time.Time
		wantLine  float64
	}{
		{
			name:      "at reset restart",
			at:        time.Date(2026, 11, 12, 15, 0, 0, 0, loc),
			wantStart: time.Date(2026, 11, 12, 15, 0, 0, 0, loc),
			wantLine:  10,
		},
		{
			name:      "20 hours into week",
			at:        time.Date(2026, 11, 13, 11, 0, 0, 0, loc),
			wantStart: time.Date(2026, 11, 12, 15, 0, 0, 0, loc),
			wantLine:  22, // 10 + 0.6*20
		},
		{
			name:      "capped at 100 before reset",
			at:        time.Date(2026, 11, 19, 14, 0, 0, 0, loc),
			wantStart: time.Date(2026, 11, 12, 15, 0, 0, 0, loc),
			wantLine:  100, // 10 + 0.6*167 = 110.2 -> 100
		},
		{
			name:      "automatically restarted after next Thu 15:00",
			at:        time.Date(2026, 11, 19, 20, 0, 0, 0, loc),
			wantStart: time.Date(2026, 11, 19, 15, 0, 0, 0, loc),
			wantLine:  13, // 10 + 0.6*5
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			start, _, ok, err := sched.WindowBounds(tc.at)
			if err != nil || !ok {
				t.Fatalf("WindowBounds(%v) = (%v, ok=%v, err=%v)", tc.at, start, ok, err)
			}
			if !start.Equal(tc.wantStart) {
				t.Fatalf("start = %v, want %v", start.In(loc), tc.wantStart)
			}
			got := sched.LineAt(start, tc.at)
			near(t, "line", got, tc.wantLine)
		})
	}
}

func TestScheduleAcrossDST(t *testing.T) {
	loc, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}
	// DST ends Sun 2026-11-01 02:00 in Los Angeles.
	// Week runs Thu 2026-10-29 15:00 PDT -> Thu 2026-11-05 15:00 PST (169 hours).
	sched := Schedule{
		Window: "7d",
		Reset:  "Thu 15:00",
		TZ:     "America/Los_Angeles",
		Rate:   0.5,
		Offset: 5,
	}
	checkAt := time.Date(2026, 11, 2, 15, 0, 0, 0, loc) // 4 days + 1 DST hour = 97h after Oct 29 15:00 PDT
	start, reset, ok, err := sched.WindowBounds(checkAt)
	if err != nil || !ok {
		t.Fatalf("WindowBounds error: ok=%v err=%v", ok, err)
	}
	wantStart := time.Date(2026, 10, 29, 15, 0, 0, 0, loc)
	wantReset := time.Date(2026, 11, 5, 15, 0, 0, 0, loc)
	if !start.Equal(wantStart) || !reset.Equal(wantReset) {
		t.Fatalf("bounds = [%v, %v), want [%v, %v)", start.In(loc), reset.In(loc), wantStart, wantReset)
	}
	near(t, "line across DST", sched.LineAt(start, checkAt), 5+0.5*97)
}

func TestGateExitCodesAndGapMath(t *testing.T) {
	loc, err := time.LoadLocation("UTC")
	if err != nil {
		t.Fatal(err)
	}
	// Thu 2026-11-12 15:00 UTC -> Thu 2026-11-19 15:00 UTC.
	// At +55h (Sat 2026-11-14 22:00Z): line = 10 + 0.5*55 = 37.5.
	reset := time.Date(2026, 11, 19, 15, 0, 0, 0, loc)
	readingAt := time.Date(2026, 11, 14, 22, 0, 0, 0, loc)
	now := readingAt.Add(3 * time.Minute)

	t.Run("pass exit 0 with schedule", func(t *testing.T) {
		s := newStore(t)
		if _, err := s.SetSchedule("work", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "UTC", Rate: 0.5, Offset: 10}, readingAt); err != nil {
			t.Fatal(err)
		}
		record(t, s, Reading{At: readingAt, Profile: "work", Window: "7d", Percent: 33.3, ResetsAt: reset, Source: "claude-usage"}, false)

		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 4.2, Stale: 6 * time.Hour}, now)
		if err != nil {
			t.Fatal(err)
		}
		if res.Exit != 0 {
			t.Fatalf("exit = %d, want 0 (reason=%q)", res.Exit, res.Reason)
		}
		near(t, "gap", *res.Gap, 4.2)
		near(t, "budgetLine", *res.BudgetLine, 37.5)
		near(t, "sevenDay", *res.SevenDay, 33.3)
		if line := res.Line(now); line != "gap=4.2 line=37.5 used=33.3 (7d, profile work, reading 3m old)" {
			t.Fatalf("line = %q", line)
		}
	})

	t.Run("below min-gap exit 1", func(t *testing.T) {
		s := newStore(t)
		if _, err := s.SetSchedule("work", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "UTC", Rate: 0.5, Offset: 10}, readingAt); err != nil {
			t.Fatal(err)
		}
		record(t, s, Reading{At: readingAt, Profile: "work", Window: "7d", Percent: 34.0, ResetsAt: reset, Source: "claude-usage"}, false)

		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 4.0, Stale: 6 * time.Hour}, now)
		if err != nil {
			t.Fatal(err)
		}
		if res.Exit != 1 {
			t.Fatalf("exit = %d, want 1", res.Exit)
		}
		near(t, "gap", *res.Gap, 3.5)
	})

	t.Run("schedule without configured reset uses reading reset", func(t *testing.T) {
		s := newStore(t)
		// Default reset from reading: winStart = reset - 7d = 2026-11-12 15:00Z.
		if _, err := s.SetSchedule("work", Schedule{Window: "7d", Rate: 0.5, Offset: 10}, readingAt); err != nil {
			t.Fatal(err)
		}
		record(t, s, Reading{At: readingAt, Profile: "work", Window: "7d", Percent: 30.0, ResetsAt: reset}, false)

		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 5.0, Stale: 6 * time.Hour}, now)
		if err != nil {
			t.Fatal(err)
		}
		if res.Exit != 0 {
			t.Fatalf("exit = %d, want 0 (reason=%q)", res.Exit, res.Reason)
		}
		near(t, "gap", *res.Gap, 7.5)
	})

	t.Run("plan start plan also works in gate", func(t *testing.T) {
		s := newStore(t)
		record(t, s, reading("work", 72, 40), false)
		if _, err := s.StartPlan("work", StartOptions{Window: "7d"}, at(72)); err != nil {
			t.Fatal(err)
		}
		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 2.0, Stale: 6 * time.Hour}, at(73))
		if err != nil {
			t.Fatal(err)
		}
		if res.Exit != 0 {
			t.Fatalf("exit = %d, want 0", res.Exit)
		}
		near(t, "budgetLine", *res.BudgetLine, 72.0/168*100)
	})

	t.Run("exit 3 no reading", func(t *testing.T) {
		s := newStore(t)
		if _, err := s.SetSchedule("work", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "UTC", Rate: 0.5, Offset: 10}, now); err != nil {
			t.Fatal(err)
		}
		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 1, Stale: 6 * time.Hour}, now)
		if err != nil || res.Exit != 3 {
			t.Fatalf("got (%+v, %v), want exit 3", res, err)
		}
	})

	t.Run("exit 3 stale reading", func(t *testing.T) {
		s := newStore(t)
		if _, err := s.SetSchedule("work", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "UTC", Rate: 0.5, Offset: 10}, readingAt); err != nil {
			t.Fatal(err)
		}
		record(t, s, Reading{At: readingAt, Profile: "work", Window: "7d", Percent: 20, ResetsAt: reset}, false)
		staleNow := readingAt.Add(7 * time.Hour)
		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 1, Stale: 6 * time.Hour}, staleNow)
		if err != nil || res.Exit != 3 {
			t.Fatalf("got (%+v, %v), want exit 3", res, err)
		}
		if res.Gap != nil || res.BudgetLine != nil {
			t.Fatalf("stale result should not expose gap/budgetLine: %+v", res)
		}
	})

	t.Run("exit 3 no plan even with reading", func(t *testing.T) {
		s := newStore(t)
		record(t, s, Reading{At: readingAt, Profile: "work", Window: "7d", Percent: 20, ResetsAt: reset}, false)
		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 1, Stale: 6 * time.Hour}, now)
		if err != nil || res.Exit != 3 {
			t.Fatalf("got (%+v, %v), want exit 3", res, err)
		}
	})

	t.Run("exit 3 window reset since last reading (scheduled)", func(t *testing.T) {
		s := newStore(t)
		if _, err := s.SetSchedule("work", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "UTC", Rate: 0.5, Offset: 10}, readingAt); err != nil {
			t.Fatal(err)
		}
		// Reading recorded 1 hour before Thu 15:00 reset; now is 1 hour after reset.
		beforeReset := reset.Add(-time.Hour)
		afterReset := reset.Add(time.Hour)
		record(t, s, Reading{At: beforeReset, Profile: "work", Window: "7d", Percent: 20, ResetsAt: reset}, false)

		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 1, Stale: 6 * time.Hour}, afterReset)
		if err != nil || res.Exit != 3 {
			t.Fatalf("got (%+v, %v), want exit 3", res, err)
		}
	})

	t.Run("exit 3 window reset since last reading (unscheduled reset)", func(t *testing.T) {
		s := newStore(t)
		if _, err := s.SetSchedule("work", Schedule{Window: "7d", Rate: 0.5, Offset: 10}, readingAt); err != nil {
			t.Fatal(err)
		}
		beforeReset := reset.Add(-time.Hour)
		afterReset := reset.Add(time.Hour)
		record(t, s, Reading{At: beforeReset, Profile: "work", Window: "7d", Percent: 20, ResetsAt: reset}, false)

		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 1, Stale: 6 * time.Hour}, afterReset)
		if err != nil || res.Exit != 3 {
			t.Fatalf("got (%+v, %v), want exit 3", res, err)
		}
	})

	t.Run("gate reads only the given profile", func(t *testing.T) {
		s := newStore(t)
		if _, err := s.SetSchedule("other", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "UTC", Rate: 0.5, Offset: 10}, readingAt); err != nil {
			t.Fatal(err)
		}
		record(t, s, Reading{At: readingAt, Profile: "other", Window: "7d", Percent: 10, ResetsAt: reset}, false)

		res, err := s.Gate(GateOptions{Profile: "work", Window: "7d", MinGap: 1, Stale: 6 * time.Hour}, now)
		if err != nil || res.Exit != 3 {
			t.Fatalf("got (%+v, %v), want exit 3", res, err)
		}
	})
}

func TestGateJSONFields(t *testing.T) {
	s := newStore(t)
	reset := time.Date(2026, 11, 19, 15, 0, 0, 0, time.UTC)
	readingAt := time.Date(2026, 11, 14, 22, 0, 0, 0, time.UTC)
	now := readingAt.Add(180 * time.Second)

	if _, err := s.SetSchedule("remote", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "UTC", Rate: 0.5, Offset: 10}, readingAt); err != nil {
		t.Fatal(err)
	}
	record(t, s, Reading{At: readingAt, Profile: "remote", Window: "7d", Percent: 33.3, ResetsAt: reset, Source: "claude-usage"}, false)

	// Without 5h recorded: fiveHour should be omitted.
	res, err := s.Gate(GateOptions{Profile: "remote", Window: "7d", MinGap: 4.0, Stale: 6 * time.Hour}, now)
	if err != nil {
		t.Fatal(err)
	}
	data, err := json.Marshal(res)
	if err != nil {
		t.Fatal(err)
	}
	var m map[string]any
	if err := json.Unmarshal(data, &m); err != nil {
		t.Fatal(err)
	}
	for _, key := range []string{"gap", "budgetLine", "sevenDay", "window", "profile", "source", "readingAt", "ageSeconds", "exit"} {
		if _, ok := m[key]; !ok {
			t.Errorf("JSON missing key %q: %s", key, data)
		}
	}
	if _, ok := m["fiveHour"]; ok {
		t.Errorf("JSON should omit fiveHour when not recorded: %s", data)
	}
	if m["ageSeconds"] != float64(180) || m["profile"] != "remote" || m["window"] != "7d" || m["source"] != "claude-usage" || m["exit"] != float64(0) {
		t.Errorf("unexpected JSON values: %v", m)
	}

	// With 5h recorded: fiveHour should be included.
	record(t, s, Reading{At: readingAt, Profile: "remote", Window: "5h", Percent: 12.5, ResetsAt: readingAt.Add(3 * time.Hour), Source: "claude-usage"}, false)
	res, err = s.Gate(GateOptions{Profile: "remote", Window: "7d", MinGap: 4.0, Stale: 6 * time.Hour}, now)
	if err != nil {
		t.Fatal(err)
	}
	data, err = json.Marshal(res)
	if err != nil {
		t.Fatal(err)
	}
	m = nil
	if err := json.Unmarshal(data, &m); err != nil {
		t.Fatal(err)
	}
	if m["fiveHour"] != 12.5 {
		t.Errorf("fiveHour = %v, want 12.5 (%s)", m["fiveHour"], data)
	}
}

func TestResetDisagreementWarningInStatus(t *testing.T) {
	loc, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}
	s := newStore(t)
	now := time.Date(2026, 11, 16, 10, 0, 0, 0, loc)
	if _, err := s.SetSchedule("work", Schedule{Window: "7d", Reset: "Thu 15:00", TZ: "America/Los_Angeles", Rate: 0.5, Offset: 0}, now); err != nil {
		t.Fatal(err)
	}

	// Within 1h (30m diff): no warning.
	closeReset := time.Date(2026, 11, 19, 15, 30, 0, 0, loc)
	record(t, s, Reading{At: now, Profile: "work", Window: "7d", Percent: 20, ResetsAt: closeReset}, false)
	snap, err := s.Status([]string{"work"}, now, StatusOptions{})
	if err != nil {
		t.Fatal(err)
	}
	if len(snap.Profiles[0].Windows[0].Warnings) != 0 {
		t.Fatalf("unexpected warnings within 1h: %v", snap.Profiles[0].Windows[0].Warnings)
	}

	// Over 1h (2h diff): warning emitted.
	farReset := time.Date(2026, 11, 19, 17, 0, 0, 0, loc)
	record(t, s, Reading{At: now.Add(time.Minute), Profile: "work", Window: "7d", Percent: 21, ResetsAt: farReset}, false)
	snap, err = s.Status([]string{"work"}, now.Add(time.Minute), StatusOptions{})
	if err != nil {
		t.Fatal(err)
	}
	warnings := snap.Profiles[0].Windows[0].Warnings
	if len(warnings) != 1 || !strings.Contains(warnings[0], "disagree") && !strings.Contains(warnings[0], "schedule") {
		t.Fatalf("warnings = %v, want reset disagreement warning", warnings)
	}
}

func TestGateWatcherNotifiesOncePerCrossing(t *testing.T) {
	var w GateWatcher
	open := GateResult{Exit: 0}
	closed := GateResult{Exit: 1}
	unknown := GateResult{Exit: 3}

	// Initial observation does not notify.
	if w.Step(open) {
		t.Fatal("initial open should not notify")
	}
	if w.Step(open) {
		t.Fatal("steady open should not notify")
	}
	// Crossing 0 -> 1 notifies once.
	if !w.Step(closed) {
		t.Fatal("crossing 0 -> 1 should notify")
	}
	if w.Step(closed) {
		t.Fatal("steady closed should not notify")
	}
	// Transient exit 3 does not notify.
	if w.Step(unknown) {
		t.Fatal("unknown should not notify")
	}
	// Still closed after unknown: does not notify.
	if w.Step(closed) {
		t.Fatal("resuming closed after unknown should not notify")
	}
	// Crossing 1 -> 0 notifies once.
	if !w.Step(open) {
		t.Fatal("crossing 1 -> 0 should notify")
	}
	if w.Step(open) {
		t.Fatal("steady open after crossing should not notify")
	}
}
