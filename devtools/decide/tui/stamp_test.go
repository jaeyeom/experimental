package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestStamp(t *testing.T) {
	loc, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 12, 14, 0, 0, 0, loc)
	cases := []struct {
		name string
		at   time.Time
		want string
	}{
		{name: "three minutes ago", at: now.Add(-3 * time.Minute), want: "13:57 · 3m ago"},
		{name: "two hours ago", at: now.Add(-2 * time.Hour), want: "12:00 · 2h ago"},
		{name: "yesterday evening", at: time.Date(2026, 11, 11, 18, 2, 0, 0, loc), want: "yesterday 18:02"},
		{name: "five days ago", at: time.Date(2026, 11, 7, 9, 15, 0, 0, loc), want: "Sat 09:15"},
		{name: "six days ago", at: time.Date(2026, 11, 6, 9, 15, 0, 0, loc), want: "Fri 09:15"},
		{name: "seven days ago", at: time.Date(2026, 11, 5, 9, 15, 0, 0, loc), want: "2026-11-05 09:15"},
		{name: "thirty days ago", at: time.Date(2026, 10, 13, 9, 15, 0, 0, loc), want: "2026-10-13 09:15"},
		{name: "later today", at: time.Date(2026, 11, 12, 17, 0, 0, 0, loc), want: "today 17:00 · in 3h"},
		{name: "tomorrow", at: time.Date(2026, 11, 13, 9, 0, 0, 0, loc), want: "tomorrow 09:00"},
		{name: "within a week", at: time.Date(2026, 11, 18, 9, 0, 0, 0, loc), want: "Wed 09:00"},
		{name: "far future", at: time.Date(2026, 12, 20, 9, 0, 0, 0, loc), want: "2026-12-20 09:00"},
		{name: "zero", at: time.Time{}, want: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := stamp(tc.at, now, loc); got != tc.want {
				t.Fatalf("stamp = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestStampUTCInstantOnThePreviousLocalDay(t *testing.T) {
	loc, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 12, 1, 30, 0, 0, loc)
	at := time.Date(2026, 11, 12, 7, 0, 0, 0, time.UTC)
	if got := stamp(at, now, loc); got != "yesterday 23:00" {
		t.Fatalf("stamp = %q, want yesterday 23:00", got)
	}
}

func TestItemHeaderShowsLocalTime(t *testing.T) {
	loc, err := time.LoadLocation("America/Los_Angeles")
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 12, 14, 0, 0, 0, loc)
	dir := t.TempDir()
	led := openLedger(t, dir)
	led.SetClock(func() time.Time { return now })
	deadline := now.Add(3 * time.Hour)
	created := now.Add(-3 * time.Minute)
	mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: created,
		Deadline:  &deadline,
		Messages:  []decide.Message{{Author: "ada", Time: created, Text: "ship it?"}},
	})
	m := New(dir)
	m.Now = func() time.Time { return now }
	m.width = 100
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	list := visibleLine(m.View())
	if strings.Contains(list, "13:57") || !strings.Contains(list, "3m") {
		t.Fatalf("list age\n%s", list)
	}
	press(t, m, "\r")
	view := m.View()
	plain := visibleLine(view)
	for _, needle := range []string{
		"Created: 13:57 · 3m ago",
		"ada  13:57 · 3m ago",
		"deadline: today 17:00 · in 3h",
	} {
		if !strings.Contains(plain, needle) {
			t.Fatalf("missing %q\n%s", needle, plain)
		}
	}
	for _, label := range []string{"Created:", "deadline:"} {
		line := visibleLine(lineContaining(view, label))
		if strings.Contains(line, "Z") || strings.Contains(line, "T15") || strings.Contains(line, "T21") {
			t.Fatalf("%s is not local\n%s", label, line)
		}
	}
	m.UTC = true
	utc := visibleLine(m.View())
	if !strings.Contains(utc, "Created: 21:57 · 3m ago") || strings.Contains(utc, "Created: 13:57") {
		t.Fatalf("utc header\n%s", utc)
	}
}
