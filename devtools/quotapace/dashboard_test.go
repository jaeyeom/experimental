package quotapace

import (
	"strings"
	"testing"
	"time"
)

func TestDashboardEmptyStoreHint(t *testing.T) {
	s := newStore(t)
	now := at(72)
	snap, err := s.Status(nil, now, StatusOptions{Recent: 6 * time.Hour, Stale: 6 * time.Hour})
	if err != nil {
		t.Fatal(err)
	}
	out := snap.Dashboard(DashboardOptions{Stale: 6 * time.Hour})
	for _, want := range []string{"record", "plan start"} {
		if !strings.Contains(out, want) {
			t.Errorf("empty dashboard lacks %q:\n%s", want, out)
		}
	}
}

func TestDashboardProfilesWindowsAndProjections(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 24, 15), false)
	record(t, s, reading("work", 72, 40), false)
	if _, err := s.StartPlan("work", StartOptions{Window: "7d", Target: 95}, at(24)); err != nil {
		t.Fatal(err)
	}
	// Recurring schedule on a second profile ("home").
	if _, err := s.SetSchedule("home", Schedule{
		Window: "7d",
		Rate:   0.5,
		Offset: 10,
	}, at(24)); err != nil {
		t.Fatal(err)
	}
	record(t, s, reading("home", 72, 45), false)

	now := at(72).Add(30 * time.Minute)
	snap, err := s.Status([]string{"home", "work"}, now, StatusOptions{Recent: 6 * time.Hour, Stale: 6 * time.Hour})
	if err != nil {
		t.Fatal(err)
	}
	out := snap.Dashboard(DashboardOptions{Stale: 6 * time.Hour})
	for _, want := range []string{
		"home  7d",
		"work  7d",
		"Legend: * sample  . plan  : projection",
		"projected",
		"target 95%",
		"Reset: in 3d23h",
		"Freshness: 30m ago",
		"gap ",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("dashboard lacks %q:\n%s", want, out)
		}
	}
}

func TestDashboardStaleProfileMarkedStale(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 72, 40), false)
	if _, err := s.StartPlan("work", StartOptions{Window: "7d"}, at(72)); err != nil {
		t.Fatal(err)
	}
	now := at(80) // 8h after last reading, > 6h stale threshold
	snap, err := s.Status([]string{"work"}, now, StatusOptions{Recent: 6 * time.Hour, Stale: 6 * time.Hour})
	if err != nil {
		t.Fatal(err)
	}
	out := snap.Dashboard(DashboardOptions{Stale: 6 * time.Hour})
	if !strings.Contains(out, "stale") || !strings.Contains(out, "8h ago") {
		t.Fatalf("stale dashboard missing stale marker or age:\n%s", out)
	}
}

func TestDashboardNoPlanRendersWithoutProjection(t *testing.T) {
	s := newStore(t)
	record(t, s, Reading{
		At:       at(70),
		Profile:  "solo",
		Window:   "5h",
		Percent:  12,
		ResetsAt: at(73),
		Source:   "claude-usage",
	}, false)
	now := at(71)
	snap, err := s.Status([]string{"solo"}, now, StatusOptions{Recent: 6 * time.Hour, Stale: 6 * time.Hour})
	if err != nil {
		t.Fatal(err)
	}
	out := snap.Dashboard(DashboardOptions{Stale: 6 * time.Hour})
	for _, want := range []string{"solo  5h", "no plan", "12%", "Reset: in 2h", "Freshness: 1h ago", "*"} {
		if !strings.Contains(out, want) {
			t.Errorf("no-plan dashboard lacks %q:\n%s", want, out)
		}
	}
	for _, unwanted := range []string{"projection", "projected"} {
		if strings.Contains(out, unwanted) {
			t.Errorf("no-plan dashboard unexpectedly contains %q:\n%s", unwanted, out)
		}
	}
}
