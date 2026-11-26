package stats

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestCompact(t *testing.T) {
	for d, want := range map[time.Duration]string{
		0:                                 "0s",
		59 * time.Second:                  "59s",
		4*time.Minute + 49*time.Second:    "4m 49s",
		time.Hour:                         "1h",
		36*time.Hour + 51*time.Minute:     "1d 12h",
		3*24*time.Hour + 2*time.Hour:      "3d 2h",
		20 * time.Hour:                    "20h",
		5 * time.Minute:                   "5m",
		-(4*time.Minute + 49*time.Second): "-4m 49s",
	} {
		if got := Compact(d); got != want {
			t.Fatalf("Compact(%s) = %q, want %q", d, got, want)
		}
	}
}

func mustLoc(t *testing.T, name string) *time.Location {
	t.Helper()
	loc, err := time.LoadLocation(name)
	if err != nil {
		t.Fatal(err)
	}
	return loc
}

func TestBucketStartLocal(t *testing.T) {
	la := mustLoc(t, "America/Los_Angeles")
	ev := time.Date(2026, 3, 10, 3, 0, 0, 0, time.UTC) // The evening of 03-09 in Los Angeles.
	if got := bucketStart(ev, ByDay, la).Format(time.DateOnly); got != "2026-03-09" {
		t.Fatalf("local bucket = %s, want 2026-03-09", got)
	}
	if got := bucketStart(ev, ByDay, time.UTC).Format(time.DateOnly); got != "2026-03-10" {
		t.Fatalf("UTC bucket = %s, want 2026-03-10", got)
	}
}

func TestBucketAcrossDST(t *testing.T) {
	la := mustLoc(t, "America/Los_Angeles")
	// 2026-03-08 is the spring-forward day in Los Angeles: 23 hours long.
	start := bucketStart(time.Date(2026, 3, 8, 12, 0, 0, 0, la), ByDay, la)
	next := nextBucket(start, ByDay)
	if start.Hour() != 0 || next.Hour() != 0 || next.Sub(start) != 23*time.Hour {
		t.Fatalf("bucket %s to %s = %s, want local midnights 23h apart", start, next, next.Sub(start))
	}
	week := bucketStart(time.Date(2026, 3, 11, 12, 0, 0, 0, la), ByWeek, la)
	if week.Weekday() != time.Monday || week.Format(time.DateOnly) != "2026-03-09" || week.Hour() != 0 {
		t.Fatalf("week start = %s", week)
	}
}

func burnData(now time.Time, nodes map[itemKey]node, finished []store.Finished) *data {
	return &data{
		opts:     Options{Now: now, Since: 3 * 24 * time.Hour, By: ByDay, Loc: time.UTC},
		start:    now.Add(-3 * 24 * time.Hour),
		cfg:      &config.Config{},
		nodes:    nodes,
		children: map[itemKey][]itemKey{},
		finished: finished,
	}
}

func TestBurndownSeriesAndProjection(t *testing.T) {
	now := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	day := func(d int) time.Time { return time.Date(2026, 11, d, 6, 0, 0, 0, time.UTC) }
	k := func(id string) itemKey { return itemKey{"jira:acme", id} }
	nodes := map[itemKey]node{
		k("A"): {key: k("A"), created: day(1), done: true},  // Finished 11-09.
		k("B"): {key: k("B"), created: day(1)},              // Open.
		k("C"): {key: k("C"), created: day(10)},             // Created 11-10.
		k("D"): {key: k("D"), created: day(1), done: true},  // Finished 11-11.
		k("E"): {key: k("E"), done: true},                   // Done with no finish time: left out.
		k("F"): {key: k("F")},                               // Unknown create time.
		k("P"): {key: k("P"), created: day(1)},              // A parent: not a leaf.
		k("Q"): {key: k("Q"), created: day(1), parent: "P"}, // Its child, open.
	}
	finished := []store.Finished{
		{Source: "jira:acme", ID: "A", FinishedAt: day(9)},
		{Source: "jira:acme", ID: "D", FinishedAt: day(11)},
	}
	d := burnData(now, nodes, finished)
	d.children[k("P")] = []itemKey{k("Q")}
	b := d.burndown()
	var got []int
	for _, pt := range b.Points {
		got = append(got, pt.Remaining)
	}
	// Buckets 11-08..11-11 end at the next midnight, or now for the last.
	// 11-08 end: A B D F Q = 5. 11-09 end: A finished = 4. 11-10 end: C created = 5.
	// 11-11 end (now 12:00): D finished at 06:00 = 4.
	want := []int{5, 4, 5, 4}
	if len(got) != len(want) {
		t.Fatalf("points = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("points = %v, want %v", got, want)
		}
	}
	if b.UnknownCreated != 1 {
		t.Fatalf("unknown created = %d, want 1", b.UnknownCreated)
	}
	// Two finishes in 7 days is 2/7 per day; 4 remaining take 14 days.
	if b.Projected == nil || b.Projected.Format(time.DateOnly) != "2026-11-25" {
		t.Fatalf("projected = %v, want 2026-11-25", b.Projected)
	}
	var out strings.Builder
	rep := Report{Window: Window{By: ByDay}, Burndown: b, loc: time.UTC}
	if err := rep.WriteTextWith(&out, TextOptions{Width: 80, ASCII: true}); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "projection: 4 remaining / 0.3 finished/day (last 7 days) ~ 14 days -> done around 2026-11-25\n") ||
		!strings.Contains(out.String(), "1 item has no create time. It counts as remaining from 2026-11-08, so the start of the line may be up to 1 too high.\n") {
		t.Fatalf("text:\n%s", out.String())
	}
}

func TestBurndownNoFinishes(t *testing.T) {
	now := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	k := itemKey{"jira:acme", "B"}
	d := burnData(now, map[itemKey]node{k: {key: k, created: now.Add(-48 * time.Hour)}}, nil)
	b := d.burndown()
	if b.Projected != nil || b.Rate != 0 {
		t.Fatalf("burndown = %+v, want no projection", b)
	}
	var out strings.Builder
	rep := Report{Window: Window{By: ByDay}, Burndown: b, loc: time.UTC}
	if err := rep.WriteTextWith(&out, TextOptions{Width: 80}); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "no projection: nothing finished in the last 7 days\n") {
		t.Fatalf("text:\n%s", out.String())
	}
	out.Reset()
	if err := rep.WriteTextWith(&out, TextOptions{Width: 39}); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(out.String(), "Burndown") {
		t.Fatalf("narrow terminal drew a chart:\n%s", out.String())
	}
}
