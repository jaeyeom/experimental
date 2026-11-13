package scan

import (
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
)

func TestHistoryNearUsesAnEarlierUTCStartDay(t *testing.T) {
	t.Parallel()

	cache := &mapHistoryCache{entries: map[string]gh.PathHistory{}}
	day := time.Date(2026, 8, 4, 0, 0, 0, 0, time.UTC)
	since := func(d time.Time) string { return "since:" + d.UTC().Format(time.DateOnly) }
	older := gh.PathHistory{Authors: []string{"alice"}}
	cache.PutPathHistory("acme/widgets", "main", "src/a.go", since(day.AddDate(0, 0, -1)), older)

	hist, ok, across := historyNear(cache, "acme/widgets", "main", "src/a.go", since(day), 2)
	if !ok || !across || !slices.Equal(hist.Authors, []string{"alice"}) {
		t.Fatalf("D-1 with slack 2: ok=%v across=%v hist=%+v", ok, across, hist)
	}
	if _, ok, across := historyNear(cache, "acme/widgets", "main", "src/a.go", since(day), 0); ok || across {
		t.Fatalf("slack 0: ok=%v across=%v, want a miss", ok, across)
	}
	if _, ok, _ := historyNear(cache, "acme/widgets", "main", "src/a.go", since(day.AddDate(0, 0, 2)), 2); ok {
		t.Fatal("D-3 with slack 2 was a hit")
	}

	newer := gh.PathHistory{Authors: []string{"carol"}}
	cache.PutPathHistory("acme/widgets", "main", "src/a.go", since(day), newer)
	hist, ok, across = historyNear(cache, "acme/widgets", "main", "src/a.go", since(day), 2)
	if !ok || across || !slices.Equal(hist.Authors, []string{"carol"}) {
		t.Fatalf("exact day: ok=%v across=%v hist=%+v, want the newest", ok, across, hist)
	}
}

func TestHistoryNearMissesAStaleEarlierDay(t *testing.T) {
	t.Parallel()

	start := time.Date(2026, 8, 3, 12, 0, 0, 0, time.UTC)
	clock := start
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), func() time.Time { return clock })
	if err != nil {
		t.Fatal(err)
	}
	cache.SetHistoryTTL(6 * time.Hour)
	cache.PutPathHistory("acme/widgets", "main", "src/a.go", "since:2026-08-03", gh.PathHistory{Authors: []string{"alice"}})
	clock = start.Add(6 * time.Hour)
	if _, ok, _ := historyNear(cache, "acme/widgets", "main", "src/a.go", "since:2026-08-04", 2); ok {
		t.Fatal("a slack hit reused history past its TTL")
	}
}

func TestRunTeamCoverHistorySurvivesUTCDayRollover(t *testing.T) {
	t.Parallel()

	start := time.Date(2026, 1, 1, 23, 0, 0, 0, time.UTC)
	clock := start
	cache, err := roundcache.Load(filepath.Join(t.TempDir(), "cache.json"), func() time.Time { return clock })
	if err != nil {
		t.Fatal(err)
	}
	cache.SetHistoryTTL(6 * time.Hour)

	first, _, _ := historyRoundAt(t, cache, "h1", "base1", clock, 6*time.Hour)
	if first.historyCalls != 1 {
		t.Fatalf("first historyCalls = %d, want 1", first.historyCalls)
	}

	clock = start.Add(2 * time.Hour)
	second, doc, logs := historyRoundAt(t, cache, "h2", "base2", clock, 6*time.Hour)
	if second.historyCalls != 0 {
		t.Fatalf("after the UTC day rollover historyCalls = %d, want 0", second.historyCalls)
	}
	if !strings.Contains(logs, `"hits":2,"misses":0`) {
		t.Fatalf("logs = %s, want the same two hits and no misses", logs)
	}
	notes := strings.Join(doc.TeamCoverNotes, "\n")
	if !strings.Contains(notes, "need history (2 hits) (2 across days)") {
		t.Fatalf("notes = %q, want both hits counted across days", notes)
	}
	oldDay := "since:" + start.AddDate(0, 0, -90).UTC().Format(time.DateOnly)
	newDay := "since:" + clock.AddDate(0, 0, -90).UTC().Format(time.DateOnly)
	if _, ok := cache.PathHistory("acme/widgets", "main", "src/a.go", oldDay); !ok {
		t.Fatal("the previous start day was not kept")
	}
	if _, ok := cache.PathHistory("acme/widgets", "main", "src/a.go", newDay); ok {
		t.Fatal("a slack hit wrote the new start day")
	}
}
