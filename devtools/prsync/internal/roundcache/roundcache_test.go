package roundcache

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

type clock struct{ t time.Time }

func (c *clock) now() time.Time { return c.t }

func newClock() *clock {
	return &clock{t: time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)}
}

func TestPathHistorySurvivesSaveAndLoad(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, err := Load(path, clk.now)
	if err != nil {
		t.Fatal(err)
	}
	want := gh.PathHistory{Authors: []string{"alice"}, Reviewers: []string{"bob"}}
	c.PutPathHistory("acme/widgets", "main", "src/a.go", "head1", want)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	clk.t = clk.t.Add(59 * time.Minute)
	next, err := Load(path, clk.now)
	if err != nil {
		t.Fatal(err)
	}
	got, ok := next.PathHistory("acme/widgets", "main", "src/a.go", "head1")
	if !ok || len(got.Authors) != 1 || got.Authors[0] != "alice" || got.Reviewers[0] != "bob" {
		t.Fatalf("PathHistory() = %+v, %v", got, ok)
	}
	if _, ok := next.PathHistory("acme/widgets", "main", "src/a.go", "head2"); ok {
		t.Fatal("hit for a different head")
	}
	if _, ok := next.PathHistory("acme/widgets", "main", "src/b.go", "head1"); ok {
		t.Fatal("hit for a different path")
	}
}

func TestEntriesExpire(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.PutPathHistory("acme/widgets", "main", "src/a.go", "base1", gh.PathHistory{Authors: []string{"alice"}})
	c.PutRollup("acme/widgets", 7, "head1", "2026-11-01T11:00:00Z", []gh.StatusCheck{{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"}})
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	// The history key holds the base head, so its TTL is only a backstop.
	clk.t = clk.t.Add(TTL + time.Second)
	next, _ := Load(path, clk.now)
	if _, ok := next.PathHistory("acme/widgets", "main", "src/a.go", "base1"); !ok {
		t.Fatal("path history for an unchanged base head expired with the rollup TTL")
	}
	if _, ok := next.Rollup("acme/widgets", 7, "head1", "2026-11-01T11:00:00Z"); ok {
		t.Fatal("expired rollup hit")
	}

	clk.t = clk.t.Add(HistoryTTL)
	last, _ := Load(path, clk.now)
	if _, ok := last.PathHistory("acme/widgets", "main", "src/a.go", "base1"); ok {
		t.Fatal("path history hit past HistoryTTL")
	}
	if err := last.Save(); err != nil {
		t.Fatal(err)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != "{\"history\":{},\"rollups\":{}}\n" {
		t.Fatalf("saved = %s, want expired entries dropped", data)
	}
}

func TestNextRoundCountsPerJobAcrossLoads(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	if got := c.NextRound("team-cover"); got != 1 {
		t.Fatalf("first round = %d, want 1", got)
	}
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}
	clk.t = clk.t.Add(2 * HistoryTTL)
	next, _ := Load(path, clk.now)
	if got := next.NextRound("team-cover"); got != 2 {
		t.Fatalf("second round = %d, want 2 (counters do not expire)", got)
	}
	if got := next.NextRound("other"); got != 1 {
		t.Fatalf("other job = %d, want its own counter", got)
	}
}

func TestHistoryBatchSurvivesSaveAndLoad(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	if got := c.HistoryBatch(); got != 0 {
		t.Fatalf("empty cache HistoryBatch = %d, want 0", got)
	}
	c.PutHistoryBatch(5)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}
	clk.t = clk.t.Add(2 * HistoryTTL)
	next, _ := Load(path, clk.now)
	if got := next.HistoryBatch(); got != 5 {
		t.Fatalf("HistoryBatch = %d, want 5 (it does not expire)", got)
	}
}

func TestRollupNeedsSameHeadAndUpdatedAt(t *testing.T) {
	t.Parallel()

	clk := newClock()
	c, _ := Load(filepath.Join(t.TempDir(), "cache.json"), clk.now)
	checks := []gh.StatusCheck{{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"}}
	c.PutRollup("acme/widgets", 7, "head1", "t1", checks)
	if got, ok := c.Rollup("acme/widgets", 7, "head1", "t1"); !ok || len(got) != 1 || got[0].Name != "ci" {
		t.Fatalf("Rollup() = %+v, %v", got, ok)
	}
	for _, tc := range []struct{ head, updated string }{{"head2", "t1"}, {"head1", "t2"}, {"", ""}} {
		if _, ok := c.Rollup("acme/widgets", 7, tc.head, tc.updated); ok {
			t.Fatalf("Rollup(%q, %q) hit, want miss", tc.head, tc.updated)
		}
	}
	if _, ok := c.Rollup("acme/widgets", 8, "head1", "t1"); ok {
		t.Fatal("hit for another PR")
	}
}

func TestLoadMissingAndCorruptFiles(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	c, err := Load(filepath.Join(dir, "missing", "cache.json"), time.Now)
	if err != nil || c == nil {
		t.Fatalf("missing file: c=%v err=%v", c, err)
	}
	corrupt := filepath.Join(dir, "corrupt.json")
	if err := os.WriteFile(corrupt, []byte("{not json"), 0o600); err != nil {
		t.Fatal(err)
	}
	c, err = Load(corrupt, time.Now)
	if err == nil {
		t.Fatal("corrupt file: want an error to log")
	}
	if c == nil {
		t.Fatal("corrupt file: want an empty usable cache")
	}
	if _, ok := c.PathHistory("acme/widgets", "main", "a", "h"); ok {
		t.Fatal("corrupt file: unexpected hit")
	}
}

func TestSaveWithoutChangesDoesNotWrite(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	c, _ := Load(path, time.Now)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("stat = %v, want no file", err)
	}
}
