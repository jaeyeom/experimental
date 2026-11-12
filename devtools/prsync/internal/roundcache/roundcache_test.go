package roundcache

import (
	"errors"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"
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

func TestSetHistoryTTLBoundsReuse(t *testing.T) {
	t.Parallel()

	clk := newClock()
	c, _ := Load(filepath.Join(t.TempDir(), "cache.json"), clk.now)
	c.SetHistoryTTL(6 * time.Hour)
	c.PutPathHistory("acme/widgets", "main", "src/a.go", "since:2026-08-03", gh.PathHistory{Authors: []string{"alice"}})

	clk.t = clk.t.Add(6*time.Hour - time.Second)
	if _, ok := c.PathHistory("acme/widgets", "main", "src/a.go", "since:2026-08-03"); !ok {
		t.Fatal("miss inside the TTL")
	}
	clk.t = clk.t.Add(2 * time.Second)
	if _, ok := c.PathHistory("acme/widgets", "main", "src/a.go", "since:2026-08-03"); ok {
		t.Fatal("hit past the TTL")
	}
}

func TestSetHistoryTTLZeroKeepsBackstop(t *testing.T) {
	t.Parallel()

	clk := newClock()
	c, _ := Load(filepath.Join(t.TempDir(), "cache.json"), clk.now)
	c.SetHistoryTTL(0)
	c.PutPathHistory("acme/widgets", "main", "src/a.go", "head1", gh.PathHistory{Authors: []string{"alice"}})
	clk.t = clk.t.Add(HistoryTTL - time.Second)
	if _, ok := c.PathHistory("acme/widgets", "main", "src/a.go", "head1"); !ok {
		t.Fatal("TTL 0: per-head entry expired before HistoryTTL")
	}
}

func TestSavePrunesHistoryOlderThanTTL(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.SetHistoryTTL(6 * time.Hour)
	c.PutPathHistory("acme/widgets", "main", "src/old.go", "since:2026-08-03", gh.PathHistory{})
	clk.t = clk.t.Add(7 * time.Hour)
	c.PutPathHistory("acme/widgets", "main", "src/new.go", "since:2026-08-03", gh.PathHistory{})
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	next, _ := Load(path, clk.now)
	next.SetHistoryTTL(48 * time.Hour)
	if _, ok := next.PathHistory("acme/widgets", "main", "src/old.go", "since:2026-08-03"); ok {
		t.Fatal("expired entry survived Save")
	}
	if _, ok := next.PathHistory("acme/widgets", "main", "src/new.go", "since:2026-08-03"); !ok {
		t.Fatal("fresh entry dropped by Save")
	}
}

func TestSavePrunesUnknownKeyFormats(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	at := clk.t.Format(time.RFC3339)
	raw := `{"history":{` +
		`"acme/widgets@main\u0000src/a.go\u00000123456789abcdef0123456789abcdef01234567":{"at":"` + at + `","value":{}},` +
		`"acme/widgets@main\u0000src/b.go\u0000since:2026-08-03":{"at":"` + at + `","value":{}},` +
		`"acme/widgets@main\u0000src/c.go":{"at":"` + at + `","value":{}}},` +
		`"rollups":{"acme/widgets#1":{"at":"` + at + `"},"garbage":{"at":"` + at + `"}},` +
		`"loads":{"acme/bob":{"at":"` + at + `","count":2},"bob":{"at":"` + at + `","count":3}}}`
	if err := os.WriteFile(path, []byte(raw), 0o600); err != nil {
		t.Fatal(err)
	}
	c, err := Load(path, clk.now)
	if err != nil {
		t.Fatal(err)
	}
	c.SetHistoryTTL(6 * time.Hour)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	next, err := Load(path, clk.now)
	if err != nil {
		t.Fatal(err)
	}
	if got := slices.Sorted(maps.Keys(next.data.History)); !slices.Equal(got, []string{"acme/widgets@main\x00src/b.go\x00since:2026-08-03"}) {
		t.Fatalf("history keys = %q, want only the day-keyed entry", got)
	}
	if got := slices.Sorted(maps.Keys(next.data.Rollups)); !slices.Equal(got, []string{"acme/widgets#1"}) {
		t.Fatalf("rollup keys = %q", got)
	}
	if got := slices.Sorted(maps.Keys(next.data.Loads)); !slices.Equal(got, []string{"acme/bob"}) {
		t.Fatalf("load keys = %q", got)
	}
}

func TestSavePrunesDayKeysWhenKeyedByHead(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.SetHistoryTTL(0)
	c.PutPathHistory("acme/widgets", "main", "src/a.go", "since:2026-08-03", gh.PathHistory{})
	c.PutPathHistory("acme/widgets", "main", "src/b.go", "head1", gh.PathHistory{})
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}
	next, _ := Load(path, clk.now)
	if _, ok := next.PathHistory("acme/widgets", "main", "src/a.go", "since:2026-08-03"); ok {
		t.Fatal("day-keyed entry survived Save while history is keyed by head")
	}
	if _, ok := next.PathHistory("acme/widgets", "main", "src/b.go", "head1"); !ok {
		t.Fatal("head-keyed entry dropped")
	}
}

func TestReviewLoadExpiresAfterLoadTTL(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.SetLoadTTL(time.Hour)
	c.PutReviewLoad("acme", "Bob", 3)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	clk.t = clk.t.Add(59 * time.Minute)
	next, _ := Load(path, clk.now)
	next.SetLoadTTL(time.Hour)
	if n, ok := next.ReviewLoad("acme", "bob"); !ok || n != 3 {
		t.Fatalf("ReviewLoad = %d, %v; want 3 within the TTL", n, ok)
	}
	clk.t = clk.t.Add(2 * time.Minute)
	if _, ok := next.ReviewLoad("acme", "bob"); ok {
		t.Fatal("hit past the load TTL")
	}
	if err := next.Save(); err != nil {
		t.Fatal(err)
	}
	if len(next.data.Loads) != 0 {
		t.Fatalf("loads = %v, want the expired entry pruned", next.data.Loads)
	}
}

func TestTeamMembersExpireAfterMembersTTL(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.SetMembersTTL(24 * time.Hour)
	want := []gh.TeamMember{{Login: "bob", Type: "User"}}
	c.PutTeamMembers("acme", "foo", want)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	clk.t = clk.t.Add(23 * time.Hour)
	next, _ := Load(path, clk.now)
	next.SetMembersTTL(24 * time.Hour)
	if got, ok := next.TeamMembers("acme", "foo"); !ok || !slices.Equal(got, want) {
		t.Fatalf("TeamMembers = %v, %v; want %v within the TTL", got, ok, want)
	}
	if _, ok := next.TeamMembers("acme", "bar"); ok {
		t.Fatal("hit for another team")
	}
	clk.t = clk.t.Add(2 * time.Hour)
	if _, ok := next.TeamMembers("acme", "foo"); ok {
		t.Fatal("hit past the members TTL")
	}
	if err := next.Save(); err != nil {
		t.Fatal(err)
	}
	if len(next.data.Members) != 0 {
		t.Fatalf("members = %v, want the expired entry pruned", next.data.Members)
	}
}

func TestChangedFilesNeedSameHead(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.PutChangedFiles("acme/widgets", 7, "head1", []string{"a.go"})
	c.PutChangedFiles("acme/widgets", 8, "", []string{"b.go"})
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	clk.t = clk.t.Add(KeepTTL - time.Minute)
	next, _ := Load(path, clk.now)
	if got, ok := next.ChangedFiles("acme/widgets", 7, "head1"); !ok || !slices.Equal(got, []string{"a.go"}) {
		t.Fatalf("ChangedFiles = %v, %v; want a.go for the same head", got, ok)
	}
	if _, ok := next.ChangedFiles("acme/widgets", 7, "head2"); ok {
		t.Fatal("hit for a new head")
	}
	if _, ok := next.ChangedFiles("acme/widgets", 8, ""); ok {
		t.Fatal("hit without a head")
	}
	clk.t = clk.t.Add(2 * time.Minute)
	if _, ok := next.ChangedFiles("acme/widgets", 7, "head1"); ok {
		t.Fatal("hit past KeepTTL")
	}
}

func TestReviewThreadsNeedSameHeadAndUpdatedAt(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	want := []gh.Thread{{ID: "PRRT_1", Comments: []gh.ThreadComment{{ID: "PRRC_1", Author: &gh.ThreadAuthor{Login: "bob"}, Body: "fix"}}}}
	c.PutReviewThreads("acme/widgets", 7, "head1", "t1", want)
	c.PutReviewThreads("acme/widgets", 8, "", "t1", want)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	next, _ := Load(path, clk.now)
	got, ok := next.ReviewThreads("acme/widgets", 7, "head1", "t1")
	if !ok || len(got) != 1 || got[0].ID != "PRRT_1" || got[0].Comments[0].Author.Login != "bob" {
		t.Fatalf("ReviewThreads() = %+v, %v", got, ok)
	}
	for _, tc := range []struct{ head, updated string }{{"head2", "t1"}, {"head1", "t2"}, {"", ""}} {
		if _, ok := next.ReviewThreads("acme/widgets", 7, tc.head, tc.updated); ok {
			t.Fatalf("ReviewThreads(%q, %q) hit, want miss", tc.head, tc.updated)
		}
	}
	if _, ok := next.ReviewThreads("acme/widgets", 8, "", "t1"); ok {
		t.Fatal("hit for an entry stored without a head")
	}
}

func TestReviewThreadsExpireAfterThreadsTTL(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.SetThreadsTTL(time.Hour)
	c.PutReviewThreads("acme/widgets", 7, "head1", "t1", []gh.Thread{})
	clk.t = clk.t.Add(59 * time.Minute)
	if _, ok := c.ReviewThreads("acme/widgets", 7, "head1", "t1"); !ok {
		t.Fatal("miss inside the TTL")
	}
	clk.t = clk.t.Add(2 * time.Minute)
	if _, ok := c.ReviewThreads("acme/widgets", 7, "head1", "t1"); ok {
		t.Fatal("hit past the TTL")
	}
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}
	if len(c.data.Threads) != 0 {
		t.Fatalf("threads = %v, want the expired entry pruned", c.data.Threads)
	}
}

func TestDefaultThreadsTTL(t *testing.T) {
	t.Parallel()

	clk := newClock()
	c, _ := Load(filepath.Join(t.TempDir(), "cache.json"), clk.now)
	c.SetThreadsTTL(0)
	c.PutReviewThreads("acme/widgets", 7, "head1", "t1", nil)
	clk.t = clk.t.Add(ThreadsTTL - time.Second)
	if _, ok := c.ReviewThreads("acme/widgets", 7, "head1", "t1"); !ok {
		t.Fatal("miss inside ThreadsTTL")
	}
	clk.t = clk.t.Add(2 * time.Second)
	if _, ok := c.ReviewThreads("acme/widgets", 7, "head1", "t1"); ok {
		t.Fatal("hit past ThreadsTTL")
	}
}

func TestPruneReviewThreadsKeepsOnlyOpenPRs(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.PutReviewThreads("acme/widgets", 7, "h", "t", nil)
	c.PutReviewThreads("acme/widgets", 8, "h", "t", nil)
	c.PutReviewThreads("acme/gadgets", 8, "h", "t", nil)
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	next, _ := Load(path, clk.now)
	next.PruneReviewThreads("acme/widgets", []int{7})
	if err := next.Save(); err != nil {
		t.Fatal(err)
	}
	last, _ := Load(path, clk.now)
	if got := slices.Sorted(maps.Keys(last.data.Threads)); !slices.Equal(got, []string{"acme/gadgets#8", "acme/widgets#7"}) {
		t.Fatalf("thread keys = %q, want the closed acme/widgets#8 pruned", got)
	}
}

func TestReviewSnapshotNeedsSameHeadAndUpdatedAt(t *testing.T) {
	t.Parallel()

	c, _ := Load(filepath.Join(t.TempDir(), "cache.json"), newClock().now)
	reviews := []gh.LatestReview{{Author: gh.ReviewAuthor{Login: "bob"}, State: "APPROVED"}}
	c.PutReviewSnapshot("acme/widgets", 7, "head1", "t1", "alice", reviews)
	author, got, ok := c.ReviewSnapshot("acme/widgets", 7, "head1", "t1")
	if !ok || author != "alice" || !slices.Equal(got, reviews) {
		t.Fatalf("ReviewSnapshot = %q, %v, %v; want the stored snapshot", author, got, ok)
	}
	if _, _, ok := c.ReviewSnapshot("acme/widgets", 7, "head2", "t1"); ok {
		t.Fatal("hit for a new head")
	}
	if _, _, ok := c.ReviewSnapshot("acme/widgets", 7, "head1", "t2"); ok {
		t.Fatal("hit for a new updatedAt")
	}
	c.PutReviewSnapshot("acme/widgets", 8, "", "t1", "alice", reviews)
	if _, _, ok := c.ReviewSnapshot("acme/widgets", 8, "", "t1"); ok {
		t.Fatal("hit without a head")
	}
}

func TestCodeownersKeepsLocationPastTTL(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	c, _ := Load(path, clk.now)
	c.SetCodeownersTTL(6 * time.Hour)
	c.PutCodeowners("acme/widgets", "main", "CODEOWNERS", "* @acme/foo\n")
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}

	clk.t = clk.t.Add(5 * time.Hour)
	next, _ := Load(path, clk.now)
	next.SetCodeownersTTL(6 * time.Hour)
	if body, at, ok := next.Codeowners("acme/widgets", "main"); !ok || body != "* @acme/foo\n" || at != "CODEOWNERS" {
		t.Fatalf("Codeowners = %q, %q, %v; want the fresh body", body, at, ok)
	}
	clk.t = clk.t.Add(2 * time.Hour)
	if err := next.Save(); err != nil {
		t.Fatal(err)
	}
	last, _ := Load(path, clk.now)
	last.SetCodeownersTTL(6 * time.Hour)
	body, at, ok := last.Codeowners("acme/widgets", "main")
	if ok || body != "" || at != "CODEOWNERS" {
		t.Fatalf("Codeowners = %q, %q, %v; want only the location past the TTL", body, at, ok)
	}
	clk.t = clk.t.Add(KeepTTL)
	if _, at, _ := last.Codeowners("acme/widgets", "main"); at != "" {
		t.Fatalf("location = %q past KeepTTL, want none", at)
	}
}

func TestSaveKeepsTeamCoverKeysAndPrunesUnknown(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache.json")
	clk := newClock()
	at := clk.t.Format(time.RFC3339)
	raw := `{"history":{},"rollups":{},` +
		`"members":{"acme/foo":{"at":"` + at + `"},"foo":{"at":"` + at + `"}},` +
		`"files":{"acme/widgets#1":{"at":"` + at + `","head":"h"},"acme/widgets":{"at":"` + at + `"}},` +
		`"codeowners":{"acme/widgets@main":{"at":"` + at + `"},"acme/widgets":{"at":"` + at + `"}},` +
		`"reviews":{"acme/widgets#1":{"at":"` + at + `","head":"h"},"x#y":{"at":"` + at + `"}}}`
	if err := os.WriteFile(path, []byte(raw), 0o600); err != nil {
		t.Fatal(err)
	}
	c, err := Load(path, clk.now)
	if err != nil {
		t.Fatal(err)
	}
	if err := c.Save(); err != nil {
		t.Fatal(err)
	}
	next, err := Load(path, clk.now)
	if err != nil {
		t.Fatal(err)
	}
	want := map[string][]string{
		"members":    {"acme/foo"},
		"files":      {"acme/widgets#1"},
		"codeowners": {"acme/widgets@main"},
		"reviews":    {"acme/widgets#1"},
	}
	got := map[string][]string{
		"members":    slices.Sorted(maps.Keys(next.data.Members)),
		"files":      slices.Sorted(maps.Keys(next.data.Files)),
		"codeowners": slices.Sorted(maps.Keys(next.data.Codeowners)),
		"reviews":    slices.Sorted(maps.Keys(next.data.Reviews)),
	}
	for name := range want {
		if !slices.Equal(got[name], want[name]) {
			t.Fatalf("%s keys = %q, want %q", name, got[name], want[name])
		}
	}
}
