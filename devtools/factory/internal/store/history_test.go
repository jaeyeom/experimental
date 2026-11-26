package store_test

import (
	"context"
	"database/sql"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

var histT0 = time.Date(2026, 11, 1, 9, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

func TestUpsertPRsNeverMovesBackwards(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	open := store.PR{
		Repo: "acme/widgets", Number: 3, Source: "jira:acme", ItemID: "PRJ-1",
		OpenedAt: histT0, Draft: true, LastSeen: histT0.Add(time.Hour),
	}
	if err := s.UpsertPRs(ctx, []store.PR{open}); err != nil {
		t.Fatal(err)
	}
	merged := open
	merged.Draft = false
	merged.MergedAt = histT0.Add(2 * time.Hour)
	merged.ClosedAt = merged.MergedAt
	merged.LastSeen = histT0.Add(3 * time.Hour)
	if err := s.UpsertPRs(ctx, []store.PR{merged}); err != nil {
		t.Fatal(err)
	}
	// An older observation, with a later open time, arrives last.
	stale := open
	stale.OpenedAt = histT0.Add(time.Minute)
	stale.Source, stale.ItemID = "jira:other", "PRJ-2"
	if err := s.UpsertPRs(ctx, []store.PR{stale}); err != nil {
		t.Fatal(err)
	}
	got, err := s.PR(ctx, "acme/widgets", 3)
	if err != nil {
		t.Fatal(err)
	}
	if !got.OpenedAt.Equal(histT0) || !got.MergedAt.Equal(merged.MergedAt) || !got.ClosedAt.Equal(merged.MergedAt) {
		t.Fatalf("times = %+v", got)
	}
	if got.Draft || !got.LastSeen.Equal(merged.LastSeen) {
		t.Fatalf("draft %t last_seen %s, want false and %s", got.Draft, got.LastSeen, merged.LastSeen)
	}
	if got.Source != "jira:acme" || got.ItemID != "PRJ-1" {
		t.Fatalf("item = %s/%s, want the first one", got.Source, got.ItemID)
	}
	openItems, err := s.OpenPRItems(ctx, "jira:acme")
	if err != nil || len(openItems) != 0 {
		t.Fatalf("OpenPRItems = %v, %v", openItems, err)
	}
}

func TestUpsertPRsRejectsBadRows(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ok := store.PR{Repo: "acme/widgets", Number: 1, OpenedAt: histT0, LastSeen: histT0}
	tests := []struct {
		name string
		edit func(*store.PR)
		want string
	}{
		{"bad repo", func(p *store.PR) { p.Repo = "widgets" }, "owner/repo#N"},
		{"zero last seen", func(p *store.PR) { p.LastSeen = time.Time{} }, "last_seen"},
		{"zero opened", func(p *store.PR) { p.OpenedAt = time.Time{} }, "opened_at"},
		{"half an item", func(p *store.PR) { p.Source = "jira:acme" }, "both item source and id"},
	}
	for _, tc := range tests {
		pr := ok
		tc.edit(&pr)
		err := s.UpsertPRs(context.Background(), []store.PR{pr})
		if err == nil || !strings.Contains(err.Error(), tc.want) {
			t.Fatalf("%s: error = %v, want %q", tc.name, err, tc.want)
		}
	}
}

func TestClaimRecordsAgentRuleAndModel(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	c := claimAt("PRJ-1", histT0, store.Started)
	c.AgentRule, c.Model = "heavy", "opus"
	if err := s.InsertClaim(ctx, c); err != nil {
		t.Fatal(err)
	}
	got, err := s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil || got.AgentRule != "heavy" || got.Model != "opus" {
		t.Fatalf("LatestClaim = %+v, %v", got, err)
	}
	bad := claimAt("PRJ-2", histT0, store.Started)
	bad.Model = "opus"
	if err := s.InsertClaim(ctx, bad); err == nil || !strings.Contains(err.Error(), "agent rule") {
		t.Fatalf("model without rule: error = %v", err)
	}
}

func TestFinishedFromClaimDone(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	it := fullItem("PRJ-1")
	if err := s.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
		t.Fatal(err)
	}
	c := claimAt("PRJ-1", histT0, store.Started)
	if err := s.InsertClaim(ctx, c); err != nil {
		t.Fatal(err)
	}
	if err := s.UpdateClaim(ctx, "jira:acme", "PRJ-1", histT0, store.HandedOff, "", "", ""); err != nil {
		t.Fatal(err)
	}
	if rows := mustFinished(t, s); len(rows) != 0 {
		t.Fatalf("finished before done = %+v", rows)
	}
	if err := s.UpdateClaim(ctx, "jira:acme", "PRJ-1", histT0, store.Done, "", "", ""); err != nil {
		t.Fatal(err)
	}
	rows := mustFinished(t, s)
	if len(rows) != 1 || rows[0].Title != it.Title || rows[0].Parent != "PRJ-9" || len(rows[0].Labels) != 2 ||
		!rows[0].ClaimedAt.Equal(histT0) || rows[0].FinishedAt.IsZero() {
		t.Fatalf("finished = %+v", rows)
	}
}

// TestFinishedSurvivesFullSyncDelete: an item finished in a sync, then
// deleted from items by a full sync, keeps its finished row.
func TestFinishedSurvivesFullSyncDelete(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	open := fullItem("PRJ-1")
	fresh := fullItem("PRJ-2")
	fresh.Done = true // First mirrored as done: not a finish this mirror saw.
	if err := s.UpsertItems(ctx, "jira:acme", []source.Item{open, fresh}); err != nil {
		t.Fatal(err)
	}
	if err := s.InsertClaim(ctx, claimAt("PRJ-1", histT0, store.HandedOff)); err != nil {
		t.Fatal(err)
	}
	done := open
	done.Done = true
	done.Updated = histT0.Add(5 * time.Hour)
	if err := s.UpsertItems(ctx, "jira:acme", []source.Item{done}); err != nil {
		t.Fatal(err)
	}
	if _, err := s.DeleteItemsNotIn(ctx, "jira:acme", map[string]bool{}); err != nil {
		t.Fatal(err)
	}
	rows := mustFinished(t, s)
	if len(rows) != 1 || rows[0].ID != "PRJ-1" || !rows[0].FinishedAt.Equal(done.Updated) || !rows[0].ClaimedAt.Equal(histT0) {
		t.Fatalf("finished = %+v", rows)
	}
	// The claim later reaching done keeps the first finish time.
	if err := s.UpdateClaim(ctx, "jira:acme", "PRJ-1", histT0, store.Done, "", "", ""); err != nil {
		t.Fatal(err)
	}
	if again := mustFinished(t, s); len(again) != 1 || !again[0].FinishedAt.Equal(done.Updated) {
		t.Fatalf("finished after claim done = %+v", again)
	}
}

func TestBacklogSnapshotHourlyAndPruned(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	snap := func(at time.Time, ready int) store.BacklogSnapshot {
		return store.BacklogSnapshot{Source: "jira:acme", TakenAt: at, Ready: ready, ByStatus: map[string]int{"To Do": ready}}
	}
	const keep = 90 * 24 * time.Hour
	old := histT0.Add(-keep - time.Hour)
	if ok, err := s.RecordBacklogSnapshot(ctx, snap(old, 9), time.Hour, keep); err != nil || !ok {
		t.Fatalf("old snapshot = %t, %v", ok, err)
	}
	if ok, err := s.RecordBacklogSnapshot(ctx, snap(histT0, 1), time.Hour, keep); err != nil || !ok {
		t.Fatalf("first snapshot = %t, %v", ok, err)
	}
	if ok, err := s.RecordBacklogSnapshot(ctx, snap(histT0.Add(59*time.Minute), 2), time.Hour, keep); err != nil || ok {
		t.Fatalf("second snapshot within the hour = %t, %v", ok, err)
	}
	got, err := s.BacklogSnapshots(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].Ready != 1 || !got[0].TakenAt.Equal(histT0) || got[0].ByStatus["To Do"] != 1 {
		t.Fatalf("snapshots = %+v", got)
	}
	if ok, err := s.RecordBacklogSnapshot(ctx, snap(histT0.Add(time.Hour), 3), time.Hour, keep); err != nil || !ok {
		t.Fatalf("snapshot an hour later = %t, %v", ok, err)
	}
}

func TestMigrateV7KeepsClaims(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatal(err)
	}
	v7 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, due TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT, last_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, prs_json TEXT NOT NULL DEFAULT '',
  stash_count INTEGER, lane TEXT NOT NULL DEFAULT '', PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (7);
INSERT INTO claims(source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at)
  VALUES ('jira:acme', 'PRJ-1', '2026-10-01T00:00:00.000000000Z', 'laptop', 't', 'w', 'done', '',
    '2026-10-01T00:00:00.000000000Z');`
	if _, err := db.Exec(v7); err != nil {
		t.Fatalf("v7 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatal(err)
	}
	s := openAt(t, path)
	got, err := s.LatestClaim(context.Background(), "jira:acme", "PRJ-1")
	if err != nil || got.AgentRule != "" || got.Model != "" || got.State != store.Done {
		t.Fatalf("migrated claim = %+v, %v", got, err)
	}
	if rows := mustFinished(t, s); len(rows) != 0 {
		t.Fatalf("finished after migration = %+v, want none", rows)
	}
}

func mustFinished(t *testing.T, s *store.Store) []store.Finished {
	t.Helper()
	rows, err := s.FinishedItems(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	return rows
}
