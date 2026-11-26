package store_test

import (
	"context"
	"database/sql"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestClaimLaneRoundTrip(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	at := time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)
	mac := claimAt("PRJ-1", at, store.Started)
	mac.Lane = "mac"
	if err := s.InsertClaim(ctx, mac); err != nil {
		t.Fatalf("InsertClaim: %v", err)
	}
	local := claimAt("PRJ-2", at, store.Started)
	if err := s.InsertClaim(ctx, local); err != nil {
		t.Fatalf("InsertClaim: %v", err)
	}
	got, err := s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil || got.Lane != "mac" {
		t.Fatalf("LatestClaim = %+v, %v", got, err)
	}
	active, err := s.ActiveClaims(ctx)
	if err != nil || len(active) != 2 {
		t.Fatalf("ActiveClaims = %+v, %v", active, err)
	}
	for _, claim := range active {
		want := ""
		if claim.ID == "PRJ-1" {
			want = "mac"
		}
		if claim.Lane != want {
			t.Fatalf("claim %s lane = %q, want %q", claim.ID, claim.Lane, want)
		}
	}
}

func TestMigrateV6AddsLane(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	v6 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, due TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE INDEX links_blocker ON links(source,blocker_id);
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT, last_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, prs_json TEXT NOT NULL DEFAULT '',
  stash_count INTEGER, PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (6);
INSERT INTO claims(source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at)
  VALUES ('jira:acme', 'PRJ-1', '2026-10-01T00:00:00.000000000Z', 'laptop', 't', 'w', 'started', '',
    '2026-10-01T00:00:00.000000000Z');`
	if _, err := db.Exec(v6); err != nil {
		t.Fatalf("v6 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	s := openAt(t, path)
	got, err := s.LatestClaim(context.Background(), "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim: %v", err)
	}
	if got.Lane != "" || got.State != store.Started {
		t.Fatalf("migrated claim = %+v", got)
	}
}
