package store_test

import (
	"context"
	"database/sql"
	"errors"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestClaimMetadataRoundTripAndUpdates(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	at := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)

	stash := 2
	c := claimAt("PRJ-1", at, store.Started)
	c.Lane = "mac"
	c.StashCount = &stash
	c.AgentRule = "default"
	c.Model = "claude-sonnet"
	c.PRs = []store.ClaimPR{{
		Repo:   "acme/repo",
		Number: 42,
		URL:    "https://github.com/acme/repo/pull/42",
		Title:  "Fix bug",
		Head:   "fix-bug",
	}}
	c.Spawner = "agentbus"
	c.WorkerHandle = "handle-1"
	c.Worktree = "/tmp/wt/prj-1"
	c.OutputMode = "merge"

	if err := s.InsertClaim(ctx, c); err != nil {
		t.Fatalf("InsertClaim: %v", err)
	}

	assertFields := func(got store.Claim, wantHandle, wantWorktree string) {
		t.Helper()
		if got.Spawner != "agentbus" || got.WorkerHandle != wantHandle || got.Worktree != wantWorktree || got.OutputMode != "merge" {
			t.Fatalf("claim metadata = (%q, %q, %q, %q), want (%q, %q, %q, %q)",
				got.Spawner, got.WorkerHandle, got.Worktree, got.OutputMode,
				"agentbus", wantHandle, wantWorktree, "merge")
		}
		if got.TabID != "tab" || got.WorkspaceID != "ws" || got.Lane != "mac" ||
			got.StashCount == nil || *got.StashCount != 2 ||
			got.AgentRule != "default" || got.Model != "claude-sonnet" ||
			len(got.PRs) != 1 || got.PRs[0].Number != 42 {
			t.Fatalf("existing fields altered: %+v", got)
		}
	}

	got, err := s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim: %v", err)
	}
	assertFields(got, "handle-1", "/tmp/wt/prj-1")

	active, err := s.ActiveClaims(ctx)
	if err != nil || len(active) != 1 {
		t.Fatalf("ActiveClaims = %+v, %v", active, err)
	}
	assertFields(active[0], "handle-1", "/tmp/wt/prj-1")

	if err := s.UpdateClaimWorktree(ctx, "jira:acme", "PRJ-1", at, "/tmp/wt/prj-1-v2"); err != nil {
		t.Fatalf("UpdateClaimWorktree: %v", err)
	}
	if err := s.UpdateClaimWorkerHandle(ctx, "jira:acme", "PRJ-1", at, "handle-2"); err != nil {
		t.Fatalf("UpdateClaimWorkerHandle: %v", err)
	}

	got, err = s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim after update: %v", err)
	}
	assertFields(got, "handle-2", "/tmp/wt/prj-1-v2")

	active, err = s.ActiveClaims(ctx)
	if err != nil || len(active) != 1 {
		t.Fatalf("ActiveClaims after update = %+v, %v", active, err)
	}
	assertFields(active[0], "handle-2", "/tmp/wt/prj-1-v2")

	if err := s.UpdateClaimWorktree(ctx, "jira:acme", "MISSING", at, "/tmp/x"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("UpdateClaimWorktree missing err = %v, want ErrNotFound", err)
	}
	if err := s.UpdateClaimWorkerHandle(ctx, "jira:acme", "MISSING", at, "h"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("UpdateClaimWorkerHandle missing err = %v, want ErrNotFound", err)
	}
	if err := s.UpdateClaimWorktree(ctx, "", "PRJ-1", at, "/tmp/x"); err == nil {
		t.Fatal("UpdateClaimWorktree empty source: expected error")
	}
	if err := s.UpdateClaimWorkerHandle(ctx, "jira:acme", "", at, "h"); err == nil {
		t.Fatal("UpdateClaimWorkerHandle empty id: expected error")
	}
}

func TestMigrateV13AddsClaimMetadata(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	v13 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, due TEXT, not_before TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE INDEX links_blocker ON links(source,blocker_id);
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT, last_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, prs_json TEXT NOT NULL DEFAULT '',
  stash_count INTEGER, lane TEXT NOT NULL DEFAULT '', agent_rule TEXT NOT NULL DEFAULT '',
  model TEXT NOT NULL DEFAULT '', PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE prs(repo TEXT NOT NULL, number INTEGER NOT NULL, source TEXT NOT NULL DEFAULT '',
  item_id TEXT NOT NULL DEFAULT '', opened_at TEXT, merged_at TEXT, closed_at TEXT,
  draft INTEGER NOT NULL DEFAULT 0, last_seen TEXT NOT NULL, PRIMARY KEY(repo,number));
CREATE INDEX prs_item ON prs(source,item_id);
CREATE TABLE finished(source TEXT NOT NULL, id TEXT NOT NULL, title TEXT NOT NULL DEFAULT '',
  parent TEXT NOT NULL DEFAULT '', labels_json TEXT NOT NULL DEFAULT 'null',
  finished_at TEXT NOT NULL, claimed_at TEXT, PRIMARY KEY(source,id));
CREATE TABLE backlog_snapshots(source TEXT NOT NULL, taken_at TEXT NOT NULL,
  ready INTEGER NOT NULL, blocked INTEGER NOT NULL, in_flight INTEGER NOT NULL,
  needs_human INTEGER NOT NULL, by_status_json TEXT NOT NULL, PRIMARY KEY(source,taken_at));
CREATE TABLE external_blockers(source TEXT NOT NULL, id TEXT NOT NULL, status TEXT NOT NULL,
  done INTEGER NOT NULL DEFAULT 0, state TEXT NOT NULL DEFAULT '', assignee TEXT NOT NULL DEFAULT '',
  title TEXT NOT NULL DEFAULT '', reason TEXT NOT NULL DEFAULT '', fetched_at TEXT NOT NULL,
  PRIMARY KEY(source,id));
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (13);
INSERT INTO claims(source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at,
  prs_json, stash_count, lane, agent_rule, model)
  VALUES ('jira:acme', 'PRJ-1', '2026-10-01T00:00:00.000000000Z', 'laptop', 't1', 'w1', 'started', 'working',
    '2026-10-01T00:01:00.000000000Z',
    '[{"repo":"acme/repo","number":7,"url":"https://github.com/acme/repo/pull/7","title":"Legacy"}]',
    3, 'linux', 'default', 'opus');`
	if _, err := db.Exec(v13); err != nil {
		t.Fatalf("v13 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	s := openAt(t, path)
	ctx := context.Background()

	checkClaim := func(got store.Claim) {
		t.Helper()
		if got.Spawner != "" || got.WorkerHandle != "" || got.Worktree != "" || got.OutputMode != "" {
			t.Fatalf("legacy claim metadata = (%q, %q, %q, %q), want all empty",
				got.Spawner, got.WorkerHandle, got.Worktree, got.OutputMode)
		}
		if got.TabID != "t1" || got.WorkspaceID != "w1" || got.Lane != "linux" ||
			got.StashCount == nil || *got.StashCount != 3 ||
			got.AgentRule != "default" || got.Model != "opus" ||
			len(got.PRs) != 1 || got.PRs[0].Number != 7 || got.State != store.Started {
			t.Fatalf("legacy claim existing fields = %+v", got)
		}
	}

	got, err := s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim: %v", err)
	}
	checkClaim(got)

	active, err := s.ActiveClaims(ctx)
	if err != nil || len(active) != 1 {
		t.Fatalf("ActiveClaims = %+v, %v", active, err)
	}
	checkClaim(active[0])

	// Reopening an already-migrated DB should be idempotent.
	s2 := openAt(t, path)
	got2, err := s2.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim after reopen: %v", err)
	}
	checkClaim(got2)
}
