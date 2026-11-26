package store

import (
	"context"
	"database/sql"
	"fmt"
	"strings"
)

// migrations are applied in order. Version n is migrations[n-1].
// v1 is the mirror schema from the store issue.
// v2 adds sync_state.me, the account recorded when a source syncs.
// v3 adds sync_state.last_sync, the time of the last successful sync.
// v4 adds claims.prs_json, the pull requests recorded with a claim.
// v5 adds claims.stash_count, the stash list length recorded at start.
// NULL means start did not record it.
// v6 adds items.due, a nullable YYYY-MM-DD. NULL means no due date.
// v7 adds claims.lane, the [[lane]] name start recorded. Empty means no lane
// was configured, which is the local herdr server.
// v8 adds the prs table: one row per observed pull request, keyed by repo
// and number. Times are NULL until seen and never move backwards.
// v9 adds claims.agent_rule and claims.model. Empty agent_rule means start
// did not record it, which is every claim from before v9.
// v10 adds the finished table: items that reached done, kept when a sync
// later deletes the item from items.
// v11 adds backlog_snapshots: per-source backlog counts after a sync.
// v12 adds items.not_before, a nullable YYYY-MM-DD deferral date.
// v13 adds external_blockers: blockers of mirrored items that are not in the
// mirror, fetched by id during sync. They are never work items.
// v14 adds claims.spawner, claims.worker_handle, claims.worktree, and
// claims.output_mode to record how a claim was started and where its checkout lives.
// v15 adds items.author, items.author_association, and items.locked for
// per-source trust classification and untrusted-author gating.
var migrations = []string{
	`CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
    done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
    created TEXT, updated TEXT, PRIMARY KEY(source,id));
  CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
    PRIMARY KEY(source,blocker_id,blocked_id));
  CREATE INDEX links_blocker ON links(source,blocker_id);
  CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT);
  CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
    workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, PRIMARY KEY(source,id,claimed_at));
  CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);`,
	`ALTER TABLE sync_state ADD COLUMN me TEXT`,
	`ALTER TABLE sync_state ADD COLUMN last_sync TEXT`,
	`ALTER TABLE claims ADD COLUMN prs_json TEXT NOT NULL DEFAULT ''`,
	`ALTER TABLE claims ADD COLUMN stash_count INTEGER`,
	`ALTER TABLE items ADD COLUMN due TEXT`,
	`ALTER TABLE claims ADD COLUMN lane TEXT NOT NULL DEFAULT ''`,
	`CREATE TABLE prs(repo TEXT NOT NULL, number INTEGER NOT NULL, source TEXT NOT NULL DEFAULT '',
    item_id TEXT NOT NULL DEFAULT '', opened_at TEXT, merged_at TEXT, closed_at TEXT,
    draft INTEGER NOT NULL DEFAULT 0, last_seen TEXT NOT NULL, PRIMARY KEY(repo,number));
  CREATE INDEX prs_item ON prs(source,item_id)`,
	`ALTER TABLE claims ADD COLUMN agent_rule TEXT NOT NULL DEFAULT '';
  ALTER TABLE claims ADD COLUMN model TEXT NOT NULL DEFAULT ''`,
	`CREATE TABLE finished(source TEXT NOT NULL, id TEXT NOT NULL, title TEXT NOT NULL DEFAULT '',
    parent TEXT NOT NULL DEFAULT '', labels_json TEXT NOT NULL DEFAULT 'null',
    finished_at TEXT NOT NULL, claimed_at TEXT, PRIMARY KEY(source,id))`,
	`CREATE TABLE backlog_snapshots(source TEXT NOT NULL, taken_at TEXT NOT NULL,
    ready INTEGER NOT NULL, blocked INTEGER NOT NULL, in_flight INTEGER NOT NULL,
    needs_human INTEGER NOT NULL, by_status_json TEXT NOT NULL, PRIMARY KEY(source,taken_at))`,
	`ALTER TABLE items ADD COLUMN not_before TEXT`,
	`CREATE TABLE external_blockers(source TEXT NOT NULL, id TEXT NOT NULL, status TEXT NOT NULL,
    done INTEGER NOT NULL DEFAULT 0, state TEXT NOT NULL DEFAULT '', assignee TEXT NOT NULL DEFAULT '',
    title TEXT NOT NULL DEFAULT '', reason TEXT NOT NULL DEFAULT '', fetched_at TEXT NOT NULL,
    PRIMARY KEY(source,id))`,
	`ALTER TABLE claims ADD COLUMN spawner TEXT NOT NULL DEFAULT '';
  ALTER TABLE claims ADD COLUMN worker_handle TEXT NOT NULL DEFAULT '';
  ALTER TABLE claims ADD COLUMN worktree TEXT NOT NULL DEFAULT '';
  ALTER TABLE claims ADD COLUMN output_mode TEXT NOT NULL DEFAULT ''`,
	`ALTER TABLE items ADD COLUMN author TEXT NOT NULL DEFAULT '';
  ALTER TABLE items ADD COLUMN author_association TEXT NOT NULL DEFAULT '';
  ALTER TABLE items ADD COLUMN locked INTEGER NOT NULL DEFAULT 0`,
}

func (s *Store) migrate(ctx context.Context) error {
	return s.withTx(ctx, func(tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `CREATE TABLE IF NOT EXISTS schema_version(v INTEGER)`); err != nil {
			return fmt.Errorf("create schema_version: %w", err)
		}
		var current int
		if err := tx.QueryRowContext(ctx, `SELECT COALESCE(MAX(v), 0) FROM schema_version`).Scan(&current); err != nil {
			return fmt.Errorf("read schema_version: %w", err)
		}
		if current > len(migrations) {
			return fmt.Errorf("schema version: expected <= %d, found %d", len(migrations), current)
		}
		for i := current; i < len(migrations); i++ {
			version := i + 1
			if err := execScript(ctx, tx, migrations[i]); err != nil {
				return fmt.Errorf("migration v%d: %w", version, err)
			}
			if _, err := tx.ExecContext(ctx, `INSERT INTO schema_version(v) VALUES (?)`, version); err != nil {
				return fmt.Errorf("record schema version %d: %w", version, err)
			}
		}
		return nil
	})
}

func execScript(ctx context.Context, tx *sql.Tx, script string) error {
	for i, stmt := range splitSQL(script) {
		if _, err := tx.ExecContext(ctx, stmt); err != nil {
			return fmt.Errorf("statement %d: %w", i+1, err)
		}
	}
	return nil
}

func splitSQL(script string) []string {
	parts := strings.Split(script, ";")
	out := make([]string, 0, len(parts))
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part != "" {
			out = append(out, part)
		}
	}
	return out
}
