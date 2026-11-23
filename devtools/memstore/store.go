package memstore

import (
	"database/sql"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/sqlitedb"
)

// ErrVaultNotInitialized is returned by OpenVault when the vault database
// file does not exist yet.
var ErrVaultNotInitialized = errors.New("vault is not initialized")

// Vault is an open memstore SQLite vault.
type Vault struct {
	db     *sql.DB
	config VaultConfig
	now    func() time.Time
}

// InitVault initializes a SQLite vault at vault.Path, enforcing git work-tree
// and permission guards (0700 parent directory, 0600 database file) and
// applying all pending forward-only schema migrations. Repeating InitVault on an
// already-initialized vault is idempotent.
func InitVault(vault VaultConfig) (*Vault, error) {
	if err := RejectGitWorkTree(vault.Path); err != nil {
		return nil, err
	}
	if err := EnsureSecureDir(filepath.Dir(vault.Path)); err != nil {
		return nil, err
	}
	f, err := os.OpenFile(vault.Path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, fmt.Errorf("create vault %s: %w", vault.Path, err)
	}
	if err := f.Close(); err != nil {
		return nil, fmt.Errorf("close new vault %s: %w", vault.Path, err)
	}
	if err := EnsureSecureFile(vault.Path); err != nil {
		return nil, err
	}
	return openAndMigrate(vault)
}

// OpenVault opens an existing SQLite vault at vault.Path after verifying git
// work-tree and permission guards, and applies any pending migrations. It
// returns ErrVaultNotInitialized if the database file does not exist.
func OpenVault(vault VaultConfig) (*Vault, error) {
	if err := RejectGitWorkTree(vault.Path); err != nil {
		return nil, err
	}
	if _, err := os.Stat(vault.Path); errors.Is(err, fs.ErrNotExist) {
		return nil, ErrVaultNotInitialized
	} else if err != nil {
		return nil, fmt.Errorf("stat vault %s: %w", vault.Path, err)
	}
	if err := EnsureSecureDir(filepath.Dir(vault.Path)); err != nil {
		return nil, err
	}
	if err := EnsureSecureFile(vault.Path); err != nil {
		return nil, err
	}
	return openAndMigrate(vault)
}

func openAndMigrate(vault VaultConfig) (*Vault, error) {
	db, err := sqlitedb.Open(vault.Path, "_pragma=foreign_keys(1)", "_txlock=immediate")
	if err != nil {
		return nil, fmt.Errorf("open vault: %w", err)
	}
	if err := migrate(db, schemaMigrations()); err != nil {
		db.Close()
		return nil, err
	}
	return &Vault{
		db:     db,
		config: vault,
		now:    time.Now,
	}, nil
}

// Close closes the underlying SQLite database connection.
func (v *Vault) Close() error {
	if err := v.db.Close(); err != nil {
		return fmt.Errorf("close vault: %w", err)
	}
	return nil
}

// SetClock replaces the clock the vault uses for timestamps, for tests and
// callers that inject time.
func (v *Vault) SetClock(now func() time.Time) {
	v.now = now
}

// DB returns the underlying *sql.DB handle.
func (v *Vault) DB() *sql.DB {
	return v.db
}

// Config returns the VaultConfig used to open the vault.
func (v *Vault) Config() VaultConfig {
	return v.config
}

// migrate brings db to len(steps). The version check and every step run in
// one immediate transaction so concurrent openers apply each step once.
func migrate(db *sql.DB, steps []string) error {
	tx, err := db.Begin()
	if err != nil {
		return fmt.Errorf("migrate: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	var version int
	if err := tx.QueryRow(`PRAGMA user_version`).Scan(&version); err != nil {
		return fmt.Errorf("migrate: read version: %w", err)
	}
	if version > len(steps) {
		return fmt.Errorf("database version %d is newer than this memstore (%d); upgrade memstore", version, len(steps))
	}
	if version == len(steps) {
		return nil
	}
	for i := version; i < len(steps); i++ {
		if _, err := tx.Exec(steps[i]); err != nil {
			return fmt.Errorf("migrate to version %d: %w", i+1, err)
		}
	}
	if _, err := tx.Exec(fmt.Sprintf(`PRAGMA user_version = %d`, len(steps))); err != nil {
		return fmt.Errorf("migrate: set version: %w", err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("migrate: %w", err)
	}
	return nil
}

// schemaMigrations returns forward-only migrations; migration i brings the
// database to PRAGMA user_version i+1. Never edit or remove an existing step,
// only append.
func schemaMigrations() []string {
	return []string{
		`CREATE TABLE entities (
			id                  TEXT PRIMARY KEY NOT NULL,
			kind                TEXT NOT NULL CHECK (kind IN (
									'user', 'agent', 'person', 'company',
									'site', 'project', 'concept', 'routine'
								)),
			slug                TEXT NOT NULL,
			title               TEXT NOT NULL,
			contacts_ref        TEXT,
			default_compartment TEXT NOT NULL DEFAULT 'general',
			sensitivity         TEXT NOT NULL DEFAULT 'normal' CHECK (
									sensitivity IN ('public', 'normal', 'private', 'restricted')
								),
			summary             TEXT NOT NULL DEFAULT '',
			pinned_l1           INTEGER NOT NULL DEFAULT 0,
			created_at          INTEGER NOT NULL,
			updated_at          INTEGER NOT NULL,
			archived_at         INTEGER,
			UNIQUE (kind, slug)
		);
		CREATE TABLE entity_aliases (
			entity_id        TEXT NOT NULL REFERENCES entities(id) ON DELETE CASCADE,
			alias            TEXT NOT NULL,
			normalized_alias TEXT NOT NULL,
			PRIMARY KEY (entity_id, normalized_alias)
		);
		CREATE INDEX idx_entity_aliases_norm ON entity_aliases(normalized_alias);
		CREATE TABLE facts (
			id            TEXT PRIMARY KEY NOT NULL,
			entity_id     TEXT NOT NULL REFERENCES entities(id) ON DELETE CASCADE,
			key           TEXT NOT NULL,
			value_json    TEXT NOT NULL,
			value_type    TEXT NOT NULL DEFAULT 'string',
			compartment   TEXT NOT NULL DEFAULT 'general',
			sensitivity   TEXT NOT NULL DEFAULT 'normal',
			confidence    REAL NOT NULL DEFAULT 1.0,
			source_type   TEXT NOT NULL,
			source_ref    TEXT NOT NULL DEFAULT '',
			valid_from    INTEGER,
			valid_until   INTEGER,
			superseded_by TEXT REFERENCES facts(id),
			created_at    INTEGER NOT NULL,
			updated_at    INTEGER NOT NULL
		);
		CREATE INDEX idx_facts_entity_active ON facts(entity_id, key) WHERE superseded_by IS NULL;
		CREATE INDEX idx_facts_compartment ON facts(compartment, sensitivity);
		CREATE INDEX idx_facts_valid_until ON facts(valid_until) WHERE valid_until IS NOT NULL;
		CREATE TABLE note_blocks (
			id          TEXT PRIMARY KEY NOT NULL,
			entity_id   TEXT NOT NULL REFERENCES entities(id) ON DELETE CASCADE,
			section_key TEXT NOT NULL,
			heading     TEXT NOT NULL,
			sort_order  INTEGER NOT NULL DEFAULT 100,
			body_md     TEXT NOT NULL,
			compartment TEXT NOT NULL DEFAULT 'general',
			sensitivity TEXT NOT NULL DEFAULT 'normal',
			updated_at  INTEGER NOT NULL,
			UNIQUE (entity_id, section_key)
		);
		CREATE TABLE entity_links (
			from_entity_id TEXT NOT NULL REFERENCES entities(id) ON DELETE CASCADE,
			relation       TEXT NOT NULL,
			to_entity_id   TEXT NOT NULL REFERENCES entities(id) ON DELETE CASCADE,
			compartment    TEXT NOT NULL DEFAULT 'general',
			sensitivity    TEXT NOT NULL DEFAULT 'normal',
			note           TEXT NOT NULL DEFAULT '',
			created_at     INTEGER NOT NULL,
			PRIMARY KEY (from_entity_id, relation, to_entity_id)
		);
		CREATE TABLE episodic_events (
			id                TEXT PRIMARY KEY NOT NULL,
			event_date        TEXT NOT NULL,
			occurred_at       INTEGER NOT NULL,
			entity_id         TEXT REFERENCES entities(id) ON DELETE SET NULL,
			compartment       TEXT NOT NULL DEFAULT 'general',
			sensitivity       TEXT NOT NULL DEFAULT 'normal',
			channel           TEXT NOT NULL DEFAULT '',
			source_ref        TEXT NOT NULL DEFAULT '',
			summary           TEXT NOT NULL,
			details_md        TEXT NOT NULL DEFAULT '',
			reporter          TEXT NOT NULL,
			compacted_into_id TEXT REFERENCES episodic_events(id),
			created_at        INTEGER NOT NULL
		);
		CREATE INDEX idx_episodic_date ON episodic_events(event_date, compartment);
		CREATE INDEX idx_episodic_entity ON episodic_events(entity_id, occurred_at);
		CREATE TABLE revisions (
			id         INTEGER PRIMARY KEY AUTOINCREMENT,
			table_name TEXT NOT NULL,
			row_id     TEXT NOT NULL,
			op         TEXT NOT NULL CHECK (op IN ('insert', 'update', 'delete', 'supersede')),
			diff_json  TEXT NOT NULL,
			actor      TEXT NOT NULL,
			reason     TEXT NOT NULL DEFAULT '',
			created_at INTEGER NOT NULL
		);
		CREATE INDEX idx_revisions_row ON revisions(table_name, row_id, created_at);
		CREATE TABLE triage_inbox (
			id                   TEXT PRIMARY KEY NOT NULL,
			source               TEXT NOT NULL,
			proposed_compartment TEXT NOT NULL,
			proposed_sensitivity TEXT NOT NULL DEFAULT 'normal',
			target_entity_id     TEXT,
			payload_json         TEXT NOT NULL,
			status               TEXT NOT NULL DEFAULT 'pending' CHECK (
									status IN ('pending', 'approved', 'rejected')
								),
			decide_ref           TEXT,
			created_at           INTEGER NOT NULL,
			resolved_at          INTEGER
		);
		CREATE INDEX idx_triage_status ON triage_inbox(status, created_at);
		CREATE TABLE access_log (
			id        INTEGER PRIMARY KEY AUTOINCREMENT,
			ts        INTEGER NOT NULL,
			caller    TEXT NOT NULL,
			profile   TEXT NOT NULL,
			command   TEXT NOT NULL,
			entity_id TEXT NOT NULL DEFAULT '',
			reason    TEXT NOT NULL DEFAULT ''
		);
		CREATE INDEX idx_access_log_ts ON access_log(ts);
		CREATE VIRTUAL TABLE fts_docs USING fts5(
			doc_id UNINDEXED,
			doc_kind UNINDEXED,
			entity_id UNINDEXED,
			compartment UNINDEXED,
			sensitivity_rank UNINDEXED,
			title,
			body,
			tokenize='unicode61'
		);`,
		`CREATE TABLE routine_state (
			routine_id TEXT NOT NULL,
			key        TEXT NOT NULL,
			value_json TEXT NOT NULL,
			updated_at INTEGER NOT NULL,
			PRIMARY KEY (routine_id, key)
		);
		CREATE TABLE routine_runs (
			id              TEXT PRIMARY KEY NOT NULL,
			routine_id      TEXT NOT NULL,
			idempotency_key TEXT NOT NULL,
			trigger         TEXT NOT NULL DEFAULT '',
			started_at      INTEGER NOT NULL,
			finished_at     INTEGER,
			status          TEXT NOT NULL CHECK (
								status IN ('running', 'ok', 'failed', 'skipped')
							),
			exit_code       INTEGER,
			metrics_json    TEXT NOT NULL DEFAULT '',
			error_summary   TEXT NOT NULL DEFAULT '',
			UNIQUE (routine_id, idempotency_key)
		);
		CREATE INDEX idx_routine_runs_routine ON routine_runs(routine_id, started_at);
		CREATE TABLE routine_outbox (
			id           TEXT PRIMARY KEY NOT NULL,
			routine_id   TEXT NOT NULL,
			action_kind  TEXT NOT NULL,
			payload_json TEXT NOT NULL,
			status       TEXT NOT NULL DEFAULT 'pending_approval' CHECK (
							 status IN ('pending_approval', 'approved', 'executed', 'rejected', 'failed')
						 ),
			decide_id    TEXT NOT NULL DEFAULT '',
			created_at   INTEGER NOT NULL,
			executed_at  INTEGER
		);
		CREATE INDEX idx_routine_outbox_status ON routine_outbox(routine_id, status, created_at);`,
		`ALTER TABLE facts ADD COLUMN expired_at INTEGER;`,
		`CREATE TABLE inbox_receipts (
			envelope_id TEXT PRIMARY KEY NOT NULL,
			ingested_at INTEGER NOT NULL
		);`,
	}
}
