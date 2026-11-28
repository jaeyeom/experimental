package memstore

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/chorong-sh/chorong/pkg/sqlitedb"
)

func testVaultConfig(t *testing.T) VaultConfig {
	t.Helper()
	return VaultConfig{
		Name:      "test",
		Path:      filepath.Join(t.TempDir(), "vaults", "test.db"),
		Residency: ResidencyLocalOnly,
	}
}

func userVersion(t *testing.T, v *Vault) int {
	t.Helper()
	var ver int
	if err := v.db.QueryRow(`PRAGMA user_version`).Scan(&ver); err != nil {
		t.Fatalf("PRAGMA user_version: %v", err)
	}
	return ver
}

func TestInitVaultAndIdempotency(t *testing.T) {
	cfg := testVaultConfig(t)
	v, err := InitVault(cfg)
	if err != nil {
		t.Fatalf("InitVault: %v", err)
	}
	if got := userVersion(t, v); got != len(schemaMigrations()) {
		t.Errorf("user_version = %d, want %d", got, len(schemaMigrations()))
	}
	if err := v.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Second InitVault must be idempotent.
	v2, err := InitVault(cfg)
	if err != nil {
		t.Fatalf("second InitVault: %v", err)
	}
	defer v2.Close()

	if got := userVersion(t, v2); got != len(schemaMigrations()) {
		t.Errorf("user_version after second init = %d, want %d", got, len(schemaMigrations()))
	}

	dirInfo, err := os.Stat(filepath.Dir(cfg.Path))
	if err != nil {
		t.Fatal(err)
	}
	if got := dirInfo.Mode().Perm(); got != 0o700 {
		t.Errorf("dir mode = %04o, want 0700", got)
	}

	fileInfo, err := os.Stat(cfg.Path)
	if err != nil {
		t.Fatal(err)
	}
	if got := fileInfo.Mode().Perm(); got != 0o600 {
		t.Errorf("file mode = %04o, want 0600", got)
	}

	tables := []string{
		"entities",
		"entity_aliases",
		"facts",
		"note_blocks",
		"entity_links",
		"episodic_events",
		"revisions",
		"triage_inbox",
		"access_log",
		"fts_docs",
		"routine_state",
		"routine_runs",
		"routine_outbox",
	}
	for _, table := range tables {
		var name string
		err := v2.db.QueryRow(`SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?`, table).Scan(&name)
		if err != nil {
			t.Errorf("table %q missing: %v", table, err)
		}
	}
}

func TestOpenVaultMissingReturnsErrVaultNotInitialized(t *testing.T) {
	cfg := testVaultConfig(t)
	_, err := OpenVault(cfg)
	if !errors.Is(err, ErrVaultNotInitialized) {
		t.Fatalf("OpenVault = %v, want ErrVaultNotInitialized", err)
	}
}

func TestVaultInsideGitWorkTreeRefused(t *testing.T) {
	repo := t.TempDir()
	if err := os.Mkdir(filepath.Join(repo, ".git"), 0o700); err != nil {
		t.Fatal(err)
	}
	cfg := VaultConfig{
		Name: "bad",
		Path: filepath.Join(repo, "sub", "vault.db"),
	}
	if _, err := InitVault(cfg); err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("InitVault inside git = %v, want git refusal", err)
	}
	if _, err := OpenVault(cfg); err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("OpenVault inside git = %v, want git refusal", err)
	}
}

func TestOpenVaultEnforcesPermissions(t *testing.T) {
	cfg := testVaultConfig(t)
	v, err := InitVault(cfg)
	if err != nil {
		t.Fatal(err)
	}
	if err := v.Close(); err != nil {
		t.Fatal(err)
	}

	if err := os.Chmod(filepath.Dir(cfg.Path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(cfg.Path, 0o644); err != nil {
		t.Fatal(err)
	}

	v2, err := OpenVault(cfg)
	if err != nil {
		t.Fatalf("OpenVault: %v", err)
	}
	defer v2.Close()

	dirInfo, err := os.Stat(filepath.Dir(cfg.Path))
	if err != nil {
		t.Fatal(err)
	}
	if got := dirInfo.Mode().Perm(); got != 0o700 {
		t.Errorf("dir mode = %04o, want 0700", got)
	}
	fileInfo, err := os.Stat(cfg.Path)
	if err != nil {
		t.Fatal(err)
	}
	if got := fileInfo.Mode().Perm(); got != 0o600 {
		t.Errorf("file mode = %04o, want 0600", got)
	}
}

func TestMigrateForwardAndNewerVersionError(t *testing.T) {
	db, err := sqlitedb.Open(filepath.Join(t.TempDir(), "m.db"), "_pragma=foreign_keys(1)", "_txlock=immediate")
	if err != nil {
		t.Fatal(err)
	}
	defer db.Close()

	steps := []string{
		`CREATE TABLE t (a TEXT)`,
		`ALTER TABLE t ADD COLUMN b TEXT NOT NULL DEFAULT 'x'`,
	}
	if err := migrate(db, steps[:1]); err != nil {
		t.Fatal(err)
	}
	if _, err := db.Exec(`INSERT INTO t (a) VALUES ('kept')`); err != nil {
		t.Fatal(err)
	}
	if err := migrate(db, steps); err != nil {
		t.Fatal(err)
	}
	if err := migrate(db, steps); err != nil {
		t.Fatalf("re-run migrate: %v", err)
	}
	var a, b string
	if err := db.QueryRow(`SELECT a, b FROM t`).Scan(&a, &b); err != nil {
		t.Fatal(err)
	}
	if a != "kept" || b != "x" {
		t.Errorf("row = (%q, %q), want (kept, x)", a, b)
	}
	if err := migrate(db, steps[:1]); err == nil || !strings.Contains(err.Error(), "newer") {
		t.Errorf("downgrade = %v, want newer-version error", err)
	}
}

func TestForeignKeysEnforcedAndCascade(t *testing.T) {
	v, err := InitVault(testVaultConfig(t))
	if err != nil {
		t.Fatal(err)
	}
	defer v.Close()

	// Invalid entity_id must fail foreign key check.
	if _, err := v.db.Exec(
		`INSERT INTO facts (id, entity_id, key, value_json, source_type, created_at, updated_at)
		VALUES ('f-bad', 'missing-entity', 'url', '"https://example.com"', 'manual', 1, 1)`,
	); err == nil {
		t.Fatal("insert fact with invalid entity_id succeeded, want FK error")
	}

	for _, id := range []string{"project:apollo", "person:ada"} {
		if _, err := v.db.Exec(
			`INSERT INTO entities (id, kind, slug, title, created_at, updated_at)
			VALUES (?, 'project', ?, ?, 1, 1)`,
			id, id, id,
		); err != nil {
			t.Fatalf("insert entity %s: %v", id, err)
		}
	}

	stmts := []string{
		`INSERT INTO entity_aliases (entity_id, alias, normalized_alias) VALUES ('project:apollo', 'Apollo', 'apollo')`,
		`INSERT INTO facts (id, entity_id, key, value_json, source_type, created_at, updated_at) VALUES ('f1', 'project:apollo', 'status', '"active"', 'manual', 1, 1)`,
		`INSERT INTO note_blocks (id, entity_id, section_key, heading, body_md, updated_at) VALUES ('n1', 'project:apollo', 'overview', 'Overview', 'hello', 1)`,
		`INSERT INTO entity_links (from_entity_id, relation, to_entity_id, created_at) VALUES ('project:apollo', 'owned_by', 'person:ada', 1)`,
		`INSERT INTO episodic_events (id, event_date, occurred_at, entity_id, summary, reporter, created_at) VALUES ('e1', '2026-11-22', 1, 'project:apollo', 'kicked off', 'cli', 1)`,
	}
	for _, q := range stmts {
		if _, err := v.db.Exec(q); err != nil {
			t.Fatalf("exec %q: %v", q, err)
		}
	}

	if _, err := v.db.Exec(`DELETE FROM entities WHERE id = 'project:apollo'`); err != nil {
		t.Fatalf("delete entity: %v", err)
	}

	for _, tc := range []struct {
		table string
		col   string
	}{
		{"entity_aliases", "entity_id"},
		{"facts", "entity_id"},
		{"note_blocks", "entity_id"},
		{"entity_links", "from_entity_id"},
	} {
		var count int
		if err := v.db.QueryRow(`SELECT count(*) FROM ` + tc.table + ` WHERE ` + tc.col + ` = 'project:apollo'`).Scan(&count); err != nil {
			t.Fatal(err)
		}
		if count != 0 {
			t.Errorf("%s remaining rows = %d, want 0", tc.table, count)
		}
	}

	// episodic_events.entity_id has ON DELETE SET NULL.
	var epEntity *string
	if err := v.db.QueryRow(`SELECT entity_id FROM episodic_events WHERE id = 'e1'`).Scan(&epEntity); err != nil {
		t.Fatal(err)
	}
	if epEntity != nil {
		t.Errorf("episodic_events.entity_id = %q, want NULL", *epEntity)
	}
}

func TestFTS5DocsInsertAndMatch(t *testing.T) {
	v, err := InitVault(testVaultConfig(t))
	if err != nil {
		t.Fatal(err)
	}
	defer v.Close()

	docs := []struct {
		docID, kind, entityID, comp, sensRank, title, body string
	}{
		{"d1", "entity", "project:apollo", "engineering", "1", "Apollo Launch", "Kubernetes deployment playbook and rollout checklist"},
		{"d2", "note", "person:ada", "general", "1", "Ada Lovelace", "Notes on analytical engine algorithms"},
	}
	for _, d := range docs {
		if _, err := v.db.Exec(
			`INSERT INTO fts_docs (doc_id, doc_kind, entity_id, compartment, sensitivity_rank, title, body)
			VALUES (?, ?, ?, ?, ?, ?, ?)`,
			d.docID, d.kind, d.entityID, d.comp, d.sensRank, d.title, d.body,
		); err != nil {
			t.Fatalf("insert fts_docs %s: %v", d.docID, err)
		}
	}

	rows, err := v.db.Query(
		`SELECT doc_id, entity_id, bm25(fts_docs) FROM fts_docs WHERE fts_docs MATCH ? ORDER BY bm25(fts_docs)`,
		"kubernetes",
	)
	if err != nil {
		t.Fatalf("MATCH query: %v", err)
	}
	defer rows.Close()

	var matched []string
	for rows.Next() {
		var docID, entityID string
		var score float64
		if err := rows.Scan(&docID, &entityID, &score); err != nil {
			t.Fatal(err)
		}
		matched = append(matched, docID+":"+entityID)
	}
	if err := rows.Err(); err != nil {
		t.Fatal(err)
	}
	if len(matched) != 1 || matched[0] != "d1:project:apollo" {
		t.Errorf("matched = %v, want [d1:project:apollo]", matched)
	}
}

func TestUpgradeMigration1To2PreservesData(t *testing.T) {
	cfg := testVaultConfig(t)
	if err := EnsureSecureDir(filepath.Dir(cfg.Path)); err != nil {
		t.Fatal(err)
	}
	db, err := sqlitedb.Open(cfg.Path, "_pragma=foreign_keys(1)", "_txlock=immediate")
	if err != nil {
		t.Fatal(err)
	}
	if err := migrate(db, schemaMigrations()[:1]); err != nil {
		db.Close()
		t.Fatal(err)
	}
	if _, err := db.Exec(
		`INSERT INTO entities (id, kind, slug, title, created_at, updated_at)
		VALUES ('person:ada', 'person', 'ada', 'Ada Lovelace', 100, 100)`,
	); err != nil {
		db.Close()
		t.Fatal(err)
	}
	if err := db.Close(); err != nil {
		t.Fatal(err)
	}
	if err := EnsureSecureFile(cfg.Path); err != nil {
		t.Fatal(err)
	}

	v, err := OpenVault(cfg)
	if err != nil {
		t.Fatalf("OpenVault upgrade: %v", err)
	}
	defer v.Close()

	if got := userVersion(t, v); got != len(schemaMigrations()) {
		t.Errorf("user_version after upgrade = %d, want %d", got, len(schemaMigrations()))
	}

	var title string
	if err := v.db.QueryRow(`SELECT title FROM entities WHERE id = 'person:ada'`).Scan(&title); err != nil {
		t.Fatalf("existing entity lost after migration: %v", err)
	}
	if title != "Ada Lovelace" {
		t.Errorf("title = %q, want Ada Lovelace", title)
	}

	for _, table := range []string{"routine_state", "routine_runs", "routine_outbox"} {
		var name string
		if err := v.db.QueryRow(`SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?`, table).Scan(&name); err != nil {
			t.Errorf("table %q missing after upgrade: %v", table, err)
		}
	}
}
