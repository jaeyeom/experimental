package contacts

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/sqlitedb"
)

// fixedNow is the tests' clock: 2026-03-10 09:30 UTC.
func fixedNow() time.Time { return time.Date(2026, 3, 10, 9, 30, 0, 0, time.UTC) }

func newStore(t *testing.T) *Store {
	t.Helper()
	dir := filepath.Join(t.TempDir(), "data")
	if err := Init(dir); err != nil {
		t.Fatalf("Init: %v", err)
	}
	s, err := Open(dir, fixedNow)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { s.Close() })
	return s
}

func userVersion(t *testing.T, s *Store) int {
	t.Helper()
	var v int
	if err := s.db.QueryRow(`PRAGMA user_version`).Scan(&v); err != nil {
		t.Fatal(err)
	}
	return v
}

func TestInitMigratesToLatestAndIsIdempotent(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "data")
	if err := Init(dir); err != nil {
		t.Fatalf("Init: %v", err)
	}
	if err := Init(dir); err != nil {
		t.Fatalf("second Init: %v", err)
	}
	s, err := Open(dir, time.Now)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	if got := userVersion(t, s); got != len(migrations) {
		t.Errorf("user_version = %d, want %d", got, len(migrations))
	}
	for path, want := range map[string]os.FileMode{
		dir:                          0o700,
		filepath.Join(dir, "people"): 0o700,
		filepath.Join(dir, DBName):   0o600,
	} {
		fi, err := os.Stat(path)
		if err != nil {
			t.Fatal(err)
		}
		if got := fi.Mode().Perm(); got != want {
			t.Errorf("%s mode = %o, want %o", path, got, want)
		}
	}
}

func TestOpenWithoutInit(t *testing.T) {
	if _, err := Open(t.TempDir(), time.Now); err != ErrNotInitialized {
		t.Fatalf("Open = %v, want ErrNotInitialized", err)
	}
}

func TestMigrateFromOlderVersionKeepsData(t *testing.T) {
	db, err := sqlitedb.Open(filepath.Join(t.TempDir(), "m.db"), "_txlock=immediate")
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
		t.Fatalf("re-run: %v", err)
	}
	var a, b string
	if err := db.QueryRow(`SELECT a, b FROM t`).Scan(&a, &b); err != nil {
		t.Fatal(err)
	}
	if a != "kept" || b != "x" {
		t.Errorf("row = %q, %q; want kept, x", a, b)
	}
	if err := migrate(db, steps[:1]); err == nil || !strings.Contains(err.Error(), "newer") {
		t.Errorf("downgrade = %v, want a newer-version error", err)
	}
}

func TestHandleUniqueAndCascade(t *testing.T) {
	s := newStore(t)
	exec := func(q string, args ...any) error {
		_, err := s.db.Exec(q, args...)
		return err //nolint:wrapcheck // test helper
	}
	for _, id := range []string{"a", "b"} {
		if err := exec(`INSERT INTO people (id, name, created_at) VALUES (?, ?, 'now')`, id, id); err != nil {
			t.Fatal(err)
		}
	}
	if err := exec(`INSERT INTO handles VALUES ('a', 'chat', 'Ada')`); err != nil {
		t.Fatal(err)
	}
	if err := exec(`INSERT INTO handles VALUES ('b', 'chat', 'ada')`); err == nil {
		t.Error("duplicate handle (case-insensitive) accepted")
	}
	if err := exec(`INSERT INTO handles VALUES ('b', 'code', 'ada')`); err != nil {
		t.Errorf("same value, other kind: %v", err)
	}
	if err := exec(`INSERT INTO events (person_id, date, channel, link, summary, reporter, created_at)
		VALUES ('a', '2026-01-01', 'chat', 'https://x', 's', 'r', 'now')`); err != nil {
		t.Fatal(err)
	}
	if err := exec(`DELETE FROM people WHERE id = 'a'`); err != nil {
		t.Fatal(err)
	}
	for _, table := range []string{"handles", "events"} {
		var n int
		if err := s.db.QueryRow(`SELECT count(*) FROM ` + table + ` WHERE person_id = 'a'`).Scan(&n); err != nil {
			t.Fatal(err)
		}
		if n != 0 {
			t.Errorf("%s rows for deleted person = %d, want 0", table, n)
		}
	}
}

func TestDataDirInsideGitRefused(t *testing.T) {
	repo := t.TempDir()
	if err := os.Mkdir(filepath.Join(repo, ".git"), 0o700); err != nil {
		t.Fatal(err)
	}
	dir := filepath.Join(repo, "sub", "contacts")
	if err := Init(dir); err == nil || !strings.Contains(err.Error(), "git work tree") {
		t.Fatalf("Init = %v, want git work tree refusal", err)
	}
	if _, err := os.Stat(dir); err == nil {
		t.Error("Init created the refused directory")
	}
}

func TestLoadConfig(t *testing.T) {
	env := map[string]string{"HOME": "/home/u", "XDG_DATA_HOME": ""}
	getenv := func(k string) string { return env[k] }
	c, err := LoadConfig(filepath.Join(t.TempDir(), "missing.toml"), getenv)
	if err != nil {
		t.Fatal(err)
	}
	if c.DataDir != "/home/u/.local/share/contacts" || c.RetentionDays != DefaultRetentionDays {
		t.Errorf("defaults = %+v", c)
	}
	env["XDG_DATA_HOME"] = "/xdg"
	if c, _ := LoadConfig("/nonexistent", getenv); c.DataDir != "/xdg/contacts" {
		t.Errorf("XDG data dir = %q", c.DataDir)
	}
	path := filepath.Join(t.TempDir(), "c.toml")
	if err := os.WriteFile(path, []byte("data_dir = \"~/c\"\nretention_days = 30\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	c, err = LoadConfig(path, getenv)
	if err != nil {
		t.Fatal(err)
	}
	if c.DataDir != "/home/u/c" || c.RetentionDays != 30 {
		t.Errorf("file config = %+v", c)
	}
}
