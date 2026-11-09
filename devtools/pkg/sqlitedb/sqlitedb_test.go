package sqlitedb

import (
	"context"
	"path/filepath"
	"testing"
)

func TestOpenExtraParams(t *testing.T) {
	db, err := Open(filepath.Join(t.TempDir(), "test.db"), "_pragma=foreign_keys(1)")
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	defer db.Close()
	// Hold one connection so the query below runs on a second one.
	conn, err := db.Conn(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()

	var fk int
	if err := db.QueryRow("PRAGMA foreign_keys").Scan(&fk); err != nil {
		t.Fatalf("PRAGMA foreign_keys: %v", err)
	}
	if fk != 1 {
		t.Errorf("foreign_keys = %d, want 1", fk)
	}
}

func TestOpen(t *testing.T) {
	db, err := Open(filepath.Join(t.TempDir(), "test.db"))
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	defer db.Close()

	var mode string
	if err := db.QueryRow("PRAGMA journal_mode").Scan(&mode); err != nil {
		t.Fatalf("PRAGMA journal_mode: %v", err)
	}
	if mode != "wal" {
		t.Errorf("journal_mode = %q, want %q", mode, "wal")
	}

	var timeout int
	if err := db.QueryRow("PRAGMA busy_timeout").Scan(&timeout); err != nil {
		t.Fatalf("PRAGMA busy_timeout: %v", err)
	}
	if timeout != 5000 {
		t.Errorf("busy_timeout = %d, want 5000", timeout)
	}

	if _, err := db.Exec("CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)"); err != nil {
		t.Fatalf("CREATE TABLE: %v", err)
	}
	if _, err := db.Exec("INSERT INTO t (name) VALUES (?)", "hello"); err != nil {
		t.Fatalf("INSERT: %v", err)
	}
	var name string
	if err := db.QueryRow("SELECT name FROM t WHERE id = 1").Scan(&name); err != nil {
		t.Fatalf("SELECT: %v", err)
	}
	if name != "hello" {
		t.Errorf("name = %q, want %q", name, "hello")
	}
}
