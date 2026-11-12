package slackwatch

import (
	"path/filepath"
	"testing"
)

func TestOpenStoreIdempotentAndWAL(t *testing.T) {
	path := filepath.Join(t.TempDir(), "sub", "slackwatch.db")
	s1, err := OpenStore(path)
	if err != nil {
		t.Fatal(err)
	}
	defer s1.Close()

	var mode string
	if err := s1.DB().QueryRow(`PRAGMA journal_mode`).Scan(&mode); err != nil {
		t.Fatal(err)
	}
	if mode != "wal" {
		t.Errorf("journal_mode = %q, want wal", mode)
	}

	// A second handle on the same file reruns migrations as a no-op and
	// reads what the first handle wrote.
	if _, err := s1.AddReply(Reply{Channel: "C1", TS: "1.1", Author: "U1", Text: "hi"}); err != nil {
		t.Fatal(err)
	}
	s2, err := OpenStore(path)
	if err != nil {
		t.Fatalf("second open: %v", err)
	}
	defer s2.Close()
	var n int
	if err := s2.DB().QueryRow(`SELECT COUNT(*) FROM replies`).Scan(&n); err != nil {
		t.Fatal(err)
	}
	if n != 1 {
		t.Errorf("replies = %d, want 1", n)
	}
	var v1, v2 int
	if err := s1.DB().QueryRow(`PRAGMA user_version`).Scan(&v1); err != nil {
		t.Fatal(err)
	}
	if err := s2.DB().QueryRow(`PRAGMA user_version`).Scan(&v2); err != nil {
		t.Fatal(err)
	}
	if v1 != len(migrations) || v2 != v1 {
		t.Errorf("user_version = %d, %d, want %d", v1, v2, len(migrations))
	}
}

func TestStoreTables(t *testing.T) {
	s, err := OpenStore(filepath.Join(t.TempDir(), "slackwatch.db"))
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	for _, table := range []string{
		"watches", "watch_posts", "replies", "reactions", "inbound",
		"subs", "cursors", "deliveries", "status", "workspace", "outbox", "posting",
	} {
		var name string
		err := s.DB().QueryRow(`SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?`, table).Scan(&name)
		if err != nil {
			t.Errorf("table %s: %v", table, err)
		}
	}
}

func TestAddReplyDuplicateIsNoop(t *testing.T) {
	s, err := OpenStore(filepath.Join(t.TempDir(), "slackwatch.db"))
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	r := Reply{Channel: "C1", TS: "1700000000.000100", ThreadTS: "1700000000.000001", Author: "U1", Text: "first"}
	added, err := s.AddReply(r)
	if err != nil || !added {
		t.Fatalf("first AddReply = %v, %v", added, err)
	}
	r.Text = "second"
	added, err = s.AddReply(r)
	if err != nil {
		t.Fatal(err)
	}
	if added {
		t.Error("duplicate AddReply reported added")
	}
	var n int
	var text string
	if err := s.DB().QueryRow(`SELECT COUNT(*), MAX(text) FROM replies`).Scan(&n, &text); err != nil {
		t.Fatal(err)
	}
	if n != 1 || text != "first" {
		t.Errorf("replies = %d %q, want 1 %q", n, text, "first")
	}
}

func TestStoreForeignKeys(t *testing.T) {
	s, err := OpenStore(filepath.Join(t.TempDir(), "slackwatch.db"))
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	db := s.DB()
	if _, err := db.Exec(`INSERT INTO watches (id, channel, thread_ts, kind, source, created_at)
VALUES (1, 'C1', '1.0', 'thread', 'manual', 0)`); err != nil {
		t.Fatal(err)
	}
	if _, err := db.Exec(`INSERT INTO watch_posts (watch_id, ts) VALUES (1, '1.0')`); err != nil {
		t.Fatal(err)
	}
	if _, err := db.Exec(`INSERT INTO watch_posts (watch_id, ts) VALUES (2, '1.0')`); err == nil {
		t.Error("watch_posts row for a missing watch was accepted")
	}
	if _, err := db.Exec(`DELETE FROM watches WHERE id = 1`); err != nil {
		t.Fatal(err)
	}
	var n int
	if err := db.QueryRow(`SELECT COUNT(*) FROM watch_posts`).Scan(&n); err != nil {
		t.Fatal(err)
	}
	if n != 0 {
		t.Errorf("watch_posts = %d after deleting the watch, want 0", n)
	}
}
