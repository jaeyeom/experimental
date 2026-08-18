package dispatch

import (
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

func TestDedupeEmptyState(t *testing.T) {
	t.Parallel()

	st := State{}
	if st.Deduped("acme/widgets#123", []string{"PRRC_a"}) {
		t.Fatal("empty state must not be deduped")
	}
}

func TestDedupeSubset(t *testing.T) {
	t.Parallel()

	st := State{"acme/widgets#123": {DispatchedCommentIDs: []string{"PRRC_a", "PRRC_b"}}}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_a"}) {
		t.Fatal("subset of stored ids must be deduped")
	}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_a", "PRRC_b"}) {
		t.Fatal("equal set must be deduped")
	}
}

func TestDedupeNewID(t *testing.T) {
	t.Parallel()

	st := State{"acme/widgets#123": {DispatchedCommentIDs: []string{"PRRC_a"}}}
	if st.Deduped("acme/widgets#123", []string{"PRRC_a", "PRRC_b"}) {
		t.Fatal("new comment id must not be deduped")
	}
}

func TestDedupeReplaceNotUnion(t *testing.T) {
	t.Parallel()

	st := State{}
	at := time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC)
	st.Record("acme/widgets#123", []string{"PRRC_a", "PRRC_b"}, at)
	st.Record("acme/widgets#123", []string{"PRRC_c"}, at)
	got := st["acme/widgets#123"].DispatchedCommentIDs
	want := []string{"PRRC_c"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("ids = %v, want %v (replace, not union)", got, want)
	}
	if st["acme/widgets#123"].DispatchedAt != "2026-01-01T09:00:00Z" {
		t.Fatalf("dispatched_at = %q", st["acme/widgets#123"].DispatchedAt)
	}
}

func TestLoadFileMissingIsEmpty(t *testing.T) {
	t.Parallel()

	st, err := LoadFile(filepath.Join(t.TempDir(), "missing.json"))
	if err != nil {
		t.Fatalf("LoadFile(missing) error = %v", err)
	}
	if len(st) != 0 {
		t.Fatalf("state = %#v, want empty", st)
	}
}

func TestLoadFileCorrupt(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "state.json")
	if err := os.WriteFile(path, []byte("{"), 0o600); err != nil {
		t.Fatal(err)
	}
	_, err := LoadFile(path)
	if err == nil {
		t.Fatal("LoadFile(corrupt) error = nil, want error")
	}
	if !errors.Is(err, ErrCorruptState) {
		t.Fatalf("error = %v, want ErrCorruptState", err)
	}
}

func TestSaveFileRoundTrip(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "nested", "state.json")
	st := State{}
	st.Record("acme/widgets#123", []string{"PRRC_a"}, time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC))
	if err := SaveFile(path, st); err != nil {
		t.Fatalf("SaveFile() error = %v", err)
	}
	got, err := LoadFile(path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !reflect.DeepEqual(got["acme/widgets#123"].DispatchedCommentIDs, []string{"PRRC_a"}) {
		t.Fatalf("round-trip = %#v", got)
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm() != 0o600 {
		t.Fatalf("perm = %o, want 0600", info.Mode().Perm())
	}
}

func TestWithLockExclusive(t *testing.T) {
	t.Parallel()

	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	unlock, err := store.AcquireLock()
	if err != nil {
		t.Fatalf("AcquireLock() error = %v", err)
	}
	defer unlock()

	if _, err := os.Stat(store.Path + ".lock"); err != nil {
		t.Fatalf("lock file missing: %v", err)
	}
	data, err := os.ReadFile(store.Path + ".lock")
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), "pid:") || !strings.Contains(string(data), "locked_at:") {
		t.Fatalf("lock contents = %q", data)
	}

	_, err = store.AcquireLock()
	if !errors.Is(err, ErrLock) {
		t.Fatalf("second AcquireLock() error = %v, want ErrLock", err)
	}
}

func TestWithLockReapsStale(t *testing.T) {
	t.Parallel()

	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	if err := os.WriteFile(store.Path+".lock", []byte("locked_at: 2020-01-01T00:00:00Z\npid: 999999\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	unlock, err := store.AcquireLock()
	if err != nil {
		t.Fatalf("AcquireLock() stale error = %v", err)
	}
	unlock()
}

func TestWithLockReleases(t *testing.T) {
	t.Parallel()

	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	unlock, err := store.AcquireLock()
	if err != nil {
		t.Fatal(err)
	}
	unlock()
	if _, err := os.Stat(store.Path + ".lock"); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("lock file still present after release")
	}
	unlock2, err := store.AcquireLock()
	if err != nil {
		t.Fatalf("re-acquire after release: %v", err)
	}
	unlock2()
}

func TestFileStoreLoad(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "state.json")
	st := State{"acme/widgets#1": {DispatchedCommentIDs: []string{"x"}}}
	if err := SaveFile(path, st); err != nil {
		t.Fatal(err)
	}
	got, err := FileStore{Path: path}.Load()
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if !got.Deduped("acme/widgets#1", []string{"x"}) {
		t.Fatalf("loaded state = %#v", got)
	}
}
