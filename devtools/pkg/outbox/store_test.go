package outbox

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
)

func TestFileStoreCreateSaveGetList(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "state")
	s := NewFileStore(dir)

	if recs, err := s.List(); err != nil || len(recs) != 0 {
		t.Fatalf("empty List = %v, %v", recs, err)
	}

	// Simulate an empty placeholder left by an interrupted Create.
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "1.json"), nil, 0o600); err != nil {
		t.Fatal(err)
	}

	r, err := s.Create(Record{
		Draft: Draft{
			Kind:     "webhook",
			Target:   "prod/hooks",
			Owner:    "cli-job",
			Question: "Send webhook?",
		},
		State: StatePending,
	})
	if err != nil {
		t.Fatal(err)
	}
	if r.ID != 2 {
		t.Fatalf("ID = %d, want 2 (skipping existing 1.json)", r.ID)
	}

	r.State = StateExecuted
	if err := s.Save(r); err != nil {
		t.Fatal(err)
	}

	got, err := s.Get(2)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateExecuted || got.Target != "prod/hooks" {
		t.Fatalf("got = %+v", got)
	}

	if _, err := s.Get(999); !errors.Is(err, ErrNotFound) {
		t.Fatalf("Get(999) err = %v, want ErrNotFound", err)
	}

	recs, err := s.List()
	if err != nil || len(recs) != 1 || recs[0].ID != 2 {
		t.Fatalf("List = %+v, %v", recs, err)
	}
}

func TestLockDirPreventsConcurrentPoll(t *testing.T) {
	dir := t.TempDir()
	unlock, err := LockDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	defer unlock()

	if _, err := LockDir(dir); !errors.Is(err, ErrLockHeld) {
		t.Fatalf("second LockDir err = %v, want ErrLockHeld", err)
	}

	unlock()
	unlock2, err := LockDir(dir)
	if err != nil {
		t.Fatalf("re-lock err = %v", err)
	}
	unlock2()
}
