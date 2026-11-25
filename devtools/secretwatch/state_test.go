package secretwatch

import (
	"fmt"
	"path/filepath"
	"testing"
	"time"
)

func TestLoadAndSaveState(t *testing.T) {
	path := filepath.Join(t.TempDir(), "secretwatch", "state.json")
	empty, err := LoadState(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(empty.Journals) != 0 || empty.HasSeenCursor("c1") {
		t.Fatalf("unexpected non-empty state: %+v", empty)
	}

	now := time.Date(2026, 11, 25, 23, 0, 0, 0, time.UTC)
	var st State
	st.setJournal(JournalUser, JournalState{Cursor: "u-cur-1", LastScan: now})
	st.setJournal(JournalSystem, JournalState{Cursor: "s-cur-9", LastScan: now})
	st.MarkCursorSeen("u-cur-1", now)

	if err := SaveState(path, st); err != nil {
		t.Fatal(err)
	}
	loaded, err := LoadState(path)
	if err != nil {
		t.Fatal(err)
	}
	if got := loaded.journal(JournalUser); got.Cursor != "u-cur-1" || !got.LastScan.Equal(now) {
		t.Errorf("user journal state = %+v", got)
	}
	if got := loaded.journal(JournalSystem); got.Cursor != "s-cur-9" || !got.LastScan.Equal(now) {
		t.Errorf("system journal state = %+v", got)
	}
	if !loaded.HasSeenCursor("u-cur-1") {
		t.Errorf("expected seen cursor u-cur-1")
	}
}

func TestPruneSeenCursors(t *testing.T) {
	base := time.Date(2026, 11, 25, 10, 0, 0, 0, time.UTC)
	seen := make(map[string]time.Time)
	for i := 0; i < 5; i++ {
		seen[fmt.Sprintf("c%d", i)] = base.Add(time.Duration(i) * time.Minute)
	}
	pruneSeenCursors(seen, 3)
	if len(seen) != 3 {
		t.Fatalf("len = %d, want 3", len(seen))
	}
	if _, ok := seen["c0"]; ok {
		t.Errorf("expected oldest cursor c0 to be pruned")
	}
	if _, ok := seen["c1"]; ok {
		t.Errorf("expected second oldest cursor c1 to be pruned")
	}
	for _, keep := range []string{"c2", "c3", "c4"} {
		if _, ok := seen[keep]; !ok {
			t.Errorf("expected cursor %s to be kept", keep)
		}
	}
}
