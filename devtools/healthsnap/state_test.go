package healthsnap

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestStateAndSnapshotPersistenceAndRetention(t *testing.T) {
	dir := t.TempDir()
	statePath := filepath.Join(dir, "state.json")

	// Missing state returns empty initialized maps.
	st, err := LoadState(statePath)
	if err != nil {
		t.Fatalf("LoadState(missing) = %v", err)
	}
	if st.Rules == nil || st.Checks == nil {
		t.Fatalf("expected non-nil maps in empty state: %+v", st)
	}

	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)
	st.Rules["r1"] = RuleState{State: SeverityWarn, LastChanged: now}
	if err := SaveState(statePath, st); err != nil {
		t.Fatalf("SaveState = %v", err)
	}
	reloaded, err := LoadState(statePath)
	if err != nil || reloaded.Rules["r1"].State != SeverityWarn {
		t.Fatalf("reloaded = %+v, err=%v", reloaded, err)
	}

	// Missing latest snapshot returns fs.ErrNotExist.
	if _, err := LoadLatestSnapshot(dir); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("expected ErrNotExist, got %v", err)
	}

	// Write 4 snapshots with keep=2 and markdown=true; oldest 2 should be pruned.
	for i := 0; i < 4; i++ {
		snap := Snapshot{
			Timestamp: now.Add(time.Duration(i) * time.Minute),
			State:     SeverityOK,
			Checks: []CheckSnapshot{
				{ID: "c1", Kind: KindCommand, State: SeverityOK, Value: i, Display: "ok", DurationMs: 5},
			},
		}
		if _, err := WriteSnapshot(dir, snap, true, 2); err != nil {
			t.Fatalf("WriteSnapshot(%d) = %v", i, err)
		}
	}

	jsonMatches, err := filepath.Glob(filepath.Join(dir, "snapshots", "*.json"))
	if err != nil {
		t.Fatal(err)
	}
	mdMatches, err := filepath.Glob(filepath.Join(dir, "snapshots", "*.md"))
	if err != nil {
		t.Fatal(err)
	}
	if len(jsonMatches) != 2 || len(mdMatches) != 2 {
		t.Fatalf("retained json=%d md=%d, want 2 each", len(jsonMatches), len(mdMatches))
	}

	latest, err := LoadLatestSnapshot(dir)
	if err != nil {
		t.Fatalf("LoadLatestSnapshot = %v", err)
	}
	if !latest.Timestamp.Equal(now.Add(3 * time.Minute)) {
		t.Errorf("latest timestamp = %v, want +3m", latest.Timestamp)
	}

	mdBytes, err := os.ReadFile(filepath.Join(dir, "latest.md"))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(mdBytes), "# healthsnap snapshot") || !strings.Contains(string(mdBytes), "| c1 |") {
		t.Errorf("unexpected latest.md:\n%s", string(mdBytes))
	}
}
