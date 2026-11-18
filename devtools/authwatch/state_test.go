package authwatch

import (
	"path/filepath"
	"testing"
	"time"
)

func TestLoadAndSaveState(t *testing.T) {
	path := filepath.Join(t.TempDir(), "authwatch", "state.json")
	empty, err := LoadState(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(empty.Profiles) != 0 {
		t.Fatalf("expected empty profiles, got %v", empty.Profiles)
	}

	now := time.Date(2026, 11, 18, 12, 0, 0, 0, time.UTC)
	st := State{
		Profiles: map[string]ProfileState{
			"default": {
				Level:             LevelWarn,
				Since:             now,
				AccessExpiresAt:   now.Add(2 * time.Hour),
				RefreshExpiresAt:  now.Add(48 * time.Hour),
				LastCheck:         now,
				LastNotifiedLevel: LevelWarn,
				LastNotifiedAt:    now,
			},
		},
	}
	if err := SaveState(path, st); err != nil {
		t.Fatal(err)
	}
	loaded, err := LoadState(path)
	if err != nil {
		t.Fatal(err)
	}
	got := loaded.profile("default")
	if got.Level != LevelWarn || !got.Since.Equal(now) || got.LastNotifiedLevel != LevelWarn {
		t.Errorf("loaded mismatch: %+v", got)
	}
}
