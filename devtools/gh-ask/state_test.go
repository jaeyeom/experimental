package ghask

import (
	"maps"
	"os"
	"path/filepath"
	"testing"
)

func TestStateRoundTrip(t *testing.T) {
	path := filepath.Join(t.TempDir(), "sub", "threads.json")
	got, err := loadState(path)
	if err != nil {
		t.Fatalf("loadState() of a missing file error = %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("loadState() of a missing file = %v, want empty", got)
	}
	want := map[string]threadEntry{
		stateKey("octo/demo", 42): {ID: "abc", Closed: true},
		stateKey("octo/demo", 7):  {ID: "def"},
	}
	if err := saveState(path, want); err != nil {
		t.Fatalf("saveState() error = %v", err)
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Errorf("state mode = %o, want 600", perm)
	}
	got, err = loadState(path)
	if err != nil {
		t.Fatalf("loadState() error = %v", err)
	}
	if !maps.Equal(got, want) {
		t.Errorf("loadState() = %v, want %v", got, want)
	}
	entries, err := os.ReadDir(filepath.Dir(path))
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 {
		t.Errorf("state dir has %d entries, want only the state file", len(entries))
	}
}

func TestLoadStateCorrupt(t *testing.T) {
	path := filepath.Join(t.TempDir(), "threads.json")
	if err := os.WriteFile(path, []byte("{not json"), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := loadState(path); err == nil {
		t.Error("loadState() of a corrupt file error = nil")
	}
}

func TestDefaultStatePath(t *testing.T) {
	tests := []struct {
		name string
		env  []string
		want string
	}{
		{name: "xdg", env: []string{"XDG_STATE_HOME=/s", "HOME=/h"}, want: "/s/gh-ask/threads.json"},
		{name: "home", env: []string{"HOME=/h"}, want: "/h/.local/state/gh-ask/threads.json"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := DefaultStatePath(tc.env)
			if err != nil {
				t.Fatalf("DefaultStatePath() error = %v", err)
			}
			if got != tc.want {
				t.Errorf("DefaultStatePath() = %q, want %q", got, tc.want)
			}
		})
	}
	if _, err := DefaultStatePath(nil); err == nil {
		t.Error("DefaultStatePath(nil) error = nil, want error")
	}
}
