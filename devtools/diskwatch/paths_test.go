package diskwatch

import (
	"path/filepath"
	"testing"
)

func TestLocatePaths(t *testing.T) {
	t.Parallel()
	linux, err := locatePaths("linux", map[string]string{"HOME": "/home/me"})
	if err != nil {
		t.Fatal(err)
	}
	if linux.ConfigFile != filepath.Join("/home/me", ".config", "diskwatch", "config.toml") {
		t.Fatalf("linux config = %s", linux.ConfigFile)
	}
	if linux.StateFile != filepath.Join("/home/me", ".local", "state", "diskwatch", "state.json") {
		t.Fatalf("linux state = %s", linux.StateFile)
	}
	mac, err := locatePaths("darwin", map[string]string{"HOME": "/Users/me"})
	if err != nil {
		t.Fatal(err)
	}
	want := filepath.Join("/Users/me", "Library", "Application Support", "diskwatch")
	if mac.ConfigFile != filepath.Join(want, "config.toml") || mac.StateFile != filepath.Join(want, "state.json") {
		t.Fatalf("mac paths = %+v", mac)
	}
	xdg, err := locatePaths("darwin", map[string]string{
		"HOME":            "/Users/me",
		"XDG_CONFIG_HOME": "/cfg",
		"XDG_STATE_HOME":  "/state",
	})
	if err != nil {
		t.Fatal(err)
	}
	if xdg.ConfigFile != filepath.Join("/cfg", "diskwatch", "config.toml") {
		t.Fatalf("xdg config = %s", xdg.ConfigFile)
	}
	if xdg.StateFile != filepath.Join("/state", "diskwatch", "state.json") {
		t.Fatalf("xdg state = %s", xdg.StateFile)
	}
	if _, err := locatePaths("linux", map[string]string{}); err == nil {
		t.Fatal("missing HOME should fail")
	}
}
