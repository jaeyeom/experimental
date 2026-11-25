package secretwatch

import (
	"path/filepath"
	"testing"
)

func TestLocatePaths(t *testing.T) {
	linux, err := locatePaths("linux", map[string]string{"HOME": "/home/me"})
	if err != nil {
		t.Fatal(err)
	}
	if linux.ConfigFile != filepath.Join("/home/me", ".config", "secretwatch", "config.pkl") {
		t.Errorf("linux ConfigFile = %q", linux.ConfigFile)
	}
	if linux.StateFile != filepath.Join("/home/me", ".local", "state", "secretwatch", "state.json") {
		t.Errorf("linux StateFile = %q", linux.StateFile)
	}

	mac, err := locatePaths("darwin", map[string]string{"HOME": "/Users/me"})
	if err != nil {
		t.Fatal(err)
	}
	wantMac := filepath.Join("/Users/me", "Library", "Application Support", "secretwatch")
	if mac.ConfigFile != filepath.Join(wantMac, "config.pkl") {
		t.Errorf("mac ConfigFile = %q", mac.ConfigFile)
	}
	if mac.StateFile != filepath.Join(wantMac, "state.json") {
		t.Errorf("mac StateFile = %q", mac.StateFile)
	}

	xdg, err := locatePaths("darwin", map[string]string{
		"HOME":            "/Users/me",
		"XDG_CONFIG_HOME": "/cfg",
		"XDG_STATE_HOME":  "/state",
	})
	if err != nil {
		t.Fatal(err)
	}
	if xdg.ConfigFile != filepath.Join("/cfg", "secretwatch", "config.pkl") {
		t.Errorf("xdg ConfigFile = %q", xdg.ConfigFile)
	}
	if xdg.StateFile != filepath.Join("/state", "secretwatch", "state.json") {
		t.Errorf("xdg StateFile = %q", xdg.StateFile)
	}

	if _, err := locatePaths("linux", map[string]string{}); err == nil {
		t.Fatal("expected error when HOME is unset")
	}
}
