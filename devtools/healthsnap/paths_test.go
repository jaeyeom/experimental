package healthsnap

import (
	"path/filepath"
	"testing"
)

func TestLocatePaths(t *testing.T) {
	linux, err := locatePaths("linux", map[string]string{"HOME": "/home/me"})
	if err != nil {
		t.Fatalf("locatePaths(linux) = %v", err)
	}
	if want := filepath.Join("/home/me", ".config", "healthsnap", "config.pkl"); linux.ConfigFile != want {
		t.Errorf("linux ConfigFile = %q, want %q", linux.ConfigFile, want)
	}
	if want := filepath.Join("/home/me", ".local", "state", "healthsnap"); linux.StateDir != want {
		t.Errorf("linux StateDir = %q, want %q", linux.StateDir, want)
	}

	mac, err := locatePaths("darwin", map[string]string{"HOME": "/Users/me"})
	if err != nil {
		t.Fatalf("locatePaths(darwin) = %v", err)
	}
	wantMac := filepath.Join("/Users/me", "Library", "Application Support", "healthsnap")
	if mac.ConfigFile != filepath.Join(wantMac, "config.pkl") {
		t.Errorf("darwin ConfigFile = %q, want %q", mac.ConfigFile, filepath.Join(wantMac, "config.pkl"))
	}
	if mac.StateDir != wantMac {
		t.Errorf("darwin StateDir = %q, want %q", mac.StateDir, wantMac)
	}

	xdg, err := locatePaths("darwin", map[string]string{
		"HOME":            "/Users/me",
		"XDG_CONFIG_HOME": "/cfg",
		"XDG_STATE_HOME":  "/state",
	})
	if err != nil {
		t.Fatalf("locatePaths(xdg) = %v", err)
	}
	if want := filepath.Join("/cfg", "healthsnap", "config.pkl"); xdg.ConfigFile != want {
		t.Errorf("xdg ConfigFile = %q, want %q", xdg.ConfigFile, want)
	}
	if want := filepath.Join("/state", "healthsnap"); xdg.StateDir != want {
		t.Errorf("xdg StateDir = %q, want %q", xdg.StateDir, want)
	}

	if _, err := locatePaths("linux", map[string]string{}); err == nil {
		t.Fatal("expected error when HOME is unset")
	}
}

func TestExpandHome(t *testing.T) {
	env := []string{"HOME=/home/alice"}
	if got := ExpandHome("~/src/app", env); got != filepath.Join("/home/alice", "src", "app") {
		t.Errorf("ExpandHome(~/src/app) = %q", got)
	}
	if got := ExpandHome("~", env); got != "/home/alice" {
		t.Errorf("ExpandHome(~) = %q", got)
	}
	if got := ExpandHome("/etc/hosts", env); got != "/etc/hosts" {
		t.Errorf("ExpandHome(/etc/hosts) = %q", got)
	}
}
