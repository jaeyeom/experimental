package config

import (
	"errors"
	"path/filepath"
	"slices"
	"testing"
	"time"
)

func TestOpenTabsDefaults(t *testing.T) {
	t.Parallel()

	cfg := Defaults()
	if cfg.OpenMissingTabs {
		t.Fatal("open_missing_tabs defaults to true, want false")
	}
	if cfg.OpenTabsMaxPerRound != 2 || cfg.OpenTabsReadyTimeout != 90*time.Second || !slices.Equal(cfg.OpenTabsAgent, []string{"claude"}) {
		t.Fatalf("defaults = max %d, ready %s, agent %v; want 2, 90s, [claude]", cfg.OpenTabsMaxPerRound, cfg.OpenTabsReadyTimeout, cfg.OpenTabsAgent)
	}
}

func TestLoadOpenTabsKeys(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "prsync.config")
	mustWrite(t, path, "machines=mac linux2\n"+
		"machine_checkouts=linux2=/home/me/repo\n"+
		"open_missing_tabs=true\n"+
		"open_tabs_workspace=prs, mac=w7\n"+
		"open_tabs_cwd=/src/local, mac=/Users/me/src/repo\n"+
		"open_tabs_agent=claude --model opus\n"+
		"open_tabs_max_per_round=3\n"+
		"open_tabs_ready_timeout=2m\n")
	got, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if !got.OpenMissingTabs || got.OpenTabsMaxPerRound != 3 || got.OpenTabsReadyTimeout != 2*time.Minute {
		t.Fatalf("open tabs = %v, max %d, ready %s", got.OpenMissingTabs, got.OpenTabsMaxPerRound, got.OpenTabsReadyTimeout)
	}
	if !slices.Equal(got.OpenTabsAgent, []string{"claude", "--model", "opus"}) {
		t.Fatalf("open_tabs_agent = %q", got.OpenTabsAgent)
	}
	for _, tc := range []struct{ machine, workspace, cwd string }{
		{"", "prs", "/src/local"},
		{LocalMachine, "prs", "/src/local"},
		{"mac", "w7", "/Users/me/src/repo"},
		// linux2 has no entries: no workspace, and its machine_checkouts path.
		{"linux2", "", "/home/me/repo"},
	} {
		if w := got.OpenTabWorkspace(tc.machine); w != tc.workspace {
			t.Errorf("OpenTabWorkspace(%q) = %q, want %q", tc.machine, w, tc.workspace)
		}
		if c := got.OpenTabCwd(tc.machine); c != tc.cwd {
			t.Errorf("OpenTabCwd(%q) = %q, want %q", tc.machine, c, tc.cwd)
		}
	}
}

func TestLoadOpenTabsRejectsBadValues(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct{ body, key string }{
		{"open_missing_tabs=maybe\n", "open_missing_tabs"},
		{"open_tabs_workspace=m3=w1\n", "open_tabs_workspace"},
		{"open_tabs_workspace=a, b\n", "open_tabs_workspace"},
		{"machines=mac\nopen_tabs_cwd=mac=src/repo\n", "open_tabs_cwd"},
		{"open_tabs_cwd=relative\n", "open_tabs_cwd"},
		{"open_tabs_max_per_round=-1\n", "open_tabs_max_per_round"},
		{"open_tabs_ready_timeout=soon\n", "open_tabs_ready_timeout"},
		{"open_tabs_ready_timeout=0s\n", "open_tabs_ready_timeout"},
		{"open_missing_tabs=true\nopen_tabs_agent=\n", "open_tabs_agent"},
	} {
		path := filepath.Join(t.TempDir(), "prsync.config")
		mustWrite(t, path, tc.body)
		var keyErr *KeyError
		if _, err := Load(path); !errors.As(err, &keyErr) || keyErr.Key != tc.key {
			t.Errorf("Load(%q) error = %v, want %s key error", tc.body, err, tc.key)
		}
	}
}
