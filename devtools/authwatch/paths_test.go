package authwatch

import (
	"path/filepath"
	"testing"
)

func TestLocatePaths(t *testing.T) {
	t.Run("linux defaults", func(t *testing.T) {
		p, err := locatePaths("linux", map[string]string{"HOME": "/home/alice"})
		if err != nil {
			t.Fatal(err)
		}
		if p.ConfigFile != "/home/alice/.config/authwatch/config.toml" {
			t.Errorf("ConfigFile = %q", p.ConfigFile)
		}
		if p.StateFile != "/home/alice/.local/state/authwatch/state.json" {
			t.Errorf("StateFile = %q", p.StateFile)
		}
	})

	t.Run("darwin defaults", func(t *testing.T) {
		p, err := locatePaths("darwin", map[string]string{"HOME": "/Users/alice"})
		if err != nil {
			t.Fatal(err)
		}
		wantCfg := "/Users/alice/Library/Application Support/authwatch/config.toml"
		wantState := "/Users/alice/Library/Application Support/authwatch/state.json"
		if p.ConfigFile != wantCfg {
			t.Errorf("ConfigFile = %q, want %q", p.ConfigFile, wantCfg)
		}
		if p.StateFile != wantState {
			t.Errorf("StateFile = %q, want %q", p.StateFile, wantState)
		}
	})

	t.Run("xdg overrides", func(t *testing.T) {
		p, err := locatePaths("darwin", map[string]string{
			"HOME":            "/Users/alice",
			"XDG_CONFIG_HOME": "/tmp/cfg",
			"XDG_STATE_HOME":  "/tmp/state",
		})
		if err != nil {
			t.Fatal(err)
		}
		if p.ConfigFile != "/tmp/cfg/authwatch/config.toml" {
			t.Errorf("ConfigFile = %q", p.ConfigFile)
		}
		if p.StateFile != "/tmp/state/authwatch/state.json" {
			t.Errorf("StateFile = %q", p.StateFile)
		}
	})
}

func TestResolveConfigDirAndKeychainService(t *testing.T) {
	home := "/Users/alice"
	env := []string{"HOME=" + home}

	dir, err := ResolveConfigDir("", env)
	if err != nil {
		t.Fatal(err)
	}
	if dir != filepath.Join(home, ".claude") {
		t.Errorf("default dir = %q", dir)
	}
	if svc := KeychainServiceName(dir, home); svc != DefaultKeychainService {
		t.Errorf("default service = %q", svc)
	}

	envCustom := []string{"HOME=" + home, "CLAUDE_CONFIG_DIR=~/.claude-work"}
	dirCustom, err := ResolveConfigDir("", envCustom)
	if err != nil {
		t.Fatal(err)
	}
	if dirCustom != filepath.Join(home, ".claude-work") {
		t.Errorf("env custom dir = %q", dirCustom)
	}
	svcCustom := KeychainServiceName(dirCustom, home)
	if svcCustom == DefaultKeychainService || len(svcCustom) <= len(DefaultKeychainService) {
		t.Errorf("expected hashed service name, got %q", svcCustom)
	}

	dirProfile, err := ResolveConfigDir("~/custom-prof", envCustom)
	if err != nil {
		t.Fatal(err)
	}
	if dirProfile != filepath.Join(home, "custom-prof") {
		t.Errorf("profile dir = %q", dirProfile)
	}
}
