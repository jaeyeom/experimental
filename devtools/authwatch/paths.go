// Package authwatch monitors agent CLI OAuth token expiry metadata and
// notifies on level changes without ever storing or printing token values.
package authwatch

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

const (
	// DefaultKeychainService is the macOS keychain service name for ~/.claude.
	DefaultKeychainService = "Claude Code-credentials"
	// DefaultCredentialsFileName is the credentials filename under config_dir on Linux.
	DefaultCredentialsFileName = ".credentials.json"
)

// Paths holds the resolved config and state file locations for authwatch.
type Paths struct {
	ConfigFile string
	StateFile  string
}

// LocatePaths resolves config and state paths from the environment.
func LocatePaths(env []string) (Paths, error) {
	return locatePaths(runtime.GOOS, envValues(env))
}

func locatePaths(goos string, env map[string]string) (Paths, error) {
	home := env["HOME"]
	if home == "" {
		return Paths{}, fmt.Errorf("HOME is not set")
	}
	configRoot := env["XDG_CONFIG_HOME"]
	if configRoot == "" {
		if goos == "darwin" {
			configRoot = filepath.Join(home, "Library", "Application Support")
		} else {
			configRoot = filepath.Join(home, ".config")
		}
	}
	stateRoot := env["XDG_STATE_HOME"]
	if stateRoot == "" {
		if goos == "darwin" {
			stateRoot = filepath.Join(home, "Library", "Application Support")
		} else {
			stateRoot = filepath.Join(home, ".local", "state")
		}
	}
	return Paths{
		ConfigFile: filepath.Join(configRoot, "authwatch", "config.toml"),
		StateFile:  filepath.Join(stateRoot, "authwatch", "state.json"),
	}, nil
}

// ResolveConfigDir resolves a profile's CLI config directory.
// If profileConfigDir is empty, CLAUDE_CONFIG_DIR from env is used, falling back to ~/.claude.
func ResolveConfigDir(profileConfigDir string, env []string) (string, error) {
	m := envValues(env)
	home := m["HOME"]
	if home == "" {
		return "", fmt.Errorf("HOME is not set")
	}
	raw := strings.TrimSpace(profileConfigDir)
	if raw == "" {
		raw = strings.TrimSpace(m["CLAUDE_CONFIG_DIR"])
	}
	if raw == "" {
		return filepath.Join(home, ".claude"), nil
	}
	return expandHome(raw, home), nil
}

// KeychainServiceName returns the macOS keychain service name for configDir.
// The default ~/.claude directory uses DefaultKeychainService; custom config
// directories use a short SHA-256 suffix of the cleaned path.
func KeychainServiceName(configDir, homeDir string) string {
	normConfig := filepath.Clean(configDir)
	normDefault := filepath.Clean(filepath.Join(homeDir, ".claude"))
	if normConfig == normDefault {
		return DefaultKeychainService
	}
	sum := sha256.Sum256([]byte(normConfig))
	return DefaultKeychainService + "-" + hex.EncodeToString(sum[:])[:8]
}

func expandHome(p, home string) string {
	if p == "~" {
		return home
	}
	if strings.HasPrefix(p, "~/") {
		return filepath.Join(home, p[2:])
	}
	return filepath.Clean(p)
}

func envValues(env []string) map[string]string {
	values := make(map[string]string, len(env))
	for _, item := range env {
		if k, v, ok := strings.Cut(item, "="); ok {
			values[k] = v
		}
	}
	return values
}

// UserEnv returns the current process environment.
func UserEnv() []string {
	return os.Environ()
}
