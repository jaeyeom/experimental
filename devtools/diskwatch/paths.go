package diskwatch

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
)

// Paths is where config and state live for this user.
type Paths struct {
	ConfigFile string
	StateFile  string
}

// LocatePaths resolves config and state from the environment.
// State is $XDG_STATE_HOME/diskwatch/state.json, or
// ~/Library/Application Support/diskwatch on macOS when XDG_STATE_HOME
// is unset. Config is $XDG_CONFIG_HOME/diskwatch/config.toml, or
// ~/.config/diskwatch/config.toml, with ~/Library/Application Support
// on macOS when XDG_CONFIG_HOME is unset.
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
		ConfigFile: filepath.Join(configRoot, "diskwatch", "config.toml"),
		StateFile:  filepath.Join(stateRoot, "diskwatch", "state.json"),
	}, nil
}

func envValues(env []string) map[string]string {
	values := make(map[string]string, len(env))
	for _, item := range env {
		key, value, ok := splitEnv(item)
		if ok {
			values[key] = value
		}
	}
	return values
}

func splitEnv(item string) (string, string, bool) {
	for i := 0; i < len(item); i++ {
		if item[i] == '=' {
			return item[:i], item[i+1:], true
		}
	}
	return "", "", false
}

// UserEnv is the process environment.
func UserEnv() []string {
	return os.Environ()
}
