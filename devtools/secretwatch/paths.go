package secretwatch

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// Paths holds the resolved config and state file locations for secretwatch.
type Paths struct {
	ConfigFile string
	StateFile  string
}

// LocatePaths resolves config and state paths from env using XDG directories
// on Linux and Application Support on macOS when XDG variables are unset.
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
		ConfigFile: filepath.Join(configRoot, "secretwatch", "config.pkl"),
		StateFile:  filepath.Join(stateRoot, "secretwatch", "state.json"),
	}, nil
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
