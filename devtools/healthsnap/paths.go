package healthsnap

import (
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// Paths holds the resolved config file and state directory for healthsnap.
type Paths struct {
	ConfigFile string
	StateDir   string
}

// LocatePaths resolves config and state paths from env using XDG directories
// on Linux and Application Support on macOS when XDG variables are unset.
func LocatePaths(env []string) (Paths, error) {
	return locatePaths(runtime.GOOS, envValues(env))
}

func locatePaths(goos string, env map[string]string) (Paths, error) {
	home := env["HOME"]
	if home == "" {
		return Paths{}, errors.New("HOME is not set")
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
		ConfigFile: filepath.Join(configRoot, "healthsnap", "config.pkl"),
		StateDir:   filepath.Join(stateRoot, "healthsnap"),
	}, nil
}

// ExpandHome expands a leading "~" or "~/" using HOME from env.
func ExpandHome(p string, env []string) string {
	if p != "~" && !strings.HasPrefix(p, "~/") {
		return p
	}
	home := envValues(env)["HOME"]
	if home == "" {
		return p
	}
	if p == "~" {
		return home
	}
	return filepath.Join(home, p[2:])
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
