package installcheck

import (
	"errors"
	"path/filepath"
	"strings"
)

// Paths locates the default config and state files.
type Paths struct {
	ConfigFile string
	StateFile  string
}

// LocatePaths resolves the defaults from env (KEY=VALUE entries):
// $XDG_CONFIG_HOME/install-check/config.toml or
// ~/.config/install-check/config.toml, and
// $XDG_STATE_HOME/install-check/state.json or
// ~/.local/state/install-check/state.json.
func LocatePaths(env []string) (Paths, error) {
	home, _ := lookupEnv(env, "HOME")
	cfgRoot, _ := lookupEnv(env, "XDG_CONFIG_HOME")
	stateRoot, _ := lookupEnv(env, "XDG_STATE_HOME")
	if home == "" && (cfgRoot == "" || stateRoot == "") {
		return Paths{}, errors.New("HOME is not set")
	}
	if cfgRoot == "" {
		cfgRoot = filepath.Join(home, ".config")
	}
	if stateRoot == "" {
		stateRoot = filepath.Join(home, ".local", "state")
	}
	return Paths{
		ConfigFile: filepath.Join(cfgRoot, "install-check", "config.toml"),
		StateFile:  filepath.Join(stateRoot, "install-check", "state.json"),
	}, nil
}

func lookupEnv(env []string, key string) (string, bool) {
	var val string
	var found bool
	for _, entry := range env {
		if k, v, ok := strings.Cut(entry, "="); ok && k == key {
			val = v
			found = true
		}
	}
	return val, found
}

func expandHomePath(p string, env []string) (string, error) {
	if p != "~" && !strings.HasPrefix(p, "~/") {
		return p, nil
	}
	home, _ := lookupEnv(env, "HOME")
	if home == "" {
		return "", errors.New("HOME is not set")
	}
	if p == "~" {
		return home, nil
	}
	return filepath.Join(home, p[2:]), nil
}
