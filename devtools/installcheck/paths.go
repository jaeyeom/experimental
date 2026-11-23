package installcheck

import (
	"errors"
	"path/filepath"
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
	get := func(key string) string {
		prefix := key + "="
		for _, e := range env {
			if len(e) >= len(prefix) && e[:len(prefix)] == prefix {
				return e[len(prefix):]
			}
		}
		return ""
	}
	home := get("HOME")
	cfgRoot, stateRoot := get("XDG_CONFIG_HOME"), get("XDG_STATE_HOME")
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
