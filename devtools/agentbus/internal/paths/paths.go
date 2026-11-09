// Package paths locates the agentbus socket and on-disk state.
package paths

import (
	"errors"
	"path/filepath"
	"strings"
)

// Dirs is where one agentbus instance keeps its socket and state.
type Dirs struct {
	SocketPath string
	StateDir   string
}

// FromEnv reads XDG_RUNTIME_DIR and the state directory from env.
// State is $XDG_STATE_HOME/agentbus, or $HOME/.local/state/agentbus when
// XDG_STATE_HOME is unset. Both locations are required; a missing one is
// reported so the CLI can fail instead of guessing.
func FromEnv(env []string) (Dirs, error) {
	values := make(map[string]string, len(env))
	for _, entry := range env {
		key, value, ok := strings.Cut(entry, "=")
		if !ok || key == "" {
			continue
		}
		values[key] = value
	}
	runtime := values["XDG_RUNTIME_DIR"]
	if runtime == "" {
		return Dirs{}, errors.New("XDG_RUNTIME_DIR is not set; the agentbus daemon socket needs it")
	}
	stateRoot := values["XDG_STATE_HOME"]
	if stateRoot == "" {
		home := values["HOME"]
		if home == "" {
			return Dirs{}, errors.New("HOME is not set; agentbus state belongs in ~/.local/state/agentbus")
		}
		stateRoot = filepath.Join(home, ".local", "state")
	}
	return Dirs{
		SocketPath: filepath.Join(runtime, "agentbus", "daemon.sock"),
		StateDir:   filepath.Join(stateRoot, "agentbus"),
	}, nil
}
