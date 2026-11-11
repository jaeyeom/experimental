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
	// HubSocketPath is where a hub's daemon takes forwarded callers from
	// other machines. An ssh -L forward points at it.
	HubSocketPath string
	// TunnelSocketPath is where a second machine in hub mode finds the
	// forwarded hub socket.
	TunnelSocketPath string
	StateDir         string
}

// StateDir is $XDG_STATE_HOME/agentbus, or $HOME/.local/state/agentbus when
// XDG_STATE_HOME is unset. The log command reads this without a daemon.
func StateDir(env []string) (string, error) {
	values := envValues(env)
	stateRoot := values["XDG_STATE_HOME"]
	if stateRoot == "" {
		home := values["HOME"]
		if home == "" {
			return "", errors.New("HOME is not set; agentbus state belongs in ~/.local/state/agentbus")
		}
		stateRoot = filepath.Join(home, ".local", "state")
	}
	return filepath.Join(stateRoot, "agentbus"), nil
}

// FromEnv reads XDG_RUNTIME_DIR and the state directory from env.
// Both locations are required; a missing one is reported so the CLI can
// fail instead of guessing.
func FromEnv(env []string) (Dirs, error) {
	state, err := StateDir(env)
	if err != nil {
		return Dirs{}, err
	}
	runtime := envValues(env)["XDG_RUNTIME_DIR"]
	if runtime == "" {
		return Dirs{}, errors.New("XDG_RUNTIME_DIR is not set; the agentbus daemon socket needs it")
	}
	return Dirs{
		SocketPath:       filepath.Join(runtime, "agentbus", "daemon.sock"),
		HubSocketPath:    filepath.Join(runtime, "agentbus", "hub.sock"),
		TunnelSocketPath: filepath.Join(runtime, "agentbus", "tunnel.sock"),
		StateDir:         state,
	}, nil
}

func envValues(env []string) map[string]string {
	values := make(map[string]string, len(env))
	for _, entry := range env {
		key, value, ok := strings.Cut(entry, "=")
		if !ok || key == "" {
			continue
		}
		values[key] = value
	}
	return values
}
