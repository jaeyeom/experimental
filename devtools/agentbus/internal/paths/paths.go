// Package paths locates the agentbus socket and on-disk state.
package paths

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// sunPathLimit is the darwin sockaddr_un path size, including the NUL.
const sunPathLimit = 104

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

// FromEnv reads the runtime and state directories from env.
// On Linux, XDG_RUNTIME_DIR is required. On darwin, an unset
// XDG_RUNTIME_DIR uses ~/Library/Caches/agentbus/run, created mode 0700.
// A set XDG_RUNTIME_DIR is used as given and that default is not created.
func FromEnv(env []string) (Dirs, error) {
	return fromEnv(env, runtime.GOOS)
}

func fromEnv(env []string, goos string) (Dirs, error) {
	state, err := StateDir(env)
	if err != nil {
		return Dirs{}, err
	}
	values := envValues(env)
	runtimeDir := values["XDG_RUNTIME_DIR"]
	if runtimeDir == "" {
		runtimeDir, err = defaultRuntimeDir(values["HOME"], goos)
		if err != nil {
			return Dirs{}, err
		}
	}
	dirs := Dirs{
		SocketPath:       filepath.Join(runtimeDir, "agentbus", "daemon.sock"),
		HubSocketPath:    filepath.Join(runtimeDir, "agentbus", "hub.sock"),
		TunnelSocketPath: filepath.Join(runtimeDir, "agentbus", "tunnel.sock"),
		StateDir:         state,
	}
	if values["XDG_RUNTIME_DIR"] != "" || goos != "darwin" {
		return dirs, nil
	}
	if err := checkSunPaths(dirs); err != nil {
		return Dirs{}, err
	}
	if err := os.MkdirAll(runtimeDir, 0o700); err != nil {
		return Dirs{}, fmt.Errorf("create runtime dir: %w", err)
	}
	if err := os.Chmod(runtimeDir, 0o700); err != nil {
		return Dirs{}, fmt.Errorf("create runtime dir: %w", err)
	}
	return dirs, nil
}

// defaultRuntimeDir is the runtime directory when XDG_RUNTIME_DIR is unset.
func defaultRuntimeDir(home, goos string) (string, error) {
	if goos != "darwin" {
		return "", errors.New("XDG_RUNTIME_DIR is not set; the agentbus daemon socket needs it")
	}
	if home == "" {
		return "", errors.New("HOME is not set; the agentbus runtime directory belongs in ~/Library/Caches/agentbus/run")
	}
	return filepath.Join(home, "Library", "Caches", "agentbus", "run"), nil
}

// checkSunPaths rejects a default darwin socket path that cannot fit in sun_path.
func checkSunPaths(dirs Dirs) error {
	for _, path := range []string{dirs.SocketPath, dirs.HubSocketPath, dirs.TunnelSocketPath} {
		if len(path) >= sunPathLimit {
			return fmt.Errorf("socket path %s is too long: %d bytes, over the %d-byte sun_path limit including the terminating NUL", path, len(path), sunPathLimit)
		}
	}
	return nil
}

// LeaseCacheDir is where the lease hook caches "you hold it" answers:
// lease-cache beside the daemon socket, in the runtime directory. It is
// empty when SocketPath is.
func (d Dirs) LeaseCacheDir() string {
	if d.SocketPath == "" {
		return ""
	}
	return filepath.Join(filepath.Dir(d.SocketPath), "lease-cache")
}

// PaneCacheDir is where `agentbus register` caches each herdr pane's
// handle, so a sandboxed call that cannot reach the herdr socket still
// knows its caller: panes beside the daemon socket, in the runtime
// directory. It is empty when SocketPath is.
func (d Dirs) PaneCacheDir() string {
	if d.SocketPath == "" {
		return ""
	}
	return filepath.Join(filepath.Dir(d.SocketPath), "panes")
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
