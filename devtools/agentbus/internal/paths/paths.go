// Package paths locates the agentbus socket and on-disk state.
package paths

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
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
// XDG_STATE_HOME is unset. The daemon and the log command use this.
func StateDir(env []string) (string, error) {
	return stateDir(env, "")
}

// ClientStateDir is where a hub client keeps its outbox. On darwin, with
// XDG_STATE_HOME unset, that is ~/Library/Caches/agentbus/state, next to
// the run sockets. XDG_STATE_HOME still wins. Linux stays under ~/.local/state.
func ClientStateDir(env []string) (string, error) {
	return stateDir(env, runtime.GOOS)
}

func stateDir(env []string, goos string) (string, error) {
	values := envValues(env)
	if root := values["XDG_STATE_HOME"]; root != "" {
		return filepath.Join(root, "agentbus"), nil
	}
	home := values["HOME"]
	if home == "" {
		return "", errors.New("HOME is not set; agentbus state belongs in ~/.local/state/agentbus")
	}
	if goos == "darwin" {
		return filepath.Join(home, "Library", "Caches", "agentbus", "state"), nil
	}
	return filepath.Join(home, ".local", "state", "agentbus"), nil
}

// chmodRefuse lists runtime directories whose chmod fails. Tests use it
// to simulate a sandbox that denies the call. The key is the directory.
var chmodRefuse sync.Map

func chmodDir(path string, mode os.FileMode) error {
	if _, ok := chmodRefuse.Load(path); ok {
		return errors.New("operation not permitted")
	}
	return os.Chmod(path, mode) //nolint:wrapcheck // ensureRuntime wraps this as "create runtime dir"
}

// Locate reads the runtime and state directories from env and does not
// create or chmod anything. Doctor uses it when creating the runtime
// directory fails, so the sandbox advice still prints.
func Locate(env []string) (Dirs, error) {
	return locate(env, runtime.GOOS)
}

// FromEnv reads the runtime and state directories from env.
// On Linux, XDG_RUNTIME_DIR is required. On darwin, an unset
// XDG_RUNTIME_DIR uses ~/Library/Caches/agentbus/run, created mode 0700.
// A set XDG_RUNTIME_DIR is used as given and that default is not created.
// A runtime directory that is already mode 0700 is left alone. A chmod
// or mkdir failure is not fatal when a socket is already there.
func FromEnv(env []string) (Dirs, error) {
	return fromEnv(env, runtime.GOOS)
}

func locate(env []string, goos string) (Dirs, error) {
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
	return Dirs{
		SocketPath:       filepath.Join(runtimeDir, "agentbus", "daemon.sock"),
		HubSocketPath:    filepath.Join(runtimeDir, "agentbus", "hub.sock"),
		TunnelSocketPath: filepath.Join(runtimeDir, "agentbus", "tunnel.sock"),
		StateDir:         state,
	}, nil
}

func fromEnv(env []string, goos string) (Dirs, error) {
	dirs, err := locate(env, goos)
	if err != nil {
		return Dirs{}, err
	}
	values := envValues(env)
	if values["XDG_RUNTIME_DIR"] != "" || goos != "darwin" {
		return dirs, nil
	}
	if err := checkSunPaths(dirs); err != nil {
		return Dirs{}, err
	}
	runtimeDir := filepath.Dir(filepath.Dir(dirs.SocketPath))
	if err := ensureRuntime(runtimeDir, dirs); err != nil {
		return Dirs{}, err
	}
	return dirs, nil
}

// ensureRuntime makes runtimeDir mode 0700. An existing 0700 directory
// is not chmodded: a sandbox denies that even when the mode is already
// right. When a socket is already in place, a later failure is ignored.
func ensureRuntime(runtimeDir string, dirs Dirs) error {
	info, err := os.Stat(runtimeDir)
	if err == nil && info.IsDir() && info.Mode().Perm() == 0o700 {
		return nil
	}
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("create runtime dir: %w", err)
	}
	if errors.Is(err, fs.ErrNotExist) {
		if mkErr := os.MkdirAll(runtimeDir, 0o700); mkErr != nil {
			return fmt.Errorf("create runtime dir: %w", mkErr)
		}
	} else if info == nil || !info.IsDir() {
		return fmt.Errorf("create runtime dir: %s is not a directory", runtimeDir)
	}
	if chErr := chmodDir(runtimeDir, 0o700); chErr != nil {
		if socketsReady(dirs) {
			return nil
		}
		return fmt.Errorf("create runtime dir: %w", chErr)
	}
	return nil
}

func socketsReady(dirs Dirs) bool {
	for _, path := range []string{dirs.SocketPath, dirs.HubSocketPath, dirs.TunnelSocketPath} {
		if _, err := os.Stat(path); err == nil {
			return true
		}
	}
	return false
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
