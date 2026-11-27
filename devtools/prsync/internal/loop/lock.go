package loop

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"golang.org/x/sys/unix"
)

// ErrAlreadyRunning is returned when another prsync loop instance already
// holds the per-config lock file.
var ErrAlreadyRunning = errors.New("loop already running")

// Lock holds an exclusive advisory flock on a per-config lock file.
type Lock struct {
	path string
	file *os.File
}

// NormalizedConfigPath returns a canonical path used to scope loop lock and
// heartbeat files per config, or "" for default (configless) runs.
func NormalizedConfigPath(configPath string) string {
	if configPath == "" {
		return ""
	}
	abs, err := filepath.Abs(configPath)
	if err != nil {
		return filepath.Clean(configPath)
	}
	if resolved, err := filepath.EvalSymlinks(abs); err == nil {
		return filepath.Clean(resolved)
	}
	return filepath.Clean(abs)
}

func configSuffix(configPath string) string {
	norm := NormalizedConfigPath(configPath)
	if norm == "" {
		return ""
	}
	sum := sha256.Sum256([]byte(norm))
	return "-" + hex.EncodeToString(sum[:6])
}

// LockPath returns the lock file path in stateDir for configPath.
func LockPath(stateDir, configPath string) string {
	return filepath.Join(stateDir, "loop"+configSuffix(configPath)+".lock")
}

// HeartbeatPath returns the heartbeat file path in stateDir for configPath.
func HeartbeatPath(stateDir, configPath string) string {
	return filepath.Join(stateDir, "loop"+configSuffix(configPath)+".heartbeat.json")
}

// AcquireLock takes a non-blocking exclusive flock on path so only one loop
// per config runs at a time.
func AcquireLock(path string) (*Lock, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return nil, fmt.Errorf("create loop lock dir: %w", err)
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600) //nolint:gosec // loop lock path inside state dir
	if err != nil {
		return nil, fmt.Errorf("open loop lock: %w", err)
	}
	for {
		err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB) //nolint:gosec // G115: file descriptor fits in int
		if err == nil {
			break
		}
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if errors.Is(err, unix.EWOULDBLOCK) {
			holder := readLockPID(f)
			_ = f.Close()
			if holder != "" {
				return nil, fmt.Errorf("%w (pid %s, lock %s)", ErrAlreadyRunning, holder, path)
			}
			return nil, fmt.Errorf("%w (lock %s)", ErrAlreadyRunning, path)
		}
		_ = f.Close()
		return nil, fmt.Errorf("lock %s: %w", path, err)
	}
	if err := f.Truncate(0); err == nil {
		_, _ = f.Seek(0, io.SeekStart)
		_, _ = fmt.Fprintf(f, "%d\n", os.Getpid())
	}
	return &Lock{path: path, file: f}, nil
}

// Close releases the flock and closes the underlying descriptor.
func (l *Lock) Close() error {
	if l == nil || l.file == nil {
		return nil
	}
	_ = unix.Flock(int(l.file.Fd()), unix.LOCK_UN) //nolint:gosec // G115: file descriptor fits in int
	err := l.file.Close()
	l.file = nil
	if err != nil {
		return fmt.Errorf("close loop lock %s: %w", l.path, err)
	}
	return nil
}

func readLockPID(f *os.File) string {
	_, _ = f.Seek(0, io.SeekStart)
	buf := make([]byte, 32)
	n, _ := f.Read(buf)
	return strings.TrimSpace(string(buf[:n]))
}
