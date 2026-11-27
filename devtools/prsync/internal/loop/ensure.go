package loop

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"golang.org/x/sys/unix"
)

// Ensure action values reported by Ensure.
const (
	EnsureHealthy = "healthy"
	EnsureStarted = "started"
)

// EnsureOptions configures Ensure.
type EnsureOptions struct {
	StaleAfter time.Duration
	Args       []string
	Clock      Clock
	IsAlive    func(pid int) bool
	Spawner    func(args []string) (int, error)
}

// EnsureResult describes whether a healthy loop was already running or a
// detached loop process was started.
type EnsureResult struct {
	Action        string `json:"action"`
	PID           int    `json:"pid"`
	Round         int    `json:"round,omitempty"`
	HeartbeatPath string `json:"heartbeat_path"` //nolint:tagliatelle // outbound contract
}

// ProcessAlive reports whether pid exists on this host.
func ProcessAlive(pid int) bool {
	if pid <= 0 {
		return false
	}
	err := unix.Kill(pid, 0)
	return err == nil || errors.Is(err, unix.EPERM)
}

// DefaultSpawner starts the current prsync binary detached in a new session
// without keeping stdio pipes open.
func DefaultSpawner(args []string) (int, error) {
	exe, err := os.Executable()
	if err != nil {
		return 0, fmt.Errorf("resolve prsync executable: %w", err)
	}
	// Re-executes the current prsync binary from os.Executable() with
	// validated CLI flags directly (no shell).
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(exe, args...) //nolint:gosec // G204: re-execs the running prsync binary with validated flags
	cmd.Stdin = nil
	cmd.Stdout = nil
	cmd.Stderr = nil
	cmd.SysProcAttr = &unix.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		return 0, fmt.Errorf("start detached loop: %w", err)
	}
	pid := cmd.Process.Pid
	_ = cmd.Process.Release()
	return pid, nil
}

// Ensure checks the per-config heartbeat and PID liveness. When the heartbeat is
// fresh and the PID is alive, it returns EnsureHealthy immediately; otherwise
// it starts a detached loop process (which enforces single-instance locking).
func Ensure(cfg config.Config, opts EnsureOptions) (EnsureResult, error) {
	if opts.Clock == nil {
		opts.Clock = realClock{}
	}
	if opts.StaleAfter <= 0 {
		opts.StaleAfter = DefaultStaleAfter
	}
	if opts.IsAlive == nil {
		opts.IsAlive = ProcessAlive
	}
	if opts.Spawner == nil {
		opts.Spawner = DefaultSpawner
	}

	hbPath := HeartbeatPath(filepath.Dir(cfg.StateFile), cfg.SourcePath)
	hb, exists, err := ReadHeartbeat(hbPath)
	now := opts.Clock.Now().UTC()
	if exists && err == nil && isHealthy(hb, now, opts.StaleAfter, opts.IsAlive) {
		return EnsureResult{
			Action:        EnsureHealthy,
			PID:           hb.PID,
			Round:         hb.Round,
			HeartbeatPath: hbPath,
		}, nil
	}

	pid, err := opts.Spawner(opts.Args)
	if err != nil {
		return EnsureResult{}, err
	}
	return EnsureResult{
		Action:        EnsureStarted,
		PID:           pid,
		HeartbeatPath: hbPath,
	}, nil
}

func isHealthy(hb Heartbeat, now time.Time, staleAfter time.Duration, isAlive func(int) bool) bool {
	if hb.PID <= 0 || !isAlive(hb.PID) {
		return false
	}
	ts := hb.freshTimestamp()
	if ts.IsZero() {
		return false
	}
	return now.Sub(ts) <= staleAfter
}
