//go:build unix

package sandbox

import (
	"errors"
	"fmt"
	"os/exec"
	"time"

	"golang.org/x/sys/unix"
)

func configureProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &unix.SysProcAttr{Setpgid: true}
	cmd.Cancel = func() error {
		if err := unix.Kill(-cmd.Process.Pid, unix.SIGKILL); err != nil && !errors.Is(err, unix.ESRCH) {
			return fmt.Errorf("kill process group %d: %w", cmd.Process.Pid, err)
		}
		return nil
	}
}

// Terminate sends SIGTERM to the process group, waits up to grace (default 5s)
// for all members to exit, and escalates to SIGKILL if any remain alive.
func (p *Process) Terminate(grace time.Duration) error {
	if grace <= 0 {
		grace = defaultTerminateGrace
	}
	if p.exited() && !processGroupAlive(p.pid) {
		return nil
	}

	if err := unix.Kill(-p.pid, unix.SIGTERM); err != nil && !errors.Is(err, unix.ESRCH) {
		return fmt.Errorf("send SIGTERM to process group %d: %w", p.pid, err)
	}

	if p.waitForGroupExit(grace) {
		return nil
	}

	if err := unix.Kill(-p.pid, unix.SIGKILL); err != nil && !errors.Is(err, unix.ESRCH) {
		return fmt.Errorf("send SIGKILL to process group %d: %w", p.pid, err)
	}
	<-p.done
	return nil
}

func (p *Process) waitForGroupExit(grace time.Duration) bool {
	deadline := time.NewTimer(grace)
	defer deadline.Stop()

	select {
	case <-p.done:
	case <-deadline.C:
		return false
	}

	if !processGroupAlive(p.pid) {
		return true
	}

	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-deadline.C:
			return !processGroupAlive(p.pid)
		case <-ticker.C:
			if !processGroupAlive(p.pid) {
				return true
			}
		}
	}
}

func processGroupAlive(pgid int) bool {
	return processAlive(-pgid)
}

func processAlive(pid int) bool {
	err := unix.Kill(pid, 0)
	return err == nil || errors.Is(err, unix.EPERM)
}
