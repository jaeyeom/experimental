//go:build unix

package remotecall

import (
	"os/exec"

	"golang.org/x/sys/unix"
)

// setProcessGroup starts cmd in a new process group and makes a cancel
// kill the whole group. With detach, cmd starts a new session instead,
// which is also a new process group, with no controlling terminal.
func setProcessGroup(cmd *exec.Cmd, detach bool) {
	if detach {
		cmd.SysProcAttr = &unix.SysProcAttr{Setsid: true}
	} else {
		cmd.SysProcAttr = &unix.SysProcAttr{Setpgid: true}
	}
	cmd.Cancel = func() error {
		return unix.Kill(-cmd.Process.Pid, unix.SIGKILL)
	}
}
