//go:build unix

package remotecall

import (
	"os/exec"

	"golang.org/x/sys/unix"
)

// setProcessGroup starts cmd in a new process group and makes a cancel
// kill the whole group.
func setProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &unix.SysProcAttr{Setpgid: true}
	cmd.Cancel = func() error {
		return unix.Kill(-cmd.Process.Pid, unix.SIGKILL)
	}
}
