//go:build darwin

package daemon

import (
	"fmt"

	"golang.org/x/sys/unix"
)

func peerPIDSyscall(fd int) (int, error) {
	pid, err := unix.GetsockoptInt(fd, unix.SOL_LOCAL, unix.LOCAL_PEERPID)
	if err != nil {
		return 0, fmt.Errorf("local peer pid: %w", err)
	}
	return pid, nil
}
