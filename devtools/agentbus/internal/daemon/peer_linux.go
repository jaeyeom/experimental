//go:build linux

package daemon

import (
	"fmt"

	"golang.org/x/sys/unix"
)

func peerPIDSyscall(fd int) (int, error) {
	cred, err := unix.GetsockoptUcred(fd, unix.SOL_SOCKET, unix.SO_PEERCRED)
	if err != nil {
		return 0, fmt.Errorf("socket peer cred: %w", err)
	}
	return int(cred.Pid), nil //nolint:gosec // G115: a pid fits in int
}
