//go:build darwin || linux

package daemon

import (
	"fmt"
)

func peerPIDFromFD(fd int) (int, error) {
	pid, err := peerPIDSyscall(fd)
	if err != nil {
		return 0, fmt.Errorf("peer pid: %w", err)
	}
	return pid, nil
}
