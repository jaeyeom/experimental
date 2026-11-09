package daemon

import (
	"net"
)

// peerPID is the process on the other end of a unix connection.
// Zero means it could not be read. The client does not send this value.
func peerPID(conn net.Conn) int {
	uc, ok := conn.(*net.UnixConn)
	if !ok {
		return 0
	}
	raw, err := uc.SyscallConn()
	if err != nil {
		return 0
	}
	var pid int
	controlErr := raw.Control(func(fd uintptr) {
		got, pidErr := peerPIDFromFD(int(fd)) //nolint:gosec // G115: a file descriptor fits in int
		if pidErr == nil {
			pid = got
		}
	})
	if controlErr != nil {
		return 0
	}
	return pid
}
