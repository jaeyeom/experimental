//go:build darwin || linux

package tui

import (
	"io"
	"os"

	"golang.org/x/sys/unix"
)

func termRows(in io.Reader) int {
	f, ok := in.(*os.File)
	if !ok {
		return 0
	}
	fd := int(f.Fd()) //nolint:gosec // G115: a file descriptor fits in int
	ws, err := unix.IoctlGetWinsize(fd, unix.TIOCGWINSZ)
	if err != nil || ws.Row == 0 {
		return 0
	}
	return int(ws.Row)
}
