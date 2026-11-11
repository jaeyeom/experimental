//go:build darwin || linux

package tui

import (
	"io"
	"os"
	"time"

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

// inputReady reports whether f has input within a wait. It uses select,
// which works on macOS terminals where poll does not.
func inputReady(f *os.File) func(time.Duration) bool {
	fd := int(f.Fd()) //nolint:gosec // G115: a file descriptor fits in int
	return func(wait time.Duration) bool {
		var set unix.FdSet
		set.Set(fd)
		tv := unix.NsecToTimeval(wait.Nanoseconds())
		n, err := unix.Select(fd+1, &set, nil, nil, &tv)
		return err == nil && n > 0
	}
}
