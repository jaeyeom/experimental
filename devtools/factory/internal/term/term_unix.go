//go:build unix

package term

import (
	"os"

	"golang.org/x/sys/unix"
)

// Width returns the column count of the terminal on f, and false when f is
// not a terminal.
func Width(f *os.File) (int, bool) {
	if f == nil {
		return 0, false
	}
	ws, err := unix.IoctlGetWinsize(int(f.Fd()), unix.TIOCGWINSZ) //nolint:gosec // G115: a file descriptor fits in an int.
	if err != nil || ws.Col == 0 {
		return 0, false
	}
	return int(ws.Col), true
}
