//go:build darwin || linux

package tui

import (
	"fmt"
	"io"
	"os"

	"golang.org/x/sys/unix"
)

func makeRaw(in io.Reader) (func(), bool, error) {
	f, ok := in.(*os.File)
	if !ok {
		return func() {}, false, nil
	}
	fd := int(f.Fd()) //nolint:gosec // G115: a file descriptor fits in int
	state, err := unix.IoctlGetTermios(fd, termiosGet)
	if err != nil {
		return func() {}, false, nil
	}
	raw := *state
	raw.Lflag &^= unix.ECHO | unix.ICANON
	raw.Cc[unix.VMIN] = 1
	raw.Cc[unix.VTIME] = 0
	if err := unix.IoctlSetTermios(fd, termiosSet, &raw); err != nil {
		return nil, false, fmt.Errorf("set decide inbox raw mode: %w", err)
	}
	return func() {
		_ = unix.IoctlSetTermios(fd, termiosSet, state)
	}, true, nil
}

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
