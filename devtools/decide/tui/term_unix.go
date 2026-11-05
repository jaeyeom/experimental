//go:build darwin || linux

package tui

import (
	"fmt"
	"io"
	"os"

	"golang.org/x/sys/unix"
)

func makeRaw(in io.Reader) (func(), error) {
	f, ok := in.(*os.File)
	if !ok {
		return func() {}, nil
	}
	fd := int(f.Fd()) //nolint:gosec // G115: a file descriptor fits in int
	state, err := unix.IoctlGetTermios(fd, termiosGet)
	if err != nil {
		return func() {}, nil
	}
	raw := *state
	raw.Lflag &^= unix.ECHO | unix.ICANON
	raw.Cc[unix.VMIN] = 1
	raw.Cc[unix.VTIME] = 0
	if err := unix.IoctlSetTermios(fd, termiosSet, &raw); err != nil {
		return nil, fmt.Errorf("set decide inbox raw mode: %w", err)
	}
	return func() {
		_ = unix.IoctlSetTermios(fd, termiosSet, state)
	}, nil
}
