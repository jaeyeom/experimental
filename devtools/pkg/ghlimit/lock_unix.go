//go:build darwin || linux

package ghlimit

import (
	"errors"
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// tryLock takes an exclusive flock on f without blocking. The kernel
// drops it when f is closed or the process dies.
func tryLock(f *os.File) (bool, error) {
	for {
		err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB) //nolint:gosec // G115: a file descriptor fits in int
		switch {
		case err == nil:
			return true, nil
		case errors.Is(err, unix.EWOULDBLOCK):
			return false, nil
		case errors.Is(err, unix.EINTR):
			continue
		default:
			return false, fmt.Errorf("flock %s: %w", f.Name(), err)
		}
	}
}
