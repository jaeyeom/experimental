package outbox

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"

	"golang.org/x/sys/unix"
)

// ErrLockHeld is returned when another poller already holds the directory lock.
var ErrLockHeld = errors.New("another poll holds the outbox lock")

// LockDir acquires a non-blocking exclusive flock on dir/poll.lock.
// The returned unlock function closes the lock file and releases the lock.
func LockDir(dir string) (func(), error) {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, fmt.Errorf("create data dir: %w", err)
	}
	f, err := os.OpenFile(filepath.Join(dir, "poll.lock"), os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open poll lock: %w", err)
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB); err != nil {
		closeErr := f.Close()
		if errors.Is(err, unix.EWOULDBLOCK) {
			return nil, errors.Join(ErrLockHeld, closeErr)
		}
		return nil, errors.Join(fmt.Errorf("lock %s: %w", dir, err), closeErr)
	}
	return func() { _ = f.Close() }, nil
}
