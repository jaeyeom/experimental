package ghoutbox

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"

	"golang.org/x/sys/unix"
)

// ErrPollRunning is returned when another poll holds the data dir lock.
var ErrPollRunning = errors.New("another gh-outbox poll is running")

// lockDir takes an exclusive lock on dir so only one poller posts.
func lockDir(dir string) (func(), error) {
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
			return nil, errors.Join(ErrPollRunning, closeErr)
		}
		return nil, errors.Join(fmt.Errorf("lock %s: %w", dir, err), closeErr)
	}
	// Closing the file releases the lock.
	return func() { _ = f.Close() }, nil
}
