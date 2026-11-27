package ghoutbox

import (
	"errors"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/pkg/outbox"
)

// ErrPollRunning is returned when another poll holds the data dir lock.
var ErrPollRunning = errors.New("another gh-outbox poll is running")

// lockDir takes an exclusive lock on dir so only one poller posts.
func lockDir(dir string) (func(), error) {
	unlock, err := outbox.LockDir(dir)
	if errors.Is(err, outbox.ErrLockHeld) {
		return nil, ErrPollRunning
	}
	if err != nil {
		return nil, fmt.Errorf("lock %s: %w", dir, err)
	}
	return unlock, nil
}
