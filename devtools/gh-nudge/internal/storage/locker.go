package storage

import (
	"fmt"
	"os"
	"path/filepath"
)

// FileLockManager implements the Locker interface using file-based locking.
type FileLockManager struct {
	store       Store
	lockManager *LockManager
}

// NewFileLockManager creates a new FileLockManager instance.
func NewFileLockManager(store Store) *FileLockManager {
	return &FileLockManager{
		store:       store,
		lockManager: NewLockManager(),
	}
}

func (flm *FileLockManager) getFilePath(key string) string {
	if fs, ok := flm.store.(*FileSystemStore); ok {
		return filepath.Join(fs.GetRootPath(), key)
	}
	return key
}

func (flm *FileLockManager) WithLock(key string, fn func() error) error {
	lockPath := flm.getFilePath(key + ".lock")

	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory for lock file: %w", err)
	}

	if err := flm.lockManager.AcquireLock(lockPath); err != nil {
		return fmt.Errorf("failed to acquire lock for key %s: %w", key, err)
	}
	defer func() {
		if err := flm.lockManager.ReleaseLock(lockPath); err != nil {
			fmt.Fprintf(os.Stderr, "Warning: failed to release lock %s: %v\n", lockPath, err)
		}
	}()

	return fn()
}
