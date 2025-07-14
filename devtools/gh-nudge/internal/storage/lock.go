package storage

import (
	"fmt"
	"os"
	"sync"
	"time"
)

// LockManager manages file-based exclusive locks for concurrent access control.
// It maintains an in-memory map of active locks and uses filesystem-level locking
// to prevent race conditions between multiple processes.
type LockManager struct {
	locks map[string]*os.File
	mutex sync.RWMutex
}

// NewLockManager creates a new LockManager instance with an empty lock registry.
// The returned LockManager is ready to acquire and manage file-based locks.
func NewLockManager() *LockManager {
	return &LockManager{
		locks: make(map[string]*os.File),
	}
}

// AcquireLock attempts to acquire an exclusive file-based lock at the specified path.
// It uses O_EXCL flag to atomically create the lock file, failing if it already exists.
// Returns an error if the lock is already held by this manager or if filesystem operations fail.
func (lm *LockManager) AcquireLock(lockPath string) error {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	if _, exists := lm.locks[lockPath]; exists {
		return fmt.Errorf("lock already held for path: %s", lockPath)
	}

	// Use O_EXCL flag for atomic exclusive lock creation - fails if file already exists
	file, err := os.OpenFile(lockPath, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("failed to acquire lock: %w", err)
	}

	if _, err := fmt.Fprintf(file, "locked_at: %s\n", time.Now().Format(time.RFC3339)); err != nil {
		// Clean up on write failure: close file handle and remove lock file
		file.Close()
		os.Remove(lockPath)
		return fmt.Errorf("failed to write lock file: %w", err)
	}

	lm.locks[lockPath] = file
	return nil
}

// ReleaseLock releases the lock at the specified path by closing the file handle
// and removing the lock file from the filesystem. Returns an error if no lock
// is held for the given path or if filesystem cleanup fails.
func (lm *LockManager) ReleaseLock(lockPath string) error {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	file, exists := lm.locks[lockPath]
	if !exists {
		return fmt.Errorf("no lock held for path: %s", lockPath)
	}

	file.Close()
	delete(lm.locks, lockPath)

	if err := os.Remove(lockPath); err != nil {
		return fmt.Errorf("failed to remove lock file: %w", err)
	}

	return nil
}

// IsLocked checks whether a lock is currently held for the specified path.
// This only checks the in-memory lock registry, not the actual filesystem state.
// Returns true if this LockManager instance holds a lock for the given path.
func (lm *LockManager) IsLocked(lockPath string) bool {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	_, exists := lm.locks[lockPath]
	return exists
}

// ListLocks returns a slice of all lock paths currently held by this LockManager.
// The returned slice contains the file paths of all active locks managed by this instance.
func (lm *LockManager) ListLocks() []string {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	var locks []string
	for path := range lm.locks {
		locks = append(locks, path)
	}

	return locks
}
