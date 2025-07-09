package storage

import (
	"fmt"
	"os"
	"sync"
	"time"
)

type LockManager struct {
	locks map[string]*os.File
	mutex sync.RWMutex
}

func NewLockManager() *LockManager {
	return &LockManager{
		locks: make(map[string]*os.File),
	}
}

func (lm *LockManager) AcquireLock(lockPath string) error {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	if _, exists := lm.locks[lockPath]; exists {
		return fmt.Errorf("lock already held for path: %s", lockPath)
	}

	file, err := os.OpenFile(lockPath, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("failed to acquire lock: %w", err)
	}

	if _, err := fmt.Fprintf(file, "locked_at: %s\n", time.Now().Format(time.RFC3339)); err != nil {
		file.Close()
		os.Remove(lockPath)
		return fmt.Errorf("failed to write lock file: %w", err)
	}

	lm.locks[lockPath] = file
	return nil
}

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

func (lm *LockManager) IsLocked(lockPath string) bool {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	_, exists := lm.locks[lockPath]
	return exists
}

func (lm *LockManager) ListLocks() []string {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	var locks []string
	for path := range lm.locks {
		locks = append(locks, path)
	}

	return locks
}
