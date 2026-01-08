package storage

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"golang.org/x/sys/unix"
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
// If the lock file exists but the process that created it is no longer running (stale lock),
// it will attempt to remove the stale lock and retry once.
// Returns an error if the lock is already held by this manager or if filesystem operations fail.
func (lm *LockManager) AcquireLock(lockPath string) error {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	if _, exists := lm.locks[lockPath]; exists {
		return fmt.Errorf("lock already held for path: %s", lockPath)
	}

	// Try to acquire lock normally first
	if file, err := lm.tryAcquireLock(lockPath); err == nil {
		lm.locks[lockPath] = file
		return nil
	}

	// If lock file exists, check if it's stale and try to clean it up
	if lm.isStalelock(lockPath) {
		// Try to remove the stale lock file. If removal fails, someone else
		// might have cleaned it up or we don't have permissions.
		// We still attempt to acquire the lock afterward since the file might be gone.
		_ = os.Remove(lockPath)

		// Retry once after stale lock cleanup attempt
		if file, err := lm.tryAcquireLock(lockPath); err == nil {
			lm.locks[lockPath] = file
			return nil
		}
	}

	return fmt.Errorf("failed to acquire lock: file exists and is not stale")
}

// tryAcquireLock attempts to create and acquire a lock file.
func (lm *LockManager) tryAcquireLock(lockPath string) (*os.File, error) {
	// Use O_EXCL flag for atomic exclusive lock creation - fails if file already exists
	file, err := os.OpenFile(lockPath, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o644)
	if err != nil {
		return nil, fmt.Errorf("failed to create lock file: %w", err)
	}

	// Write lock metadata including PID
	lockInfo := fmt.Sprintf("locked_at: %s\npid: %d\n",
		time.Now().Format(time.RFC3339), os.Getpid())

	if _, err := fmt.Fprintf(file, "%s", lockInfo); err != nil {
		// Clean up on write failure: close file handle and remove lock file
		file.Close()
		os.Remove(lockPath)
		return nil, fmt.Errorf("failed to write lock file: %w", err)
	}

	return file, nil
}

// isStalelock checks if a lock file represents a stale lock (process no longer exists).
func (lm *LockManager) isStalelock(lockPath string) bool {
	content, err := os.ReadFile(lockPath)
	if err != nil {
		return false // Can't read file, assume it's not stale
	}

	// Parse PID from lock file content
	lines := strings.Split(string(content), "\n")
	var pid int
	for _, line := range lines {
		if strings.HasPrefix(line, "pid: ") {
			if parsedPID, err := strconv.Atoi(strings.TrimPrefix(line, "pid: ")); err == nil {
				pid = parsedPID
				break
			}
		}
	}

	if pid == 0 {
		return false // No PID found, assume it's not stale for safety
	}

	return !isProcessRunning(pid)
}

// isProcessRunning checks if a process with the given PID is currently running.
// It uses the kill(pid, 0) technique which doesn't actually send a signal but
// checks if the process exists and we have permission to send it a signal.
func isProcessRunning(pid int) bool {
	// ESRCH means the process doesn't exist.
	// No error or other errors (like EPERM) mean process exists.
	return unix.Kill(pid, 0) != unix.ESRCH
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
