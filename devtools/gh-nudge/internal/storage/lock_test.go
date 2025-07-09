package storage

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestLockManager_NewLockManager(t *testing.T) {
	lm := NewLockManager()

	if lm.locks == nil {
		t.Error("Expected locks map to be initialized")
	}

	if len(lm.locks) != 0 {
		t.Error("Expected locks map to be empty initially")
	}
}

func TestLockManager_AcquireLock(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lock-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lm := NewLockManager()
	lockPath := filepath.Join(tempDir, "test.lock")

	// Test acquiring a lock
	err = lm.AcquireLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to acquire lock: %v", err)
	}

	// Verify lock file was created
	if _, err := os.Stat(lockPath); errors.Is(err, os.ErrNotExist) {
		t.Error("Expected lock file to be created")
	}

	// Verify lock is tracked internally
	if !lm.IsLocked(lockPath) {
		t.Error("Expected lock to be tracked internally")
	}

	// Test that acquiring the same lock again fails
	err = lm.AcquireLock(lockPath)
	if err == nil {
		t.Error("Expected error when acquiring already held lock")
	}
}

func TestLockManager_ReleaseLock(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lock-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lm := NewLockManager()
	lockPath := filepath.Join(tempDir, "test.lock")

	// First acquire a lock
	err = lm.AcquireLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to acquire lock: %v", err)
	}

	// Test releasing the lock
	err = lm.ReleaseLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to release lock: %v", err)
	}

	// Verify lock file was removed
	if _, err := os.Stat(lockPath); !errors.Is(err, os.ErrNotExist) {
		t.Error("Expected lock file to be removed")
	}

	// Verify lock is no longer tracked internally
	if lm.IsLocked(lockPath) {
		t.Error("Expected lock to no longer be tracked internally")
	}

	// Test that releasing a non-existent lock fails
	err = lm.ReleaseLock(lockPath)
	if err == nil {
		t.Error("Expected error when releasing non-existent lock")
	}
}

func TestLockManager_IsLocked(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lock-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lm := NewLockManager()
	lockPath := filepath.Join(tempDir, "test.lock")

	// Initially should not be locked
	if lm.IsLocked(lockPath) {
		t.Error("Expected lock to not be held initially")
	}

	// Acquire lock
	err = lm.AcquireLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to acquire lock: %v", err)
	}

	// Should now be locked
	if !lm.IsLocked(lockPath) {
		t.Error("Expected lock to be held after acquiring")
	}

	// Release lock
	err = lm.ReleaseLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to release lock: %v", err)
	}

	// Should no longer be locked
	if lm.IsLocked(lockPath) {
		t.Error("Expected lock to not be held after releasing")
	}
}

func TestLockManager_ListLocks(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lock-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lm := NewLockManager()

	// Initially should have no locks
	locks := lm.ListLocks()
	if len(locks) != 0 {
		t.Error("Expected no locks initially")
	}

	// Acquire multiple locks
	lockPaths := []string{
		filepath.Join(tempDir, "lock1.lock"),
		filepath.Join(tempDir, "lock2.lock"),
		filepath.Join(tempDir, "lock3.lock"),
	}

	for _, lockPath := range lockPaths {
		err = lm.AcquireLock(lockPath)
		if err != nil {
			t.Fatalf("Failed to acquire lock %s: %v", lockPath, err)
		}
	}

	// Should now have all locks
	locks = lm.ListLocks()
	if len(locks) != len(lockPaths) {
		t.Errorf("Expected %d locks, got %d", len(lockPaths), len(locks))
	}

	// Verify all lock paths are in the list
	for _, expectedPath := range lockPaths {
		found := false
		for _, actualPath := range locks {
			if actualPath == expectedPath {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected lock path %s not found in list", expectedPath)
		}
	}

	// Release one lock
	err = lm.ReleaseLock(lockPaths[0])
	if err != nil {
		t.Fatalf("Failed to release lock: %v", err)
	}

	// Should now have one fewer lock
	locks = lm.ListLocks()
	if len(locks) != len(lockPaths)-1 {
		t.Errorf("Expected %d locks after release, got %d", len(lockPaths)-1, len(locks))
	}
}

func TestLockManager_AcquireLockFileCreation(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lock-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lm := NewLockManager()
	lockPath := filepath.Join(tempDir, "test.lock")

	// Acquire lock
	err = lm.AcquireLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to acquire lock: %v", err)
	}

	// Verify lock file contains expected content
	content, err := os.ReadFile(lockPath)
	if err != nil {
		t.Fatalf("Failed to read lock file: %v", err)
	}

	contentStr := string(content)
	if !containsString(contentStr, "locked_at:") {
		t.Error("Expected lock file to contain 'locked_at:' timestamp")
	}

	// Verify timestamp is valid
	lines := splitLines(contentStr)
	if len(lines) == 0 {
		t.Error("Expected lock file to contain at least one line")
	}

	timestampLine := lines[0]
	if len(timestampLine) < 20 { // "locked_at: " + RFC3339 timestamp
		t.Error("Expected timestamp line to be longer")
	}
}

func TestLockManager_ConcurrentAccess(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lock-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lm := NewLockManager()

	// Test concurrent access to different locks
	lockPath1 := filepath.Join(tempDir, "lock1.lock")
	lockPath2 := filepath.Join(tempDir, "lock2.lock")

	done := make(chan error, 2)

	// Goroutine 1: acquire lock1
	go func() {
		err := lm.AcquireLock(lockPath1)
		if err != nil {
			done <- err
			return
		}
		time.Sleep(100 * time.Millisecond)
		err = lm.ReleaseLock(lockPath1)
		done <- err
	}()

	// Goroutine 2: acquire lock2
	go func() {
		err := lm.AcquireLock(lockPath2)
		if err != nil {
			done <- err
			return
		}
		time.Sleep(100 * time.Millisecond)
		err = lm.ReleaseLock(lockPath2)
		done <- err
	}()

	// Wait for both goroutines to complete
	for i := 0; i < 2; i++ {
		if err := <-done; err != nil {
			t.Errorf("Concurrent access failed: %v", err)
		}
	}
}

func TestLockManager_ExclusiveLock(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lock-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lm := NewLockManager()
	lockPath := filepath.Join(tempDir, "exclusive.lock")

	// Acquire lock in first goroutine
	err = lm.AcquireLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to acquire lock: %v", err)
	}

	// Try to acquire same lock in second goroutine (should fail)
	err = lm.AcquireLock(lockPath)
	if err == nil {
		t.Error("Expected error when trying to acquire already held lock")
	}

	// Release the lock
	err = lm.ReleaseLock(lockPath)
	if err != nil {
		t.Fatalf("Failed to release lock: %v", err)
	}

	// Now should be able to acquire the lock again
	err = lm.AcquireLock(lockPath)
	if err != nil {
		t.Errorf("Failed to acquire lock after release: %v", err)
	}
}

// Helper functions

func containsString(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || (len(s) > len(substr) &&
		(s[:len(substr)] == substr || s[len(s)-len(substr):] == substr ||
			findSubstring(s, substr))))
}

func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

func splitLines(s string) []string {
	var lines []string
	var currentLine string

	for _, char := range s {
		if char == '\n' {
			lines = append(lines, currentLine)
			currentLine = ""
		} else {
			currentLine += string(char)
		}
	}

	if currentLine != "" {
		lines = append(lines, currentLine)
	}

	return lines
}
