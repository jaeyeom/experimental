package storage

import (
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

func TestFileLockManagerWithLockRetry(t *testing.T) {
	tempDir := t.TempDir()
	store, err := NewFileSystemStore(tempDir)
	if err != nil {
		t.Fatalf("Failed to create store: %v", err)
	}

	manager := NewFileLockManager(store)

	t.Run("single lock acquisition succeeds", func(t *testing.T) {
		key := "test_single_lock"
		executed := false
		config := FileLockConfig{
			MaxRetries:    5,
			InitialDelay:  10 * time.Millisecond,
			MaxDelay:      100 * time.Millisecond,
			BackoffFactor: 2.0,
		}

		err := manager.WithLockRetry(key, config, func() error {
			executed = true
			return nil
		})
		if err != nil {
			t.Fatalf("Failed to acquire lock: %v", err)
		}
		if !executed {
			t.Fatal("Function was not executed")
		}
	})

	t.Run("concurrent locks with retry succeed", func(t *testing.T) {
		key := "test_concurrent_retry"
		var successCount int32
		var wg sync.WaitGroup
		config := FileLockConfig{
			MaxRetries:    10,
			InitialDelay:  5 * time.Millisecond,
			MaxDelay:      50 * time.Millisecond,
			BackoffFactor: 1.5,
		}

		// Start 5 concurrent goroutines trying to acquire the same lock
		for i := 0; i < 5; i++ {
			wg.Add(1)
			go func(id int) {
				defer wg.Done()
				err := manager.WithLockRetry(key, config, func() error {
					// Simulate some work
					time.Sleep(10 * time.Millisecond)
					atomic.AddInt32(&successCount, 1)
					return nil
				})
				if err != nil {
					t.Errorf("Goroutine %d failed to acquire lock: %v", id, err)
				}
			}(i)
		}

		wg.Wait()

		if successCount != 5 {
			t.Fatalf("Expected 5 successful operations, got %d", successCount)
		}
	})

	t.Run("lock retry timeout after max attempts", func(t *testing.T) {
		key := "test_timeout"
		lockPath := filepath.Join(tempDir, key+".lock")

		// Create a lock file manually to simulate lock being held
		if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
			t.Fatalf("Failed to create directory: %v", err)
		}
		file, err := os.Create(lockPath)
		if err != nil {
			t.Fatalf("Failed to create lock file: %v", err)
		}
		file.Close()
		defer os.Remove(lockPath)

		config := FileLockConfig{
			MaxRetries:    3,
			InitialDelay:  5 * time.Millisecond,
			MaxDelay:      20 * time.Millisecond,
			BackoffFactor: 2.0,
		}

		err = manager.WithLockRetry(key, config, func() error {
			t.Fatal("Function should not be executed")
			return nil
		})

		if err == nil {
			t.Fatal("Expected error due to lock timeout, got nil")
		}
		expectedError := fmt.Sprintf("failed to acquire lock after %d attempts", config.MaxRetries)
		if !containsSubstring(err.Error(), expectedError) {
			t.Fatalf("Expected error containing '%s', got: %v", expectedError, err)
		}
	})

	t.Run("backoff delay increases correctly", func(t *testing.T) {
		key := "test_backoff"
		lockPath := filepath.Join(tempDir, key+".lock")

		// Create a lock file
		file, err := os.Create(lockPath)
		if err != nil {
			t.Fatalf("Failed to create lock file: %v", err)
		}
		file.Close()

		config := FileLockConfig{
			MaxRetries:    5,
			InitialDelay:  10 * time.Millisecond,
			MaxDelay:      100 * time.Millisecond,
			BackoffFactor: 2.0,
		}

		// Track the time when the lock is removed
		var lockRemoved bool
		var mu sync.Mutex

		go func() {
			// Remove the lock after a fixed time to allow measuring backoff
			time.Sleep(35 * time.Millisecond)
			mu.Lock()
			os.Remove(lockPath)
			lockRemoved = true
			mu.Unlock()
		}()

		startTime := time.Now()
		err = manager.WithLockRetry(key, config, func() error {
			return nil
		})
		elapsed := time.Since(startTime)

		mu.Lock()
		wasRemoved := lockRemoved
		mu.Unlock()

		if err != nil {
			t.Fatalf("Failed to acquire lock: %v", err)
		}

		if !wasRemoved {
			t.Fatal("Lock was not removed by goroutine")
		}

		// With initial delay 10ms, backoff 2.0, we expect:
		// Attempt 1: immediate fail
		// Attempt 2: wait 10ms
		// Attempt 3: wait 20ms
		// Attempt 4: wait 40ms (but lock removed at 35ms, so succeeds)
		// Total wait time should be at least 30ms (allowing for some timing variance)
		if elapsed < 30*time.Millisecond {
			t.Fatalf("Expected at least 30ms elapsed, got %v", elapsed)
		}
		// Upper bound is more flexible due to system scheduling
		if elapsed > 100*time.Millisecond {
			t.Fatalf("Expected less than 100ms elapsed, got %v", elapsed)
		}
	})
}

func TestFileLockManagerConcurrentCommentAddition(t *testing.T) {
	tempDir := t.TempDir()
	store, err := NewFileSystemStore(tempDir)
	if err != nil {
		t.Fatalf("Failed to create store: %v", err)
	}

	manager := NewFileLockManager(store)
	config := FileLockConfig{
		MaxRetries:    20,
		InitialDelay:  10 * time.Millisecond,
		MaxDelay:      100 * time.Millisecond,
		BackoffFactor: 1.2,
	}

	// Simulate multiple LLM instances adding comments concurrently
	t.Run("parallel comment addition", func(t *testing.T) {
		commentsFile := "comments.json"
		var comments []string
		var mu sync.Mutex
		var wg sync.WaitGroup

		// 10 concurrent writers
		for i := 0; i < 10; i++ {
			wg.Add(1)
			go func(id int) {
				defer wg.Done()
				comment := fmt.Sprintf("Comment from LLM instance %d", id)

				err := manager.WithLockRetry(commentsFile, config, func() error {
					// Read existing comments
					var existingComments []string
					if store.Exists(commentsFile) {
						if err := store.Get(commentsFile, &existingComments); err != nil {
							return err
						}
					}

					// Add new comment
					existingComments = append(existingComments, comment)

					// Write back
					return store.Set(commentsFile, existingComments)
				})

				if err != nil {
					t.Errorf("LLM instance %d failed to add comment: %v", id, err)
				} else {
					mu.Lock()
					comments = append(comments, comment)
					mu.Unlock()
				}
			}(i)
		}

		wg.Wait()

		// Verify all comments were added
		var finalComments []string
		if err := store.Get(commentsFile, &finalComments); err != nil {
			t.Fatalf("Failed to read final comments: %v", err)
		}

		if len(finalComments) != 10 {
			t.Fatalf("Expected 10 comments, got %d", len(finalComments))
		}

		// Verify no duplicate or missing comments
		commentMap := make(map[string]bool)
		for _, c := range finalComments {
			if commentMap[c] {
				t.Fatalf("Duplicate comment found: %s", c)
			}
			commentMap[c] = true
		}
	})
}

func TestDefaultFileLockConfig(t *testing.T) {
	config := DefaultFileLockConfig()

	if config.MaxRetries != 10 {
		t.Errorf("Expected MaxRetries to be 10, got %d", config.MaxRetries)
	}
	if config.InitialDelay != 50*time.Millisecond {
		t.Errorf("Expected InitialDelay to be 50ms, got %v", config.InitialDelay)
	}
	if config.MaxDelay != 2*time.Second {
		t.Errorf("Expected MaxDelay to be 2s, got %v", config.MaxDelay)
	}
	if config.BackoffFactor != 1.5 {
		t.Errorf("Expected BackoffFactor to be 1.5, got %f", config.BackoffFactor)
	}
}

func TestGetFileLockConfig(t *testing.T) {
	// Save original env vars
	origMaxRetries := os.Getenv("GH_STORAGE_LOCK_MAX_RETRIES")
	origInitialDelay := os.Getenv("GH_STORAGE_LOCK_INITIAL_DELAY")
	origMaxDelay := os.Getenv("GH_STORAGE_LOCK_MAX_DELAY")
	origBackoffFactor := os.Getenv("GH_STORAGE_LOCK_BACKOFF_FACTOR")

	defer func() {
		// Restore original env vars
		os.Setenv("GH_STORAGE_LOCK_MAX_RETRIES", origMaxRetries)
		os.Setenv("GH_STORAGE_LOCK_INITIAL_DELAY", origInitialDelay)
		os.Setenv("GH_STORAGE_LOCK_MAX_DELAY", origMaxDelay)
		os.Setenv("GH_STORAGE_LOCK_BACKOFF_FACTOR", origBackoffFactor)
	}()

	t.Run("default configuration", func(t *testing.T) {
		// Clear all env vars
		os.Unsetenv("GH_STORAGE_LOCK_MAX_RETRIES")
		os.Unsetenv("GH_STORAGE_LOCK_INITIAL_DELAY")
		os.Unsetenv("GH_STORAGE_LOCK_MAX_DELAY")
		os.Unsetenv("GH_STORAGE_LOCK_BACKOFF_FACTOR")

		config := GetFileLockConfig()

		if config.MaxRetries != 10 {
			t.Errorf("Expected default MaxRetries to be 10, got %d", config.MaxRetries)
		}
		if config.InitialDelay != 50*time.Millisecond {
			t.Errorf("Expected default InitialDelay to be 50ms, got %v", config.InitialDelay)
		}
		if config.MaxDelay != 2*time.Second {
			t.Errorf("Expected default MaxDelay to be 2s, got %v", config.MaxDelay)
		}
		if config.BackoffFactor != 1.5 {
			t.Errorf("Expected default BackoffFactor to be 1.5, got %f", config.BackoffFactor)
		}
	})

	t.Run("environment variable configuration", func(t *testing.T) {
		os.Setenv("GH_STORAGE_LOCK_MAX_RETRIES", "20")
		os.Setenv("GH_STORAGE_LOCK_INITIAL_DELAY", "100ms")
		os.Setenv("GH_STORAGE_LOCK_MAX_DELAY", "5s")
		os.Setenv("GH_STORAGE_LOCK_BACKOFF_FACTOR", "1.2")

		config := GetFileLockConfig()

		if config.MaxRetries != 20 {
			t.Errorf("Expected MaxRetries to be 20, got %d", config.MaxRetries)
		}
		if config.InitialDelay != 100*time.Millisecond {
			t.Errorf("Expected InitialDelay to be 100ms, got %v", config.InitialDelay)
		}
		if config.MaxDelay != 5*time.Second {
			t.Errorf("Expected MaxDelay to be 5s, got %v", config.MaxDelay)
		}
		if config.BackoffFactor != 1.2 {
			t.Errorf("Expected BackoffFactor to be 1.2, got %f", config.BackoffFactor)
		}
	})

	t.Run("invalid environment variables use defaults", func(t *testing.T) {
		os.Setenv("GH_STORAGE_LOCK_MAX_RETRIES", "invalid")
		os.Setenv("GH_STORAGE_LOCK_INITIAL_DELAY", "not-a-duration")
		os.Setenv("GH_STORAGE_LOCK_MAX_DELAY", "xyz")
		os.Setenv("GH_STORAGE_LOCK_BACKOFF_FACTOR", "not-a-float")

		config := GetFileLockConfig()

		// Should fall back to defaults
		if config.MaxRetries != 10 {
			t.Errorf("Expected default MaxRetries to be 10, got %d", config.MaxRetries)
		}
		if config.InitialDelay != 50*time.Millisecond {
			t.Errorf("Expected default InitialDelay to be 50ms, got %v", config.InitialDelay)
		}
		if config.MaxDelay != 2*time.Second {
			t.Errorf("Expected default MaxDelay to be 2s, got %v", config.MaxDelay)
		}
		if config.BackoffFactor != 1.5 {
			t.Errorf("Expected default BackoffFactor to be 1.5, got %f", config.BackoffFactor)
		}
	})

	t.Run("negative or zero values use defaults", func(t *testing.T) {
		os.Setenv("GH_STORAGE_LOCK_MAX_RETRIES", "-1")
		os.Setenv("GH_STORAGE_LOCK_INITIAL_DELAY", "0s")
		os.Setenv("GH_STORAGE_LOCK_MAX_DELAY", "-1s")
		os.Setenv("GH_STORAGE_LOCK_BACKOFF_FACTOR", "0.5")

		config := GetFileLockConfig()

		// Should fall back to defaults for invalid values
		if config.MaxRetries != 10 {
			t.Errorf("Expected default MaxRetries to be 10, got %d", config.MaxRetries)
		}
		if config.InitialDelay != 50*time.Millisecond {
			t.Errorf("Expected default InitialDelay to be 50ms, got %v", config.InitialDelay)
		}
		if config.MaxDelay != 2*time.Second {
			t.Errorf("Expected default MaxDelay to be 2s, got %v", config.MaxDelay)
		}
		if config.BackoffFactor != 1.5 {
			t.Errorf("Expected default BackoffFactor to be 1.5, got %f", config.BackoffFactor)
		}
	})
}

// Helper function.
func containsSubstring(s, substr string) bool {
	return len(s) >= len(substr) && s[:len(substr)] == substr || len(s) > len(substr) && containsSubstring(s[1:], substr)
}
