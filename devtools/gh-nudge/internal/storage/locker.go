package storage

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"time"
)

// FileLockConfig contains configuration for retry behavior when acquiring locks.
type FileLockConfig struct {
	MaxRetries    int           // Maximum number of retry attempts
	InitialDelay  time.Duration // Initial delay between retries
	MaxDelay      time.Duration // Maximum delay between retries
	BackoffFactor float64       // Multiplier for exponential backoff
}

// DefaultFileLockConfig returns the default configuration for file lock retries.
func DefaultFileLockConfig() FileLockConfig {
	return FileLockConfig{
		MaxRetries:    10,
		InitialDelay:  50 * time.Millisecond,
		MaxDelay:      2 * time.Second,
		BackoffFactor: 1.5,
	}
}

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

// WithLockRetry attempts to acquire a lock with retry logic using exponential backoff.
// It will retry up to config.MaxRetries times with increasing delays between attempts.
// This is useful for operations where transient lock contention is expected.
func (flm *FileLockManager) WithLockRetry(key string, config FileLockConfig, fn func() error) error {
	lockPath := flm.getFilePath(key + ".lock")

	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory for lock file: %w", err)
	}

	var lastErr error
	delay := config.InitialDelay

	for attempt := 0; attempt <= config.MaxRetries; attempt++ {
		err := flm.lockManager.AcquireLock(lockPath)
		if err == nil {
			// Lock acquired successfully, execute function
			defer func() {
				if releaseErr := flm.lockManager.ReleaseLock(lockPath); releaseErr != nil {
					fmt.Fprintf(os.Stderr, "Warning: failed to release lock %s: %v\n", lockPath, releaseErr)
				}
			}()
			return fn()
		}

		lastErr = err
		if attempt < config.MaxRetries {
			// Wait before retrying with exponential backoff
			time.Sleep(delay)
			delay = time.Duration(float64(delay) * config.BackoffFactor)
			if delay > config.MaxDelay {
				delay = config.MaxDelay
			}
		}
	}

	return fmt.Errorf("failed to acquire lock after %d attempts: %w", config.MaxRetries, lastErr)
}

// parseIntEnv parses an integer environment variable with validation.
func parseIntEnv(envVar string, minValue int) (int, bool) {
	if value := os.Getenv(envVar); value != "" {
		if parsed, err := strconv.Atoi(value); err == nil && parsed >= minValue {
			return parsed, true
		}
	}
	return 0, false
}

// parseDurationEnv parses a duration environment variable with validation.
func parseDurationEnv(envVar string) (time.Duration, bool) {
	if value := os.Getenv(envVar); value != "" {
		if parsed, err := time.ParseDuration(value); err == nil && parsed > 0 {
			return parsed, true
		}
	}
	return 0, false
}

// parseFloatEnv parses a float environment variable with validation.
func parseFloatEnv(envVar string, minValue float64) (float64, bool) {
	if value := os.Getenv(envVar); value != "" {
		if parsed, err := strconv.ParseFloat(value, 64); err == nil && parsed > minValue {
			return parsed, true
		}
	}
	return 0, false
}

// GetFileLockConfig returns the file lock configuration based on environment variables.
// If environment variables are not set, returns the default configuration.
func GetFileLockConfig() FileLockConfig {
	config := DefaultFileLockConfig()

	if maxRetries, ok := parseIntEnv("GH_STORAGE_LOCK_MAX_RETRIES", 0); ok {
		config.MaxRetries = maxRetries
	}

	if initialDelay, ok := parseDurationEnv("GH_STORAGE_LOCK_INITIAL_DELAY"); ok {
		config.InitialDelay = initialDelay
	}

	if maxDelay, ok := parseDurationEnv("GH_STORAGE_LOCK_MAX_DELAY"); ok {
		config.MaxDelay = maxDelay
	}

	if backoffFactor, ok := parseFloatEnv("GH_STORAGE_LOCK_BACKOFF_FACTOR", 1.0); ok {
		config.BackoffFactor = backoffFactor
	}

	return config
}
