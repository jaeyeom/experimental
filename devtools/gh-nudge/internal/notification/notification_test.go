package notification

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestShouldNotify(t *testing.T) {
	tracker := NewTracker()
	prURL := "https://github.com/org/repo/pull/1"
	reviewer := "github-user"

	t.Run("should notify when no previous notification exists", func(t *testing.T) {
		shouldNotify := tracker.ShouldNotify(prURL, reviewer, 24)
		if !shouldNotify {
			t.Error("Expected to notify when no previous notification exists")
		}
	})

	// Record a notification
	if err := tracker.RecordNotification(prURL, reviewer); err != nil {
		t.Fatalf("Failed to record notification: %v", err)
	}

	t.Run("should not notify within threshold period", func(t *testing.T) {
		shouldNotify := tracker.ShouldNotify(prURL, reviewer, 24)
		if shouldNotify {
			t.Error("Expected not to notify within threshold period")
		}
	})

	t.Run("should notify for different PR", func(t *testing.T) {
		differentPRURL := "https://github.com/org/repo/pull/2"
		shouldNotify := tracker.ShouldNotify(differentPRURL, reviewer, 24)
		if !shouldNotify {
			t.Error("Expected to notify for different PR")
		}
	})

	t.Run("should notify for different reviewer", func(t *testing.T) {
		differentReviewer := "github-user2"
		shouldNotify := tracker.ShouldNotify(prURL, differentReviewer, 24)
		if !shouldNotify {
			t.Error("Expected to notify for different reviewer")
		}
	})
}

func TestShouldNotifyAfterThreshold(t *testing.T) {
	// Create a custom time function for testing
	originalTimeNow := timeNow
	defer func() { timeNow = originalTimeNow }()

	mockTime := time.Now()
	timeNow = func() time.Time {
		return mockTime
	}

	tracker := NewTracker()
	prURL := "https://github.com/org/repo/pull/1"
	reviewer := "github-user"

	// Record a notification at the current mock time
	if err := tracker.RecordNotification(prURL, reviewer); err != nil {
		t.Fatalf("Failed to record notification: %v", err)
	}

	// Advance time by 23 hours (less than threshold)
	mockTime = mockTime.Add(23 * time.Hour)

	t.Run("should not notify before threshold period", func(t *testing.T) {
		shouldNotify := tracker.ShouldNotify(prURL, reviewer, 24)
		if shouldNotify {
			t.Error("Expected not to notify before threshold period")
		}
	})

	// Advance time by 2 more hours (total 25 hours, more than threshold)
	mockTime = mockTime.Add(2 * time.Hour)

	t.Run("should notify after threshold period", func(t *testing.T) {
		shouldNotify := tracker.ShouldNotify(prURL, reviewer, 24)
		if !shouldNotify {
			t.Error("Expected to notify after threshold period")
		}
	})
}

func TestPersistence(t *testing.T) {
	// Create a temporary directory for the test
	tempDir, err := os.MkdirTemp("", "notification-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	persistPath := filepath.Join(tempDir, "notifications.json")

	// Create a custom time function for testing
	originalTimeNow := timeNow
	defer func() { timeNow = originalTimeNow }()

	mockTime := time.Date(2025, 3, 19, 12, 0, 0, 0, time.UTC)
	timeNow = func() time.Time {
		return mockTime
	}

	// Test case 1: Create a new tracker, save some notifications
	t.Run("should save notifications to file", func(t *testing.T) {
		tracker, err := NewPersistentTracker(persistPath)
		if err != nil {
			t.Fatalf("Failed to create persistent tracker: %v", err)
		}

		prURL := "https://github.com/org/repo/pull/1"
		reviewer := "github-user"

		if err := tracker.RecordNotification(prURL, reviewer); err != nil {
			t.Fatalf("Failed to record notification: %v", err)
		}

		// Verify the file exists
		if _, err := os.Stat(persistPath); os.IsNotExist(err) {
			t.Error("Expected notification file to exist")
		}
	})

	// Test case 2: Create a new tracker, load notifications from file
	t.Run("should load notifications from file", func(t *testing.T) {
		tracker, err := NewPersistentTracker(persistPath)
		if err != nil {
			t.Fatalf("Failed to create persistent tracker: %v", err)
		}

		prURL := "https://github.com/org/repo/pull/1"
		reviewer := "github-user"

		// Should not notify because we loaded the previous state
		shouldNotify := tracker.ShouldNotify(prURL, reviewer, 24)
		if shouldNotify {
			t.Error("Expected not to notify after loading from file")
		}

		// Should notify for a different PR
		differentPRURL := "https://github.com/org/repo/pull/2"
		shouldNotify = tracker.ShouldNotify(differentPRURL, reviewer, 24)
		if !shouldNotify {
			t.Error("Expected to notify for different PR after loading from file")
		}
	})

	// Test case 3: Advance time beyond threshold and check
	t.Run("should respect threshold after loading from file", func(t *testing.T) {
		// Advance time by 25 hours (beyond threshold)
		mockTime = mockTime.Add(25 * time.Hour)

		tracker, err := NewPersistentTracker(persistPath)
		if err != nil {
			t.Fatalf("Failed to create persistent tracker: %v", err)
		}

		prURL := "https://github.com/org/repo/pull/1"
		reviewer := "github-user"

		// Should notify because threshold has passed
		shouldNotify := tracker.ShouldNotify(prURL, reviewer, 24)
		if !shouldNotify {
			t.Error("Expected to notify after threshold period when loading from file")
		}
	})
}
