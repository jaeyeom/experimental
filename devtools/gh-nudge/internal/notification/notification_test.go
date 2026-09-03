package notification

import (
	"errors"
	"io/fs"
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
		if _, err := os.Stat(persistPath); errors.Is(err, fs.ErrNotExist) {
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

func TestShouldNotifyReviewerNewRequestCycle(t *testing.T) {
	originalTimeNow := timeNow
	defer func() { timeNow = originalTimeNow }()

	pingTime := time.Date(2026, 3, 1, 12, 0, 0, 0, time.UTC)
	timeNow = func() time.Time { return pingTime }

	tracker := NewTracker()
	prURL := "https://github.com/org/repo/pull/1"
	reviewer := "alice"

	if err := tracker.RecordNotification(prURL, reviewer); err != nil {
		t.Fatalf("Failed to record notification: %v", err)
	}

	tests := []struct {
		name           string
		reviewer       string
		latestReviewAt time.Time
		elapsed        time.Duration
		thresholdHours int
		want           bool
	}{
		{
			name:           "review after ping starts a new cycle within threshold",
			reviewer:       reviewer,
			latestReviewAt: pingTime.Add(2 * time.Hour),
			thresholdHours: 24,
			want:           true,
		},
		{
			name:           "no review within threshold does not notify",
			reviewer:       reviewer,
			thresholdHours: 24,
			want:           false,
		},
		{
			name:           "review before ping within threshold does not notify",
			reviewer:       reviewer,
			latestReviewAt: pingTime.Add(-time.Hour),
			thresholdHours: 24,
			want:           false,
		},
		{
			name:           "review at exact ping time does not start a new cycle",
			reviewer:       reviewer,
			latestReviewAt: pingTime,
			thresholdHours: 24,
			want:           false,
		},
		{
			name:           "no review after threshold still notifies",
			reviewer:       reviewer,
			elapsed:        25 * time.Hour,
			thresholdHours: 24,
			want:           true,
		},
		{
			name:           "review before ping after threshold still notifies",
			reviewer:       reviewer,
			latestReviewAt: pingTime.Add(-time.Hour),
			elapsed:        25 * time.Hour,
			thresholdHours: 24,
			want:           true,
		},
		{
			name:           "reviewer with no notification history is notified",
			reviewer:       "bob",
			latestReviewAt: pingTime.Add(2 * time.Hour),
			thresholdHours: 24,
			want:           true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			timeNow = func() time.Time { return pingTime.Add(tc.elapsed) }
			got := tracker.ShouldNotifyReviewer(prURL, tc.reviewer, tc.thresholdHours, tc.latestReviewAt)
			if got != tc.want {
				t.Errorf("ShouldNotifyReviewer() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRecordNotificationAtUsesGivenTime(t *testing.T) {
	originalTimeNow := timeNow
	defer func() { timeNow = originalTimeNow }()

	now := time.Date(2026, 3, 1, 15, 0, 0, 0, time.UTC)
	timeNow = func() time.Time { return now }

	tracker := NewTracker()
	prURL := "https://github.com/org/repo/pull/1"
	reviewer := "alice"
	recordedAt := time.Date(2026, 3, 1, 12, 0, 0, 0, time.UTC)

	if err := tracker.RecordNotificationAt(prURL, reviewer, recordedAt); err != nil {
		t.Fatalf("RecordNotificationAt() error = %v", err)
	}

	if tracker.ShouldNotifyReviewer(prURL, reviewer, 24, time.Time{}) {
		t.Error("Expected recorded timestamp to be within the threshold")
	}

	reviewBetween := recordedAt.Add(time.Hour)
	if !tracker.ShouldNotifyReviewer(prURL, reviewer, 24, reviewBetween) {
		t.Error("Expected a review after the recorded timestamp to start a new cycle")
	}
}

func TestShouldNotifyReviewerFirstPingIgnoresOldReview(t *testing.T) {
	tracker := NewTracker()
	prURL := "https://github.com/org/repo/pull/1"
	reviewer := "alice"
	oldReview := time.Date(2026, 2, 1, 12, 0, 0, 0, time.UTC)

	if !tracker.ShouldNotifyReviewer(prURL, reviewer, 24, oldReview) {
		t.Error("Expected to notify when no previous notification exists, even if the reviewer has an older review")
	}
}

func TestShouldNotifyReviewerCooldownAfterNewCyclePing(t *testing.T) {
	originalTimeNow := timeNow
	defer func() { timeNow = originalTimeNow }()

	firstPing := time.Date(2026, 3, 1, 12, 0, 0, 0, time.UTC)
	reviewAt := firstPing.Add(2 * time.Hour)
	secondPing := firstPing.Add(3 * time.Hour)

	timeNow = func() time.Time { return firstPing }
	tracker := NewTracker()
	prURL := "https://github.com/org/repo/pull/1"
	reviewer := "alice"

	if err := tracker.RecordNotification(prURL, reviewer); err != nil {
		t.Fatalf("Failed to record first notification: %v", err)
	}

	timeNow = func() time.Time { return secondPing }
	if !tracker.ShouldNotifyReviewer(prURL, reviewer, 24, reviewAt) {
		t.Fatal("Expected to notify after a review submitted after the last ping")
	}

	if err := tracker.RecordNotification(prURL, reviewer); err != nil {
		t.Fatalf("Failed to record second notification: %v", err)
	}

	if tracker.ShouldNotifyReviewer(prURL, reviewer, 24, reviewAt) {
		t.Error("Expected not to notify again within the threshold after pinging the new request cycle")
	}

	laterReview := secondPing.Add(time.Hour)
	if !tracker.ShouldNotifyReviewer(prURL, reviewer, 24, laterReview) {
		t.Error("Expected to notify again after a subsequent review starts another request cycle")
	}
}

func TestShouldNotifyReviewerPersistsAcrossLoad(t *testing.T) {
	tempDir := t.TempDir()
	persistPath := filepath.Join(tempDir, "notifications.json")

	originalTimeNow := timeNow
	defer func() { timeNow = originalTimeNow }()

	pingTime := time.Date(2026, 3, 1, 12, 0, 0, 0, time.UTC)
	timeNow = func() time.Time { return pingTime }

	tracker, err := NewPersistentTracker(persistPath)
	if err != nil {
		t.Fatalf("Failed to create persistent tracker: %v", err)
	}

	prURL := "https://github.com/org/repo/pull/1"
	reviewer := "alice"
	if err := tracker.RecordNotification(prURL, reviewer); err != nil {
		t.Fatalf("Failed to record notification: %v", err)
	}

	loaded, err := NewPersistentTracker(persistPath)
	if err != nil {
		t.Fatalf("Failed to reload persistent tracker: %v", err)
	}

	timeNow = func() time.Time { return pingTime.Add(3 * time.Hour) }
	if loaded.ShouldNotifyReviewer(prURL, reviewer, 24, time.Time{}) {
		t.Error("Expected loaded tracker not to notify within threshold with no later review")
	}
	if !loaded.ShouldNotifyReviewer(prURL, reviewer, 24, pingTime.Add(2*time.Hour)) {
		t.Error("Expected loaded tracker to notify when a review was submitted after the stored ping")
	}
}
