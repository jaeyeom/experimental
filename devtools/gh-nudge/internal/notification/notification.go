// Package notification provides functionality to track and manage notification history.
package notification

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sync"
	"time"
)

// For testing purposes, we can override this function
var timeNow = time.Now

// PRNotificationKey uniquely identifies a PR notification to a specific reviewer.
type PRNotificationKey struct {
	PRURL         string // URL of the PR
	ReviewerLogin string // GitHub login of the reviewer
}

// notificationRecord stores the time when a notification was sent.
type notificationRecord struct {
	Time time.Time `json:"time"`
}

// persistedData represents the structure of the data stored in the persistence file.
type persistedData struct {
	Notifications map[string]notificationRecord `json:"notifications"`
}

// Tracker keeps track of when notifications were last sent for PRs to specific reviewers.
type Tracker struct {
	notifications map[PRNotificationKey]time.Time
	mutex         sync.RWMutex
	persistPath   string
}

// NewTracker creates a new notification tracker.
func NewTracker() *Tracker {
	return &Tracker{
		notifications: make(map[PRNotificationKey]time.Time),
	}
}

// NewPersistentTracker creates a new notification tracker that persists its state to a file.
func NewPersistentTracker(persistPath string) (*Tracker, error) {
	tracker := &Tracker{
		notifications: make(map[PRNotificationKey]time.Time),
		persistPath:   persistPath,
	}

	// Create directory if it doesn't exist
	dir := filepath.Dir(persistPath)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, err
	}

	// Load existing data if available
	if err := tracker.Load(); err != nil && !os.IsNotExist(err) {
		return nil, err
	}

	return tracker, nil
}

// keyToString converts a PRNotificationKey to a string for JSON serialization.
func keyToString(key PRNotificationKey) string {
	return key.PRURL + "|" + key.ReviewerLogin
}

// stringToKey converts a string back to a PRNotificationKey.
func stringToKey(s string) PRNotificationKey {
	parts := []string{"", ""}
	for i, c := range s {
		if c == '|' {
			parts[1] = s[i+1:]
			break
		}
		parts[0] += string(c)
	}
	return PRNotificationKey{
		PRURL:         parts[0],
		ReviewerLogin: parts[1],
	}
}

// Load loads the notification state from the persistence file.
func (t *Tracker) Load() error {
	if t.persistPath == "" {
		return nil // Nothing to load for non-persistent trackers
	}

	data, err := os.ReadFile(t.persistPath)
	if err != nil {
		return err
	}

	var persisted persistedData
	if err := json.Unmarshal(data, &persisted); err != nil {
		return err
	}

	t.mutex.Lock()
	defer t.mutex.Unlock()

	for keyStr, record := range persisted.Notifications {
		key := stringToKey(keyStr)
		t.notifications[key] = record.Time
	}

	return nil
}

// Save saves the notification state to the persistence file.
func (t *Tracker) Save() error {
	if t.persistPath == "" {
		return nil // Nothing to save for non-persistent trackers
	}

	t.mutex.RLock()
	defer t.mutex.RUnlock()

	persisted := persistedData{
		Notifications: make(map[string]notificationRecord),
	}

	for key, time := range t.notifications {
		persisted.Notifications[keyToString(key)] = notificationRecord{Time: time}
	}

	data, err := json.MarshalIndent(persisted, "", "  ")
	if err != nil {
		return err
	}

	return os.WriteFile(t.persistPath, data, 0o600)
}

// ShouldNotify determines if a notification should be sent based on the threshold hours.
// Returns true if no previous notification exists or if the threshold hours have passed since the last notification.
func (t *Tracker) ShouldNotify(prURL, reviewerLogin string, thresholdHours int) bool {
	key := PRNotificationKey{
		PRURL:         prURL,
		ReviewerLogin: reviewerLogin,
	}

	t.mutex.RLock()
	lastNotified, exists := t.notifications[key]
	t.mutex.RUnlock()

	if !exists {
		return true
	}

	// Check if threshold hours have passed since the last notification
	thresholdDuration := time.Duration(thresholdHours) * time.Hour
	return timeNow().Sub(lastNotified) >= thresholdDuration
}

// RecordNotification records that a notification was sent for a PR to a specific reviewer.
func (t *Tracker) RecordNotification(prURL, reviewerLogin string) error {
	key := PRNotificationKey{
		PRURL:         prURL,
		ReviewerLogin: reviewerLogin,
	}

	t.mutex.Lock()
	t.notifications[key] = timeNow()
	t.mutex.Unlock()

	// Persist the updated state
	if t.persistPath != "" {
		return t.Save()
	}
	return nil
}
