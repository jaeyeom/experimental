package worker

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

const pauseFilename = "paused"

// Pause writes the local pause marker file under stateDir so the worker stops
// claiming new leases while allowing in-flight jobs to finish.
func Pause(stateDir, reason string) error {
	if err := os.MkdirAll(stateDir, 0o700); err != nil {
		return fmt.Errorf("create state dir %q: %w", stateDir, err)
	}
	trimmed := strings.TrimSpace(reason)
	if trimmed == "" {
		trimmed = "paused"
	}
	path := filepath.Join(stateDir, pauseFilename)
	if err := os.WriteFile(path, []byte(trimmed+"\n"), 0o600); err != nil {
		return fmt.Errorf("write pause marker %q: %w", path, err)
	}
	return nil
}

// Resume removes the local pause marker file under stateDir. It succeeds if
// the worker is not currently paused.
func Resume(stateDir string) error {
	path := filepath.Join(stateDir, pauseFilename)
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("remove pause marker %q: %w", path, err)
	}
	return nil
}

// IsPaused reports whether the local pause marker file exists under stateDir
// along with the recorded pause reason.
func IsPaused(stateDir string) (bool, string) {
	if stateDir == "" {
		return false, ""
	}
	path := filepath.Join(stateDir, pauseFilename)
	raw, err := os.ReadFile(path)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return false, ""
		}
		return true, "pause marker unreadable"
	}
	reason := strings.TrimSpace(string(raw))
	if reason == "" {
		reason = "paused"
	}
	return true, reason
}
