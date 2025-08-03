// Package testutil provides test utilities for the todo application.
package testutil

import (
	"fmt"
	"os"
	"testing"
)

// WithTempHome creates a temporary directory, sets it as HOME environment variable,
// and returns a cleanup function. This is useful for tests that need a temporary
// HOME directory.
func WithTempHome(t *testing.T) (cleanup func()) {
	t.Helper()

	origHome := os.Getenv("HOME")
	tempHome, err := os.MkdirTemp("", "test-home")
	if err != nil {
		t.Fatalf("Error creating temp home dir: %v", err)
	}

	os.Setenv("HOME", tempHome)

	return func() {
		os.Setenv("HOME", origHome)
		os.RemoveAll(tempHome)
	}
}

// WithTempHomeForExample creates a temporary directory and sets it as HOME
// environment variable for use in Example tests. It returns the cleanup function
// and any error that occurred during setup.
func WithTempHomeForExample() (cleanup func(), err error) {
	origHome := os.Getenv("HOME")
	tempHome, err := os.MkdirTemp("", "test-home")
	if err != nil {
		return nil, fmt.Errorf("failed to create temp home directory: %w", err)
	}

	os.Setenv("HOME", tempHome)

	return func() {
		os.Setenv("HOME", origHome)
		os.RemoveAll(tempHome)
	}, nil
}
