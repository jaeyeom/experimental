// Package ghtest provides test utilities for GitHub CLI testing.
package ghtest

import (
	"context"
	"os/exec"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/executor"
	"github.com/jaeyeom/experimental/devtools/pkg/ghauth"
)

// SkipWithoutGH skips the test if the gh CLI is not available in PATH.
func SkipWithoutGH(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("gh"); err != nil {
		t.Skip("gh CLI not available, skipping test")
	}
}

// SkipWithoutGHAuth skips the test if the gh CLI is not authenticated.
func SkipWithoutGHAuth(t *testing.T) {
	t.Helper()
	SkipWithoutGH(t)

	exec := executor.NewBasicExecutor()
	if !ghauth.IsGHCLIAvailable(context.Background(), exec) {
		t.Skip("gh CLI not authenticated, skipping test")
	}
}

// RequireGH fails the test if the gh CLI is not available.
func RequireGH(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("gh"); err != nil {
		t.Fatalf("gh CLI is required for this test: %v", err)
	}
}

// RequireGHAuth fails the test if the gh CLI is not authenticated.
func RequireGHAuth(t *testing.T) {
	t.Helper()
	RequireGH(t)

	exec := executor.NewBasicExecutor()
	if !ghauth.IsGHCLIAvailable(context.Background(), exec) {
		t.Fatal("gh CLI authentication is required for this test")
	}
}

// SkipIfShort skips the test in short mode (-short flag).
// Useful for skipping integration tests that make real API calls.
func SkipIfShort(t *testing.T) {
	t.Helper()
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
}
