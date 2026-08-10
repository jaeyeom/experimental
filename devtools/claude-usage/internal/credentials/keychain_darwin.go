//go:build darwin

package credentials

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
	"time"
)

// defaultKeychainTimeout bounds Keychain lookups so a stuck security(1)
// cannot hang the CLI indefinitely.
const defaultKeychainTimeout = 3 * time.Second

// defaultKeychainLoader reads a generic password via /usr/bin/security.
// The secret is returned only to the caller and must not be logged.
func defaultKeychainLoader(ctx context.Context, service, account string) (string, error) {
	if service == "" {
		return "", fmt.Errorf("keychain service name is empty")
	}
	args := []string{"find-generic-password", "-s", service}
	if account != "" {
		args = append(args, "-a", account)
	}
	args = append(args, "-w")

	// Bound keychain waits; Claude Code may prompt for access.
	if _, hasDeadline := ctx.Deadline(); !hasDeadline {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, defaultKeychainTimeout)
		defer cancel()
	}

	// Absolute path avoids PATH hijacking (same approach as claude-hud).
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, "/usr/bin/security", args...) //nolint:gosec // fixed binary path; service/account are not shell-interpreted
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		// Prefer stderr for missing-item detection; never return stdout (token).
		msg := bytes.TrimSpace(stderr.Bytes())
		if len(msg) == 0 {
			if ctx.Err() != nil {
				return "", fmt.Errorf("keychain lookup timed out after %s", defaultKeychainTimeout)
			}
			return "", fmt.Errorf("keychain lookup failed: %w", err)
		}
		// Strip any accidental secret-looking content; security(1) errors are plain text.
		return "", fmt.Errorf("%s", string(msg))
	}
	// security -w prints the password with a trailing newline.
	return string(bytes.TrimSuffix(stdout.Bytes(), []byte{'\n'})), nil
}
