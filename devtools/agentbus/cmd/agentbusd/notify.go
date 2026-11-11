package main

import (
	"context"
	"fmt"
	"io"
	"os/exec"
	"time"
)

// decideTimeout bounds one decide call, which runs on the lease watcher.
const decideTimeout = 10 * time.Second

// decideArgv puts a lease note in the user's decide inbox. `decide send`
// only writes to an agent recipient's pane, so the note is a relay item
// the user answers or archives.
func decideArgv(decide, text string) []string {
	return []string{
		decide, "ask", "--owner", "agentbus", "--kind", "relay",
		"--option", "adopt:I will run agentbus lease adopt",
		"--option", "later:Leave it for the next claim",
		"--rec", "adopt", text,
	}
}

// decideNotifier tells the user about lease events through decide, or
// returns nil when decide is not installed. lookPath finds decide; run
// runs a command. A failed note is reported on stderr and never stops
// the daemon.
func decideNotifier(lookPath func(string) (string, error), run func(ctx context.Context, argv []string) error, stderr io.Writer) func(string) {
	decide, err := lookPath("decide")
	if err != nil {
		return nil
	}
	return func(text string) {
		ctx, cancel := context.WithTimeout(context.Background(), decideTimeout)
		defer cancel()
		if err := run(ctx, decideArgv(decide, text)); err != nil {
			fmt.Fprintf(stderr, "agentbusd: decide note: %s\n", err)
		}
	}
}

// runQuiet runs argv, discarding its output.
func runQuiet(ctx context.Context, argv []string) error {
	// argv[0] is decide from PATH; the note is its own argument, not a
	// shell string.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: see above
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s: %w", argv[0], err)
	}
	return nil
}
