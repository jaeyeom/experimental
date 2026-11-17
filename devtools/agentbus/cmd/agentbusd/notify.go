package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os/exec"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// decideTimeout bounds one decide call, which runs on the lease watcher.
const decideTimeout = 10 * time.Second

// decideArgv puts a lease note in the user's decide inbox. `decide send`
// only writes to an agent recipient's pane, so the note is a relay item
// the user answers or archives. Only a needs-adopt note offers the adopt
// options; other notes, such as a granted pause, are free text.
func decideArgv(decide, text string) []string {
	argv := []string{decide, "ask", longLinesFlag, "--owner", "agentbus", "--kind", "relay"}
	if strings.Contains(text, lease.NeedsAdoptMark) {
		argv = append(argv,
			"--option", "adopt:I will run agentbus lease adopt",
			"--option", "later:Leave it for the next claim",
			"--rec", "adopt")
	}
	return append(argv, text)
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
		argv := decideArgv(decide, text)
		err := run(ctx, argv)
		if olderDecide(err) {
			err = run(ctx, withoutLongLines(argv))
		}
		if err != nil {
			fmt.Fprintf(stderr, "agentbusd: decide note: %s\n", err)
		}
	}
}

// longLinesFlag lets decide ask take the notes and gate questions
// agentbusd builds itself past decide's line limit for agents.
const longLinesFlag = "--long-lines"

// olderDecide reports whether err is a decide that does not know
// --long-lines. The same command then runs again without it.
func olderDecide(err error) bool {
	return err != nil && strings.Contains(err.Error(), "flag provided but not defined: -long-lines")
}

// withoutLongLines is argv without --long-lines.
func withoutLongLines(argv []string) []string {
	return slices.DeleteFunc(slices.Clone(argv), func(a string) bool { return a == longLinesFlag })
}

// runQuiet runs argv, discarding its output. Its stderr is part of the
// error.
func runQuiet(ctx context.Context, argv []string) error {
	// argv[0] is decide from PATH; the note is its own argument, not a
	// shell string.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: see above
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s: %w: %s", argv[0], err, strings.TrimSpace(stderr.String()))
	}
	return nil
}
