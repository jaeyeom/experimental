package main

import (
	"fmt"
	"os"
	"strings"
)

// rejectLongLinesFlag returns a usage error when --long-lines is passed.
func rejectLongLinesFlag(args []string) error {
	for _, a := range args {
		if a == "--" {
			return nil
		}
		if a == "--long-lines" || a == "-long-lines" || strings.HasPrefix(a, "--long-lines=") || strings.HasPrefix(a, "-long-lines=") {
			return fmt.Errorf(`--long-lines was removed; no flag skips inbox shape checks
A person reads this in an inbox, often at a glance. Expected shape:
  - First line: the verdict (max 100 chars), readable alone, followed by a blank line or bullet.
  - Details as short bullet lines (no paragraph over 3 lines, max 160 chars per line, max 1200 chars total).
  - Pass multi-line text through --text-file or stdin, and exact post drafts or forwarded user text through --verbatim-file`)
		}
	}
	return nil
}

// resolveVerbatim returns the verbatim draft from --verbatim or --verbatim-file.
// The file is read byte for byte. Passing both is a usage error.
func resolveVerbatim(verbatim, verbatimFile string) (string, error) {
	if verbatim != "" && verbatimFile != "" {
		return "", fmt.Errorf("decide verbatim comes from --verbatim or --verbatim-file, not both")
	}
	if verbatimFile == "" {
		return verbatim, nil
	}
	raw, err := os.ReadFile(verbatimFile) //nolint:gosec // G304: user-specified draft file path
	if err != nil {
		return "", fmt.Errorf("decide verbatim file: %w", err)
	}
	return string(raw), nil
}
