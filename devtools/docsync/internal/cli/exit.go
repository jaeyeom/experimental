package cli

import (
	"errors"
	"fmt"
	"io"
)

const (
	// ExitOK is a successful run with no implicated docs (check) or a clean mapping (validate).
	ExitOK = 0
	// ExitDocsAffected is check: one or more docs implicated; validate: warnings only.
	ExitDocsAffected = 1
	// ExitUsage is usage, config, or flag errors (and validate hard errors).
	ExitUsage = 2
	// ExitPrecondition is a missing tool, not a git repo, or mapping file not found via discovery.
	ExitPrecondition = 3
)

// ExitError carries a process exit code.
type ExitError struct {
	Code int
	Err  error
}

func (e *ExitError) Error() string {
	if e == nil {
		return ""
	}
	if e.Err != nil {
		return e.Err.Error()
	}
	return fmt.Sprintf("exit %d", e.Code)
}

func (e *ExitError) Unwrap() error {
	if e == nil {
		return nil
	}
	return e.Err
}

func report(stderr io.Writer, err error) int {
	if err == nil {
		return ExitOK
	}
	var exitErr *ExitError
	if errors.As(err, &exitErr) {
		if exitErr.Err != nil {
			fmt.Fprintf(stderr, "docsync: %v\n", exitErr.Err)
		}
		return exitErr.Code
	}
	fmt.Fprintf(stderr, "docsync: %v\n", err)
	return ExitUsage
}
