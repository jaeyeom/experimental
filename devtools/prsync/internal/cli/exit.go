package cli

import (
	"errors"
	"fmt"
	"io"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
)

const (
	// ExitOK is a successful run.
	ExitOK = 0
	// ExitUnsafe is gate only: busy set non-empty.
	ExitUnsafe = 1
	// ExitUsage is usage, config, or flag errors.
	ExitUsage = 2
	// ExitPrecondition is a missing tool or auth failure.
	ExitPrecondition = 3
	// ExitGateTimeout is a live dispatch that timed out on the gate.
	ExitGateTimeout = 4
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
	var keyErr *config.KeyError
	if errors.As(err, &keyErr) {
		fmt.Fprintf(stderr, "prsync: config error: %s\n", keyErr)
		return ExitUsage
	}
	var exitErr *ExitError
	if errors.As(err, &exitErr) {
		if exitErr.Err != nil {
			fmt.Fprintf(stderr, "prsync: %v\n", exitErr.Err)
		}
		return exitErr.Code
	}
	fmt.Fprintf(stderr, "prsync: %v\n", err)
	return ExitUsage
}
