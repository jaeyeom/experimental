package cli

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

// ExitOK means the command finished successfully.
const ExitOK = 0

// ExitError means a command returned an error.
const ExitError = 1

// ExitUsage means the command line was an unknown command or a bad flag.
const ExitUsage = 2

// ExitStartRefused means start refused before changing anything.
const ExitStartRefused = 3

// ExitStartClaimKept means start failed after claiming and kept the claim.
const ExitStartClaimKept = 4

// ExitStartReleased means start failed after claiming and released the claim.
const ExitStartReleased = 5

type exitStatuser interface {
	ExitStatus() int
}

type silentExitError struct {
	code int
}

func (e silentExitError) Error() string   { return fmt.Sprintf("exit status %d", e.code) }
func (e silentExitError) ExitStatus() int { return e.code }

type exitStatusError struct {
	code int
	err  error
}

func (e exitStatusError) Error() string {
	if e.err == nil {
		return fmt.Sprintf("exit status %d", e.code)
	}
	return e.err.Error()
}

func (e exitStatusError) Unwrap() error   { return e.err }
func (e exitStatusError) ExitStatus() int { return e.code }

// usageError marks an unknown command or a bad flag.
type usageError struct {
	err error
}

func (e usageError) Error() string {
	if e.err == nil {
		return "usage error"
	}
	return e.err.Error()
}

func (e usageError) Unwrap() error { return e.err }

func isUsageError(err error) bool {
	var usage usageError
	if errors.As(err, &usage) {
		return true
	}
	return strings.HasPrefix(err.Error(), "unknown command ")
}

func exitCode(err error, stderr io.Writer) int {
	if err == nil {
		return ExitOK
	}
	if isUsageError(err) {
		writeStderr(stderr, err.Error()+"\n")
		return ExitUsage
	}
	var silent silentExitError
	if errors.As(err, &silent) {
		return silent.code
	}
	writeStderr(stderr, fmt.Sprintf("factory: %s\n", err.Error()))
	var es exitStatuser
	if errors.As(err, &es) {
		return es.ExitStatus()
	}
	return ExitError
}

func writeStderr(stderr io.Writer, msg string) {
	_, err := io.WriteString(stderr, msg)
	if err != nil {
		return
	}
}
