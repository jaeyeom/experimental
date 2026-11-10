// Package remotecall runs commands that reach another machine, such as
// `herdr --machine` or ssh, and tells an unreachable machine apart from a
// real error on a reachable one.
//
// SSH can hang instead of failing: a locked key or an expired agent cache
// waits for a passphrase or a hardware-key touch that never comes. Callers
// therefore put an overall deadline on ctx, and Run kills the whole
// process group when it passes.
package remotecall

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// The two causes of an unreachable machine. Errors, held-message states,
// and warnings use these exact phrases, so an operator knows whether to fix
// the network or unlock the agent.
const (
	CauseHostUnreachable = "host unreachable"
	CauseAuthLocked      = "authentication locked (unlock your SSH/gpg agent)"
)

// waitDelay bounds how long Run waits for output pipes after a kill.
const waitDelay = time.Second

// UnreachableError means the machine could not be reached. It is not a
// failure of the command on that machine.
type UnreachableError struct {
	Machine string
	Cause   string
}

func (e *UnreachableError) Error() string {
	return "machine " + e.Machine + " unreachable: " + e.Cause
}

// AsUnreachable returns the UnreachableError in err's chain, or nil.
func AsUnreachable(err error) *UnreachableError {
	var ue *UnreachableError
	if errors.As(err, &ue) {
		return ue
	}
	return nil
}

// Result is one finished command. Code is -1 when the process was killed.
// TimedOut means ctx's deadline passed before the command finished.
type Result struct {
	Stdout   string
	Stderr   string
	Code     int
	TimedOut bool
}

// Run executes name with args in its own process group. When ctx ends,
// the whole group is killed, so a hung ssh child cannot outlive the call.
// A non-nil error means the process did not start.
func Run(ctx context.Context, name string, args ...string) (Result, error) {
	// The caller names the binary, such as herdr or ssh, and passes an
	// argument list, not a shell line.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, name, args...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	setProcessGroup(cmd)
	cmd.WaitDelay = waitDelay
	err := cmd.Run()
	res := Result{
		Stdout:   stdout.String(),
		Stderr:   stderr.String(),
		TimedOut: errors.Is(ctx.Err(), context.DeadlineExceeded),
	}
	if err == nil {
		return res, nil
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		res.Code = exitErr.ExitCode()
		return res, nil
	}
	if ctx.Err() != nil && cmd.Process != nil {
		res.Code = -1
		return res, nil
	}
	return res, fmt.Errorf("run %s: %w", name, err)
}

// Probe reports whether the machine's host answers a quick check that
// never prompts, such as a TCP connect to the SSH port or
// `herdr machine status`.
type Probe func(ctx context.Context) bool

// authMarkers are BatchMode authentication failures: the host answered.
var authMarkers = []string{
	"permission denied",
	"publickey",
	"authentication failed",
	"too many authentication failures",
	"agent refused operation",
	"sign_and_send_pubkey",
	"passphrase",
}

// networkMarkers mean the host did not answer or the machine is disabled.
var networkMarkers = []string{
	"connection refused",
	"connection timed out",
	"operation timed out",
	"no route to host",
	"network is unreachable",
	"could not resolve hostname",
	"name or service not known",
	"nodename nor servname",
	"is disabled",
}

// Classify returns an UnreachableError when the failed call res means
// machine could not be reached, or nil when res succeeded or is a real
// error from a reachable machine.
//
// An authentication error is CauseAuthLocked and a network error is
// CauseHostUnreachable. A deadline or any other failure asks probe: a
// host that answers makes a deadline CauseAuthLocked and any other
// failure a real error. A nil probe counts as a host that answers.
func Classify(ctx context.Context, machine string, res Result, probe Probe) *UnreachableError {
	if res.Code == 0 && !res.TimedOut {
		return nil
	}
	if !res.TimedOut {
		text := strings.ToLower(res.Stderr + "\n" + res.Stdout)
		if containsAny(text, authMarkers) {
			return &UnreachableError{Machine: machine, Cause: CauseAuthLocked}
		}
		if containsAny(text, networkMarkers) {
			return &UnreachableError{Machine: machine, Cause: CauseHostUnreachable}
		}
	}
	if probe != nil && !probe(ctx) {
		return &UnreachableError{Machine: machine, Cause: CauseHostUnreachable}
	}
	if res.TimedOut {
		return &UnreachableError{Machine: machine, Cause: CauseAuthLocked}
	}
	return nil
}

func containsAny(text string, markers []string) bool {
	for _, marker := range markers {
		if strings.Contains(text, marker) {
			return true
		}
	}
	return false
}
