// Package executor provides command execution capabilities with mocking support.
// This package is designed to be imported by external projects.
package executor

import (
	"fmt"
	"io"
	"time"
)

// ToolConfig represents the configuration for executing a tool.
type ToolConfig struct {
	// Command is the executable command to run
	Command string

	// Args are the arguments to pass to the command
	Args []string

	// WorkingDir is the directory where the command should be executed
	// If empty, uses the current working directory
	WorkingDir string

	// Timeout is the maximum duration to allow the command to run
	// If zero, no timeout is applied
	Timeout time.Duration

	// MaxRetries is the maximum number of retry attempts for flaky tools
	MaxRetries int

	// RetryDelay is the delay between retry attempts
	RetryDelay time.Duration

	// Env contains additional environment variables for the command
	// These will be added to the current environment
	Env map[string]string

	// Stdin is an optional reader for providing input to the command
	// If nil, the command will have no stdin
	Stdin io.Reader

	// CommandBuilder defines how to build the command for execution.
	// If nil, defaults to DirectCommandBuilder for direct execution.
	// Use ShellCommandBuilder for tools that need shell execution (e.g., Bazel, Gradle).
	CommandBuilder CommandBuilder
}

// Validate ensures the ToolConfig has valid data.
func (tc *ToolConfig) Validate() error {
	if tc.Command == "" {
		return &ValidationError{Field: "Command", Message: "command cannot be empty"}
	}

	if tc.MaxRetries < 0 {
		return &ValidationError{Field: "MaxRetries", Message: "maxRetries cannot be negative"}
	}

	if tc.RetryDelay < 0 {
		return &ValidationError{Field: "RetryDelay", Message: "retryDelay cannot be negative"}
	}

	if tc.Timeout < 0 {
		return &ValidationError{Field: "Timeout", Message: "timeout cannot be negative"}
	}

	return nil
}

// Error types for different failure scenarios

// ValidationError represents a validation failure in tool configuration.
type ValidationError struct {
	Field   string
	Message string
}

func (e *ValidationError) Error() string {
	return "validation error in field '" + e.Field + "': " + e.Message
}

// TimeoutError represents a timeout during command execution.
type TimeoutError struct {
	Command string
	Timeout time.Duration
}

func (e *TimeoutError) Error() string {
	return "command '" + e.Command + "' timed out after " + e.Timeout.String()
}

// ExecutableNotFoundError represents a missing executable.
type ExecutableNotFoundError struct {
	Command string
}

func (e *ExecutableNotFoundError) Error() string {
	return "executable not found: " + e.Command
}

// RetryExhaustedError represents failure after all retry attempts.
type RetryExhaustedError struct {
	Command   string
	Attempts  int
	LastError error
}

func (e *RetryExhaustedError) Error() string {
	return fmt.Sprintf("retry exhausted for command %q after %d attempts. Last error: %v",
		e.Command, e.Attempts, e.LastError)
}

// Unwrap returns the last error for error chain compatibility.
func (e *RetryExhaustedError) Unwrap() error {
	return e.LastError
}
