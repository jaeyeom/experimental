package runner

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// BasicExecutor handles the execution of external tools and commands.
type BasicExecutor struct{}

// NewBasicExecutor creates a new BasicExecutor instance.
func NewBasicExecutor() *BasicExecutor {
	return &BasicExecutor{}
}

// Execute runs a tool with the given configuration and returns the result.
func (e *BasicExecutor) Execute(ctx context.Context, cfg ToolConfig) (*config.ExecutionResult, error) {
	// Validate configuration
	if err := cfg.Validate(); err != nil {
		return nil, err
	}

	// Create timeout context if timeout is specified
	execCtx := ctx
	var cancel context.CancelFunc
	if cfg.Timeout > 0 {
		execCtx, cancel = context.WithTimeout(ctx, cfg.Timeout)
		defer cancel()
	}

	// Create the command
	// #nosec G204 - This is intentional as we need to execute external tools with user-provided arguments
	cmd := exec.CommandContext(execCtx, cfg.Command, cfg.Args...)

	// Set working directory if specified
	if cfg.WorkingDir != "" {
		cmd.Dir = cfg.WorkingDir
	}

	// Set up environment variables
	if len(cfg.Env) > 0 {
		cmd.Env = os.Environ()
		for key, value := range cfg.Env {
			cmd.Env = append(cmd.Env, fmt.Sprintf("%s=%s", key, value))
		}
	}

	// Set up output buffers
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	// Record start time
	startTime := time.Now()

	// Execute the command
	err := cmd.Run()

	// Record end time
	endTime := time.Now()

	// Check if the command timed out
	timedOut := false
	if err != nil && execCtx.Err() == context.DeadlineExceeded {
		timedOut = true
		// Return a timeout error if this was due to our configured timeout
		if cfg.Timeout > 0 {
			return nil, &TimeoutError{
				Command: buildCommandString(cfg.Command, cfg.Args),
				Timeout: cfg.Timeout,
			}
		}
	}

	// Get exit code
	exitCode := 0
	if err != nil && !timedOut {
		// Check if it's an exec.ExitError to get the exit code
		if exitErr, ok := err.(*exec.ExitError); ok {
			exitCode = exitErr.ExitCode()
		} else if err == exec.ErrNotFound {
			// Handle missing executable
			return nil, &ExecutableNotFoundError{Command: cfg.Command}
		} else {
			// For other errors, use -1 as exit code
			exitCode = -1
		}
	} else if timedOut {
		// Use special exit code for timeout
		exitCode = -2
	}

	// Create the execution result
	result := &config.ExecutionResult{
		Command:    cfg.Command,
		Args:       cfg.Args,
		WorkingDir: cfg.WorkingDir,
		Output:     stdout.String(),
		Stderr:     stderr.String(),
		ExitCode:   exitCode,
		StartTime:  startTime,
		EndTime:    endTime,
		TimedOut:   timedOut,
	}

	// Set error message if execution failed (but not for timeout, as we return TimeoutError)
	if err != nil && err != exec.ErrNotFound && !timedOut {
		result.Error = err.Error()
	}

	return result, nil
}

// IsAvailable checks if a command is available in the system PATH.
func (e *BasicExecutor) IsAvailable(command string) bool {
	_, err := exec.LookPath(command)
	return err == nil
}

// buildCommandString constructs a shell-like command string for display purposes.
func buildCommandString(command string, args []string) string {
	parts := []string{command}
	for _, arg := range args {
		// Simple quoting for args with spaces
		if strings.Contains(arg, " ") {
			parts = append(parts, fmt.Sprintf("%q", arg))
		} else {
			parts = append(parts, arg)
		}
	}
	return strings.Join(parts, " ")
}
