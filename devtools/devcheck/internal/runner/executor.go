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

	// Create the command
	// #nosec G204 - This is intentional as we need to execute external tools with user-provided arguments
	cmd := exec.CommandContext(ctx, cfg.Command, cfg.Args...)

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

	// Get exit code
	exitCode := 0
	if err != nil {
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
		TimedOut:   false,
	}

	// Set error message if execution failed
	if err != nil && err != exec.ErrNotFound {
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
