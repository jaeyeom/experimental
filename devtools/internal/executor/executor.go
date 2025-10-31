package executor

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// BasicExecutor handles the execution of external tools and commands.
type BasicExecutor struct{}

// NewBasicExecutor creates a new BasicExecutor instance.
func NewBasicExecutor() *BasicExecutor {
	return &BasicExecutor{}
}

// Execute runs a tool with the given configuration and returns the result.
func (e *BasicExecutor) Execute(ctx context.Context, cfg ToolConfig) (*ExecutionResult, error) {
	if err := cfg.Validate(); err != nil {
		return nil, err
	}

	execCtx, cancel := e.createExecutionContext(ctx, cfg.Timeout)
	if cancel != nil {
		defer cancel()
	}

	cmd := e.createCommand(execCtx, cfg)
	e.setupCommand(cmd, cfg)

	stdout, stderr, startTime, endTime, err := e.executeCommand(cmd)

	if timedOut := e.handleTimeout(execCtx, err, cfg); timedOut {
		return nil, &TimeoutError{
			Command: buildCommandString(cfg.Command, cfg.Args),
			Timeout: cfg.Timeout,
		}
	}

	exitCode, err := e.processExecutionError(err, cfg.Command)
	if err != nil {
		return nil, err
	}

	return e.buildExecutionResult(cfg, stdout, stderr, startTime, endTime, exitCode, err), nil
}

func (e *BasicExecutor) createExecutionContext(ctx context.Context, timeout time.Duration) (context.Context, context.CancelFunc) {
	if timeout > 0 {
		return context.WithTimeout(ctx, timeout)
	}
	return ctx, nil
}

func (e *BasicExecutor) createCommand(ctx context.Context, cfg ToolConfig) *exec.Cmd {
	if requiresShellExecution(cfg.Command) {
		fullCommand := buildShellCommand(cfg.Command, cfg.Args)
		// #nosec G204 - This is intentional as we need to execute external tools with user-provided arguments
		return exec.CommandContext(ctx, "sh", "-c", fullCommand)
	}
	// #nosec G204 - This is intentional as we need to execute external tools with user-provided arguments
	return exec.CommandContext(ctx, cfg.Command, cfg.Args...)
}

func (e *BasicExecutor) setupCommand(cmd *exec.Cmd, cfg ToolConfig) {
	if cfg.WorkingDir != "" {
		cmd.Dir = cfg.WorkingDir
	}

	if len(cfg.Env) > 0 {
		cmd.Env = os.Environ()
		for key, value := range cfg.Env {
			cmd.Env = append(cmd.Env, fmt.Sprintf("%s=%s", key, value))
		}
	}

	if cfg.Stdin != nil {
		cmd.Stdin = cfg.Stdin
	}
}

func (e *BasicExecutor) executeCommand(cmd *exec.Cmd) (bytes.Buffer, bytes.Buffer, time.Time, time.Time, error) {
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	startTime := time.Now()
	err := cmd.Run()
	endTime := time.Now()

	return stdout, stderr, startTime, endTime, err //nolint:wrapcheck // Need to preserve original error type for exit code extraction
}

func (e *BasicExecutor) handleTimeout(ctx context.Context, err error, cfg ToolConfig) bool {
	return err != nil && ctx.Err() == context.DeadlineExceeded && cfg.Timeout > 0
}

func (e *BasicExecutor) processExecutionError(err error, command string) (int, error) {
	if err == nil {
		return 0, nil
	}

	if err == exec.ErrNotFound {
		return 0, &ExecutableNotFoundError{Command: command}
	}

	if exitErr, ok := err.(*exec.ExitError); ok {
		return exitErr.ExitCode(), nil
	}

	return -1, nil
}

func (e *BasicExecutor) buildExecutionResult(cfg ToolConfig, stdout, stderr bytes.Buffer, startTime, endTime time.Time, exitCode int, execErr error) *ExecutionResult {
	result := &ExecutionResult{
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

	if execErr != nil && execErr != exec.ErrNotFound {
		result.Error = execErr.Error()
	}

	return result
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

// requiresShellExecution determines if a command should be run through a shell.
// Some tools (like Bazel) have client-server architectures that work better
// when executed in a proper shell environment.
func requiresShellExecution(command string) bool {
	shellRequiredTools := map[string]bool{
		"bazel":  true,
		"gradle": true, // Gradle daemon has similar issues
		"maven":  true, // Maven can have similar server-based issues
		"sbt":    true, // Scala Build Tool has a server mode
	}
	return shellRequiredTools[command]
}

// buildShellCommand constructs a properly quoted shell command string.
func buildShellCommand(command string, args []string) string {
	parts := []string{command}
	for _, arg := range args {
		// Properly quote arguments for shell execution
		if strings.Contains(arg, " ") || strings.Contains(arg, "'") || strings.Contains(arg, "\"") {
			// Use single quotes and escape any single quotes in the argument
			escaped := strings.ReplaceAll(arg, "'", "'\"'\"'")
			parts = append(parts, fmt.Sprintf("'%s'", escaped))
		} else {
			parts = append(parts, arg)
		}
	}
	return strings.Join(parts, " ")
}
