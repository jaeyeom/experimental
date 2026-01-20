package executor

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
)

// Output runs a command and returns its stdout output, similar to exec.Command().Output().
// Returns an error if the command exits with a non-zero status.
func Output(ctx context.Context, executor Executor, command string, args ...string) ([]byte, error) {
	result, err := executor.Execute(ctx, ToolConfig{
		Command: command,
		Args:    args,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to execute %s: %w", command, err)
	}

	if result.ExitCode != 0 {
		return nil, &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return []byte(result.Output), nil
}

// OutputJSON runs a command and unmarshals the JSON output into the provided type.
// This is a generic helper that combines Output with JSON unmarshaling.
func OutputJSON[T any](ctx context.Context, executor Executor, command string, args ...string) (T, error) {
	var result T

	output, err := Output(ctx, executor, command, args...)
	if err != nil {
		return result, err
	}

	if err := json.Unmarshal(output, &result); err != nil {
		return result, fmt.Errorf("failed to parse JSON output from %s: %w", command, err)
	}

	return result, nil
}

// Run runs a command and returns an error if it exits with a non-zero status,
// similar to exec.Command().Run().
func Run(ctx context.Context, executor Executor, command string, args ...string) error {
	result, err := executor.Execute(ctx, ToolConfig{
		Command: command,
		Args:    args,
	})
	if err != nil {
		return fmt.Errorf("failed to execute %s: %w", command, err)
	}

	if result.ExitCode != 0 {
		return &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return nil
}

// CombinedOutput runs a command and returns its combined stdout and stderr output,
// similar to exec.Command().CombinedOutput().
// Returns an error if the command exits with a non-zero status.
func CombinedOutput(ctx context.Context, executor Executor, command string, args ...string) ([]byte, error) {
	result, err := executor.Execute(ctx, ToolConfig{
		Command: command,
		Args:    args,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to execute %s: %w", command, err)
	}

	combined := result.Output
	if result.Stderr != "" {
		if combined != "" {
			combined += "\n"
		}
		combined += result.Stderr
	}

	if result.ExitCode != 0 {
		return []byte(combined), &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return []byte(combined), nil
}

// OutputWithWorkDir runs a command in a specific working directory and returns its stdout output.
// Similar to Output but allows specifying a working directory.
func OutputWithWorkDir(ctx context.Context, executor Executor, workDir, command string, args ...string) ([]byte, error) {
	result, err := executor.Execute(ctx, ToolConfig{
		Command:    command,
		Args:       args,
		WorkingDir: workDir,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to execute %s: %w", command, err)
	}

	if result.ExitCode != 0 {
		return nil, &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return []byte(result.Output), nil
}

// RunWithWorkDir runs a command in a specific working directory.
// Similar to Run but allows specifying a working directory.
func RunWithWorkDir(ctx context.Context, executor Executor, workDir, command string, args ...string) error {
	result, err := executor.Execute(ctx, ToolConfig{
		Command:    command,
		Args:       args,
		WorkingDir: workDir,
	})
	if err != nil {
		return fmt.Errorf("failed to execute %s: %w", command, err)
	}

	if result.ExitCode != 0 {
		return &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return nil
}

// CombinedOutputWithWorkDir runs a command in a specific working directory and returns combined output.
// Similar to CombinedOutput but allows specifying a working directory.
func CombinedOutputWithWorkDir(ctx context.Context, executor Executor, workDir, command string, args ...string) ([]byte, error) {
	result, err := executor.Execute(ctx, ToolConfig{
		Command:    command,
		Args:       args,
		WorkingDir: workDir,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to execute %s: %w", command, err)
	}

	combined := result.Output
	if result.Stderr != "" {
		if combined != "" {
			combined += "\n"
		}
		combined += result.Stderr
	}

	if result.ExitCode != 0 {
		return []byte(combined), &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return []byte(combined), nil
}

// OutputWithStdin runs a command with stdin input and returns its stdout output.
func OutputWithStdin(ctx context.Context, executor Executor, stdin string, command string, args ...string) ([]byte, error) {
	cfg := ToolConfig{
		Command: command,
		Args:    args,
		Stdin:   strings.NewReader(stdin),
	}

	result, err := executor.Execute(ctx, cfg)
	if err != nil {
		return nil, fmt.Errorf("failed to execute %s: %w", command, err)
	}

	if result.ExitCode != 0 {
		return nil, &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return []byte(result.Output), nil
}

// CombinedOutputWithStdin runs a command with stdin input and returns combined stdout+stderr.
func CombinedOutputWithStdin(ctx context.Context, executor Executor, stdin string, command string, args ...string) ([]byte, error) {
	cfg := ToolConfig{
		Command: command,
		Args:    args,
		Stdin:   strings.NewReader(stdin),
	}

	result, err := executor.Execute(ctx, cfg)
	if err != nil {
		return nil, fmt.Errorf("failed to execute %s: %w", command, err)
	}

	combined := result.Output
	if result.Stderr != "" {
		if combined != "" {
			combined += "\n"
		}
		combined += result.Stderr
	}

	if result.ExitCode != 0 {
		return []byte(combined), &ExitError{
			ExitCode: result.ExitCode,
			Stderr:   result.Stderr,
		}
	}

	return []byte(combined), nil
}

// ExitError is returned when a command exits with a non-zero status.
type ExitError struct {
	ExitCode int
	Stderr   string
}

func (e *ExitError) Error() string {
	if e.Stderr != "" {
		// Trim the stderr to avoid very long error messages
		stderr := strings.TrimSpace(e.Stderr)
		if len(stderr) > 200 {
			stderr = stderr[:200] + "..."
		}
		return fmt.Sprintf("exit status %d: %s", e.ExitCode, stderr)
	}
	return fmt.Sprintf("exit status %d", e.ExitCode)
}
