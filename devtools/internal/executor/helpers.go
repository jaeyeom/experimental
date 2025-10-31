package executor

import (
	"context"
	"fmt"
	"os"
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
		Env:     map[string]string{"STDIN_DATA": stdin}, // Pass stdin data via env for now
	}

	// For commands that need stdin, we'll need to extend the executor
	// For now, use a workaround with shell piping
	if stdin != "" {
		shellCmd := fmt.Sprintf("echo %q | %s %s", stdin, command, strings.Join(args, " "))
		cfg.Command = "sh"
		cfg.Args = []string{"-c", shellCmd}
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

// prepareStdinViaFile creates a temporary file with stdin content and returns a shell command configuration.
// The cleanup function must be called by the caller to remove the temporary file.
//
// TODO(https://github.com/jaeyeom/experimental/issues/73): Replace this with native Stdin support in ToolConfig.
// This temporary file approach is a workaround for the lack of stdin support in the executor framework.
// Once ToolConfig has a Stdin field, this function can be removed and CombinedOutputWithStdin can
// directly set cfg.Stdin instead of using shell wrappers.
func prepareStdinViaFile(stdin, command string, args ...string) (cfg ToolConfig, cleanup func(), err error) {
	// Create a temporary file with the stdin content
	tmpfile, err := os.CreateTemp("", "stdin-*.tmp")
	if err != nil {
		return ToolConfig{}, nil, fmt.Errorf("failed to create temp file for stdin: %w", err)
	}
	tmpfileName := tmpfile.Name()

	// Cleanup function to remove the temp file
	cleanup = func() {
		os.Remove(tmpfileName)
	}

	// Write stdin content to the temp file
	if _, err := tmpfile.WriteString(stdin); err != nil {
		tmpfile.Close()
		cleanup()
		return ToolConfig{}, nil, fmt.Errorf("failed to write stdin to temp file: %w", err)
	}
	if err := tmpfile.Close(); err != nil {
		cleanup()
		return ToolConfig{}, nil, fmt.Errorf("failed to close temp file: %w", err)
	}

	// Build the command with proper quoting for args
	var quotedArgs []string
	for _, arg := range args {
		// Simple quoting: wrap in single quotes and escape any existing single quotes
		quoted := "'" + strings.ReplaceAll(arg, "'", "'\\''") + "'"
		quotedArgs = append(quotedArgs, quoted)
	}

	// Use cat to pipe the temp file to the command
	shellCmd := fmt.Sprintf("cat %s | %s %s", tmpfileName, command, strings.Join(quotedArgs, " "))
	cfg = ToolConfig{
		Command: "sh",
		Args:    []string{"-c", shellCmd},
	}

	return cfg, cleanup, nil
}

// CombinedOutputWithStdin runs a command with stdin input and returns combined stdout+stderr.
func CombinedOutputWithStdin(ctx context.Context, executor Executor, stdin string, command string, args ...string) ([]byte, error) {
	cfg := ToolConfig{
		Command: command,
		Args:    args,
	}

	// For commands that need stdin, use a temporary file approach
	if stdin != "" {
		var cleanup func()
		var err error
		cfg, cleanup, err = prepareStdinViaFile(stdin, command, args...)
		if err != nil {
			return nil, err
		}
		defer cleanup()
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
