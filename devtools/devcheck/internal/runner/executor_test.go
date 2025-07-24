package runner

import (
	"context"
	"errors"
	"os"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestNewBasicExecutor(t *testing.T) {
	executor := NewBasicExecutor()
	if executor == nil {
		t.Fatal("NewBasicExecutor() returned nil")
	}
}

func TestBasicExecutor_Execute(t *testing.T) {
	executor := NewBasicExecutor()
	ctx := context.Background()

	tests := []struct {
		name        string
		config      ToolConfig
		wantErr     bool
		checkOutput func(t *testing.T, output, stderr string, exitCode int)
	}{
		{
			name: "simple echo command",
			config: ToolConfig{
				Command: "echo",
				Args:    []string{"hello", "world"},
			},
			wantErr: false,
			checkOutput: func(t *testing.T, output, stderr string, exitCode int) {
				expectedOutput := "hello world\n"
				if output != expectedOutput {
					t.Errorf("output = %q, want %q", output, expectedOutput)
				}
				if stderr != "" {
					t.Errorf("stderr = %q, want empty", stderr)
				}
				if exitCode != 0 {
					t.Errorf("exitCode = %d, want 0", exitCode)
				}
			},
		},
		{
			name: "command with working directory",
			config: ToolConfig{
				Command:    "pwd",
				WorkingDir: "/tmp",
			},
			wantErr: false,
			checkOutput: func(t *testing.T, output, _ string, exitCode int) {
				if runtime.GOOS != "windows" {
					expectedOutput := "/tmp\n"
					if output != expectedOutput {
						t.Errorf("output = %q, want %q", output, expectedOutput)
					}
				}
				if exitCode != 0 {
					t.Errorf("exitCode = %d, want 0", exitCode)
				}
			},
		},
		{
			name: "command with environment variables",
			config: ToolConfig{
				Command: "sh",
				Args:    []string{"-c", "echo $TEST_VAR"},
				Env:     map[string]string{"TEST_VAR": "test_value"},
			},
			wantErr: false,
			checkOutput: func(t *testing.T, output, _ string, exitCode int) {
				expectedOutput := "test_value\n"
				if output != expectedOutput {
					t.Errorf("output = %q, want %q", output, expectedOutput)
				}
				if exitCode != 0 {
					t.Errorf("exitCode = %d, want 0", exitCode)
				}
			},
		},
		{
			name: "command with non-zero exit code",
			config: ToolConfig{
				Command: "sh",
				Args:    []string{"-c", "exit 42"},
			},
			wantErr: false,
			checkOutput: func(t *testing.T, _, _ string, exitCode int) {
				if exitCode != 42 {
					t.Errorf("exitCode = %d, want 42", exitCode)
				}
			},
		},
		{
			name: "command that writes to stderr",
			config: ToolConfig{
				Command: "sh",
				Args:    []string{"-c", "echo error >&2"},
			},
			wantErr: false,
			checkOutput: func(t *testing.T, _, stderr string, exitCode int) {
				expectedStderr := "error\n"
				if stderr != expectedStderr {
					t.Errorf("stderr = %q, want %q", stderr, expectedStderr)
				}
				if exitCode != 0 {
					t.Errorf("exitCode = %d, want 0", exitCode)
				}
			},
		},
		{
			name: "nonexistent command",
			config: ToolConfig{
				Command: "nonexistent-command-that-should-not-exist",
			},
			wantErr: true,
		},
		{
			name: "invalid config - empty command",
			config: ToolConfig{
				Command: "",
			},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := executor.Execute(ctx, tt.config)

			if tt.wantErr {
				if err == nil && result != nil && result.ExitCode == 0 {
					t.Errorf("Execute() error = nil, wantErr = true")
				}
				return
			}

			if err != nil {
				t.Errorf("Execute() unexpected error = %v", err)
				return
			}

			if result == nil {
				t.Fatal("Execute() returned nil result")
			}

			// Verify basic fields
			if result.Command != tt.config.Command {
				t.Errorf("Command = %q, want %q", result.Command, tt.config.Command)
			}

			if len(result.Args) != len(tt.config.Args) {
				t.Errorf("Args length = %d, want %d", len(result.Args), len(tt.config.Args))
			} else {
				for i, arg := range result.Args {
					if arg != tt.config.Args[i] {
						t.Errorf("Args[%d] = %q, want %q", i, arg, tt.config.Args[i])
					}
				}
			}

			if result.WorkingDir != tt.config.WorkingDir {
				t.Errorf("WorkingDir = %q, want %q", result.WorkingDir, tt.config.WorkingDir)
			}

			// Verify timing
			if result.StartTime.IsZero() {
				t.Error("StartTime is zero")
			}
			if result.EndTime.IsZero() {
				t.Error("EndTime is zero")
			}
			if result.EndTime.Before(result.StartTime) {
				t.Error("EndTime is before StartTime")
			}
			if result.Duration() < 0 {
				t.Errorf("Duration() = %v, want positive", result.Duration())
			}

			// Check custom output validation
			if tt.checkOutput != nil {
				tt.checkOutput(t, result.Output, result.Stderr, result.ExitCode)
			}
		})
	}
}

func TestBasicExecutor_Execute_Context(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping context cancellation test on Windows")
	}

	executor := NewBasicExecutor()

	// Test context cancellation
	ctx, cancel := context.WithCancel(context.Background())

	toolConfig := ToolConfig{
		Command: "sleep",
		Args:    []string{"10"},
	}

	// Start execution in a goroutine
	done := make(chan struct{})
	var result *config.ExecutionResult
	var err error

	go func() {
		result, err = executor.Execute(ctx, toolConfig)
		close(done)
	}()

	// Give the command time to start
	time.Sleep(100 * time.Millisecond)

	// Cancel the context
	cancel()

	// Wait for execution to complete
	select {
	case <-done:
		// Command completed (was cancelled)
	case <-time.After(2 * time.Second):
		t.Fatal("Execute() did not respond to context cancellation")
	}

	// The command should have been interrupted
	if err == nil && result != nil && result.ExitCode == 0 {
		t.Error("Expected non-zero exit code for cancelled command")
	}
}

func TestBasicExecutor_Execute_Timeout(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping timeout test on Windows")
	}

	executor := NewBasicExecutor()
	ctx := context.Background()

	tests := []struct {
		name           string
		config         ToolConfig
		wantErr        bool
		wantTimeoutErr bool
		checkResult    func(t *testing.T, result *config.ExecutionResult, err error)
	}{
		{
			name: "command with timeout that completes in time",
			config: ToolConfig{
				Command: "sleep",
				Args:    []string{"0.1"},
				Timeout: 1 * time.Second,
			},
			wantErr:        false,
			wantTimeoutErr: false,
			checkResult: func(t *testing.T, result *config.ExecutionResult, _ error) {
				if result == nil {
					t.Fatal("Expected result, got nil")
				}
				if result.TimedOut {
					t.Error("Expected TimedOut to be false")
				}
				if result.ExitCode != 0 {
					t.Errorf("Expected exit code 0, got %d", result.ExitCode)
				}
			},
		},
		{
			name: "command with timeout that times out",
			config: ToolConfig{
				Command: "sleep",
				Args:    []string{"2"},
				Timeout: 200 * time.Millisecond,
			},
			wantErr:        true,
			wantTimeoutErr: true,
			checkResult: func(t *testing.T, result *config.ExecutionResult, err error) {
				if result != nil {
					t.Error("Expected nil result for timeout error")
				}

				// Check that we got a TimeoutError
				var timeoutErr *TimeoutError
				if !errors.As(err, &timeoutErr) {
					t.Errorf("Expected TimeoutError, got %T: %v", err, err)
				} else {
					if timeoutErr.Timeout != 200*time.Millisecond {
						t.Errorf("Expected timeout 200ms, got %v", timeoutErr.Timeout)
					}
					if !strings.Contains(timeoutErr.Command, "sleep") {
						t.Errorf("Expected command to contain 'sleep', got %q", timeoutErr.Command)
					}
				}
			},
		},
		{
			name: "command without timeout runs normally",
			config: ToolConfig{
				Command: "echo",
				Args:    []string{"test"},
				Timeout: 0, // No timeout
			},
			wantErr:        false,
			wantTimeoutErr: false,
			checkResult: func(t *testing.T, result *config.ExecutionResult, _ error) {
				if result == nil {
					t.Fatal("Expected result, got nil")
				}
				if result.TimedOut {
					t.Error("Expected TimedOut to be false")
				}
				if result.ExitCode != 0 {
					t.Errorf("Expected exit code 0, got %d", result.ExitCode)
				}
				if !strings.Contains(result.Output, "test") {
					t.Errorf("Expected output to contain 'test', got %q", result.Output)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			start := time.Now()
			result, err := executor.Execute(ctx, tt.config)
			duration := time.Since(start)

			if tt.wantErr != (err != nil) {
				t.Errorf("Execute() error = %v, wantErr = %v", err, tt.wantErr)
				return
			}

			// For timeout tests, verify the timing
			if tt.wantTimeoutErr && tt.config.Timeout > 0 {
				// Should complete close to the timeout duration
				expectedMax := tt.config.Timeout + 500*time.Millisecond // Allow some overhead
				if duration > expectedMax {
					t.Errorf("Timeout took too long: %v, expected max: %v", duration, expectedMax)
				}
			}

			if tt.checkResult != nil {
				tt.checkResult(t, result, err)
			}
		})
	}
}

func TestBasicExecutor_IsAvailable(t *testing.T) {
	executor := NewBasicExecutor()

	tests := []struct {
		name          string
		command       string
		wantAvailable bool
	}{
		{
			name:          "common command - echo",
			command:       "echo",
			wantAvailable: true,
		},
		{
			name:          "common command - sh",
			command:       "sh",
			wantAvailable: runtime.GOOS != "windows",
		},
		{
			name:          "nonexistent command",
			command:       "nonexistent-command-that-should-not-exist",
			wantAvailable: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := executor.IsAvailable(tt.command)
			if got != tt.wantAvailable {
				t.Errorf("IsAvailable(%q) = %v, want %v", tt.command, got, tt.wantAvailable)
			}
		})
	}
}

func TestBuildCommandString(t *testing.T) {
	tests := []struct {
		name    string
		command string
		args    []string
		want    string
	}{
		{
			name:    "simple command",
			command: "echo",
			args:    []string{"hello"},
			want:    "echo hello",
		},
		{
			name:    "command with multiple args",
			command: "go",
			args:    []string{"test", "./..."},
			want:    "go test ./...",
		},
		{
			name:    "args with spaces",
			command: "echo",
			args:    []string{"hello world", "foo"},
			want:    `echo "hello world" foo`,
		},
		{
			name:    "no args",
			command: "ls",
			args:    []string{},
			want:    "ls",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := buildCommandString(tt.command, tt.args)
			if got != tt.want {
				t.Errorf("buildCommandString() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestBasicExecutor_Execute_Permissions(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping permission test on Windows")
	}

	// Create a test file without execute permissions
	tmpFile, err := os.CreateTemp("", "test-no-exec-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())

	// Write a simple script
	if _, err := tmpFile.WriteString("#!/bin/sh\necho test\n"); err != nil {
		t.Fatal(err)
	}
	tmpFile.Close()

	// Make sure it's not executable
	if err := os.Chmod(tmpFile.Name(), 0o644); err != nil {
		t.Fatal(err)
	}

	executor := NewBasicExecutor()
	toolConfig := ToolConfig{
		Command: tmpFile.Name(),
	}

	result, err := executor.Execute(context.Background(), toolConfig)
	if err == nil && result != nil && result.ExitCode == 0 {
		t.Error("Expected error for non-executable file")
	}
}

func TestBasicExecutor_Execute_TimeoutTiming(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping timeout timing test on Windows")
	}

	executor := NewBasicExecutor()
	ctx := context.Background()

	// Test that timeout is enforced accurately
	config := ToolConfig{
		Command: "sleep",
		Args:    []string{"5"},          // Sleep for 5 seconds
		Timeout: 500 * time.Millisecond, // But timeout after 500ms
	}

	start := time.Now()
	result, err := executor.Execute(ctx, config)
	duration := time.Since(start)

	// Should have timed out
	if err == nil {
		t.Fatal("Expected timeout error, got nil")
	}

	// Should return TimeoutError
	var timeoutErr *TimeoutError
	if !errors.As(err, &timeoutErr) {
		t.Fatalf("Expected TimeoutError, got %T: %v", err, err)
	}

	// Should not return a result
	if result != nil {
		t.Error("Expected nil result for timeout")
	}

	// Should complete within reasonable time of the timeout
	expectedMin := 500 * time.Millisecond
	expectedMax := 1000 * time.Millisecond // Allow some overhead

	if duration < expectedMin {
		t.Errorf("Command completed too quickly: %v, expected at least: %v", duration, expectedMin)
	}
	if duration > expectedMax {
		t.Errorf("Command took too long: %v, expected at most: %v", duration, expectedMax)
	}
}
