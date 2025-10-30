package executor_test

import (
	"context"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

func TestOutput(t *testing.T) {
	tests := []struct {
		name        string
		mockResult  *executor.ExecutionResult
		mockError   error
		wantOutput  string
		wantErr     bool
		errContains string
	}{
		{
			name: "successful command",
			mockResult: &executor.ExecutionResult{
				Command:  "echo",
				Args:     []string{"hello"},
				Output:   "hello\n",
				ExitCode: 0,
			},
			wantOutput: "hello\n",
			wantErr:    false,
		},
		{
			name: "non-zero exit code",
			mockResult: &executor.ExecutionResult{
				Command:  "false",
				ExitCode: 1,
				Stderr:   "command failed",
			},
			wantErr:     true,
			errContains: "exit status 1",
		},
		{
			name:        "execution error",
			mockError:   &executor.ExecutableNotFoundError{Command: "notfound"},
			wantErr:     true,
			errContains: "failed to execute",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mock := executor.NewMockExecutor()
			mock.SetResult(tt.mockResult, tt.mockError)

			output, err := executor.Output(context.Background(), mock, "test", "arg")

			if (err != nil) != tt.wantErr {
				t.Errorf("Output() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr && tt.errContains != "" && !strings.Contains(err.Error(), tt.errContains) {
				t.Errorf("Output() error = %v, should contain %q", err, tt.errContains)
			}

			if !tt.wantErr && string(output) != tt.wantOutput {
				t.Errorf("Output() = %q, want %q", output, tt.wantOutput)
			}
		})
	}
}

func TestRun(t *testing.T) {
	tests := []struct {
		name        string
		mockResult  *executor.ExecutionResult
		mockError   error
		wantErr     bool
		errContains string
	}{
		{
			name: "successful command",
			mockResult: &executor.ExecutionResult{
				Command:  "true",
				ExitCode: 0,
			},
			wantErr: false,
		},
		{
			name: "non-zero exit code",
			mockResult: &executor.ExecutionResult{
				Command:  "false",
				ExitCode: 1,
				Stderr:   "command failed",
			},
			wantErr:     true,
			errContains: "exit status 1",
		},
		{
			name:        "execution error",
			mockError:   &executor.ExecutableNotFoundError{Command: "notfound"},
			wantErr:     true,
			errContains: "failed to execute",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mock := executor.NewMockExecutor()
			mock.SetResult(tt.mockResult, tt.mockError)

			err := executor.Run(context.Background(), mock, "test", "arg")

			if (err != nil) != tt.wantErr {
				t.Errorf("Run() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr && tt.errContains != "" && !strings.Contains(err.Error(), tt.errContains) {
				t.Errorf("Run() error = %v, should contain %q", err, tt.errContains)
			}
		})
	}
}

func TestCombinedOutput(t *testing.T) {
	tests := []struct {
		name        string
		mockResult  *executor.ExecutionResult
		mockError   error
		wantOutput  string
		wantErr     bool
		errContains string
	}{
		{
			name: "successful command with only stdout",
			mockResult: &executor.ExecutionResult{
				Command:  "echo",
				Args:     []string{"hello"},
				Output:   "hello\n",
				ExitCode: 0,
			},
			wantOutput: "hello\n",
			wantErr:    false,
		},
		{
			name: "successful command with stdout and stderr",
			mockResult: &executor.ExecutionResult{
				Command:  "cmd",
				Output:   "stdout output",
				Stderr:   "stderr output",
				ExitCode: 0,
			},
			wantOutput: "stdout output\nstderr output",
			wantErr:    false,
		},
		{
			name: "non-zero exit code with output",
			mockResult: &executor.ExecutionResult{
				Command:  "false",
				Output:   "some output",
				Stderr:   "error message",
				ExitCode: 1,
			},
			wantOutput:  "some output\nerror message",
			wantErr:     true,
			errContains: "exit status 1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mock := executor.NewMockExecutor()
			mock.SetResult(tt.mockResult, tt.mockError)

			output, err := executor.CombinedOutput(context.Background(), mock, "test", "arg")

			if (err != nil) != tt.wantErr {
				t.Errorf("CombinedOutput() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr && tt.errContains != "" && !strings.Contains(err.Error(), tt.errContains) {
				t.Errorf("CombinedOutput() error = %v, should contain %q", err, tt.errContains)
			}

			if string(output) != tt.wantOutput {
				t.Errorf("CombinedOutput() = %q, want %q", output, tt.wantOutput)
			}
		})
	}
}

func TestOutputWithWorkDir(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.SetResult(&executor.ExecutionResult{
		Command:    "ls",
		WorkingDir: "/test/dir",
		Output:     "file.txt\n",
		ExitCode:   0,
	}, nil)

	output, err := executor.OutputWithWorkDir(context.Background(), mock, "/test/dir", "ls")
	if err != nil {
		t.Errorf("OutputWithWorkDir() error = %v", err)
		return
	}

	if string(output) != "file.txt\n" {
		t.Errorf("OutputWithWorkDir() = %q, want %q", output, "file.txt\n")
	}

	// Verify the working directory was set
	executions := mock.Executions()
	if len(executions) != 1 {
		t.Fatalf("Expected 1 execution, got %d", len(executions))
	}
	if executions[0].WorkingDir != "/test/dir" {
		t.Errorf("WorkingDir = %q, want %q", executions[0].WorkingDir, "/test/dir")
	}
}

func TestRunWithWorkDir(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.SetResult(&executor.ExecutionResult{
		Command:    "make",
		WorkingDir: "/project",
		ExitCode:   0,
	}, nil)

	err := executor.RunWithWorkDir(context.Background(), mock, "/project", "make")
	if err != nil {
		t.Errorf("RunWithWorkDir() error = %v", err)
		return
	}

	// Verify the working directory was set
	executions := mock.Executions()
	if len(executions) != 1 {
		t.Fatalf("Expected 1 execution, got %d", len(executions))
	}
	if executions[0].WorkingDir != "/project" {
		t.Errorf("WorkingDir = %q, want %q", executions[0].WorkingDir, "/project")
	}
}

func TestCombinedOutputWithWorkDir(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.SetResult(&executor.ExecutionResult{
		Command:    "git",
		Args:       []string{"status"},
		WorkingDir: "/repo",
		Output:     "On branch main",
		Stderr:     "Your branch is up to date",
		ExitCode:   0,
	}, nil)

	output, err := executor.CombinedOutputWithWorkDir(context.Background(), mock, "/repo", "git", "status")
	if err != nil {
		t.Errorf("CombinedOutputWithWorkDir() error = %v", err)
		return
	}

	expectedOutput := "On branch main\nYour branch is up to date"
	if string(output) != expectedOutput {
		t.Errorf("CombinedOutputWithWorkDir() = %q, want %q", output, expectedOutput)
	}

	// Verify the working directory was set
	executions := mock.Executions()
	if len(executions) != 1 {
		t.Fatalf("Expected 1 execution, got %d", len(executions))
	}
	if executions[0].WorkingDir != "/repo" {
		t.Errorf("WorkingDir = %q, want %q", executions[0].WorkingDir, "/repo")
	}
}

func TestExitError(t *testing.T) {
	tests := []struct {
		name     string
		exitErr  *executor.ExitError
		wantMsg  string
		contains string
	}{
		{
			name: "with stderr",
			exitErr: &executor.ExitError{
				ExitCode: 1,
				Stderr:   "command not found",
			},
			contains: "exit status 1",
		},
		{
			name: "without stderr",
			exitErr: &executor.ExitError{
				ExitCode: 127,
			},
			wantMsg: "exit status 127",
		},
		{
			name: "with long stderr (truncated)",
			exitErr: &executor.ExitError{
				ExitCode: 1,
				Stderr:   strings.Repeat("a", 300),
			},
			contains: "...",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			msg := tt.exitErr.Error()

			if tt.wantMsg != "" && msg != tt.wantMsg {
				t.Errorf("Error() = %q, want %q", msg, tt.wantMsg)
			}

			if tt.contains != "" && !strings.Contains(msg, tt.contains) {
				t.Errorf("Error() = %q, should contain %q", msg, tt.contains)
			}
		})
	}
}
