package runner

import (
	"context"
	"strings"
	"testing"
)

func TestRequiresShellExecution(t *testing.T) {
	tests := []struct {
		command  string
		expected bool
	}{
		{"bazel", true},
		{"gradle", true},
		{"maven", true},
		{"sbt", true},
		{"go", false},
		{"echo", false},
		{"ls", false},
		{"git", false},
		{"golangci-lint", false},
	}

	for _, tt := range tests {
		t.Run(tt.command, func(t *testing.T) {
			result := requiresShellExecution(tt.command)
			if result != tt.expected {
				t.Errorf("requiresShellExecution(%q) = %v, want %v", tt.command, result, tt.expected)
			}
		})
	}
}

func TestBuildShellCommand(t *testing.T) {
	tests := []struct {
		name     string
		command  string
		args     []string
		expected string
	}{
		{
			name:     "simple command",
			command:  "bazel",
			args:     []string{"info"},
			expected: "bazel info",
		},
		{
			name:     "command with multiple args",
			command:  "bazel",
			args:     []string{"build", "//..."},
			expected: "bazel build //...",
		},
		{
			name:     "args with spaces",
			command:  "echo",
			args:     []string{"hello world", "test"},
			expected: "echo 'hello world' test",
		},
		{
			name:     "args with single quotes",
			command:  "echo",
			args:     []string{"don't", "test"},
			expected: "echo 'don'\"'\"'t' test",
		},
		{
			name:     "args with double quotes",
			command:  "echo",
			args:     []string{"hello \"world\"", "test"},
			expected: "echo 'hello \"world\"' test",
		},
		{
			name:     "no args",
			command:  "bazel",
			args:     []string{},
			expected: "bazel",
		},
		{
			name:     "complex bazel command",
			command:  "bazel",
			args:     []string{"query", "//devtools/...", "--output=label_kind"},
			expected: "bazel query //devtools/... --output=label_kind",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := buildShellCommand(tt.command, tt.args)
			if result != tt.expected {
				t.Errorf("buildShellCommand(%q, %v) = %q, want %q", tt.command, tt.args, result, tt.expected)
			}
		})
	}
}

func TestShellExecutionIntegration(t *testing.T) {
	executor := NewBasicExecutor()
	ctx := context.Background()

	// Test that shell execution works for basic commands
	cfg := ToolConfig{
		Command: "echo",
		Args:    []string{"hello", "world"},
	}

	result, err := executor.Execute(ctx, cfg)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
	}
	if result == nil {
		t.Fatal("Execute() returned nil result")
	}
	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0", result.ExitCode)
	}

	expectedOutput := "hello world\n"
	if result.Output != expectedOutput {
		t.Errorf("Output = %q, want %q", result.Output, expectedOutput)
	}
}

func TestBazelShellExecution(t *testing.T) {
	executor := NewBasicExecutor()
	ctx := context.Background()

	// Only run this test if bazel is available
	if !executor.IsAvailable("bazel") {
		t.Skip("bazel not available, skipping test")
	}

	// Test bazel help command (should work with shell execution)
	cfg := ToolConfig{
		Command: "bazel",
		Args:    []string{"help"},
	}

	result, err := executor.Execute(ctx, cfg)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
	}
	if result == nil {
		t.Fatal("Execute() returned nil result")
	}

	// Bazel help should return 0 and contain usage information
	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0. Stderr: %s", result.ExitCode, result.Stderr)
	}

	if !strings.Contains(result.Output, "Usage:") && !strings.Contains(result.Output, "bazel") {
		t.Errorf("Output doesn't contain expected help text, got: %q", result.Output)
	}
}
