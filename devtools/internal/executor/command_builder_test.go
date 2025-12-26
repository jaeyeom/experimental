package executor

import (
	"context"
	"strings"
	"testing"
)

func TestDirectCommandBuilder(t *testing.T) {
	builder := &DirectCommandBuilder{}
	ctx := context.Background()

	tests := []struct {
		name    string
		command string
		args    []string
	}{
		{"simple command", "echo", []string{"hello"}},
		{"multiple args", "git", []string{"status", "--short"}},
		{"no args", "ls", []string{}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := builder.Build(ctx, tt.command, tt.args)
			if cmd == nil {
				t.Fatal("Build() returned nil")
				return
			}
			if cmd.Path != tt.command && !strings.Contains(cmd.Path, tt.command) {
				t.Errorf("Command path = %q, want to contain %q", cmd.Path, tt.command)
			}
		})
	}
}

func TestShellCommandBuilder(t *testing.T) {
	builder := &ShellCommandBuilder{}
	ctx := context.Background()

	tests := []struct {
		name    string
		command string
		args    []string
	}{
		{"simple command", "echo", []string{"hello"}},
		{"bazel command", "bazel", []string{"info", "workspace"}},
		{"args with spaces", "echo", []string{"hello world"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := builder.Build(ctx, tt.command, tt.args)
			if cmd == nil {
				t.Fatal("Build() returned nil")
				return
			}
			// Should be using sh -c
			if !strings.Contains(cmd.Path, "sh") {
				t.Errorf("Command path = %q, want to contain 'sh'", cmd.Path)
			}
			if len(cmd.Args) < 2 || cmd.Args[1] != "-c" {
				t.Errorf("Command args = %v, want to contain '-c'", cmd.Args)
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
			expected: "'bazel' 'info'",
		},
		{
			name:     "command with multiple args",
			command:  "bazel",
			args:     []string{"build", "//..."},
			expected: "'bazel' 'build' '//...'",
		},
		{
			name:     "args with spaces",
			command:  "echo",
			args:     []string{"hello world", "test"},
			expected: "'echo' 'hello world' 'test'",
		},
		{
			name:     "args with single quotes",
			command:  "echo",
			args:     []string{"don't", "test"},
			expected: "'echo' 'don'\"'\"'t' 'test'",
		},
		{
			name:     "args with double quotes",
			command:  "echo",
			args:     []string{"hello \"world\"", "test"},
			expected: "'echo' 'hello \"world\"' 'test'",
		},
		{
			name:     "no args",
			command:  "bazel",
			args:     []string{},
			expected: "'bazel'",
		},
		{
			name:     "complex bazel command",
			command:  "bazel",
			args:     []string{"query", "//devtools/...", "--output=label_kind"},
			expected: "'bazel' 'query' '//devtools/...' '--output=label_kind'",
		},
		// Security tests - shell injection prevention
		{
			name:     "command injection via semicolon",
			command:  "bazel",
			args:     []string{"query", "//...;echo injected"},
			expected: "'bazel' 'query' '//...;echo injected'",
		},
		{
			name:     "command injection via pipe",
			command:  "echo",
			args:     []string{"data | cat /dev/null"},
			expected: "'echo' 'data | cat /dev/null'",
		},
		{
			name:     "command injection via backticks",
			command:  "echo",
			args:     []string{"`whoami`"},
			expected: "'echo' '`whoami`'",
		},
		{
			name:     "command injection via dollar sign",
			command:  "echo",
			args:     []string{"$(whoami)"},
			expected: "'echo' '$(whoami)'",
		},
		{
			name:     "command injection via ampersand",
			command:  "bazel",
			args:     []string{"build", "//... & echo background"},
			expected: "'bazel' 'build' '//... & echo background'",
		},
		{
			name:     "command injection via redirect",
			command:  "echo",
			args:     []string{"secret > /tmp/test_redirect"},
			expected: "'echo' 'secret > /tmp/test_redirect'",
		},
		{
			name:     "command injection via newline",
			command:  "echo",
			args:     []string{"foo\necho injected"},
			expected: "'echo' 'foo\necho injected'",
		},
		{
			name:     "malicious command name",
			command:  "bazel; echo malicious",
			args:     []string{"info"},
			expected: "'bazel; echo malicious' 'info'",
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

func TestShellQuote(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{"simple string", "hello", "'hello'"},
		{"string with spaces", "hello world", "'hello world'"},
		{"string with single quote", "don't", "'don'\"'\"'t'"},
		{"string with double quote", "hello \"world\"", "'hello \"world\"'"},
		{"semicolon (injection attempt)", "foo;echo bad", "'foo;echo bad'"},
		{"pipe (injection attempt)", "foo | cat /dev/null", "'foo | cat /dev/null'"},
		{"backticks (injection attempt)", "`whoami`", "'`whoami`'"},
		{"dollar sign (injection attempt)", "$(whoami)", "'$(whoami)'"},
		{"ampersand (injection attempt)", "foo & bar", "'foo & bar'"},
		{"redirect (injection attempt)", "foo > /tmp/test", "'foo > /tmp/test'"},
		{"newline (injection attempt)", "foo\nbar", "'foo\nbar'"},
		{"multiple single quotes", "it's a'test", "'it'\"'\"'s a'\"'\"'test'"},
		{"empty string", "", "''"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := shellQuote(tt.input)
			if result != tt.expected {
				t.Errorf("shellQuote(%q) = %q, want %q", tt.input, result, tt.expected)
			}
		})
	}
}

func TestDirectCommandBuilderIntegration(t *testing.T) {
	executor := NewBasicExecutor()
	ctx := context.Background()

	// Test direct execution (default)
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
		return
	}
	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0", result.ExitCode)
	}

	expectedOutput := "hello world\n"
	if result.Output != expectedOutput {
		t.Errorf("Output = %q, want %q", result.Output, expectedOutput)
	}
}

func TestShellCommandBuilderIntegration(t *testing.T) {
	executor := NewBasicExecutor()
	ctx := context.Background()

	// Test shell execution
	cfg := ToolConfig{
		Command:        "echo",
		Args:           []string{"hello", "world"},
		CommandBuilder: &ShellCommandBuilder{},
	}

	result, err := executor.Execute(ctx, cfg)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
	}
	if result == nil {
		t.Fatal("Execute() returned nil result")
		return
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

	// Test bazel help command with shell execution
	cfg := ToolConfig{
		Command: "bazel",
		Args: []string{
			"help",
			// Workaround for Termux: System network usage
			// collection causes crash
			"--noexperimental_collect_system_network_usage",
		},
		CommandBuilder: &ShellCommandBuilder{},
	}

	result, err := executor.Execute(ctx, cfg)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
	}
	if result == nil {
		t.Fatal("Execute() returned nil result")
		return
	}

	// Bazel help should return 0 and contain usage information
	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0. Stderr: %s", result.ExitCode, result.Stderr)
	}

	if !strings.Contains(result.Output, "Usage:") && !strings.Contains(result.Output, "bazel") {
		t.Errorf("Output doesn't contain expected help text, got: %q", result.Output)
	}
}

// TestShellInjectionPrevention verifies that shell metacharacters are properly escaped.
func TestShellInjectionPrevention(t *testing.T) {
	executor := NewBasicExecutor()
	ctx := context.Background()

	tests := []struct {
		name           string
		args           []string
		expectedOutput string // What echo should literally output
	}{
		{
			name:           "semicolon is literal",
			args:           []string{"foo;whoami"},
			expectedOutput: "foo;whoami\n",
		},
		{
			name:           "pipe is literal",
			args:           []string{"secret|cat"},
			expectedOutput: "secret|cat\n",
		},
		{
			name:           "backtick is literal",
			args:           []string{"`whoami`"},
			expectedOutput: "`whoami`\n",
		},
		{
			name:           "dollar sign is literal",
			args:           []string{"$PATH"},
			expectedOutput: "$PATH\n",
		},
		{
			name:           "ampersand is literal",
			args:           []string{"foo&bar"},
			expectedOutput: "foo&bar\n",
		},
		{
			name:           "redirect is literal",
			args:           []string{"foo>bar"},
			expectedOutput: "foo>bar\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cfg := ToolConfig{
				Command:        "echo",
				Args:           tt.args,
				CommandBuilder: &ShellCommandBuilder{},
			}

			result, err := executor.Execute(ctx, cfg)
			if err != nil {
				t.Fatalf("Execute() error = %v", err)
			}
			if result == nil {
				t.Fatal("Execute() returned nil result")
				return
			}

			// The output should be the literal string, not the result of command injection
			if result.Output != tt.expectedOutput {
				t.Errorf("Output = %q, want %q (injection may have occurred!)", result.Output, tt.expectedOutput)
			}
		})
	}
}
