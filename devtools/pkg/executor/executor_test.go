package executor

import (
	"context"
	"testing"
	"time"
)

func TestBasicExecutorSimpleCommand(t *testing.T) {
	exec := NewBasicExecutor()

	result, err := exec.Execute(context.Background(), ToolConfig{
		Command: "echo",
		Args:    []string{"hello"},
	})
	if err != nil {
		t.Fatalf("Execute failed: %v", err)
	}

	if result.ExitCode != 0 {
		t.Errorf("expected exit code 0, got %d", result.ExitCode)
	}

	// echo adds a newline
	expected := "hello\n"
	if result.Output != expected {
		t.Errorf("expected output %q, got %q", expected, result.Output)
	}

	// Verify basic result fields are populated
	if result.Command != "echo" {
		t.Errorf("expected command 'echo', got %q", result.Command)
	}
	if len(result.Args) != 1 || result.Args[0] != "hello" {
		t.Errorf("expected args ['hello'], got %v", result.Args)
	}
	if result.StartTime.IsZero() {
		t.Error("StartTime should be set")
	}
	if result.EndTime.IsZero() {
		t.Error("EndTime should be set")
	}
}

func TestBasicExecutorTimeout(t *testing.T) {
	exec := NewBasicExecutor()

	_, err := exec.Execute(context.Background(), ToolConfig{
		Command: "sleep",
		Args:    []string{"10"},
		Timeout: 100 * time.Millisecond,
	})

	if err == nil {
		t.Fatal("expected timeout error, got nil")
	}

	timeoutErr, ok := err.(*TimeoutError)
	if !ok {
		t.Errorf("expected *TimeoutError, got %T: %v", err, err)
	}

	// Verify timeout error contains correct information
	if timeoutErr.Command != "sleep 10" {
		t.Errorf("expected command 'sleep 10', got %q", timeoutErr.Command)
	}
	if timeoutErr.Timeout != 100*time.Millisecond {
		t.Errorf("expected timeout 100ms, got %v", timeoutErr.Timeout)
	}
}

func TestBasicExecutorIsAvailable(t *testing.T) {
	exec := NewBasicExecutor()

	tests := []struct {
		name      string
		command   string
		available bool
	}{
		{
			name:      "available command - echo",
			command:   "echo",
			available: true,
		},
		{
			name:      "available command - ls",
			command:   "ls",
			available: true,
		},
		{
			name:      "unavailable command",
			command:   "nonexistent-command-12345",
			available: false,
		},
		{
			name:      "unavailable command with special chars",
			command:   "invalid/command/path",
			available: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := exec.IsAvailable(tt.command)
			if got != tt.available {
				t.Errorf("IsAvailable(%q) = %v, want %v", tt.command, got, tt.available)
			}
		})
	}
}

func TestToolConfigValidation(t *testing.T) {
	tests := []struct {
		name         string
		cfg          ToolConfig
		wantErr      bool
		errFieldName string // expected field name in ValidationError
	}{
		{
			name:    "valid config - basic",
			cfg:     ToolConfig{Command: "echo"},
			wantErr: false,
		},
		{
			name:    "valid config - with args",
			cfg:     ToolConfig{Command: "echo", Args: []string{"hello", "world"}},
			wantErr: false,
		},
		{
			name:    "valid config - with timeout",
			cfg:     ToolConfig{Command: "echo", Timeout: 5 * time.Second},
			wantErr: false,
		},
		{
			name:    "valid config - with zero timeout",
			cfg:     ToolConfig{Command: "echo", Timeout: 0},
			wantErr: false,
		},
		{
			name:    "valid config - with retries",
			cfg:     ToolConfig{Command: "echo", MaxRetries: 3, RetryDelay: time.Second},
			wantErr: false,
		},
		{
			name:         "invalid - empty command",
			cfg:          ToolConfig{Command: ""},
			wantErr:      true,
			errFieldName: "Command",
		},
		{
			name:         "invalid - negative timeout",
			cfg:          ToolConfig{Command: "echo", Timeout: -1 * time.Second},
			wantErr:      true,
			errFieldName: "Timeout",
		},
		{
			name:         "invalid - negative max retries",
			cfg:          ToolConfig{Command: "echo", MaxRetries: -1},
			wantErr:      true,
			errFieldName: "MaxRetries",
		},
		{
			name:         "invalid - negative retry delay",
			cfg:          ToolConfig{Command: "echo", RetryDelay: -1 * time.Second},
			wantErr:      true,
			errFieldName: "RetryDelay",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.cfg.Validate()
			if (err != nil) != tt.wantErr {
				t.Errorf("Validate() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				// Verify it's a ValidationError with correct field
				validationErr, ok := err.(*ValidationError)
				if !ok {
					t.Errorf("expected *ValidationError, got %T", err)
					return
				}
				if validationErr.Field != tt.errFieldName {
					t.Errorf("expected error field %q, got %q", tt.errFieldName, validationErr.Field)
				}
			}
		})
	}
}

func TestDirectCommandBuilder(t *testing.T) {
	builder := &DirectCommandBuilder{}
	ctx := context.Background()

	tests := []struct {
		name            string
		command         string
		args            []string
		expectedArgsLen int
	}{
		{
			name:            "command with args",
			command:         "echo",
			args:            []string{"hello", "world"},
			expectedArgsLen: 3, // command + 2 args
		},
		{
			name:            "command without args",
			command:         "ls",
			args:            []string{},
			expectedArgsLen: 1, // just command
		},
		{
			name:            "command with special characters in args",
			command:         "echo",
			args:            []string{"hello world", "$VAR", ";ls"},
			expectedArgsLen: 4, // command + 3 args
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := builder.Build(ctx, tt.command, tt.args)

			// Verify the command is created
			if cmd == nil {
				t.Fatal("Build() returned nil")
			}

			// Verify Path is set (resolved executable path)
			if cmd.Path == "" {
				t.Error("cmd.Path should not be empty")
			}

			// Verify Args contains command and arguments (no shell wrapper)
			if len(cmd.Args) != tt.expectedArgsLen {
				t.Errorf("expected %d args, got %d: %v", tt.expectedArgsLen, len(cmd.Args), cmd.Args)
			}

			// Verify first arg is the command name (not "sh" or similar)
			if len(cmd.Args) > 0 && cmd.Args[0] != tt.command {
				t.Errorf("expected first arg to be %q, got %q", tt.command, cmd.Args[0])
			}

			// Verify remaining args match input
			for i, arg := range tt.args {
				if cmd.Args[i+1] != arg {
					t.Errorf("arg[%d]: expected %q, got %q", i, arg, cmd.Args[i+1])
				}
			}
		})
	}
}

func TestShellCommandBuilder(t *testing.T) {
	builder := &ShellCommandBuilder{}
	cmd := builder.Build(context.Background(), "echo", []string{"hello", "world"})

	// Should use sh -c
	if cmd.Args[0] != "sh" {
		t.Errorf("expected sh, got %s", cmd.Args[0])
	}
	if cmd.Args[1] != "-c" {
		t.Errorf("expected -c, got %s", cmd.Args[1])
	}
}
