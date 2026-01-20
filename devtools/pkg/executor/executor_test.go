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

	_, ok := err.(*TimeoutError)
	if !ok {
		t.Errorf("expected *TimeoutError, got %T: %v", err, err)
	}
}

func TestBasicExecutorIsAvailable(t *testing.T) {
	exec := NewBasicExecutor()

	// echo should be available on all systems
	if !exec.IsAvailable("echo") {
		t.Error("echo should be available")
	}

	// nonexistent command
	if exec.IsAvailable("nonexistent-command-12345") {
		t.Error("nonexistent command should not be available")
	}
}

func TestToolConfigValidation(t *testing.T) {
	tests := []struct {
		name    string
		cfg     ToolConfig
		wantErr bool
	}{
		{
			name:    "valid config",
			cfg:     ToolConfig{Command: "echo"},
			wantErr: false,
		},
		{
			name:    "empty command",
			cfg:     ToolConfig{Command: ""},
			wantErr: true,
		},
		{
			name:    "negative timeout",
			cfg:     ToolConfig{Command: "echo", Timeout: -1},
			wantErr: true,
		},
		{
			name:    "negative max retries",
			cfg:     ToolConfig{Command: "echo", MaxRetries: -1},
			wantErr: true,
		},
		{
			name:    "negative retry delay",
			cfg:     ToolConfig{Command: "echo", RetryDelay: -1},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.cfg.Validate()
			if (err != nil) != tt.wantErr {
				t.Errorf("Validate() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestDirectCommandBuilder(t *testing.T) {
	builder := &DirectCommandBuilder{}
	cmd := builder.Build(context.Background(), "echo", []string{"hello", "world"})

	if cmd.Path == "" {
		t.Error("cmd.Path should not be empty")
	}

	// Args should contain the command and arguments
	if len(cmd.Args) != 3 {
		t.Errorf("expected 3 args, got %d", len(cmd.Args))
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
