package runner

import (
	"strings"
	"testing"
	"time"
)

func TestToolConfig_Validate(t *testing.T) {
	tests := []struct {
		name    string
		config  ToolConfig
		wantErr bool
		errMsg  string
	}{
		{
			name: "valid config",
			config: ToolConfig{
				Command:    "go",
				Args:       []string{"test", "./..."},
				WorkingDir: "/test/dir",
				Timeout:    30 * time.Second,
				MaxRetries: 3,
				RetryDelay: 1 * time.Second,
				Env:        map[string]string{"GO_ENV": "test"},
			},
			wantErr: false,
		},
		{
			name: "minimal valid config",
			config: ToolConfig{
				Command: "ls",
			},
			wantErr: false,
		},
		{
			name:    "empty command",
			config:  ToolConfig{},
			wantErr: true,
			errMsg:  "command cannot be empty",
		},
		{
			name: "negative max retries",
			config: ToolConfig{
				Command:    "go",
				MaxRetries: -1,
			},
			wantErr: true,
			errMsg:  "maxRetries cannot be negative",
		},
		{
			name: "negative retry delay",
			config: ToolConfig{
				Command:    "go",
				RetryDelay: -1 * time.Second,
			},
			wantErr: true,
			errMsg:  "retryDelay cannot be negative",
		},
		{
			name: "negative timeout",
			config: ToolConfig{
				Command: "go",
				Timeout: -1 * time.Second,
			},
			wantErr: true,
			errMsg:  "timeout cannot be negative",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.config.Validate()
			if (err != nil) != tt.wantErr {
				t.Errorf("Validate() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("Validate() error = %v, want error containing %v", err, tt.errMsg)
			}
		})
	}
}

func TestValidationError(t *testing.T) {
	err := &ValidationError{
		Field:   "Command",
		Message: "command cannot be empty",
	}

	expected := "validation error in field 'Command': command cannot be empty"
	if err.Error() != expected {
		t.Errorf("ValidationError.Error() = %v, want %v", err.Error(), expected)
	}
}

func TestTimeoutError(t *testing.T) {
	err := &TimeoutError{
		Command: "go test",
		Timeout: 30 * time.Second,
	}

	expected := "command 'go test' timed out after 30s"
	if err.Error() != expected {
		t.Errorf("TimeoutError.Error() = %v, want %v", err.Error(), expected)
	}
}

func TestExecutableNotFoundError(t *testing.T) {
	err := &ExecutableNotFoundError{
		Command: "nonexistent-tool",
	}

	expected := "executable not found: nonexistent-tool"
	if err.Error() != expected {
		t.Errorf("ExecutableNotFoundError.Error() = %v, want %v", err.Error(), expected)
	}
}

func TestRetryExhaustedError(t *testing.T) {
	lastErr := &ValidationError{Field: "test", Message: "test error"}
	err := &RetryExhaustedError{
		Command:   "flaky-tool",
		Attempts:  3,
		LastError: lastErr,
	}

	// Check that error message contains expected components
	errMsg := err.Error()
	if !strings.Contains(errMsg, "retry exhausted") {
		t.Errorf("RetryExhaustedError.Error() should contain 'retry exhausted', got %v", errMsg)
	}
	if !strings.Contains(errMsg, "flaky-tool") {
		t.Errorf("RetryExhaustedError.Error() should contain command name, got %v", errMsg)
	}

	// Test Unwrap method
	if err.Unwrap() != lastErr {
		t.Errorf("RetryExhaustedError.Unwrap() = %v, want %v", err.Unwrap(), lastErr)
	}
}

func TestToolConfig_Fields(t *testing.T) {
	config := ToolConfig{
		Command:    "go",
		Args:       []string{"test", "-v"},
		WorkingDir: "/project",
		Timeout:    1 * time.Minute,
		MaxRetries: 2,
		RetryDelay: 5 * time.Second,
		Env:        map[string]string{"GOOS": "linux"},
	}

	// Verify all fields are accessible
	if config.Command != "go" {
		t.Errorf("Command = %v, want go", config.Command)
	}
	if len(config.Args) != 2 || config.Args[0] != "test" || config.Args[1] != "-v" {
		t.Errorf("Args = %v, want [test -v]", config.Args)
	}
	if config.WorkingDir != "/project" {
		t.Errorf("WorkingDir = %v, want /project", config.WorkingDir)
	}
	if config.Timeout != 1*time.Minute {
		t.Errorf("Timeout = %v, want 1m0s", config.Timeout)
	}
	if config.MaxRetries != 2 {
		t.Errorf("MaxRetries = %v, want 2", config.MaxRetries)
	}
	if config.RetryDelay != 5*time.Second {
		t.Errorf("RetryDelay = %v, want 5s", config.RetryDelay)
	}
	if config.Env["GOOS"] != "linux" {
		t.Errorf("Env[GOOS] = %v, want linux", config.Env["GOOS"])
	}
}
