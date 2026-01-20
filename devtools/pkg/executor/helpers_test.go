package executor

import (
	"context"
	"testing"
	"time"
)

func TestOutputJSON(t *testing.T) {
	mock := NewMockExecutor()

	// Set up mock to return JSON
	mock.ExpectCommand("test-cmd").WillSucceed(`{"name":"test","value":42}`, 0).Build()

	type TestData struct {
		Name  string `json:"name"`
		Value int    `json:"value"`
	}

	result, err := OutputJSON[TestData](context.Background(), mock, "test-cmd")
	if err != nil {
		t.Fatalf("OutputJSON failed: %v", err)
	}

	if result.Name != "test" {
		t.Errorf("expected Name='test', got %q", result.Name)
	}
	if result.Value != 42 {
		t.Errorf("expected Value=42, got %d", result.Value)
	}
}

func TestOutputJSONInvalidJSON(t *testing.T) {
	mock := NewMockExecutor()

	// Set up mock to return invalid JSON
	mock.ExpectCommand("test-cmd").WillSucceed(`not json`, 0).Build()

	type TestData struct {
		Name string `json:"name"`
	}

	_, err := OutputJSON[TestData](context.Background(), mock, "test-cmd")
	if err == nil {
		t.Fatal("expected error for invalid JSON, got nil")
	}
}

func TestOutput(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("echo").WillSucceed("hello world", 0).Build()

	output, err := Output(context.Background(), mock, "echo")
	if err != nil {
		t.Fatalf("Output failed: %v", err)
	}

	if string(output) != "hello world" {
		t.Errorf("expected 'hello world', got %q", string(output))
	}
}

func TestOutputFailure(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("failing-cmd").WillFail("command failed", 1).Build()

	_, err := Output(context.Background(), mock, "failing-cmd")
	if err == nil {
		t.Fatal("expected error for failed command, got nil")
	}

	exitErr, ok := err.(*ExitError)
	if !ok {
		t.Fatalf("expected *ExitError, got %T", err)
	}
	if exitErr.ExitCode != 1 {
		t.Errorf("expected exit code 1, got %d", exitErr.ExitCode)
	}
}

func TestRun(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("test-cmd").WillSucceed("", 0).Build()

	err := Run(context.Background(), mock, "test-cmd")
	if err != nil {
		t.Fatalf("Run failed: %v", err)
	}
}

func TestCombinedOutput(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("test-cmd").WillReturn(&ExecutionResult{
		Output:    "stdout",
		Stderr:    "stderr",
		ExitCode:  0,
		StartTime: time.Now(),
		EndTime:   time.Now(),
	}, nil).Build()

	output, err := CombinedOutput(context.Background(), mock, "test-cmd")
	if err != nil {
		t.Fatalf("CombinedOutput failed: %v", err)
	}

	// Should contain both stdout and stderr
	outputStr := string(output)
	if outputStr != "stdout\nstderr" {
		t.Errorf("expected 'stdout\\nstderr', got %q", outputStr)
	}
}

func TestExitErrorMessage(t *testing.T) {
	tests := []struct {
		name     string
		err      ExitError
		expected string
	}{
		{
			name:     "with stderr",
			err:      ExitError{ExitCode: 1, Stderr: "error message"},
			expected: "exit status 1: error message",
		},
		{
			name:     "without stderr",
			err:      ExitError{ExitCode: 127, Stderr: ""},
			expected: "exit status 127",
		},
		{
			name:     "long stderr truncated",
			err:      ExitError{ExitCode: 1, Stderr: string(make([]byte, 300))},
			expected: "exit status 1: " + string(make([]byte, 200)) + "...",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.err.Error(); got != tt.expected {
				t.Errorf("Error() = %q, want %q", got, tt.expected)
			}
		})
	}
}
