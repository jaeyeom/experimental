package runner

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestNewMockExecutor(t *testing.T) {
	mock := NewMockExecutor()
	if mock == nil {
		t.Fatal("NewMockExecutor() returned nil")
	}
	if mock.AvailableCommands == nil {
		t.Error("AvailableCommands map not initialized")
	}
	if mock.CallHistory == nil {
		t.Error("CallHistory slice not initialized")
	}
	if mock.expectations == nil {
		t.Error("expectations slice not initialized")
	}
}

func TestMockExecutor_Execute_DefaultBehavior(t *testing.T) {
	mock := NewMockExecutor()
	ctx := context.Background()

	cfg := ToolConfig{
		Command: "echo",
		Args:    []string{"hello", "world"},
	}

	result, err := mock.Execute(ctx, cfg)
	if err != nil {
		t.Errorf("Execute() unexpected error = %v", err)
	}
	if result == nil {
		t.Fatal("Execute() returned nil result")
	}
	if result.Command != cfg.Command {
		t.Errorf("Command = %q, want %q", result.Command, cfg.Command)
	}
	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0", result.ExitCode)
	}
	if !strings.Contains(result.Output, "Mock execution of") {
		t.Errorf("Output doesn't contain expected mock message, got: %q", result.Output)
	}
}

func TestMockExecutor_Execute_WithExpectation(t *testing.T) {
	mock := NewMockExecutor()
	ctx := context.Background()

	// Set up expectation
	expectedOutput := "Hello from mock!"
	mock.ExpectCommand("echo").
		WillSucceed(expectedOutput, 0).
		Once().
		Build()

	cfg := ToolConfig{
		Command: "echo",
		Args:    []string{"test"},
	}

	result, err := mock.Execute(ctx, cfg)
	if err != nil {
		t.Errorf("Execute() unexpected error = %v", err)
	}
	if result == nil {
		t.Fatal("Execute() returned nil result")
	}
	if result.Output != expectedOutput {
		t.Errorf("Output = %q, want %q", result.Output, expectedOutput)
	}
	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0", result.ExitCode)
	}
}

func TestMockExecutor_Execute_WithArgsExpectation(t *testing.T) {
	mock := NewMockExecutor()
	ctx := context.Background()

	// Set up expectation for specific args
	mock.ExpectCommandWithArgs("go", "test", "./...").
		WillSucceed("PASS\nok github.com/example/pkg\n", 0).
		Build()

	// This should match
	cfg1 := ToolConfig{
		Command: "go",
		Args:    []string{"test", "./..."},
	}
	result1, err1 := mock.Execute(ctx, cfg1)
	if err1 != nil {
		t.Errorf("Execute() unexpected error = %v", err1)
	}
	if result1 == nil || !strings.Contains(result1.Output, "PASS") {
		t.Error("Expected successful match for correct args")
	}

	// This should not match (different args)
	cfg2 := ToolConfig{
		Command: "go",
		Args:    []string{"build", "./..."},
	}
	result2, err2 := mock.Execute(ctx, cfg2)
	if err2 != nil {
		t.Errorf("Execute() unexpected error = %v", err2)
	}
	if result2 == nil || strings.Contains(result2.Output, "PASS") {
		t.Error("Expected default behavior for non-matching args")
	}
}

func TestMockExecutor_Execute_MultipleExpectations(t *testing.T) {
	mock := NewMockExecutor()
	ctx := context.Background()

	// Set up multiple expectations
	mock.ExpectCommand("echo").
		WillSucceed("First call", 0).
		Once().
		Build()

	mock.ExpectCommand("echo").
		WillSucceed("Second call", 0).
		Once().
		Build()

	cfg := ToolConfig{Command: "echo"}

	// First call should match first expectation
	result1, _ := mock.Execute(ctx, cfg)
	if result1 == nil || result1.Output != "First call" {
		t.Errorf("First call output = %q, want %q", result1.Output, "First call")
	}

	// Second call should match second expectation
	result2, _ := mock.Execute(ctx, cfg)
	if result2 == nil || result2.Output != "Second call" {
		t.Errorf("Second call output = %q, want %q", result2.Output, "Second call")
	}

	// Third call should get default behavior
	result3, _ := mock.Execute(ctx, cfg)
	if result3 == nil || !strings.Contains(result3.Output, "Mock execution") {
		t.Error("Third call should get default behavior")
	}
}

func TestMockExecutor_Execute_FailureScenarios(t *testing.T) {
	t.Run("WillFail", func(t *testing.T) {
		mock := NewMockExecutor()
		ctx := context.Background()

		mock.ExpectCommand("failing-cmd").
			WillFail("command failed", 1).
			Build()

		cfg := ToolConfig{Command: "failing-cmd"}
		result, err := mock.Execute(ctx, cfg)
		if err != nil {
			t.Errorf("Execute() unexpected error = %v", err)
		}
		if result == nil {
			t.Fatal("Execute() returned nil result")
		}
		if result.Stderr != "command failed" {
			t.Errorf("Stderr = %q, want %q", result.Stderr, "command failed")
		}
		if result.ExitCode != 1 {
			t.Errorf("ExitCode = %d, want 1", result.ExitCode)
		}
	})

	t.Run("WillTimeout", func(t *testing.T) {
		mock := NewMockExecutor()
		ctx := context.Background()

		mock.ExpectCommand("slow-cmd").
			WillTimeout(5 * time.Second).
			Build()

		cfg := ToolConfig{Command: "slow-cmd"}
		result, err := mock.Execute(ctx, cfg)

		if result != nil {
			t.Errorf("Execute() result = %v, want nil", result)
		}
		var timeoutErr *TimeoutError
		if !errors.As(err, &timeoutErr) {
			t.Errorf("Expected TimeoutError, got %T: %v", err, err)
		}
	})

	t.Run("WillError", func(t *testing.T) {
		mock := NewMockExecutor()
		ctx := context.Background()

		customErr := errors.New("custom error")
		mock.ExpectCommand("error-cmd").
			WillError(customErr).
			Build()

		cfg := ToolConfig{Command: "error-cmd"}
		result, err := mock.Execute(ctx, cfg)

		if result != nil {
			t.Errorf("Execute() result = %v, want nil", result)
		}
		if err != customErr {
			t.Errorf("Execute() error = %v, want %v", err, customErr)
		}
	})
}

func TestMockExecutor_Execute_CustomMatcher(t *testing.T) {
	mock := NewMockExecutor()
	ctx := context.Background()

	// Custom matcher that checks working directory
	mock.ExpectCustom(func(_ context.Context, cfg ToolConfig) bool {
		return cfg.WorkingDir == "/tmp"
	}).WillSucceed("Matched working dir", 0).Build()

	// Should match
	cfg1 := ToolConfig{
		Command:    "ls",
		WorkingDir: "/tmp",
	}
	result1, _ := mock.Execute(ctx, cfg1)
	if result1 == nil || result1.Output != "Matched working dir" {
		t.Error("Expected custom matcher to match")
	}

	// Should not match
	cfg2 := ToolConfig{
		Command:    "ls",
		WorkingDir: "/home",
	}
	result2, _ := mock.Execute(ctx, cfg2)
	if result2 == nil || result2.Output == "Matched working dir" {
		t.Error("Expected custom matcher not to match")
	}
}

func TestMockExecutor_IsAvailable(t *testing.T) {
	mock := NewMockExecutor()

	// Initially, no commands are available
	if mock.IsAvailable("echo") {
		t.Error("Expected echo to be unavailable initially")
	}

	// Set command as available
	mock.SetAvailableCommand("echo", true)
	if !mock.IsAvailable("echo") {
		t.Error("Expected echo to be available after setting")
	}

	// Set command as unavailable
	mock.SetAvailableCommand("echo", false)
	if mock.IsAvailable("echo") {
		t.Error("Expected echo to be unavailable after setting to false")
	}
}

func TestMockExecutor_CallHistory(t *testing.T) {
	mock := NewMockExecutor()
	ctx := context.Background()

	// Make some calls
	cfg1 := ToolConfig{Command: "echo", Args: []string{"first"}}
	cfg2 := ToolConfig{Command: "ls", Args: []string{"-la"}}

	_, _ = mock.Execute(ctx, cfg1)
	_, _ = mock.Execute(ctx, cfg2)

	// Check call history
	history := mock.GetCallHistory()
	if len(history) != 2 {
		t.Fatalf("CallHistory length = %d, want 2", len(history))
	}

	if history[0].Config.Command != "echo" {
		t.Errorf("First call command = %q, want %q", history[0].Config.Command, "echo")
	}
	if history[1].Config.Command != "ls" {
		t.Errorf("Second call command = %q, want %q", history[1].Config.Command, "ls")
	}

	// Clear history
	mock.ClearCallHistory()
	history = mock.GetCallHistory()
	if len(history) != 0 {
		t.Errorf("CallHistory length after clear = %d, want 0", len(history))
	}
}

func TestMockExecutor_SetDefaultBehavior(t *testing.T) {
	mock := NewMockExecutor()
	ctx := context.Background()

	// Set custom default behavior
	defaultResult := &config.ExecutionResult{
		Output:   "Custom default output",
		ExitCode: 42,
	}
	mock.SetDefaultBehavior(defaultResult, nil)

	cfg := ToolConfig{Command: "any-command"}
	result, err := mock.Execute(ctx, cfg)
	if err != nil {
		t.Errorf("Execute() unexpected error = %v", err)
	}
	if result == nil {
		t.Fatal("Execute() returned nil result")
	}
	if result.Output != "Custom default output" {
		t.Errorf("Output = %q, want %q", result.Output, "Custom default output")
	}
	if result.ExitCode != 42 {
		t.Errorf("ExitCode = %d, want 42", result.ExitCode)
	}
}

func TestMockExecutor_AssertExpectationsMet(t *testing.T) {
	t.Run("All expectations met", func(t *testing.T) {
		mock := NewMockExecutor()
		ctx := context.Background()

		// Set expectation that should be called exactly twice
		mock.ExpectCommand("echo").
			WillSucceed("ok", 0).
			Times(2).
			Build()

		// Call it twice
		cfg := ToolConfig{Command: "echo"}
		_, _ = mock.Execute(ctx, cfg)
		_, _ = mock.Execute(ctx, cfg)

		// Should not error
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Errorf("AssertExpectationsMet() error = %v", err)
		}
	})

	t.Run("Expectation not met", func(t *testing.T) {
		mock := NewMockExecutor()
		ctx := context.Background()

		// Set expectation that should be called twice
		mock.ExpectCommand("echo").
			WillSucceed("ok", 0).
			Times(2).
			Build()

		// Call it only once
		cfg := ToolConfig{Command: "echo"}
		_, _ = mock.Execute(ctx, cfg)

		// Should error
		if err := mock.AssertExpectationsMet(); err == nil {
			t.Error("AssertExpectationsMet() expected error but got nil")
		}
	})

	t.Run("Unlimited expectations don't need to be met", func(t *testing.T) {
		mock := NewMockExecutor()

		// Set expectation with unlimited times (Times = 0)
		mock.ExpectCommand("echo").
			WillSucceed("ok", 0).
			Build()

		// Don't call it at all
		// Should not error
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Errorf("AssertExpectationsMet() error = %v", err)
		}
	})
}

func TestMockExecutor_RealWorldScenario(t *testing.T) {
	// Simulate a real test scenario where we're testing code that runs multiple tools
	mock := NewMockExecutor()
	ctx := context.Background()

	// Set up expectations for a typical CI workflow
	mock.SetAvailableCommand("go", true)
	mock.SetAvailableCommand("golangci-lint", true)
	mock.SetAvailableCommand("make", false) // Not available

	// Expect go mod download
	mock.ExpectCommandWithArgs("go", "mod", "download").
		WillSucceed("", 0).
		Once().
		Build()

	// Expect go test with coverage
	mock.ExpectCommandWithArgs("go", "test", "-cover", "./...").
		WillSucceed("PASS\ncoverage: 85.3% of statements\n", 0).
		Once().
		Build()

	// Expect golangci-lint
	mock.ExpectCommand("golangci-lint").
		WillSucceed("", 0).
		Once().
		Build()

	// Simulate the workflow
	// 1. Download dependencies
	result1, err1 := mock.Execute(ctx, ToolConfig{
		Command: "go",
		Args:    []string{"mod", "download"},
	})
	if err1 != nil || result1.ExitCode != 0 {
		t.Error("go mod download failed")
	}

	// 2. Run tests
	result2, err2 := mock.Execute(ctx, ToolConfig{
		Command: "go",
		Args:    []string{"test", "-cover", "./..."},
	})
	if err2 != nil || !strings.Contains(result2.Output, "coverage") {
		t.Error("go test failed or missing coverage")
	}

	// 3. Run linter
	result3, err3 := mock.Execute(ctx, ToolConfig{
		Command: "golangci-lint",
		Args:    []string{"run"},
	})
	if err3 != nil || result3.ExitCode != 0 {
		t.Error("golangci-lint failed")
	}

	// Verify all expectations were met
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Errorf("Not all expectations were met: %v", err)
	}

	// Verify call history
	history := mock.GetCallHistory()
	if len(history) != 3 {
		t.Errorf("Expected 3 calls in history, got %d", len(history))
	}
}
