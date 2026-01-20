package executor

import (
	"context"
	"testing"
	"time"
)

func TestMockExecutorExpectCommand(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("test").WillSucceed("test output", 0).Build()

	result, err := mock.Execute(context.Background(), ToolConfig{Command: "test"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.Output != "test output" {
		t.Errorf("expected 'test output', got %q", result.Output)
	}
}

func TestMockExecutorExpectCommandWithArgs(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommandWithArgs("git", "status", "-s").WillSucceed("M file.go", 0).Build()

	// Matching args
	result, err := mock.Execute(context.Background(), ToolConfig{
		Command: "git",
		Args:    []string{"status", "-s"},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Output != "M file.go" {
		t.Errorf("expected 'M file.go', got %q", result.Output)
	}
}

func TestMockExecutorCallHistory(t *testing.T) {
	mock := NewMockExecutor()

	_, _ = mock.Execute(context.Background(), ToolConfig{Command: "cmd1"})
	_, _ = mock.Execute(context.Background(), ToolConfig{Command: "cmd2", Args: []string{"arg1"}})

	history := mock.GetCallHistory()
	if len(history) != 2 {
		t.Fatalf("expected 2 calls, got %d", len(history))
	}

	if history[0].Config.Command != "cmd1" {
		t.Errorf("expected cmd1, got %s", history[0].Config.Command)
	}
	if history[1].Config.Command != "cmd2" {
		t.Errorf("expected cmd2, got %s", history[1].Config.Command)
	}
}

func TestMockExecutorSetAvailableCommand(t *testing.T) {
	mock := NewMockExecutor()

	if mock.IsAvailable("mycommand") {
		t.Error("command should not be available initially")
	}

	mock.SetAvailableCommand("mycommand", true)
	if !mock.IsAvailable("mycommand") {
		t.Error("command should be available after SetAvailableCommand")
	}

	mock.SetAvailableCommand("mycommand", false)
	if mock.IsAvailable("mycommand") {
		t.Error("command should not be available after SetAvailableCommand(false)")
	}
}

func TestMockExecutorTimes(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("limited").WillSucceed("first", 0).Times(1).Build()
	mock.SetDefaultBehavior(&ExecutionResult{
		Output:    "default",
		StartTime: time.Now(),
		EndTime:   time.Now(),
	}, nil)

	// First call matches expectation
	result1, _ := mock.Execute(context.Background(), ToolConfig{Command: "limited"})
	if result1.Output != "first" {
		t.Errorf("expected 'first', got %q", result1.Output)
	}

	// Second call falls through to default
	result2, _ := mock.Execute(context.Background(), ToolConfig{Command: "limited"})
	if result2.Output != "default" {
		t.Errorf("expected 'default', got %q", result2.Output)
	}
}

func TestMockExecutorAssertExpectationsMet(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("required").WillSucceed("output", 0).Times(2).Build()

	// Only call once
	_, _ = mock.Execute(context.Background(), ToolConfig{Command: "required"})

	err := mock.AssertExpectationsMet()
	if err == nil {
		t.Error("expected error for unmet expectations")
	}

	// Call second time
	_, _ = mock.Execute(context.Background(), ToolConfig{Command: "required"})

	err = mock.AssertExpectationsMet()
	if err != nil {
		t.Errorf("expectations should be met: %v", err)
	}
}

func TestMockExecutorWillTimeout(t *testing.T) {
	mock := NewMockExecutor()
	mock.ExpectCommand("slow").WillTimeout(5 * time.Second).Build()

	_, err := mock.Execute(context.Background(), ToolConfig{Command: "slow"})
	if err == nil {
		t.Fatal("expected timeout error")
	}

	_, ok := err.(*TimeoutError)
	if !ok {
		t.Errorf("expected *TimeoutError, got %T", err)
	}
}

func TestMockExecutorExecutions(t *testing.T) {
	mock := NewMockExecutor()

	_, _ = mock.Execute(context.Background(), ToolConfig{Command: "cmd1"})
	_, _ = mock.Execute(context.Background(), ToolConfig{Command: "cmd2"})

	executions := mock.Executions()
	if len(executions) != 2 {
		t.Fatalf("expected 2 executions, got %d", len(executions))
	}

	if executions[0].Command != "cmd1" {
		t.Errorf("expected cmd1, got %s", executions[0].Command)
	}
}
