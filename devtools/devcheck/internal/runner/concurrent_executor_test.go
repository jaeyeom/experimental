package runner

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestNewConcurrentExecutor(t *testing.T) {
	basicExecutor := NewBasicExecutor()
	concurrentExecutor := NewConcurrentExecutor(basicExecutor)

	if concurrentExecutor == nil {
		t.Fatal("NewConcurrentExecutor() returned nil")
	}

	if concurrentExecutor.GetMaxConcurrency() != 10 {
		t.Errorf("Default max concurrency = %d, want 10", concurrentExecutor.GetMaxConcurrency())
	}
}

func TestConcurrentExecutor_SetMaxConcurrency(t *testing.T) {
	executor := NewConcurrentExecutor(NewBasicExecutor())

	tests := []struct {
		name     string
		input    int
		expected int
	}{
		{
			name:     "positive value",
			input:    5,
			expected: 5,
		},
		{
			name:     "zero value",
			input:    0,
			expected: 1,
		},
		{
			name:     "negative value",
			input:    -5,
			expected: 1,
		},
		{
			name:     "large value",
			input:    100,
			expected: 100,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			executor.SetMaxConcurrency(tt.input)
			if got := executor.GetMaxConcurrency(); got != tt.expected {
				t.Errorf("GetMaxConcurrency() = %d, want %d", got, tt.expected)
			}
		})
	}
}

func TestConcurrentExecutor_Execute(t *testing.T) {
	basicExecutor := NewBasicExecutor()
	concurrentExecutor := NewConcurrentExecutor(basicExecutor)
	ctx := context.Background()

	cfg := ToolConfig{
		Command: "echo",
		Args:    []string{"hello", "world"},
	}

	result, err := concurrentExecutor.Execute(ctx, cfg)
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
}

func TestConcurrentExecutor_IsAvailable(t *testing.T) {
	basicExecutor := NewBasicExecutor()
	concurrentExecutor := NewConcurrentExecutor(basicExecutor)

	// Test with a command that should be available
	if !concurrentExecutor.IsAvailable("echo") {
		t.Error("echo command should be available")
	}

	// Test with a command that should not be available
	if concurrentExecutor.IsAvailable("nonexistent-command-12345") {
		t.Error("nonexistent command should not be available")
	}
}

func TestConcurrentExecutor_ExecuteAll_Empty(t *testing.T) {
	executor := NewConcurrentExecutor(NewBasicExecutor())
	ctx := context.Background()

	results, err := executor.ExecuteAll(ctx, []ToolConfig{})
	if err != nil {
		t.Errorf("ExecuteAll() error = %v", err)
	}
	if len(results) != 0 {
		t.Errorf("ExecuteAll() returned %d results, want 0", len(results))
	}
}

func TestConcurrentExecutor_ExecuteAll_SingleCommand(t *testing.T) {
	executor := NewConcurrentExecutor(NewBasicExecutor())
	ctx := context.Background()

	configs := []ToolConfig{
		{
			Command: "echo",
			Args:    []string{"test"},
		},
	}

	results, err := executor.ExecuteAll(ctx, configs)
	if err != nil {
		t.Errorf("ExecuteAll() error = %v", err)
	}
	if len(results) != 1 {
		t.Fatalf("ExecuteAll() returned %d results, want 1", len(results))
	}

	result := results[0]
	if result.Index != 0 {
		t.Errorf("Result index = %d, want 0", result.Index)
	}
	if result.Error != nil {
		t.Errorf("Result error = %v, want nil", result.Error)
	}
	if result.Result == nil {
		t.Fatal("Result.Result is nil")
	}
	if result.Result.ExitCode != 0 {
		t.Errorf("Result exit code = %d, want 0", result.Result.ExitCode)
	}
}

func TestConcurrentExecutor_ExecuteAll_MultipleCommands(t *testing.T) {
	executor := NewConcurrentExecutor(NewBasicExecutor())
	ctx := context.Background()

	configs := []ToolConfig{
		{Command: "echo", Args: []string{"first"}},
		{Command: "echo", Args: []string{"second"}},
		{Command: "echo", Args: []string{"third"}},
	}

	results, err := executor.ExecuteAll(ctx, configs)
	if err != nil {
		t.Errorf("ExecuteAll() error = %v", err)
	}
	if len(results) != 3 {
		t.Fatalf("ExecuteAll() returned %d results, want 3", len(results))
	}

	// Check that all results are present and in correct order
	for i, result := range results {
		if result.Index != i {
			t.Errorf("Result[%d] index = %d, want %d", i, result.Index, i)
		}
		if result.Error != nil {
			t.Errorf("Result[%d] error = %v, want nil", i, result.Error)
		}
		if result.Result == nil {
			t.Fatalf("Result[%d].Result is nil", i)
		}
		if result.Result.ExitCode != 0 {
			t.Errorf("Result[%d] exit code = %d, want 0", i, result.Result.ExitCode)
		}
	}
}

func TestConcurrentExecutor_ExecuteConcurrent_ConcurrencyLimit(t *testing.T) {
	// Create a mock executor that tracks concurrent executions
	mock := NewMockExecutor()
	var concurrentCount int64
	var maxConcurrent int64

	// Set up expectation that tracks concurrency
	mock.ExpectCustom(func(_ context.Context, cfg ToolConfig) bool {
		return cfg.Command == "sleep"
	}).WillReturn(&config.ExecutionResult{
		Output:   "sleep completed",
		ExitCode: 0,
	}, nil)

	// Wrap the mock to track concurrency
	trackingExecutor := &concurrencyTrackingExecutor{
		executor:        mock,
		concurrentCount: &concurrentCount,
		maxConcurrent:   &maxConcurrent,
	}

	executor := NewConcurrentExecutor(trackingExecutor)
	ctx := context.Background()

	// Create multiple sleep commands
	configs := make([]ToolConfig, 5)
	for i := range configs {
		configs[i] = ToolConfig{
			Command: "sleep",
			Args:    []string{"0.1"},
		}
	}

	// Test with concurrency limit of 2
	results, err := executor.ExecuteConcurrent(ctx, configs, 2)
	if err != nil {
		t.Errorf("ExecuteConcurrent() error = %v", err)
	}
	if len(results) != 5 {
		t.Errorf("ExecuteConcurrent() returned %d results, want 5", len(results))
	}

	// Check that max concurrent executions didn't exceed the limit
	maxReached := atomic.LoadInt64(&maxConcurrent)
	if maxReached > 2 {
		t.Errorf("Max concurrent executions = %d, want <= 2", maxReached)
	}
}

func TestConcurrentExecutor_ExecuteAll_WithErrors(t *testing.T) {
	mock := NewMockExecutor()

	// Set up expectations: some succeed, some fail
	mock.ExpectCommand("echo").WillSucceed("success", 0).Build()
	mock.ExpectCommand("fail").WillError(errors.New("command failed")).Build()
	mock.ExpectCommand("echo").WillSucceed("success again", 0).Build()

	executor := NewConcurrentExecutor(mock)
	ctx := context.Background()

	configs := []ToolConfig{
		{Command: "echo", Args: []string{"test1"}},
		{Command: "fail", Args: []string{"test2"}},
		{Command: "echo", Args: []string{"test3"}},
	}

	results, err := executor.ExecuteAll(ctx, configs)
	if err != nil {
		t.Errorf("ExecuteAll() error = %v", err)
	}
	if len(results) != 3 {
		t.Fatalf("ExecuteAll() returned %d results, want 3", len(results))
	}

	// First command should succeed
	if results[0].Error != nil {
		t.Errorf("Result[0] error = %v, want nil", results[0].Error)
	}

	// Second command should fail
	if results[1].Error == nil {
		t.Error("Result[1] error = nil, want error")
	}

	// Third command should succeed
	if results[2].Error != nil {
		t.Errorf("Result[2] error = %v, want nil", results[2].Error)
	}
}

func TestConcurrentExecutor_ExecuteConcurrent_ContextCancellation(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping context cancellation test on Windows")
	}

	executor := NewConcurrentExecutor(NewBasicExecutor())

	// Create a context that will be cancelled
	ctx, cancel := context.WithCancel(context.Background())

	configs := []ToolConfig{
		{Command: "sleep", Args: []string{"10"}}, // Long-running command
		{Command: "sleep", Args: []string{"10"}},
		{Command: "sleep", Args: []string{"10"}},
	}

	// Start execution in a goroutine
	done := make(chan struct{})
	var results []ConcurrentResult

	go func() {
		defer close(done)
		results, _ = executor.ExecuteAll(ctx, configs)
	}()

	// Give commands time to start
	time.Sleep(100 * time.Millisecond)

	// Cancel the context
	cancel()

	// Wait for execution to complete
	select {
	case <-done:
		// Execution completed
	case <-time.After(5 * time.Second):
		t.Fatal("ExecuteAll() did not respond to context cancellation")
	}

	// Results should be returned even if commands were cancelled
	if len(results) != 3 {
		t.Errorf("ExecuteAll() returned %d results, want 3", len(results))
	}
}

func TestConcurrentExecutor_ExecuteConcurrent_PreservesOrder(t *testing.T) {
	mock := NewMockExecutor()

	// Set up expectations with different outputs
	for i := 0; i < 10; i++ {
		mock.ExpectCommand("echo").
			WillSucceed(fmt.Sprintf("output-%d", i), 0).
			Build()
	}

	executor := NewConcurrentExecutor(mock)
	ctx := context.Background()

	configs := make([]ToolConfig, 10)
	for i := range configs {
		configs[i] = ToolConfig{
			Command: "echo",
			Args:    []string{fmt.Sprintf("arg-%d", i)},
		}
	}

	results, err := executor.ExecuteConcurrent(ctx, configs, 5)
	if err != nil {
		t.Errorf("ExecuteConcurrent() error = %v", err)
	}
	if len(results) != 10 {
		t.Fatalf("ExecuteConcurrent() returned %d results, want 10", len(results))
	}

	// Check that results are in the correct order
	for i, result := range results {
		if result.Index != i {
			t.Errorf("Result[%d] index = %d, want %d", i, result.Index, i)
		}
		if result.Config.Args[0] != fmt.Sprintf("arg-%d", i) {
			t.Errorf("Result[%d] config args = %v, want [arg-%d]", i, result.Config.Args, i)
		}
	}
}

func TestConcurrentExecutor_ExecuteConcurrent_ZeroConcurrency(t *testing.T) {
	executor := NewConcurrentExecutor(NewBasicExecutor())
	ctx := context.Background()

	configs := []ToolConfig{
		{Command: "echo", Args: []string{"test"}},
	}

	// Test with zero concurrency (should default to 1)
	results, err := executor.ExecuteConcurrent(ctx, configs, 0)
	if err != nil {
		t.Errorf("ExecuteConcurrent() error = %v", err)
	}
	if len(results) != 1 {
		t.Errorf("ExecuteConcurrent() returned %d results, want 1", len(results))
	}
}

func TestConcurrentExecutor_ExecuteConcurrent_LargeConcurrency(t *testing.T) {
	executor := NewConcurrentExecutor(NewBasicExecutor())
	ctx := context.Background()

	configs := []ToolConfig{
		{Command: "echo", Args: []string{"test1"}},
		{Command: "echo", Args: []string{"test2"}},
	}

	// Test with very large concurrency limit
	results, err := executor.ExecuteConcurrent(ctx, configs, 1000)
	if err != nil {
		t.Errorf("ExecuteConcurrent() error = %v", err)
	}
	if len(results) != 2 {
		t.Errorf("ExecuteConcurrent() returned %d results, want 2", len(results))
	}
}

// concurrencyTrackingExecutor wraps an executor to track concurrent executions.
type concurrencyTrackingExecutor struct {
	executor        Executor
	concurrentCount *int64
	maxConcurrent   *int64
}

func (e *concurrencyTrackingExecutor) Execute(ctx context.Context, cfg ToolConfig) (*config.ExecutionResult, error) {
	// Increment concurrent count
	current := atomic.AddInt64(e.concurrentCount, 1)

	// Update max concurrent if needed
	for {
		maxConcurrent := atomic.LoadInt64(e.maxConcurrent)
		if current <= maxConcurrent || atomic.CompareAndSwapInt64(e.maxConcurrent, maxConcurrent, current) {
			break
		}
	}

	// Add a small delay to ensure concurrency is actually happening
	time.Sleep(50 * time.Millisecond)

	// Execute the actual command
	result, err := e.executor.Execute(ctx, cfg)

	// Decrement concurrent count
	atomic.AddInt64(e.concurrentCount, -1)

	return result, err //nolint:wrapcheck // test helper
}

func (e *concurrencyTrackingExecutor) IsAvailable(command string) bool {
	return e.executor.IsAvailable(command)
}
