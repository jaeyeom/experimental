package runner

import (
	"os"
	"runtime"
	"syscall"
	"testing"
	"time"
)

func TestNewExecutorWithSignalHandling(t *testing.T) {
	executor := NewExecutorWithSignalHandling()
	if executor == nil {
		t.Fatal("NewExecutorWithSignalHandling() returned nil")
	}
	if executor.executor == nil {
		t.Error("BasicExecutor not initialized")
	}
	if executor.signalHandler == nil {
		t.Error("SignalHandler not initialized")
	}
	if executor.processes == nil {
		t.Error("processes map not initialized")
	}
}

func TestExecutorWithSignalHandling_Start_Stop(t *testing.T) {
	executor := NewExecutorWithSignalHandling()

	// Start the executor
	ctx, err := executor.Start()
	if err != nil {
		t.Fatalf("Start() failed: %v", err)
	}

	if ctx == nil {
		t.Fatal("Start() returned nil context")
	}

	// Stop the executor
	executor.Stop()

	// Should be able to stop multiple times
	executor.Stop()
}

func TestExecutorWithSignalHandling_Execute(t *testing.T) {
	executor := NewExecutorWithSignalHandling()

	ctx, err := executor.Start()
	if err != nil {
		t.Fatalf("Start() failed: %v", err)
	}
	defer executor.Stop()

	// Test simple command execution
	config := ToolConfig{
		Command: "echo",
		Args:    []string{"hello", "world"},
	}

	result, err := executor.Execute(ctx, config)
	if err != nil {
		t.Fatalf("Execute() failed: %v", err)
	}

	if result == nil {
		t.Fatal("Execute() returned nil result")
	}

	if result.ExitCode != 0 {
		t.Errorf("Expected exit code 0, got %d", result.ExitCode)
	}

	expectedOutput := "hello world\n"
	if result.Output != expectedOutput {
		t.Errorf("Expected output %q, got %q", expectedOutput, result.Output)
	}
}

func TestExecutorWithSignalHandling_ProcessTracking(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping process tracking test on Windows")
	}

	executor := NewExecutorWithSignalHandling()

	ctx, err := executor.Start()
	if err != nil {
		t.Fatalf("Start() failed: %v", err)
	}
	defer executor.Stop()

	// Check initial process count
	if count := executor.GetRunningProcesses(); count != 0 {
		t.Errorf("Expected 0 running processes, got %d", count)
	}

	// Start a long-running command
	config := ToolConfig{
		Command: "sleep",
		Args:    []string{"2"},
	}

	done := make(chan struct{})
	go func() {
		defer close(done)
		_, err := executor.Execute(ctx, config)
		if err != nil {
			t.Errorf("Execute() failed: %v", err)
		}
	}()

	// Give command time to start
	time.Sleep(100 * time.Millisecond)

	// Check that process is tracked
	if count := executor.GetRunningProcesses(); count != 1 {
		t.Errorf("Expected 1 running process, got %d", count)
	}

	// Wait for command to complete
	select {
	case <-done:
		// Command completed
	case <-time.After(5 * time.Second):
		t.Fatal("Command did not complete within timeout")
	}

	// Check that process is no longer tracked
	if count := executor.GetRunningProcesses(); count != 0 {
		t.Errorf("Expected 0 running processes after completion, got %d", count)
	}
}

func TestExecutorWithSignalHandling_SignalCancellation(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping signal cancellation test on Windows")
	}

	executor := NewExecutorWithSignalHandling()

	ctx, err := executor.Start()
	if err != nil {
		t.Fatalf("Start() failed: %v", err)
	}
	defer executor.Stop()

	// Start a long-running command
	config := ToolConfig{
		Command: "sleep",
		Args:    []string{"10"},
	}

	done := make(chan struct{})
	var execErr error

	go func() {
		defer close(done)
		_, execErr = executor.Execute(ctx, config)
	}()

	// Give command time to start
	time.Sleep(100 * time.Millisecond)

	// Check that process is tracked
	if count := executor.GetRunningProcesses(); count != 1 {
		t.Errorf("Expected 1 running process, got %d", count)
	}

	// Send SIGTERM to cancel
	go func() {
		time.Sleep(100 * time.Millisecond)
		if err := syscall.Kill(os.Getpid(), syscall.SIGTERM); err != nil {
			t.Errorf("Failed to send SIGTERM: %v", err)
		}
	}()

	// Wait for command to be cancelled
	select {
	case <-done:
		// Command was cancelled
	case <-time.After(3 * time.Second):
		t.Fatal("Command was not cancelled within timeout")
	}

	// The command should have been interrupted or the context should be done
	if execErr == nil && ctx.Err() == nil {
		t.Error("Expected error from cancelled command or context cancellation")
	}

	// Process should no longer be tracked
	if count := executor.GetRunningProcesses(); count != 0 {
		t.Errorf("Expected 0 running processes after cancellation, got %d", count)
	}
}

func TestExecutorWithSignalHandling_IsAvailable(t *testing.T) {
	executor := NewExecutorWithSignalHandling()

	// Test with a command that should be available
	if !executor.IsAvailable("echo") {
		t.Error("echo command should be available")
	}

	// Test with a command that should not be available
	if executor.IsAvailable("nonexistent-command-12345") {
		t.Error("nonexistent command should not be available")
	}
}
