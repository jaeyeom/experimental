package runner

import (
	"os"
	"runtime"
	"testing"
	"time"

	"golang.org/x/sys/unix"
)

func TestNewSignalHandler(t *testing.T) {
	handler := NewSignalHandler()
	if handler == nil {
		t.Fatal("NewSignalHandler() returned nil")
		return
	}
	if handler.signals == nil {
		t.Error("signals channel not initialized")
	}
	if handler.running {
		t.Error("handler should not be running initially")
	}
}

func TestSignalHandler_Start(t *testing.T) {
	handler := NewSignalHandler()

	ctx, err := handler.Start()
	if err != nil {
		t.Fatalf("Start() failed: %v", err)
	}

	if ctx == nil {
		t.Fatal("Start() returned nil context")
	}

	// Check that handler is running
	handler.mu.Lock()
	running := handler.running
	handler.mu.Unlock()

	if !running {
		t.Error("handler should be running after Start()")
	}

	// Clean up
	handler.Stop()
}

func TestSignalHandler_StartTwice(t *testing.T) {
	handler := NewSignalHandler()

	// First start should succeed
	_, err := handler.Start()
	if err != nil {
		t.Fatalf("First Start() failed: %v", err)
	}

	// Second start should fail
	_, err = handler.Start()
	if err == nil {
		t.Error("Second Start() should have failed")
	}

	// Check error type
	if _, ok := err.(*SignalHandlerError); !ok {
		t.Errorf("Expected SignalHandlerError, got %T", err)
	}

	// Clean up
	handler.Stop()
}

func TestSignalHandler_Stop(t *testing.T) {
	handler := NewSignalHandler()

	// Start the handler
	_, err := handler.Start()
	if err != nil {
		t.Fatalf("Start() failed: %v", err)
	}

	// Stop the handler
	handler.Stop()

	// Check that handler is not running
	handler.mu.Lock()
	running := handler.running
	handler.mu.Unlock()

	if running {
		t.Error("handler should not be running after Stop()")
	}

	// Stop again should be safe
	handler.Stop()
}

func TestSignalHandler_SignalHandling(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Skipping signal test on Windows")
	}

	handler := NewSignalHandler()

	ctx, err := handler.Start()
	if err != nil {
		t.Fatalf("Start() failed: %v", err)
	}

	// Send SIGTERM to ourselves
	go func() {
		time.Sleep(100 * time.Millisecond)
		if err := unix.Kill(os.Getpid(), unix.SIGTERM); err != nil {
			t.Errorf("Failed to send SIGTERM: %v", err)
		}
	}()

	// Wait for context to be cancelled
	select {
	case <-ctx.Done():
		// Context was cancelled, which is expected
	case <-time.After(2 * time.Second):
		t.Error("Context was not cancelled within timeout")
	}

	// Clean up
	handler.Stop()
}

func TestSignalHandlerError(t *testing.T) {
	err := &SignalHandlerError{Message: "test error"}
	expected := "signal handler error: test error"
	if err.Error() != expected {
		t.Errorf("Error() = %q, want %q", err.Error(), expected)
	}
}
