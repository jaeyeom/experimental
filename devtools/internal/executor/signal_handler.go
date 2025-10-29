package executor

import (
	"context"
	"log/slog"
	"os"
	"os/signal"
	"sync"

	"golang.org/x/sys/unix"
)

// SignalHandler manages OS signal handling and graceful shutdown of processes.
type SignalHandler struct {
	// signals is the channel for receiving OS signals
	signals chan os.Signal

	// cancel is the function to cancel the context
	cancel context.CancelFunc

	// wg tracks goroutines for graceful shutdown
	wg sync.WaitGroup

	// mu protects the running state
	mu sync.Mutex

	// running indicates if the handler is active
	running bool
}

// NewSignalHandler creates a new signal handler.
func NewSignalHandler() *SignalHandler {
	return &SignalHandler{
		signals: make(chan os.Signal, 1),
	}
}

// Start begins listening for OS signals and returns a context that will be
// cancelled when a termination signal is received.
func (sh *SignalHandler) Start() (context.Context, error) {
	sh.mu.Lock()
	defer sh.mu.Unlock()

	if sh.running {
		return nil, &SignalHandlerError{Message: "signal handler is already running"}
	}

	// Create a cancellable context
	ctx, cancel := context.WithCancel(context.Background())
	sh.cancel = cancel
	sh.running = true

	// Register for termination signals
	signal.Notify(sh.signals,
		unix.SIGINT,  // Ctrl+C
		unix.SIGTERM, // Termination signal
		unix.SIGHUP,  // Hangup
	)

	// Start the signal handling goroutine
	sh.wg.Add(1)
	go sh.handleSignals()

	slog.Info("Signal handler started", "signals", []string{"SIGINT", "SIGTERM", "SIGHUP"})

	return ctx, nil
}

// Stop gracefully shuts down the signal handler.
func (sh *SignalHandler) Stop() {
	sh.mu.Lock()
	defer sh.mu.Unlock()

	if !sh.running {
		return
	}

	// Stop receiving signals
	signal.Stop(sh.signals)
	close(sh.signals)

	// Cancel the context
	if sh.cancel != nil {
		sh.cancel()
	}

	// Wait for the signal handling goroutine to finish
	sh.wg.Wait()

	sh.running = false
	slog.Info("Signal handler stopped")
}

// handleSignals processes incoming OS signals.
func (sh *SignalHandler) handleSignals() {
	defer sh.wg.Done()

	for sig := range sh.signals {
		slog.Info("Received signal", "signal", sig.String())

		switch sig {
		case unix.SIGINT, unix.SIGTERM:
			// Cancel the context for graceful shutdown
			if sh.cancel != nil {
				slog.Info("Initiating graceful shutdown", "signal", sig.String())
				sh.cancel()
			}
			// For SIGINT/SIGTERM, we stop listening for more signals
			signal.Stop(sh.signals)
			return
		case unix.SIGHUP:
			// SIGHUP typically means reload configuration, but for now we just log it
			slog.Info("Received SIGHUP signal (reload not implemented)")
		}
	}
}

// SignalHandlerError represents errors related to signal handling.
type SignalHandlerError struct {
	Message string
}

func (e *SignalHandlerError) Error() string {
	return "signal handler error: " + e.Message
}
