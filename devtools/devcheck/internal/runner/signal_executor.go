package runner

import (
	"context"
	"log/slog"
	"sync"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// ExecutorWithSignalHandling wraps a BasicExecutor with signal handling capabilities.
type ExecutorWithSignalHandling struct {
	executor      *BasicExecutor
	signalHandler *SignalHandler

	// mu protects the processes map
	mu sync.Mutex
	// processes tracks running processes for cleanup
	processes map[string]context.CancelFunc
}

// NewExecutorWithSignalHandling creates a new executor with signal handling.
func NewExecutorWithSignalHandling() *ExecutorWithSignalHandling {
	return &ExecutorWithSignalHandling{
		executor:      NewBasicExecutor(),
		signalHandler: NewSignalHandler(),
		processes:     make(map[string]context.CancelFunc),
	}
}

// Start initializes the signal handler and returns a context for the executor.
func (e *ExecutorWithSignalHandling) Start() (context.Context, error) {
	return e.signalHandler.Start()
}

// Stop gracefully shuts down the executor and signal handler.
func (e *ExecutorWithSignalHandling) Stop() {
	// Cancel all running processes
	e.mu.Lock()
	for id, cancel := range e.processes {
		slog.Info("Cancelling process", "id", id)
		cancel()
	}
	e.processes = make(map[string]context.CancelFunc)
	e.mu.Unlock()

	// Stop the signal handler
	e.signalHandler.Stop()
}

// Execute runs a command with signal handling support.
func (e *ExecutorWithSignalHandling) Execute(ctx context.Context, cfg ToolConfig) (*config.ExecutionResult, error) {
	// Create a unique ID for this execution
	execID := buildCommandString(cfg.Command, cfg.Args)

	// Create a cancellable context for this specific execution
	execCtx, cancel := context.WithCancel(ctx)

	// Register the process
	e.mu.Lock()
	e.processes[execID] = cancel
	e.mu.Unlock()

	// Clean up when done
	defer func() {
		e.mu.Lock()
		delete(e.processes, execID)
		e.mu.Unlock()
		cancel()
	}()

	slog.Debug("Starting command execution with signal handling",
		"command", cfg.Command,
		"args", cfg.Args,
		"exec_id", execID)

	// Execute using the wrapped executor
	result, err := e.executor.Execute(execCtx, cfg)

	slog.Debug("Command execution completed",
		"command", cfg.Command,
		"exec_id", execID,
		"exit_code", func() int {
			if result != nil {
				return result.ExitCode
			}
			return -1
		}())

	return result, err
}

// IsAvailable checks if a command is available (delegates to BasicExecutor).
func (e *ExecutorWithSignalHandling) IsAvailable(command string) bool {
	return e.executor.IsAvailable(command)
}

// GetRunningProcesses returns the number of currently running processes.
func (e *ExecutorWithSignalHandling) GetRunningProcesses() int {
	e.mu.Lock()
	defer e.mu.Unlock()
	return len(e.processes)
}
