package runner

import (
	"context"
	"sync"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// ConcurrentResult represents the result of a concurrent command execution.
type ConcurrentResult struct {
	// Index is the original index of the command in the input slice
	Index int

	// Config is the original tool configuration
	Config ToolConfig

	// Result is the execution result (nil if Error is set)
	Result *config.ExecutionResult

	// Error is any error that occurred during execution
	Error error
}

// Executor defines the interface for executing external tools and commands.
// It is implemented by BasicExecutor for production use and MockExecutor for testing.
type Executor interface {
	// Execute runs a tool with the given configuration and returns the result.
	Execute(ctx context.Context, cfg ToolConfig) (*config.ExecutionResult, error)

	// IsAvailable checks if a command is available in the system PATH.
	IsAvailable(command string) bool
}

// ConcurrentExecutor wraps an Executor to provide concurrent execution capabilities.
type ConcurrentExecutor struct {
	executor       Executor
	maxConcurrency int
	mu             sync.RWMutex
}

// NewConcurrentExecutor creates a new concurrent executor wrapping the given executor.
func NewConcurrentExecutor(executor Executor) *ConcurrentExecutor {
	return &ConcurrentExecutor{
		executor:       executor,
		maxConcurrency: 10, // Default to 10 concurrent executions
	}
}

// Execute implements the Executor interface by delegating to the wrapped executor.
func (ce *ConcurrentExecutor) Execute(ctx context.Context, cfg ToolConfig) (*config.ExecutionResult, error) {
	return ce.executor.Execute(ctx, cfg) //nolint:wrapcheck // delegation pattern
}

// IsAvailable implements the Executor interface by delegating to the wrapped executor.
func (ce *ConcurrentExecutor) IsAvailable(command string) bool {
	return ce.executor.IsAvailable(command)
}

// SetMaxConcurrency sets the maximum number of concurrent executions.
func (ce *ConcurrentExecutor) SetMaxConcurrency(maxConcurrency int) {
	ce.mu.Lock()
	defer ce.mu.Unlock()
	if maxConcurrency <= 0 {
		maxConcurrency = 1
	}
	ce.maxConcurrency = maxConcurrency
}

// GetMaxConcurrency returns the current maximum concurrency setting.
func (ce *ConcurrentExecutor) GetMaxConcurrency() int {
	ce.mu.RLock()
	defer ce.mu.RUnlock()
	return ce.maxConcurrency
}

// ExecuteAll runs all commands concurrently using the default max concurrency.
func (ce *ConcurrentExecutor) ExecuteAll(ctx context.Context, configs []ToolConfig) ([]ConcurrentResult, error) {
	maxConcurrency := ce.GetMaxConcurrency()
	return ce.ExecuteConcurrent(ctx, configs, maxConcurrency)
}

// ExecuteConcurrent runs multiple commands with the specified concurrency limit.
func (ce *ConcurrentExecutor) ExecuteConcurrent(ctx context.Context, configs []ToolConfig, maxConcurrency int) ([]ConcurrentResult, error) {
	if len(configs) == 0 {
		return []ConcurrentResult{}, nil
	}

	if maxConcurrency <= 0 {
		maxConcurrency = 1
	}

	// Create a semaphore to limit concurrency
	semaphore := make(chan struct{}, maxConcurrency)
	results := make([]ConcurrentResult, len(configs))
	var wg sync.WaitGroup

	// Execute commands concurrently
	for i, cfg := range configs {
		wg.Add(1)
		go func(index int, config ToolConfig) {
			defer wg.Done()

			// Acquire semaphore
			semaphore <- struct{}{}
			defer func() { <-semaphore }()

			// Execute the command
			result, err := ce.executor.Execute(ctx, config)

			// Store the result
			results[index] = ConcurrentResult{
				Index:  index,
				Config: config,
				Result: result,
				Error:  err,
			}
		}(i, cfg)
	}

	// Wait for all commands to complete
	wg.Wait()

	return results, nil
}
