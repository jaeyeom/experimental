package runner

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// MockExecutor is a mock implementation of the Executor interface for testing.
type MockExecutor struct {
	mu sync.RWMutex

	// Expectations for Execute calls
	expectations []MockExpectation

	// AvailableCommands is a map of commands that IsAvailable should return true for
	AvailableCommands map[string]bool

	// CallHistory records all Execute calls made
	CallHistory []MockCall

	// Default behavior when no expectation matches
	DefaultResult *config.ExecutionResult
	DefaultError  error
}

// MockExpectation represents an expected call to Execute with a predefined response.
type MockExpectation struct {
	// Matcher function to determine if this expectation matches the call
	Matcher func(ctx context.Context, cfg ToolConfig) bool

	// Response to return when matched
	Result *config.ExecutionResult
	Error  error

	// Times specifies how many times this expectation can be used (0 = unlimited)
	Times int
	used  int
}

// MockCall represents a recorded call to Execute.
type MockCall struct {
	Config    ToolConfig
	Timestamp time.Time
	Context   context.Context
}

// NewMockExecutor creates a new MockExecutor instance.
func NewMockExecutor() *MockExecutor {
	return &MockExecutor{
		AvailableCommands: make(map[string]bool),
		CallHistory:       make([]MockCall, 0),
		expectations:      make([]MockExpectation, 0),
	}
}

// Execute implements the Executor interface.
func (m *MockExecutor) Execute(ctx context.Context, cfg ToolConfig) (*config.ExecutionResult, error) {
	m.mu.Lock()
	defer m.mu.Unlock()

	// Record the call
	m.CallHistory = append(m.CallHistory, MockCall{
		Config:    cfg,
		Timestamp: time.Now(),
		Context:   ctx,
	})

	// Find matching expectation
	for i := range m.expectations {
		exp := &m.expectations[i]
		if exp.Matcher(ctx, cfg) && (exp.Times == 0 || exp.used < exp.Times) {
			exp.used++
			return exp.Result, exp.Error
		}
	}

	// No expectation matched, use default behavior
	if m.DefaultResult != nil || m.DefaultError != nil {
		return m.DefaultResult, m.DefaultError
	}

	// If no default is set, return a generic success result
	return &config.ExecutionResult{
		Command:    cfg.Command,
		Args:       cfg.Args,
		WorkingDir: cfg.WorkingDir,
		Output:     fmt.Sprintf("Mock execution of: %s %s", cfg.Command, strings.Join(cfg.Args, " ")),
		Stderr:     "",
		ExitCode:   0,
		StartTime:  time.Now(),
		EndTime:    time.Now(),
		TimedOut:   false,
	}, nil
}

// IsAvailable implements the Executor interface.
func (m *MockExecutor) IsAvailable(command string) bool {
	m.mu.RLock()
	defer m.mu.RUnlock()

	available, exists := m.AvailableCommands[command]
	return exists && available
}

// ExpectCommand adds an expectation for a specific command.
func (m *MockExecutor) ExpectCommand(command string) *MockExpectationBuilder {
	return &MockExpectationBuilder{
		mock: m,
		expectation: MockExpectation{
			Matcher: func(ctx context.Context, cfg ToolConfig) bool {
				return cfg.Command == command
			},
		},
	}
}

// ExpectCommandWithArgs adds an expectation for a command with specific arguments.
func (m *MockExecutor) ExpectCommandWithArgs(command string, args ...string) *MockExpectationBuilder {
	return &MockExpectationBuilder{
		mock: m,
		expectation: MockExpectation{
			Matcher: func(ctx context.Context, cfg ToolConfig) bool {
				if cfg.Command != command {
					return false
				}
				if len(cfg.Args) != len(args) {
					return false
				}
				for i, arg := range args {
					if cfg.Args[i] != arg {
						return false
					}
				}
				return true
			},
		},
	}
}

// ExpectCustom adds an expectation with a custom matcher function.
func (m *MockExecutor) ExpectCustom(matcher func(ctx context.Context, cfg ToolConfig) bool) *MockExpectationBuilder {
	return &MockExpectationBuilder{
		mock: m,
		expectation: MockExpectation{
			Matcher: matcher,
		},
	}
}

// SetDefaultBehavior sets the default response when no expectation matches.
func (m *MockExecutor) SetDefaultBehavior(result *config.ExecutionResult, err error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.DefaultResult = result
	m.DefaultError = err
}

// SetAvailableCommand marks a command as available or unavailable.
func (m *MockExecutor) SetAvailableCommand(command string, available bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.AvailableCommands[command] = available
}

// GetCallHistory returns a copy of the call history.
func (m *MockExecutor) GetCallHistory() []MockCall {
	m.mu.RLock()
	defer m.mu.RUnlock()
	history := make([]MockCall, len(m.CallHistory))
	copy(history, m.CallHistory)
	return history
}

// ClearCallHistory clears the recorded call history.
func (m *MockExecutor) ClearCallHistory() {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.CallHistory = make([]MockCall, 0)
}

// AssertExpectationsMet checks if all expectations with fixed times have been met.
func (m *MockExecutor) AssertExpectationsMet() error {
	m.mu.RLock()
	defer m.mu.RUnlock()

	for _, exp := range m.expectations {
		if exp.Times > 0 && exp.used < exp.Times {
			return fmt.Errorf("expectation not met: expected %d calls, got %d", exp.Times, exp.used)
		}
	}
	return nil
}

// MockExpectationBuilder provides a fluent interface for building expectations.
type MockExpectationBuilder struct {
	mock        *MockExecutor
	expectation MockExpectation
}

// WillReturn sets the result to return when the expectation is matched.
func (b *MockExpectationBuilder) WillReturn(result *config.ExecutionResult, err error) *MockExpectationBuilder {
	b.expectation.Result = result
	b.expectation.Error = err
	return b
}

// WillSucceed sets a successful execution result.
func (b *MockExpectationBuilder) WillSucceed(output string, exitCode int) *MockExpectationBuilder {
	b.expectation.Result = &config.ExecutionResult{
		Output:    output,
		ExitCode:  exitCode,
		StartTime: time.Now(),
		EndTime:   time.Now(),
	}
	b.expectation.Error = nil
	return b
}

// WillFail sets a failed execution result.
func (b *MockExpectationBuilder) WillFail(stderr string, exitCode int) *MockExpectationBuilder {
	b.expectation.Result = &config.ExecutionResult{
		Stderr:    stderr,
		ExitCode:  exitCode,
		StartTime: time.Now(),
		EndTime:   time.Now(),
	}
	b.expectation.Error = nil
	return b
}

// WillTimeout sets a timeout result.
func (b *MockExpectationBuilder) WillTimeout(timeout time.Duration) *MockExpectationBuilder {
	b.expectation.Result = nil
	b.expectation.Error = &TimeoutError{
		Command: "mock command",
		Timeout: timeout,
	}
	return b
}

// WillError sets an error response.
func (b *MockExpectationBuilder) WillError(err error) *MockExpectationBuilder {
	b.expectation.Result = nil
	b.expectation.Error = err
	return b
}

// Times sets how many times this expectation should match.
func (b *MockExpectationBuilder) Times(n int) *MockExpectationBuilder {
	b.expectation.Times = n
	return b
}

// Once is a convenience method for Times(1).
func (b *MockExpectationBuilder) Once() *MockExpectationBuilder {
	return b.Times(1)
}

// Build finalizes the expectation and adds it to the mock.
func (b *MockExpectationBuilder) Build() {
	b.mock.mu.Lock()
	defer b.mock.mu.Unlock()
	b.mock.expectations = append(b.mock.expectations, b.expectation)
}
