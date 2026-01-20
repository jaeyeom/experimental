package ghtest

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/executor"
)

// MockResponse represents a mock response for a gh command.
type MockResponse struct {
	// Stdout is the standard output to return
	Stdout string

	// Stderr is the standard error to return
	Stderr string

	// ExitCode is the exit code to return (0 for success)
	ExitCode int

	// Error is an optional error to return from Execute
	Error error
}

// GHMockExecutor provides mock responses for gh CLI commands.
// It wraps the standard MockExecutor with gh-specific helpers.
type GHMockExecutor struct {
	*executor.MockExecutor
}

// NewGHMockExecutor creates a new mock executor for gh CLI testing.
func NewGHMockExecutor() *GHMockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gh", true)
	return &GHMockExecutor{MockExecutor: mock}
}

// ExpectGHCommand adds an expectation for a gh subcommand (e.g., "auth token").
func (m *GHMockExecutor) ExpectGHCommand(subcommand string) *GHMockBuilder {
	parts := strings.Fields(subcommand)
	return &GHMockBuilder{
		mock:       m,
		subcommand: parts,
	}
}

// ExpectAuthToken sets up a mock for "gh auth token" command.
func (m *GHMockExecutor) ExpectAuthToken(token string) {
	m.ExpectGHCommand("auth token").WillReturn(token)
}

// ExpectAPICall sets up a mock for "gh api" command.
func (m *GHMockExecutor) ExpectAPICall(endpoint string, response any) error {
	jsonBytes, err := json.Marshal(response)
	if err != nil {
		return fmt.Errorf("marshaling response: %w", err)
	}

	m.MockExecutor.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		if cfg.Command != "gh" || len(cfg.Args) < 2 {
			return false
		}
		if cfg.Args[0] != "api" {
			return false
		}
		// Check if endpoint matches (it might be at different positions due to flags)
		for _, arg := range cfg.Args[1:] {
			if strings.Contains(arg, endpoint) || arg == endpoint {
				return true
			}
		}
		return false
	}).WillSucceed(string(jsonBytes), 0).Build()

	return nil
}

// GHMockBuilder provides a fluent interface for building gh command mocks.
type GHMockBuilder struct {
	mock       *GHMockExecutor
	subcommand []string
	response   MockResponse
}

// WillReturn sets the stdout response for the mock.
func (b *GHMockBuilder) WillReturn(stdout string) *GHMockBuilder {
	b.response.Stdout = stdout
	return b.build()
}

// WillReturnJSON marshals the value as JSON for the stdout response.
func (b *GHMockBuilder) WillReturnJSON(v any) *GHMockBuilder {
	data, _ := json.Marshal(v)
	b.response.Stdout = string(data)
	return b.build()
}

// WillFail sets the mock to return an error exit code.
func (b *GHMockBuilder) WillFail(stderr string, exitCode int) *GHMockBuilder {
	b.response.Stderr = stderr
	b.response.ExitCode = exitCode
	return b.build()
}

func (b *GHMockBuilder) build() *GHMockBuilder {
	subcommand := b.subcommand
	response := b.response

	b.mock.MockExecutor.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		if cfg.Command != "gh" {
			return false
		}
		if len(cfg.Args) < len(subcommand) {
			return false
		}
		for i, part := range subcommand {
			if cfg.Args[i] != part {
				return false
			}
		}
		return true
	}).WillReturn(&executor.ExecutionResult{
		Command:   "gh",
		Args:      append(subcommand, "..."),
		Output:    response.Stdout,
		Stderr:    response.Stderr,
		ExitCode:  response.ExitCode,
		StartTime: time.Now(),
		EndTime:   time.Now(),
	}, response.Error).Build()

	return b
}

// Executor returns the underlying executor for use with code that expects executor.Executor.
func (m *GHMockExecutor) Executor() executor.Executor {
	return m.MockExecutor
}
