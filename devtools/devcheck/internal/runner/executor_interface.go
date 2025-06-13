package runner

import (
	"context"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// Executor defines the interface for executing external tools and commands.
// It is implemented by BasicExecutor for production use and MockExecutor for testing.
type Executor interface {
	// Execute runs a tool with the given configuration and returns the result.
	Execute(ctx context.Context, cfg ToolConfig) (*config.ExecutionResult, error)

	// IsAvailable checks if a command is available in the system PATH.
	IsAvailable(command string) bool
}
