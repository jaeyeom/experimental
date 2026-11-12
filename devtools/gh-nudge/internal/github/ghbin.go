package github

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
	executor "github.com/jaeyeom/go-cmdexec"
)

// GHExecutor wraps base so that a command named "gh" runs ghcli.Bin()
// instead. With GH_BIN unset, base is returned unchanged.
func GHExecutor(base executor.Executor) executor.Executor {
	bin := ghcli.Bin()
	if bin == "gh" {
		return base
	}
	return &ghBinExecutor{base: base, bin: bin}
}

type ghBinExecutor struct {
	base executor.Executor
	bin  string
}

func (e *ghBinExecutor) resolve(command string) string {
	if command == "gh" {
		return e.bin
	}
	return command
}

func (e *ghBinExecutor) Execute(ctx context.Context, cfg executor.ToolConfig) (*executor.ExecutionResult, error) {
	cfg.Command = e.resolve(cfg.Command)
	result, err := e.base.Execute(ctx, cfg)
	if err != nil {
		return result, fmt.Errorf("run %s: %w", cfg.Command, err)
	}
	return result, nil
}

func (e *ghBinExecutor) IsAvailable(command string) bool {
	return e.base.IsAvailable(e.resolve(command))
}
