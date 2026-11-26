package cli

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

const callerFlagHelp = "identifier substituted for {caller} (else config caller, else the herdr pane id)"

func addCallerFlag(cmd *cobra.Command) {
	cmd.Flags().String("caller", "", callerFlagHelp)
}

// resolveCaller picks the {caller} value for this command.
// A set --caller wins, then [defaults] caller, then CallerPaneID.
// CallerPaneID is the pane Load already stored. This does not read the environment.
// An empty result expands {caller} to empty.
func resolveCaller(cmd *cobra.Command, cfg *config.Config) (string, error) {
	if cmd == nil {
		return "", errors.New("caller: expected a command, found nil")
	}
	if cfg == nil {
		return "", errors.New("caller: expected a config, found nil")
	}
	if cmd.Flags().Changed("caller") {
		value, err := cmd.Flags().GetString("caller")
		if err != nil {
			return "", fmt.Errorf("read --caller flag: %w", err)
		}
		if strings.TrimSpace(value) == "" {
			return "", usageError{err: fmt.Errorf("%s: expected a non-empty caller, found empty", cmd.Name())}
		}
		return value, nil
	}
	if cfg.Defaults.Caller != "" {
		return cfg.Defaults.Caller, nil
	}
	return cfg.CallerPaneID, nil
}

// resolveCallerBus is the caller's agentbus name or handle, for
// {caller_bus} and {report_back}. It runs `agentbus whoami` only when some
// start_prompt uses one of them. A missing agentbus is empty, so
// {report_back} falls back to a herdr prompt to {caller}. Any other
// agentbus failure is also empty, with a warning that names it.
func resolveCallerBus(ctx context.Context, cfg *config.Config, exec executor.Executor) string {
	if cfg == nil || exec == nil || !usesCallerBus(cfg) {
		return ""
	}
	name, err := agentbus.NewClient(exec, "agentbus").Whoami(ctx)
	if errors.Is(err, agentbus.ErrMissing) {
		return ""
	}
	if err != nil {
		slog.Warn("caller agentbus name not read; {report_back} uses herdr agent prompt {caller}", "err", err)
		return ""
	}
	return name
}

func usesCallerBus(cfg *config.Config) bool {
	if hasCallerBusPlaceholder(cfg.Defaults.StartPromptAppend) {
		return true
	}
	for _, src := range cfg.Sources {
		if hasCallerBusPlaceholder(src.StartPrompt) || hasCallerBusPlaceholder(src.StartPromptAppend) {
			return true
		}
	}
	for _, lane := range cfg.Lanes {
		if hasCallerBusPlaceholder(lane.StartPromptAppend) {
			return true
		}
	}
	return false
}

func hasCallerBusPlaceholder(tmpl string) bool {
	return strings.Contains(tmpl, "{caller_bus}") || strings.Contains(tmpl, "{report_back}")
}
