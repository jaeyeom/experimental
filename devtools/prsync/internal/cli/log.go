package cli

import (
	"context"
	"fmt"
	"log/slog"
	"os"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/spf13/cobra"
)

func attachRunLog(cmd *cobra.Command, logLevel string, logFile **runlog.File) error {
	if cmd.Name() == "logs" {
		return nil
	}
	level, err := resolveLogLevel(logLevel, os.Getenv("PRSYNC_LOG"))
	if err != nil {
		return &ExitError{Code: ExitUsage, Err: err}
	}
	f, err := runlog.OpenDefault(level)
	if err != nil {
		f = runlog.Discard()
	}
	if logFile != nil {
		*logFile = f
	}
	cmd.SetContext(runlog.WithLogger(cmd.Context(), f.Logger))
	f.Logger.Info("command", "name", cmd.Name())
	return nil
}

func resolveLogLevel(flagValue, envValue string) (slog.Level, error) {
	if flagValue != "" {
		level, err := runlog.ParseLevel(flagValue)
		if err != nil {
			return 0, fmt.Errorf("log level: %w", err)
		}
		return level, nil
	}
	if envValue != "" {
		level, err := runlog.ParseLevel(envValue)
		if err != nil {
			return 0, fmt.Errorf("log level: %w", err)
		}
		return level, nil
	}
	return slog.LevelInfo, nil
}

func logStartup(ctx context.Context, command string, cfg config.Config) {
	runlog.FromContext(ctx).Info("startup",
		"command", command,
		"config_path", cfg.SourcePath,
		"concurrency_wait_on", cfg.ConcurrencyWaitOn,
	)
}
