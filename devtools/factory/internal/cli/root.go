// Package cli is the factory command line.
package cli

import (
	"context"
	"fmt"
	"io"
	"log/slog"

	"github.com/spf13/cobra"
)

// Execute runs the factory CLI with args and writes to stdout and stderr.
// It returns a process exit code.
func Execute(ctx context.Context, args []string, stdout, stderr io.Writer) int {
	if args == nil {
		args = []string{}
	}
	root := newRoot(ctx, stderr)
	root.SetOut(stdout)
	root.SetErr(stderr)
	root.SetArgs(args)
	root.SetContext(ctx)
	return exitCode(root.Execute(), stderr)
}

func newRoot(ctx context.Context, stderr io.Writer) *cobra.Command {
	root := &cobra.Command{
		Use:           "factory",
		Short:         "Pick the next ready work item and start an agent on it",
		SilenceErrors: true,
		SilenceUsage:  true,
	}
	// --config is read by loadConfig. init, version, and skill do not load a config.
	root.PersistentFlags().String("config", "", "path to config file")
	// --db overrides the default store path. It is hidden; tests pass a temp file.
	root.PersistentFlags().String("db", "", "path to the factory database")
	if err := root.PersistentFlags().MarkHidden("db"); err != nil {
		panic(fmt.Sprintf("hide --db: expected the flag, found error %v", err))
	}
	level := logLevel("info")
	root.PersistentFlags().Var(&level, "log-level", "log level: debug, info, warn, or error")
	root.PersistentPreRunE = func(_ *cobra.Command, _ []string) error {
		return setupLogger(string(level), stderr)
	}
	root.SetFlagErrorFunc(func(_ *cobra.Command, err error) error {
		return usageError{err: err}
	})
	root.AddCommand(newVersionCmd())
	root.AddCommand(newUpdateCmd(ctx))
	root.AddCommand(newSkillCmd())
	root.AddCommand(newApproveCmd(ctx))
	root.AddCommand(newSyncCmd(ctx))
	root.AddCommand(newNextCmd(ctx))
	root.AddCommand(newPrioritiesCmd(ctx))
	root.AddCommand(newShowCmd(ctx))
	root.AddCommand(newOwnerCmd(ctx))
	root.AddCommand(newVerifyCmd(ctx))
	root.AddCommand(newGateCmd(ctx))
	root.AddCommand(newHoldCmd())
	root.AddCommand(newReleaseHoldCmd())
	root.AddCommand(newInitCmd(ctx))
	root.AddCommand(newAdoptCmd(ctx))
	root.AddCommand(newClaimPRCmd(ctx))
	root.AddCommand(newStartCmd(ctx))
	root.AddCommand(newTickCmd(ctx))
	root.AddCommand(newSettleCmd(ctx))
	root.AddCommand(newRunCmd(ctx))
	root.AddCommand(newReleaseCmd(ctx))
	root.AddCommand(newLinkCmd(ctx))
	root.AddCommand(newUnlinkCmd(ctx))
	root.AddCommand(newFileCmd(ctx))
	root.AddCommand(newCloseCmd(ctx))
	root.AddCommand(newDoctorCmd(ctx))
	root.AddCommand(newLogsCmd(ctx))
	root.AddCommand(newStatsCmd(ctx))
	return root
}

// logLevel rejects any value other than debug, info, warn, or error at parse time.
type logLevel string

func (l *logLevel) String() string {
	if l == nil {
		return ""
	}
	return string(*l)
}

func (l *logLevel) Set(s string) error {
	if _, err := parseLogLevel(s); err != nil {
		return err
	}
	*l = logLevel(s)
	return nil
}

func (l *logLevel) Type() string { return "string" }

func parseLogLevel(level string) (slog.Level, error) {
	switch level {
	case "debug":
		return slog.LevelDebug, nil
	case "info":
		return slog.LevelInfo, nil
	case "warn":
		return slog.LevelWarn, nil
	case "error":
		return slog.LevelError, nil
	default:
		return 0, fmt.Errorf("invalid --log-level %q: expected debug, info, warn, or error", level)
	}
}

func setupLogger(level string, stderr io.Writer) error {
	slogLevel, err := parseLogLevel(level)
	if err != nil {
		return usageError{err: err}
	}
	slog.SetDefault(slog.New(slog.NewTextHandler(stderr, &slog.HandlerOptions{Level: slogLevel})))
	return nil
}
