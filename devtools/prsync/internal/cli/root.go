// Package cli implements the prsync cobra command tree.
package cli

import (
	"context"
	"io"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

// Execute runs the prsync CLI with injected IO and returns an exit code.
func Execute(ctx context.Context, args []string, stdout, stderr io.Writer, exec executor.Executor) int {
	var logFile *runlog.File
	root := newRoot(stdout, exec, &logFile)
	root.SetArgs(args)
	root.SetOut(stdout)
	root.SetErr(stderr)
	err := root.ExecuteContext(ctx)
	code := report(stderr, err)
	if logFile != nil {
		attrs := []any{"code", code}
		if err != nil {
			attrs = append(attrs, "error", err.Error())
		}
		logFile.Logger.Info("exit", attrs...)
		_ = logFile.Close()
	}
	return code
}

func newRoot(stdout io.Writer, exec executor.Executor, logFile **runlog.File) *cobra.Command {
	var logLevel string
	root := &cobra.Command{
		Use:           "prsync",
		Short:         "Survey open GitHub PRs and match them to herdr agent tabs",
		SilenceErrors: true,
		PersistentPreRunE: func(cmd *cobra.Command, _ []string) error {
			cmd.SilenceUsage = true
			return attachRunLog(cmd, logLevel, logFile)
		},
	}
	root.PersistentFlags().StringVar(&logLevel, "log-level", "", "log verbosity (debug, info, warn, error); default info; also PRSYNC_LOG")
	root.AddCommand(newVersionCmd(stdout))
	root.AddCommand(newScanCmd(stdout, exec))
	root.AddCommand(newTabsCmd(stdout, exec))
	root.AddCommand(newDispatchCmd(stdout, exec))
	root.AddCommand(newCommentCmd(stdout, exec))
	root.AddCommand(newGateCmd(stdout, exec))
	root.AddCommand(newLogsCmd(stdout))
	return root
}
