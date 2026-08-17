// Package cli implements the prsync cobra command tree.
package cli

import (
	"context"
	"io"

	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

// Execute runs the prsync CLI with injected IO and returns an exit code.
func Execute(ctx context.Context, args []string, stdout, stderr io.Writer, exec executor.Executor) int {
	root := newRoot(stdout, exec)
	root.SetArgs(args)
	root.SetOut(stdout)
	root.SetErr(stderr)
	return report(stderr, root.ExecuteContext(ctx))
}

func newRoot(stdout io.Writer, exec executor.Executor) *cobra.Command {
	root := &cobra.Command{
		Use:           "prsync",
		Short:         "Survey open GitHub PRs and match them to herdr agent tabs",
		SilenceErrors: true,
		PersistentPreRun: func(cmd *cobra.Command, _ []string) {
			cmd.SilenceUsage = true
		},
	}
	root.AddCommand(newVersionCmd(stdout))
	root.AddCommand(newScanCmd(stdout, exec))
	return root
}
