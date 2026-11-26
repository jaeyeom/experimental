package cli

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"
)

func newUpdateCmd(ctx context.Context) *cobra.Command {
	var check bool
	var ref string
	var asJSON bool
	cmd := &cobra.Command{
		Use:           "update",
		Short:         "Update factory to the latest version",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("update: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			code, err := factoryTool().RunUpdate(ctx, check, ref, asJSON, cmd.OutOrStdout())
			if err != nil {
				return exitStatusError{code: code, err: err}
			}
			if code != 0 {
				return silentExitError{code: code}
			}
			return nil
		},
	}
	cmd.Flags().BoolVar(&check, "check", false, "check if a newer revision exists without installing")
	cmd.Flags().StringVar(&ref, "ref", "latest", "target branch, tag, or version")
	cmd.Flags().BoolVar(&asJSON, "json", false, "output JSON")
	return cmd
}
