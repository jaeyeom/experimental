package cli

import (
	"encoding/json"
	"fmt"
	"io"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/version"
	"github.com/spf13/cobra"
)

func newVersionCmd(stdout io.Writer) *cobra.Command {
	return &cobra.Command{
		Use:   "version",
		Short: "Print the prsync version as JSON",
		Args:  cobra.NoArgs,
		RunE: func(_ *cobra.Command, _ []string) error {
			out, err := json.Marshal(map[string]string{"version": version.Version})
			if err != nil {
				return fmt.Errorf("encode version: %w", err)
			}
			_, err = fmt.Fprintf(stdout, "%s\n", out)
			if err != nil {
				return fmt.Errorf("write version: %w", err)
			}
			return nil
		},
	}
}
