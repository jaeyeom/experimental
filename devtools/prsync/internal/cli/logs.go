package cli

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/spf13/cobra"
)

func newLogsCmd(stdout io.Writer) *cobra.Command {
	var showPath bool
	cmd := &cobra.Command{
		Use:   "logs",
		Short: "Show the persistent run log (or print its path)",
		Args:  cobra.NoArgs,
		RunE: func(_ *cobra.Command, _ []string) error {
			return runLogs(stdout, showPath)
		},
	}
	cmd.Flags().BoolVar(&showPath, "path", false, "print the log file path as JSON")
	return cmd
}

func runLogs(stdout io.Writer, showPath bool) error {
	path, err := runlog.FilePath()
	if err != nil {
		return fmt.Errorf("run log path: %w", err)
	}
	if showPath {
		out, err := json.Marshal(map[string]string{"path": path})
		if err != nil {
			return fmt.Errorf("encode log path: %w", err)
		}
		if _, err := fmt.Fprintf(stdout, "%s\n", out); err != nil {
			return fmt.Errorf("write log path: %w", err)
		}
		return nil
	}
	f, err := os.Open(path) //nolint:gosec // operator-local state file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil
		}
		return fmt.Errorf("open run log: %w", err)
	}
	defer f.Close() //nolint:errcheck
	if _, err := io.Copy(stdout, f); err != nil {
		return fmt.Errorf("read run log: %w", err)
	}
	return nil
}
