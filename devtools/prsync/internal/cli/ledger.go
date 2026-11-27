package cli

import (
	"encoding/json"
	"fmt"
	"io"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/spf13/cobra"
)

func newLedgerCmd(stdout io.Writer) *cobra.Command {
	var (
		configPath string
		pr         string
		kind       string
		head       string
		limit      int
		jsonOut    bool
	)
	cmd := &cobra.Command{
		Use:   "ledger",
		Short: "Query the dispatch ledger",
		Args:  cobra.NoArgs,
		RunE: func(_ *cobra.Command, _ []string) error {
			cfg, err := config.Load(configPath)
			if err != nil {
				return fmt.Errorf("load config: %w", err)
			}
			store := &ledger.Store{Path: cfg.Policy.LedgerFile}
			entries, err := store.Query(ledger.Filter{
				PR:      pr,
				Kind:    kind,
				HeadSHA: head,
				Limit:   limit,
			})
			if err != nil {
				return &ExitError{Code: ExitPrecondition, Err: err}
			}
			if jsonOut {
				data, err := json.MarshalIndent(entries, "", "  ")
				if err != nil {
					return fmt.Errorf("encode ledger: %w", err)
				}
				if _, err := fmt.Fprintf(stdout, "%s\n", data); err != nil {
					return fmt.Errorf("write ledger JSON: %w", err)
				}
				return nil
			}
			if _, err := io.WriteString(stdout, ledger.FormatLines(entries)); err != nil {
				return fmt.Errorf("write ledger output: %w", err)
			}
			return nil
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().StringVar(&pr, "pr", "", "filter by owner/repo#N")
	cmd.Flags().StringVar(&kind, "kind", "", "filter by kind (catch-up, rebase, nit-reply, ci-fix, queue-fix, bot-rereview, waive-repost)")
	cmd.Flags().StringVar(&head, "head", "", "filter by head SHA")
	cmd.Flags().IntVar(&limit, "limit", 0, "show only the last N matching entries")
	cmd.Flags().BoolVar(&jsonOut, "json", false, "output entries as JSON")
	return cmd
}
