package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

func newTabsCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var (
		configPath string
		orphans    bool
		jsonFlag   bool
		readStdin  bool
	)
	cmd := &cobra.Command{
		Use:   "tabs",
		Short: "Report herdr tabs to reclaim (live agent, no open/draft PR)",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			_ = jsonFlag
			if !orphans {
				return &ExitError{Code: ExitUsage, Err: errors.New("tabs requires --orphans")}
			}
			return runTabsOrphans(cmd.Context(), stdout, exec, configPath, readStdin)
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().BoolVar(&orphans, "orphans", false, "report live-agent tabs with no open/draft PR")
	cmd.Flags().BoolVar(&jsonFlag, "json", false, "accepted, no-op (stdout is always JSON)")
	cmd.Flags().BoolVar(&readStdin, "stdin", false, "reuse open-PR matches from a scan document on stdin")
	return cmd
}

func runTabsOrphans(ctx context.Context, stdout io.Writer, exec executor.Executor, configPath string, readStdin bool) error {
	cfg, err := config.Load(configPath)
	if err != nil {
		return fmt.Errorf("load config: %w", err)
	}
	openTabs, err := openTabsFromStdin(readStdin)
	if err != nil {
		return err
	}
	deps := scan.OrphanDeps{
		GH:    gh.NewClient(exec, cfg.GHBin),
		Herdr: herdr.NewClient(exec, cfg.HerdrBin),
	}
	doc, err := scan.Orphans(ctx, deps, cfg, openTabs, time.Now())
	if scan.OrphansStarted(doc) {
		if werr := writeOrphanJSON(stdout, doc); werr != nil {
			return werr
		}
	}
	if err != nil {
		return gateError(err, cfg)
	}
	return nil
}

// openTabsFromStdin returns the set of tab ids a piped scan document already
// matched to an open PR. Without --stdin (or with non-file stdin) it is nil, so
// every live-agent tab is resolved from scratch.
func openTabsFromStdin(readStdin bool) (map[string]struct{}, error) {
	doc, fromStdin, err := readStdinScan(os.Stdin, readStdin)
	if err != nil {
		return nil, &ExitError{Code: ExitUsage, Err: fmt.Errorf("stdin scan JSON: %w", err)}
	}
	if !fromStdin {
		return nil, nil
	}
	return dispatch.MatchedTabs(doc), nil
}

func writeOrphanJSON(w io.Writer, doc scan.OrphanDocument) error {
	out, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode tabs: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write tabs: %w", err)
	}
	return nil
}
