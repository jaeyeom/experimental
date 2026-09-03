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
		configPath  string
		orphans     bool
		jsonFlag    bool
		readStdin   bool
		closeMerged bool
		goLive      bool
		force       bool
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
			if goLive && !closeMerged {
				return &ExitError{Code: ExitUsage, Err: errors.New("--go requires --close-merged")}
			}
			return runTabsOrphans(cmd.Context(), stdout, exec, configPath, readStdin, closeMerged, goLive, force)
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().BoolVar(&orphans, "orphans", false, "report live-agent tabs with no open/draft PR")
	cmd.Flags().BoolVar(&jsonFlag, "json", false, "accepted, no-op (stdout is always JSON)")
	cmd.Flags().BoolVar(&readStdin, "stdin", false, "reuse open-PR matches from a scan document on stdin")
	cmd.Flags().BoolVar(&closeMerged, "close-merged", false, "close merged-bucket orphan tabs (default is dry-run)")
	cmd.Flags().BoolVar(&goLive, "go", false, "actually close tabs (default is dry-run)")
	cmd.Flags().BoolVar(&force, "force", false, "allow --close-merged when classification is all no_pr or unknown")
	return cmd
}

func runTabsOrphans(ctx context.Context, stdout io.Writer, exec executor.Executor, configPath string, readStdin, closeMerged, goLive, force bool) error {
	cfg, err := config.Load(configPath)
	if err != nil {
		return fmt.Errorf("load config: %w", err)
	}
	if goLive {
		cfg.DryRun = false
	}
	openTabs, err := openTabsFromStdin(readStdin)
	if err != nil {
		return err
	}
	client := herdr.NewClient(exec, cfg.HerdrBin)
	deps := scan.OrphanDeps{
		GH:    gh.NewClient(exec, cfg.GHBin),
		Herdr: client,
	}
	doc, err := scan.Orphans(ctx, deps, cfg, openTabs, time.Now())
	if err != nil {
		if scan.OrphansStarted(doc) {
			if werr := writeOrphanJSON(stdout, doc); werr != nil {
				return werr
			}
		}
		return gateError(err, cfg)
	}
	if !closeMerged {
		return writeOrphanJSON(stdout, doc)
	}
	if !force {
		if reason := scan.UntrustedCloseReason(doc.OrphanTabs); reason != nil {
			warnings := append(append([]string{}, doc.Warnings...), reason.Error())
			out := scan.CloseDocument{
				GeneratedAt: time.Now().UTC().Format(time.RFC3339),
				DryRun:      cfg.DryRun,
				Results:     []scan.CloseItem{},
				Warnings:    warnings,
			}
			if werr := writeCloseJSON(stdout, out); werr != nil {
				return werr
			}
			return gateError(reason, cfg)
		}
	}
	out, closeErr := scan.CloseMerged(ctx, scan.CloseDeps{
		GH:    deps.GH,
		Herdr: client,
	}, cfg, doc.Author, doc.OrphanTabs, time.Now())
	if werr := writeCloseJSON(stdout, out); werr != nil {
		return werr
	}
	if closeErr != nil {
		return gateError(closeErr, cfg)
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

func writeCloseJSON(w io.Writer, doc scan.CloseDocument) error {
	out, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode tabs: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write tabs: %w", err)
	}
	return nil
}
