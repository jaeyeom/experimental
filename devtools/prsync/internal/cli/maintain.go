package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/backend"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/lease"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/policy"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

func newMaintainCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var (
		configPath string
		prs        []string
		jsonOut    bool
		force      bool
		readStdin  bool
	)
	cmd := &cobra.Command{
		Use:   "maintain",
		Short: "Evaluate configured PR maintenance policies and dispatch eligible prompts",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			for _, p := range prs {
				if _, _, err := dispatch.ParsePR(p); err != nil {
					return &ExitError{Code: ExitUsage, Err: err}
				}
			}
			cfg, err := config.Load(configPath)
			if err != nil {
				return fmt.Errorf("load config: %w", err)
			}
			logStartup(cmd.Context(), "maintain", cfg)
			scanDoc, err := loadMaintainDoc(cmd.Context(), cfg, exec, readStdin, prs)
			if err != nil {
				return err
			}
			eng := &policy.Engine{
				Backend: buildBackend(exec, cfg),
				Lease:   buildLeaseProvider(exec, cfg),
				Ledger:  &ledger.Store{Path: cfg.Policy.LedgerFile},
				Guard:   policy.ExecGuardRunner{Exec: exec},
				GH:      gh.NewClient(exec, cfg.GHBin),
			}
			doc, err := eng.Run(cmd.Context(), cfg, scanDoc, policy.RunOptions{Force: force})
			if err != nil {
				return scanExit(err, cfg)
			}
			if err := writeMaintainOutput(stdout, doc, jsonOut); err != nil {
				return err
			}
			for _, d := range doc.Decisions {
				if d.Decision == policy.DecisionError {
					return &ExitError{Code: ExitPrecondition, Err: errors.New("one or more dispatches failed")}
				}
			}
			return nil
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().StringArrayVar(&prs, "pr", nil, "evaluate only owner/repo#N instead of a full scan (repeatable)")
	cmd.Flags().BoolVar(&jsonOut, "json", false, "output decisions as JSON")
	cmd.Flags().BoolVar(&force, "force", false, "re-dispatch even if the dispatch ledger already recorded this head")
	cmd.Flags().BoolVar(&readStdin, "stdin", false, "read a scan document from stdin")
	return cmd
}

func loadMaintainDoc(ctx context.Context, cfg config.Config, exec executor.Executor, readStdin bool, prs []string) (scan.Document, error) {
	doc, fromStdin, err := readStdinScan(os.Stdin, readStdin)
	if err != nil {
		return scan.Document{}, &ExitError{Code: ExitUsage, Err: fmt.Errorf("stdin scan JSON: %w", err)}
	}
	if fromStdin {
		return doc, nil
	}
	deps, saveCache := scanDeps(ctx, exec, cfg, herdr.NewClient(exec, cfg.HerdrBin))
	defer saveCache()
	if c, ok := deps.GH.(*gh.Client); ok && len(cfg.Policy.HoldLabels) > 0 {
		c.WithLabels()
	}
	if len(prs) > 0 {
		targets := make([]scan.Target, 0, len(prs))
		for _, raw := range prs {
			repo, n, err := dispatch.ParsePR(raw)
			if err != nil {
				return scan.Document{}, &ExitError{Code: ExitUsage, Err: err}
			}
			targets = append(targets, scan.Target{Repo: repo, Number: n})
		}
		res, err := scan.RunTargets(ctx, deps, cfg, targets, time.Now())
		return finishScanLoad(res, err, cfg)
	}
	res, err := scan.Run(ctx, deps, cfg, nil, time.Now())
	return finishScanLoad(res, err, cfg)
}

func buildBackend(exec executor.Executor, cfg config.Config) policy.Backend {
	if cfg.Policy.AgentBackend == config.BackendCommand {
		return &backend.CommandBackend{
			Exec: exec,
			Bin:  cfg.Policy.AgentBackendCmd,
		}
	}
	return &backend.HerdrBackend{
		Local:     herdr.NewClient(exec, cfg.HerdrBin),
		WaitUntil: cfg.WaitUntil,
	}
}

func buildLeaseProvider(exec executor.Executor, cfg config.Config) policy.LeaseProvider {
	switch cfg.Policy.LeaseProvider {
	case config.LeaseFile:
		return &lease.FileProvider{Dir: cfg.Policy.LeaseDir}
	case config.LeaseAgentbus:
		return &lease.AgentbusProvider{Exec: exec, Bin: "agentbus"}
	case config.LeaseCommand:
		return &lease.CommandProvider{
			Exec:       exec,
			ClaimCmd:   cfg.Policy.LeaseClaimCmd,
			ReleaseCmd: cfg.Policy.LeaseReleaseCmd,
		}
	default:
		return lease.NoopProvider{}
	}
}

func writeMaintainOutput(w io.Writer, doc policy.Document, jsonOut bool) error {
	if jsonOut {
		data, err := json.MarshalIndent(doc, "", "  ")
		if err != nil {
			return fmt.Errorf("encode maintain output: %w", err)
		}
		if _, err := fmt.Fprintf(w, "%s\n", data); err != nil {
			return fmt.Errorf("write maintain JSON: %w", err)
		}
		return nil
	}
	if _, err := io.WriteString(w, policy.FormatLines(doc)); err != nil {
		return fmt.Errorf("write maintain output: %w", err)
	}
	return nil
}
