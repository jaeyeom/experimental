package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

func newGateCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var configPath string
	cmd := &cobra.Command{
		Use:   "gate",
		Short: "Check whether it is safe to start another agent",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runGate(cmd.Context(), stdout, exec, configPath)
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	return cmd
}

func runGate(ctx context.Context, stdout io.Writer, exec executor.Executor, configPath string) error {
	cfg, err := config.Load(configPath)
	if err != nil {
		return fmt.Errorf("load config: %w", err)
	}
	res, err := evaluateGate(ctx, cfg, exec)
	if err != nil {
		return gateError(err, cfg)
	}
	if err := writeGateJSON(stdout, res); err != nil {
		return err
	}
	if !res.Safe {
		return &ExitError{Code: ExitUnsafe}
	}
	return nil
}

func evaluateGate(ctx context.Context, cfg config.Config, exec executor.Executor) (dispatch.Result, error) {
	h := herdr.NewClient(exec, cfg.HerdrBin)
	if err := h.RequireMin(ctx, "0.8.0"); err != nil {
		return dispatch.Result{}, fmt.Errorf("herdr version: %w", err)
	}
	runner := h.RunnerPane(ctx)
	matched, err := managedTabs(ctx, cfg, exec, h)
	if err != nil {
		return dispatch.Result{}, err
	}
	res, err := dispatch.Check(ctx, h, cfg.ConcurrencyWaitOn, runner, matched)
	if err != nil {
		return dispatch.Result{}, fmt.Errorf("gate: %w", err)
	}
	return res, nil
}

func managedTabs(ctx context.Context, cfg config.Config, exec executor.Executor, h *herdr.Client) (map[string]struct{}, error) {
	if cfg.ConcurrencyWaitOn != "managed" {
		return nil, nil
	}
	deps := scan.Deps{
		GH:    gh.NewClient(exec, cfg.GHBin),
		Herdr: h,
	}
	doc, err := scan.Run(ctx, deps, cfg, nil, time.Now())
	if err != nil {
		return nil, fmt.Errorf("scan: %w", err)
	}
	return dispatch.MatchedTabs(doc), nil
}

func writeGateJSON(w io.Writer, res dispatch.Result) error {
	out, err := json.MarshalIndent(res, "", "  ")
	if err != nil {
		return fmt.Errorf("encode gate: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write gate: %w", err)
	}
	return nil
}

func gateError(err error, cfg config.Config) error {
	if errors.Is(err, herdr.ErrNotInstalled) {
		return &ExitError{Code: ExitPrecondition, Err: fmt.Errorf("herdr binary not found (herdr_bin=%q)", cfg.HerdrBin)}
	}
	return scanExit(err, cfg)
}
