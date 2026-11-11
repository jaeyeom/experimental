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
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

const gateLong = `Check whether it is safe to start another agent, and print the gate JSON.

A working or blocked agent holds the gate (signal "blocked", detail
"blocked awaiting input"); set concurrency_ignore_blocked = true to let
a blocked agent pass. An idle tab still holds the gate for a background job (a child of its
agent at least gate_job_min_age old) or a pending decide question (an
item for its pane answered but not done, or opened since the round
started).

With machines set, agents on each machine in the tab_machine scope are
read through herdr --machine. An unreachable machine (signal
"machine-unreachable") or a remote tab whose signals cannot be read
(signal "signal-unavailable") holds the gate.

Config keys for the gate:
  gate_ignore_procs       Comma-separated patterns for agent helpers that
                          are never a background job. Each is checked
                          against the process name, the argv[0] base, and
                          the full command line (argv joined by spaces).
                          A plain pattern matches as a substring; one with
                          *, ?, or [...] is a glob (* matches /).
                          Whitespace around entries is trimmed, so a
                          pattern cannot contain a comma. Example:
                            gate_ignore_procs = claude-hud, statusline
                          Each skipped child is listed under "ignored"
                          with the pattern that matched.
  gate_job_min_age        Minimum child age for a background job (10s).
  gate_build_tools        Build-tool names, matched as the whole process
                          name or argv[0] base (or as globs).
  gate_build_tool_ignore  Patterns, matched like gate_ignore_procs, for
                          build processes that never count.
  gate_pending_question   decide (default) or off.
  gate_exempt_panes       Pane-id globs the gate never waits on.
  gate_exempt_tabs        Tab-label globs the gate never waits on, e.g.
                          pm*,merger. Listed under "exempt".
  concurrency_ignore_blocked
                          false (default) or true. When true, a blocked
                          status alone does not hold the gate.`

func newGateCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var configPath string
	cmd := &cobra.Command{
		Use:   "gate",
		Short: "Check whether it is safe to start another agent",
		Long:  gateLong,
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
	logStartup(ctx, "gate", cfg)
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

// gateProbes is what the gate reads beyond herdr agent status: the
// tab's process tree, the local process table, the decide ledger, and
// the tab labels for gate_exempt_tabs.
// Tests replace it so they never read real processes.
var gateProbes = func(exec executor.Executor, h *herdr.Client) dispatch.Probes {
	return dispatch.Probes{
		Panes:  h,
		Procs:  dispatch.NewProcessTable(exec),
		Decide: dispatch.DecideCLI{Exec: exec, Bin: "decide"},
		Tabs:   h,
		Self:   os.Getpid(),
	}
}

// withGateProbes attaches gateProbes, with the agents and panes on each
// of cfg.Machines read through `herdr --machine`.
func withGateProbes(ctx context.Context, exec executor.Executor, cfg config.Config, h *herdr.Client) context.Context {
	p := gateProbes(exec, h)
	p.Remote = func(machine string) dispatch.RemoteHerdr { return machineHerdr(exec, cfg, machine) }
	return dispatch.WithProbes(ctx, p)
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
	if cfg.Checkout == "" {
		cfg.Checkout = dispatch.CheckoutOfRepos(cfg.Repos)
	}
	if cfg.ConcurrencyCwd == "checkout" && cfg.Checkout == "" {
		cfg.Checkout = dispatch.CheckoutOfCWD()
	}
	ctx = withGateProbes(ctx, exec, cfg, h)
	res, err := dispatch.GateCheck(ctx, h, cfg, runner, matched)
	if err != nil {
		return dispatch.Result{}, fmt.Errorf("gate: %w", err)
	}
	return res, nil
}

func managedTabs(ctx context.Context, cfg config.Config, exec executor.Executor, h *herdr.Client) (map[string]struct{}, error) {
	if cfg.ConcurrencyWaitOn != "managed" {
		return nil, nil
	}
	deps := scanDeps(exec, cfg, h)
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
