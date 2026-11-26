package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func newGateCmd(ctx context.Context) *cobra.Command {
	var scope scopeFlags
	var asJSON bool
	cmd := &cobra.Command{
		Use:           "gate",
		Short:         "Report whether another agent can start now",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("gate: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runGate(ctx, cmd, scope, asJSON)
		},
	}
	cmd.Flags().StringVar(&scope.only, "source", "", "checkout to inspect (kind:name, for example jira:acme)")
	addScopeFlags(cmd, &scope)
	cmd.Flags().BoolVar(&asJSON, "json", false, "print the gate result as JSON")
	return cmd
}

func runGate(ctx context.Context, cmd *cobra.Command, scope scopeFlags, asJSON bool) (err error) {
	if ctx == nil {
		return errors.New("gate: expected a context, found nil")
	}
	if err := scope.check("gate"); err != nil {
		return err
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	if cfg.HasLanes() {
		if scope.only != "" {
			return runLaneGate(ctx, cmd, cfg, scope.only, asJSON)
		}
		if !scope.all {
			if _, err := scope.resolve(cmd, cfg); err != nil {
				return err
			}
		}
		return runLaneGate(ctx, cmd, cfg, "", asJSON)
	}
	only, err := scope.resolve(cmd, cfg)
	if err != nil {
		return err
	}
	cwd, err := gateCwd(cfg, only)
	if err != nil {
		return err
	}
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
	}()
	in, err := gateInputs(ctx, st, cfg)
	if err != nil {
		return err
	}
	return printGate(cmd.OutOrStdout(), gate.Check(ctx, in, cwd, cfg.Defaults), asJSON)
}

// gateInputs loads the global claims, the last start, and every herdr tab and pane.
// --source selects the checkout before this runs. It does not filter these inputs.
func gateInputs(ctx context.Context, st *store.Store, cfg *config.Config) (gate.Inputs, error) {
	if cfg == nil {
		return gate.Inputs{}, errors.New("gate: expected a config, found nil")
	}
	claims, err := st.ActiveClaims(ctx)
	if err != nil {
		return gate.Inputs{}, err //nolint:wrapcheck // propagate context cancellation
	}
	started, err := st.LastStartedAt(ctx)
	if err != nil {
		return gate.Inputs{}, err //nolint:wrapcheck // propagate context cancellation
	}
	exec := newCommandExec()
	if exec == nil {
		return gate.Inputs{}, errors.New("gate: expected an executor, found nil")
	}
	client := herdr.NewClient(exec, "herdr")
	tabs, err := client.Tabs(ctx)
	if err != nil {
		return gate.Inputs{}, err //nolint:wrapcheck // propagate context cancellation
	}
	panes, err := client.Panes(ctx)
	if err != nil {
		return gate.Inputs{}, err //nolint:wrapcheck // propagate context cancellation
	}
	pulls, err := ghClient(exec, cfg.GHBin)
	if err != nil {
		return gate.Inputs{}, err
	}
	prs, err := pulls.OpenByMe(ctx)
	if err != nil {
		return gate.Inputs{}, fmt.Errorf("gh open prs: %w", err)
	}
	checkouts, err := cfg.SourceCheckouts()
	if err != nil {
		return gate.Inputs{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	return gate.Inputs{
		Tabs:           tabs,
		Panes:          panes,
		CallerPaneID:   cfg.CallerPaneID,
		IgnorePanes:    cfg.Gate.IgnorePanes,
		IgnoreTabs:     cfg.Gate.IgnoreTabs,
		Checkouts:      checkouts,
		HoldFiles:      cfg.Gate.ActiveHoldFiles(),
		HoldTabs:       cfg.Gate.HoldTabs,
		HoldTTL:        cfg.Gate.HoldTTL,
		GitStatus:      gate.GitStatus(exec),
		BaseBranch:     cfg.Gate.BaseBranch,
		Require:        cfg.Gate.Require,
		GitBranch:      gate.GitBranch(exec),
		GitAhead:       gate.GitAhead(exec),
		ActiveClaims:   claims,
		Pulls:          prs,
		PRTitlePattern: cfg.PRTitlePattern,
		LastStartedAt:  started,
		Now:            time.Now().UTC(),
		LeaseGate:      cfg.Lease.Gate,
		Lease:          gate.LeaseReader(agentbus.NewClient(exec, "agentbus")),
		MaxWorkers:     cfg.Worker.MaxWorkers,
		Worktree:       cfg.Worker.Worktree,
	}, nil
}

// runLaneGate checks the global conditions once and then every lane.
// It prints the global result as gate does without lanes, then one line per lane.
func runLaneGate(ctx context.Context, cmd *cobra.Command, cfg *config.Config, only string, asJSON bool) (err error) {
	if only != "" {
		return usageError{err: fmt.Errorf("gate --source: expected no --source with [[lane]] config, found %q; each lane has its own checkout", only)}
	}
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
	}()
	claims, err := st.ActiveClaims(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	started, err := st.LastStartedAt(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	exec := newCommandExec()
	if exec == nil {
		return errors.New("gate: expected an executor, found nil")
	}
	env := laneEnv{exec: exec, cfg: cfg, claims: claims}
	if len(cfg.Gate.HoldTabs) > 0 || anyLocalLane(cfg.Lanes) {
		env.localTabs, err = herdr.NewClient(exec, "herdr").Tabs(ctx)
		if err != nil {
			return err //nolint:wrapcheck // propagate context cancellation
		}
		env.localLoaded = true
	}
	pulls, err := ghClient(exec, cfg.GHBin)
	if err != nil {
		return err
	}
	prs, err := pulls.OpenByMe(ctx)
	if err != nil {
		return fmt.Errorf("gh open prs: %w", err)
	}
	global := gate.CheckGlobal(globalInputs(cfg, env.localTabs, claims, prs, started), cfg.Defaults)
	results := make([]gate.LaneResult, 0, len(cfg.Lanes))
	for _, lane := range cfg.Lanes {
		view, err := env.check(ctx, lane)
		if err != nil {
			return err
		}
		results = append(results, view.result)
	}
	return printLaneGate(cmd.OutOrStdout(), global, results, asJSON)
}

func printLaneGate(w io.Writer, global gate.Result, lanes []gate.LaneResult, asJSON bool) error {
	if asJSON {
		return printGateJSON(w, global, lanes)
	}
	if err := printGate(w, global, false); err != nil {
		return err
	}
	for _, lane := range lanes {
		if _, err := fmt.Fprintln(w, lane.Line()); err != nil {
			return fmt.Errorf("write gate: %w", err)
		}
	}
	return nil
}

func printGate(w io.Writer, res gate.Result, asJSON bool) error {
	if asJSON {
		return printGateJSON(w, res, nil)
	}
	switch {
	case res.OK && len(res.Reasons) == 0:
		if _, err := fmt.Fprintf(w, "OK: %d in flight (max %d)\n", res.InFlight, res.MaxInFlight); err != nil {
			return fmt.Errorf("write gate: %w", err)
		}
	case !res.OK && len(res.Reasons) > 0:
		for _, reason := range res.Reasons {
			if _, err := fmt.Fprintln(w, reason); err != nil {
				return fmt.Errorf("write gate: %w", err)
			}
		}
	default:
		return fmt.Errorf("gate: expected OK or reasons, found ok=%t reasons=%d", res.OK, len(res.Reasons))
	}
	return printNotes(w, res.Notes)
}

type laneJSON struct {
	Name    string `json:"name"`
	OK      bool   `json:"ok"`
	Line    string `json:"line"`
	Skipped string `json:"skipped,omitempty"`
}

func printGateJSON(w io.Writer, res gate.Result, lanes []gate.LaneResult) error {
	if res.Reasons == nil {
		res.Reasons = []string{}
	}
	var payload any = res
	if lanes != nil {
		rows := make([]laneJSON, len(lanes))
		for i, lane := range lanes {
			rows[i] = laneJSON{Name: lane.Name, OK: lane.OK, Line: lane.Line(), Skipped: lane.Skipped}
		}
		payload = struct {
			gate.Result
			Lanes []laneJSON `json:"lanes"`
		}{res, rows}
	}
	raw, err := json.MarshalIndent(payload, "", "  ")
	if err != nil {
		return fmt.Errorf("gate json: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", raw); err != nil {
		return fmt.Errorf("write gate: %w", err)
	}
	return nil
}

// printNotes prints each gate note as "note: TEXT".
func printNotes(w io.Writer, notes []string) error {
	for _, note := range notes {
		if _, err := fmt.Fprintln(w, "note: "+note); err != nil {
			return fmt.Errorf("write gate: %w", err)
		}
	}
	return nil
}

// gateCwd returns the checkout Check should inspect.
// --source names one source. Without it, every source must share one cwd.
func gateCwd(cfg *config.Config, only string) (string, error) {
	srcs, err := sourcesFor(cfg, "")
	if err != nil {
		return "", err
	}
	if only != "" {
		return oneCwd(srcs, only)
	}
	return sharedCwd(srcs)
}

func oneCwd(srcs []config.Source, only string) (string, error) {
	for i := range srcs {
		key, err := sourceKey(srcs[i].Kind, srcs[i].Name)
		if err != nil {
			return "", err
		}
		if key != only {
			continue
		}
		if srcs[i].Cwd == "" {
			return "", fmt.Errorf("gate: source %s: expected a cwd, found empty", key)
		}
		return srcs[i].Cwd, nil
	}
	keys, err := sourceKeys(srcs)
	if err != nil {
		return "", err
	}
	return "", fmt.Errorf("gate --source: expected one of %s, found %q", keys, only)
}

func sharedCwd(srcs []config.Source) (string, error) {
	if len(srcs) == 0 {
		return "", errors.New("gate: expected at least one source, found none")
	}
	cwd := srcs[0].Cwd
	if cwd == "" {
		key, err := sourceKey(srcs[0].Kind, srcs[0].Name)
		if err != nil {
			return "", err
		}
		return "", fmt.Errorf("gate: source %s: expected a cwd, found empty", key)
	}
	for i := range srcs {
		if srcs[i].Cwd == cwd {
			continue
		}
		found, err := describeCheckouts(srcs)
		if err != nil {
			return "", err
		}
		return "", fmt.Errorf("gate: expected one checkout, found %s; pass --source", found)
	}
	return cwd, nil
}

func sourceKeys(srcs []config.Source) (string, error) {
	if len(srcs) == 0 {
		return "(none)", nil
	}
	keys := make([]string, len(srcs))
	for i := range srcs {
		key, err := sourceKey(srcs[i].Kind, srcs[i].Name)
		if err != nil {
			return "", err
		}
		keys[i] = key
	}
	return strings.Join(keys, ", "), nil
}

func describeCheckouts(srcs []config.Source) (string, error) {
	parts := make([]string, len(srcs))
	for i := range srcs {
		key, err := sourceKey(srcs[i].Kind, srcs[i].Name)
		if err != nil {
			return "", err
		}
		parts[i] = key + "=" + srcs[i].Cwd
	}
	return strings.Join(parts, ", "), nil
}
