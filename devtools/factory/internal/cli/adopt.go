package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/adopt"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func newAdoptCmd(ctx context.Context) *cobra.Command {
	var pane, tab, machine string
	var prs []string
	cmd := &cobra.Command{
		Use:           "adopt <id>",
		Short:         "Record a claim for an existing item without starting an agent",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 1 {
				return usageError{err: fmt.Errorf("adopt: expected one id, found %d", len(args))}
			}
			if args[0] == "" {
				return usageError{err: errors.New("adopt: expected a non-empty id, found empty")}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runAdopt(ctx, cmd, args[0], adoptFlags{pane: pane, tab: tab, prs: prs, machine: machine})
		},
	}
	cmd.Flags().StringVar(&pane, "pane", "", "pane id; record the tab that contains it")
	cmd.Flags().StringVar(&tab, "tab", "", "tab id or tab label to record")
	cmd.Flags().StringArrayVar(&prs, "pr", nil, "pull request to record, as owner/repo#N or a URL (repeatable)")
	cmd.Flags().StringVar(&machine, "machine", "", "lane name or lane herdr machine the tab is on (needs [[lane]] config)")
	return cmd
}

// adoptFlags are the adopt command flags.
type adoptFlags struct {
	pane, tab string
	prs       []string
	machine   string
}

func runAdopt(ctx context.Context, cmd *cobra.Command, id string, flags adoptFlags) (err error) {
	if ctx == nil {
		return errors.New("adopt: expected a context, found nil")
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	if err := checkMachineFlag(cmd, cfg, flags.machine); err != nil {
		return err
	}
	pane, tab, prs := flags.pane, flags.tab, flags.prs
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
	}()
	it, srcCfg, err := startTarget(ctx, st, cfg, id)
	if err != nil {
		return err
	}
	snap, err := loadSnapshot(ctx, st, srcCfg)
	if err != nil {
		return err
	}
	lane, err := adoptLane(ctx, st, cfg, it, flags.machine)
	if err != nil {
		return err
	}
	snap, panes, err := adoptSessions(ctx, snap, lane, pane)
	if err != nil {
		return err
	}
	backend, err := oneSource(cfg, it.Source)
	if err != nil {
		return err
	}
	host, err := adoptHostname()
	if err != nil {
		return err
	}
	return adopt.Adopt(ctx, adopt.Input{ //nolint:wrapcheck // propagate context cancellation
		Src:      backend,
		Cfg:      srcCfg,
		Kind:     srcCfg.Kind,
		Store:    st,
		Snap:     snap,
		Panes:    panes,
		Hostname: host,
		Now:      func() time.Time { return time.Now().UTC() },
		Lane:     lane.Name,
	}, adopt.Request{ID: id, Pane: pane, Tab: tab, PRs: prs})
}

// adoptSessions reads the tabs and, for --pane, the panes on the lane's machine.
func adoptSessions(ctx context.Context, snap ready.Snapshot, lane config.Lane, pane string) (ready.Snapshot, []herdr.Pane, error) {
	client, err := laneHerdr(lane)
	if err != nil {
		return ready.Snapshot{}, nil, err
	}
	snap, err = onLane(ctx, snap, lane, client)
	if err != nil {
		return ready.Snapshot{}, nil, err
	}
	panes, err := adoptPanes(ctx, client, pane)
	if err != nil {
		return ready.Snapshot{}, nil, err
	}
	return snap, panes, nil
}

func adoptPanes(ctx context.Context, client *herdr.Client, pane string) ([]herdr.Pane, error) {
	if pane == "" {
		return nil, nil
	}
	if client == nil {
		return nil, errors.New("adopt: expected a herdr client, found nil")
	}
	panes, err := client.Panes(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	return panes, nil
}

// adoptLane is the lane whose herdr machine holds the item's tab.
// No lanes is the zero lane, the local server. An open claim's lane wins,
// and a --machine naming another lane is an error. Otherwise --machine
// picks one of the item's routed lanes, or the item routes to one lane.
// More than one possible lane without --machine is an error, not a guess.
func adoptLane(ctx context.Context, st *store.Store, cfg *config.Config, it source.Item, machine string) (config.Lane, error) {
	if !cfg.HasLanes() {
		return config.Lane{}, nil
	}
	routed, err := cfg.RouteLanes(it.Labels)
	if err != nil {
		return config.Lane{}, fmt.Errorf("adopt %s: %w", it.ID, err)
	}
	open, err := openClaimLane(ctx, st, it)
	if err != nil {
		return config.Lane{}, err
	}
	if open != "" {
		lane, err := claimLane(cfg, open)
		if err != nil {
			return config.Lane{}, fmt.Errorf("adopt %s: %w", it.ID, err)
		}
		if machine != "" && machine != lane.Name && machine != lane.Machine {
			return config.Lane{}, fmt.Errorf("adopt %s --machine %s: expected the open claim's lane %s, found another", it.ID, machine, lane.Name)
		}
		return lane, nil
	}
	if machine != "" {
		lane, err := cfg.PickLane(machine, routed)
		if err != nil {
			return config.Lane{}, fmt.Errorf("adopt %s: %w", it.ID, err)
		}
		return lane, nil
	}
	if len(routed) == 1 {
		return routed[0], nil
	}
	return config.Lane{}, fmt.Errorf("adopt %s: expected --machine naming one of %s, found none", it.ID, laneList(routed))
}

// openClaimLane is the lane on the item's open claim, or empty.
func openClaimLane(ctx context.Context, st *store.Store, it source.Item) (string, error) {
	claims, err := st.ActiveClaims(ctx)
	if err != nil {
		return "", err //nolint:wrapcheck // propagate context cancellation
	}
	for _, claim := range claims {
		if claim.Source == it.Source && claim.ID == it.ID {
			return claim.Lane, nil
		}
	}
	return "", nil
}

func adoptHostname() (string, error) {
	host, err := os.Hostname()
	if err != nil {
		return "", fmt.Errorf("adopt: hostname: %w", err)
	}
	if host == "" {
		return "", errors.New("adopt: expected a hostname, found empty")
	}
	return host, nil
}
