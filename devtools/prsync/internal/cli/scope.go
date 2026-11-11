package cli

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

// scopeFlags are --tab-machine, --untabbed, and --skip. Each set flag
// overrides its config key.
type scopeFlags struct {
	tabMachine string
	untabbed   string
	skip       []string
}

var scopeFlagNames = map[string]string{
	"tab_machine": "--tab-machine",
	"untabbed":    "--untabbed",
	"skip_jobs":   "--skip",
}

// addTabMachineFlag registers --tab-machine alone (tabs).
func addTabMachineFlag(cmd *cobra.Command, f *scopeFlags) {
	cmd.Flags().StringVar(&f.tabMachine, "tab-machine", "", "only PRs whose tab is on this herdr machine: local or one of machines (overrides tab_machine)")
}

// addScopeFlags registers every scope flag (scan, dispatch).
func addScopeFlags(cmd *cobra.Command, f *scopeFlags) {
	addTabMachineFlag(cmd, f)
	cmd.Flags().StringVar(&f.untabbed, "untabbed", "", "include or exclude PRs with no tab; default only --tab-machine local includes them (overrides untabbed)")
	cmd.Flags().StringArrayVar(&f.skip, "skip", nil, "machine-wide job this run skips: "+strings.Join(config.Jobs, ", ")+" (repeatable; overrides skip_jobs)")
}

// apply sets the changed flags on cfg and validates them. An invalid
// value is a usage error naming its flag.
func (f scopeFlags) apply(cmd *cobra.Command, cfg *config.Config) error {
	flags := cmd.Flags()
	if flags.Changed("tab-machine") {
		cfg.TabMachine = f.tabMachine
	}
	if flags.Lookup("untabbed") != nil && flags.Changed("untabbed") {
		cfg.Untabbed = f.untabbed
	}
	if flags.Lookup("skip") != nil && flags.Changed("skip") {
		cfg.SkipJobs = config.ParseJobs(strings.Join(f.skip, ","))
	}
	if err := cfg.Validate(); err != nil {
		var keyErr *config.KeyError
		if errors.As(err, &keyErr) {
			if name, ok := scopeFlagNames[keyErr.Key]; ok {
				return &ExitError{Code: ExitUsage, Err: fmt.Errorf("%s: %s", name, keyErr.Reason)}
			}
		}
		return fmt.Errorf("validate config: %w", err)
	}
	return nil
}

// tabsHerdr lists and closes tabs for `tabs`: the local server, or
// cfg.TabMachine when it names another machine.
type tabsHerdr interface {
	RequireMin(ctx context.Context, minimum string) error
	TabList(ctx context.Context) ([]herdr.Tab, error)
	AgentList(ctx context.Context) ([]herdr.Agent, error)
	TabClose(ctx context.Context, tabID string) error
}

// remoteTabs is a machine's herdr client whose version check runs on the
// local binary that reaches it.
type remoteTabs struct {
	*herdr.Client
	local *herdr.Client
}

func (r remoteTabs) RequireMin(ctx context.Context, minimum string) error {
	return r.local.RequireMin(ctx, minimum) //nolint:wrapcheck // same error as the local client
}

func newTabsHerdr(exec executor.Executor, cfg config.Config) tabsHerdr {
	local := herdr.NewClient(exec, cfg.HerdrBin)
	if cfg.TabMachine == "" || cfg.TabMachine == config.LocalMachine {
		return local
	}
	return remoteTabs{Client: machineHerdr(exec, cfg, cfg.TabMachine), local: local}
}
