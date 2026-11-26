package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/stats"
	"github.com/jaeyeom/experimental/devtools/factory/internal/term"
)

type statsOpts struct {
	since    string
	by       string
	split    string
	epic     string
	asJSON   bool
	color    string
	ascii    bool
	burndown string
	pace     bool
	utc      bool
	barWidth int
	scope    scopeFlags
}

// Color modes for stats --color.
const (
	colorAuto   = "auto"
	colorAlways = "always"
	colorNever  = "never"
	// fallbackWidth is the chart width when stdout is not a terminal.
	fallbackWidth = 80
)

// statsTerminal reports the stdout terminal width, and false when stdout is
// not a terminal. Tests replace it.
var statsTerminal = func(cmd *cobra.Command) (int, bool) {
	f, ok := cmd.OutOrStdout().(*os.File)
	if !ok {
		return 0, false
	}
	return term.Width(f)
}

func newStatsCmd(ctx context.Context) *cobra.Command {
	var opts statsOpts
	cmd := &cobra.Command{
		Use:           "stats",
		Short:         "Report throughput, backlog, epic progress, and flow from the local store",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("stats: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runStats(ctx, cmd, opts)
		},
	}
	cmd.Flags().StringVar(&opts.since, "since", "7d", "window length: Nd, Nw, or a duration such as 36h")
	cmd.Flags().StringVar(&opts.by, "by", "", "throughput bucket: day or week (default day up to 14d, else week)")
	cmd.Flags().StringVar(&opts.split, "split", "", "add throughput and flow per model or rule")
	cmd.Flags().StringVar(&opts.epic, "epic", "", "limit every section to this item's subtree and print the tree")
	cmd.Flags().BoolVar(&opts.asJSON, "json", false, "print the report as JSON")
	cmd.Flags().StringVar(&opts.color, "color", colorAuto, "color charts and status cells: auto (a terminal without NO_COLOR), always, or never")
	cmd.Flags().BoolVar(&opts.ascii, "ascii", false, "draw charts with #, =, . and * instead of blocks and braille")
	cmd.Flags().StringVar(&opts.burndown, "burndown", stats.BurndownBraille, "burndown chart: braille or bars")
	cmd.Flags().BoolVar(&opts.pace, "burndown-pace", false, "add a dotted pace line to zero by the window end to the braille burndown (reference only)")
	cmd.Flags().IntVar(&opts.barWidth, "bar-width", stats.DefaultBarWidth, "cells in the bar column of the throughput, backlog, and epics tables; 0 for none")
	cmd.Flags().BoolVar(&opts.utc, "utc", false, "bucket and print times in UTC instead of the local zone")
	addScopeFlags(cmd, &opts.scope)
	return cmd
}

func statsOptions(opts statsOpts) (stats.Options, error) {
	since, err := stats.ParseSince(opts.since)
	if err != nil {
		return stats.Options{}, usageError{err: err}
	}
	switch opts.by {
	case "", stats.ByDay, stats.ByWeek:
	default:
		return stats.Options{}, usageError{err: fmt.Errorf("stats --by: expected day or week, found %q", opts.by)}
	}
	switch opts.split {
	case "", stats.SplitModel, stats.SplitRule:
	default:
		return stats.Options{}, usageError{err: fmt.Errorf("stats --split: expected model or rule, found %q", opts.split)}
	}
	switch opts.color {
	case colorAuto, colorAlways, colorNever:
	default:
		return stats.Options{}, usageError{err: fmt.Errorf("stats --color: expected auto, always, or never, found %q", opts.color)}
	}
	switch opts.burndown {
	case stats.BurndownBraille, stats.BurndownBars:
	default:
		return stats.Options{}, usageError{err: fmt.Errorf("stats --burndown: expected braille or bars, found %q", opts.burndown)}
	}
	if err := opts.scope.check("stats"); err != nil {
		return stats.Options{}, err
	}
	if opts.barWidth < 0 {
		return stats.Options{}, usageError{err: fmt.Errorf("stats --bar-width: expected 0 or more, found %d", opts.barWidth)}
	}
	loc := time.Local
	if opts.utc {
		loc = time.UTC
	}
	return stats.Options{Now: nowUTC(), Since: since, By: opts.by, Split: opts.split, Epic: opts.epic, Loc: loc}, nil
}

// textOptions picks the chart width and color. A terminal gets its width;
// anything else gets fallbackWidth. --color auto colors only a terminal
// without NO_COLOR.
func textOptions(cmd *cobra.Command, opts statsOpts, noColor bool) stats.TextOptions {
	width, tty := statsTerminal(cmd)
	if !tty {
		width = fallbackWidth
	}
	color := opts.color == colorAlways || (opts.color == colorAuto && tty && !noColor)
	return stats.TextOptions{Width: width, Color: color, ASCII: opts.ascii, Burndown: opts.burndown, BurndownPace: opts.pace, BarWidth: opts.barWidth}
}

// runStats reads only the local store. It does not sync and calls no
// herdr, gh, or tracker command.
func runStats(ctx context.Context, cmd *cobra.Command, opts statsOpts) (err error) {
	if ctx == nil {
		return errors.New("stats: expected a context, found nil")
	}
	in, err := statsOptions(opts)
	if err != nil {
		return err
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	if in.Source, err = opts.scope.resolve(cmd, cfg); err != nil {
		return err
	}
	if _, err := sourcesFor(cfg, in.Source); err != nil {
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
	rep, err := stats.Build(ctx, st, cfg, in)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	out := cmd.OutOrStdout()
	if !opts.asJSON {
		return rep.WriteTextWith(out, textOptions(cmd, opts, cfg.NoColor)) //nolint:wrapcheck // internal factory error already carries context
	}
	enc := json.NewEncoder(out)
	enc.SetIndent("", "  ")
	if err := enc.Encode(rep); err != nil {
		return fmt.Errorf("write stats: %w", err)
	}
	return nil
}
