package cli

import (
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"slices"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/priority"
	"github.com/jaeyeom/experimental/devtools/factory/internal/rank"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/snapshot"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

type nextOpts struct {
	limit     int
	only      string
	all       bool
	asJSON    bool
	explain   string
	sync      bool
	noSync    bool
	human     bool
	onlyLabel []string
	mine      bool
	deferred  bool
}

type picked struct {
	source string
	item   source.Item
	score  rank.Score
	order  []string
}

type counts struct {
	open        int
	blocked     int
	notEligible int
	deferred    []picked // Items whose not_before date is still ahead.
}

func newNextCmd(ctx context.Context) *cobra.Command {
	var opts nextOpts
	cmd := &cobra.Command{
		Use:           "next",
		Short:         "List the next ready work items",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("next: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runNext(ctx, cmd, opts)
		},
	}
	cmd.Flags().IntVarP(&opts.limit, "n", "n", 10, "maximum ready items to print")
	cmd.Flags().StringVar(&opts.only, "source", "", "limit to this source key (kind:name, for example jira:acme)")
	cmd.Flags().BoolVar(&opts.all, "all", false, "show every source, not only the one whose cwd holds the current directory")
	cmd.Flags().BoolVar(&opts.asJSON, "json", false, "print the ready items as JSON")
	cmd.Flags().StringVar(&opts.explain, "explain", "", "print readiness checks and the score for this item ID")
	cmd.Flags().BoolVar(&opts.human, "human", false, "list ready items that carry a human_labels label")
	cmd.Flags().StringArrayVar(&opts.onlyLabel, "only-label", nil, "list ready items with this label (repeatable; replaces human_labels)")
	cmd.Flags().BoolVar(&opts.mine, "mine", false, "with --human or --only-label, keep items assigned to you")
	cmd.Flags().BoolVar(&opts.sync, "sync", false, "refresh the mirror before ranking")
	cmd.Flags().BoolVar(&opts.noSync, "no-sync", false, "do not refresh a stale mirror")
	cmd.Flags().BoolVar(&opts.deferred, "deferred", false, "list deferred items (not_before date ahead), soonest first")
	return cmd
}

func runNext(ctx context.Context, cmd *cobra.Command, opts nextOpts) (err error) {
	if ctx == nil {
		return errors.New("next: expected a context, found nil")
	}
	scope := scopeFlags{only: opts.only, all: opts.all}
	if err := errors.Join(validateNext(opts), scope.check("next")); err != nil {
		return err
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	// --explain names one item, so the current directory does not narrow it.
	if opts.explain == "" {
		if opts.only, err = scope.resolve(cmd, cfg); err != nil {
			return err
		}
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
	if _, err := sourcesFor(cfg, opts.only); err != nil {
		return err
	}
	if opts.sync {
		if err := syncBeforeNext(ctx, cmd, cfg, st, opts.only); err != nil {
			return err
		}
	} else if err := refreshBeforeRead(ctx, cmd, cfg, st, opts.only, opts.noSync); err != nil {
		return err
	}
	if opts.explain != "" {
		return explainItem(ctx, cmd, st, cfg, opts.explain, opts.only)
	}
	return printRanked(ctx, cmd, st, cfg, opts)
}

func validateNext(opts nextOpts) error {
	if opts.limit < 1 {
		return usageError{err: fmt.Errorf("next: expected -n >= 1, found %d", opts.limit)}
	}
	if opts.explain != "" && opts.asJSON {
		return usageError{err: errors.New("next: expected --explain or --json, found both")}
	}
	if opts.sync && opts.noSync {
		return usageError{err: errors.New("next: expected --sync or --no-sync, found both")}
	}
	if opts.explain != "" && humanListing(opts) {
		return usageError{err: errors.New("next: expected --explain or --human/--only-label, found both")}
	}
	for _, label := range opts.onlyLabel {
		if label == "" {
			return usageError{err: errors.New("next: expected a non-empty --only-label, found empty")}
		}
	}
	if opts.deferred && (opts.explain != "" || humanListing(opts)) {
		return usageError{err: errors.New("next: expected --deferred alone, found it with --explain, --human, or --only-label")}
	}
	if opts.mine && !humanListing(opts) {
		return usageError{err: errors.New("next: expected --mine with --human or --only-label, found --mine alone")}
	}
	return nil
}

func humanListing(opts nextOpts) bool {
	return opts.human || len(opts.onlyLabel) > 0
}

func syncBeforeNext(ctx context.Context, cmd *cobra.Command, cfg *config.Config, st *store.Store, only string) error {
	deps, err := sourceDeps()
	if err != nil {
		return err
	}
	srcs, err := buildSources(cfg, deps)
	if err != nil {
		return err
	}
	results, err := syncer.Sync(ctx, st, srcs, nowUTC(), syncer.Options{
		Only:            only,
		FullSyncEvery:   cfg.Defaults.FullSyncEvery,
		AfterSync:       backlog.Hook(st, cfg),
		ExternalLookups: cfg.ExternalLookups(),
	})
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	if err := writeSyncWarnings(cmd.ErrOrStderr(), "sync", results); err != nil {
		return err
	}
	return partialFailure(results)
}

func explainItem(ctx context.Context, cmd *cobra.Command, st *store.Store, cfg *config.Config, id, only string) error {
	it, err := resolveItem(ctx, st, id, only)
	if err != nil {
		return err
	}
	srcCfg, err := configFor(cfg, it.Source)
	if err != nil {
		return err
	}
	if err := checkKind(it.Source, srcCfg.Kind); err != nil {
		return err
	}
	snap, err := loadSnapshot(ctx, st, srcCfg)
	if err != nil {
		return err
	}
	res := ready.Evaluate(it, snap, srcCfg, srcCfg.Kind)
	ranked, err := rankItems([]source.Item{it}, snap.Items, srcCfg)
	if err != nil {
		return err
	}
	if len(ranked) != 1 {
		return fmt.Errorf("explain %s: expected one score, found %d", it.ID, len(ranked))
	}
	notes, err := priority.Explain(it.ID, snap.Items, srcCfg)
	if err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	order, err := rank.ParseOrder(srcCfg.RankOrder)
	if err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	text, err := formatExplain(res, ranked[0].Score, order)
	if err != nil {
		return err
	}
	if len(notes) > 0 {
		text += strings.Join(notes, "\n") + "\n"
	}
	tail, err := explainStart(cfg, it)
	if err != nil {
		return err
	}
	text += tail
	cfgs, err := sourcesFor(cfg, only)
	if err != nil {
		return err
	}
	if err := writeMirrorAge(ctx, cmd, st, cfgs, cfg.Sync.MaxAge, false); err != nil {
		return err
	}
	if _, err := io.WriteString(cmd.OutOrStdout(), text); err != nil {
		return fmt.Errorf("write explain: %w", err)
	}
	return nil
}

func printRanked(ctx context.Context, cmd *cobra.Command, st *store.Store, cfg *config.Config, opts nextOpts) error {
	if humanListing(opts) {
		return printHuman(ctx, cmd, st, cfg, opts)
	}
	srcs, err := sourcesFor(cfg, opts.only)
	if err != nil {
		return err
	}
	rows, tallies, order, err := collect(ctx, st, srcs)
	if err != nil {
		return err
	}
	if opts.deferred {
		rows = sortDeferred(tallies.deferred)
	} else if err := checkLaneRoutes(cfg, rows); err != nil {
		return err
	}
	if len(rows) > opts.limit {
		rows = rows[:opts.limit]
	}
	if err := writeMirrorAge(ctx, cmd, st, srcs, cfg.Sync.MaxAge, opts.asJSON); err != nil {
		return err
	}
	out := cmd.OutOrStdout()
	switch {
	case opts.asJSON:
		return writeJSON(out, rows)
	case opts.deferred:
		return writeDeferred(out, rows)
	case len(rows) == 0:
		err = writeEmpty(out, tallies)
	default:
		err = writeTable(out, rows, order)
	}
	if err != nil {
		return err
	}
	return writeDeferredSummary(out, tallies.deferred)
}

// sortDeferred orders deferred items by date, then source and id.
func sortDeferred(rows []picked) []picked {
	out := slices.Clone(rows)
	slices.SortStableFunc(out, func(a, b picked) int {
		if c := a.item.NotBefore.Compare(*b.item.NotBefore); c != 0 {
			return c
		}
		if c := strings.Compare(a.source, b.source); c != 0 {
			return c
		}
		return strings.Compare(a.item.ID, b.item.ID)
	})
	return out
}

// writeDeferredSummary prints "N deferred (earliest YYYY-MM-DD)" so
// deferred items are not invisible. None prints nothing.
func writeDeferredSummary(w io.Writer, rows []picked) error {
	if len(rows) == 0 {
		return nil
	}
	earliest := sortDeferred(rows)[0].item.NotBefore.Format(time.DateOnly)
	if _, err := fmt.Fprintf(w, "%d deferred (earliest %s); factory next --deferred lists them\n", len(rows), earliest); err != nil {
		return fmt.Errorf("write next: %w", err)
	}
	return nil
}

// writeDeferred lists deferred items with their date.
func writeDeferred(w io.Writer, rows []picked) error {
	if len(rows) == 0 {
		if _, err := fmt.Fprintln(w, "no deferred items"); err != nil {
			return fmt.Errorf("write next: %w", err)
		}
		return nil
	}
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	if _, err := fmt.Fprintln(tw, "#\tID\tNOT_BEFORE\tSOURCE\tTITLE"); err != nil {
		return fmt.Errorf("write next: %w", err)
	}
	for i, row := range rows {
		if _, err := fmt.Fprintf(tw, "%d\t%s\t%s\t%s\t%s\n", i+1, row.item.ID, row.item.NotBefore.Format(time.DateOnly), row.source, truncateRunes(row.item.Title, 60)); err != nil {
			return fmt.Errorf("write next: %w", err)
		}
	}
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("write next: %w", err)
	}
	return nil
}

func collect(ctx context.Context, st *store.Store, cfgs []config.Source) ([]picked, counts, rank.Order, error) {
	var rows []picked
	var tallies counts
	for i := range cfgs {
		part, partCounts, err := rankSource(ctx, st, cfgs[i])
		if err != nil {
			return nil, counts{}, rank.Order{}, err
		}
		rows = append(rows, part...)
		tallies.open += partCounts.open
		tallies.blocked += partCounts.blocked
		tallies.notEligible += partCounts.notEligible
		tallies.deferred = append(tallies.deferred, partCounts.deferred...)
	}
	order, err := rank.CommonOrder(pickedOrders(rows))
	if err != nil {
		return nil, counts{}, rank.Order{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	slices.SortStableFunc(rows, func(a, b picked) int {
		return comparePicked(order, a, b)
	})
	return rows, tallies, order, nil
}

// checkLaneRoutes reports every ready item whose labels match more than one
// lane. That is a config error, not a guess. No lanes reports nothing.
func checkLaneRoutes(cfg *config.Config, rows []picked) error {
	if !cfg.HasLanes() {
		return nil
	}
	var errs []error
	for _, row := range rows {
		if _, err := cfg.RouteLanes(row.item.Labels); err != nil {
			errs = append(errs, fmt.Errorf("next %s: %w", row.item.ID, err))
		}
	}
	return errors.Join(errs...)
}

// explainStart is the explain lines about how start would run the item:
// the agent args, then the lanes when lanes are configured.
func explainStart(cfg *config.Config, it source.Item) (string, error) {
	argsLine, err := config.FormatAgentArgs(config.ChooseAgentArgs(it.Labels, cfg.Defaults.AgentArgs, cfg.AgentRules, nil, false))
	if err != nil {
		return "", err //nolint:wrapcheck // internal factory error already carries context
	}
	lanes, err := explainLanes(cfg, it)
	if err != nil {
		return "", err
	}
	return argsLine + lanes, nil
}

// explainLanes is the `lanes` line of next --explain: the lanes the item
// may start on, in order. No lanes prints nothing.
func explainLanes(cfg *config.Config, it source.Item) (string, error) {
	if !cfg.HasLanes() {
		return "", nil
	}
	routed, err := cfg.RouteLanes(it.Labels)
	if err != nil {
		return "", fmt.Errorf("explain %s: %w", it.ID, err)
	}
	return fmt.Sprintf("lanes  %s\n", laneList(routed)), nil
}

func laneList(lanes []config.Lane) string {
	names := make([]string, len(lanes))
	for i, lane := range lanes {
		names[i] = lane.Name
	}
	return strings.Join(names, ", ")
}

func pickedOrders(rows []picked) []rank.NamedOrder {
	out := make([]rank.NamedOrder, len(rows))
	for i, row := range rows {
		out[i] = rank.NamedOrder{Name: row.source, Order: row.order}
	}
	return out
}

func rankSource(ctx context.Context, st *store.Store, cfg config.Source) ([]picked, counts, error) {
	key, err := sourceKey(cfg.Kind, cfg.Name)
	if err != nil {
		return nil, counts{}, err
	}
	if err := checkKind(key, cfg.Kind); err != nil {
		return nil, counts{}, err
	}
	snap, err := loadSnapshot(ctx, st, cfg)
	if err != nil {
		return nil, counts{}, err
	}
	var tallies counts
	readyItems := make([]source.Item, 0, len(snap.Items))
	for _, it := range snap.Items {
		res := ready.Evaluate(it, snap, cfg, cfg.Kind)
		if res.Ready {
			readyItems = append(readyItems, it)
			continue
		}
		if notBeforeFailed(res) {
			tallies.deferred = append(tallies.deferred, picked{source: key, item: it})
		}
		open, blocked, notEligible := classify(res)
		tallies.open += open
		tallies.blocked += blocked
		tallies.notEligible += notEligible
	}
	ranked, err := rankItems(readyItems, snap.Items, cfg)
	if err != nil {
		return nil, counts{}, err
	}
	out := make([]picked, len(ranked))
	for i, row := range ranked {
		out[i] = picked{source: key, item: row.Item, score: row.Score, order: cfg.RankOrder}
	}
	return out, tallies, nil
}

func notBeforeFailed(res ready.Result) bool {
	return slices.ContainsFunc(res.Checks, func(c ready.Check) bool {
		return c.Rule == ready.RuleNotBefore && !c.Pass
	})
}

// classify buckets one not-ready item. See ready.Classify.
func classify(res ready.Result) (open, blocked, notEligible int) {
	return ready.Classify(res)
}

// comparePicked orders like rank.Rank, then by source.
// order is the shared rank_order. ID ascending stays inside rank.Order.Compare.
func comparePicked(order rank.Order, a, b picked) int {
	if c := order.Compare(rank.Ranked{Item: a.item, Score: a.score}, rank.Ranked{Item: b.item, Score: b.score}); c != 0 {
		return c
	}
	return cmp.Compare(a.source, b.source)
}

type sourcePair struct {
	key string
	src config.Source
}

func sourcesFor(cfg *config.Config, only string) ([]config.Source, error) {
	if cfg == nil {
		return nil, errors.New("sources: expected a config, found nil")
	}
	pairs := make([]sourcePair, 0, len(cfg.Sources))
	for i := range cfg.Sources {
		key, err := sourceKey(cfg.Sources[i].Kind, cfg.Sources[i].Name)
		if err != nil {
			return nil, err
		}
		pairs = append(pairs, sourcePair{key: key, src: cfg.Sources[i]})
	}
	slices.SortFunc(pairs, func(a, b sourcePair) int { return cmp.Compare(a.key, b.key) })
	if only == "" {
		out := make([]config.Source, len(pairs))
		for i, p := range pairs {
			out[i] = p.src
		}
		return out, nil
	}
	for _, p := range pairs {
		if p.key == only {
			return []config.Source{p.src}, nil
		}
	}
	return nil, fmt.Errorf("--source: expected one of %s, found %q", formatSources(pairs), only)
}

func formatSources(pairs []sourcePair) string {
	if len(pairs) == 0 {
		return "(none)"
	}
	names := make([]string, len(pairs))
	for i, p := range pairs {
		names[i] = p.key
	}
	return strings.Join(names, ", ")
}

func configFor(cfg *config.Config, key string) (config.Source, error) {
	srcs, err := sourcesFor(cfg, "")
	if err != nil {
		return config.Source{}, err
	}
	for i := range srcs {
		got, err := sourceKey(srcs[i].Kind, srcs[i].Name)
		if err != nil {
			return config.Source{}, err
		}
		if got == key {
			return srcs[i], nil
		}
	}
	return config.Source{}, fmt.Errorf("source %s: expected a configured source, found none", key)
}

func checkKind(key, kind string) error {
	if kind == "jira" || kind == "github" {
		return nil
	}
	return fmt.Errorf("source %s: expected kind jira or github, found %q", key, kind)
}

func loadSnapshot(ctx context.Context, st *store.Store, cfg config.Source) (ready.Snapshot, error) {
	key, err := sourceKey(cfg.Kind, cfg.Name)
	if err != nil {
		return ready.Snapshot{}, err
	}
	me, err := st.Me(ctx, key)
	if err != nil {
		return ready.Snapshot{}, err //nolint:wrapcheck // propagate context cancellation
	}
	if me == "" {
		return ready.Snapshot{}, fmt.Errorf("%s: run factory sync first", key)
	}
	herd, pulls, err := externalClients(cfg.GHBin)
	if err != nil {
		return ready.Snapshot{}, err
	}
	return snapshot.Build(ctx, st, herd, pulls, key, me) //nolint:wrapcheck // propagate context cancellation
}

func resolveItem(ctx context.Context, st *store.Store, id, only string) (source.Item, error) {
	if id == "" {
		return source.Item{}, errors.New("item: expected a non-empty id, found empty")
	}
	items, err := st.FindItem(ctx, id)
	if err != nil {
		return source.Item{}, err //nolint:wrapcheck // propagate context cancellation
	}
	if only != "" {
		matched := make([]source.Item, 0, len(items))
		for _, it := range items {
			if it.Source == only {
				matched = append(matched, it)
			}
		}
		items = matched
	}
	switch len(items) {
	case 0:
		if only != "" {
			return source.Item{}, fmt.Errorf("item %s: expected it in %s, found none", id, only)
		}
		return source.Item{}, fmt.Errorf("item %s: expected one item, found none", id)
	case 1:
		return items[0], nil
	default:
		names := make([]string, len(items))
		for i, it := range items {
			names[i] = it.Source
		}
		return source.Item{}, fmt.Errorf("item %s: expected one source, found %s", id, strings.Join(names, ", "))
	}
}

func writeTable(w io.Writer, rows []picked, order rank.Order) error {
	keys := order.Keys()
	header := make([]string, 0, len(keys)+3)
	header = append(header, "#", "ID")
	for _, key := range keys {
		name, err := columnHeader(key)
		if err != nil {
			return err
		}
		header = append(header, name)
	}
	header = append(header, "TITLE")
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	if _, err := fmt.Fprintln(tw, strings.Join(header, "\t")); err != nil {
		return fmt.Errorf("write next table: %w", err)
	}
	for i, row := range rows {
		cells := make([]string, 0, len(header))
		cells = append(cells, strconv.Itoa(i+1), row.item.ID)
		for _, key := range keys {
			cell, err := columnCell(key, row)
			if err != nil {
				return err
			}
			cells = append(cells, cell)
		}
		cells = append(cells, truncateRunes(row.item.Title, 60))
		if _, err := fmt.Fprintln(tw, strings.Join(cells, "\t")); err != nil {
			return fmt.Errorf("write next table: %w", err)
		}
	}
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("write next table: %w", err)
	}
	return nil
}

func columnHeader(key string) (string, error) {
	switch key {
	case "priority":
		return "PRI", nil
	case "unblocks":
		return "UNBLOCKS", nil
	case "parent_rank":
		return "PARENT", nil
	case "created":
		return "CREATED", nil
	default:
		return "", fmt.Errorf("next table: expected priority, unblocks, parent_rank, or created, found %q", key)
	}
}

func columnCell(key string, row picked) (string, error) {
	switch key {
	case "priority":
		return strconv.Itoa(row.score.Priority), nil
	case "unblocks":
		return strconv.Itoa(row.score.Unblocks), nil
	case "parent_rank":
		return row.item.Parent, nil
	case "created":
		return formatStamp(row.score.Created), nil
	default:
		return "", fmt.Errorf("next table: expected priority, unblocks, parent_rank, or created, found %q", key)
	}
}

type nextJSON struct {
	Source string    `json:"source"`
	ID     string    `json:"id"`
	Title  string    `json:"title"`
	URL    string    `json:"url"`
	Parent string    `json:"parent"`
	Score  scoreJSON `json:"score"`
	// NotBefore is the item's deferral date, YYYY-MM-DD, or null.
	NotBefore *string `json:"notBefore"`
}

type scoreJSON struct {
	Unblocks   int    `json:"unblocks"`
	ParentRank int    `json:"parent_rank"` //nolint:tagliatelle // documented factory JSON output field
	Priority   int    `json:"priority"`
	Created    string `json:"created"`
}

func writeJSON(w io.Writer, rows []picked) error {
	out := make([]nextJSON, len(rows))
	for i, row := range rows {
		out[i] = nextJSON{
			Source: row.source,
			ID:     row.item.ID,
			Title:  row.item.Title,
			URL:    row.item.URL,
			Parent: row.item.Parent,
			Score: scoreJSON{
				Unblocks:   row.score.Unblocks,
				ParentRank: row.score.ParentRank,
				Priority:   row.score.Priority,
				Created:    formatStamp(row.score.Created),
			},
			NotBefore: dueJSON(row.item.NotBefore),
		}
	}
	raw, err := json.MarshalIndent(out, "", "  ")
	if err != nil {
		return fmt.Errorf("next json: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", raw); err != nil {
		return fmt.Errorf("write next json: %w", err)
	}
	return nil
}

type humanRow struct {
	item     source.Item
	unblocks []string
}

func printHuman(ctx context.Context, cmd *cobra.Command, st *store.Store, cfg *config.Config, opts nextOpts) error {
	srcs, err := sourcesFor(cfg, opts.only)
	if err != nil {
		return err
	}
	rows, tallies, err := collectHuman(ctx, st, srcs, opts)
	if err != nil {
		return err
	}
	if len(rows) > opts.limit {
		rows = rows[:opts.limit]
	}
	if err := writeMirrorAge(ctx, cmd, st, srcs, cfg.Sync.MaxAge, opts.asJSON); err != nil {
		return err
	}
	out := cmd.OutOrStdout()
	if opts.asJSON {
		return writeHumanJSON(out, rows)
	}
	if len(rows) == 0 {
		return writeEmpty(out, tallies)
	}
	return writeHumanTable(out, rows)
}

func collectHuman(ctx context.Context, st *store.Store, cfgs []config.Source, opts nextOpts) ([]humanRow, counts, error) {
	var rows []humanRow
	var tallies counts
	for i := range cfgs {
		part, partCounts, err := humanSource(ctx, st, cfgs[i], opts)
		if err != nil {
			return nil, counts{}, err
		}
		rows = append(rows, part...)
		tallies.open += partCounts.open
		tallies.blocked += partCounts.blocked
		tallies.notEligible += partCounts.notEligible
	}
	slices.SortStableFunc(rows, compareHuman)
	return rows, tallies, nil
}

func humanSource(ctx context.Context, st *store.Store, cfg config.Source, opts nextOpts) ([]humanRow, counts, error) {
	key, err := sourceKey(cfg.Kind, cfg.Name)
	if err != nil {
		return nil, counts{}, err
	}
	if err := checkKind(key, cfg.Kind); err != nil {
		return nil, counts{}, err
	}
	snap, err := loadSnapshot(ctx, st, cfg)
	if err != nil {
		return nil, counts{}, err
	}
	ignore, err := config.IgnoreAncestorBlocks(cfg.ParentBlocksChild)
	if err != nil {
		return nil, counts{}, fmt.Errorf("source %q field \"parent_blocks_child\": %w", cfg.Name, err)
	}
	selected := selectedLabels(cfg, opts)
	evalCfg := cfg
	evalCfg.ExcludeLabels = withoutLabels(cfg.ExcludeLabels, selected)
	var tallies counts
	var out []humanRow
	for _, it := range snap.Items {
		if !hasAnyLabel(it.Labels, selected) {
			continue
		}
		res := ready.Evaluate(it, snap, evalCfg, cfg.Kind)
		if !res.Ready {
			open, blocked, notEligible := classify(res)
			tallies.open += open
			tallies.blocked += blocked
			tallies.notEligible += notEligible
			continue
		}
		if opts.mine && it.Assignee != snap.Me {
			continue
		}
		out = append(out, humanRow{item: it, unblocks: rank.Unblocked(it.ID, snap.Items, ignore)})
	}
	return out, tallies, nil
}

// selectedLabels is human_labels, unless --only-label was set. That flag replaces the set.
func selectedLabels(cfg config.Source, opts nextOpts) []string {
	if len(opts.onlyLabel) > 0 {
		return append([]string(nil), opts.onlyLabel...)
	}
	return append([]string(nil), cfg.HumanLabels...)
}

// withoutLabels copies exclude without any selected label.
// Those labels are not exclude labels for this run. Other exclude labels stay.
func withoutLabels(exclude, selected []string) []string {
	drop := make(map[string]struct{}, len(selected))
	for _, label := range selected {
		drop[label] = struct{}{}
	}
	out := make([]string, 0, len(exclude))
	for _, label := range exclude {
		if _, ok := drop[label]; ok {
			continue
		}
		out = append(out, label)
	}
	return out
}

func hasAnyLabel(labels, selected []string) bool {
	for _, want := range selected {
		if slices.Contains(labels, want) {
			return true
		}
	}
	return false
}

// compareHuman orders by priority ascending, due date ascending with nil last,
// unblock count descending, then id ascending. Source is the final tie-break.
func compareHuman(a, b humanRow) int {
	if c := cmp.Compare(a.item.Priority, b.item.Priority); c != 0 {
		return c
	}
	if c := compareDue(a.item.Due, b.item.Due); c != 0 {
		return c
	}
	if c := cmp.Compare(len(b.unblocks), len(a.unblocks)); c != 0 {
		return c
	}
	if c := cmp.Compare(a.item.ID, b.item.ID); c != 0 {
		return c
	}
	return cmp.Compare(a.item.Source, b.item.Source)
}

func compareDue(a, b *time.Time) int {
	switch {
	case a == nil && b == nil:
		return 0
	case a == nil:
		return 1
	case b == nil:
		return -1
	default:
		return a.UTC().Compare(b.UTC())
	}
}

func writeHumanTable(w io.Writer, rows []humanRow) error {
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	if _, err := fmt.Fprintln(tw, "KEY\tPRI\tDUE\tTITLE\tURL\tUNBLOCKS"); err != nil {
		return fmt.Errorf("write next table: %w", err)
	}
	for _, row := range rows {
		line := strings.Join([]string{
			row.item.ID,
			strconv.Itoa(row.item.Priority),
			formatDue(row.item.Due),
			row.item.Title,
			row.item.URL,
			strings.Join(row.unblocks, ", "),
		}, "\t")
		if _, err := fmt.Fprintln(tw, line); err != nil {
			return fmt.Errorf("write next table: %w", err)
		}
	}
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("write next table: %w", err)
	}
	return nil
}

func formatDue(t *time.Time) string {
	if t == nil {
		return ""
	}
	return t.UTC().Format("2006-01-02")
}

type humanJSON struct {
	Key      string   `json:"key"`
	Priority int      `json:"priority"`
	Due      *string  `json:"due"`
	Title    string   `json:"title"`
	URL      string   `json:"url"`
	Unblocks []string `json:"unblocks"`
}

func writeHumanJSON(w io.Writer, rows []humanRow) error {
	out := make([]humanJSON, len(rows))
	for i, row := range rows {
		ids := row.unblocks
		if ids == nil {
			ids = []string{}
		}
		out[i] = humanJSON{
			Key:      row.item.ID,
			Priority: row.item.Priority,
			Due:      dueJSON(row.item.Due),
			Title:    row.item.Title,
			URL:      row.item.URL,
			Unblocks: ids,
		}
	}
	raw, err := json.MarshalIndent(out, "", "  ")
	if err != nil {
		return fmt.Errorf("next json: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", raw); err != nil {
		return fmt.Errorf("write next json: %w", err)
	}
	return nil
}

func dueJSON(t *time.Time) *string {
	if t == nil {
		return nil
	}
	s := t.UTC().Format("2006-01-02")
	return &s
}

func writeEmpty(w io.Writer, tallies counts) error {
	_, err := fmt.Fprintf(w, "no ready items (%d open, %d blocked, %d not eligible)\n",
		tallies.open, tallies.blocked, tallies.notEligible)
	if err != nil {
		return fmt.Errorf("write next: %w", err)
	}
	return nil
}

func rankItems(items []source.Item, all map[string]source.Item, cfg config.Source) ([]rank.Ranked, error) {
	ignore, err := config.IgnoreAncestorBlocks(cfg.ParentBlocksChild)
	if err != nil {
		return nil, fmt.Errorf("source %q field \"parent_blocks_child\": %w", cfg.Name, err)
	}
	return rank.Rank(items, all, cfg.ParentPriority, cfg.RankOrder, ignore) //nolint:wrapcheck // internal factory error already carries context
}

func formatExplain(res ready.Result, score rank.Score, order rank.Order) (string, error) {
	var b strings.Builder
	for _, check := range res.Checks {
		status := "FAIL"
		if check.Pass {
			status = "PASS"
		}
		if check.Detail == "" {
			b.WriteString(status + " " + string(check.Rule) + "\n")
			continue
		}
		b.WriteString(status + " " + string(check.Rule) + "  " + check.Detail + "\n")
	}
	ancestor := "none"
	if score.Ancestor != "" {
		ancestor = score.Ancestor
	}
	bprintf(&b, "ancestor  %s\n", ancestor)
	bprintf(&b, "rank_order  %s\n", strings.Join(order.Keys(), ", "))
	line, err := formatScoreLine(score, order)
	if err != nil {
		return "", err
	}
	b.WriteString(line)
	return b.String(), nil
}

func formatScoreLine(score rank.Score, order rank.Order) (string, error) {
	keys := order.Keys()
	parts := make([]string, 0, len(keys))
	for _, key := range keys {
		part, err := scorePart(key, score)
		if err != nil {
			return "", err
		}
		parts = append(parts, part)
	}
	return "score  " + strings.Join(parts, "  ") + "\n", nil
}

func scorePart(key string, score rank.Score) (string, error) {
	switch key {
	case "priority":
		return fmt.Sprintf("priority=%d", score.Priority), nil
	case "unblocks":
		return fmt.Sprintf("unblocks=%d", score.Unblocks), nil
	case "parent_rank":
		return fmt.Sprintf("parent_rank=%d", score.ParentRank), nil
	case "created":
		return "created=" + formatStamp(score.Created), nil
	default:
		return "", fmt.Errorf("score: expected priority, unblocks, parent_rank, or created, found %q", key)
	}
}

func truncateRunes(s string, n int) string {
	r := []rune(s)
	if len(r) <= n {
		return s
	}
	return string(r[:n])
}

func formatStamp(t time.Time) string {
	return t.UTC().Format(time.RFC3339Nano)
}

// bprintf writes to b. A strings.Builder write does not fail.
func bprintf(b *strings.Builder, format string, args ...any) {
	if _, err := fmt.Fprintf(b, format, args...); err != nil {
		panic(fmt.Sprintf("format: expected a successful write, found %v", err))
	}
}
