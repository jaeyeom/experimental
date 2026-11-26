// Package stats builds the factory stats report from the local store.
// It reads items, claims, prs, finished, and backlog_snapshots, and makes no
// network calls. Every time is bucketed in UTC.
package stats

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	// ByDay buckets throughput by UTC calendar day.
	ByDay = "day"
	// ByWeek buckets throughput by UTC week, starting Monday.
	ByWeek = "week"
	// SplitModel splits throughput and flow by the claim's model.
	SplitModel = "model"
	// SplitRule splits throughput and flow by the claim's agent rule.
	SplitRule = "rule"
	// Unknown is the split group of a claim that recorded no agent rule,
	// and of an event with no claim.
	Unknown = "unknown"
	// dayLimit is the longest window that defaults to day buckets.
	dayLimit = 14 * 24 * time.Hour
)

// Options selects the window, the bucket, the split, and one epic.
type Options struct {
	Now   time.Time
	Since time.Duration
	By    string // ByDay, ByWeek, or empty to pick by Since.
	Split string // SplitModel, SplitRule, or empty for no split.
	Epic  string // Item ID whose subtree limits every section. Empty is all.
	// Source, a key such as jira:acme, limits every section to that
	// source. Empty is every source.
	Source string
	// Loc is the zone day and week buckets start in, and the text report's
	// times. Nil is UTC. JSON times stay UTC.
	Loc *time.Location
}

// Report is every section. JSON has one object per section.
type Report struct {
	// Source is the one source the report covers, or empty for every source.
	Source     string      `json:"source,omitempty"`
	Window     Window      `json:"window"`
	Freshness  Freshness   `json:"freshness"`
	Throughput Throughput  `json:"throughput"`
	Backlog    BacklogPart `json:"backlog"`
	Epics      Epics       `json:"epics"`
	Flow       Flow        `json:"flow"`
	Split      *Split      `json:"split"`
	Tree       []TreeNode  `json:"tree,omitempty"`
	// Burndown is for the text report only.
	Burndown *Burndown `json:"-"`
	// loc is the zone the text report prints times in.
	loc *time.Location
}

// Window is the report window. Both ends are inclusive.
type Window struct {
	Start time.Time `json:"start"`
	End   time.Time `json:"end"`
	By    string    `json:"by"`
	Epic  string    `json:"epic,omitempty"`
	// TimeZone is the zone the buckets used: an IANA name, UTC, or an
	// offset such as -08:00 for the local zone.
	TimeZone string `json:"time_zone"` //nolint:tagliatelle // documented factory JSON output field
}

// Freshness says how old the stored data is.
type Freshness struct {
	LastSync []SourceSync `json:"last_sync"` //nolint:tagliatelle // documented factory JSON output field
	// Nil when no pull request was recorded.
	PRLastSeen *time.Time `json:"pr_last_seen"` //nolint:tagliatelle // documented factory JSON output field
}

// SourceSync is one source's last successful sync. Nil At is never.
type SourceSync struct {
	Source string     `json:"source"`
	At     *time.Time `json:"at"`
}

type itemKey struct {
	source string
	id     string
}

func (k itemKey) String() string { return k.source + "/" + k.id }

// node is one item in the mirror or in finished history.
type node struct {
	key      itemKey
	title    string
	parent   string
	created  time.Time
	done     bool
	mirrored bool
	item     source.Item
}

// data is everything the report reads, loaded once.
type data struct {
	opts     Options
	start    time.Time
	cfg      *config.Config
	views    map[string]backlog.View
	nodes    map[itemKey]node
	children map[itemKey][]itemKey
	claims   []store.Claim
	finished []store.Finished
	prs      []store.PR
	snaps    []store.BacklogSnapshot
	scope    map[itemKey]bool // Nil means every item.
	root     itemKey
}

// Build reads st and computes the report.
func Build(ctx context.Context, st *store.Store, cfg *config.Config, opts Options) (Report, error) {
	by, err := validate(st, cfg, opts)
	if err != nil {
		return Report{}, err
	}
	opts.By = by
	if opts.Loc == nil {
		opts.Loc = time.UTC
	}
	d, err := load(ctx, st, cfg, opts)
	if err != nil {
		return Report{}, err
	}
	fresh, err := freshness(ctx, st, d)
	if err != nil {
		return Report{}, err
	}
	epics, err := d.epics()
	if err != nil {
		return Report{}, err
	}
	rep := Report{
		Source:     opts.Source,
		Window:     Window{Start: d.start, End: opts.Now, By: by, Epic: opts.Epic, TimeZone: zoneName(opts.Loc, opts.Now)},
		Freshness:  fresh,
		Throughput: d.throughput(),
		Backlog:    d.backlog(),
		Epics:      epics,
		Flow:       d.flow(),
		Split:      d.split(),
		Burndown:   d.burndown(),
		loc:        opts.Loc,
	}
	if opts.Epic != "" {
		rep.Tree = d.tree()
	}
	return rep, nil
}

// zoneName names loc for JSON: its IANA name, or for the process's local
// zone (named "Local"), its UTC offset at now, such as -08:00.
func zoneName(loc *time.Location, now time.Time) string {
	if loc.String() != "Local" {
		return loc.String()
	}
	return now.In(loc).Format("-07:00")
}

func validate(st *store.Store, cfg *config.Config, opts Options) (string, error) {
	if st == nil {
		return "", errors.New("stats: expected a store, found nil")
	}
	if cfg == nil {
		return "", errors.New("stats: expected a config, found nil")
	}
	if opts.Now.IsZero() {
		return "", errors.New("stats: expected a clock time, found zero")
	}
	if opts.Since <= 0 {
		return "", fmt.Errorf("stats --since: expected a positive window, found %s", opts.Since)
	}
	switch opts.Split {
	case "", SplitModel, SplitRule:
	default:
		return "", fmt.Errorf("stats --split: expected model or rule, found %q", opts.Split)
	}
	switch opts.By {
	case ByDay, ByWeek:
		return opts.By, nil
	case "":
		if opts.Since <= dayLimit {
			return ByDay, nil
		}
		return ByWeek, nil
	default:
		return "", fmt.Errorf("stats --by: expected day or week, found %q", opts.By)
	}
}

func load(ctx context.Context, st *store.Store, cfg *config.Config, opts Options) (*data, error) {
	d := &data{opts: opts, start: opts.Now.Add(-opts.Since), cfg: cfg, views: map[string]backlog.View{}}
	for i := range cfg.Sources {
		if opts.Source != "" && cfg.Sources[i].Kind+":"+cfg.Sources[i].Name != opts.Source {
			continue
		}
		view, err := backlog.Load(ctx, st, cfg.Sources[i])
		if err != nil {
			return nil, err //nolint:wrapcheck // propagate context cancellation
		}
		d.views[view.Key] = view
	}
	if opts.Source != "" && len(d.views) == 0 {
		return nil, fmt.Errorf("stats --source: expected a configured source, found %q", opts.Source)
	}
	var err error
	if d.claims, err = st.Claims(ctx); err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	if d.finished, err = st.FinishedItems(ctx); err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	if d.prs, err = st.PRs(ctx); err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	if d.snaps, err = st.BacklogSnapshots(ctx); err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	d.buildNodes()
	if opts.Epic != "" {
		if err := d.limitTo(opts.Epic); err != nil {
			return nil, err
		}
	}
	return d, nil
}

func (d *data) buildNodes() {
	d.nodes = map[itemKey]node{}
	for _, view := range d.views {
		for _, it := range view.Snap.Items {
			k := itemKey{it.Source, it.ID}
			d.nodes[k] = node{key: k, title: it.Title, parent: it.Parent, created: it.Created, done: it.Done, mirrored: true, item: it}
		}
	}
	for _, f := range d.finished {
		k := itemKey{f.Source, f.ID}
		if _, ok := d.nodes[k]; ok || !d.inSource(f.Source) {
			continue
		}
		d.nodes[k] = node{key: k, title: f.Title, parent: f.Parent, done: true}
	}
	d.children = map[itemKey][]itemKey{}
	for k, n := range d.nodes {
		if n.parent == "" {
			continue
		}
		p := itemKey{k.source, n.parent}
		d.children[p] = append(d.children[p], k)
	}
	for p := range d.children {
		slices.SortFunc(d.children[p], compareKeys)
	}
}

func compareKeys(a, b itemKey) int {
	if c := strings.Compare(a.source, b.source); c != 0 {
		return c
	}
	return strings.Compare(a.id, b.id)
}

// limitTo sets the scope to id's subtree: the item and its descendants.
func (d *data) limitTo(id string) error {
	var found []itemKey
	for k := range d.nodes {
		if k.id == id {
			found = append(found, k)
		}
	}
	for p := range d.children {
		if p.id == id && !slices.Contains(found, p) {
			found = append(found, p)
		}
	}
	switch len(found) {
	case 0:
		return fmt.Errorf("stats --epic %s: expected an item in the mirror or finished history, found none", id)
	case 1:
	default:
		slices.SortFunc(found, compareKeys)
		names := make([]string, len(found))
		for i, k := range found {
			names[i] = k.source
		}
		return fmt.Errorf("stats --epic %s: expected one source, found %s", id, strings.Join(names, ", "))
	}
	d.root = found[0]
	d.scope = map[itemKey]bool{}
	return d.addSubtree(d.root)
}

func (d *data) addSubtree(k itemKey) error {
	if d.scope[k] {
		return fmt.Errorf("stats --epic: expected a parent tree, found a cycle at %s", k)
	}
	d.scope[k] = true
	for _, c := range d.children[k] {
		if err := d.addSubtree(c); err != nil {
			return err
		}
	}
	return nil
}

func (d *data) inScope(k itemKey) bool {
	return d.inSource(k.source) && (d.scope == nil || d.scope[k])
}

// inSource reports whether key is in the report: every source, or the one
// Options.Source names.
func (d *data) inSource(key string) bool {
	return d.opts.Source == "" || key == d.opts.Source
}

func (d *data) inWindow(t time.Time) bool {
	return !t.IsZero() && !t.Before(d.start) && !t.After(d.opts.Now)
}

func freshness(ctx context.Context, st *store.Store, d *data) (Freshness, error) {
	keys := make([]string, 0, len(d.views))
	for k := range d.views {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	out := Freshness{LastSync: make([]SourceSync, 0, len(keys))}
	for _, k := range keys {
		at, err := st.LastSync(ctx, k)
		if err != nil {
			return Freshness{}, err //nolint:wrapcheck // propagate context cancellation
		}
		out.LastSync = append(out.LastSync, SourceSync{Source: k, At: timePtr(at)})
	}
	var seen time.Time
	for _, pr := range d.prs {
		if pr.LastSeen.After(seen) {
			seen = pr.LastSeen
		}
	}
	out.PRLastSeen = timePtr(seen)
	return out, nil
}

func timePtr(t time.Time) *time.Time {
	if t.IsZero() {
		return nil
	}
	u := t.UTC()
	return &u
}

// claimFor is the latest claim of k at or before at that did not fail.
func (d *data) claimFor(k itemKey, at time.Time) (store.Claim, bool) {
	var best store.Claim
	found := false
	for _, c := range d.claims {
		if c.Source != k.source || c.ID != k.id || c.State == store.Failed || c.ClaimedAt.After(at) {
			continue
		}
		if !found || c.ClaimedAt.After(best.ClaimedAt) {
			best, found = c, true
		}
	}
	return best, found
}
