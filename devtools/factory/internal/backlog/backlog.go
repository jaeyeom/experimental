// Package backlog counts one source's open items from the local store and
// records hourly backlog snapshots after a sync. It makes no network calls.
package backlog

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	// SnapshotEvery is the least time between two snapshots of one source.
	SnapshotEvery = time.Hour
	// SnapshotKeep is how long a snapshot is kept.
	SnapshotKeep = 90 * 24 * time.Hour
)

// Bucket is where one open item counts.
type Bucket string

const (
	// InFlight is an item with an active claim or an open pull request in prs.
	InFlight Bucket = "in_flight"
	// Ready is an item factory next would list.
	Ready Bucket = "ready"
	// NeedsHuman is a not-ready item with a human_labels label.
	NeedsHuman Bucket = "needs_human"
	// Blocked is a not-ready item whose first failing rule is blockers.
	Blocked Bucket = "blocked"
	// Other is any other open item. It counts only by status.
	Other Bucket = "other"
)

// Counts is one source's open items by bucket and by raw status.
type Counts struct {
	Ready      int
	Blocked    int
	InFlight   int
	NeedsHuman int
	ByStatus   map[string]int
}

// Add adds o to c.
func (c *Counts) Add(o Counts) {
	c.Ready += o.Ready
	c.Blocked += o.Blocked
	c.InFlight += o.InFlight
	c.NeedsHuman += o.NeedsHuman
	if c.ByStatus == nil {
		c.ByStatus = map[string]int{}
	}
	for status, n := range o.ByStatus {
		c.ByStatus[status] += n
	}
}

// View is one source's offline readiness inputs.
type View struct {
	Key    string
	Cfg    config.Source
	Snap   ready.Snapshot
	OpenPR map[string]bool // Item IDs with an open pull request in prs.
}

// Load reads the offline view of cfg's source: mirrored items, active
// claims, and open pull requests from the prs table. herdr tabs and live gh
// state are not read, so the tab and pr rules see nothing.
func Load(ctx context.Context, st *store.Store, cfg config.Source) (View, error) {
	if st == nil {
		return View{}, errors.New("backlog: expected a store, found nil")
	}
	key, err := sourceKey(cfg)
	if err != nil {
		return View{}, err
	}
	me, err := st.Me(ctx, key)
	if err != nil {
		return View{}, err //nolint:wrapcheck // propagate context cancellation
	}
	if me == "" {
		return View{}, fmt.Errorf("%s: run factory sync first", key)
	}
	items, err := st.Items(ctx, key)
	if err != nil {
		return View{}, err //nolint:wrapcheck // propagate context cancellation
	}
	claims, err := activeClaims(ctx, st, key)
	if err != nil {
		return View{}, err
	}
	open, err := st.OpenPRItems(ctx, key)
	if err != nil {
		return View{}, err //nolint:wrapcheck // propagate context cancellation
	}
	external, err := st.ExternalBlockers(ctx, key)
	if err != nil {
		return View{}, err //nolint:wrapcheck // propagate context cancellation
	}
	byID := make(map[string]source.Item, len(items))
	for _, it := range items {
		byID[it.ID] = it
	}
	return View{
		Key:    key,
		Cfg:    cfg,
		Snap:   ready.Snapshot{Items: byID, Claims: claims, Me: me, External: external},
		OpenPR: open,
	}, nil
}

// Classify returns the bucket of one open item. In flight wins, then ready,
// then needs-human, then blocked. Ready and blocked use ready.Evaluate and
// ready.Classify, the rules factory next uses.
func (v View) Classify(it source.Item) Bucket {
	if _, ok := v.Snap.Claims[it.ID]; ok || v.OpenPR[it.ID] {
		return InFlight
	}
	res := ready.Evaluate(it, v.Snap, v.Cfg, v.Cfg.Kind)
	if res.Ready {
		return Ready
	}
	for _, label := range v.Cfg.HumanLabels {
		if slices.Contains(it.Labels, label) {
			return NeedsHuman
		}
	}
	if _, blocked, _ := ready.Classify(res); blocked == 1 {
		return Blocked
	}
	return Other
}

// Count counts the open items of v that keep accepts. A nil keep accepts all.
func (v View) Count(keep func(source.Item) bool) Counts {
	out := Counts{ByStatus: map[string]int{}}
	for _, it := range v.Snap.Items {
		if it.Done || (keep != nil && !keep(it)) {
			continue
		}
		out.ByStatus[it.State]++
		switch v.Classify(it) {
		case InFlight:
			out.InFlight++
		case Ready:
			out.Ready++
		case NeedsHuman:
			out.NeedsHuman++
		case Blocked:
			out.Blocked++
		case Other:
		}
	}
	return out
}

// Hook returns a syncer AfterSync function. It stores one snapshot of the
// synced source, at most once per SnapshotEvery, and prunes snapshots older
// than SnapshotKeep.
func Hook(st *store.Store, cfg *config.Config) func(context.Context, string, time.Time) error {
	return func(ctx context.Context, sourceName string, now time.Time) error {
		src, err := sourceFor(cfg, sourceName)
		if err != nil {
			return err
		}
		view, err := Load(ctx, st, src)
		if err != nil {
			return err
		}
		counts := view.Count(nil)
		_, err = st.RecordBacklogSnapshot(ctx, store.BacklogSnapshot{
			Source:     sourceName,
			TakenAt:    now,
			Ready:      counts.Ready,
			Blocked:    counts.Blocked,
			InFlight:   counts.InFlight,
			NeedsHuman: counts.NeedsHuman,
			ByStatus:   counts.ByStatus,
		}, SnapshotEvery, SnapshotKeep)
		return err //nolint:wrapcheck // propagate context cancellation
	}
}

func sourceFor(cfg *config.Config, key string) (config.Source, error) {
	if cfg == nil {
		return config.Source{}, errors.New("backlog: expected a config, found nil")
	}
	for i := range cfg.Sources {
		got, err := sourceKey(cfg.Sources[i])
		if err != nil {
			return config.Source{}, err
		}
		if got == key {
			return cfg.Sources[i], nil
		}
	}
	return config.Source{}, fmt.Errorf("backlog %s: expected a configured source, found none", key)
}

func sourceKey(cfg config.Source) (string, error) {
	if cfg.Kind != "jira" && cfg.Kind != "github" {
		return "", fmt.Errorf("backlog: expected kind jira or github, found %q", cfg.Kind)
	}
	if cfg.Name == "" {
		return "", errors.New("backlog: expected a non-empty source name, found empty")
	}
	return cfg.Kind + ":" + cfg.Name, nil
}

func activeClaims(ctx context.Context, st *store.Store, key string) (map[string]store.ClaimState, error) {
	claims, err := st.ActiveClaims(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	out := map[string]store.ClaimState{}
	for _, claim := range claims {
		if claim.Source != key {
			continue
		}
		if _, ok := out[claim.ID]; ok {
			return nil, fmt.Errorf("active claims for %s/%s: expected one, found more than one", key, claim.ID)
		}
		out[claim.ID] = claim.State
	}
	return out, nil
}
