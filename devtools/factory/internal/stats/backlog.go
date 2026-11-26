package stats

import (
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// BacklogPart is the current backlog and its change since the snapshot
// nearest the window start. Change is nil (n/a) when a source has no
// snapshot, or under --epic, because snapshots are not kept per subtree.
type BacklogPart struct {
	Now        BacklogCounts  `json:"now"`
	Change     *BacklogCounts `json:"change"`
	SnapshotAt []SourceSync   `json:"snapshot_at,omitempty"` //nolint:tagliatelle // documented factory JSON output field
}

// BacklogCounts is one set of backlog counts.
type BacklogCounts struct {
	Ready      int            `json:"ready"`
	Blocked    int            `json:"blocked"`
	InFlight   int            `json:"in_flight"`   //nolint:tagliatelle // documented factory JSON output field
	NeedsHuman int            `json:"needs_human"` //nolint:tagliatelle // documented factory JSON output field
	ByStatus   map[string]int `json:"by_status"`   //nolint:tagliatelle // documented factory JSON output field
}

func (d *data) backlog() BacklogPart {
	var total backlog.Counts
	total.ByStatus = map[string]int{}
	keep := func(it source.Item) bool { return d.inScope(itemKey{it.Source, it.ID}) }
	for _, k := range d.sourceKeys() {
		total.Add(d.views[k].Count(keep))
	}
	out := BacklogPart{Now: fromCounts(total)}
	if d.scope != nil {
		return out
	}
	base, at, ok := d.nearestSnapshots()
	if !ok {
		return out
	}
	change := diff(out.Now, base)
	out.Change = &change
	out.SnapshotAt = at
	return out
}

func (d *data) sourceKeys() []string {
	keys := make([]string, 0, len(d.views))
	for k := range d.views {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	return keys
}

// nearestSnapshots sums, per source, the snapshot closest to the window
// start. ok is false when any source has none.
func (d *data) nearestSnapshots() (BacklogCounts, []SourceSync, bool) {
	sum := BacklogCounts{ByStatus: map[string]int{}}
	var at []SourceSync
	for _, k := range d.sourceKeys() {
		snap, ok := nearest(d.snaps, k, d.start)
		if !ok {
			return BacklogCounts{}, nil, false
		}
		sum.Ready += snap.Ready
		sum.Blocked += snap.Blocked
		sum.InFlight += snap.InFlight
		sum.NeedsHuman += snap.NeedsHuman
		for status, n := range snap.ByStatus {
			sum.ByStatus[status] += n
		}
		at = append(at, SourceSync{Source: k, At: timePtr(snap.TakenAt)})
	}
	return sum, at, len(at) > 0
}

func nearest(snaps []store.BacklogSnapshot, key string, t time.Time) (store.BacklogSnapshot, bool) {
	var best store.BacklogSnapshot
	found := false
	for _, s := range snaps {
		if s.Source != key {
			continue
		}
		if !found || absDur(s.TakenAt.Sub(t)) < absDur(best.TakenAt.Sub(t)) {
			best, found = s, true
		}
	}
	return best, found
}

func absDur(d time.Duration) time.Duration {
	if d < 0 {
		return -d
	}
	return d
}

func fromCounts(c backlog.Counts) BacklogCounts {
	return BacklogCounts{Ready: c.Ready, Blocked: c.Blocked, InFlight: c.InFlight, NeedsHuman: c.NeedsHuman, ByStatus: c.ByStatus}
}

func diff(now, base BacklogCounts) BacklogCounts {
	out := BacklogCounts{
		Ready:      now.Ready - base.Ready,
		Blocked:    now.Blocked - base.Blocked,
		InFlight:   now.InFlight - base.InFlight,
		NeedsHuman: now.NeedsHuman - base.NeedsHuman,
		ByStatus:   map[string]int{},
	}
	for status, n := range now.ByStatus {
		out.ByStatus[status] += n
	}
	for status, n := range base.ByStatus {
		out.ByStatus[status] -= n
	}
	return out
}
