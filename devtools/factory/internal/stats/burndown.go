package stats

import (
	"slices"
	"strings"
	"time"
)

// rateWindow is how far back the projection's finish rate looks.
const rateWindow = 7 * 24 * time.Hour

// Burndown is the remaining-work series for the text report. It is not in
// the JSON report.
type Burndown struct {
	// Points are the remaining items at the end of each bucket.
	Points []BurnPoint
	// UnknownCreated counts items with no create time, counted as present
	// from Start.
	UnknownCreated int
	// Start is the window start, where an item with no create time begins.
	Start time.Time
	// Rate is items finished per day over the last 7 days.
	Rate float64
	// RemainingNow is the remaining count at the report's now, the
	// projection's numerator.
	RemainingNow int
	// Projected is when remaining work reaches zero at Rate. Nil when Rate
	// is zero or nothing remains.
	Projected *time.Time
}

// BurnPoint is the remaining count at At.
type BurnPoint struct {
	At        time.Time
	Remaining int
}

// burnItem is one work item the burndown counts.
type burnItem struct {
	created  time.Time // Zero when unknown.
	finished time.Time // Zero when not finished.
	done     bool      // Done with no recorded finish time: never counted.
}

// burndown counts the leaf items in scope: an item with no children, whose
// type is not in its source's skip_types. Remaining at t is an item created
// on or before t and not finished by t. A done item with no finish time is
// left out, because when it finished is unknown.
func (d *data) burndown() *Burndown {
	items := d.burnItems()
	out := &Burndown{Start: d.start.UTC()}
	for _, it := range items {
		if it.created.IsZero() && !it.done {
			out.UnknownCreated++
		}
	}
	for _, s := range d.bucketStarts() {
		end := nextBucket(s, d.opts.By)
		if end.After(d.opts.Now) {
			end = d.opts.Now
		}
		out.Points = append(out.Points, BurnPoint{At: end.UTC(), Remaining: d.remainingAt(items, end)})
	}
	finishedRecently := 0
	for _, it := range items {
		if !it.finished.IsZero() && it.finished.After(d.opts.Now.Add(-rateWindow)) && !it.finished.After(d.opts.Now) {
			finishedRecently++
		}
	}
	out.Rate = float64(finishedRecently) / (rateWindow.Hours() / 24)
	now := d.remainingAt(items, d.opts.Now)
	out.RemainingNow = now
	if out.Rate > 0 && now > 0 {
		days := float64(now) / out.Rate
		at := d.opts.Now.Add(time.Duration(days * 24 * float64(time.Hour))).UTC()
		out.Projected = &at
	}
	return out
}

func (d *data) remainingAt(items []burnItem, t time.Time) int {
	n := 0
	for _, it := range items {
		if it.done {
			continue
		}
		created := it.created
		if created.IsZero() {
			created = d.start
		}
		if created.After(t) {
			continue
		}
		if !it.finished.IsZero() && !it.finished.After(t) {
			continue
		}
		n++
	}
	return n
}

func (d *data) burnItems() []burnItem {
	finished := map[itemKey]time.Time{}
	for _, f := range d.finished {
		finished[itemKey{f.Source, f.ID}] = f.FinishedAt
	}
	keys := make([]itemKey, 0, len(d.nodes))
	for k := range d.nodes {
		keys = append(keys, k)
	}
	slices.SortFunc(keys, compareKeys)
	var out []burnItem
	for _, k := range keys {
		n := d.nodes[k]
		if !d.inScope(k) || len(d.children[k]) > 0 || d.skippedType(k, n) {
			continue
		}
		it := burnItem{created: n.created, finished: finished[k]}
		if n.done && it.finished.IsZero() {
			it.done = true
		}
		out = append(out, it)
	}
	return out
}

// skippedType reports whether a mirrored item's type is in its source's
// skip_types, such as an epic with no children.
func (d *data) skippedType(k itemKey, n node) bool {
	if !n.mirrored {
		return false
	}
	for i := range d.cfg.Sources {
		src := d.cfg.Sources[i]
		if src.Kind+":"+src.Name != k.source {
			continue
		}
		return slices.ContainsFunc(src.SkipTypes, func(t string) bool { return strings.EqualFold(t, n.item.Type) })
	}
	return false
}
