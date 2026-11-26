package stats

import (
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Throughput is finished, started, and pull request counts per bucket.
type Throughput struct {
	By      string   `json:"by"`
	Buckets []Bucket `json:"buckets"`
	Total   Counts   `json:"total"`
}

// Bucket is one day or week, starting at Start.
type Bucket struct {
	Start time.Time `json:"start"`
	Counts
}

// Counts is one throughput row.
type Counts struct {
	Finished  int `json:"finished"`
	Started   int `json:"started"`
	PRsOpened int `json:"prs_opened"` //nolint:tagliatelle // documented factory JSON output field
	PRsMerged int `json:"prs_merged"` //nolint:tagliatelle // documented factory JSON output field
}

type eventKind int

const (
	evFinished eventKind = iota
	evStarted
	evOpened
	evMerged
)

// event is one throughput event in the window and in scope.
type event struct {
	kind  eventKind
	at    time.Time
	key   itemKey
	claim *store.Claim // The claim the event belongs to, or nil.
}

func (c *Counts) add(kind eventKind) {
	switch kind {
	case evFinished:
		c.Finished++
	case evStarted:
		c.Started++
	case evOpened:
		c.PRsOpened++
	case evMerged:
		c.PRsMerged++
	}
}

func (d *data) events() []event {
	var out []event
	for _, f := range d.finished {
		k := itemKey{f.Source, f.ID}
		if d.inScope(k) && d.inWindow(f.FinishedAt) {
			out = append(out, event{kind: evFinished, at: f.FinishedAt, key: k, claim: d.finishClaim(f)})
		}
	}
	for i := range d.claims {
		c := d.claims[i]
		k := itemKey{c.Source, c.ID}
		if c.State != store.Failed && d.inScope(k) && d.inWindow(c.ClaimedAt) {
			out = append(out, event{kind: evStarted, at: c.ClaimedAt, key: k, claim: &c})
		}
	}
	for _, pr := range d.prs {
		k := itemKey{pr.Source, pr.ItemID}
		if !d.inScope(k) {
			continue
		}
		claim := d.prClaim(pr)
		if d.inWindow(pr.OpenedAt) {
			out = append(out, event{kind: evOpened, at: pr.OpenedAt, key: k, claim: claim})
		}
		if d.inWindow(pr.MergedAt) {
			out = append(out, event{kind: evMerged, at: pr.MergedAt, key: k, claim: claim})
		}
	}
	return out
}

// finishClaim is the claim recorded on f, else the latest claim before it.
func (d *data) finishClaim(f store.Finished) *store.Claim {
	k := itemKey{f.Source, f.ID}
	at := f.FinishedAt
	if !f.ClaimedAt.IsZero() {
		at = f.ClaimedAt
	}
	if c, ok := d.claimFor(k, at); ok {
		return &c
	}
	return nil
}

// prClaim is the item's latest claim at or before the pull request opened.
func (d *data) prClaim(pr store.PR) *store.Claim {
	if c, ok := d.claimFor(itemKey{pr.Source, pr.ItemID}, pr.OpenedAt); ok {
		return &c
	}
	return nil
}

func (d *data) throughput() Throughput {
	out := Throughput{By: d.opts.By}
	index := map[time.Time]int{}
	for _, t := range d.bucketStarts() {
		index[t] = len(out.Buckets)
		// JSON stays UTC; the instant is the local bucket start.
		out.Buckets = append(out.Buckets, Bucket{Start: t.UTC()})
	}
	for _, ev := range d.events() {
		i := index[bucketStart(ev.at, d.opts.By, d.opts.Loc)]
		out.Buckets[i].add(ev.kind)
		out.Total.add(ev.kind)
	}
	return out
}

// bucketStarts are the starts of every bucket from the window start to now,
// in d.opts.Loc.
func (d *data) bucketStarts() []time.Time {
	var out []time.Time
	last := bucketStart(d.opts.Now, d.opts.By, d.opts.Loc)
	for t := bucketStart(d.start, d.opts.By, d.opts.Loc); !t.After(last); t = nextBucket(t, d.opts.By) {
		out = append(out, t)
	}
	return out
}

// bucketStart is midnight of t's day in loc, or of the Monday of t's week.
// It is built with time.Date, so a DST day keeps its 23 or 25 hours.
func bucketStart(t time.Time, by string, loc *time.Location) time.Time {
	l := t.In(loc)
	day := time.Date(l.Year(), l.Month(), l.Day(), 0, 0, 0, 0, loc)
	if by != ByWeek {
		return day
	}
	back := (int(day.Weekday()) + 6) % 7
	return day.AddDate(0, 0, -back)
}

// nextBucket is the next local midnight a day or a week later. AddDate
// keeps the wall clock, so it stays midnight across DST.
func nextBucket(t time.Time, by string) time.Time {
	if by == ByWeek {
		return t.AddDate(0, 0, 7)
	}
	return t.AddDate(0, 0, 1)
}
