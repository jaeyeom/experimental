package stats

import (
	"cmp"
	"math"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Flow is how long work took, for pull requests merged in the window,
// plus the open draft pull requests.
type Flow struct {
	// Per item: claim to its first pull request opened.
	ClaimToPR Dist `json:"claim_to_first_pr"` //nolint:tagliatelle // documented factory JSON output field
	// Per pull request: opened to merged.
	PRToMerge Dist `json:"pr_open_to_merge"` //nolint:tagliatelle // documented factory JSON output field
	// Oldest first.
	Drafts []Draft `json:"open_drafts"` //nolint:tagliatelle // documented factory JSON output field
}

// Dist is a median and a p90, by nearest rank, in seconds. Both are nil
// when Count is 0.
type Dist struct {
	Count         int    `json:"count"`
	MedianSeconds *int64 `json:"median_seconds"` //nolint:tagliatelle // documented factory JSON output field
	P90Seconds    *int64 `json:"p90_seconds"`    //nolint:tagliatelle // documented factory JSON output field
}

// Draft is one open draft pull request.
type Draft struct {
	Repo       string    `json:"repo"`
	Number     int       `json:"number"`
	Source     string    `json:"source"`
	ID         string    `json:"id"`
	OpenedAt   time.Time `json:"opened_at"`   //nolint:tagliatelle // documented factory JSON output field
	AgeSeconds int64     `json:"age_seconds"` //nolint:tagliatelle // documented factory JSON output field
}

// sample is one flow duration and the claim it belongs to.
type sample struct {
	toPR  bool // True for claim to first PR, false for PR open to merge.
	dur   time.Duration
	claim *store.Claim
}

func (d *data) flow() Flow {
	var toPR, toMerge []time.Duration
	for _, s := range d.samples() {
		if s.toPR {
			toPR = append(toPR, s.dur)
		} else {
			toMerge = append(toMerge, s.dur)
		}
	}
	return Flow{ClaimToPR: dist(toPR), PRToMerge: dist(toMerge), Drafts: d.drafts()}
}

// samples are the flow durations for pull requests merged in the window.
// An item whose first pull request opened before any claim has no claim
// to first PR sample.
func (d *data) samples() []sample {
	var out []sample
	items := map[itemKey]bool{}
	for _, pr := range d.prs {
		k := itemKey{pr.Source, pr.ItemID}
		if !d.inScope(k) || !d.inWindow(pr.MergedAt) {
			continue
		}
		out = append(out, sample{dur: pr.MergedAt.Sub(pr.OpenedAt), claim: d.prClaim(pr)})
		items[k] = true
	}
	keys := make([]itemKey, 0, len(items))
	for k := range items {
		keys = append(keys, k)
	}
	slices.SortFunc(keys, compareKeys)
	for _, k := range keys {
		first := d.firstOpened(k)
		if c, ok := d.claimFor(k, first); ok {
			out = append(out, sample{toPR: true, dur: first.Sub(c.ClaimedAt), claim: &c})
		}
	}
	return out
}

func (d *data) firstOpened(k itemKey) time.Time {
	var first time.Time
	for _, pr := range d.prs {
		if pr.Source == k.source && pr.ItemID == k.id && (first.IsZero() || pr.OpenedAt.Before(first)) {
			first = pr.OpenedAt
		}
	}
	return first
}

func (d *data) drafts() []Draft {
	out := []Draft{}
	for _, pr := range d.prs {
		k := itemKey{pr.Source, pr.ItemID}
		if !pr.Draft || !pr.MergedAt.IsZero() || !pr.ClosedAt.IsZero() || !d.inScope(k) {
			continue
		}
		out = append(out, Draft{
			Repo: pr.Repo, Number: pr.Number, Source: pr.Source, ID: pr.ItemID,
			OpenedAt: pr.OpenedAt.UTC(), AgeSeconds: int64(d.opts.Now.Sub(pr.OpenedAt) / time.Second),
		})
	}
	slices.SortFunc(out, func(a, b Draft) int {
		if c := a.OpenedAt.Compare(b.OpenedAt); c != 0 {
			return c
		}
		if c := cmp.Compare(a.Repo, b.Repo); c != 0 {
			return c
		}
		return cmp.Compare(a.Number, b.Number)
	})
	return out
}

func dist(durs []time.Duration) Dist {
	out := Dist{Count: len(durs)}
	if len(durs) == 0 {
		return out
	}
	sorted := slices.Clone(durs)
	slices.Sort(sorted)
	out.MedianSeconds = rank(sorted, 0.5)
	out.P90Seconds = rank(sorted, 0.9)
	return out
}

// rank is the nearest-rank percentile p of sorted, in whole seconds.
func rank(sorted []time.Duration, p float64) *int64 {
	i := int(math.Ceil(p*float64(len(sorted)))) - 1
	i = max(i, 0)
	s := int64(sorted[i] / time.Second)
	return &s
}
