package stats

import (
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// DefaultModel is the model group of a claim whose agent args named none.
const DefaultModel = "default"

// Split is the throughput totals and flow per model or agent rule.
type Split struct {
	By   string     `json:"by"`
	Rows []SplitRow `json:"rows"`
}

// SplitRow is one group.
type SplitRow struct {
	Group     string `json:"group"`
	Counts           // Totals over the window.
	ClaimToPR Dist   `json:"claim_to_first_pr"` //nolint:tagliatelle // documented factory JSON output field
	PRToMerge Dist   `json:"pr_open_to_merge"`  //nolint:tagliatelle // documented factory JSON output field
}

func (d *data) split() *Split {
	if d.opts.Split == "" {
		return nil
	}
	rows := map[string]*SplitRow{}
	row := func(c *store.Claim) *SplitRow {
		g := group(c, d.opts.Split)
		if rows[g] == nil {
			rows[g] = &SplitRow{Group: g}
		}
		return rows[g]
	}
	for _, ev := range d.events() {
		row(ev.claim).add(ev.kind)
	}
	toPR := map[string][]time.Duration{}
	toMerge := map[string][]time.Duration{}
	for _, s := range d.samples() {
		r := row(s.claim)
		if s.toPR {
			toPR[r.Group] = append(toPR[r.Group], s.dur)
		} else {
			toMerge[r.Group] = append(toMerge[r.Group], s.dur)
		}
	}
	out := &Split{By: d.opts.Split, Rows: []SplitRow{}}
	for g, r := range rows {
		r.ClaimToPR = dist(toPR[g])
		r.PRToMerge = dist(toMerge[g])
		out.Rows = append(out.Rows, *r)
	}
	slices.SortFunc(out.Rows, func(a, b SplitRow) int { return strings.Compare(a.Group, b.Group) })
	return out
}

// group is the claim's agent rule or model. A claim from before those were
// recorded, or no claim, is unknown.
func group(c *store.Claim, by string) string {
	if c == nil || c.AgentRule == "" {
		return Unknown
	}
	if by == SplitRule {
		return c.AgentRule
	}
	if c.Model == "" {
		return DefaultModel
	}
	return c.Model
}
