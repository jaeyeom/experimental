// Package doctor reports dependency-graph and claim problems.
// It performs no I/O and changes nothing.
package doctor

import (
	"cmp"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	// CheckCycle is a cycle in the blocks graph.
	CheckCycle = "cycle"
	// CheckUnknownBlocker is a blocker id that is not in the mirror.
	CheckUnknownBlocker = "unknown-blocker"
	// CheckParentBlocksChild is an ancestor that blocks its own descendant.
	CheckParentBlocksChild = "parent-blocks-child"
	// CheckChildBlocksParent is a descendant that blocks its own ancestor.
	CheckChildBlocksParent = "child-blocks-parent"
	// CheckOrphan is an open Jira item with no parent, outside skip_types,
	// orphan_ignore, and orphan_ignore_labels.
	CheckOrphan = "orphan"
	// CheckBadDeferLabel is a GitHub label with defer_label_prefix whose
	// date does not parse, so it defers nothing.
	CheckBadDeferLabel = "bad-defer-label"
	// CheckDroppedClaim is an item in claim_status with no tab and no open pull request.
	CheckDroppedClaim = "dropped-claim"
	// CheckStaleTab is a done item's tab whose agent is idle or done.
	CheckStaleTab = "stale-tab"
	// CheckStalled is a started claim older than StallAfter with no open pull request.
	CheckStalled = "stalled"
	// CheckAlmostReady is an item that fails only a missing require_labels entry.
	CheckAlmostReady = "almost-ready"
	// CheckPriorityInversion is a less urgent item blocking a more urgent one.
	CheckPriorityInversion = "priority-inversion"
	// CheckDuplicateClaim is an item with more than one active claim row.
	CheckDuplicateClaim = "duplicate-claim"
	// CheckClosableParent is an open parent whose known children are all done.
	CheckClosableParent = "closable-parent"
)

// Severity is how serious a finding is.
type Severity string

const (
	// SeverityWarn is a problem worth fixing.
	SeverityWarn Severity = "warn"
	// SeverityInfo is a candidate, not a broken edge.
	SeverityInfo Severity = "info"
)

// Finding is one doctor report.
// Items for a stale tab are the work item id and then the tab id.
// Other checks list work item ids only.
type Finding struct {
	Check      string   `json:"check"`
	Severity   Severity `json:"severity"`
	Items      []string `json:"items"`
	Message    string   `json:"message"`
	Suggestion string   `json:"suggestion"`
	// Suggestions lists the options that Suggestion joins, when a finding
	// has more than one. It is empty otherwise.
	Suggestions []string `json:"suggestions,omitempty"`
}

// BlockerStatus is the result of looking up a blocker outside the mirror.
type BlockerStatus string

const (
	// BlockerFound means the source returned the blocker.
	BlockerFound BlockerStatus = "found"
	// BlockerNotFound means the source has no such item, or it is not visible.
	BlockerNotFound BlockerStatus = "not-found"
	// BlockerUnchecked means the blocker was not looked up: the lookup bound
	// was reached, or the fetch failed.
	BlockerUnchecked BlockerStatus = "unchecked"
)

// BlockerLookup is one blocker fetched by id because it is not in the mirror.
type BlockerLookup struct {
	Status BlockerStatus
	// Item is the fetched blocker when Status is BlockerFound.
	Item source.Item
	// Reason says why the blocker was not checked when Status is BlockerUnchecked.
	Reason string
}

// LaneTabs are one configured lane's herdr workspaces and tabs.
type LaneTabs struct {
	Workspaces []herdr.Workspace
	Tabs       []herdr.Tab
	// Unreachable is why the lane machine did not answer. Empty means it did.
	Unreachable string
}

// Input is one source's doctor inputs.
// Check does not modify it.
type Input struct {
	// Kind is "jira" or "github".
	Kind string
	// SourceKey is the mirror key, for example jira:acme.
	SourceKey string
	// Config is the source's readiness and claim settings.
	Config config.Source
	// Snapshot is the mirror, tabs, workspaces, open pull requests, and me.
	Snapshot ready.Snapshot
	// Claims are local claim rows. Rows for other sources are ignored.
	Claims []store.Claim
	// Finished are local finished-item rows. Rows for other sources are ignored.
	Finished []store.Finished
	// Now is the clock stalled claims are compared to.
	Now time.Time
	// StallAfter is how long a started claim may sit with no open pull request.
	StallAfter time.Duration
	// Blockers are lookups of blocker ids that are not in the mirror, keyed
	// by id. A blocker with no entry is reported as not in the mirror.
	Blockers map[string]BlockerLookup
	// Lanes are the configured lanes' tabs, keyed by lane name. A claim
	// recorded on a lane in this map is matched against that lane's tabs.
	Lanes map[string]LaneTabs
	// LandedCommits maps item IDs in merge output mode to true when a linear
	// commit for the item has landed on origin/<base>.
	LandedCommits map[string]bool
}

// Check reports every problem in one source.
// A Done blocker is not a problem for the other checks: readiness already
// treats that link as satisfied, so Check does not report it there and does
// not suggest unlink for that reason. parent-blocks-child still lists an
// ancestor that blocks its descendant, including when that ancestor is Done.
// priority-inversion is a not-done blocker whose rank is worse than a not-done
// item it blocks. The suggestion names factory priorities --apply. Check does
// not change priority.
// The result is empty when nothing is wrong. It is never nil.
func Check(in Input) ([]Finding, error) {
	if err := validate(in); err != nil {
		return nil, err
	}
	var out []Finding
	parts, err := cycles(in)
	if err != nil {
		return nil, err
	}
	out = append(out, parts...)
	out = append(out, unknownBlockers(in)...)
	links, err := parentChildLinks(in)
	if err != nil {
		return nil, err
	}
	out = append(out, links...)
	out = append(out, orphans(in)...)
	out = append(out, badDeferLabels(in)...)
	dropped, err := droppedClaims(in)
	if err != nil {
		return nil, err
	}
	out = append(out, dropped...)
	tabs, err := staleTabs(in)
	if err != nil {
		return nil, err
	}
	out = append(out, tabs...)
	stalled, err := stalledClaims(in)
	if err != nil {
		return nil, err
	}
	out = append(out, stalled...)
	out = append(out, almostReady(in)...)
	inversions, err := priorityInversions(in)
	if err != nil {
		return nil, err
	}
	out = append(out, inversions...)
	dups, err := duplicateClaims(in)
	if err != nil {
		return nil, err
	}
	out = append(out, dups...)
	closable, err := closableParents(in)
	if err != nil {
		return nil, err
	}
	out = append(out, closable...)
	return order(out), nil
}

func validate(in Input) error {
	if in.Kind != "jira" && in.Kind != "github" {
		return fmt.Errorf("doctor: expected kind jira or github, found %q", in.Kind)
	}
	if in.SourceKey == "" {
		return errors.New("doctor: expected a non-empty source key, found empty")
	}
	if in.Now.IsZero() {
		return errors.New("doctor: expected a clock time, found zero")
	}
	for _, id := range sortedIDs(in.Snapshot.Items) {
		if err := validateItem(in, id); err != nil {
			return err
		}
	}
	if in.Kind == "github" && in.Config.Workspace == "" {
		return repoLabel(in.Config.Repo)
	}
	return nil
}

func validateItem(in Input, id string) error {
	it := in.Snapshot.Items[id]
	if it.ID != id {
		return fmt.Errorf("doctor: expected item id %q, found %q", id, it.ID)
	}
	if in.Kind == "github" {
		if _, _, err := source.ParseGitHubID(id); err != nil {
			return fmt.Errorf("doctor: %w", err)
		}
	}
	for _, ref := range it.BlockedBy {
		if ref.ID == "" {
			return fmt.Errorf("doctor: item %s: expected a non-empty blocker id, found empty", id)
		}
	}
	return nil
}

// repoLabel accepts the same owner/repo shape TabRef panics on when Workspace is empty.
func repoLabel(repo string) error {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") ||
		strings.ContainsAny(owner, " \t") || strings.ContainsAny(name, " \t") {
		return fmt.Errorf("doctor: expected owner/repo, found %q", repo)
	}
	return nil
}

func sortedIDs(items map[string]source.Item) []string {
	ids := make([]string, 0, len(items))
	for id := range items {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	return ids
}

func order(findings []Finding) []Finding {
	if len(findings) == 0 {
		return []Finding{}
	}
	slices.SortStableFunc(findings, compareFindings)
	return findings
}

func compareFindings(a, b Finding) int {
	if c := cmp.Compare(checkRank(a.Check), checkRank(b.Check)); c != 0 {
		return c
	}
	if c := compareStrings(a.Items, b.Items); c != 0 {
		return c
	}
	if c := cmp.Compare(a.Message, b.Message); c != 0 {
		return c
	}
	return cmp.Compare(a.Suggestion, b.Suggestion)
}

func checkRank(check string) int {
	switch check {
	case CheckCycle:
		return 0
	case CheckUnknownBlocker:
		return 1
	case CheckParentBlocksChild:
		return 2
	case CheckChildBlocksParent:
		return 3
	case CheckOrphan:
		return 4
	case CheckDroppedClaim:
		return 5
	case CheckStaleTab:
		return 6
	case CheckStalled:
		return 7
	case CheckAlmostReady:
		return 8
	case CheckPriorityInversion:
		return 9
	case CheckDuplicateClaim:
		return 10
	case CheckClosableParent:
		return 11
	default:
		return 100
	}
}

func compareStrings(a, b []string) int {
	n := min(len(a), len(b))
	for i := 0; i < n; i++ {
		if c := cmp.Compare(a[i], b[i]); c != 0 {
			return c
		}
	}
	return cmp.Compare(len(a), len(b))
}

func finding(check string, severity Severity, message, suggestion string, items ...string) Finding {
	if items == nil {
		items = []string{}
	}
	copied := make([]string, len(items))
	copy(copied, items)
	return Finding{
		Check:      check,
		Severity:   severity,
		Items:      copied,
		Message:    message,
		Suggestion: suggestion,
	}
}
