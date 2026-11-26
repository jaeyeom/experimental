// Package ready decides whether a work item is ready to start and why.
// It performs no I/O.
package ready

import (
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Snapshot is one source's items, active claims, herdr state, and open pull requests.
type Snapshot struct {
	Items      map[string]source.Item      // Keyed by item ID. IDs are unique within one source.
	Claims     map[string]store.ClaimState // Active claims by item ID. Any entry blocks.
	Tabs       []herdr.Tab
	Workspaces []herdr.Workspace
	OpenPRs    []gh.PR
	Me         string // Authenticated account id or login.
	// External are the last sync's records of blockers that are not in the
	// mirror, keyed by id. They are never work items.
	External map[string]store.ExternalBlocker
	// Approvals holds local content-hash approvals for this source, keyed by item ID.
	Approvals map[string]store.Approval
	// Now is the clock for the not-before rule. Zero uses time.Now.
	Now time.Time
}

// Rule is one readiness rule name.
type Rule string

const (
	// RuleDone is the "done" rule.
	RuleDone Rule = "done"
	// RuleType is the "type" rule.
	RuleType Rule = "type"
	// RuleLabels is the "labels" rule.
	RuleLabels Rule = "labels"
	// RuleAssignee is the "assignee" rule.
	RuleAssignee Rule = "assignee"
	// RuleStatus is the "status" rule.
	RuleStatus Rule = "status"
	// RuleTrust is the "trust" rule: untrusted-author gating and tamper guards.
	RuleTrust Rule = "trust"
	// RuleBlockers is the "blockers" rule.
	RuleBlockers Rule = "blockers"
	// RuleParent is the "parent" rule.
	RuleParent Rule = "parent"
	// RuleChildren is the "children" rule: an item with open children waits for them.
	RuleChildren Rule = "children"
	// RuleClaimed is the "claimed" rule.
	RuleClaimed Rule = "claimed"
	// RuleTab is the "tab" rule.
	RuleTab Rule = "tab"
	// RulePR is the "pr" rule.
	RulePR Rule = "pr"
	// RuleNotBefore is the "not_before" rule: a deferred item waits for its date.
	RuleNotBefore Rule = "not_before"
)

// Check is the result of one readiness rule.
type Check struct {
	Rule   Rule
	Pass   bool
	Detail string
}

// Result is every rule for one item. Ready is true when every check passes.
type Result struct {
	ItemID string
	Ready  bool
	Checks []Check
}

// Evaluate runs every readiness rule for it.
// Ready is true only when every rule passes. kind is "jira" or "github".
func Evaluate(it source.Item, snap Snapshot, cfg config.Source, kind string) Result {
	checks := []Check{
		checkDone(it),
		checkType(it, cfg),
		checkLabels(it, cfg),
		checkAssignee(it, snap, cfg),
		checkStatus(it, cfg),
		CheckTrust(it, snap, cfg),
		checkBlockers(it, snap, cfg),
		checkParent(it, snap, cfg),
		checkChildren(it, snap),
		checkClaimed(it, snap),
		checkTab(it, snap, cfg, kind),
		checkPR(it, snap, cfg),
		checkNotBefore(it, snap),
	}
	ready := true
	for _, c := range checks {
		if !c.Pass {
			ready = false
		}
	}
	return Result{ItemID: it.ID, Ready: ready, Checks: checks}
}

// checkNotBefore fails while the item's not-before date is after today, in
// local time. On and after that date it passes, with no write to the source.
func checkNotBefore(it source.Item, snap Snapshot) Check {
	if it.NotBefore == nil {
		return Check{Rule: RuleNotBefore, Pass: true}
	}
	if Deferred(it, snap.Now) {
		return Check{Rule: RuleNotBefore, Pass: false, Detail: "deferred until " + it.NotBefore.Format(time.DateOnly)}
	}
	return Check{Rule: RuleNotBefore, Pass: true, Detail: "not before " + it.NotBefore.Format(time.DateOnly) + " (passed)"}
}

// Deferred reports whether it has a not-before date after today's local
// date at now. A zero now uses time.Now.
func Deferred(it source.Item, now time.Time) bool {
	if it.NotBefore == nil {
		return false
	}
	if now.IsZero() {
		now = time.Now()
	}
	local := now.In(time.Local)
	today := time.Date(local.Year(), local.Month(), local.Day(), 0, 0, 0, 0, time.UTC)
	return it.NotBefore.After(today)
}

func checkDone(it source.Item) Check {
	if it.Done {
		return Check{Rule: RuleDone, Pass: false, Detail: "done"}
	}
	return Check{Rule: RuleDone, Pass: true}
}

func checkType(it source.Item, cfg config.Source) Check {
	for _, skip := range cfg.SkipTypes {
		if strings.EqualFold(it.Type, skip) {
			return Check{Rule: RuleType, Pass: false, Detail: fmt.Sprintf("type %q in skip_types", it.Type)}
		}
	}
	return Check{Rule: RuleType, Pass: true}
}

func effectiveClaim(cfg config.Source) config.Claim {
	if cfg.Claim == (config.Claim{}) {
		return config.Claim{
			RequireLeaf:       true,
			RequireUnblocked:  true,
			SourceContainment: true,
			Assign:            true,
			MarkInProgress:    true,
			InProgressRule:    config.InProgressBlock,
			Status:            cfg.ClaimStatus,
			Transition:        cfg.ClaimTransition,
			Label:             cfg.ClaimLabel,
		}
	}
	c := cfg.Claim
	if c.InProgressRule == "" {
		c.InProgressRule = config.InProgressBlock
	}
	if c.Status == "" {
		c.Status = cfg.ClaimStatus
	}
	if c.Label == "" {
		c.Label = cfg.ClaimLabel
	}
	return c
}

func checkLabels(it source.Item, cfg config.Source) Check {
	var missing, excluded []string
	for _, want := range cfg.RequireLabels {
		if !slices.Contains(it.Labels, want) {
			missing = append(missing, want)
		}
	}
	for _, bad := range cfg.ExcludeLabels {
		if slices.Contains(it.Labels, bad) {
			excluded = append(excluded, bad)
		}
	}
	var parts []string
	if len(missing) > 0 {
		parts = append(parts, "missing "+strings.Join(missing, ", "))
	}
	if len(excluded) > 0 {
		parts = append(parts, "excluded "+strings.Join(excluded, ", "))
	}
	claim := effectiveClaim(cfg)
	if claim.MarkInProgress && claim.InProgressRule != config.InProgressAllow && claim.Label != "" &&
		slices.Contains(it.Labels, claim.Label) && !slices.Contains(cfg.ExcludeLabels, claim.Label) {
		parts = append(parts, "already in progress ("+claim.Label+")")
	}
	if len(parts) == 0 {
		return Check{Rule: RuleLabels, Pass: true}
	}
	return Check{Rule: RuleLabels, Pass: false, Detail: strings.Join(parts, "; ")}
}

func checkAssignee(it source.Item, snap Snapshot, cfg config.Source) Check {
	mode := cfg.AssigneeOK
	if mode == "" && cfg.Claim.OwnershipRule != "" {
		switch cfg.Claim.OwnershipRule {
		case config.OwnershipUnassignedOrMe:
			mode = "me"
		case config.OwnershipUnassigned:
			mode = "unassigned"
		case config.OwnershipAny:
			mode = "any"
		default:
			mode = cfg.Claim.OwnershipRule
		}
	}
	switch mode {
	case "any":
		return Check{Rule: RuleAssignee, Pass: true}
	case "unassigned":
		if it.Assignee == "" {
			return Check{Rule: RuleAssignee, Pass: true}
		}
		return Check{Rule: RuleAssignee, Pass: false, Detail: fmt.Sprintf("assignee %q, want empty", it.Assignee)}
	case "me":
		if it.Assignee == "" || it.Assignee == snap.Me {
			return Check{Rule: RuleAssignee, Pass: true}
		}
		return Check{Rule: RuleAssignee, Pass: false, Detail: fmt.Sprintf("assignee %q, want empty or %q", it.Assignee, snap.Me)}
	default:
		return Check{Rule: RuleAssignee, Pass: false, Detail: fmt.Sprintf("assignee_ok: expected me, unassigned, or any, found %q", mode)}
	}
}

func checkStatus(it source.Item, cfg config.Source) Check {
	if len(cfg.ReadyStatuses) > 0 && !slices.Contains(cfg.ReadyStatuses, it.State) {
		return Check{Rule: RuleStatus, Pass: false, Detail: fmt.Sprintf("status %q not in ready_statuses", it.State)}
	}
	claim := effectiveClaim(cfg)
	if claim.MarkInProgress && claim.InProgressRule != config.InProgressAllow && claim.Status != "" && it.State == claim.Status {
		return Check{Rule: RuleStatus, Pass: false, Detail: fmt.Sprintf("status %q is already in progress", it.State)}
	}
	return Check{Rule: RuleStatus, Pass: true}
}

// IsTrustedAuthor reports whether it's author is trusted in snap under cfg:
// authenticated snap.Me, an association in trusted_author_associations, or an
// explicit login in trusted_authors. Empty author (e.g. deleted user) is never trusted.
func IsTrustedAuthor(it source.Item, snap Snapshot, cfg config.Source) bool {
	if it.Author != "" && snap.Me != "" && strings.EqualFold(it.Author, snap.Me) {
		return true
	}
	if it.AuthorAssociation != "" {
		for _, assoc := range cfg.Trust.EffectiveTrustedAuthorAssociations() {
			if strings.EqualFold(it.AuthorAssociation, assoc) {
				return true
			}
		}
	}
	if it.Author != "" {
		for _, login := range cfg.Trust.TrustedAuthors {
			if strings.EqualFold(it.Author, login) {
				return true
			}
		}
	}
	return false
}

// CheckTrust enforces [source.trust] gating and tamper guards when
// cfg.Trust.Environment is "untrusted". Unset or "trusted" always passes.
func CheckTrust(it source.Item, snap Snapshot, cfg config.Source) Check {
	if cfg.Trust.Environment != config.TrustEnvUntrusted {
		return Check{Rule: RuleTrust, Pass: true}
	}
	if IsTrustedAuthor(it, snap, cfg) {
		return Check{Rule: RuleTrust, Pass: true}
	}
	var parts []string
	statuses := cfg.Trust.EffectiveUntrustedReadyStatuses()
	switch {
	case len(statuses) == 0:
		parts = append(parts, fmt.Sprintf("untrusted author %q is blocked (untrusted_ready_statuses is empty)", it.Author))
	case !slices.Contains(statuses, it.State):
		parts = append(parts, fmt.Sprintf("untrusted author %q requires status in [%s] (got %q)", it.Author, strings.Join(statuses, ", "), it.State))
	}
	guards := cfg.Trust.EffectiveGuards()
	if len(guards) > 0 && !anyTrustGuardSatisfied(it, snap, guards) {
		parts = append(parts, trustGuardFailureReason(it, snap, guards))
	}
	if len(parts) == 0 {
		return Check{Rule: RuleTrust, Pass: true}
	}
	return Check{Rule: RuleTrust, Pass: false, Detail: strings.Join(parts, "; ")}
}

func anyTrustGuardSatisfied(it source.Item, snap Snapshot, guards []string) bool {
	for _, guard := range guards {
		switch guard {
		case config.TrustGuardLocked:
			if it.Locked {
				return true
			}
		case config.TrustGuardApprovedHash:
			if approvedHashMatches(it, snap) {
				return true
			}
		}
	}
	return false
}

func approvedHashMatches(it source.Item, snap Snapshot) bool {
	approval, ok := snap.Approvals[it.ID]
	if !ok || approval.ContentHash == "" || it.ContentHash == "" {
		return false
	}
	return approval.ContentHash == it.ContentHash
}

func approvedHashReason(it source.Item, snap Snapshot) string {
	approval, ok := snap.Approvals[it.ID]
	if !ok || approval.ContentHash == "" {
		return "not approved"
	}
	if it.ContentHash == "" {
		return "content hash unavailable; run sync"
	}
	return "edited since approval"
}

func trustGuardFailureReason(it source.Item, snap Snapshot, guards []string) string {
	if len(guards) == 1 {
		switch guards[0] {
		case config.TrustGuardLocked:
			return fmt.Sprintf("untrusted author %q requires locked issue", it.Author)
		case config.TrustGuardApprovedHash:
			return fmt.Sprintf("untrusted author %q requires approved_hash (%s)", it.Author, approvedHashReason(it, snap))
		}
	}
	base := fmt.Sprintf("untrusted author %q requires one of guards [%s]", it.Author, strings.Join(guards, ", "))
	if slices.Contains(guards, config.TrustGuardApprovedHash) {
		return fmt.Sprintf("%s (%s)", base, approvedHashReason(it, snap))
	}
	return base
}

func checkBlockers(it source.Item, snap Snapshot, cfg config.Source) Check {
	var parts []string
	pass := true
	for _, ref := range it.BlockedBy {
		notes, gate := blockerNotes(it, ref, snap, cfg)
		if gate {
			pass = false
		}
		parts = append(parts, notes...)
	}
	if len(parts) == 0 {
		return Check{Rule: RuleBlockers, Pass: true}
	}
	return Check{Rule: RuleBlockers, Pass: pass, Detail: strings.Join(parts, "; ")}
}

// checkParent applies the allowlist and the parent-done rule.
// A nil ParentAllowlist is unset. A non-nil list is set, even when it is empty.
// An empty Parent is no parent. A non-empty parent missing from the snapshot
// passes the done half of the rule.
func checkParent(it source.Item, snap Snapshot, cfg config.Source) Check {
	var parts []string
	if cfg.ParentAllowlist != nil && !slices.Contains(cfg.ParentAllowlist, it.Parent) {
		parts = append(parts, fmt.Sprintf("parent %q not in allowlist", it.Parent))
	}
	if it.Parent != "" {
		parent, ok := snap.Items[it.Parent]
		switch {
		case !ok:
			if len(parts) == 0 {
				return Check{Rule: RuleParent, Pass: true, Detail: "parent not mirrored"}
			}
			parts = append(parts, "parent not mirrored")
		case parent.Done:
			parts = append(parts, fmt.Sprintf("parent %s is done", it.Parent))
		}
	}
	if len(parts) == 0 {
		return Check{Rule: RuleParent, Pass: true}
	}
	return Check{Rule: RuleParent, Pass: false, Detail: strings.Join(parts, "; ")}
}

// checkChildren fails while any other mirrored item with Parent == it.ID is not done.
func checkChildren(it source.Item, snap Snapshot) Check {
	if it.ID == "" {
		return Check{Rule: RuleChildren, Pass: true}
	}
	var open []string
	for _, child := range snap.Items {
		if child.ID != it.ID && child.Parent == it.ID && !child.Done {
			open = append(open, child.ID)
		}
	}
	if len(open) == 0 {
		return Check{Rule: RuleChildren, Pass: true}
	}
	slices.Sort(open)
	parts := make([]string, len(open))
	for i, id := range open {
		parts[i] = fmt.Sprintf("has open child %s", id)
	}
	return Check{Rule: RuleChildren, Pass: false, Detail: strings.Join(parts, "; ")}
}

func checkClaimed(it source.Item, snap Snapshot) Check {
	state, ok := snap.Claims[it.ID]
	if !ok {
		return Check{Rule: RuleClaimed, Pass: true}
	}
	return Check{Rule: RuleClaimed, Pass: false, Detail: fmt.Sprintf("claim %s", state)}
}

func checkTab(it source.Item, snap Snapshot, cfg config.Source, kind string) Check {
	switch kind {
	case "jira":
		_, tabLabel := TabRef(it, cfg, kind)
		for _, tab := range snap.Tabs {
			if tab.Label == tabLabel {
				return Check{Rule: RuleTab, Pass: false, Detail: "tab " + tabLabel}
			}
		}
	case "github":
		wsLabel, tabLabel := TabRef(it, cfg, kind)
		ids := workspaceIDs(snap.Workspaces, wsLabel)
		for _, tab := range snap.Tabs {
			if _, ok := ids[tab.WorkspaceID]; ok && tab.Label == tabLabel {
				return Check{Rule: RuleTab, Pass: false, Detail: fmt.Sprintf("tab %s in workspace %s", tabLabel, wsLabel)}
			}
		}
	default:
		panic(fmt.Sprintf("tab ref: expected kind jira or github, found %q", kind))
	}
	return Check{Rule: RuleTab, Pass: true}
}

func workspaceIDs(workspaces []herdr.Workspace, label string) map[string]struct{} {
	ids := make(map[string]struct{})
	for _, ws := range workspaces {
		if ws.Label == label {
			ids[ws.ID] = struct{}{}
		}
	}
	return ids
}

func checkPR(it source.Item, snap Snapshot, cfg config.Source) Check {
	prs := gh.ForItem(snap.OpenPRs, it, cfg.PRTitlePattern)
	if len(prs) == 0 {
		return Check{Rule: RulePR, Pass: true}
	}
	urls := make([]string, len(prs))
	for i, pr := range prs {
		urls[i] = pr.URL
	}
	return Check{Rule: RulePR, Pass: false, Detail: strings.Join(urls, ", ")}
}

// Classify buckets one not-ready result by the first rule that failed.
// blockers counts as blocked. labels, type, status, trust, assignee, parent,
// and children count as not eligible. An item that is not done is open,
// including those two buckets. A done item is none of the three. claimed, tab,
// and pr are open only. factory next and the backlog counts share this.
func Classify(res Result) (open, blocked, notEligible int) {
	if res.Ready {
		return 0, 0, 0
	}
	for _, check := range res.Checks {
		if check.Pass {
			continue
		}
		switch check.Rule {
		case RuleDone:
			return 0, 0, 0
		case RuleBlockers:
			return 1, 1, 0
		case RuleType, RuleLabels, RuleAssignee, RuleStatus, RuleTrust, RuleParent, RuleChildren:
			return 1, 0, 1
		default:
			return 1, 0, 0
		}
	}
	return 0, 0, 0
}
