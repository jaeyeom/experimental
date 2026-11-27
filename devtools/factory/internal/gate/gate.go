// Package gate decides whether it is safe to start another agent now.
// Check evaluates every start condition and returns each failing reason.
package gate

import (
	"context"
	"fmt"
	"log/slog"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Inputs are the facts Check needs.
type Inputs struct {
	// Tabs are the herdr tabs scanned for a working agent.
	Tabs []herdr.Tab
	// Panes are the herdr panes used to leave out the caller and ignored panes.
	Panes []herdr.Pane
	// CallerPaneID is the pane running factory. Empty means no pane is excluded.
	CallerPaneID string
	// IgnorePanes are pane ids the working-agent check skips.
	IgnorePanes []string
	// IgnoreTabs are tab labels the working-agent check skips.
	IgnoreTabs []string
	// Checkouts are the configured source directories.
	// A working agent counts only when its cwd or foreground_cwd is one of
	// these directories or a subdirectory of one.
	// Nil uses cwd. An empty path in the list is a failing reason.
	Checkouts []string
	// HoldFiles are paths that block a start while they exist.
	// The caller includes the default path when the config list is empty.
	// An empty file still holds.
	HoldFiles []string
	// HoldTabs are tab labels that block a start until the tab is missing or done.
	HoldTabs []string
	// HoldTTL, when set, marks a hold file older than this duration as stale.
	// A stale hold still refuses. Nil means no age check.
	HoldTTL *time.Duration
	// GitStatus returns porcelain text for a directory.
	// A non-nil error is a failing reason. The text is not used in that case.
	GitStatus func(ctx context.Context, dir string) (string, error)
	// BaseBranch is the branch the checkout is checked against.
	// Empty means no branch check and no git command beyond GitStatus.
	BaseBranch string
	// Require is config.GateOnBase or config.GateCleanAgainstBase.
	// It is read only when BaseBranch is set.
	Require string
	// GitBranch returns the short name of HEAD. "HEAD" means detached.
	// It is called only when BaseBranch is set and Require is known.
	GitBranch func(ctx context.Context, dir string) (string, error)
	// GitAhead returns how many commits are on HEAD and not on origin/base.
	// It is called only when Require is config.GateCleanAgainstBase.
	GitAhead func(ctx context.Context, dir, base string) (int, error)
	// ActiveClaims are the claims counted against the WIP limit.
	ActiveClaims []store.Claim
	// Pulls are the open pull requests the gh client's OpenByMe query returned.
	// A pull request that is the same work as a claim counts once.
	Pulls []gh.PR
	// PRTitlePattern returns the pr_title_pattern for a claim source key.
	// It is called only when a claim is matched by title or head branch name.
	PRTitlePattern func(source string) (string, error)
	// LastStartedAt is the time of the latest start attempt.
	// Zero means nothing has started.
	LastStartedAt time.Time
	// Now is the clock Check compares to LastStartedAt.
	Now time.Time
	// LeaseGate is config.LeaseGateAuto, LeaseGateOff, LeaseGateRequired,
	// LeaseGateQueue, or LeaseGateQueueRequired.
	// Empty means no lease check, as before the lease existed. Load always
	// sets a value. Any other value is a failing reason.
	LeaseGate string
	// Lease reads the agentbus lease of a checkout. It is called only when
	// LeaseGate is auto, required, queue, or queue-required.
	Lease func(ctx context.Context, dir string) (agentbus.Reading, error)
	// LeaseOwner is the agent this start is for. A lease held by it is
	// usable. The zero value matches no holder, so only a free lease is.
	LeaseOwner agentbus.Owner
	// Leases lists every checkout's lease from the hub (`agentbus lease
	// status --all`). CheckLane reads it for a remote lane, to count a
	// claim that holds <machine>:<checkout> as a running worker. Nil skips that.
	Leases func(ctx context.Context) ([]agentbus.Status, error)
	// MaxWorkers caps concurrent running workers when > 1 (non-lane mode).
	// Values <= 1 keep the single-worker check (any working tab refuses).
	MaxWorkers int
	// Worktree is true when each worker runs in its own git worktree.
	Worktree bool
}

// Result is every start check.
// OK is true only when Reasons is empty.
type Result struct {
	// OK is true when every check passed.
	OK bool `json:"ok"`
	// Reasons is every failing reason, in check order. Nil when OK is true.
	Reasons []string `json:"reasons"`
	// InFlight is active claims plus open pull requests, minus Overlap.
	// It is the count compared with MaxInFlight.
	InFlight int `json:"inFlight"`
	// Claims is the number of active claims.
	Claims int `json:"claims"`
	// OpenPRs is the number of open pull requests.
	OpenPRs int `json:"openPRs"`
	// Overlap is how many open pull requests are the same work as a claim.
	Overlap int `json:"overlap"`
	// MaxInFlight is the WIP limit InFlight was compared with.
	MaxInFlight int `json:"maxInFlight"`
	// Notes are facts that did not refuse the start, for example a lease
	// that could not be read under lease gate auto.
	Notes []string `json:"notes,omitempty"`
	// LeaseGate is the configured [lease] gate mode that applied.
	LeaseGate string `json:"leaseGate,omitempty"`
}

// Check reports whether it is safe to start another agent in cwd.
// It evaluates every condition. A git failure is a reason, not a panic.
// d supplies the WIP limit and the minimum time since the last start.
// An empty BaseBranch does not run a branch check.
func Check(ctx context.Context, in Inputs, cwd string, d config.Defaults) Result {
	reasons := holdFileReasons(in)
	reasons = append(reasons, holdTabReasons(in)...)
	if in.MaxWorkers > 1 {
		reasons = append(reasons, parallelWorkingReasons(in, cwd)...)
	} else {
		reasons = append(reasons, workingReasons(in, cwd)...)
	}
	lease := leaseCheck(ctx, in, cwd)
	if !lease.skipCheckout {
		reasons = append(reasons, checkoutReasons(ctx, in, cwd)...)
	}
	if lease.reason != "" {
		reasons = append(reasons, lease.reason)
	}
	reasons, flight, counted := applyFlight(reasons, in, d)
	if elapsed := in.Now.Sub(in.LastStartedAt); elapsed < d.MinStartInterval {
		reasons = append(reasons, fmt.Sprintf("last start %s ago (min %s)", elapsed, d.MinStartInterval))
	}
	return finishFlight(Result{
		OK:        len(reasons) == 0,
		Reasons:   reasons,
		Notes:     lease.notes,
		LeaseGate: in.LeaseGate,
	}, flight, counted, d.MaxInFlight)
}

func applyFlight(reasons []string, in Inputs, d config.Defaults) ([]string, Flight, bool) {
	flight, err := countInFlight(in.ActiveClaims, in.Pulls, in.PRTitlePattern)
	if err != nil {
		return append(reasons, err.Error()), Flight{}, false
	}
	if reason := inFlightLine(flight, d.MaxInFlight); reason != "" {
		reasons = append(reasons, reason)
	}
	return reasons, flight, true
}

func finishFlight(res Result, flight Flight, counted bool, limit int) Result {
	res.MaxInFlight = limit
	if !counted {
		return res
	}
	res.InFlight = flight.InFlight
	res.Claims = flight.Claims
	res.OpenPRs = flight.OpenPRs
	res.Overlap = flight.Overlap
	return res
}

func workingReasons(in Inputs, cwd string) []string {
	if reason := paneShape(in.Panes); reason != "" {
		return []string{reason}
	}
	if reason := listShape("gate.ignore_panes", "pane id", in.IgnorePanes); reason != "" {
		return []string{reason}
	}
	if reason := listShape("gate.ignore_tabs", "tab label", in.IgnoreTabs); reason != "" {
		return []string{reason}
	}
	byID, reason := tabIndex(in.Tabs)
	if reason != "" {
		return []string{reason}
	}
	roots, reason := checkoutRoots(in, cwd)
	if reason != "" {
		return []string{reason}
	}
	return countedTabs(in, byID, roots)
}

func parallelWorkingReasons(in Inputs, cwd string) []string {
	if reason := paneShape(in.Panes); reason != "" {
		return []string{reason}
	}
	if reason := listShape("gate.ignore_panes", "pane id", in.IgnorePanes); reason != "" {
		return []string{reason}
	}
	if reason := listShape("gate.ignore_tabs", "tab label", in.IgnoreTabs); reason != "" {
		return []string{reason}
	}
	byID, reason := tabIndex(in.Tabs)
	if reason != "" {
		return []string{reason}
	}
	roots, reason := checkoutRoots(in, cwd)
	if reason != "" {
		return []string{reason}
	}
	if missing := missingPaneTabs(in, byID, stringSet(in.IgnorePanes)); len(missing) > 0 {
		return missing
	}
	running := countRunningWorkers(in, byID, roots)
	if running >= in.MaxWorkers {
		return []string{fmt.Sprintf("%d running workers (max_workers %d)", running, in.MaxWorkers)}
	}
	return nil
}

func countRunningWorkers(in Inputs, byID map[string]herdr.Tab, roots []string) int {
	claimTabs := make(map[string]struct{}, len(in.ActiveClaims))
	running := 0
	for _, claim := range in.ActiveClaims {
		if claim.TabID != "" {
			claimTabs[claim.TabID] = struct{}{}
		}
		_, tabOpen := byID[claim.TabID]
		switch {
		case claim.State == store.Claimed:
			running++
		case claim.TabID != "" && tabOpen:
			running++
		case claim.State == store.Started && claim.TabID == "":
			running++
		}
	}
	ignoredPanes := stringSet(in.IgnorePanes)
	ignoredTabs := stringSet(in.IgnoreTabs)
	for _, tab := range in.Tabs {
		if _, tracked := claimTabs[tab.ID]; tracked {
			continue
		}
		if _, skip := ignoredTabs[tab.Label]; skip {
			continue
		}
		if tabCounts(tab, in.Panes, in.CallerPaneID, ignoredPanes, roots) {
			running++
		}
	}
	return running
}

func checkoutRoots(in Inputs, cwd string) ([]string, string) {
	roots := in.Checkouts
	if roots == nil {
		if cwd == "" {
			return nil, ""
		}
		roots = []string{cwd}
	}
	for _, root := range roots {
		if root == "" {
			return nil, "checkouts: expected a non-empty path, found empty"
		}
	}
	return roots, ""
}

func paneShape(panes []herdr.Pane) string {
	seen := make(map[string]struct{}, len(panes))
	for _, pane := range panes {
		if pane.ID == "" {
			return "panes: expected a non-empty pane id, found empty"
		}
		if _, ok := seen[pane.ID]; ok {
			return fmt.Sprintf("panes: expected unique pane id %q, found more than one", pane.ID)
		}
		seen[pane.ID] = struct{}{}
		if pane.TabID == "" {
			return fmt.Sprintf("pane %s: expected a non-empty tab id, found empty", pane.ID)
		}
		if reason := paneStatus(pane); reason != "" {
			return reason
		}
	}
	return ""
}

func paneStatus(pane herdr.Pane) string {
	switch pane.AgentStatus {
	case herdr.AgentStatusIdle, herdr.AgentStatusWorking, herdr.AgentStatusBlocked, herdr.AgentStatusDone, herdr.AgentStatusUnknown:
		return ""
	default:
		return fmt.Sprintf("pane %s: expected agent_status one of idle, working, blocked, done, unknown, found %q", pane.ID, pane.AgentStatus)
	}
}

func listShape(name, what string, values []string) string {
	for _, value := range values {
		if value == "" {
			return fmt.Sprintf("%s: expected a non-empty %s, found empty", name, what)
		}
	}
	return ""
}

func tabIndex(tabs []herdr.Tab) (map[string]herdr.Tab, string) {
	byID := make(map[string]herdr.Tab, len(tabs))
	for _, tab := range tabs {
		if tab.ID == "" {
			return nil, "tabs: expected a non-empty tab id, found empty"
		}
		if _, ok := byID[tab.ID]; ok {
			return nil, fmt.Sprintf("tabs: expected unique tab id %q, found more than one", tab.ID)
		}
		byID[tab.ID] = tab
	}
	return byID, ""
}

func countedTabs(in Inputs, byID map[string]herdr.Tab, roots []string) []string {
	ignoredPanes := stringSet(in.IgnorePanes)
	ignoredTabs := stringSet(in.IgnoreTabs)
	callerTab := tabOfPane(in.Panes, in.CallerPaneID)
	var reasons []string
	for _, tab := range in.Tabs {
		if _, skip := ignoredTabs[tab.Label]; skip {
			continue
		}
		if !tabCounts(tab, in.Panes, in.CallerPaneID, ignoredPanes, roots) {
			continue
		}
		reasons = append(reasons, workingLine(tab, callerTab))
	}
	return append(reasons, missingPaneTabs(in, byID, ignoredPanes)...)
}

func tabCounts(tab herdr.Tab, panes []herdr.Pane, caller string, ignored map[string]struct{}, roots []string) bool {
	var working, counted []herdr.Pane
	for _, pane := range panes {
		if pane.TabID != tab.ID || pane.AgentStatus != herdr.AgentStatusWorking {
			continue
		}
		working = append(working, pane)
		if paneExcluded(pane.ID, caller, ignored) {
			continue
		}
		counted = append(counted, pane)
	}
	if len(working) > 0 {
		inside := false
		for _, pane := range counted {
			if paneInside(pane, roots) {
				inside = true
				continue
			}
			logOutside(tab, pane)
		}
		return inside
	}
	if tab.AgentStatus != herdr.AgentStatusWorking {
		return false
	}
	var inTab []herdr.Pane
	for _, pane := range panes {
		if pane.TabID == tab.ID {
			inTab = append(inTab, pane)
		}
	}
	if len(inTab) == 0 {
		logOutsideTab(tab)
		return false
	}
	inside := false
	for _, pane := range inTab {
		if paneInside(pane, roots) {
			inside = true
		}
	}
	if !inside {
		logOutsideTab(tab)
	}
	return inside
}

func paneInside(pane herdr.Pane, roots []string) bool {
	for _, root := range roots {
		if pathInside(pane.Cwd, root) || pathInside(pane.ForegroundCwd, root) {
			return true
		}
	}
	return false
}

func pathInside(dir, root string) bool {
	if dir == "" || root == "" {
		return false
	}
	dir = filepath.Clean(dir)
	root = filepath.Clean(root)
	if dir == root {
		return true
	}
	rel, err := filepath.Rel(root, dir)
	if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return false
	}
	return rel != ""
}

func logOutside(tab herdr.Tab, pane herdr.Pane) {
	slog.Debug("ignored working agent outside checkout",
		"agent", tab.Label,
		"tab", tab.ID,
		"pane", pane.ID,
		"cwd", pane.Cwd,
		"foreground_cwd", pane.ForegroundCwd,
	)
}

func logOutsideTab(tab herdr.Tab) {
	slog.Debug("ignored working agent outside checkout",
		"agent", tab.Label,
		"tab", tab.ID,
	)
}

func paneExcluded(id, caller string, ignored map[string]struct{}) bool {
	if caller != "" && id == caller {
		return true
	}
	_, ok := ignored[id]
	return ok
}

func tabOfPane(panes []herdr.Pane, id string) string {
	if id == "" {
		return ""
	}
	for _, pane := range panes {
		if pane.ID == id {
			return pane.TabID
		}
	}
	return ""
}

func workingLine(tab herdr.Tab, callerTab string) string {
	line := fmt.Sprintf("agent %s (%s) is working", tab.Label, tab.ID)
	if callerTab != "" && tab.ID == callerTab {
		line += " (this pane)"
	}
	return line
}

func missingPaneTabs(in Inputs, byID map[string]herdr.Tab, ignored map[string]struct{}) []string {
	var reasons []string
	for _, pane := range in.Panes {
		if pane.AgentStatus != herdr.AgentStatusWorking || paneExcluded(pane.ID, in.CallerPaneID, ignored) {
			continue
		}
		if _, ok := byID[pane.TabID]; ok {
			continue
		}
		reasons = append(reasons, fmt.Sprintf("pane %s: expected tab %s, found none", pane.ID, pane.TabID))
	}
	return reasons
}

func stringSet(values []string) map[string]struct{} {
	set := make(map[string]struct{}, len(values))
	for _, value := range values {
		set[value] = struct{}{}
	}
	return set
}

func checkoutReason(ctx context.Context, status func(context.Context, string) (string, error), cwd string) string {
	if ctx == nil {
		return "git status: expected a context, found nil"
	}
	if status == nil {
		return "git status: expected a function, found nil"
	}
	if cwd == "" {
		return "cwd: expected a non-empty path, found empty"
	}
	out, err := status(ctx, cwd)
	if err != nil {
		if err.Error() == "" {
			return "git status: expected an error text, found empty"
		}
		return err.Error()
	}
	n := porcelainFiles(out)
	if n == 0 {
		return ""
	}
	return fmt.Sprintf("%s has uncommitted changes (%d files)", cwd, n)
}

func porcelainFiles(out string) int {
	n := 0
	for _, line := range strings.Split(out, "\n") {
		if strings.TrimSpace(line) != "" {
			n++
		}
	}
	return n
}

func checkoutReasons(ctx context.Context, in Inputs, cwd string) []string {
	var reasons []string
	if reason := checkoutReason(ctx, in.GitStatus, cwd); reason != "" {
		reasons = append(reasons, reason)
	}
	if reason := branchReason(ctx, in, cwd); reason != "" {
		reasons = append(reasons, reason)
	}
	return reasons
}

func branchReason(ctx context.Context, in Inputs, cwd string) string {
	if in.BaseBranch == "" {
		return ""
	}
	if reason := requireValue(in.Require); reason != "" {
		return reason
	}
	if ctx == nil {
		return "git rev-parse: expected a context, found nil"
	}
	if cwd == "" {
		return ""
	}
	branch, reason := currentBranch(ctx, in, cwd)
	if reason != "" {
		return reason
	}
	shown := branch
	if branch == "HEAD" {
		shown = "detached HEAD"
	}
	if in.Require == config.GateOnBase {
		if branch == in.BaseBranch {
			return ""
		}
		return fmt.Sprintf("%s: expected branch %s, found %s", cwd, in.BaseBranch, shown)
	}
	return aheadReason(ctx, in, cwd, shown)
}

func requireValue(require string) string {
	switch require {
	case config.GateOnBase, config.GateCleanAgainstBase:
		return ""
	default:
		return config.InvalidGateRequire(require).Error()
	}
}

func currentBranch(ctx context.Context, in Inputs, cwd string) (string, string) {
	if in.GitBranch == nil {
		return "", "git rev-parse: expected a function, found nil"
	}
	branch, err := in.GitBranch(ctx, cwd)
	if err != nil {
		if err.Error() == "" {
			return "", "git rev-parse: expected an error text, found empty"
		}
		return "", err.Error()
	}
	if branch == "" {
		return "", "git rev-parse: expected a branch name, found empty"
	}
	return branch, ""
}

func aheadReason(ctx context.Context, in Inputs, cwd, shown string) string {
	if in.GitAhead == nil {
		return "git rev-list: expected a function, found nil"
	}
	n, err := in.GitAhead(ctx, cwd, in.BaseBranch)
	if err != nil {
		if err.Error() == "" {
			return "git rev-list: expected an error text, found empty"
		}
		return err.Error()
	}
	if n == 0 {
		return ""
	}
	if n < 0 {
		return fmt.Sprintf("git rev-list: expected a non-negative count, found %d", n)
	}
	return fmt.Sprintf("%s: expected no commits beyond origin/%s, found %d on %s", cwd, in.BaseBranch, n, shown)
}
