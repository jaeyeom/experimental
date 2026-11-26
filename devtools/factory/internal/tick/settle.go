package tick

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/prlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
	"github.com/jaeyeom/experimental/devtools/factory/internal/verify"
)

// Settle syncs, finishes merged work, and stops. It does not start an agent.
// An empty ids list uses the same merged-pull-request window as tick and every
// active claim. Ids search merged pull requests by the item key in the title,
// with no merged-after date. An id with no merged pull request is an error.
func (e *Engine) Settle(ctx context.Context, ids []string) error {
	if err := e.validate(ctx); err != nil {
		return err
	}
	at := e.Now()
	if at.IsZero() {
		return errors.New("settle: expected a clock time, found zero")
	}
	if err := e.sync(ctx, at); err != nil {
		return err
	}
	_, err := e.settle(ctx, at, ids)
	return err
}

type settleTarget struct {
	it     source.Item
	cfg    config.Source
	claims []store.Claim
}

type tabSnap struct {
	tabs       []herdr.Tab
	workspaces []herdr.Workspace
	panes      []herdr.Pane
	panesReady bool
}

// machineSnaps holds one tab snapshot per herdr machine, keyed by machine
// name. The empty key is the local server. A remote one is listed on first use.
type machineSnaps map[string]tabSnap

func (e *Engine) settle(ctx context.Context, now time.Time, ids []string) (map[string]struct{}, error) {
	if err := distinctIDs(ids); err != nil {
		return nil, err
	}
	if len(ids) == 0 {
		prs, err := e.settlePRs(ctx, now)
		if err != nil {
			return nil, err
		}
		return e.settleActive(ctx, prs)
	}
	return e.settleIDs(ctx, now, ids)
}

func distinctIDs(ids []string) error {
	seen := make(map[string]struct{}, len(ids))
	for _, id := range ids {
		if id == "" {
			return errors.New("settle: expected a non-empty id, found empty")
		}
		if _, ok := seen[id]; ok {
			return fmt.Errorf("settle: expected distinct ids, found %s repeated", id)
		}
		seen[id] = struct{}{}
	}
	return nil
}

func (e *Engine) settlePRs(ctx context.Context, now time.Time) ([]gh.PR, error) {
	since, err := e.mergeSince(ctx, now)
	if err != nil {
		return nil, err
	}
	prs, err := e.Pulls.MergedByMeSince(ctx, since)
	if err != nil {
		return nil, fmt.Errorf("tick: merged pull requests: %w", err)
	}
	// Record before settling: settled claims are no longer active.
	if err := e.observe(ctx, now, prs); err != nil {
		return nil, err
	}
	return prs, nil
}

func (e *Engine) settleActive(ctx context.Context, prs []gh.PR) (map[string]struct{}, error) {
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	order := make([]string, 0, len(claims))
	grouped := map[string]*settleTarget{}
	var release []store.Claim
	for _, claim := range claims {
		matched, cfg, it, err := e.claimSettled(ctx, claim, prs)
		if errors.Is(err, config.ErrSourceNotFound) {
			slog.Debug("skipping active claim for unconfigured source", "source", claim.Source, "id", claim.ID)
			continue
		}
		if err != nil {
			return nil, err
		}
		if !matched {
			if it.Done {
				release = append(release, claim)
			}
			continue
		}
		key := closedKey(claim.Source, claim.ID)
		target, ok := grouped[key]
		if !ok {
			target = &settleTarget{it: it, cfg: cfg}
			grouped[key] = target
			order = append(order, key)
		}
		target.claims = append(target.claims, claim)
	}
	if err := e.releaseDoneClaims(ctx, release); err != nil {
		return nil, err
	}
	return e.finishTargets(ctx, targetsInOrder(grouped, order))
}

// releaseDoneClaims marks active claims released when the item is already done
// and no merged pull request matched. It does not call Source.Release, so the
// tracker item stays done.
func (e *Engine) releaseDoneClaims(ctx context.Context, claims []store.Claim) error {
	for _, claim := range claims {
		if err := e.setClaim(ctx, claim, store.Released); err != nil {
			return err
		}
		if err := e.writeLine("released " + claim.ID); err != nil {
			return err
		}
		if claim.Worktree != "" {
			cfg, err := e.configFor(claim.Source)
			if err != nil {
				return err
			}
			lane, err := e.claimLane(claim.Lane)
			if err != nil {
				return err
			}
			if !lane.Remote() {
				cwd := cfg.Cwd
				if lane.Checkout != "" {
					cwd = lane.Checkout
				}
				e.cleanupOneWorktree(ctx, claim.ID, cwd, claim.Worktree)
			}
		}
	}
	return nil
}

func (e *Engine) claimSettled(ctx context.Context, claim store.Claim, prs []gh.PR) (bool, config.Source, source.Item, error) {
	if claim.OutputMode != config.OutputModeMerge {
		return e.matched(ctx, claim, prs)
	}
	cfg, err := e.configFor(claim.Source)
	if err != nil {
		return false, config.Source{}, source.Item{}, err
	}
	it, err := e.Store.Item(ctx, claim.Source, claim.ID)
	if err != nil {
		return false, config.Source{}, source.Item{}, err //nolint:wrapcheck // propagate context cancellation
	}
	_, reason, err := e.mergeLanded(ctx, claim, cfg, it)
	if err != nil {
		return false, cfg, it, err
	}
	return reason == "", cfg, it, nil
}

func (e *Engine) mergeLanded(ctx context.Context, claim store.Claim, cfg config.Source, it source.Item) (verify.LandedCommit, string, error) {
	lane, err := e.claimLane(claim.Lane)
	if err != nil {
		return verify.LandedCommit{}, "", err
	}
	srcCfg := cfg
	if lane.Name != "" {
		srcCfg = config.LaneSource(cfg, lane)
	}
	dir := srcCfg.Cwd
	if claim.State.Active() && claim.Worktree != "" {
		dir = claim.Worktree
	}
	checker, err := e.mergeCheckerFor(lane)
	if err != nil {
		return verify.LandedCommit{}, "", err
	}
	return checker(ctx, dir, srcCfg.Base, it.ID, srcCfg.PRTitlePattern)
}

func (e *Engine) mergeCheckerFor(lane config.Lane) (MergeChecker, error) {
	if !lane.Remote() {
		if e.GitMerge == nil {
			return nil, errors.New("settle: expected a git merge checker, found nil")
		}
		return e.GitMerge, nil
	}
	if e.RemoteGitMerge == nil {
		return nil, fmt.Errorf("settle: lane %s: expected a remote git merge checker, found nil", lane.Name)
	}
	return e.RemoteGitMerge(lane)
}

func (e *Engine) settleIDs(ctx context.Context, now time.Time, ids []string) (map[string]struct{}, error) {
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	targets := make([]settleTarget, 0, len(ids))
	for _, id := range ids {
		it, err := e.oneItem(ctx, id)
		if err != nil {
			return nil, err
		}
		cfg, err := e.configFor(it.Source)
		if err != nil {
			return nil, err
		}
		var itemClaims []store.Claim
		for _, claim := range claims {
			if claim.Source == it.Source && claim.ID == it.ID {
				itemClaims = append(itemClaims, claim)
			}
		}
		var firstClaim store.Claim
		if len(itemClaims) > 0 {
			firstClaim = itemClaims[0]
		}
		if verify.EffectiveOutputMode(len(itemClaims) > 0, firstClaim, cfg) == config.OutputModeMerge {
			_, reason, err := e.mergeLanded(ctx, firstClaim, cfg, it)
			if err != nil {
				return nil, fmt.Errorf("settle %s: %w", id, err)
			}
			if reason != "" {
				return nil, fmt.Errorf("settle %s: %s", id, reason)
			}
		} else {
			found, err := e.mergedByTitle(ctx, it, cfg.PRTitlePattern)
			if err != nil {
				return nil, err
			}
			if len(found) == 0 {
				return nil, fmt.Errorf("settle %s: expected a merged pull request, found none", id)
			}
			if err := prlog.Record(ctx, e.Store, now, it, found); err != nil {
				return nil, fmt.Errorf("settle %s: %w", id, err)
			}
		}
		targets = append(targets, settleTarget{it: it, cfg: cfg, claims: itemClaims})
	}
	return e.finishTargets(ctx, targets)
}

func (e *Engine) mergedByTitle(ctx context.Context, it source.Item, pattern string) ([]gh.PR, error) {
	prs, err := e.Pulls.MergedByTitle(ctx, it.ID)
	if err != nil {
		return nil, fmt.Errorf("settle %s: merged pull requests: %w", it.ID, err)
	}
	return prsForItem(prs, it, pattern)
}

func targetsInOrder(grouped map[string]*settleTarget, order []string) []settleTarget {
	out := make([]settleTarget, 0, len(order))
	for _, key := range order {
		out = append(out, *grouped[key])
	}
	return out
}

func (e *Engine) finishTargets(ctx context.Context, targets []settleTarget) (map[string]struct{}, error) {
	closed := map[string]struct{}{}
	if len(targets) == 0 {
		return closed, nil
	}
	snaps := machineSnaps{}
	// Closed claims are not start candidates in this tick. Finish updates the
	// source, not the mirror, so the item would still look ready.
	doneNow := map[string]struct{}{}
	for _, target := range targets {
		if err := e.settleOne(ctx, target, snaps, doneNow); err != nil {
			return nil, err
		}
		closed[closedKey(target.it.Source, target.it.ID)] = struct{}{}
	}
	return closed, nil
}

func (e *Engine) settleOne(ctx context.Context, target settleTarget, snaps machineSnaps, doneNow map[string]struct{}) error {
	newly, err := e.finishIfOpen(ctx, target.it, target.cfg)
	if err != nil {
		return err
	}
	for _, claim := range target.claims {
		if err := e.setClaim(ctx, claim, store.Done); err != nil {
			return err
		}
	}
	lane, err := e.settleLane(target.claims)
	if err != nil {
		return err
	}
	e.cleanupTargetWorktree(ctx, target, lane)
	if lane.Name != "" {
		if err := e.recordSettle(ctx, target.it.ID, lane.Name); err != nil {
			return err
		}
	}
	if newly {
		doneNow[target.it.ID] = struct{}{}
		if err := e.printUnblocked(ctx, target.it, doneNow); err != nil {
			return err
		}
		e.refreshFinished(ctx, target.it)
	}
	if !subagentOnly(target.claims) {
		if err := e.closeOnLane(ctx, target, lane, snaps); err != nil {
			return err
		}
	}
	return nil
}

// refreshFinished rereads a newly finished item so the mirror shows it done
// before the next sync. An item that left the source query, such as one whose
// query excludes done work, loses its mirror row. It runs after
// printUnblocked, which reads the item's links from the mirror.
// A failure is logged like a failed Finish: the claim is already done, and
// the next sync repairs the row.
func (e *Engine) refreshFinished(ctx context.Context, it source.Item) {
	src, err := e.backend(it.Source)
	if err == nil {
		_, err = syncer.Refresh(ctx, e.Store, it.Source, src, it.ID)
	}
	if err != nil {
		slog.Warn("refresh after finish failed", "source", it.Source, "id", it.ID, "err", err)
	}
}

func (e *Engine) finishIfOpen(ctx context.Context, it source.Item, cfg config.Source) (bool, error) {
	if !cfg.FinishOnMerge || it.Done {
		return false, nil
	}
	src, err := e.backend(it.Source)
	if err != nil {
		return false, err
	}
	if err := src.Finish(ctx, it.ID); err != nil {
		slog.Warn("finish on merge failed", "source", it.Source, "id", it.ID, "err", err)
		return false, nil
	}
	return true, nil
}

func subagentOnly(claims []store.Claim) bool {
	if len(claims) == 0 {
		return false
	}
	for _, claim := range claims {
		if claim.Spawner != config.SpawnerSubagent {
			return false
		}
	}
	return true
}

func oneTabID(claims []store.Claim) (string, error) {
	var ids []string
	for _, claim := range claims {
		if claim.TabID == "" || slices.Contains(ids, claim.TabID) {
			continue
		}
		ids = append(ids, claim.TabID)
	}
	switch len(ids) {
	case 0:
		return "", nil
	case 1:
		return ids[0], nil
	default:
		return "", fmt.Errorf("settle %s/%s: expected one tab id, found %s", claims[0].Source, claims[0].ID, strings.Join(ids, ", "))
	}
}

func (e *Engine) cleanupTargetWorktree(ctx context.Context, target settleTarget, lane config.Lane) {
	if lane.Remote() {
		return
	}
	cwd := target.cfg.Cwd
	if lane.Checkout != "" {
		cwd = lane.Checkout
	}
	dirs := worktreeDirsForClaims(target.claims)
	if len(dirs) == 0 && target.cfg.Worker.Worktree {
		dir, _ := start.WorktreeSpec(cwd, target.it.ID)
		dirs = []string{dir}
	}
	for _, dir := range dirs {
		e.cleanupOneWorktree(ctx, target.it.ID, cwd, dir)
	}
}

func worktreeDirsForClaims(claims []store.Claim) []string {
	var dirs []string
	for _, claim := range claims {
		if claim.Worktree != "" && !slices.Contains(dirs, claim.Worktree) {
			dirs = append(dirs, claim.Worktree)
		}
	}
	return dirs
}

func (e *Engine) cleanupOneWorktree(ctx context.Context, id, cwd, dir string) {
	if dir == "" || e.WorktreeRemove == nil {
		return
	}
	if e.Git == nil {
		msg := fmt.Sprintf("warning: left worktree %s on disk: git status unavailable", dir)
		slog.Warn("skip worktree removal: git status unavailable", "id", id, "dir", dir)
		if err := e.writeLine(msg); err != nil {
			slog.Warn("write worktree warning failed", "err", err)
		}
		return
	}
	out, err := e.Git(ctx, dir)
	if err != nil {
		msg := fmt.Sprintf("warning: left worktree %s on disk: %v", dir, err)
		slog.Warn("check worktree status before removal failed", "id", id, "dir", dir, "err", err)
		if werr := e.writeLine(msg); werr != nil {
			slog.Warn("write worktree warning failed", "err", werr)
		}
		return
	}
	if strings.TrimSpace(out) != "" {
		msg := fmt.Sprintf("warning: left dirty worktree %s on disk (uncommitted changes)", dir)
		slog.Warn("left dirty worktree on disk", "id", id, "dir", dir)
		if werr := e.writeLine(msg); werr != nil {
			slog.Warn("write worktree warning failed", "err", werr)
		}
		return
	}
	if err := e.WorktreeRemove(ctx, cwd, dir); err != nil {
		msg := fmt.Sprintf("warning: remove worktree %s failed: %v", dir, err)
		slog.Warn("remove worktree after settle failed", "id", id, "dir", dir, "err", err)
		if werr := e.writeLine(msg); werr != nil {
			slog.Warn("write worktree warning failed", "err", werr)
		}
	}
}

// settleLane is the one lane recorded on claims. No claims, or claims made
// without lanes, is the zero lane: the local server.
func (e *Engine) settleLane(claims []store.Claim) (config.Lane, error) {
	var names []string
	for _, claim := range claims {
		if !slices.Contains(names, claim.Lane) {
			names = append(names, claim.Lane)
		}
	}
	switch len(names) {
	case 0:
		return config.Lane{}, nil
	case 1:
		return e.claimLane(names[0])
	default:
		return config.Lane{}, fmt.Errorf("settle %s/%s: expected one lane, found %q", claims[0].Source, claims[0].ID, names)
	}
}

// closeOnLane closes the item's tab on the lane's machine. A remote
// machine that does not answer leaves the tab and says so: the claim is
// already done, and one offline machine must not stop the rest of settle.
func (e *Engine) closeOnLane(ctx context.Context, target settleTarget, lane config.Lane, snaps machineSnaps) error {
	tabID, err := oneTabID(target.claims)
	if err != nil {
		return err
	}
	m, err := e.machineFor(lane)
	if err != nil {
		return err
	}
	snap, ok := snaps[lane.Machine]
	if !ok {
		snap, err = e.tabSnap(ctx, m.Sessions)
		if herdr.Unreachable(err) {
			return e.writeLine(fmt.Sprintf("lane %s: tab for %s not checked: %s", lane.Name, target.it.ID, unreachableWhy(ctx, m, err)))
		}
		if err != nil {
			return fmt.Errorf("lane %s: %w", lane.Name, err)
		}
		snaps[lane.Machine] = snap
	}
	tab, found, err := snap.find(target.it, target.cfg, tabID)
	if err != nil {
		return err
	}
	mode := targetOutputMode(target, lane)
	selfSettle := false
	if mode == config.OutputModeMerge && found && tab.AgentStatus == herdr.AgentStatusWorking && !lane.Remote() {
		if caller := e.callerPaneID(); caller != "" {
			if !snap.panesReady {
				panes, err := m.Sessions.Panes(ctx)
				if herdr.Unreachable(err) {
					return e.writeLine(fmt.Sprintf("lane %s: tab for %s not checked: %s", lane.Name, target.it.ID, unreachableWhy(ctx, m, err)))
				}
				if err != nil {
					return fmt.Errorf("lane %s: herdr panes: %w", lane.Name, err)
				}
				snap.panes = panes
				snap.panesReady = true
				snaps[lane.Machine] = snap
			}
			selfSettle = paneInTab(snap.panes, caller, tab.ID)
		}
	}
	return e.closeSettledTab(ctx, m, tab, found, lane.Name, mode, selfSettle)
}

func unreachableWhy(ctx context.Context, m LaneMachine, err error) string {
	if m.Why != nil {
		return m.Why(ctx, err)
	}
	return err.Error()
}

func (e *Engine) callerPaneID() string {
	if e.Config == nil {
		return ""
	}
	return e.Config.CallerPaneID
}

func targetOutputMode(target settleTarget, lane config.Lane) string {
	srcCfg := target.cfg
	if lane.Name != "" {
		srcCfg = config.LaneSource(target.cfg, lane)
	}
	var first store.Claim
	if len(target.claims) > 0 {
		first = target.claims[0]
	}
	return verify.EffectiveOutputMode(len(target.claims) > 0, first, srcCfg)
}

func paneInTab(panes []herdr.Pane, paneID, tabID string) bool {
	if paneID == "" || tabID == "" {
		return false
	}
	for _, pane := range panes {
		if pane.ID == paneID && pane.TabID == tabID {
			return true
		}
	}
	return false
}

func waitAgentFor(m LaneMachine) func(context.Context, string, []string, time.Duration) (string, error) {
	if m.WaitAgent != nil {
		return m.WaitAgent
	}
	if m.Agent != nil {
		return m.Agent.WaitAgent
	}
	return nil
}

// recordSettle writes a settle run row that names the lane.
func (e *Engine) recordSettle(ctx context.Context, id, lane string) error {
	at, err := e.clock()
	if err != nil {
		return err
	}
	return e.recordAs(ctx, settleCommand, at, outcomeSettled, fmt.Sprintf("settled %s (lane %s)", id, lane))
}

func (e *Engine) tabSnap(ctx context.Context, sessions Sessions) (tabSnap, error) {
	if sessions == nil {
		return tabSnap{}, errors.New("settle: expected a herdr client, found nil")
	}
	workspaces, err := sessions.Workspaces(ctx)
	if err != nil {
		return tabSnap{}, fmt.Errorf("herdr workspaces: %w", err)
	}
	tabs, err := sessions.Tabs(ctx)
	if err != nil {
		return tabSnap{}, fmt.Errorf("herdr tabs: %w", err)
	}
	return tabSnap{tabs: tabs, workspaces: workspaces}, nil
}

func (s tabSnap) find(it source.Item, cfg config.Source, claimTab string) (herdr.Tab, bool, error) {
	if claimTab != "" {
		for _, tab := range s.tabs {
			if tab.ID == claimTab {
				return tab, true, nil
			}
		}
		return herdr.Tab{}, false, nil
	}
	wsLabel, tabLabel := ready.TabRef(it, cfg, cfg.Kind)
	switch cfg.Kind {
	case "jira":
		return herdr.FindTab(s.tabs, "", tabLabel) //nolint:wrapcheck // internal factory error already carries context
	case "github":
		ws, ok, err := herdr.FindWorkspace(s.workspaces, wsLabel)
		if err != nil || !ok {
			return herdr.Tab{}, false, err //nolint:wrapcheck // internal factory error already carries context
		}
		return herdr.FindTab(s.tabs, ws.ID, tabLabel) //nolint:wrapcheck // internal factory error already carries context
	default:
		return herdr.Tab{}, false, fmt.Errorf("settle: expected kind jira or github, found %q", cfg.Kind)
	}
}

// closeSettledTab closes tab through m.Closer. A non-empty lane is named
// at the end of the tab lines. Under output.mode = "pr", a working tab is left
// open. Under output.mode = "merge", self-settle closes a working tab directly
// and external settle waits briefly for idle/done before closing.
func (e *Engine) closeSettledTab(ctx context.Context, m LaneMachine, tab herdr.Tab, found bool, lane, outputMode string, selfSettle bool) error {
	if !found {
		return nil
	}
	on := ""
	if lane != "" {
		on = " on lane " + lane
	}
	switch tab.AgentStatus {
	case herdr.AgentStatusIdle, herdr.AgentStatusBlocked, herdr.AgentStatusDone, herdr.AgentStatusUnknown:
	case herdr.AgentStatusWorking:
		if outputMode != config.OutputModeMerge {
			return e.writeLine(fmt.Sprintf("left tab %s (%s) open%s: agent is working", tab.Label, tab.ID, on))
		}
		if !selfSettle {
			proceed, err := e.waitWorkingMergeTab(ctx, m, tab, on)
			if err != nil || !proceed {
				return err
			}
		}
	default:
		return fmt.Errorf("settle: tab %s: expected agent_status one of idle, working, blocked, done, unknown, found %q", tab.ID, tab.AgentStatus)
	}
	if m.Closer == nil {
		return errors.New("settle: expected a tab closer, found nil")
	}
	result, err := m.Closer.CloseTab(ctx, tab.ID)
	if err != nil {
		if errors.Is(err, herdr.ErrTabNotFound) {
			return nil
		}
		return fmt.Errorf("settle: close tab %s: %w", tab.ID, err)
	}
	if result == "" {
		return fmt.Errorf("settle: close tab %s: expected a result type, found empty", tab.ID)
	}
	if err := e.writeLine(result); err != nil {
		return err
	}
	return e.writeLine(fmt.Sprintf("closed tab %s (%s)%s", tab.Label, tab.ID, on))
}

// waitWorkingMergeTab waits briefly for a working merge-mode worker tab to
// finish its turn before closing. It returns (true, nil) when the tab should be
// closed, or (false, nil) when it was already closed or left open with a note.
func (e *Engine) waitWorkingMergeTab(ctx context.Context, m LaneMachine, tab herdr.Tab, on string) (bool, error) {
	wait := waitAgentFor(m)
	if wait == nil {
		return false, e.writeLine(fmt.Sprintf("left tab %s (%s) open%s: agent is working", tab.Label, tab.ID, on))
	}
	if _, err := wait(ctx, tab.ID, []string{herdr.AgentStatusIdle, herdr.AgentStatusDone}, mergeSettleWaitTimeout); err != nil {
		if errors.Is(err, herdr.ErrTabNotFound) {
			return false, nil
		}
		slog.Warn("wait for merge-mode agent before tab close failed", "tab", tab.ID, "err", err)
		return false, e.writeLine(fmt.Sprintf("left tab %s (%s) open%s: agent is working (%v)", tab.Label, tab.ID, on, err))
	}
	return true, nil
}

func (e *Engine) printUnblocked(ctx context.Context, it source.Item, doneNow map[string]struct{}) error {
	deps, err := e.Store.Blocks(ctx, it.Source, it.ID)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	items, err := e.Store.Items(ctx, it.Source)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	byID := make(map[string]source.Item, len(items))
	for _, item := range items {
		byID[item.ID] = item
	}
	var lines []string
	for _, dep := range deps {
		other, ok := byID[dep]
		if !ok {
			return fmt.Errorf("settle %s: expected dependent %s in the mirror, found none", it.ID, dep)
		}
		if blockersDone(other, byID, doneNow) {
			lines = append(lines, dep)
		}
	}
	slices.Sort(lines)
	for _, id := range lines {
		if err := e.writeLine("unblocked " + id); err != nil {
			return err
		}
	}
	return nil
}

func blockersDone(it source.Item, byID map[string]source.Item, doneNow map[string]struct{}) bool {
	if len(it.BlockedBy) == 0 {
		return false
	}
	for _, ref := range it.BlockedBy {
		if _, ok := doneNow[ref.ID]; ok {
			continue
		}
		blocker, ok := byID[ref.ID]
		if !ok || !blocker.Done {
			return false
		}
	}
	return true
}

func (e *Engine) oneItem(ctx context.Context, id string) (source.Item, error) {
	items, err := e.Store.FindItem(ctx, id)
	if err != nil {
		return source.Item{}, err //nolint:wrapcheck // propagate context cancellation
	}
	switch len(items) {
	case 0:
		return source.Item{}, fmt.Errorf("settle %s: expected one item, found none", id)
	case 1:
		return items[0], nil
	default:
		names := make([]string, len(items))
		for i, it := range items {
			names[i] = it.Source
		}
		return source.Item{}, fmt.Errorf("settle %s: expected one source, found %s", id, strings.Join(names, ", "))
	}
}
