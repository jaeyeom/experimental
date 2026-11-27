package tick

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"log/slog"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/rank"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

type candidate struct {
	key   string
	cfg   config.Source
	item  source.Item
	snap  ready.Snapshot
	score rank.Score
}

func (e *Engine) pick(ctx context.Context, at time.Time, ranked []candidate, panes []herdr.Pane, open []gh.PR) error {
	if len(ranked) == 0 {
		return e.finish(ctx, at, outcomeIdle, "no ready items")
	}
	res, err := e.gate(ctx, at, ranked[0], panes, open)
	if err != nil {
		return err
	}
	if !res.OK {
		if !e.Apply {
			if err := e.writeBlocked(ranked[0]); err != nil {
				return err
			}
		}
		return e.finish(ctx, at, outcomeGated, "gate: "+joinParts(res.Reasons))
	}
	return e.startRanked(ctx, at, ranked, res)
}

func (e *Engine) gate(ctx context.Context, at time.Time, first candidate, panes []herdr.Pane, open []gh.PR) (gate.Result, error) {
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return gate.Result{}, err //nolint:wrapcheck // propagate context cancellation
	}
	started, err := e.Store.LastStartedAt(ctx)
	if err != nil {
		return gate.Result{}, err //nolint:wrapcheck // propagate context cancellation
	}
	checkouts, err := e.Config.SourceCheckouts()
	if err != nil {
		return gate.Result{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	res := gate.Check(ctx, gate.Inputs{
		Tabs:           first.snap.Tabs,
		Panes:          panes,
		CallerPaneID:   e.Config.CallerPaneID,
		IgnorePanes:    e.Config.Gate.IgnorePanes,
		IgnoreTabs:     e.Config.Gate.IgnoreTabs,
		Checkouts:      checkouts,
		HoldFiles:      e.Config.Gate.ActiveHoldFiles(),
		HoldTabs:       e.Config.Gate.HoldTabs,
		HoldTTL:        e.Config.Gate.HoldTTL,
		GitStatus:      e.Git,
		BaseBranch:     e.Config.Gate.BaseBranch,
		Require:        e.Config.Gate.Require,
		GitBranch:      e.GitBranch,
		GitAhead:       e.GitAhead,
		ActiveClaims:   claims,
		Pulls:          open,
		PRTitlePattern: e.Config.PRTitlePattern,
		LastStartedAt:  started,
		Now:            at,
		// A tick can fall through to a later candidate after a lost claim
		// race, so no one item owns the lease here: only a free lease is usable.
		LeaseGate:  e.Config.Lease.Gate,
		Lease:      e.Lease,
		MaxWorkers: first.cfg.Worker.MaxWorkers,
		Worktree:   first.cfg.Worker.Worktree,
	}, first.cfg.Cwd, e.Config.Defaults)
	for _, note := range res.Notes {
		slog.Info("gate note", "note", note)
	}
	switch {
	case res.OK && len(res.Reasons) == 0:
		return res, nil
	case !res.OK && len(res.Reasons) > 0:
		return res, nil
	default:
		return gate.Result{}, fmt.Errorf("gate: expected OK or reasons, found ok=%t reasons=%d", res.OK, len(res.Reasons))
	}
}

func (e *Engine) startRanked(ctx context.Context, at time.Time, ranked []candidate, res gate.Result) error {
	maxWorkers := ranked[0].cfg.Worker.MaxWorkers
	if maxWorkers <= 1 || !e.Apply {
		return e.startUpTo(ctx, at, ranked, 1)
	}
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	limit := min(maxWorkers-len(claims), res.MaxInFlight-res.InFlight)
	if limit < 1 {
		limit = 1
	}
	return e.startUpTo(ctx, at, ranked, limit)
}

func (e *Engine) startUpTo(ctx context.Context, at time.Time, ranked []candidate, limit int) error {
	lost := make([]string, 0, maxStartAttempts)
	attempts := 0
	started := 0
	for _, cand := range ranked {
		if started >= limit || attempts == maxStartAttempts+limit-1 {
			break
		}
		attempts++
		err := e.startOne(ctx, cand, gate.Result{OK: true})
		if err == nil {
			if err := e.afterStart(ctx, at, cand); err != nil {
				return err
			}
			started++
			continue
		}
		if errors.Is(err, start.ErrLostRace) {
			slog.Info("lost the claim race", "id", cand.item.ID)
			lost = append(lost, cand.item.ID)
			continue
		}
		return fmt.Errorf("tick: %w", err)
	}
	if started > 0 {
		return nil
	}
	if len(lost) == 0 {
		return errors.New("tick: expected a candidate to start, found none")
	}
	return e.finish(ctx, at, outcomeLost, "lost the claim race for "+strings.Join(lost, ", "))
}

func (e *Engine) writeBlocked(cand candidate) error {
	starter := &start.Starter{
		Cfg:       cand.cfg,
		Kind:      cand.cfg.Kind,
		Defaults:  e.Config.Defaults,
		Rules:     e.Config.AgentRules,
		Store:     e.Store,
		Out:       e.Out,
		Caller:    e.Caller,
		CallerBus: e.CallerBus,
	}
	return starter.WriteBlocked(cand.item, start.Options{}) //nolint:wrapcheck // internal factory error already carries context
}

func (e *Engine) startOne(ctx context.Context, cand candidate, res gate.Result) error {
	if e.Agent == nil && cand.cfg.Worker.Spawner != config.SpawnerSubagent {
		return errors.New("tick: expected a herdr agent client, found nil")
	}
	src, err := e.backend(cand.key)
	if err != nil {
		return err
	}
	starter := &start.Starter{
		Src:            src,
		Cfg:            cand.cfg,
		Kind:           cand.cfg.Kind,
		Defaults:       e.Config.Defaults,
		Rules:          e.Config.AgentRules,
		Store:          e.Store,
		Herdr:          e.Agent,
		Hostname:       e.Hostname,
		Now:            e.Now,
		Out:            e.Out,
		Caller:         e.Caller,
		Stash:          e.Stash,
		LeaseStatus:    e.Lease,
		Wake:           e.Wake,
		CallerBus:      e.CallerBus,
		Sleep:          e.Sleep,
		WorktreeAdd:    e.WorktreeAdd,
		WorktreeRemove: e.WorktreeRemove,
	}
	return starter.Start(ctx, cand.item.ID, cand.snap, res, start.Options{Apply: e.Apply}) //nolint:wrapcheck // propagate context cancellation
}

func (e *Engine) afterStart(ctx context.Context, at time.Time, cand candidate) error {
	return e.afterStartOn(ctx, at, cand, "")
}

// afterStartOn writes and records the start line. A non-empty lane is
// named at the end of the line.
func (e *Engine) afterStartOn(ctx context.Context, at time.Time, cand candidate, lane string) error {
	var line string
	outcome := outcomeDryRun
	if cand.cfg.Worker.Spawner == config.SpawnerSubagent {
		line = fmt.Sprintf("dry-run %s (subagent)", cand.item.ID)
		if e.Apply {
			line = fmt.Sprintf("started %s (subagent)", cand.item.ID)
			outcome = outcomeStarted
		}
	} else {
		ws, tab := ready.TabRef(cand.item, cand.cfg, cand.cfg.Kind)
		if ws == "" || tab == "" {
			return fmt.Errorf("tick: expected a workspace and a tab for %s, found workspace %q tab %q", cand.item.ID, ws, tab)
		}
		line = fmt.Sprintf("dry-run %s in %s/%s", cand.item.ID, ws, tab)
		if e.Apply {
			line = fmt.Sprintf("started %s in %s/%s", cand.item.ID, ws, tab)
			outcome = outcomeStarted
		}
	}
	if lane != "" {
		line += " (lane " + lane + ")"
	}
	return e.finish(ctx, at, outcome, line)
}

func (e *Engine) ready(ctx context.Context, open []gh.PR, tabs []herdr.Tab, workspaces []herdr.Workspace, closed map[string]struct{}) ([]candidate, error) {
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	rows := make([]candidate, 0)
	for i := range e.Config.Sources {
		if e.Only != "" {
			key, err := sourceKey(e.Config.Sources[i].Kind, e.Config.Sources[i].Name)
			if err != nil {
				return nil, err
			}
			if key != e.Only {
				continue
			}
		}
		part, err := e.readySource(ctx, e.Config.Sources[i], claims, open, tabs, workspaces, closed)
		if err != nil {
			return nil, err
		}
		rows = append(rows, part...)
	}
	order, err := rank.CommonOrder(candidateOrders(rows))
	if err != nil {
		return nil, err //nolint:wrapcheck // internal factory error already carries context
	}
	slices.SortStableFunc(rows, func(a, b candidate) int {
		return compareCandidates(order, a, b)
	})
	return rows, nil
}

func candidateOrders(rows []candidate) []rank.NamedOrder {
	out := make([]rank.NamedOrder, len(rows))
	for i, row := range rows {
		out[i] = rank.NamedOrder{Name: row.key, Order: row.cfg.RankOrder}
	}
	return out
}

func (e *Engine) readySource(ctx context.Context, cfg config.Source, claims []store.Claim, open []gh.PR, tabs []herdr.Tab, workspaces []herdr.Workspace, closed map[string]struct{}) ([]candidate, error) {
	key, err := sourceKey(cfg.Kind, cfg.Name)
	if err != nil {
		return nil, err
	}
	if err := checkKind(key, cfg.Kind); err != nil {
		return nil, err
	}
	me, err := e.Store.Me(ctx, key)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	if me == "" {
		return nil, fmt.Errorf("%s: run factory sync first", key)
	}
	items, err := e.Store.Items(ctx, key)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	byID := make(map[string]source.Item, len(items))
	for _, it := range items {
		byID[it.ID] = it
	}
	states, err := claimsByID(claims, key)
	if err != nil {
		return nil, err
	}
	external, err := e.Store.ExternalBlockers(ctx, key)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	snap := ready.Snapshot{
		External:   external,
		Items:      byID,
		Claims:     states,
		Tabs:       tabs,
		Workspaces: workspaces,
		OpenPRs:    open,
		Me:         me,
		Now:        e.Now(),
	}
	ignore, err := config.IgnoreAncestorBlocks(cfg.ParentBlocksChild)
	if err != nil {
		return nil, fmt.Errorf("source %q field \"parent_blocks_child\": %w", cfg.Name, err)
	}
	ranked, err := rank.Rank(filterReady(items, snap, cfg, key, closed), byID, cfg.ParentPriority, cfg.RankOrder, ignore)
	if err != nil {
		return nil, err //nolint:wrapcheck // internal factory error already carries context
	}
	out := make([]candidate, len(ranked))
	for i, row := range ranked {
		out[i] = candidate{key: key, cfg: cfg, item: row.Item, snap: snap, score: row.Score}
	}
	return out, nil
}

func checkKind(key, kind string) error {
	if kind == "jira" || kind == "github" {
		return nil
	}
	return fmt.Errorf("source %s: expected kind jira or github, found %q", key, kind)
}

func filterReady(items []source.Item, snap ready.Snapshot, cfg config.Source, sourceName string, closed map[string]struct{}) []source.Item {
	out := make([]source.Item, 0, len(items))
	for _, it := range items {
		if _, ok := closed[closedKey(sourceName, it.ID)]; ok {
			continue
		}
		if ready.Evaluate(it, snap, cfg, cfg.Kind).Ready {
			out = append(out, it)
		}
	}
	return out
}

func claimsByID(claims []store.Claim, sourceName string) (map[string]store.ClaimState, error) {
	out := make(map[string]store.ClaimState)
	for _, claim := range claims {
		if claim.Source != sourceName {
			continue
		}
		if _, ok := out[claim.ID]; ok {
			return nil, fmt.Errorf("active claims for %s/%s: expected one, found more than one", sourceName, claim.ID)
		}
		out[claim.ID] = claim.State
	}
	return out, nil
}

// compareCandidates orders like rank.Rank, then by source key.
// order is the shared rank_order. ID ascending stays inside rank.Order.Compare.
func compareCandidates(order rank.Order, a, b candidate) int {
	if c := order.Compare(rank.Ranked{Item: a.item, Score: a.score}, rank.Ranked{Item: b.item, Score: b.score}); c != 0 {
		return c
	}
	return cmp.Compare(a.key, b.key)
}
