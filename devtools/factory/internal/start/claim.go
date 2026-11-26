package start

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// agentUsed is the agent rule and model a claim records.
type agentUsed struct {
	rule  string
	model string
}

func chosenAgent(id string, choice config.AgentArgChoice) (agentUsed, error) {
	rule, err := choice.RuleName()
	if err != nil {
		return agentUsed{}, fmt.Errorf("start %s: %w", id, err)
	}
	model, err := config.ModelArg(choice.Args)
	if err != nil {
		return agentUsed{}, fmt.Errorf("start %s: %w", id, err)
	}
	return agentUsed{rule: rule, model: model}, nil
}

type claimResult struct {
	claimedAt      time.Time
	changedTracker bool
}

// claim records the agent rule and model from choice on the claim row.
// A bad --model in choice fails before anything is read or claimed.
func (s *Starter) claim(ctx context.Context, id string, snap ready.Snapshot, opts Options, ownClaim bool, choice config.AgentArgChoice) (claimResult, Outcome, error) {
	used, err := chosenAgent(id, choice)
	if err != nil {
		return claimResult{}, refusedOutcome(id, err), err
	}
	live, err := s.Src.Get(ctx, id)
	if err != nil {
		return claimResult{}, refusedOutcome(id, err), err
	}
	if live.ID != id {
		err := fmt.Errorf("get %s: expected id %q, found %q", id, id, live.ID)
		return claimResult{}, refusedOutcome(id, err), err
	}
	if err := s.recheck(ctx, id, live, snap, opts, ownClaim); err != nil {
		return claimResult{}, refusedOutcome(id, err), err
	}
	if err := s.applyBackendClaim(ctx, id, live, snap.Me, opts); err != nil {
		return claimResult{}, refusedOutcome(id, err), err
	}
	confirmed, err := s.Src.Get(ctx, id)
	if err != nil {
		cerr := claimed(id, fmt.Errorf("read claim %s: %w", id, err))
		return claimResult{}, claimKeptOutcome(id, cerr), cerr
	}
	if err := s.confirm(confirmed, snap.Me); err != nil {
		return claimResult{}, refusedOutcome(id, err), err
	}
	changed := trackerChanged(s.Cfg, s.Kind, live, confirmed)
	now := s.Now()
	stashed, err := s.stashCount(ctx, id)
	if err != nil {
		return claimResult{}, claimKeptOutcome(id, err), err
	}
	row := store.Claim{
		Source:     s.Src.Name(),
		ID:         id,
		ClaimedAt:  now,
		Machine:    s.Hostname,
		State:      store.Claimed,
		UpdatedAt:  now,
		StashCount: stashed,
		Lane:       s.Lane,
		AgentRule:  used.rule,
		Model:      used.model,
		Spawner:    s.effectiveSpawner(),
		OutputMode: s.effectiveOutputMode(),
	}
	if err := s.Store.InsertClaim(ctx, row); err != nil {
		if errors.Is(err, store.ErrActiveClaim) {
			return claimResult{}, refusedOutcome(id, err), err
		}
		cerr := claimed(id, fmt.Errorf("insert claim: %w", err))
		return claimResult{}, claimKeptOutcome(id, cerr), cerr
	}
	claim := s.effectiveClaim()
	if claim.Comment && !ownClaim {
		body := renderClaimComment(claim, s.effectiveAgent(), now.UTC().Format(time.RFC3339), id, s.sourceKey())
		if err := s.Src.PostClaimComment(ctx, id, body); err != nil {
			ferr := s.fail(ctx, row, 2, fmt.Errorf("post claim comment: %w", err))
			return claimResult{}, claimKeptOutcome(id, ferr), ferr
		}
	}
	return claimResult{claimedAt: now, changedTracker: changed}, Outcome{}, nil
}

func (s *Starter) applyBackendClaim(ctx context.Context, id string, live source.Item, me string, opts Options) error {
	if s.Cfg.Claim == (config.Claim{}) && !opts.ForceTakeOwnership {
		return s.Src.Claim(ctx, id) //nolint:wrapcheck // propagate context cancellation
	}
	claim := s.effectiveClaim()
	assign := claim.Assign
	markInProgress := claim.MarkInProgress
	createLabel := claim.CreateLabel
	claimOpts := source.ClaimOptions{
		Assign:         &assign,
		MarkInProgress: &markInProgress,
		CreateLabel:    &createLabel,
	}
	if opts.ForceTakeOwnership && live.Assignee != "" && !strings.EqualFold(live.Assignee, me) {
		claimOpts.RemoveAssignees = []string{live.Assignee}
	}
	return s.Src.ClaimWithOptions(ctx, id, claimOpts) //nolint:wrapcheck // propagate context cancellation
}

func trackerChanged(cfg config.Source, kind string, before, after source.Item) bool {
	if before.State != after.State || before.Assignee != after.Assignee {
		return true
	}
	claim := effectiveClaimFor(cfg, kind)
	if claim.Label != "" {
		return !slices.Contains(before.Labels, claim.Label) && slices.Contains(after.Labels, claim.Label)
	}
	return false
}

func claimKeptOutcome(id string, err error) Outcome {
	return Outcome{
		ID:       id,
		Status:   OutcomeFailedClaimKept,
		Reason:   err.Error(),
		NextStep: fmt.Sprintf("fix and re-run with --force, or factory release %s", id),
		ExitCode: ExitClaimKept,
	}
}

// stashCount reads the checkout's stash length. A remote lane's checkout is
// on another machine, so nothing is recorded and verify skips that check.
func (s *Starter) stashCount(ctx context.Context, id string) (*int, error) {
	if s.Machine != "" {
		return nil, nil
	}
	if s.Stash == nil {
		return nil, claimed(id, errors.New("start: expected a stash count function, found nil"))
	}
	stashed, err := s.Stash(ctx, s.Cfg.Cwd)
	if err != nil {
		return nil, claimed(id, fmt.Errorf("stash count: %w", err))
	}
	return &stashed, nil
}

// recheck applies the live ready/safety rules after Get.
// --force skips queue checks but still enforces done, require_leaf,
// require_unblocked, ownership_rule, and in_progress_rule.
func (s *Starter) recheck(ctx context.Context, id string, live source.Item, snap ready.Snapshot, opts Options, ownClaim bool) error {
	liveSnap := withLiveItem(snap, live)
	if opts.Force {
		if err := checkSafety(id, live, liveSnap, s.Cfg, s.Kind, opts, ownClaim); err != nil {
			return err
		}
	} else {
		part, err := readyFailureForStart(live, liveSnap, s.Cfg, s.Kind, opts, ownClaim)
		if err != nil {
			return err
		}
		if part != "" {
			return fmt.Errorf("start %s: %s", id, part)
		}
	}
	claim := s.effectiveClaim()
	if !opts.ForceTakeOwnership && !ownClaim && claim.Nickname != "" && isInProgress(live, claim, s.Kind) {
		if err := checkNicknameComment(ctx, s.Src, id, claim); err != nil {
			return err
		}
	}
	return nil
}

func withLiveItem(snap ready.Snapshot, it source.Item) ready.Snapshot {
	items := make(map[string]source.Item, len(snap.Items)+1)
	for id, item := range snap.Items {
		items[id] = item
	}
	items[it.ID] = it
	snap.Items = items
	return snap
}

func (s *Starter) confirm(it source.Item, me string) error {
	claim := s.effectiveClaim()
	if claim.Assign && it.Assignee != me {
		return fmt.Errorf("claim %s: expected assignee %q, found %q: %w", it.ID, me, it.Assignee, ErrLostRace)
	}
	if !claim.MarkInProgress {
		return nil
	}
	switch s.Kind {
	case "jira", "github":
		if claim.Status != "" && it.State != claim.Status {
			return fmt.Errorf("claim %s: expected status %q, found %q: %w", it.ID, claim.Status, it.State, ErrLostRace)
		}
		if claim.Label != "" && !slices.Contains(it.Labels, claim.Label) {
			return fmt.Errorf("claim %s: expected label %q, found labels %q: %w", it.ID, claim.Label, it.Labels, ErrLostRace)
		}
		return nil
	default:
		return fmt.Errorf("start: expected kind jira or github, found %q", s.Kind)
	}
}

func claimed(id string, err error) error {
	return fmt.Errorf("%w; item is claimed; fix and re-run with --force, or factory release %s", err, id)
}
