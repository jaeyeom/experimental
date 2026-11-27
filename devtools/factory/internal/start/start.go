// Package start claims one work item and opens a herdr tab running the agent.
package start

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	agentTimeout  = 60 * time.Second
	promptTimeout = 30 * time.Second
)

// ErrLostRace means the claim did not stick to this account.
// The item must not be released.
var ErrLostRace = errors.New("lost the claim race")

// Starter claims one item and starts an agent in herdr.
type Starter struct {
	Src      source.Source      // Backend Get and Claim call.
	Cfg      config.Source      // Source configuration. Name is the configured name.
	Kind     string             // "jira" or "github".
	Defaults config.Defaults    // Agent kind and default agent args come from here.
	Rules    []config.AgentRule // First matching label rule replaces Defaults.AgentArgs.
	Store    *store.Store       // Local claim and run rows.
	Herdr    HerdrAPI           // Herdr calls. Tests pass a fake.
	Hostname string             // Stored on the claim as the machine.
	Now      func() time.Time   // Clock for the claim and the run row.
	Out      io.Writer          // Plan and warning lines.
	Caller   string             // Substituted for {caller}. Empty expands to empty.
	// CallerBus is the caller's agentbus name or handle, for {caller_bus}
	// and {report_back}. Empty when agentbus is not available.
	CallerBus string
	// Stash counts `git stash list` lines in Cfg.Cwd. Apply records it on the claim.
	// Nil is an error when Apply is set, unless Machine is set.
	Stash func(context.Context, string) (int, error)
	// Lane is the [[lane]] name recorded on the claim and shown in the plan.
	// Empty means no lanes are configured, and the plan has no lane line.
	Lane string
	// Machine is the lane's herdr saved machine. Empty is the local server.
	// A remote checkout is not read locally, so the claim records no stash count.
	Machine string
	// LeaseStatus reads the agentbus lease of Cfg.Cwd. Apply calls it just
	// before the launch when Cfg.Lease.Mode is prompt+check. Nil, or a
	// remote Machine, records the lease as unknown.
	LeaseStatus func(context.Context, string) (agentbus.Reading, error)
	// Wake wakes a hibernated agent in pane on machine (empty is the local
	// server) just before its prompt, and reports whether it woke one.
	// Nil never wakes (wake_before_prompt = "off"). agentbus.ErrMissing
	// skips the wake. Any other error fails the start before the prompt.
	Wake func(ctx context.Context, machine, pane string) (bool, error)
	// Sleep pauses between agent-start retries and after the ready wait.
	// Nil sleeps for real, ending early when ctx ends.
	Sleep func(ctx context.Context, d time.Duration) error
	// WorktreeAdd creates a per-worker git worktree when Worker.Worktree is true.
	WorktreeAdd func(ctx context.Context, repoDir, worktreeDir, branch, base string) error
	// WorktreeRemove removes a per-worker git worktree on launch failure or settle.
	WorktreeRemove func(ctx context.Context, repoDir, worktreeDir string) error
}

// Options selects a dry run and whether the gate and ready checks are skipped.
type Options struct {
	Apply bool // Apply claims the item and starts the agent.
	// Force skips queue/gate checks (require_labels, exclude_labels, not_before,
	// parent, non-in-progress status, tab, pr, claimed, and the start gate).
	// Safety checks (not done, require_leaf, require_unblocked, ownership_rule,
	// in_progress_rule) and live existing tab still refuse.
	Force bool
	// ForceTakeOwnership allows taking over an item assigned to someone else,
	// already marked in-progress, or claimed on another machine nickname.
	ForceTakeOwnership bool
	// AgentArgs replaces rules and defaults when AgentArgsSet is true.
	AgentArgs    []string
	AgentArgsSet bool
	// JSON prints the start outcome as one JSON object on stdout.
	JSON bool
}

// Start claims id and starts an agent when opts.Apply is set.
// A dry run prints the plan and returns. When the gate refuses, the dry run
// still prints the plan, marked blocked, then returns the refusal. Force
// skips queue checks and the start gate, while safety checks (not done,
// require_leaf, require_unblocked, ownership_rule, in_progress_rule) still
// refuse. ForceTakeOwnership takes over foreign assignee, in-progress state,
// and foreign machine nickname claims. An existing tab is an error before any
// claim. Apply does not print that blocked plan.
func (s *Starter) Start(ctx context.Context, id string, snap ready.Snapshot, gateRes gate.Result, opts Options) error {
	it, err := s.validate(ctx, id, snap)
	if err != nil {
		if s != nil && s.Out != nil && id != "" {
			return s.finishOutcome(opts, refusedOutcome(id, err), err)
		}
		return err
	}
	ownClaim, err := s.hasOwnClaim(ctx, id)
	if err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	snap, err = s.withStoreApproval(ctx, id, snap)
	if err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	if err := s.admit(id, it, snap, gateRes, opts, ownClaim); err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	ws, tab, prompt, err := s.prepared(it)
	if err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	agent, err := s.agentNameFor(id, tab)
	if err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	choice := config.ChooseAgentArgs(it.Labels, s.effectiveDefaultArgs(), s.Rules, opts.AgentArgs, opts.AgentArgsSet)
	if !opts.JSON {
		if err := s.writePlan(ctx, it, ws, tab, prompt, false, choice, ownClaim); err != nil {
			return err
		}
		if err := s.writeNotes(gateNotes(gateRes, opts)); err != nil {
			return err
		}
	}
	if !opts.Apply {
		return s.finishDryRun(ctx, id, prompt, choice, opts)
	}
	return s.applyStart(ctx, id, ws, tab, prompt, agent, choice, snap, opts, ownClaim)
}

func (s *Starter) finishDryRun(ctx context.Context, id, prompt string, choice config.AgentArgChoice, opts Options) error {
	if !opts.JSON {
		return nil
	}
	out, err := s.dryRunOutcome(ctx, id, prompt, choice)
	if err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	return s.writeOutcome(opts, out)
}

func (s *Starter) dryRunOutcome(ctx context.Context, id, prompt string, choice config.AgentArgChoice) (Outcome, error) {
	mode := s.effectiveOutputMode()
	out := Outcome{
		ID:         id,
		Status:     OutcomeDryRun,
		OutputMode: mode,
		KeepWorker: keepWorkerForMode(mode),
		ExitCode:   ExitStarted,
	}
	if s.effectiveSpawner() != config.SpawnerSubagent {
		return out, nil
	}
	srcName := s.Kind + ":" + s.Cfg.Name
	if s.Src != nil && s.Src.Name() != "" {
		srcName = s.Src.Name()
	}
	cwd := s.Cfg.Cwd
	if s.effectiveWorktree() && s.Machine == "" {
		var err error
		cwd, _, err = s.allocateWorktree(ctx, id)
		if err != nil {
			return Outcome{}, err
		}
	}
	out.Spawner = config.SpawnerSubagent
	out.Source = srcName
	out.Cwd = cwd
	out.Agent = s.effectiveAgent()
	out.AgentArgs = choice.Args
	out.Env = map[string]string{
		config.ItemSourceEnv: srcName,
		config.OutputModeEnv: mode,
	}
	out.Brief = prompt
	return out, nil
}

func (s *Starter) applyStart(ctx context.Context, id, ws, tab, prompt, agent string, choice config.AgentArgChoice, snap ready.Snapshot, opts Options, ownClaim bool) error {
	if err := s.validateApply(); err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	if err := s.checkActiveClaim(ctx, id); err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	// The tab check is before Claim. Force skips the ready rules, and a tab
	// that is already open in live herdr must still fail with no claim and no release.
	if s.effectiveSpawner() != config.SpawnerSubagent {
		if err := s.existingTab(ctx, snap.Items[id]); err != nil {
			return s.finishOutcome(opts, refusedOutcome(id, err), err)
		}
	}
	lease, err := s.checkLease(ctx)
	if err != nil {
		return s.finishOutcome(opts, refusedOutcome(id, err), err)
	}
	cl, out, err := s.claim(ctx, id, snap, opts, ownClaim, choice)
	if err != nil {
		return s.finishOutcome(opts, out, err)
	}
	out, err = s.launch(ctx, launchArgs{
		id:             id,
		claimedAt:      cl.claimedAt,
		changedTracker: cl.changedTracker,
		ws:             ws,
		tab:            tab,
		agent:          agent,
		prompt:         prompt,
		agentArgs:      choice.Args,
		lease:          lease,
		launchOpts:     opts,
	})
	return s.finishOutcome(opts, out, err)
}

func (s *Starter) checkActiveClaim(ctx context.Context, id string) error {
	c, ok, err := s.Store.ActiveClaim(ctx, s.Src.Name(), id)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	if !ok {
		return nil
	}
	tab := c.TabID
	if tab == "" {
		tab = "none"
	}
	machine := c.Machine
	if machine == "" {
		machine = "unknown"
	}
	return fmt.Errorf("active %s claim on %s at %s (tab %s); run factory release %s or adopt",
		c.State, machine, c.ClaimedAt.UTC().Format(time.RFC3339), tab, id)
}

// admit applies the gate and the ready checks, or prints the --force /
// --force-taking-ownership warnings instead. A refused dry run prints the
// plan marked blocked when only the gate refused.
func (s *Starter) admit(id string, it source.Item, snap ready.Snapshot, gateRes gate.Result, opts Options, ownClaim bool) error {
	if opts.Force {
		if err := checkSafety(id, it, snap, s.Cfg, s.Kind, opts, ownClaim); err != nil {
			return err
		}
		if opts.JSON {
			return nil
		}
		if err := s.writeWarning(it, snap); err != nil {
			return err
		}
		if opts.ForceTakeOwnership {
			return s.writeTakeoverWarning(id, it, snap.Me)
		}
		return nil
	}
	err := refuse(id, it, snap, gateRes, s.Cfg, s.Kind, opts, ownClaim)
	if err != nil {
		if !opts.Apply && gateRefused(gateRes) && !opts.JSON {
			if werr := s.WriteBlocked(it, opts); werr != nil {
				return werr
			}
		}
		return err
	}
	if opts.ForceTakeOwnership && !opts.JSON {
		return s.writeTakeoverWarning(id, it, snap.Me)
	}
	return nil
}

// gateNotes are the gate notes Start prints. --force skips the gate, so none.
func gateNotes(res gate.Result, opts Options) []string {
	if opts.Force {
		return nil
	}
	return res.Notes
}

func (s *Starter) agentNameFor(id, tab string) (string, error) {
	name, err := agentName(tab, s.Kind, s.Cfg.Repo)
	if err != nil {
		return "", err
	}
	if err := checkAgentName(id, name); err != nil {
		return "", err
	}
	return name, nil
}

func (s *Starter) validate(ctx context.Context, id string, snap ready.Snapshot) (source.Item, error) {
	if ctx == nil {
		return source.Item{}, errors.New("start: expected a context, found nil")
	}
	if s == nil {
		return source.Item{}, errors.New("start: expected a starter, found nil")
	}
	if id == "" {
		return source.Item{}, errors.New("start: expected a non-empty id, found empty")
	}
	if s.Out == nil {
		return source.Item{}, errors.New("start: expected a writer, found nil")
	}
	if err := s.validateKind(); err != nil {
		return source.Item{}, err
	}
	it, ok := snap.Items[id]
	if !ok {
		return source.Item{}, fmt.Errorf("start %s: expected the item in the snapshot, found none", id)
	}
	if it.ID != id {
		return source.Item{}, fmt.Errorf("start %s: expected snapshot id %q, found %q", id, id, it.ID)
	}
	if snap.Me == "" {
		return source.Item{}, errors.New("start: expected a non-empty me, found empty")
	}
	if err := s.validateConfig(); err != nil {
		return source.Item{}, err
	}
	if err := validateTabRef(it, s.Cfg, s.Kind); err != nil {
		return source.Item{}, err
	}
	return it, nil
}

func (s *Starter) validateKind() error {
	if s.Kind != "jira" && s.Kind != "github" {
		return fmt.Errorf("start: expected kind jira or github, found %q", s.Kind)
	}
	if s.Cfg.Kind != "" && s.Cfg.Kind != s.Kind {
		return fmt.Errorf("start: expected kind %s, found config kind %q", s.Kind, s.Cfg.Kind)
	}
	return nil
}

func (s *Starter) validateConfig() error {
	if s.Cfg.Name == "" {
		return errors.New("start: expected a non-empty source name, found empty")
	}
	if s.Cfg.Cwd == "" {
		return errors.New("start: expected a cwd, found empty")
	}
	if s.Cfg.StartPrompt == "" {
		return errors.New("start: expected a start prompt, found empty")
	}
	if s.effectiveAgent() == "" {
		return errors.New("start: expected an agent kind, found empty")
	}
	if s.Cfg.Worker.Spawner != "" {
		if err := config.ValidSpawner(s.Cfg.Worker.Spawner); err != nil {
			return fmt.Errorf("start: worker.spawner: %w", err)
		}
	}
	claim := s.effectiveClaim()
	if claim.MarkInProgress {
		switch s.Kind {
		case "jira":
			if claim.Status == "" && claim.Label == "" {
				return errors.New("start: expected claim_status, found empty")
			}
		case "github":
			if claim.Status == "" && claim.Label == "" {
				return errors.New("start: expected claim_status or claim_label, found empty")
			}
		}
	}
	return nil
}

func (s *Starter) validateApply() error {
	if s.Src == nil {
		return errors.New("start: expected a source, found nil")
	}
	if s.Store == nil {
		return errors.New("start: expected a store, found nil")
	}
	if s.Herdr == nil && s.effectiveSpawner() != config.SpawnerSubagent {
		return errors.New("start: expected a herdr client, found nil")
	}
	if s.effectiveWorktree() && s.Machine == "" && s.WorktreeAdd == nil {
		return errors.New("start: expected a git worktree add function, found nil")
	}
	if s.Now == nil {
		return errors.New("start: expected a clock, found nil")
	}
	if s.Hostname == "" {
		return errors.New("start: expected a hostname, found empty")
	}
	if s.Src.Name() == "" {
		return errors.New("start: expected a non-empty source name, found empty")
	}
	if s.Stash == nil && s.Machine == "" {
		return errors.New("start: expected a stash count function, found nil")
	}
	if s.Machine != "" && s.Lane == "" {
		return fmt.Errorf("start: expected a lane for machine %s, found empty", s.Machine)
	}
	return nil
}

func refuse(id string, it source.Item, snap ready.Snapshot, gateRes gate.Result, cfg config.Source, kind string, opts Options, ownClaim bool) error {
	var parts []string
	if part := gateFailure(gateRes); part != "" {
		parts = append(parts, part)
	}
	part, err := readyFailureForStart(it, snap, cfg, kind, opts, ownClaim)
	if err != nil {
		return err
	}
	if part != "" {
		parts = append(parts, part)
	}
	if len(parts) == 0 {
		return nil
	}
	return fmt.Errorf("start %s: %s", id, strings.Join(parts, "; "))
}

func gateRefused(res gate.Result) bool {
	return !res.OK && len(res.Reasons) > 0
}

func gateFailure(res gate.Result) string {
	switch {
	case res.OK && len(res.Reasons) == 0:
		return ""
	case !res.OK && len(res.Reasons) > 0:
		return "gate: " + strings.Join(res.Reasons, "; ")
	default:
		return fmt.Sprintf("gate: expected OK or reasons, found ok=%t reasons=%d", res.OK, len(res.Reasons))
	}
}

func failingChecks(result ready.Result) []string {
	parts := make([]string, 0, len(result.Checks))
	for _, check := range result.Checks {
		if check.Pass {
			continue
		}
		parts = append(parts, formatCheck(check))
	}
	return parts
}
