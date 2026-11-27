// Package tick runs one factory pass: sync, post-merge handling, the start
// gate, and at most one start. Run repeats that pass on an interval.
package tick

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
	"github.com/jaeyeom/experimental/devtools/factory/internal/verify"
)

const (
	runCommand             = "tick"
	settleCommand          = "settle"
	outcomeSettled         = "settled"
	outcomeStarted         = "started"
	outcomeGated           = "gated"
	outcomeIdle            = "idle"
	outcomeDryRun          = "dry_run"
	outcomeLost            = "lost_race"
	maxStartAttempts       = 3
	defaultTickLookback    = 7 * 24 * time.Hour
	mergeOverlap           = time.Hour
	mergeSettleWaitTimeout = 5 * time.Second
)

// Pulls is the gh surface a tick reads.
type Pulls interface {
	MergedByMe(ctx context.Context) ([]gh.PR, error)
	MergedByMeSince(ctx context.Context, t time.Time) ([]gh.PR, error)
	MergedByTitle(ctx context.Context, id string) ([]gh.PR, error)
	OpenByMe(ctx context.Context) ([]gh.PR, error)
}

// TabCloser closes one herdr tab.
// CloseTab returns the herdr result.type when the close succeeded.
type TabCloser interface {
	CloseTab(ctx context.Context, tabID string) (string, error)
}

// Sessions is the herdr list surface a tick reads.
type Sessions interface {
	Workspaces(ctx context.Context) ([]herdr.Workspace, error)
	Tabs(ctx context.Context) ([]herdr.Tab, error)
	Panes(ctx context.Context) ([]herdr.Pane, error)
}

// MergeChecker checks origin/<base> in dir for a linear commit matching id
// and pattern. It returns the newest matching commit, a failure reason when
// none landed or history is not linear, and any execution error.
type MergeChecker func(ctx context.Context, dir, base, id, pattern string) (verify.LandedCommit, string, error)

// Engine is one factory tick, or a loop of ticks.
type Engine struct {
	Store    *store.Store             // Mirror, claims, and runs.
	Sources  map[string]source.Source // Backends keyed by kind:name.
	Config   *config.Config           // Loaded factory.toml.
	Pulls    Pulls                    // Merged and open pull requests.
	Sessions Sessions                 // Herdr workspaces, tabs, and panes.
	Closer   TabCloser                // Closes a settled item's tab. Nil is an error when a tab should close.
	Agent    start.HerdrAPI           // Herdr calls Start makes. Tests pass a fake.
	// WaitAgent waits for a working merge-mode worker tab to finish its turn
	// before closing. Nil falls back to Agent.WaitAgent when Agent is set.
	WaitAgent func(ctx context.Context, target string, until []string, timeout time.Duration) (string, error)
	Git       func(context.Context, string) (string, error)
	GitBranch func(context.Context, string) (string, error)      // Short name of HEAD when gate.base_branch is set.
	GitAhead  func(context.Context, string, string) (int, error) // Commits on HEAD that are not on origin/base.
	Stash     func(context.Context, string) (int, error)         // Stash list length recorded when a start applies.
	// GitMerge checks origin/<base> for a linear commit matching a merge-mode claim.
	GitMerge MergeChecker
	// RemoteGitMerge returns a MergeChecker for a remote lane using lane.Exec.
	RemoteGitMerge func(lane config.Lane) (MergeChecker, error)
	// Lease reads the agentbus lease of a checkout for the gate. Nil with
	// [lease] gate auto or required is a gate reason.
	Lease func(context.Context, string) (agentbus.Reading, error)
	// Wake is start.Starter.Wake: it wakes a hibernated agent before the
	// prompt. Nil never wakes.
	Wake func(ctx context.Context, machine, pane string) (bool, error)
	// Machine returns the herdr surface on a remote lane's machine. It is
	// required when Config has a remote [[lane]]. A local lane uses
	// Sessions, Closer, and Agent.
	Machine func(machine string) (LaneMachine, error)
	// Leases lists every checkout's lease from the agentbus hub, so a
	// remote lane counts a claim that holds its checkout as running. Nil
	// skips that check.
	Leases func(ctx context.Context) ([]agentbus.Status, error)
	// Sleep is start.Starter.Sleep. Nil sleeps for real.
	Sleep func(ctx context.Context, d time.Duration) error
	// WorktreeAdd creates a per-worker git worktree when Worker.Worktree is true.
	WorktreeAdd func(ctx context.Context, repoDir, worktreeDir, branch, base string) error
	// WorktreeRemove removes a per-worker git worktree on launch failure or settle.
	WorktreeRemove func(ctx context.Context, repoDir, worktreeDir string) error
	Now            func() time.Time // Clock for the merge window, the gate, and the run row.
	Hostname       string           // Stored on a claim as the machine.
	Out            io.Writer        // Summary lines, and the start plan.
	Apply          bool             // Apply claims the item and starts the agent.
	Caller         string           // Substituted for {caller}. Empty expands to empty.
	// CallerBus is the caller's agentbus name or handle, for {caller_bus}
	// and {report_back}. Empty when agentbus is not available.
	CallerBus string
	// Enqueue prints a start-item job per ready item to Jobs instead of
	// gating and starting one.
	Enqueue bool
	Jobs    io.Writer
	// Only, when non-empty, limits sync and picking to this source key (kind:name).
	Only string
}

func (e *Engine) validate(ctx context.Context) error {
	if ctx == nil {
		return errors.New("tick: expected a context, found nil")
	}
	if e == nil {
		return errors.New("tick: expected an engine, found nil")
	}
	if e.Store == nil {
		return errors.New("tick: expected a store, found nil")
	}
	if e.Config == nil {
		return errors.New("tick: expected a config, found nil")
	}
	if e.Sources == nil {
		return errors.New("tick: expected a source map, found nil")
	}
	if e.Pulls == nil {
		return errors.New("tick: expected a gh client, found nil")
	}
	if e.Sessions == nil {
		return errors.New("tick: expected a herdr client, found nil")
	}
	if e.Git == nil {
		return errors.New("tick: expected a git status function, found nil")
	}
	if err := e.validateGit(); err != nil {
		return err
	}
	if e.Now == nil {
		return errors.New("tick: expected a clock, found nil")
	}
	if e.Out == nil {
		return errors.New("tick: expected a writer, found nil")
	}
	if e.Hostname == "" {
		return errors.New("tick: expected a hostname, found empty")
	}
	return nil
}

func (e *Engine) validateGit() error {
	if e.Config.Gate.BaseBranch == "" {
		return nil
	}
	if e.GitBranch == nil {
		return errors.New("tick: expected a git branch function, found nil")
	}
	if e.Config.Gate.Require == config.GateCleanAgainstBase && e.GitAhead == nil {
		return errors.New("tick: expected a git rev-list function, found nil")
	}
	return nil
}

// Tick syncs, settles merged and handed-off claims, and starts at most one item.
// With [[lane]] config it starts at most one item per lane that has room.
// A sync error is returned and nothing is started. Gate refusals and an empty
// ready list are successful ticks.
func (e *Engine) Tick(ctx context.Context) error {
	if err := e.validate(ctx); err != nil {
		return err
	}
	at := e.Now()
	if at.IsZero() {
		return errors.New("tick: expected a clock time, found zero")
	}
	if err := e.sync(ctx, at); err != nil {
		return err
	}
	closed, err := e.merged(ctx, at)
	if err != nil {
		return err
	}
	open, err := e.Pulls.OpenByMe(ctx)
	if err != nil {
		return fmt.Errorf("tick: open pull requests: %w", err)
	}
	if err := e.observe(ctx, at, open); err != nil {
		return err
	}
	if err := e.handOff(ctx, open); err != nil {
		return err
	}
	tabs, workspaces, panes, err := e.sessions(ctx)
	if err != nil {
		return err
	}
	ranked, err := e.ready(ctx, open, tabs, workspaces, closed)
	if err != nil {
		return err
	}
	if e.Enqueue {
		return e.enqueue(ctx, at, ranked)
	}
	if e.Config.HasLanes() {
		return e.pickLanes(ctx, at, ranked, tabs, panes, open)
	}
	return e.pick(ctx, at, ranked, panes, open)
}

func (e *Engine) sync(ctx context.Context, now time.Time) error {
	if err := e.sourcesMatch(); err != nil {
		return err
	}
	results, err := syncer.Sync(ctx, e.Store, e.Sources, now, syncer.Options{
		Only:            e.Only,
		FullSyncEvery:   e.Config.Defaults.FullSyncEvery,
		AfterSync:       backlog.Hook(e.Store, e.Config),
		ExternalLookups: e.Config.ExternalLookups(),
	})
	if ferr := syncFailure(results, err); ferr != nil {
		return fmt.Errorf("tick: %w", ferr)
	}
	for _, res := range results {
		if res.Warning != "" {
			slog.Warn("sync warning", "source", res.Source, "warning", res.Warning)
		}
	}
	return nil
}

func (e *Engine) sourcesMatch() error {
	seen := make(map[string]struct{}, len(e.Config.Sources))
	for i := range e.Config.Sources {
		key, err := sourceKey(e.Config.Sources[i].Kind, e.Config.Sources[i].Name)
		if err != nil {
			return err
		}
		if _, ok := seen[key]; ok {
			return fmt.Errorf("source %s: expected a unique key, found a duplicate", key)
		}
		seen[key] = struct{}{}
		src, ok := e.Sources[key]
		if !ok || src == nil {
			return fmt.Errorf("source %s: expected a source, found none", key)
		}
		if got := src.Name(); got != key {
			return fmt.Errorf("source %s: expected name %q, found %q", key, key, got)
		}
	}
	if len(seen) != len(e.Sources) {
		return fmt.Errorf("tick: expected %d sources, found %d", len(seen), len(e.Sources))
	}
	return nil
}

func syncFailure(results []syncer.Result, err error) error {
	if err != nil {
		return err
	}
	parts := make([]string, 0, len(results))
	for _, res := range results {
		if res.Err != nil {
			parts = append(parts, res.Source+": "+res.Err.Error())
		}
	}
	if len(parts) == 0 {
		return nil
	}
	return fmt.Errorf("sync: %s", joinParts(parts))
}

func (e *Engine) sessions(ctx context.Context) ([]herdr.Tab, []herdr.Workspace, []herdr.Pane, error) {
	workspaces, err := e.Sessions.Workspaces(ctx)
	if err != nil {
		return nil, nil, nil, fmt.Errorf("herdr workspaces: %w", err)
	}
	tabs, err := e.Sessions.Tabs(ctx)
	if err != nil {
		return nil, nil, nil, fmt.Errorf("herdr tabs: %w", err)
	}
	panes, err := e.Sessions.Panes(ctx)
	if err != nil {
		return nil, nil, nil, fmt.Errorf("herdr panes: %w", err)
	}
	return tabs, workspaces, panes, nil
}

func (e *Engine) finish(ctx context.Context, at time.Time, outcome, line string) error {
	if err := e.writeLine(line); err != nil {
		return err
	}
	return e.record(ctx, at, outcome, line)
}

func (e *Engine) writeLine(line string) error {
	if _, err := fmt.Fprintln(e.Out, line); err != nil {
		return fmt.Errorf("write tick: %w", err)
	}
	return nil
}

func (e *Engine) record(ctx context.Context, at time.Time, outcome, summary string) error {
	leaseGate := ""
	if e.Config != nil {
		leaseGate = e.Config.Lease.Gate
	}
	return e.recordDetail(ctx, runCommand, at, outcome, summary, leaseGate)
}

func (e *Engine) recordAs(ctx context.Context, command string, at time.Time, outcome, summary string) error {
	return e.recordDetail(ctx, command, at, outcome, summary, "")
}

func (e *Engine) recordDetail(ctx context.Context, command string, at time.Time, outcome, summary, leaseGate string) error {
	detail, err := detailJSON(summary, leaseGate)
	if err != nil {
		return err
	}
	if err := e.Store.AppendRun(ctx, store.Run{
		StartedAt:  at,
		Command:    command,
		Outcome:    outcome,
		DetailJSON: detail,
	}); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	return nil
}

func detailJSON(summary, leaseGate string) (string, error) {
	raw, err := json.Marshal(struct {
		Summary   string `json:"summary"`
		LeaseGate string `json:"lease_gate,omitempty"` //nolint:tagliatelle // documented factory run log field
	}{Summary: summary, LeaseGate: leaseGate})
	if err != nil {
		return "", fmt.Errorf("tick detail: %w", err)
	}
	return string(raw), nil
}

func sourceKey(kind, name string) (string, error) {
	if kind == "" || name == "" {
		return "", fmt.Errorf("source: expected non-empty kind and name, found kind %q name %q", kind, name)
	}
	return kind + ":" + name, nil
}
