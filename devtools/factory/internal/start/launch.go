package start

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

type placed struct {
	ws    herdr.Workspace
	pane  herdr.Pane
	tabID string
}

// launchArgs is what launch needs once the item is claimed.
type launchArgs struct {
	id             string
	claimedAt      time.Time
	changedTracker bool
	ws, tab        string // Workspace and tab labels.
	agent          string
	prompt         string
	agentArgs      []string
	lease          *leaseRecord // Nil unless the lease mode is prompt+check.
	launchOpts     Options
}

func (s *Starter) launch(ctx context.Context, a launchArgs) (Outcome, error) {
	id, claimedAt := a.id, a.claimedAt
	row := store.Claim{
		Source:    s.Src.Name(),
		ID:        id,
		ClaimedAt: claimedAt,
		Machine:   s.Hostname,
		State:     store.Claimed,
	}
	worktreeDir, err := s.prepareWorktree(ctx, id)
	if err != nil {
		ferr := s.fail(ctx, row, 3, err)
		return claimKeptOutcome(id, ferr), ferr
	}
	if worktreeDir != "" {
		if err := s.Store.UpdateClaimWorktree(ctx, row.Source, row.ID, row.ClaimedAt, worktreeDir); err != nil {
			if cerr := s.cleanupWorktree(ctx, worktreeDir); cerr != nil {
				slog.Warn("cleanup worktree after claim update failure", "dir", worktreeDir, "err", cerr)
			}
			ferr := s.fail(ctx, row, 3, err)
			return claimKeptOutcome(id, ferr), ferr
		}
	}
	cwd := s.Cfg.Cwd
	if worktreeDir != "" {
		cwd = worktreeDir
	}
	spawner := s.effectiveSpawner()
	if spawner == config.SpawnerSubagent {
		return s.launchSubagent(ctx, row, a, cwd, worktreeDir)
	}
	return s.launchHerdr(ctx, row, a, spawner, cwd, worktreeDir)
}

func (s *Starter) launchHerdr(ctx context.Context, row store.Claim, a launchArgs, spawner, cwd, worktreeDir string) (Outcome, error) {
	opened, step, err := s.place(ctx, a.ws, a.tab, cwd)
	if err != nil {
		ferr := s.fail(ctx, row, step, err)
		return claimKeptOutcome(a.id, ferr), ferr
	}
	pane := opened.pane.ID
	if err := s.startAgent(ctx, a.agent, pane, a.agentArgs); err != nil {
		if cerr := s.cleanupWorktree(ctx, worktreeDir); cerr != nil {
			slog.Warn("cleanup worktree after failed agent start", "dir", worktreeDir, "err", cerr)
		}
		return s.abandon(ctx, row, opened, 5, err, a.changedTracker)
	}
	log, err := s.waitAndPrompt(ctx, pane, a.prompt, spawner)
	if err != nil {
		ferr := s.fail(ctx, row, log.failStep, err)
		return claimKeptOutcome(a.id, ferr), ferr
	}
	if err := s.Store.UpdateClaim(ctx, row.Source, row.ID, row.ClaimedAt, store.Started, "", opened.tabID, opened.ws.ID); err != nil {
		ferr := s.fail(ctx, row, 7, err)
		return claimKeptOutcome(a.id, ferr), ferr
	}
	row.TabID = opened.tabID
	row.WorkspaceID = opened.ws.ID
	recSpawner := ""
	if spawner != config.SpawnerAgentbus {
		recSpawner = spawner
	}
	if err := s.record(ctx, a.id, a.claimedAt, opened.tabID, opened.ws.ID, a.lease, log.launchLog, recSpawner, worktreeDir); err != nil {
		ferr := s.fail(ctx, row, 7, err)
		return claimKeptOutcome(a.id, ferr), ferr
	}
	mode := s.effectiveOutputMode()
	return Outcome{
		ID:         a.id,
		Status:     OutcomeStarted,
		OutputMode: mode,
		KeepWorker: keepWorkerForMode(mode),
		TabID:      opened.tabID,
		Worktree:   worktreeDir,
		ClaimTime:  a.claimedAt.UTC().Format(time.RFC3339),
		ExitCode:   ExitStarted,
	}, nil
}

type waitPromptLog struct {
	launchLog
	failStep int
}

func (s *Starter) waitAndPrompt(ctx context.Context, pane, prompt, spawner string) (waitPromptLog, error) {
	var out waitPromptLog
	var err error
	if out.readyWait, err = s.waitReady(ctx, pane); err != nil {
		out.failStep = 5
		return out, err
	}
	if spawner != config.SpawnerHerdr {
		if out.woke, err = s.wakeBeforePrompt(ctx, pane); err != nil {
			out.failStep = 6
			return out, err
		}
	}
	if err := s.Herdr.Prompt(ctx, pane, prompt, []string{herdr.AgentStatusWorking}, promptTimeout); err != nil {
		if !stalled(err) {
			out.failStep = 6
			return out, err //nolint:wrapcheck // propagate context cancellation
		}
		out.stalled = true
		if out.recovery, err = s.recoverStall(ctx, pane, prompt, err); err != nil {
			out.failStep = 6
			return out, err
		}
	}
	return out, nil
}

func (s *Starter) launchSubagent(ctx context.Context, row store.Claim, a launchArgs, cwd, worktreeDir string) (Outcome, error) {
	if err := s.Store.UpdateClaim(ctx, row.Source, row.ID, row.ClaimedAt, store.Started, "", "", ""); err != nil {
		ferr := s.fail(ctx, row, 7, err)
		return claimKeptOutcome(a.id, ferr), ferr
	}
	if err := s.record(ctx, a.id, a.claimedAt, "", "", a.lease, launchLog{}, config.SpawnerSubagent, worktreeDir); err != nil {
		ferr := s.fail(ctx, row, 7, err)
		return claimKeptOutcome(a.id, ferr), ferr
	}
	mode := s.effectiveOutputMode()
	agent := s.effectiveAgent()
	if !a.launchOpts.JSON {
		var hdr strings.Builder
		fmt.Fprintf(&hdr, "--- subagent brief ---\n")
		fmt.Fprintf(&hdr, "id: %s\n", a.id)
		fmt.Fprintf(&hdr, "source: %s\n", row.Source)
		fmt.Fprintf(&hdr, "output: %s\n", subagentOutputHeader(mode))
		fmt.Fprintf(&hdr, "env: %s=%s %s=%s\n", config.ItemSourceEnv, row.Source, config.OutputModeEnv, mode)
		fmt.Fprintf(&hdr, "cwd: %s\n", cwd)
		fmt.Fprintf(&hdr, "agent: %s\n", agent)
		if len(a.agentArgs) > 0 {
			fmt.Fprintf(&hdr, "agent_args: %s\n", strings.Join(a.agentArgs, " "))
		}
		fmt.Fprintf(&hdr, "\n%s\n--- end brief ---\n", a.prompt)
		if _, err := io.WriteString(s.Out, hdr.String()); err != nil {
			return Outcome{}, fmt.Errorf("write start: %w", err)
		}
	}
	return Outcome{
		ID:         a.id,
		Status:     OutcomeStarted,
		Spawner:    config.SpawnerSubagent,
		OutputMode: mode,
		KeepWorker: keepWorkerForMode(mode),
		Source:     row.Source,
		Cwd:        cwd,
		Agent:      agent,
		AgentArgs:  a.agentArgs,
		Env: map[string]string{
			config.ItemSourceEnv: row.Source,
			config.OutputModeEnv: mode,
		},
		Worktree:  worktreeDir,
		Brief:     a.prompt,
		ClaimTime: a.claimedAt.UTC().Format(time.RFC3339),
		ExitCode:  ExitStarted,
	}, nil
}

func subagentOutputHeader(mode string) string {
	if mode == config.OutputModeMerge {
		return "merge (exit on completion; do not keep subagent)"
	}
	return "pr (keep subagent for review feedback until settled)"
}

func (s *Starter) place(ctx context.Context, wsLabel, tabLabel, cwd string) (placed, int, error) {
	ws, ok, err := s.Herdr.FindWorkspace(ctx, wsLabel)
	if err != nil {
		return placed{}, 3, err //nolint:wrapcheck // propagate context cancellation
	}
	if !ok {
		return s.openWorkspace(ctx, wsLabel, cwd)
	}
	if ws.ID == "" {
		return placed{}, 3, fmt.Errorf("workspace %q: expected an id, found empty", wsLabel)
	}
	return s.openTab(ctx, ws, tabLabel, cwd)
}

func (s *Starter) openWorkspace(ctx context.Context, label, cwd string) (placed, int, error) {
	ws, pane, err := s.Herdr.CreateWorkspace(ctx, label, cwd, s.workerEnv())
	if err != nil {
		return placed{}, 3, err //nolint:wrapcheck // propagate context cancellation
	}
	if err := requireIDs(label, ws, pane); err != nil {
		return placed{}, 3, err
	}
	return placed{ws: ws, pane: pane, tabID: pane.TabID}, 0, nil
}

func (s *Starter) openTab(ctx context.Context, ws herdr.Workspace, label, cwd string) (placed, int, error) {
	tab, pane, err := s.Herdr.CreateTab(ctx, ws.ID, label, cwd, s.workerEnv())
	if err != nil {
		return placed{}, 4, err //nolint:wrapcheck // propagate context cancellation
	}
	if err := requireTab(ws, tab, pane); err != nil {
		return placed{}, 4, err
	}
	return placed{ws: ws, pane: pane, tabID: tab.ID}, 0, nil
}

// workerEnv is the environment the worker's tab starts with.
func (s *Starter) workerEnv() []string {
	return []string{
		config.ItemSourceEnv + "=" + s.Src.Name(),
		config.OutputModeEnv + "=" + s.effectiveOutputMode(),
	}
}

func requireIDs(label string, ws herdr.Workspace, pane herdr.Pane) error {
	if ws.ID == "" || pane.ID == "" || pane.TabID == "" {
		return fmt.Errorf("workspace %q: expected workspace, pane, and tab ids, found workspace %q pane %q tab %q",
			label, ws.ID, pane.ID, pane.TabID)
	}
	if pane.WorkspaceID != "" && pane.WorkspaceID != ws.ID {
		return fmt.Errorf("workspace %q: expected pane workspace %q, found %q", label, ws.ID, pane.WorkspaceID)
	}
	return nil
}

func requireTab(ws herdr.Workspace, tab herdr.Tab, pane herdr.Pane) error {
	if err := requireIDs(ws.Label, ws, pane); err != nil {
		return err
	}
	if tab.ID == "" {
		return fmt.Errorf("tab %q: expected an id, found empty", tab.Label)
	}
	if pane.TabID != tab.ID {
		return fmt.Errorf("tab %q: expected pane tab %q, found %q", tab.Label, tab.ID, pane.TabID)
	}
	if tab.WorkspaceID != "" && tab.WorkspaceID != ws.ID {
		return fmt.Errorf("tab %q: expected workspace %q, found %q", tab.Label, ws.ID, tab.WorkspaceID)
	}
	return nil
}

func (s *Starter) fail(ctx context.Context, row store.Claim, step int, cause error) error {
	detail := fmt.Sprintf("step %d: %s", step, cause.Error())
	wrapped := fmt.Errorf("step %d: %w; item is claimed; fix and re-run with --force, or factory release %s", step, cause, row.ID)
	uerr := s.Store.UpdateClaim(ctx, row.Source, row.ID, row.ClaimedAt, store.Failed, detail, row.TabID, row.WorkspaceID)
	if uerr != nil {
		return errors.Join(wrapped, fmt.Errorf("mark claim failed: %w", uerr))
	}
	return wrapped
}

// wakeBeforePrompt runs s.Wake on pane. A missing agentbus is not an
// error: the prompt is sent as it was before wake_before_prompt existed.
func (s *Starter) wakeBeforePrompt(ctx context.Context, pane string) (bool, error) {
	if s.Wake == nil {
		return false, nil
	}
	woke, err := s.Wake(ctx, s.Machine, pane)
	if errors.Is(err, agentbus.ErrMissing) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("wake before prompt: %w; prompt not sent", err)
	}
	return woke, nil
}

func (s *Starter) record(ctx context.Context, id string, at time.Time, tabID, wsID string, lease *leaseRecord, log launchLog, spawner, worktree string) error {
	detail, err := runDetail(runInfo{id: id, source: s.Src.Name(), tabID: tabID, wsID: wsID, lane: s.Lane, lease: lease, log: log, spawner: spawner, worktree: worktree})
	if err != nil {
		return err
	}
	if err := s.Store.AppendRun(ctx, store.Run{
		StartedAt:  at,
		Command:    "start",
		Outcome:    "ok",
		DetailJSON: detail,
	}); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	return nil
}

// runInfo is what a start run's detail records.
type runInfo struct {
	id, source, tabID, wsID string
	lane                    string       // Empty without lanes.
	spawner                 string       // Empty when default (agentbus).
	worktree                string       // Empty when worktree isolation is off.
	lease                   *leaseRecord // Nil unless the lease mode is prompt+check.
	log                     launchLog
}

// runDetail is the start run's detail. lane, lease, and woke are omitted
// when unset, so a start without them records what it did before.
func runDetail(in runInfo) (string, error) {
	id, sourceName, tabID, wsID, lease, woke := in.id, in.source, in.tabID, in.wsID, in.lease, in.log.woke
	recovery := ""
	if in.log.stalled {
		recovery = in.log.recovery
	}
	summary := "started " + id
	if in.lane != "" {
		summary += " (lane " + in.lane + ")"
	}
	if woke {
		summary += " (woke hibernated agent)"
	}
	raw, err := json.Marshal(struct {
		Summary     string       `json:"summary"`
		Source      string       `json:"source"`
		ID          string       `json:"id"`
		TabID       string       `json:"tab_id"`       //nolint:tagliatelle // documented factory JSON output field
		WorkspaceID string       `json:"workspace_id"` //nolint:tagliatelle // documented factory JSON output field
		Lane        string       `json:"lane,omitempty"`
		Spawner     string       `json:"spawner,omitempty"`
		Worktree    string       `json:"worktree,omitempty"`
		Lease       *leaseRecord `json:"lease,omitempty"`
		Woke        bool         `json:"woke,omitempty"`
		ReadyWaitMS int64        `json:"ready_wait_ms"` //nolint:tagliatelle // documented factory JSON output field
		Stalled     bool         `json:"stalled,omitempty"`
		Recovery    string       `json:"recovery,omitempty"`
	}{
		Summary:     summary,
		Source:      sourceName,
		ID:          id,
		TabID:       tabID,
		WorkspaceID: wsID,
		Lane:        in.lane,
		Spawner:     in.spawner,
		Worktree:    in.worktree,
		Lease:       lease,
		Woke:        woke,
		ReadyWaitMS: in.log.readyWait.Milliseconds(),
		Stalled:     in.log.stalled,
		Recovery:    recovery,
	})
	if err != nil {
		return "", fmt.Errorf("start detail: %w", err)
	}
	return string(raw), nil
}
