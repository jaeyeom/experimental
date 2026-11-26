package config

import (
	"errors"
	"fmt"
	"strings"
)

const (
	// SpawnerAgentbus spawns a worker through agentbus with the selected coding agent.
	SpawnerAgentbus = "agentbus"
	// SpawnerSubagent claims the item and outputs the brief for the coordinator's subagent tool.
	SpawnerSubagent = "subagent"
	// SpawnerHerdr spawns a worker directly in a herdr tab.
	SpawnerHerdr = "herdr"

	// DefaultAgent is the coding agent when neither worker.agent nor defaults.agent_kind is set.
	DefaultAgent = "claude"
	// DefaultMaxWorkers is the parallel running worker cap when worker.max_workers is unset.
	DefaultMaxWorkers = 1
)

// Worker is the [worker] and [source.worker] configuration: how workers are
// spawned, how many run in parallel, and whether each uses a git worktree.
// A source inherits every field from the top-level [worker] and overrides
// only the fields it sets.
type Worker struct {
	// Spawner is SpawnerAgentbus (the default), SpawnerSubagent, or SpawnerHerdr.
	Spawner string `toml:"spawner"`
	// Agent is the coding agent name, such as "agy", "claude", "codex", or "grok".
	Agent string `toml:"agent"`
	// AgentArgs are arguments passed to the coding agent. Empty means none.
	AgentArgs []string `toml:"agent_args"`
	// MaxWorkers caps the workers running in parallel, orthogonal to Spawner and Defaults.MaxInFlight.
	MaxWorkers int `toml:"max_workers"`
	// Worktree isolates each worker in its own git worktree when true.
	Worktree bool `toml:"worktree"`
}

// clone returns w with a distinct AgentArgs slice.
func (w Worker) clone() Worker {
	w.AgentArgs = copyStrings(w.AgentArgs)
	return w
}

// LaneWorker is [lane.worker]. Only set fields override the source's Worker;
// max_workers lives on [[lane]] itself.
type LaneWorker struct {
	// Spawner overrides Worker.Spawner when non-empty.
	Spawner string `toml:"spawner"`
	// Agent overrides Worker.Agent when non-empty.
	Agent string `toml:"agent"`
	// AgentArgs replaces Worker.AgentArgs when non-nil; an empty slice clears inherited args.
	AgentArgs []string `toml:"agent_args"`
	// Worktree overrides Worker.Worktree when non-nil.
	Worktree *bool `toml:"worktree"`
}

// Over returns base with every field set in w replacing base's.
func (w LaneWorker) Over(base Worker) Worker {
	out := base.clone()
	if w.Spawner != "" {
		out.Spawner = w.Spawner
	}
	if w.Agent != "" {
		out.Agent = w.Agent
	}
	if w.AgentArgs != nil {
		out.AgentArgs = copyStrings(w.AgentArgs)
	}
	if w.Worktree != nil {
		out.Worktree = *w.Worktree
	}
	return out
}

type rawWorker struct {
	Spawner    *string  `toml:"spawner"`
	Agent      *string  `toml:"agent"`
	AgentArgs  []string `toml:"agent_args"`
	MaxWorkers *int     `toml:"max_workers"`
	Worktree   *bool    `toml:"worktree"`
}

type rawLaneWorker struct {
	Spawner   *string  `toml:"spawner"`
	Agent     *string  `toml:"agent"`
	AgentArgs []string `toml:"agent_args"`
	Worktree  *bool    `toml:"worktree"`
}

func (raw rawWorker) resolveTop(d *Defaults, rawDef rawDefaults) (Worker, error) {
	if raw.Agent != nil && rawDef.AgentKind != nil {
		return Worker{}, errors.New("worker.agent: expected one of worker.agent or defaults.agent_kind, found both")
	}
	if raw.AgentArgs != nil && rawDef.AgentArgs != nil {
		return Worker{}, errors.New("worker.agent_args: expected one of worker.agent_args or defaults.agent_args, found both")
	}
	base := Worker{
		Spawner:    SpawnerAgentbus,
		Agent:      d.AgentKind,
		AgentArgs:  copyStrings(d.AgentArgs),
		MaxWorkers: DefaultMaxWorkers,
	}
	out := raw.over(base)
	if raw.Agent != nil {
		d.AgentKind = out.Agent
	}
	if raw.AgentArgs != nil {
		d.AgentArgs = copyStrings(out.AgentArgs)
	}
	return out, nil
}

func (raw rawWorker) over(base Worker) Worker {
	out := base.clone()
	if raw.Spawner != nil {
		out.Spawner = *raw.Spawner
	}
	if raw.Agent != nil {
		out.Agent = *raw.Agent
	}
	if raw.AgentArgs != nil {
		out.AgentArgs = copyStrings(raw.AgentArgs)
	}
	if raw.MaxWorkers != nil {
		out.MaxWorkers = *raw.MaxWorkers
	}
	if raw.Worktree != nil {
		out.Worktree = *raw.Worktree
	}
	return out
}

func (raw rawLaneWorker) toLaneWorker(name string) (LaneWorker, error) {
	if raw.Spawner != nil && *raw.Spawner == "" {
		return LaneWorker{}, fmt.Errorf("lane %q worker.spawner: %w", name, ValidSpawner(""))
	}
	if raw.Agent != nil && *raw.Agent == "" {
		return LaneWorker{}, fmt.Errorf("lane %q worker.agent: %w", name, ValidAgent(""))
	}
	var out LaneWorker
	if raw.Spawner != nil {
		out.Spawner = *raw.Spawner
	}
	if raw.Agent != nil {
		out.Agent = *raw.Agent
	}
	if raw.AgentArgs != nil {
		out.AgentArgs = copyStrings(raw.AgentArgs)
	}
	if raw.Worktree != nil {
		v := *raw.Worktree
		out.Worktree = &v
	}
	return out, nil
}

// ValidSpawner checks a worker spawner value: agentbus, subagent, or herdr.
func ValidSpawner(value string) error {
	switch value {
	case SpawnerAgentbus, SpawnerSubagent, SpawnerHerdr:
		return nil
	case "":
		return fmt.Errorf("expected %s, %s, or %s, found empty", SpawnerAgentbus, SpawnerSubagent, SpawnerHerdr)
	default:
		return fmt.Errorf("expected %s, %s, or %s, found %q", SpawnerAgentbus, SpawnerSubagent, SpawnerHerdr, value)
	}
}

// ValidAgent checks a coding agent name: non-empty and without whitespace.
func ValidAgent(value string) error {
	if value == "" {
		return errors.New("expected a non-empty coding agent, found empty")
	}
	if strings.ContainsAny(value, " \t\r\n") {
		return fmt.Errorf("expected an agent name without spaces, found %q", value)
	}
	return nil
}

func validWorker(where string, w Worker, checkAgent, checkArgs bool) error {
	if err := ValidSpawner(w.Spawner); err != nil {
		return fmt.Errorf("%s.spawner: %w", where, err)
	}
	if checkAgent {
		if err := ValidAgent(w.Agent); err != nil {
			return fmt.Errorf("%s.agent: %w", where, err)
		}
	}
	if w.MaxWorkers < 1 {
		return fmt.Errorf("%s.max_workers: expected a positive integer, found %d", where, w.MaxWorkers)
	}
	if checkArgs {
		if err := validateArgList(where+`.agent_args`, w.AgentArgs); err != nil {
			return err
		}
	}
	return nil
}
