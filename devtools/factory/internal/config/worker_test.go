package config_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestLoadWorkerDefaults(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatal(err)
	}
	want := config.Worker{
		Spawner:    config.SpawnerAgentbus,
		Agent:      config.DefaultAgent,
		MaxWorkers: config.DefaultMaxWorkers,
		Worktree:   false,
	}
	if !reflect.DeepEqual(cfg.Worker, want) {
		t.Fatalf("top worker = %+v, want %+v", cfg.Worker, want)
	}
	if !reflect.DeepEqual(cfg.Sources[0].Worker, want) {
		t.Fatalf("source worker = %+v, want %+v", cfg.Sources[0].Worker, want)
	}
}

func TestLoadWorkerInheritanceAndOverrides(t *testing.T) {
	src := githubLines()
	lane := []string{
		"[[lane]]",
		`name = "linux"`,
		`checkout = "/work/repo"`,
		"max_workers = 4",
	}
	order := []string{"[lanes]", `default = ["linux"]`}

	tests := []struct {
		name           string
		lines          []string
		wantTop        config.Worker
		wantSrc        config.Worker
		wantLane       config.LaneWorker
		wantLaneSource config.Worker
	}{
		{
			name: "top level inherited by source and lane",
			lines: cat(
				[]string{
					"[worker]",
					`spawner = "subagent"`,
					`agent = "agy"`,
					`agent_args = ["--model", "opus"]`,
					"max_workers = 3",
					"worktree = true",
				},
				src, lane, order,
			),
			wantTop: config.Worker{
				Spawner:    "subagent",
				Agent:      "agy",
				AgentArgs:  []string{"--model", "opus"},
				MaxWorkers: 3,
				Worktree:   true,
			},
			wantSrc: config.Worker{
				Spawner:    "subagent",
				Agent:      "agy",
				AgentArgs:  []string{"--model", "opus"},
				MaxWorkers: 3,
				Worktree:   true,
			},
			wantLane: config.LaneWorker{},
			wantLaneSource: config.Worker{
				Spawner:    "subagent",
				Agent:      "agy",
				AgentArgs:  []string{"--model", "opus"},
				MaxWorkers: 4,
				Worktree:   true,
			},
		},
		{
			name: "source and lane override selectively including false and empty args",
			lines: cat(
				[]string{
					"[worker]",
					`spawner = "agentbus"`,
					`agent = "grok"`,
					`agent_args = ["--fast"]`,
					"max_workers = 2",
					"worktree = true",
				},
				src,
				[]string{
					"[source.worker]",
					`agent = "codex"`,
					"max_workers = 5",
				},
				lane,
				[]string{
					"[lane.worker]",
					`spawner = "herdr"`,
					`agent_args = []`,
					"worktree = false",
				},
				order,
			),
			wantTop: config.Worker{
				Spawner:    "agentbus",
				Agent:      "grok",
				AgentArgs:  []string{"--fast"},
				MaxWorkers: 2,
				Worktree:   true,
			},
			wantSrc: config.Worker{
				Spawner:    "agentbus",
				Agent:      "codex",
				AgentArgs:  []string{"--fast"},
				MaxWorkers: 5,
				Worktree:   true,
			},
			wantLane: config.LaneWorker{
				Spawner:   "herdr",
				AgentArgs: []string{},
				Worktree:  boolPtr(false),
			},
			wantLaneSource: config.Worker{
				Spawner:    "herdr",
				Agent:      "codex",
				AgentArgs:  []string{},
				MaxWorkers: 4,
				Worktree:   false,
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg, err := newFake(t).load(tomlDoc(tc.lines))
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(cfg.Worker, tc.wantTop) {
				t.Fatalf("top = %#v, want %#v", cfg.Worker, tc.wantTop)
			}
			if !reflect.DeepEqual(cfg.Sources[0].Worker, tc.wantSrc) {
				t.Fatalf("source = %#v, want %#v", cfg.Sources[0].Worker, tc.wantSrc)
			}
			if !reflect.DeepEqual(cfg.Lanes[0].Worker, tc.wantLane) {
				t.Fatalf("lane = %#v, want %#v", cfg.Lanes[0].Worker, tc.wantLane)
			}
			gotLaneSrc := config.LaneSource(cfg.Sources[0], cfg.Lanes[0]).Worker
			if !reflect.DeepEqual(gotLaneSrc, tc.wantLaneSource) {
				t.Fatalf("lane source worker = %#v, want %#v", gotLaneSrc, tc.wantLaneSource)
			}
		})
	}
}

func TestLoadWorkerAgentsAndDefaultsCompat(t *testing.T) {
	for _, agent := range []string{"agy", "claude", "codex", "grok"} {
		cfg, err := newFake(t).load(tomlDoc(cat([]string{"[worker]", `agent = "` + agent + `"`}, githubLines())))
		if err != nil {
			t.Fatalf("agent %s: %v", agent, err)
		}
		if cfg.Worker.Agent != agent || cfg.Defaults.AgentKind != agent || cfg.Sources[0].Worker.Agent != agent {
			t.Fatalf("agent %s: worker=%q defaults=%q src=%q", agent, cfg.Worker.Agent, cfg.Defaults.AgentKind, cfg.Sources[0].Worker.Agent)
		}
	}

	fromDefaults, err := newFake(t).load(tomlDoc(cat([]string{"[defaults]", `agent_kind = "codex"`, `agent_args = ["--model", "o3"]`}, githubLines())))
	if err != nil {
		t.Fatal(err)
	}
	if fromDefaults.Worker.Agent != "codex" || !reflect.DeepEqual(fromDefaults.Worker.AgentArgs, []string{"--model", "o3"}) {
		t.Fatalf("worker from defaults = %+v", fromDefaults.Worker)
	}
	fromDefaults.Worker.AgentArgs[0] = "mutated"
	if fromDefaults.Defaults.AgentArgs[0] == "mutated" || fromDefaults.Sources[0].Worker.AgentArgs[0] == "mutated" {
		t.Fatal("worker agent_args slices are shared")
	}
}

func TestLoadWorkerRejected(t *testing.T) {
	src := githubLines()
	lane := []string{
		"[[lane]]",
		`name = "linux"`,
		`checkout = "/work/repo"`,
		"max_workers = 2",
	}
	order := []string{"[lanes]", `default = ["linux"]`}

	tests := []struct {
		name    string
		lines   []string
		wantErr string
	}{
		{
			name:    "unknown spawner",
			lines:   cat([]string{"[worker]", `spawner = "tmux"`}, src),
			wantErr: `worker.spawner: expected agentbus, subagent, or herdr, found "tmux"`,
		},
		{
			name:    "empty spawner",
			lines:   cat([]string{"[worker]", `spawner = ""`}, src),
			wantErr: "worker.spawner: expected agentbus, subagent, or herdr, found empty",
		},
		{
			name:    "unknown source spawner",
			lines:   cat(src, []string{"[source.worker]", `spawner = "tmux"`}),
			wantErr: `source "experimental" worker.spawner: expected agentbus, subagent, or herdr, found "tmux"`,
		},
		{
			name:    "empty lane spawner",
			lines:   cat(src, lane, []string{"[lane.worker]", `spawner = ""`}, order),
			wantErr: `lane "linux" worker.spawner: expected agentbus, subagent, or herdr, found empty`,
		},
		{
			name:    "unknown lane spawner",
			lines:   cat(src, lane, []string{"[lane.worker]", `spawner = "tmux"`}, order),
			wantErr: `lane "linux" worker.spawner: expected agentbus, subagent, or herdr, found "tmux"`,
		},
		{
			name:    "empty worker agent",
			lines:   cat([]string{"[worker]", `agent = ""`}, src),
			wantErr: "worker.agent: expected a non-empty coding agent, found empty",
		},
		{
			name:    "worker agent with space",
			lines:   cat([]string{"[worker]", `agent = "claude code"`}, src),
			wantErr: `worker.agent: expected an agent name without spaces, found "claude code"`,
		},
		{
			name:    "empty defaults agent_kind",
			lines:   cat([]string{"[defaults]", `agent_kind = ""`}, src),
			wantErr: `defaults field "agent_kind": expected a non-empty coding agent, found empty`,
		},
		{
			name:    "empty lane agent",
			lines:   cat(src, lane, []string{"[lane.worker]", `agent = ""`}, order),
			wantErr: `lane "linux" worker.agent: expected a non-empty coding agent, found empty`,
		},
		{
			name:    "both worker.agent and defaults.agent_kind",
			lines:   cat([]string{"[defaults]", `agent_kind = "claude"`, "[worker]", `agent = "agy"`}, src),
			wantErr: "worker.agent: expected one of worker.agent or defaults.agent_kind, found both",
		},
		{
			name:    "both worker.agent_args and defaults.agent_args",
			lines:   cat([]string{"[defaults]", `agent_args = ["--a"]`, "[worker]", `agent_args = ["--b"]`}, src),
			wantErr: "worker.agent_args: expected one of worker.agent_args or defaults.agent_args, found both",
		},
		{
			name:    "empty worker agent_args entry",
			lines:   cat([]string{"[worker]", `agent_args = ["--model", ""]`}, src),
			wantErr: "worker.agent_args: expected a non-empty argument, found empty",
		},
		{
			name:    "empty lane agent_args entry",
			lines:   cat(src, lane, []string{"[lane.worker]", `agent_args = [""]`}, order),
			wantErr: `lane "linux" worker.agent_args: expected a non-empty argument, found empty`,
		},
		{
			name:    "zero max_workers",
			lines:   cat([]string{"[worker]", "max_workers = 0"}, src),
			wantErr: "worker.max_workers: expected a positive integer, found 0",
		},
		{
			name:    "negative source max_workers",
			lines:   cat(src, []string{"[source.worker]", "max_workers = -2"}),
			wantErr: `source "experimental" worker.max_workers: expected a positive integer, found -2`,
		},
		{
			name:    "lane.worker max_workers is rejected in favor of lane.max_workers",
			lines:   cat(src, lane, []string{"[lane.worker]", "max_workers = 3"}, order),
			wantErr: "unknown key lane.worker.max_workers",
		},
		{
			name:    "unknown worker key",
			lines:   cat([]string{"[worker]", "threads = 4"}, src),
			wantErr: "unknown key worker.threads",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newFake(t).load(tomlDoc(tc.lines))
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

func boolPtr(v bool) *bool {
	return &v
}
