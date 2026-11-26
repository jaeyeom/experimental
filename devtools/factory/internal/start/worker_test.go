package start

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestLeaseRestoreRule(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Lease = config.LeasePrompt{
		Mode:     config.LeaseModePrompt,
		ClaimFor: "30m",
		Restore:  true,
	}
	if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h.herdr.prompts != 1 {
		t.Fatalf("prompts = %d, want 1", h.herdr.prompts)
	}
	prompt := h.herdr.promptText
	if !strings.Contains(prompt, "agentbus lease claim --for 30m") {
		t.Fatalf("prompt missing lease claim:\n%s", prompt)
	}
	if !strings.Contains(prompt, "agentbus lease restore") {
		t.Fatalf("prompt missing lease restore:\n%s", prompt)
	}
}

func TestOutputModeMerge(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Output = config.Output{Mode: config.OutputModeMerge}
	if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	plan := h.out.String()
	if !strings.Contains(plan, "output   merge (exit worker on completion)\n") {
		t.Fatalf("plan missing output merge:\n%s", plan)
	}

	h2 := newHarness(t)
	h2.starter.Cfg.Output = config.Output{Mode: config.OutputModeMerge}
	if err := h2.starter.Start(context.Background(), "PRJ-123", h2.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h2.herdr.prompts != 1 {
		t.Fatalf("prompts = %d, want 1", h2.herdr.prompts)
	}
	if !strings.Contains(h2.herdr.promptText, "Output mode is merge: do not open a pull request. Rebase onto origin/main") {
		t.Fatalf("prompt missing merge instruction:\n%s", h2.herdr.promptText)
	}
	for _, want := range []string{
		"fetching origin/main",
		"no merge commits",
		"fast-forward main",
		"push to origin/main",
		"PRJ-123",
		"factory verify PRJ-123",
		"agentbus lease release",
		"factory settle PRJ-123",
	} {
		if !strings.Contains(h2.herdr.promptText, want) {
			t.Fatalf("prompt missing %q:\n%s", want, h2.herdr.promptText)
		}
	}
	cl, ok, err := h2.st.ActiveClaim(context.Background(), "jira:acme", "PRJ-123")
	if err != nil || !ok {
		t.Fatalf("ActiveClaim = %+v, %v, %v", cl, ok, err)
	}
	if cl.OutputMode != config.OutputModeMerge {
		t.Fatalf("claim OutputMode = %q, want %q", cl.OutputMode, config.OutputModeMerge)
	}
	if got := mergeOutputInstruction("main", "acme/widgets#42", config.SpawnerAgentbus, false); !strings.Contains(got, "Fixes #42") {
		t.Fatalf("github merge instruction missing Fixes #42: %q", got)
	}

	// Remote-lane tab worker closes its own tab immediately after reporting back.
	remotePrompt := mergeOutputInstruction("main", "PRJ-123", config.SpawnerAgentbus, true)
	if !strings.Contains(remotePrompt, `herdr tab close "$HERDR_TAB_ID"`) {
		t.Fatalf("remote merge instruction missing tab close: %q", remotePrompt)
	}
	if strings.Contains(remotePrompt, "factory settle") {
		t.Fatalf("remote merge instruction should not run factory settle: %q", remotePrompt)
	}

	// Subagent worker returns immediately so coordinator settles and discards it.
	subagentPrompt := mergeOutputInstruction("main", "PRJ-123", config.SpawnerSubagent, false)
	for _, want := range []string{"factory verify PRJ-123", "finish and return immediately", "coordinator will run `factory settle PRJ-123`"} {
		if !strings.Contains(subagentPrompt, want) {
			t.Fatalf("subagent merge instruction missing %q: %q", want, subagentPrompt)
		}
	}
}

func TestWorkerAgentAndArgsOverride(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Worker = config.Worker{
		Spawner:    config.SpawnerAgentbus,
		Agent:      "codex",
		AgentArgs:  []string{"--fast"},
		MaxWorkers: 1,
	}
	if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(h.out.String(), "agent    codex\n") {
		t.Fatalf("plan = %q, want agent codex", h.out.String())
	}
	if got := h.herdr.agentKind; got != "codex" {
		t.Fatalf("StartAgent kind = %q, want codex", got)
	}
	if got := h.herdr.agentArgs; len(got) != 1 || got[0] != "--fast" {
		t.Fatalf("StartAgent args = %v, want [--fast]", got)
	}
}

func TestSpawnerSubagent(t *testing.T) {
	t.Parallel()
	t.Run("apply_json", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Herdr = nil
		h.starter.Cfg.Worker = config.Worker{
			Spawner:    config.SpawnerSubagent,
			Agent:      "claude",
			AgentArgs:  []string{"--model", "sonnet"},
			MaxWorkers: 1,
		}
		if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, Options{Apply: true, JSON: true}); err != nil {
			t.Fatal(err)
		}
		var out Outcome
		if err := json.Unmarshal(h.out.Bytes(), &out); err != nil {
			t.Fatalf("decode outcome: %v (%q)", err, h.out.String())
		}
		if out.Status != OutcomeStarted || out.TabID != "" || out.Spawner != config.SpawnerSubagent {
			t.Fatalf("outcome = %+v", out)
		}
		if out.Source != "jira:acme" || out.Cwd != "~/src/monorepo" || out.Agent != "claude" {
			t.Fatalf("outcome metadata = %+v", out)
		}
		if len(out.AgentArgs) != 2 || out.AgentArgs[0] != "--model" || out.AgentArgs[1] != "sonnet" {
			t.Fatalf("agent_args = %v", out.AgentArgs)
		}
		if out.Env[config.ItemSourceEnv] != "jira:acme" || out.Env[config.OutputModeEnv] != config.OutputModePR {
			t.Fatalf("env = %v", out.Env)
		}
		if out.OutputMode != config.OutputModePR || out.KeepWorker == nil || !*out.KeepWorker {
			t.Fatalf("output lifecycle = mode=%q keep=%v", out.OutputMode, out.KeepWorker)
		}
		if !strings.Contains(out.Brief, "PRJ-123 Ship it") {
			t.Fatalf("brief = %q", out.Brief)
		}
		cl, ok, err := h.st.ActiveClaim(context.Background(), "jira:acme", "PRJ-123")
		if err != nil || !ok {
			t.Fatalf("ActiveClaim = %+v, %v, %v", cl, ok, err)
		}
		if cl.State != store.Started || cl.TabID != "" || cl.Spawner != config.SpawnerSubagent || cl.OutputMode != config.OutputModePR {
			t.Fatalf("claim = %+v", cl)
		}
	})

	t.Run("apply_text_and_worktree", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Herdr = nil
		h.starter.Cfg.Worker = config.Worker{
			Spawner:    config.SpawnerSubagent,
			Agent:      "codex",
			AgentArgs:  []string{"--fast"},
			MaxWorkers: 1,
			Worktree:   true,
		}
		h.starter.WorktreeAdd = func(context.Context, string, string, string, string) error { return nil }
		if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		wantDir, _ := WorktreeSpec("~/src/monorepo", "PRJ-123")
		got := h.out.String()
		for _, want := range []string{
			"--- subagent brief ---",
			"id: PRJ-123",
			"source: jira:acme",
			"output: pr (keep subagent for review feedback until settled)",
			"env: FACTORY_ITEM_SOURCE=jira:acme FACTORY_OUTPUT_MODE=pr",
			"cwd: " + wantDir,
			"agent: codex",
			"agent_args: --fast",
			"PRJ-123 Ship it",
			"--- end brief ---",
			"factory start PRJ-123: started (subagent, claim ",
		} {
			if !strings.Contains(got, want) {
				t.Fatalf("output missing %q:\n%s", want, got)
			}
		}
		cl, ok, err := h.st.ActiveClaim(context.Background(), "jira:acme", "PRJ-123")
		if err != nil || !ok {
			t.Fatalf("ActiveClaim = %+v, %v, %v", cl, ok, err)
		}
		if cl.Spawner != config.SpawnerSubagent || cl.Worktree != wantDir {
			t.Fatalf("claim = %+v, want spawner=subagent worktree=%q", cl, wantDir)
		}
	})

	t.Run("merge_mode_text_and_json", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Herdr = nil
		h.starter.Cfg.Worker = config.Worker{
			Spawner:    config.SpawnerSubagent,
			Agent:      "claude",
			MaxWorkers: 1,
		}
		h.starter.Cfg.Output = config.Output{Mode: config.OutputModeMerge}
		if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		got := h.out.String()
		for _, want := range []string{
			"output: merge (exit on completion; do not keep subagent)",
			"env: FACTORY_ITEM_SOURCE=jira:acme FACTORY_OUTPUT_MODE=merge",
			"finish and return immediately; do not wait for review comments",
		} {
			if !strings.Contains(got, want) {
				t.Fatalf("merge subagent output missing %q:\n%s", want, got)
			}
		}

		h2 := newHarness(t)
		h2.starter.Herdr = nil
		h2.starter.Cfg.Worker = config.Worker{
			Spawner:    config.SpawnerSubagent,
			Agent:      "claude",
			MaxWorkers: 1,
		}
		h2.starter.Cfg.Output = config.Output{Mode: config.OutputModeMerge}
		if err := h2.starter.Start(context.Background(), "PRJ-123", h2.snap, okGate, Options{Apply: true, JSON: true}); err != nil {
			t.Fatal(err)
		}
		var out Outcome
		if err := json.Unmarshal(h2.out.Bytes(), &out); err != nil {
			t.Fatalf("decode outcome: %v (%q)", err, h2.out.String())
		}
		if out.OutputMode != config.OutputModeMerge || out.KeepWorker == nil || *out.KeepWorker {
			t.Fatalf("outcome lifecycle = mode=%q keep=%v", out.OutputMode, out.KeepWorker)
		}
		if out.Env[config.OutputModeEnv] != config.OutputModeMerge {
			t.Fatalf("env = %v", out.Env)
		}
	})

	t.Run("dry_run_no_claim", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Herdr = nil
		h.starter.Cfg.Worker = config.Worker{
			Spawner:    config.SpawnerSubagent,
			Agent:      "claude",
			MaxWorkers: 1,
		}
		if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, Options{}); err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(h.out.String(), "spawner  subagent (cwd ~/src/monorepo)") {
			t.Fatalf("dry-run plan = %q", h.out.String())
		}
		if _, ok, err := h.st.ActiveClaim(context.Background(), "jira:acme", "PRJ-123"); err != nil || ok {
			t.Fatalf("expected no claim on dry-run, got ok=%v err=%v", ok, err)
		}
	})
}

func TestSpawnerHerdrSkipsWake(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	wokeCalled := false
	h.starter.Wake = func(context.Context, string, string) (bool, error) {
		wokeCalled = true
		return true, nil
	}
	h.starter.Cfg.Worker = config.Worker{
		Spawner:    config.SpawnerHerdr,
		Agent:      "claude",
		MaxWorkers: 1,
	}
	if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if wokeCalled {
		t.Fatal("Wake was called for spawner=herdr; want skipped")
	}
}

func TestWorktreeIsolation(t *testing.T) {
	t.Parallel()
	t.Run("dry_run_shows_planned_worktree", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.Worker = config.Worker{
			Spawner:    config.SpawnerAgentbus,
			Agent:      "claude",
			MaxWorkers: 1,
			Worktree:   true,
		}
		added := false
		h.starter.WorktreeAdd = func(context.Context, string, string, string, string) error {
			added = true
			return nil
		}
		if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, Options{}); err != nil {
			t.Fatal(err)
		}
		if added {
			t.Fatal("WorktreeAdd called during dry-run")
		}
		wantDir, wantBranch := WorktreeSpec("~/src/monorepo", "PRJ-123")
		wantLine := "worktree " + wantDir + " (branch " + wantBranch + ")\n"
		if !strings.Contains(h.out.String(), wantLine) {
			t.Fatalf("dry-run plan missing %q:\n%s", wantLine, h.out.String())
		}
	})

	t.Run("apply_creates_and_uses_worktree", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		var added [4]string
		h.starter.Cfg.Worker = config.Worker{
			Spawner:    config.SpawnerAgentbus,
			Agent:      "claude",
			MaxWorkers: 1,
			Worktree:   true,
		}
		h.starter.WorktreeAdd = func(_ context.Context, repoDir, worktreeDir, branch, base string) error {
			added = [4]string{repoDir, worktreeDir, branch, base}
			return nil
		}
		if err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		wantDir, wantBranch := WorktreeSpec("~/src/monorepo", "PRJ-123")
		if added[0] != "~/src/monorepo" || added[1] != wantDir || added[2] != wantBranch || added[3] != "main" {
			t.Fatalf("WorktreeAdd = %v, want [~/src/monorepo %s %s main]", added, wantDir, wantBranch)
		}
		if h.herdr.cwd != wantDir {
			t.Fatalf("CreateTab cwd = %q, want %q", h.herdr.cwd, wantDir)
		}
	})
}

func TestWorktreeCleanedUpOnFailedAgentStart(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.Worker = config.Worker{
		Spawner:    config.SpawnerAgentbus,
		Agent:      "claude",
		MaxWorkers: 1,
		Worktree:   true,
	}
	h.starter.WorktreeAdd = func(context.Context, string, string, string, string) error { return nil }
	var removed string
	h.starter.WorktreeRemove = func(_ context.Context, _ string, worktreeDir string) error {
		removed = worktreeDir
		return nil
	}
	h.herdr.startErr = errors.New("boom")
	err := h.starter.Start(context.Background(), "PRJ-123", h.snap, okGate, applyOpt)
	if err == nil {
		t.Fatal("expected error")
	}
	wantDir, _ := WorktreeSpec("~/src/monorepo", "PRJ-123")
	if removed != wantDir {
		t.Fatalf("WorktreeRemove = %q, want %q", removed, wantDir)
	}
}
