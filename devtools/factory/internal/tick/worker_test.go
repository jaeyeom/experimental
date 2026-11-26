package tick

import (
	"context"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestTickMaxWorkersStartsMultiple(t *testing.T) {
	items := []source.Item{
		readyItem("PRJ-1", "First"),
		readyItem("PRJ-2", "Second"),
		readyItem("PRJ-3", "Third"),
	}
	env := newTickEnv(t, items, true, false)
	env.eng.Config.Defaults.MaxInFlight = 3
	env.eng.Config.Sources[0].Worker = config.Worker{
		Spawner:    config.SpawnerSubagent,
		Agent:      "claude",
		MaxWorkers: 3,
	}
	env.eng.Agent = nil

	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := len(env.src.claimIDs()); got != 3 {
		t.Fatalf("claimed %d items (%v), want 3", got, env.src.claimIDs())
	}
	out := env.out.String()
	for _, id := range []string{"PRJ-1", "PRJ-2", "PRJ-3"} {
		if !strings.Contains(out, "started "+id+" (subagent)") {
			t.Fatalf("output missing started %s (subagent):\n%s", id, out)
		}
	}
}

func TestSettleRemovesWorktree(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, true)
	env.eng.Config.Sources[0].Worker.Worktree = true
	wantDir, _ := start.WorktreeSpec(env.eng.Config.Sources[0].Cwd, "PRJ-123")
	var statusDir, removed string
	env.eng.Git = func(_ context.Context, dir string) (string, error) {
		statusDir = dir
		return "", nil
	}
	env.eng.WorktreeRemove = func(_ context.Context, _ string, worktreeDir string) error {
		removed = worktreeDir
		return nil
	}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-123] ship")
	seedClaim(t, env.st, "PRJ-123", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if statusDir != wantDir {
		t.Fatalf("Git status dir = %q, want %q", statusDir, wantDir)
	}
	if removed != wantDir {
		t.Fatalf("WorktreeRemove = %q, want %q", removed, wantDir)
	}
}

func TestSettlePreservesDirtyWorktree(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, true)
	wantDir, _ := start.WorktreeSpec(env.eng.Config.Sources[0].Cwd, "PRJ-123")
	env.eng.Git = func(_ context.Context, dir string) (string, error) {
		if dir != wantDir {
			t.Fatalf("Git status dir = %q, want %q", dir, wantDir)
		}
		return " M uncommitted.go\n", nil
	}
	removed := false
	env.eng.WorktreeRemove = func(_ context.Context, _ string, _ string) error {
		removed = true
		return nil
	}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-123] ship")
	c := store.Claim{
		Source:      "jira:acme",
		ID:          "PRJ-123",
		ClaimedAt:   env.now,
		Machine:     "host",
		State:       store.Started,
		UpdatedAt:   env.now,
		TabID:       "w1:t1",
		WorkspaceID: "w1",
		Worktree:    wantDir,
	}
	if err := env.st.InsertClaim(context.Background(), c); err != nil {
		t.Fatal(err)
	}

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if removed {
		t.Fatal("WorktreeRemove called on dirty worktree; want preserved")
	}
	out := env.out.String()
	if !strings.Contains(out, wantDir) || !strings.Contains(out, "uncommitted") {
		t.Fatalf("output missing dirty worktree warning naming %q:\n%s", wantDir, out)
	}
}

func TestSettleSubagentSkipsCloseTab(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, true)
	env.eng.Closer = nil
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-123] ship")
	c := store.Claim{
		Source:    "jira:acme",
		ID:        "PRJ-123",
		ClaimedAt: env.now,
		Machine:   "host",
		State:     store.Started,
		UpdatedAt: env.now,
		Spawner:   config.SpawnerSubagent,
	}
	if err := env.st.InsertClaim(context.Background(), c); err != nil {
		t.Fatal(err)
	}

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if len(env.herd.closed) != 0 {
		t.Fatalf("closed tabs = %v, want none", env.herd.closed)
	}
	got, err := env.st.LatestClaim(context.Background(), "jira:acme", "PRJ-123")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != store.Done {
		t.Fatalf("claim state = %q, want done", got.State)
	}
}
