package tick

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
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
	if err := env.st.InsertClaim(context.Background(), store.Claim{
		Source:      "jira:acme",
		ID:          "PRJ-123",
		ClaimedAt:   env.now,
		Machine:     "test-host",
		State:       store.Started,
		UpdatedAt:   env.now,
		TabID:       "w1:t1",
		WorkspaceID: "w1",
		Worktree:    wantDir,
	}); err != nil {
		t.Fatal(err)
	}

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

func TestTickWorktreeSlotReuseAfterSettle(t *testing.T) {
	items := []source.Item{
		readyItem("PRJ-1", "First"),
		readyItem("PRJ-2", "Second"),
	}
	env := newTickEnv(t, items, true, true)
	repoDir := filepath.Join(t.TempDir(), "monorepo")
	env.eng.Config.Defaults.MaxInFlight = 2
	env.eng.Config.Defaults.MinStartInterval = 0
	env.eng.Config.Sources[0].Cwd = repoDir
	env.eng.Config.Sources[0].Worker = config.Worker{
		Spawner:    config.SpawnerSubagent,
		Agent:      "claude",
		MaxWorkers: 2,
		Worktree:   true,
	}
	env.eng.Agent = nil
	env.eng.WorktreeAdd = func(_ context.Context, _, worktreeDir, _, _ string) error {
		return os.MkdirAll(worktreeDir, 0o755)
	}
	env.eng.WorktreeRemove = func(_ context.Context, _, worktreeDir string) error {
		return os.RemoveAll(worktreeDir)
	}

	// First tick claims PRJ-1 -> slot-1 and PRJ-2 -> slot-2.
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	slot1Dir, _ := start.WorktreeSlotSpec(repoDir, "PRJ-1", 1)
	slot2Dir, _ := start.WorktreeSlotSpec(repoDir, "PRJ-2", 2)
	c1 := claimRow(t, env.st, "PRJ-1")
	c2 := claimRow(t, env.st, "PRJ-2")
	if c1.Worktree != slot1Dir || c2.Worktree != slot2Dir {
		t.Fatalf("worktrees = (%q, %q), want (%q, %q)", c1.Worktree, c2.Worktree, slot1Dir, slot2Dir)
	}

	// PRJ-1 merges cleanly; settle removes slot-1 while PRJ-2 remains in slot-2.
	env.pulls.merged = ghPR("[PRJ-1] First")
	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Lstat(slot1Dir); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("slot-1 still exists after clean settle: err=%v", err)
	}

	// Next tick starts PRJ-3 and reuses slot-1.
	env.pulls.merged = nil
	item3 := readyItem("PRJ-3", "Third")
	env.src.items["PRJ-3"] = item3
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	c3 := claimRow(t, env.st, "PRJ-3")
	if c3.Worktree != slot1Dir {
		t.Fatalf("PRJ-3 worktree = %q, want reused %q", c3.Worktree, slot1Dir)
	}
}
