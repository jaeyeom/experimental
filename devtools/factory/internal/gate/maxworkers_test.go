package gate

import (
	"context"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestGateMaxWorkers(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	baseInputs := func(claims []store.Claim) Inputs {
		return Inputs{
			Tabs: []herdr.Tab{
				{ID: "w1:t1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusWorking},
			},
			Panes: []herdr.Pane{
				{ID: "w1:p1", TabID: "w1:t1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/repo"},
			},
			GitStatus:    func(context.Context, string) (string, error) { return "", nil },
			ActiveClaims: claims,
			Now:          now,
			MaxWorkers:   3,
		}
	}
	d := config.Defaults{MaxInFlight: 5}

	res := Check(context.Background(), baseInputs([]store.Claim{
		{Source: "jira:acme", ID: "PRJ-1", State: store.Started, TabID: "w1:t1"},
		{Source: "jira:acme", ID: "PRJ-2", State: store.Started}, // subagent claim
	}), "/repo", d)
	if !res.OK {
		t.Fatalf("expected OK with 2/3 workers, got reasons: %v", res.Reasons)
	}

	res = Check(context.Background(), baseInputs([]store.Claim{
		{Source: "jira:acme", ID: "PRJ-1", State: store.Started, TabID: "w1:t1"},
		{Source: "jira:acme", ID: "PRJ-2", State: store.Started},
		{Source: "jira:acme", ID: "PRJ-3", State: store.Claimed},
	}), "/repo", d)
	if res.OK {
		t.Fatal("expected refusal with 3/3 workers")
	}
	if len(res.Reasons) != 1 || !strings.Contains(res.Reasons[0], "3 running workers (max_workers 3)") {
		t.Fatalf("reasons = %v", res.Reasons)
	}
}

func TestGitWorktreeAddAndRemove(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "worktree", "add", "-b", "factory/PRJ-1", "/repo-wt/PRJ-1", "main").
		WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "worktree", "remove", "/repo-wt/PRJ-1").
		WillSucceed("", 0).Once().Build()

	add := GitWorktreeAdd(mock)
	if err := add(context.Background(), "/repo", "/repo-wt/PRJ-1", "factory/PRJ-1", "main"); err != nil {
		t.Fatalf("GitWorktreeAdd: %v", err)
	}
	rm := GitWorktreeRemove(mock)
	if err := rm(context.Background(), "/repo", "/repo-wt/PRJ-1"); err != nil {
		t.Fatalf("GitWorktreeRemove: %v", err)
	}
}

func TestGateWorktreeIsolation(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	d := config.Defaults{MaxInFlight: 5}
	baseInputs := func(branch string) Inputs {
		return Inputs{
			Tabs: []herdr.Tab{
				{ID: "w1:t1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusWorking},
			},
			Panes: []herdr.Pane{
				{ID: "w1:p1", TabID: "w1:t1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/repo-wt/PRJ-1"},
			},
			GitStatus: func(_ context.Context, dir string) (string, error) {
				if dir == "/repo-wt/PRJ-1" {
					return " M dirty.go\n", nil
				}
				return "", nil
			},
			BaseBranch: "main",
			Require:    config.GateOnBase,
			GitBranch: func(_ context.Context, dir string) (string, error) {
				if dir == "/repo-wt/PRJ-1" {
					return "factory/PRJ-1", nil
				}
				return branch, nil
			},
			ActiveClaims: []store.Claim{
				{Source: "jira:acme", ID: "PRJ-1", State: store.Started, TabID: "w1:t1", Worktree: "/repo-wt/PRJ-1"},
			},
			Now:        now,
			MaxWorkers: 2,
			Worktree:   true,
		}
	}

	res := Check(context.Background(), baseInputs("main"), "/repo", d)
	if !res.OK {
		t.Fatalf("expected OK with dirty isolated worktree, got reasons: %v", res.Reasons)
	}

	res = Check(context.Background(), baseInputs("other"), "/repo", d)
	if res.OK {
		t.Fatal("expected refusal when main checkout is not on base branch")
	}
	if len(res.Reasons) != 1 || !strings.Contains(res.Reasons[0], "/repo: expected branch main, found other") {
		t.Fatalf("reasons = %v", res.Reasons)
	}
}
