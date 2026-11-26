package gate

import (
	"bytes"
	"context"
	"errors"
	"log/slog"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestCheck(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	const cwd = "/src"
	tests := []struct {
		name    string
		nilCtx  bool
		skipGit bool
		edit    func(*Inputs, *string, *config.Defaults)
		git     func(context.Context, string) (string, error)
		want    []string
	}{
		{name: "agent_pass"},
		{
			name: "agent_fail",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking}}
				in.Panes = []herdr.Pane{{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"}}
			},
			want: []string{"agent PRJ-2 (w1:t2) is working"},
		},
		{
			name: "every_working_tab",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{
					{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking},
					{ID: "w1:t3", Label: "PRJ-3", AgentStatus: herdr.AgentStatusWorking},
				}
				in.Panes = []herdr.Pane{
					{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
					{ID: "w1:p3", TabID: "w1:t3", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src/pkg"},
				}
			},
			want: []string{
				"agent PRJ-2 (w1:t2) is working",
				"agent PRJ-3 (w1:t3) is working",
			},
		},
		{
			name: "caller_only_passes",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{
					ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
				}}
				in.Panes = []herdr.Pane{{
					ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking,
				}}
				in.CallerPaneID = "w1:p1"
			},
		},
		{
			name: "other_pane_still_refuses",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{
					{ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusWorking},
					{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking},
				}
				in.Panes = []herdr.Pane{
					{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
					{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
				}
				in.CallerPaneID = "w1:p1"
			},
			want: []string{"agent PRJ-2 (w1:t2) is working"},
		},
		{
			name: "same_tab_marks_this_pane",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{
					ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusWorking, PaneCount: 2,
				}}
				in.Panes = []herdr.Pane{
					{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
					{ID: "w1:p2", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
				}
				in.CallerPaneID = "w1:p1"
			},
			want: []string{"agent coord (w1:t1) is working (this pane)"},
		},
		{
			name: "every_counted_tab",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{
					{ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusWorking},
					{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking},
				}
				in.Panes = []herdr.Pane{
					{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
					{ID: "w1:p1b", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
					{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
				}
				in.CallerPaneID = "w1:p1"
			},
			want: []string{
				"agent coord (w1:t1) is working (this pane)",
				"agent PRJ-2 (w1:t2) is working",
			},
		},
		{
			name: "ignore_pane",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{
					ID: "w1:t2", Label: "worker", AgentStatus: herdr.AgentStatusWorking,
				}}
				in.Panes = []herdr.Pane{{
					ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking,
				}}
				in.IgnorePanes = []string{"w1:p2"}
			},
		},
		{
			name: "ignore_tab",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{
					ID: "w1:t8", Label: "dashboard", AgentStatus: herdr.AgentStatusWorking,
				}}
				in.Panes = []herdr.Pane{{
					ID: "w1:p8", TabID: "w1:t8", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking,
				}}
				in.IgnoreTabs = []string{"dashboard"}
			},
		},
		{
			name: "ignore_leaves_other_pane",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{
					{ID: "w1:t8", Label: "dashboard", AgentStatus: herdr.AgentStatusWorking},
					{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking},
				}
				in.Panes = []herdr.Pane{
					{ID: "w1:p8", TabID: "w1:t8", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
					{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"},
				}
				in.IgnoreTabs = []string{"dashboard"}
				in.IgnorePanes = []string{"w1:p8"}
			},
			want: []string{"agent PRJ-2 (w1:t2) is working"},
		},
		{
			name: "caller_still_counted",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{
					ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
				}}
				in.Panes = []herdr.Pane{{
					ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusIdle, Cwd: "/src",
				}}
				in.CallerPaneID = "w1:p1"
			},
			want: []string{"agent coord (w1:t1) is working (this pane)"},
		},
		{
			name: "pane_working_tab_idle",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{
					ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusIdle,
				}}
				in.Panes = []herdr.Pane{{
					ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src",
				}}
			},
			want: []string{"agent PRJ-2 (w1:t2) is working"},
		},
		{
			name: "empty_pane_id",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Panes = []herdr.Pane{{
					ID: "", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking,
				}}
			},
			want: []string{"panes: expected a non-empty pane id, found empty"},
		},
		{
			name: "empty_ignore_pane",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.IgnorePanes = []string{""}
			},
			want: []string{"gate.ignore_panes: expected a non-empty pane id, found empty"},
		},
		{
			name: "empty_ignore_tab",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.IgnoreTabs = []string{""}
			},
			want: []string{"gate.ignore_tabs: expected a non-empty tab label, found empty"},
		},
		{
			name: "blocked_agent_is_not_working",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{ID: "w1:t4", Label: "PRJ-4", AgentStatus: herdr.AgentStatusBlocked}}
			},
		},
		{name: "checkout_pass", git: func(context.Context, string) (string, error) { return "\n", nil }},
		{
			name: "checkout_fail",
			git: func(context.Context, string) (string, error) {
				return " M a.go\n?? b.go\n", nil
			},
			want: []string{"/src has uncommitted changes (2 files)"},
		},
		{
			name: "checkout_counts_rename_as_one_file",
			git: func(context.Context, string) (string, error) {
				return " M a.go\nR  old.go -> new.go\n?? c.go\n", nil
			},
			want: []string{"/src has uncommitted changes (3 files)"},
		},
		{
			name: "blocked_agent_dirty_tree",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.Tabs = []herdr.Tab{{ID: "w1:t4", Label: "PRJ-4", AgentStatus: herdr.AgentStatusBlocked}}
			},
			git: func(context.Context, string) (string, error) {
				return " M a.go\n", nil
			},
			want: []string{"/src has uncommitted changes (1 files)"},
		},
		{
			name: "wip_pass",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.ActiveClaims = make([]store.Claim, 2)
			},
		},
		{
			name: "wip_fail",
			edit: func(in *Inputs, _ *string, d *config.Defaults) {
				in.ActiveClaims = make([]store.Claim, 3)
				d.MaxInFlight = 3
			},
			want: []string{"in flight: 3 (claims 3, PRs 0, overlap 0) of max 3"},
		},
		{
			name: "wip_zero_max",
			edit: func(_ *Inputs, _ *string, d *config.Defaults) {
				d.MaxInFlight = 0
			},
			want: []string{"in flight: 0 (claims 0, PRs 0, overlap 0) of max 0"},
		},
		{
			name: "rate_pass",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.LastStartedAt = now.Add(-10 * time.Minute)
			},
		},
		{
			name: "rate_fail",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.LastStartedAt = now.Add(-9 * time.Minute)
			},
			want: []string{"last start 9m0s ago (min 10m0s)"},
		},
		{
			name: "rate_zero_last_start_passes",
			edit: func(in *Inputs, _ *string, _ *config.Defaults) {
				in.LastStartedAt = time.Time{}
			},
		},
		{name: "nil_git_status", skipGit: true, want: []string{"git status: expected a function, found nil"}},
		{
			name: "empty_cwd",
			edit: func(_ *Inputs, dir *string, _ *config.Defaults) {
				*dir = ""
			},
			want: []string{"cwd: expected a non-empty path, found empty"},
		},
		{name: "nil_context", nilCtx: true, want: []string{"git status: expected a context, found nil"}},
		{
			name: "git_error",
			git: func(context.Context, string) (string, error) {
				return " M ignored.go\n", errors.New("fatal: not a git repository")
			},
			want: []string{"fatal: not a git repository"},
		},
		{
			name: "git_error_empty_text",
			git: func(context.Context, string) (string, error) {
				return "", errors.New("")
			},
			want: []string{"git status: expected an error text, found empty"},
		},
		{
			name: "all_four",
			edit: func(in *Inputs, _ *string, d *config.Defaults) {
				in.Tabs = []herdr.Tab{{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking}}
				in.Panes = []herdr.Pane{{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"}}
				in.ActiveClaims = make([]store.Claim, 2)
				in.LastStartedAt = now.Add(-time.Minute)
				d.MaxInFlight = 2
			},
			git: func(context.Context, string) (string, error) {
				return " M a.go\n?? b.go\n", nil
			},
			want: []string{
				"agent PRJ-2 (w1:t2) is working",
				"/src has uncommitted changes (2 files)",
				"in flight: 2 (claims 2, PRs 0, overlap 0) of max 2",
				"last start 1m0s ago (min 10m0s)",
			},
		},
		{
			name: "git_error_with_other_failures",
			edit: func(in *Inputs, _ *string, d *config.Defaults) {
				in.Tabs = []herdr.Tab{{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking}}
				in.Panes = []herdr.Pane{{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"}}
				in.ActiveClaims = make([]store.Claim, 1)
				in.LastStartedAt = now.Add(-time.Minute)
				d.MaxInFlight = 1
			},
			git: func(context.Context, string) (string, error) {
				return " M a.go\n", errors.New("fatal: not a git repository")
			},
			want: []string{
				"agent PRJ-2 (w1:t2) is working",
				"fatal: not a git repository",
				"in flight: 1 (claims 1, PRs 0, overlap 0) of max 1",
				"last start 1m0s ago (min 10m0s)",
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			in := Inputs{
				Tabs:          []herdr.Tab{{ID: "w1:t1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusIdle}},
				LastStartedAt: now.Add(-time.Hour),
				Now:           now,
			}
			dir := cwd
			d := config.Defaults{MaxInFlight: 3, MinStartInterval: 10 * time.Minute}
			if tc.edit != nil {
				tc.edit(&in, &dir, &d)
			}
			called := false
			in.GitBranch = func(context.Context, string) (string, error) {
				t.Errorf("GitBranch called")
				return "", errors.New("GitBranch called")
			}
			in.GitAhead = func(context.Context, string, string) (int, error) {
				t.Errorf("GitAhead called")
				return 0, errors.New("GitAhead called")
			}
			if !tc.skipGit {
				in.GitStatus = func(ctx context.Context, got string) (string, error) {
					called = true
					if ctx == nil {
						t.Fatal("GitStatus context is nil")
					}
					if got != dir {
						t.Fatalf("GitStatus dir = %q, want %q", got, dir)
					}
					if tc.git != nil {
						return tc.git(ctx, got)
					}
					return "", nil
				}
			}
			ctx := context.Background()
			if tc.nilCtx {
				ctx = nil
			}
			got := Check(ctx, in, dir, d)
			if got.OK != (len(tc.want) == 0) || !slices.Equal(got.Reasons, tc.want) {
				t.Fatalf("Check() = %+v, want reasons %q", got, tc.want)
			}
			wantCall := !tc.skipGit && !tc.nilCtx && dir != ""
			if called != wantCall {
				t.Fatalf("GitStatus called = %v, want %v", called, wantCall)
			}
		})
	}
}

func TestInFlightIncludesOpenPRs(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	check := func(t *testing.T, claims []store.Claim, prs []gh.PR, limit int) Result {
		t.Helper()
		in := Inputs{
			Tabs:          []herdr.Tab{{ID: "w1:t1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusIdle}},
			ActiveClaims:  claims,
			Pulls:         prs,
			LastStartedAt: now.Add(-time.Hour),
			Now:           now,
			GitStatus:     func(context.Context, string) (string, error) { return "", nil },
			PRTitlePattern: func(string) (string, error) {
				return `\[{id}\]`, nil
			},
		}
		d := config.Defaults{MaxInFlight: limit, MinStartInterval: 10 * time.Minute}
		return Check(context.Background(), in, "/src", d)
	}
	unrelated := gh.PR{Repo: "acme/widgets", Number: 9, URL: "https://github.com/acme/widgets/pull/9", Title: "review me", Head: "other"}
	tests := []struct {
		name   string
		claims []store.Claim
		prs    []gh.PR
		limit  int
		want   []string
		flight Flight
	}{
		{
			name:   "unmatched claim plus open pr",
			claims: []store.Claim{{ID: "PRJ-1", Source: "jira:acme"}},
			prs:    []gh.PR{unrelated},
			limit:  2,
			want:   []string{"in flight: 2 (claims 1, PRs 1, overlap 0) of max 2"},
			flight: Flight{InFlight: 2, Claims: 1, OpenPRs: 1},
		},
		{
			name:   "under the limit",
			claims: []store.Claim{{ID: "PRJ-1", Source: "jira:acme"}},
			prs:    []gh.PR{unrelated},
			limit:  3,
			flight: Flight{InFlight: 2, Claims: 1, OpenPRs: 1},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := check(t, tc.claims, tc.prs, tc.limit)
			if got.OK != (len(tc.want) == 0) || !slices.Equal(got.Reasons, tc.want) {
				t.Fatalf("Check() = %+v, want reasons %q", got, tc.want)
			}
			if got.InFlight != tc.flight.InFlight || got.Claims != tc.flight.Claims || got.OpenPRs != tc.flight.OpenPRs || got.Overlap != tc.flight.Overlap {
				t.Fatalf("flight = %+v, want %+v", Flight{got.InFlight, got.Claims, got.OpenPRs, got.Overlap}, tc.flight)
			}
		})
	}
}

func TestGitStashCount(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "stash", "list").
		WillSucceed("stash@{0}: one\nstash@{1}: two\n", 0).Once().Build()
	got, err := GitStashCount(mock)(context.Background(), "/repo")
	if err != nil {
		t.Fatal(err)
	}
	if got != 2 {
		t.Fatalf("stash count = %d", got)
	}
	mock = executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "stash", "list").
		WillSucceed("", 0).Once().Build()
	got, err = GitStashCount(mock)(context.Background(), "/repo")
	if err != nil || got != 0 {
		t.Fatalf("empty stash = %d, %v", got, err)
	}
}

func TestGitStatus(t *testing.T) {
	t.Run("porcelain", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "status", "--porcelain").
			WillSucceed(" M a.go\n", 0).Once().Build()
		got, err := GitStatus(mock)(context.Background(), "/repo")
		if err != nil {
			t.Fatalf("GitStatus: %v", err)
		}
		if got != " M a.go\n" {
			t.Fatalf("GitStatus = %q", got)
		}
		assertGitStatusCall(t, mock, "/repo")
	})
	t.Run("clean", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "status", "--porcelain").
			WillSucceed("", 0).Once().Build()
		got, err := GitStatus(mock)(context.Background(), "/repo")
		if err != nil || got != "" {
			t.Fatalf("GitStatus = %q, %v", got, err)
		}
		assertGitStatusCall(t, mock, "/repo")
	})
	t.Run("not_a_repo", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "status", "--porcelain").
			WillFail("fatal: not a git repository\n", 128).Once().Build()
		_, err := GitStatus(mock)(context.Background(), "/repo")
		if err == nil || err.Error() != "git status: fatal: not a git repository" {
			t.Fatalf("GitStatus error = %v", err)
		}
		assertGitStatusCall(t, mock, "/repo")
	})
	t.Run("empty_failure", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "status", "--porcelain").
			WillFail("", 128).Once().Build()
		_, err := GitStatus(mock)(context.Background(), "/repo")
		if err == nil || err.Error() != "git status: expected success, found exit 128" {
			t.Fatalf("GitStatus error = %v", err)
		}
	})
	t.Run("executor_error", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "status", "--porcelain").
			WillError(&executor.ExecutableNotFoundError{Command: "git"}).Once().Build()
		_, err := GitStatus(mock)(context.Background(), "/repo")
		if err == nil || err.Error() != "git status: executable not found: git" {
			t.Fatalf("GitStatus error = %v", err)
		}
	})
	t.Run("nil_result", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "status", "--porcelain").
			WillReturn(nil, nil).Once().Build()
		_, err := GitStatus(mock)(context.Background(), "/repo")
		if err == nil || err.Error() != "git status: expected an execution result, found nil" {
			t.Fatalf("GitStatus error = %v", err)
		}
	})
	t.Run("nil_executor", func(t *testing.T) {
		_, err := GitStatus(nil)(context.Background(), "/repo")
		if err == nil || err.Error() != "git status: expected an executor, found nil" {
			t.Fatalf("GitStatus error = %v", err)
		}
	})
	t.Run("empty_dir", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		_, err := GitStatus(mock)(context.Background(), "")
		if err == nil || err.Error() != "git status: expected a directory, found empty" {
			t.Fatalf("GitStatus error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
}

func assertGitStatusCall(t *testing.T, mock *executor.MockExecutor, dir string) {
	t.Helper()
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	calls := mock.Executions()
	if len(calls) != 1 {
		t.Fatalf("executions = %d, want 1", len(calls))
	}
	call := calls[0]
	if call.Command != "git" {
		t.Fatalf("command = %q", call.Command)
	}
	wantArgs := []string{"-C", dir, "status", "--porcelain"}
	if !slices.Equal(call.Args, wantArgs) {
		t.Fatalf("args = %#v, want %#v", call.Args, wantArgs)
	}
	if call.WorkingDir != "" {
		t.Fatalf("WorkingDir = %q, want empty", call.WorkingDir)
	}
	if call.Timeout != gitCommandTimeout {
		t.Fatalf("Timeout = %s, want %s", call.Timeout, gitCommandTimeout)
	}
}

func TestCheckBaseBranch(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	const cwd = "/src"
	tests := []struct {
		name       string
		nilCtx     bool
		emptyDir   bool
		base       string
		require    string
		branch     string
		branchErr  error
		nilBranch  bool
		ahead      int
		aheadErr   error
		nilAhead   bool
		dirty      string
		want       []string
		wantBranch bool
		wantAhead  bool
	}{
		{
			name: "on_base_match", base: "main", require: config.GateOnBase,
			branch: "main", wantBranch: true,
		},
		{
			name: "on_base_other", base: "main", require: config.GateOnBase,
			branch: "feature", wantBranch: true,
			want: []string{"/src: expected branch main, found feature"},
		},
		{
			name: "on_base_detached", base: "main", require: config.GateOnBase,
			branch: "HEAD", wantBranch: true,
			want: []string{"/src: expected branch main, found detached HEAD"},
		},
		{
			name: "clean_zero_on_other_branch", base: "main", require: config.GateCleanAgainstBase,
			branch: "feature", wantBranch: true, wantAhead: true,
		},
		{
			name: "clean_ahead", base: "main", require: config.GateCleanAgainstBase,
			branch: "feature", ahead: 2, wantBranch: true, wantAhead: true,
			want: []string{"/src: expected no commits beyond origin/main, found 2 on feature"},
		},
		{
			name: "clean_ahead_on_same_name", base: "main", require: config.GateCleanAgainstBase,
			branch: "main", ahead: 1, wantBranch: true, wantAhead: true,
			want: []string{"/src: expected no commits beyond origin/main, found 1 on main"},
		},
		{
			name: "clean_detached_ahead", base: "main", require: config.GateCleanAgainstBase,
			branch: "HEAD", ahead: 4, wantBranch: true, wantAhead: true,
			want: []string{"/src: expected no commits beyond origin/main, found 4 on detached HEAD"},
		},
		{
			name: "dirty_and_wrong_branch", base: "main", require: config.GateOnBase,
			branch: "feature", dirty: " M a.go\n", wantBranch: true,
			want: []string{
				"/src has uncommitted changes (1 files)",
				"/src: expected branch main, found feature",
			},
		},
		{
			name: "unknown_require", base: "main", require: "maybe",
			want: []string{`gate.require: expected on_base or clean_against_base, found "maybe"`},
		},
		{
			name: "missing_require", base: "main",
			want: []string{"gate.require: expected on_base or clean_against_base, found empty"},
		},
		{
			name: "nil_branch_func", base: "main", require: config.GateOnBase, nilBranch: true,
			want: []string{"git rev-parse: expected a function, found nil"},
		},
		{
			name: "empty_branch_name", base: "main", require: config.GateOnBase, wantBranch: true,
			want: []string{"git rev-parse: expected a branch name, found empty"},
		},
		{
			name: "branch_error", base: "main", require: config.GateCleanAgainstBase,
			branchErr: errors.New("git rev-parse: fatal: not a git repository"), wantBranch: true,
			want: []string{"git rev-parse: fatal: not a git repository"},
		},
		{
			name: "branch_error_empty_text", base: "main", require: config.GateOnBase,
			branchErr: errors.New(""), wantBranch: true,
			want: []string{"git rev-parse: expected an error text, found empty"},
		},
		{
			name: "nil_ahead_func", base: "main", require: config.GateCleanAgainstBase,
			branch: "main", nilAhead: true, wantBranch: true,
			want: []string{"git rev-list: expected a function, found nil"},
		},
		{
			name: "ahead_error", base: "main", require: config.GateCleanAgainstBase,
			branch: "main", aheadErr: errors.New("git rev-list: fatal: unknown revision"),
			wantBranch: true, wantAhead: true,
			want: []string{"git rev-list: fatal: unknown revision"},
		},
		{
			name: "ahead_error_empty_text", base: "main", require: config.GateCleanAgainstBase,
			branch: "main", aheadErr: errors.New(""), wantBranch: true, wantAhead: true,
			want: []string{"git rev-list: expected an error text, found empty"},
		},
		{
			name: "negative_ahead", base: "main", require: config.GateCleanAgainstBase,
			branch: "main", ahead: -1, wantBranch: true, wantAhead: true,
			want: []string{"git rev-list: expected a non-negative count, found -1"},
		},
		{
			name: "nil_context", base: "main", require: config.GateOnBase, nilCtx: true,
			want: []string{
				"git status: expected a context, found nil",
				"git rev-parse: expected a context, found nil",
			},
		},
		{
			name: "empty_cwd", base: "main", require: config.GateOnBase, emptyDir: true,
			want: []string{"cwd: expected a non-empty path, found empty"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			dir := cwd
			if tc.emptyDir {
				dir = ""
			}
			statusCalls, branchCalls, aheadCalls := 0, 0, 0
			in := Inputs{
				Tabs:          []herdr.Tab{{ID: "w1:t1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusIdle}},
				LastStartedAt: now.Add(-time.Hour),
				Now:           now,
				BaseBranch:    tc.base,
				Require:       tc.require,
				GitStatus: func(context.Context, string) (string, error) {
					statusCalls++
					return tc.dirty, nil
				},
			}
			if !tc.nilBranch {
				in.GitBranch = func(_ context.Context, got string) (string, error) {
					branchCalls++
					if got != dir {
						t.Fatalf("GitBranch dir = %q, want %q", got, dir)
					}
					if tc.branchErr != nil {
						return "", tc.branchErr
					}
					return tc.branch, nil
				}
			}
			if !tc.nilAhead {
				in.GitAhead = func(_ context.Context, got, base string) (int, error) {
					aheadCalls++
					if got != dir || base != tc.base {
						t.Fatalf("GitAhead dir = %q base = %q", got, base)
					}
					if tc.aheadErr != nil {
						return 0, tc.aheadErr
					}
					return tc.ahead, nil
				}
			}
			ctx := context.Background()
			if tc.nilCtx {
				ctx = nil
			}
			got := Check(ctx, in, dir, config.Defaults{MaxInFlight: 3, MinStartInterval: 10 * time.Minute})
			if got.OK != (len(tc.want) == 0) || !slices.Equal(got.Reasons, tc.want) {
				t.Fatalf("Check() = %+v, want reasons %q", got, tc.want)
			}
			wantStatus := !tc.nilCtx && dir != ""
			if statusCalls != boolCount(wantStatus) || branchCalls != boolCount(tc.wantBranch) || aheadCalls != boolCount(tc.wantAhead) {
				t.Fatalf("calls status=%d branch=%d ahead=%d, want %d %d %d", statusCalls, branchCalls, aheadCalls, boolCount(wantStatus), boolCount(tc.wantBranch), boolCount(tc.wantAhead))
			}
		})
	}
}

func boolCount(ok bool) int {
	if ok {
		return 1
	}
	return 0
}

func TestGitBranch(t *testing.T) {
	t.Run("name", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-parse", "--abbrev-ref", "HEAD").
			WillSucceed("feature\n", 0).Once().Build()
		got, err := GitBranch(mock)(context.Background(), "/repo")
		if err != nil || got != "feature" {
			t.Fatalf("GitBranch = %q, %v", got, err)
		}
		assertGitCommand(t, mock, "/repo", "rev-parse", "--abbrev-ref", "HEAD")
	})
	t.Run("detached", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-parse", "--abbrev-ref", "HEAD").
			WillSucceed("HEAD\n", 0).Once().Build()
		got, err := GitBranch(mock)(context.Background(), "/repo")
		if err != nil || got != "HEAD" {
			t.Fatalf("GitBranch = %q, %v", got, err)
		}
	})
	t.Run("blank", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-parse", "--abbrev-ref", "HEAD").
			WillSucceed(" \n", 0).Once().Build()
		_, err := GitBranch(mock)(context.Background(), "/repo")
		if err == nil || err.Error() != `git rev-parse: expected one branch name, found " \n"` {
			t.Fatalf("GitBranch error = %v", err)
		}
	})
	t.Run("not_a_repo", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-parse", "--abbrev-ref", "HEAD").
			WillFail("fatal: not a git repository\n", 128).Once().Build()
		_, err := GitBranch(mock)(context.Background(), "/repo")
		if err == nil || err.Error() != "git rev-parse: fatal: not a git repository" {
			t.Fatalf("GitBranch error = %v", err)
		}
	})
	t.Run("nil_executor", func(t *testing.T) {
		_, err := GitBranch(nil)(context.Background(), "/repo")
		if err == nil || err.Error() != "git rev-parse: expected an executor, found nil" {
			t.Fatalf("GitBranch error = %v", err)
		}
	})
	t.Run("empty_dir", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		_, err := GitBranch(mock)(context.Background(), "")
		if err == nil || err.Error() != "git rev-parse: expected a directory, found empty" {
			t.Fatalf("GitBranch error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
}

func TestGitAhead(t *testing.T) {
	t.Run("zero", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-list", "--count", "origin/main..HEAD").
			WillSucceed("0\n", 0).Once().Build()
		got, err := GitAhead(mock)(context.Background(), "/repo", "main")
		if err != nil || got != 0 {
			t.Fatalf("GitAhead = %d, %v", got, err)
		}
		assertGitCommand(t, mock, "/repo", "rev-list", "--count", "origin/main..HEAD")
	})
	t.Run("ahead", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-list", "--count", "origin/main..HEAD").
			WillSucceed("3\n", 0).Once().Build()
		got, err := GitAhead(mock)(context.Background(), "/repo", "main")
		if err != nil || got != 3 {
			t.Fatalf("GitAhead = %d, %v", got, err)
		}
	})
	t.Run("not_an_integer", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-list", "--count", "origin/main..HEAD").
			WillSucceed("nope\n", 0).Once().Build()
		_, err := GitAhead(mock)(context.Background(), "/repo", "main")
		if err == nil || err.Error() != `git rev-list: expected an integer, found "nope\n"` {
			t.Fatalf("GitAhead error = %v", err)
		}
	})
	t.Run("negative", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-list", "--count", "origin/main..HEAD").
			WillSucceed("-1\n", 0).Once().Build()
		_, err := GitAhead(mock)(context.Background(), "/repo", "main")
		if err == nil || err.Error() != "git rev-list: expected a non-negative count, found -1" {
			t.Fatalf("GitAhead error = %v", err)
		}
	})
	t.Run("missing_ref", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "rev-list", "--count", "origin/main..HEAD").
			WillFail("fatal: ambiguous argument 'origin/main..HEAD'\n", 128).Once().Build()
		_, err := GitAhead(mock)(context.Background(), "/repo", "main")
		if err == nil || err.Error() != "git rev-list: fatal: ambiguous argument 'origin/main..HEAD'" {
			t.Fatalf("GitAhead error = %v", err)
		}
	})
	t.Run("empty_base", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		_, err := GitAhead(mock)(context.Background(), "/repo", "")
		if err == nil || err.Error() != "git rev-list: expected a branch, found empty" {
			t.Fatalf("GitAhead error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
	t.Run("nil_executor", func(t *testing.T) {
		_, err := GitAhead(nil)(context.Background(), "/repo", "main")
		if err == nil || err.Error() != "git rev-list: expected an executor, found nil" {
			t.Fatalf("GitAhead error = %v", err)
		}
	})
}

func assertGitCommand(t *testing.T, mock *executor.MockExecutor, dir string, args ...string) {
	t.Helper()
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	calls := mock.Executions()
	if len(calls) != 1 {
		t.Fatalf("executions = %d, want 1", len(calls))
	}
	call := calls[0]
	wantArgs := append([]string{"-C", dir}, args...)
	if call.Command != "git" || !slices.Equal(call.Args, wantArgs) {
		t.Fatalf("call = %s %#v, want git %#v", call.Command, call.Args, wantArgs)
	}
	if call.Timeout != gitCommandTimeout {
		t.Fatalf("Timeout = %s, want %s", call.Timeout, gitCommandTimeout)
	}
	if call.WorkingDir != "" {
		t.Fatalf("WorkingDir = %q, want empty", call.WorkingDir)
	}
}

func TestWorkingAgentCheckout(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	d := config.Defaults{MaxInFlight: 3, MinStartInterval: 10 * time.Minute}
	gitOK := func(context.Context, string) (string, error) { return "", nil }
	working := func(id, tab, label, cwd, fg string) (herdr.Tab, herdr.Pane) {
		return herdr.Tab{ID: tab, Label: label, AgentStatus: herdr.AgentStatusWorking},
			herdr.Pane{ID: id, TabID: tab, WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: cwd, ForegroundCwd: fg}
	}
	check := func(t *testing.T, in Inputs) Result {
		t.Helper()
		in.LastStartedAt = now.Add(-time.Hour)
		in.Now = now
		in.GitStatus = gitOK
		return Check(context.Background(), in, "/src", d)
	}

	t.Run("outside", func(t *testing.T) {
		logs := captureDebug(t)
		tab, pane := working("w1:p9", "w1:t9", "PRJ-9", "/other", "/other/bin")
		got := check(t, Inputs{
			Checkouts: []string{"/src"},
			Tabs:      []herdr.Tab{tab},
			Panes:     []herdr.Pane{pane},
		})
		if !got.OK || got.Reasons != nil {
			t.Fatalf("Check() = %+v, want OK", got)
		}
		if !strings.Contains(logs.String(), "agent=PRJ-9") || !strings.Contains(logs.String(), "tab=w1:t9") {
			t.Fatalf("debug log = %q", logs.String())
		}
	})

	t.Run("inside", func(t *testing.T) {
		logs := captureDebug(t)
		tab, pane := working("w1:p2", "w1:t2", "PRJ-2", "/src/pkg", "")
		got := check(t, Inputs{
			Checkouts: []string{"/src"},
			Tabs:      []herdr.Tab{tab},
			Panes:     []herdr.Pane{pane},
		})
		if got.OK || len(got.Reasons) != 1 || got.Reasons[0] != "agent PRJ-2 (w1:t2) is working" {
			t.Fatalf("Check() = %+v", got)
		}
		if strings.Contains(logs.String(), "outside checkout") {
			t.Fatalf("debug log = %q, want none", logs.String())
		}
	})

	t.Run("foreground cwd inside", func(t *testing.T) {
		tab, pane := working("w1:p2", "w1:t2", "PRJ-2", "/other", "/src/pkg")
		got := check(t, Inputs{
			Checkouts: []string{"/src"},
			Tabs:      []herdr.Tab{tab},
			Panes:     []herdr.Pane{pane},
		})
		if got.OK || len(got.Reasons) != 1 || got.Reasons[0] != "agent PRJ-2 (w1:t2) is working" {
			t.Fatalf("Check() = %+v", got)
		}
	})

	t.Run("other configured checkout", func(t *testing.T) {
		tab, pane := working("w1:p2", "w1:t2", "PRJ-2", "/elsewhere/repo", "")
		got := check(t, Inputs{
			Checkouts: []string{"/src", "/elsewhere"},
			Tabs:      []herdr.Tab{tab},
			Panes:     []herdr.Pane{pane},
		})
		if got.OK || len(got.Reasons) != 1 || got.Reasons[0] != "agent PRJ-2 (w1:t2) is working" {
			t.Fatalf("Check() = %+v", got)
		}
	})

	t.Run("ignore tab inside checkout", func(t *testing.T) {
		logs := captureDebug(t)
		tab, pane := working("w1:p8", "w1:t8", "dashboard", "/src", "")
		got := check(t, Inputs{
			Checkouts:  []string{"/src"},
			IgnoreTabs: []string{"dashboard"},
			Tabs:       []herdr.Tab{tab},
			Panes:      []herdr.Pane{pane},
		})
		if !got.OK || got.Reasons != nil {
			t.Fatalf("Check() = %+v, want OK", got)
		}
		if strings.Contains(logs.String(), "outside checkout") {
			t.Fatalf("debug log = %q, want none", logs.String())
		}
	})

	t.Run("sibling prefix is outside", func(t *testing.T) {
		tab, pane := working("w1:p9", "w1:t9", "PRJ-9", "/src2", "")
		got := check(t, Inputs{
			Checkouts: []string{"/src"},
			Tabs:      []herdr.Tab{tab},
			Panes:     []herdr.Pane{pane},
		})
		if !got.OK || got.Reasons != nil {
			t.Fatalf("Check() = %+v, want OK", got)
		}
	})

	t.Run("empty checkout", func(t *testing.T) {
		got := check(t, Inputs{Checkouts: []string{""}})
		if got.OK || len(got.Reasons) != 1 || got.Reasons[0] != "checkouts: expected a non-empty path, found empty" {
			t.Fatalf("Check() = %+v", got)
		}
	})
}

func captureDebug(t *testing.T) *bytes.Buffer {
	t.Helper()
	var buf bytes.Buffer
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&buf, &slog.HandlerOptions{Level: slog.LevelDebug})))
	t.Cleanup(func() { slog.SetDefault(prev) })
	return &buf
}

func TestCheckCarriesInFlight(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	in := Inputs{
		Tabs:          []herdr.Tab{{ID: "w1:t1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusIdle}},
		ActiveClaims:  make([]store.Claim, 2),
		Pulls:         []gh.PR{{Repo: "acme/widgets", Number: 1, URL: "https://github.com/acme/widgets/pull/1"}},
		LastStartedAt: now.Add(-time.Hour),
		Now:           now,
		GitStatus:     func(context.Context, string) (string, error) { return "", nil },
	}
	d := config.Defaults{MaxInFlight: 5, MinStartInterval: 10 * time.Minute}
	got := Check(context.Background(), in, "/src", d)
	if !got.OK || got.InFlight != 3 || got.MaxInFlight != 5 {
		t.Fatalf("Check() = %+v, want OK with InFlight 3 and MaxInFlight 5", got)
	}
}
