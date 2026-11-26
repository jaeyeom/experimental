package gate

import (
	"context"
	"slices"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestCheckLane(t *testing.T) {
	clean := func(context.Context, string) (string, error) { return "", nil }
	var gitDirs []string
	recordGit := func(_ context.Context, dir string) (string, error) {
		gitDirs = append(gitDirs, dir)
		return "", nil
	}
	// started claims with no tab id count as running; see CountLane.
	started := func(lane string, n int) []store.Claim {
		out := make([]store.Claim, n)
		for i := range out {
			out[i] = store.Claim{ID: "PRJ-" + string(rune('A'+i)), State: store.Started, Lane: lane}
		}
		return out
	}
	working := Inputs{
		Tabs:  []herdr.Tab{{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking}},
		Panes: []herdr.Pane{{ID: "w1:p2", TabID: "w1:t2", AgentStatus: herdr.AgentStatusWorking, Cwd: "/linux/pkg"}},
	}
	tests := []struct {
		name string
		in   Inputs
		lane Lane
		want string
	}{
		{
			name: "ok",
			in:   Inputs{GitStatus: clean, ActiveClaims: started("linux", 2)},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 4},
			want: "linux: OK workers 2/4 (running), active 2",
		},
		{
			name: "full",
			in:   Inputs{GitStatus: clean, ActiveClaims: started("linux", 4)},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 4},
			want: "linux: blocked workers 4/4 (running), active 4: 4 running workers on lane linux (max_workers 4)",
		},
		{
			name: "working agent in the lane checkout",
			in:   Inputs{Tabs: working.Tabs, Panes: working.Panes, GitStatus: clean},
			lane: Lane{Name: "linux", Checkout: "/linux", MaxWorkers: 4},
			want: "linux: blocked workers 0/4 (running), active 0: agent PRJ-2 (w1:t2) is working",
		},
		{
			name: "working agent outside the lane checkout",
			in:   Inputs{Tabs: working.Tabs, Panes: working.Panes, GitStatus: clean},
			lane: Lane{Name: "other", Checkout: "/other", MaxWorkers: 4},
			want: "other: OK workers 0/4 (running), active 0",
		},
		{
			name: "remote checkout is not read",
			in:   Inputs{GitStatus: recordGit, ActiveClaims: started("mac", 1)},
			lane: Lane{Name: "mac", Machine: "mac1", Checkout: "/on/mac", MaxWorkers: 2},
			want: "mac: OK workers 1/2 (running), active 1 (checkout not checked on remote machine mac1)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := CheckLane(context.Background(), tc.in, tc.lane).Line()
			if got != tc.want {
				t.Fatalf("Line() = %q, want %q", got, tc.want)
			}
		})
	}
	if len(gitDirs) != 0 {
		t.Fatalf("git ran in %v for a remote lane", gitDirs)
	}
}

func TestSkippedLaneLine(t *testing.T) {
	got := SkippedLane(Lane{Name: "mac", MaxWorkers: 2}, nil, "host unreachable: timeout").Line()
	if got != "mac: unreachable (skipped): host unreachable: timeout" {
		t.Fatalf("Line() = %q", got)
	}
}

func TestCheckGlobal(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	d := config.Defaults{MaxInFlight: 2, MinStartInterval: 10 * time.Minute}
	in := Inputs{
		// Working agents and checkouts are per lane. The global check ignores them.
		Tabs:         []herdr.Tab{{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking}},
		ActiveClaims: []store.Claim{{ID: "PRJ-1"}},
		Pulls: []gh.PR{{
			Repo: "acme/widgets", Number: 1, URL: "https://github.com/acme/widgets/pull/1",
			Title: "review", Head: "other",
		}},
		PRTitlePattern: func(string) (string, error) { return `\[{id}\]`, nil },
		LastStartedAt:  now.Add(-time.Minute),
		Now:            now,
	}
	res := CheckGlobal(in, d)
	want := []string{"in flight: 2 (claims 1, PRs 1, overlap 0) of max 2", "last start 1m0s ago (min 10m0s)"}
	if res.OK || !slices.Equal(res.Reasons, want) || res.InFlight != 2 || res.MaxInFlight != 2 || res.Overlap != 0 {
		t.Fatalf("CheckGlobal() = %#v", res)
	}
	in.Pulls = nil
	in.LastStartedAt = time.Time{}
	res = CheckGlobal(in, d)
	if !res.OK || res.Reasons != nil {
		t.Fatalf("CheckGlobal() = %#v", res)
	}
}
