package tick

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestTickStartsTopItem(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("started PRJ-123 in factory/PRJ-123\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.src.claimIDs(); len(got) != 1 || got[0] != "PRJ-123" {
		t.Fatalf("claims = %v", got)
	}
	got := claimRow(t, env.st, "PRJ-123")
	if got.State != store.Started || got.Machine != "test-host" || got.TabID != "w1:t1" || got.WorkspaceID != "w1" {
		t.Fatalf("claim = %+v", got)
	}
	run := tickRun(t, env.st)
	if run.Outcome != outcomeStarted || !run.StartedAt.Equal(env.now) {
		t.Fatalf("run = %+v", run)
	}
	const want = `"summary":"started PRJ-123 in factory/PRJ-123"`
	if !bytes.Contains([]byte(run.DetailJSON), []byte(want)) {
		t.Fatalf("detail = %s", run.DetailJSON)
	}
	if tabs := env.herd.createdTabs(); len(tabs) != 1 || tabs[0] != "PRJ-123" {
		t.Fatalf("tabs = %v", tabs)
	}
}

func TestTickAppliesAgentRule(t *testing.T) {
	it := readyItem("PRJ-123", "Ship it")
	it.Labels = []string{"mechanical"}
	env := newTickEnv(t, []source.Item{it}, true, false)
	env.eng.Config.Defaults.AgentArgs = []string{"--model", "opus"}
	env.eng.Config.AgentRules = []config.AgentRule{{
		Labels:    []string{"mechanical"},
		AgentArgs: []string{"--model", "sonnet"},
	}}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("args     --model sonnet (rule 1 labels mechanical)\n")) {
		t.Fatalf("stdout =\n%s", env.out.String())
	}
	if !slices.Equal(env.herd.agentArgs(), []string{"--model", "sonnet"}) {
		t.Fatalf("agent args = %#v", env.herd.agentArgs())
	}
	// The rule has no name, so the claim records its first label.
	if got := claimRow(t, env.st, "PRJ-123"); got.AgentRule != "mechanical" || got.Model != "sonnet" {
		t.Fatalf("claim rule %q model %q, want mechanical and sonnet", got.AgentRule, got.Model)
	}
}

func TestTickCallerPaneDoesNotBlock(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t9", WorkspaceID: "w1", Label: "coord",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	env.herd.panes = []herdr.Pane{{
		ID: "w1:p9", TabID: "w1:t9", WorkspaceID: "w1",
		AgentStatus: herdr.AgentStatusWorking, Cwd: env.eng.Config.Sources[0].Cwd,
	}}
	env.eng.Config.CallerPaneID = "w1:p9"
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("started PRJ-123 in factory/PRJ-123\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.src.claimIDs(); len(got) != 1 || got[0] != "PRJ-123" {
		t.Fatalf("claims = %v", got)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeStarted {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickIgnoredPaneAndTabDoNotBlock(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.herd.tabs = []herdr.Tab{
		{ID: "w1:t1", WorkspaceID: "w1", Label: "dashboard", AgentStatus: herdr.AgentStatusWorking, PaneCount: 1},
		{ID: "w1:t2", WorkspaceID: "w1", Label: "worker", AgentStatus: herdr.AgentStatusWorking, PaneCount: 1},
	}
	cwd := env.eng.Config.Sources[0].Cwd
	env.herd.panes = []herdr.Pane{
		{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: cwd},
		{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: cwd},
	}
	env.eng.Config.Gate.IgnoreTabs = []string{"dashboard"}
	env.eng.Config.Gate.IgnorePanes = []string{"w1:p2"}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("started PRJ-123 in factory/PRJ-123\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.src.claimIDs(); len(got) != 1 || got[0] != "PRJ-123" {
		t.Fatalf("claims = %v", got)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeStarted {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickGatedDoesNotClaim(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t9", WorkspaceID: "w1", Label: "PRJ-9",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	env.herd.panes = []herdr.Pane{{
		ID: "w1:p9", TabID: "w1:t9", WorkspaceID: "w1",
		AgentStatus: herdr.AgentStatusWorking, Cwd: env.eng.Config.Sources[0].Cwd,
	}}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	const want = "gate: agent PRJ-9 (w1:t9) is working\n"
	if env.out.String() != want {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	assertNoClaim(t, env.st, "PRJ-123")
	if run := tickRun(t, env.st); run.Outcome != outcomeGated {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickDryRunGatePrintsBlockedPlan(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, false, false)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t9", WorkspaceID: "w1", Label: "PRJ-9",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	env.herd.panes = []herdr.Pane{{
		ID: "w1:p9", TabID: "w1:t9", WorkspaceID: "w1",
		AgentStatus: herdr.AgentStatusWorking, Cwd: env.eng.Config.Sources[0].Cwd,
	}}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	cwd := env.eng.Config.Sources[0].Cwd
	want := "blocked  yes\n" +
		"claim    PRJ-123 → In Progress\n" +
		"tab      workspace \"factory\" / tab \"PRJ-123\" (cwd " + cwd + ")\n" +
		"agent    claude\n" +
		"args     (default)\n" +
		"prompt   Implement PRJ-123…\n" +
		"gate: agent PRJ-9 (w1:t9) is working\n"
	if env.out.String() != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", env.out.String(), want)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	assertNoClaim(t, env.st, "PRJ-123")
	if tabs := env.herd.createdTabs(); len(tabs) != 0 {
		t.Fatalf("tabs = %v", tabs)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeGated {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickOpenPRsCountTowardInFlight(t *testing.T) {
	t.Run("open pull request alone", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
		env.eng.Config.Defaults.MaxInFlight = 1
		env.pulls.open = []gh.PR{pullRequest("review me", false)}
		if err := env.eng.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
		const want = "gate: in flight: 1 (claims 0, PRs 1, overlap 0) of max 1\n"
		if env.out.String() != want {
			t.Fatalf("stdout = %q, want %q", env.out.String(), want)
		}
		if got := env.src.claimIDs(); len(got) != 0 {
			t.Fatalf("claims = %v", got)
		}
		assertNoClaim(t, env.st, "PRJ-123")
	})
	t.Run("handed off claim plus its pull request", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{
			readyItem("PRJ-123", "Ship it"),
			readyItem("PRJ-9", "Other"),
		}, true, false)
		env.eng.Config.Defaults.MaxInFlight = 1
		seedClaim(t, env.st, "PRJ-9", store.HandedOff, env.now, "w1:t9", "w1")
		env.pulls.open = []gh.PR{pullRequest("[PRJ-9] waiting", false)}
		if err := env.eng.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
		const want = "gate: in flight: 1 (claims 1, PRs 1, overlap 1) of max 1\n"
		if env.out.String() != want {
			t.Fatalf("stdout = %q, want %q", env.out.String(), want)
		}
		if got := env.src.claimIDs(); len(got) != 0 {
			t.Fatalf("claims = %v", got)
		}
		assertNoClaim(t, env.st, "PRJ-123")
	})
}

func TestTickBaseBranchRefuses(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.eng.Config.Gate.BaseBranch = "main"
	env.eng.Config.Gate.Require = "on_base"
	env.eng.GitBranch = func(context.Context, string) (string, error) {
		return "feature", nil
	}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	cwd := env.eng.Config.Sources[0].Cwd
	want := "gate: " + cwd + ": expected branch main, found feature\n"
	if env.out.String() != want {
		t.Fatalf("stdout = %q, want %q", env.out.String(), want)
	}
	if env.gitCalls != 1 {
		t.Fatalf("git status calls = %d, want 1", env.gitCalls)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeGated {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickHoldThenReleaseStarts(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	path := filepath.Join(t.TempDir(), "hold")
	if err := os.WriteFile(path, []byte("owner: ada\nstill going\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	env.eng.Config.Gate.HoldFiles = []string{path}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	want := "gate: hold " + path + ": owner ada: still going\n"
	if env.out.String() != want {
		t.Fatalf("stdout = %q, want %q", env.out.String(), want)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeGated {
		t.Fatalf("outcome = %s", run.Outcome)
	}
	if err := os.Remove(path); err != nil {
		t.Fatal(err)
	}
	env.out.Reset()
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("started PRJ-123 in factory/PRJ-123\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.src.claimIDs(); len(got) != 1 || got[0] != "PRJ-123" {
		t.Fatalf("claims = %v", got)
	}
}

func TestTickHoldTabRefuses(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.eng.Config.Gate.HoldTabs = []string{"coord"}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "coord", AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	const want = "gate: hold tab coord (w1:t1) is idle\n"
	if env.out.String() != want {
		t.Fatalf("stdout = %q, want %q", env.out.String(), want)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
}

func TestTickDoneHoldTabStarts(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.eng.Config.Gate.HoldTabs = []string{"coord"}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t9", WorkspaceID: "w1", Label: "coord", AgentStatus: herdr.AgentStatusDone, PaneCount: 1,
	}}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("started PRJ-123 in factory/PRJ-123\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestTickDefaultHoldFileRefuses(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	path := filepath.Join(t.TempDir(), "hold")
	if err := os.WriteFile(path, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	env.eng.Config.Gate.DefaultHoldFile = path
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	want := "gate: hold " + path + "\n"
	if env.out.String() != want {
		t.Fatalf("stdout = %q, want %q", env.out.String(), want)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
}

func TestTickStartsHighestBeforeUnblocks(t *testing.T) {
	highest := readyItem("PRJ-1", "highest")
	highest.Priority = 0
	medium := readyItem("PRJ-2", "medium")
	medium.Priority = 2
	one := readyItem("PRJ-3", "one")
	one.BlockedBy = []source.BlockRef{{ID: "PRJ-2"}}
	two := readyItem("PRJ-4", "two")
	two.BlockedBy = []source.BlockRef{{ID: "PRJ-2"}}
	env := newTickEnv(t, []source.Item{medium, one, two, highest}, true, false)
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("started PRJ-1 in factory/PRJ-1\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.src.claimIDs(); len(got) != 1 || got[0] != "PRJ-1" {
		t.Fatalf("claims = %v", got)
	}
}

func TestTickDoesNotRaisePriority(t *testing.T) {
	low := readyItem("PRJ-10", "low")
	low.Priority = 2
	high := readyItem("PRJ-20", "high")
	high.Priority = 0
	high.BlockedBy = []source.BlockRef{{ID: "PRJ-10"}}
	env := newTickEnv(t, []source.Item{low, high}, true, false)
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := env.src.priorityIDs(); len(got) != 0 {
		t.Fatalf("SetPriority calls = %v", got)
	}
	if got := env.src.items["PRJ-10"].Priority; got != 2 {
		t.Fatalf("priority = %d, want 2", got)
	}
}

func TestTickIdle(t *testing.T) {
	done := readyItem("PRJ-123", "Ship it")
	done.Done = true
	done.State = "Done"
	env := newTickEnv(t, []source.Item{done}, true, false)
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if env.out.String() != "no ready items\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if env.gitCalls != 0 {
		t.Fatalf("git calls = %d", env.gitCalls)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeIdle {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickLostRaceStartsNext(t *testing.T) {
	env := newTickEnv(t, []source.Item{
		readyItem("PRJ-1", "First"),
		readyItem("PRJ-2", "Second"),
	}, true, false)
	env.src.lose["PRJ-1"] = true
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := env.src.claimIDs(); len(got) != 2 || got[0] != "PRJ-1" || got[1] != "PRJ-2" {
		t.Fatalf("claims = %v", got)
	}
	if !bytes.Contains(env.out.Bytes(), []byte("started PRJ-2 in factory/PRJ-2\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	assertNoClaim(t, env.st, "PRJ-1")
	if got := claimRow(t, env.st, "PRJ-2"); got.State != store.Started {
		t.Fatalf("PRJ-2 claim = %+v", got)
	}
}

func TestTickLostRaceStopsAfterThree(t *testing.T) {
	items := make([]source.Item, 4)
	for i := range items {
		items[i] = readyItem(fmt.Sprintf("PRJ-%d", i+1), "Lose")
	}
	env := newTickEnv(t, items, true, false)
	for i := 1; i <= 4; i++ {
		env.src.lose[fmt.Sprintf("PRJ-%d", i)] = true
	}
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := env.src.claimIDs(); len(got) != 3 || got[0] != "PRJ-1" || got[1] != "PRJ-2" || got[2] != "PRJ-3" {
		t.Fatalf("claims = %v", got)
	}
	const want = "lost the claim race for PRJ-1, PRJ-2, PRJ-3\n"
	if !bytes.HasSuffix(env.out.Bytes(), []byte(want)) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	for _, id := range []string{"PRJ-1", "PRJ-2", "PRJ-3", "PRJ-4"} {
		assertNoClaim(t, env.st, id)
	}
	if tabs := env.herd.createdTabs(); len(tabs) != 0 {
		t.Fatalf("tabs = %v", tabs)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeLost {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickMergedPRFinish(t *testing.T) {
	tests := []struct {
		name       string
		finish     bool
		apply      bool
		done       bool
		title      string
		finishErr  error
		wantFinish bool
		wantState  store.ClaimState
		wantLog    bool
	}{
		{name: "finish when configured", finish: true, apply: true, title: "[PRJ-123] ship", wantFinish: true, wantState: store.Done},
		{name: "skip finish when disabled", apply: true, title: "[PRJ-123] ship", wantState: store.Done},
		{name: "skip finish when already done", finish: true, apply: true, done: true, title: "[PRJ-123] ship", wantState: store.Done},
		{name: "unrelated pull request", finish: true, apply: true, title: "[OTHER] nope", wantState: store.Started},
		{name: "dry-run still finishes", finish: true, title: "[PRJ-123] ship", wantFinish: true, wantState: store.Done},
		{name: "finish error still closes the claim", finish: true, apply: true, title: "[PRJ-123] ship", finishErr: errors.New("finish broke"), wantFinish: true, wantState: store.Done, wantLog: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			item := readyItem("PRJ-123", "Ship it")
			if tc.done {
				item.Done = true
				item.State = "Done"
			}
			env := newTickEnv(t, []source.Item{item}, tc.apply, tc.finish)
			env.pulls.merged = []gh.PR{pullRequest(tc.title, true)}
			env.src.finishErr = tc.finishErr
			seedClaim(t, env.st, "PRJ-123", store.Started, env.now, "w1:t1", "w1")
			var logs bytes.Buffer
			if tc.wantLog {
				restore := captureLog(t, &logs)
				t.Cleanup(restore)
			}
			if err := env.eng.Tick(context.Background()); err != nil {
				t.Fatal(err)
			}
			got := claimRow(t, env.st, "PRJ-123")
			if got.State != tc.wantState || !got.ClaimedAt.Equal(env.now.Add(-time.Hour)) {
				t.Fatalf("claim = %+v", got)
			}
			finished := env.src.finishIDs()
			if tc.wantFinish {
				if len(finished) != 1 || finished[0] != "PRJ-123" {
					t.Fatalf("finishes = %v", finished)
				}
			} else if len(finished) != 0 {
				t.Fatalf("finishes = %v", finished)
			}
			if env.out.String() != "no ready items\n" {
				t.Fatalf("stdout = %q", env.out.String())
			}
			if run := tickRun(t, env.st); run.Outcome != outcomeIdle {
				t.Fatalf("outcome = %s", run.Outcome)
			}
			if tc.wantLog && (!bytes.Contains(logs.Bytes(), []byte("finish on merge failed")) || !bytes.Contains(logs.Bytes(), []byte("PRJ-123")) || !bytes.Contains(logs.Bytes(), []byte("finish broke"))) {
				t.Fatalf("log = %q", logs.String())
			}
		})
	}
}

func TestTickSkipsClaimClosedThisTick(t *testing.T) {
	env := newTickEnv(t, []source.Item{
		readyItem("PRJ-1", "Merged"),
		readyItem("PRJ-2", "Next"),
	}, true, true)
	env.pulls.merged = []gh.PR{pullRequest("[PRJ-1] ship", true)}
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	closed := claimRow(t, env.st, "PRJ-1")
	if closed.State != store.Done || !closed.ClaimedAt.Equal(env.now.Add(-time.Hour)) {
		t.Fatalf("closed claim = %+v", closed)
	}
	if got := env.src.finishIDs(); len(got) != 1 || got[0] != "PRJ-1" {
		t.Fatalf("finishes = %v", got)
	}
	if got := env.src.claimIDs(); len(got) != 1 || got[0] != "PRJ-2" {
		t.Fatalf("claims = %v", got)
	}
	if got := claimRow(t, env.st, "PRJ-2"); got.State != store.Started {
		t.Fatalf("PRJ-2 claim = %+v", got)
	}
	if bytes.Contains(env.out.Bytes(), []byte("started PRJ-1")) || !bytes.Contains(env.out.Bytes(), []byte("started PRJ-2 in factory/PRJ-2\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestTickHandsOffWhenPROpen(t *testing.T) {
	t.Run("open pull request", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{
			readyItem("PRJ-1", "Started"),
			readyItem("PRJ-2", "Claimed"),
		}, true, true)
		seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")
		seedClaim(t, env.st, "PRJ-2", store.Claimed, env.now, "", "")
		env.pulls.open = []gh.PR{
			pullRequest("[PRJ-1] one", false),
			pullRequest("[PRJ-2] two", false),
		}
		if err := env.eng.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
		got := claimRow(t, env.st, "PRJ-1")
		if got.State != store.HandedOff || got.TabID != "w1:t1" || got.WorkspaceID != "w1" {
			t.Fatalf("PRJ-1 claim = %+v", got)
		}
		if len(got.PRs) != 1 || got.PRs[0].Title != "[PRJ-1] one" || got.PRs[0].URL != "https://example.test/pull/1" {
			t.Fatalf("PRJ-1 prs = %+v", got.PRs)
		}
		if claimRow(t, env.st, "PRJ-2").State != store.Claimed {
			t.Fatalf("PRJ-2 claim = %+v", claimRow(t, env.st, "PRJ-2"))
		}
		if finished := env.src.finishIDs(); len(finished) != 0 {
			t.Fatalf("finishes = %v", finished)
		}
		if env.out.String() != "no ready items\n" {
			t.Fatalf("stdout = %q", env.out.String())
		}
	})
	t.Run("no pull request", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Started")}, true, false)
		seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")
		if err := env.eng.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
		got := claimRow(t, env.st, "PRJ-1")
		if got.State != store.Started || got.TabID != "w1:t1" || got.WorkspaceID != "w1" {
			t.Fatalf("claim = %+v", got)
		}
	})
}

func TestTickMergeWindow(t *testing.T) {
	t.Run("no prior tick", func(t *testing.T) {
		env := newTickEnv(t, nil, true, false)
		if err := env.eng.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
		want := env.now.Add(-(7*24*time.Hour + time.Hour))
		if len(env.pulls.since) != 1 || !env.pulls.since[0].Equal(want) {
			t.Fatalf("since = %v, want %s", env.pulls.since, want)
		}
	})
	t.Run("previous tick minus one hour", func(t *testing.T) {
		env := newTickEnv(t, nil, true, false)
		prior := env.now.Add(-3 * time.Hour)
		seedRun(t, env.st, prior, "tick")
		seedRun(t, env.st, env.now.Add(-time.Minute), "sync")
		if err := env.eng.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
		want := prior.Add(-time.Hour)
		if len(env.pulls.since) != 1 || !env.pulls.since[0].Equal(want) {
			t.Fatalf("since = %v, want %s", env.pulls.since, want)
		}
	})
}

func TestTickSyncErrorStops(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, false)
	env.src.syncErr = errors.New("sync broke")
	err := env.eng.Tick(context.Background())
	if err == nil || !bytes.Contains([]byte(err.Error()), []byte("sync broke")) {
		t.Fatalf("error = %v", err)
	}
	if env.pulls.mergedCalls != 0 {
		t.Fatalf("merged calls = %d", env.pulls.mergedCalls)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	assertNoTickRun(t, env.st)
}

func TestTickOtherErrorStops(t *testing.T) {
	env := newTickEnv(t, []source.Item{
		readyItem("PRJ-1", "First"),
		readyItem("PRJ-2", "Second"),
	}, true, false)
	env.src.getErr = errors.New("get broke")
	env.src.getErrID = "PRJ-1"
	err := env.eng.Tick(context.Background())
	if err == nil || !bytes.Contains([]byte(err.Error()), []byte("get broke")) || errors.Is(err, start.ErrLostRace) {
		t.Fatalf("error = %v", err)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	assertNoClaim(t, env.st, "PRJ-1")
	assertNoClaim(t, env.st, "PRJ-2")
	assertNoTickRun(t, env.st)
	if bytes.Contains(env.out.Bytes(), []byte("started ")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestTickDryRunDoesNotClaim(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, false, false)
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
	assertNoClaim(t, env.st, "PRJ-123")
	if !bytes.Contains(env.out.Bytes(), []byte("dry-run PRJ-123 in factory/PRJ-123\n")) {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if tabs := env.herd.createdTabs(); len(tabs) != 0 {
		t.Fatalf("tabs = %v", tabs)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeDryRun {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestRunStopsOnCancel(t *testing.T) {
	t.Run("already cancelled", func(t *testing.T) {
		env := newTickEnv(t, nil, true, false)
		ctx, cancel := context.WithCancel(context.Background())
		cancel()
		if err := env.eng.Run(ctx, time.Millisecond); err != nil {
			t.Fatal(err)
		}
		if env.src.syncCount() != 0 {
			t.Fatalf("syncs = %d", env.src.syncCount())
		}
	})
	t.Run("cancel during tick", func(t *testing.T) {
		env := newTickEnv(t, nil, true, false)
		ctx, cancel := context.WithCancel(context.Background())
		env.src.onSync = cancel
		errc := make(chan error, 1)
		go func() {
			errc <- env.eng.Run(ctx, time.Hour)
		}()
		select {
		case err := <-errc:
			if err != nil {
				t.Fatal(err)
			}
		case <-time.After(10 * time.Second):
			cancel()
			t.Fatal("Run did not return after cancel")
		}
		if env.src.syncCount() != 1 {
			t.Fatalf("syncs = %d", env.src.syncCount())
		}
	})
}

func TestRunExitsAfterFiveErrors(t *testing.T) {
	env := newTickEnv(t, nil, true, false)
	env.src.syncErr = errors.New("sync broke")
	err := env.eng.Run(context.Background(), time.Millisecond)
	if err == nil || !bytes.Contains([]byte(err.Error()), []byte("5 consecutive")) || !bytes.Contains([]byte(err.Error()), []byte("sync broke")) {
		t.Fatalf("error = %v", err)
	}
	if env.src.syncCount() != 5 {
		t.Fatalf("syncs = %d", env.src.syncCount())
	}
	assertNoTickRun(t, env.st)
}

type tickEnv struct {
	eng      *Engine
	src      *memSource
	pulls    *fakePulls
	herd     *fakeHerd
	st       *store.Store
	out      *bytes.Buffer
	now      time.Time
	gitCalls int
}

func newTickEnv(t *testing.T, items []source.Item, apply, finishOnMerge bool) *tickEnv {
	t.Helper()
	now := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	st := openTickStore(t)
	src := newMemSource(items...)
	pulls := &fakePulls{}
	herd := &fakeHerd{
		workspaces: []herdr.Workspace{{
			ID: "w1", Label: "factory", AgentStatus: herdr.AgentStatusIdle,
		}},
	}
	out := &bytes.Buffer{}
	env := &tickEnv{src: src, pulls: pulls, herd: herd, st: st, out: out, now: now}
	env.eng = &Engine{
		Store:    st,
		Sources:  map[string]source.Source{"jira:acme": src},
		Config:   tickConfig(t, finishOnMerge),
		Pulls:    pulls,
		Sessions: herd,
		Closer:   herd,
		Agent:    herd,
		Git: func(context.Context, string) (string, error) {
			env.gitCalls++
			return "", nil
		},
		Now: func() time.Time { return now },
		Stash: func(context.Context, string) (int, error) {
			return 0, nil
		},
		Hostname: "test-host",
		Out:      out,
		Apply:    apply,
	}
	return env
}

func tickConfig(t *testing.T, finishOnMerge bool) *config.Config {
	t.Helper()
	return &config.Config{
		Defaults: config.Defaults{
			MaxInFlight:      3,
			MinStartInterval: 10 * time.Minute,
			FullSyncEvery:    24 * time.Hour,
			AgentKind:        "claude",
		},
		Sources: []config.Source{{
			Name:              "acme",
			Kind:              "jira",
			Cwd:               t.TempDir(),
			Workspace:         "factory",
			StartPrompt:       "Implement {id}",
			AssigneeOK:        "me",
			ParentBlocksChild: config.ParentBlocksIgnore,
			ClaimStatus:       "In Progress",
			PRTitlePattern:    `\[{id}\]`,
			Base:              "main",
			FinishOnMerge:     finishOnMerge,
		}},
	}
}

func readyItem(id, title string) source.Item {
	return source.Item{
		Source:  "jira:acme",
		ID:      id,
		Title:   title,
		Type:    "Task",
		State:   "To Do",
		Created: time.Date(2026, 10, 1, 0, 0, 0, 0, time.UTC),
		Updated: time.Date(2026, 10, 30, 0, 0, 0, 0, time.UTC),
	}
}

func pullRequest(title string, merged bool) gh.PR {
	pr := gh.PR{
		Repo: "acme/widgets", Number: 1, Title: title,
		URL: "https://example.test/pull/1", Merged: merged,
		CreatedAt: time.Date(2026, 10, 30, 6, 0, 0, 0, time.UTC),
	}
	if merged {
		pr.MergedAt = time.Date(2026, 10, 30, 9, 0, 0, 0, time.UTC)
		pr.ClosedAt = pr.MergedAt
	}
	return pr
}

func openTickStore(t *testing.T) *store.Store {
	t.Helper()
	st, err := store.Open(context.Background(), filepath.Join(t.TempDir(), "factory.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("close store: %v", err)
		}
	})
	return st
}

func seedClaim(t *testing.T, st *store.Store, id string, state store.ClaimState, now time.Time, tabID, wsID string) {
	t.Helper()
	at := now.Add(-time.Hour)
	err := st.InsertClaim(context.Background(), store.Claim{
		Source: "jira:acme", ID: id, ClaimedAt: at, Machine: "test-host",
		TabID: tabID, WorkspaceID: wsID, State: state, UpdatedAt: at,
	})
	if err != nil {
		t.Fatal(err)
	}
}

func seedRun(t *testing.T, st *store.Store, at time.Time, command string) {
	t.Helper()
	err := st.AppendRun(context.Background(), store.Run{
		StartedAt: at, Command: command, Outcome: "ok", DetailJSON: `{"summary":"prior"}`,
	})
	if err != nil {
		t.Fatal(err)
	}
}

func claimRow(t *testing.T, st *store.Store, id string) store.Claim {
	t.Helper()
	got, err := st.LatestClaim(context.Background(), "jira:acme", id)
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func assertNoClaim(t *testing.T, st *store.Store, id string) {
	t.Helper()
	_, err := st.LatestClaim(context.Background(), "jira:acme", id)
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("claim %s error = %v", id, err)
	}
}

func tickRun(t *testing.T, st *store.Store) store.Run {
	t.Helper()
	runs, err := st.Runs(context.Background(), 20)
	if err != nil {
		t.Fatal(err)
	}
	for _, run := range runs {
		if run.Command == "tick" {
			return run
		}
	}
	t.Fatal("no tick run")
	return store.Run{}
}

func assertNoTickRun(t *testing.T, st *store.Store) {
	t.Helper()
	runs, err := st.Runs(context.Background(), 20)
	if err != nil {
		t.Fatal(err)
	}
	for _, run := range runs {
		if run.Command == "tick" {
			t.Fatalf("recorded tick run %+v", run)
		}
	}
}

func captureLog(t *testing.T, buf *bytes.Buffer) func() {
	t.Helper()
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(buf, &slog.HandlerOptions{Level: slog.LevelDebug})))
	return func() { slog.SetDefault(prev) }
}

// memSource is an in-memory source. lose makes Claim leave the item with
// another assignee, so confirm returns ErrLostRace.
type memSource struct {
	mu         sync.Mutex
	name       string
	account    string
	items      map[string]source.Item
	lose       map[string]bool
	claims     []string
	finishes   []string
	syncs      int
	syncErr    error
	getErr     error
	getErrID   string
	finishErr  error
	onSync     func()
	priorities []string
	// query, when set, is the source query: Sync and IDs return only the
	// items it accepts.
	query  func(source.Item) bool
	idsErr error
}

func (m *memSource) IDs(ctx context.Context) ([]string, error) {
	if err := ctx.Err(); err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.idsErr != nil {
		return nil, m.idsErr
	}
	out := make([]string, 0, len(m.items))
	for id, it := range m.items {
		if m.query == nil || m.query(it) {
			out = append(out, id)
		}
	}
	return out, nil
}

func newMemSource(items ...source.Item) *memSource {
	m := &memSource{
		name:    "jira:acme",
		account: "ada",
		items:   map[string]source.Item{},
		lose:    map[string]bool{},
	}
	for _, it := range items {
		m.items[it.ID] = it
	}
	return m
}

func (m *memSource) Name() string { return m.name }

func (m *memSource) Me(ctx context.Context) (string, error) {
	if err := ctx.Err(); err != nil {
		return "", err //nolint:wrapcheck // propagate context cancellation
	}
	if m.account == "" {
		return "", errors.New("me: expected an account, found empty")
	}
	return m.account, nil
}

func (m *memSource) Sync(ctx context.Context, since time.Time) ([]source.Item, error) {
	if err := ctx.Err(); err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	m.mu.Lock()
	m.syncs++
	err := m.syncErr
	var out []source.Item
	if err == nil {
		out = make([]source.Item, 0, len(m.items))
		for _, it := range m.items {
			if it.Updated.Before(since) || (m.query != nil && !m.query(it)) {
				continue
			}
			out = append(out, cloneMemItem(it))
		}
	}
	hook := m.onSync
	m.mu.Unlock()
	if err != nil {
		return nil, err
	}
	if hook != nil {
		hook()
	}
	return out, nil
}

func (m *memSource) Get(ctx context.Context, id string) (source.Item, error) {
	if err := ctx.Err(); err != nil {
		return source.Item{}, err //nolint:wrapcheck // propagate context cancellation
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.getErr != nil && (m.getErrID == "" || m.getErrID == id) {
		return source.Item{}, m.getErr
	}
	it, ok := m.items[id]
	if !ok {
		return source.Item{}, fmt.Errorf("get %q: %w", id, source.ErrNotFound)
	}
	return cloneMemItem(it), nil
}

func (m *memSource) Claim(ctx context.Context, id string) error {
	if err := ctx.Err(); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	it, ok := m.items[id]
	if !ok {
		return fmt.Errorf("claim %q: %w", id, source.ErrNotFound)
	}
	m.claims = append(m.claims, id)
	it.State = "In Progress"
	it.Assignee = m.account
	if m.lose[id] {
		it.State = "To Do"
		it.Assignee = "other"
	}
	m.items[id] = it
	return nil
}

func (m *memSource) ClaimWithOptions(ctx context.Context, id string, _ source.ClaimOptions) error {
	return m.Claim(ctx, id)
}

func (m *memSource) PostClaimComment(context.Context, string, string) error {
	return source.ErrNotSupported
}

func (m *memSource) LatestClaimComment(context.Context, string) (*source.ClaimComment, error) {
	return nil, source.ErrNotSupported
}

func (m *memSource) Release(context.Context, string) error { return source.ErrNotSupported }

func (m *memSource) Finish(ctx context.Context, id string) error {
	if err := ctx.Err(); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.finishes = append(m.finishes, id)
	if m.finishErr != nil {
		return m.finishErr
	}
	it, ok := m.items[id]
	if !ok {
		return fmt.Errorf("finish %q: %w", id, source.ErrNotFound)
	}
	it.Done = true
	it.State = "Done"
	m.items[id] = it
	return nil
}

func (m *memSource) Close(context.Context, string, string) error { return source.ErrNotSupported }

func (m *memSource) SetPriority(ctx context.Context, id string, rank int) error {
	if err := ctx.Err(); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.priorities = append(m.priorities, id)
	it, ok := m.items[id]
	if !ok {
		return fmt.Errorf("set priority %q: %w", id, source.ErrNotFound)
	}
	it.Priority = rank
	m.items[id] = it
	return nil
}

func (m *memSource) Link(context.Context, string, string) error { return source.ErrNotSupported }

func (m *memSource) Unlink(context.Context, string, string) error { return source.ErrNotSupported }

func (m *memSource) Create(context.Context, source.CreateRequest) (string, error) {
	return "", source.ErrNotSupported
}

func (m *memSource) syncCount() int {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.syncs
}

func (m *memSource) claimIDs() []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	return append([]string(nil), m.claims...)
}

func (m *memSource) finishIDs() []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	return append([]string(nil), m.finishes...)
}

func (m *memSource) priorityIDs() []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	return append([]string(nil), m.priorities...)
}

func cloneMemItem(it source.Item) source.Item {
	it.Labels = append([]string(nil), it.Labels...)
	it.BlockedBy = append([]source.BlockRef(nil), it.BlockedBy...)
	return it
}

type fakePulls struct {
	merged      []gh.PR
	allMerged   []gh.PR
	byTitle     []gh.PR
	open        []gh.PR
	since       []time.Time
	titleIDs    []string
	mergedCalls int
	allCalls    int
	titleCalls  int
	mergedErr   error
	allErr      error
	titleErr    error
}

func (f *fakePulls) MergedByMe(context.Context) ([]gh.PR, error) {
	f.allCalls++
	if f.allErr != nil {
		return nil, f.allErr
	}
	return append([]gh.PR(nil), f.allMerged...), nil
}

func (f *fakePulls) MergedByMeSince(_ context.Context, at time.Time) ([]gh.PR, error) {
	f.since = append(f.since, at)
	f.mergedCalls++
	if f.mergedErr != nil {
		return nil, f.mergedErr
	}
	return append([]gh.PR(nil), f.merged...), nil
}

func (f *fakePulls) MergedByTitle(_ context.Context, id string) ([]gh.PR, error) {
	f.titleCalls++
	f.titleIDs = append(f.titleIDs, id)
	if f.titleErr != nil {
		return nil, f.titleErr
	}
	var out []gh.PR
	for _, pr := range f.byTitle {
		if strings.Contains(pr.Title, id) {
			out = append(out, pr)
		}
	}
	return out, nil
}

func (f *fakePulls) OpenByMe(context.Context) ([]gh.PR, error) {
	return append([]gh.PR(nil), f.open...), nil
}

type fakeHerd struct {
	mu         sync.Mutex
	workspaces []herdr.Workspace
	tabs       []herdr.Tab
	panes      []herdr.Pane
	created    []string
	started    []string
	closed     []string
	closeErr   error
	closeType  string
}

func (f *fakeHerd) Workspaces(context.Context) ([]herdr.Workspace, error) {
	return append([]herdr.Workspace(nil), f.workspaces...), nil
}

func (f *fakeHerd) Tabs(context.Context) ([]herdr.Tab, error) {
	return append([]herdr.Tab(nil), f.tabs...), nil
}

func (f *fakeHerd) Panes(context.Context) ([]herdr.Pane, error) {
	return append([]herdr.Pane(nil), f.panes...), nil
}

func (f *fakeHerd) FindWorkspace(_ context.Context, label string) (herdr.Workspace, bool, error) {
	for _, ws := range f.workspaces {
		if ws.Label == label {
			return ws, true, nil
		}
	}
	return herdr.Workspace{}, false, nil
}

func (f *fakeHerd) CreateWorkspace(context.Context, string, string, []string) (herdr.Workspace, herdr.Pane, error) {
	return herdr.Workspace{}, herdr.Pane{}, errors.New("create workspace: expected an existing workspace, found none")
}

func (f *fakeHerd) CreateTab(_ context.Context, workspaceID, label, _ string, _ []string) (herdr.Tab, herdr.Pane, error) {
	f.mu.Lock()
	f.created = append(f.created, label)
	f.mu.Unlock()
	tab := herdr.Tab{ID: "w1:t1", WorkspaceID: workspaceID, Label: label, AgentStatus: herdr.AgentStatusIdle, PaneCount: 1}
	pane := herdr.Pane{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: workspaceID}
	return tab, pane, nil
}

func (f *fakeHerd) StartAgent(_ context.Context, _, _, _ string, _ time.Duration, agentArgs []string) error {
	f.mu.Lock()
	f.started = append([]string(nil), agentArgs...)
	f.mu.Unlock()
	return nil
}

func (f *fakeHerd) agentArgs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.started...)
}

func (f *fakeHerd) Prompt(context.Context, string, string, []string, time.Duration) error {
	return nil
}

func (f *fakeHerd) WaitAgent(_ context.Context, _ string, until []string, _ time.Duration) (string, error) {
	return until[0], nil
}

func (f *fakeHerd) ReadAgent(context.Context, string, int) (string, error) { return "", nil }

func (f *fakeHerd) SendKeys(context.Context, string, ...string) error { return nil }

func (f *fakeHerd) createdTabs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.created...)
}

func (f *fakeHerd) CloseTab(_ context.Context, tabID string) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.closed = append(f.closed, tabID)
	if f.closeErr != nil {
		return "", f.closeErr
	}
	if f.closeType != "" {
		return f.closeType, nil
	}
	return "tab_closed", nil
}

func (f *fakeHerd) closedTabs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.closed...)
}
