package dispatch

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

var update = flag.Bool("update", false, "update golden files")

var fixtureNow = time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC)

func TestRunDryRunGolden(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	doc := goldenScanDoc()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	pre := State{}
	pre.Record("acme/widgets#204", []string{"PRRC_old"}, fixtureNow)
	if err := SaveFile(store.Path, pre); err != nil {
		t.Fatal(err)
	}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}

	gotDoc, err := Run(context.Background(), h, store, cfg, Request{
		Doc: doc,
		PRs: []string{
			"acme/gizmos#50",
			"acme/missing#9",
			"acme/widgets#123",
			"acme/widgets#200",
			"acme/widgets#201",
			"acme/widgets#202",
			"acme/widgets#203",
			"acme/widgets#204",
		},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if !gotDoc.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(gotDoc.Results) != 8 {
		t.Fatalf("len(results) = %d, want 8 (never silently drop)", len(gotDoc.Results))
	}

	got, err := json.MarshalIndent(gotDoc, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	got = append(got, '\n')
	path := filepath.Join("testdata", "golden", "dispatch-dry-run.json")
	if *update {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, got, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := os.ReadFile(path) //nolint:gosec // testdata path
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("golden mismatch\n got:\n%s\nwant:\n%s", got, want)
	}
}

func TestRunDryRunRebaseRendersRebaseTemplate(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Rebase: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch", got.Results)
	}
	prompt := got.Results[0].RenderedPrompt
	if !strings.Contains(prompt, "Check out fix-widget") {
		t.Fatalf("missing checkout of head: %q", prompt)
	}
	if !strings.Contains(prompt, "origin/main") {
		t.Fatalf("missing origin/base: %q", prompt)
	}
	if !strings.Contains(prompt, "--force-with-lease") {
		t.Fatalf("missing force-with-lease: %q", prompt)
	}
	if !strings.Contains(strings.ToLower(prompt), "do not create a new worktree") {
		t.Fatalf("missing no-worktree: %q", prompt)
	}
	if strings.Contains(prompt, "unresolved review comments") {
		t.Fatalf("used comment template: %q", prompt)
	}
}

func TestRunDryRunDoesNotWriteState(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "state.json")
	store := FileStore{Path: path}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	cfg := config.Defaults()
	_, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if _, err := os.Stat(path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("state file written on dry-run: %v", err)
	}
}

func TestRunDryRunDoesNotPoll(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pX", "w2:tX")}}}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if h.n != 1 {
		t.Fatalf("AgentList calls = %d, want 1 (one-shot Check, never poll)", h.n)
	}
	if got.Results[0].Action != ActionWouldDispatch {
		t.Fatalf("action = %q, want would_dispatch (never queued)", got.Results[0].Action)
	}
	if got.Results[0].Detail != "gate currently busy: pane w2:pX" {
		t.Fatalf("detail = %q", got.Results[0].Detail)
	}
}

func TestRunDryRunNeverQueued(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{
		{workingAgent("w2:pX", "w2:tX")},
		{idleAgent("w2:pX", "w2:tX")},
	}}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	for _, r := range got.Results {
		if r.Action == ActionQueued {
			t.Fatalf("dry-run emitted queued: %+v", r)
		}
	}
	if h.n != 1 {
		t.Fatalf("AgentList calls = %d, want 1", h.n)
	}
}

func TestRunDryRunHerdrRequired(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{minErr: herdr.ErrNotInstalled}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	_, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if !errors.Is(err, herdr.ErrNotInstalled) {
		t.Fatalf("error = %v, want ErrNotInstalled", err)
	}
}

func TestRunLiveDispatchedWritesState(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if got.DryRun {
		t.Fatal("dry_run = true, want false")
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}
	if got.Results[0].PaneID != "w2:pC" || got.Results[0].RenderedPrompt == "" {
		t.Fatalf("dispatched item missing pane/prompt: %+v", got.Results[0])
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls = %d, want 1", h.promptN)
	}
	if !slices.Equal(h.sawUntil, []string{"idle", "done"}) {
		t.Fatalf("until = %v, want [idle done]", h.sawUntil)
	}

	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("state after send = %#v", st)
	}

	got2, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionSkippedDeduped {
		t.Fatalf("second results = %+v, want skipped_deduped", got2.Results)
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls after dedupe = %d, want 1", h.promptN)
	}
}

func TestRunLiveRebaseWritesHeadSHA(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.HeadSHA = "abc123def456"
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Rebase: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}
	if !strings.Contains(got.Results[0].RenderedPrompt, "Check out fix-widget") {
		t.Fatalf("live rebase used wrong template: %q", got.Results[0].RenderedPrompt)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.DedupedHead("acme/widgets#123", "abc123def456") {
		t.Fatalf("state after rebase = %#v", st)
	}
	if st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("rebase must not record comment ids: %#v", st)
	}

	got2, err := Run(context.Background(), h, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Rebase: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionSkippedDeduped {
		t.Fatalf("second results = %+v, want skipped_deduped", got2.Results)
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls after SHA dedupe = %d, want 1", h.promptN)
	}

	pr.HeadSHA = "fff000aaa111"
	got3, err := Run(context.Background(), h, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Rebase: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("third Run() unexpected error: %v", err)
	}
	if len(got3.Results) != 1 || got3.Results[0].Action != ActionDispatched {
		t.Fatalf("changed SHA results = %+v, want dispatched", got3.Results)
	}
	if h.promptN != 2 {
		t.Fatalf("Prompt calls after SHA change = %d, want 2", h.promptN)
	}
}

func TestRunLiveRebaseBlockedStopsAdvancing(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pr1 := fixtureEligiblePR()
	pr1.Unaddressed = false
	pr1.BlockingComments = nil
	pr2 := fixtureEligiblePR()
	pr2.Number = 124
	pr2.Unaddressed = false
	pr2.BlockingComments = nil
	pr2.HeadSHA = "bbb222"
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{
			Status: herdr.PromptMatched,
			Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "blocked"},
		}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr1, pr2}},
		Rebase: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	if got.Results[0].Action != ActionDispatchedBlocked || got.Results[0].Number != 123 {
		t.Fatalf("first = %+v, want dispatched_blocked #123", got.Results[0])
	}
	if got.Results[1].Action != ActionQueued || got.Results[1].Number != 124 {
		t.Fatalf("second = %+v, want queued #124", got.Results[1])
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls = %d, want 1 (gate/serial unchanged)", h.promptN)
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on blocked rebase")
	}
}

func TestRunLiveStallDoesNotWriteState(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{
		lists:   [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{Status: herdr.PromptStalled}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedStalled {
		t.Fatalf("results = %+v, want skipped_stalled", got.Results)
	}
	if got.Results[0].PaneID != "" || got.Results[0].RenderedPrompt != "" {
		t.Fatalf("stalled item should omit pane/prompt: %+v", got.Results[0])
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on stall")
	}
}

func TestRunLiveHerdrTimeoutWritesState(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{
		lists:   [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedTimeout {
		t.Fatalf("results = %+v, want dispatched_timeout", got.Results)
	}
	if got.Results[0].PaneID == "" || got.Results[0].RenderedPrompt == "" {
		t.Fatalf("timeout item missing pane/prompt: %+v", got.Results[0])
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("state after herdr timeout = %#v", st)
	}
}

func TestRunLiveProcessKillNoWrite(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{
			Status: herdr.PromptError,
			Err:    errors.New("run herdr: process timeout"),
		}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err == nil {
		t.Fatal("Run() error = nil, want failed")
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionFailed {
		t.Fatalf("results = %+v, want failed", got.Results)
	}
	if got.Results[0].PaneID != "" || got.Results[0].RenderedPrompt != "" {
		t.Fatalf("failed item should omit pane/prompt: %+v", got.Results[0])
	}
	if got.Results[0].Detail == "" {
		t.Fatal("failed item missing detail")
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on process-kill PromptError")
	}
}

func TestRunLiveDoneSettlementIsDispatched(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pr := fixtureEligiblePR()
	pr.Tab.AgentStatus = "done"
	h := &scriptHerdr{
		lists:   [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: "w2:pC", AgentStatus: "done"}}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched (done settlement)", got.Results)
	}
	if got.Results[0].Action == ActionDispatchedTimeout {
		t.Fatal("done settlement must not be dispatched_timeout")
	}
}

func TestRunLiveBlockedSettlementIsDispatchedBlocked(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{
			Status: herdr.PromptMatched,
			Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "blocked"},
		}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedBlocked {
		t.Fatalf("results = %+v, want dispatched_blocked", got.Results)
	}
	if got.Results[0].Action == ActionDispatched {
		t.Fatal("blocked settlement must not be recorded as plain dispatched")
	}
	if got.Results[0].PaneID == "" || got.Results[0].RenderedPrompt == "" {
		t.Fatalf("blocked item missing pane/prompt: %+v", got.Results[0])
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on transient blocked settlement")
	}
}

func TestRunLiveBlockedStopsAdvancing(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pr2 := fixtureEligiblePR()
	pr2.Number = 124
	pr2.BlockingComments = []scan.Comment{{CommentID: "PRRC_second"}}
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{
			Status: herdr.PromptMatched,
			Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "blocked"},
		}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR(), pr2}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	if got.Results[0].Action != ActionDispatchedBlocked || got.Results[0].Number != 123 {
		t.Fatalf("first = %+v, want dispatched_blocked #123", got.Results[0])
	}
	if got.Results[1].Action != ActionQueued || got.Results[1].Number != 124 {
		t.Fatalf("second = %+v, want queued #124", got.Results[1])
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls = %d, want 1 (do not start the next agent)", h.promptN)
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on blocked send")
	}
}

func TestRunLiveBlockedDoesNotDedupeOnRerun(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pr := fixtureEligiblePR()
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{
			{
				Status: herdr.PromptMatched,
				Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "blocked"},
			},
			{
				Status: herdr.PromptMatched,
				Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "idle"},
			},
		},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("first Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedBlocked {
		t.Fatalf("first = %+v, want dispatched_blocked", got.Results)
	}

	got2, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionDispatched {
		t.Fatalf("second = %+v, want dispatched (blocked must not skip as deduped)", got2.Results)
	}
	if h.promptN != 2 {
		t.Fatalf("Prompt calls = %d, want 2", h.promptN)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatal(err)
	}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("state after real completion = %#v", st)
	}
}

func TestRunLiveReplaceNotUnion(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pre := State{}
	pre.Record("acme/widgets#123", []string{"PRRC_old", "PRRC_gone"}, fixtureNow)
	if err := SaveFile(store.Path, pre); err != nil {
		t.Fatal(err)
	}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	if _, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow); err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatal(err)
	}
	got := st["acme/widgets#123"].DispatchedCommentIDs
	if !slices.Equal(got, []string{"PRRC_widget"}) {
		t.Fatalf("ids = %v, want [PRRC_widget] (replace, not union)", got)
	}
}

func TestRunLiveMidLoopFatalPartialJSON(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pr2 := fixtureEligiblePR()
	pr2.Number = 124
	pr2.BlockingComments = []scan.Comment{{CommentID: "PRRC_second"}}
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{
			{Status: herdr.PromptMatched},
			{Status: herdr.PromptError, Err: errors.New("unparseable result")},
		},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR(), pr2}},
	}, fixtureNow)
	if err == nil {
		t.Fatal("Run() error = nil, want mid-loop fatal")
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	if got.Results[0].Action != ActionDispatched || got.Results[0].Number != 123 {
		t.Fatalf("first = %+v, want dispatched #123", got.Results[0])
	}
	if got.Results[1].Action != ActionFailed || got.Results[1].Number != 124 {
		t.Fatalf("second = %+v, want failed #124", got.Results[1])
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatal(err)
	}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("state missing first PR: %#v", st)
	}
	if _, ok := st["acme/widgets#124"]; ok {
		t.Fatalf("state has second PR: %#v", st)
	}
}

func TestRunLiveGateTimeoutQueuesRest(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pr2 := fixtureEligiblePR()
	pr2.Number = 124
	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pX", "w2:tX")}}}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR(), pr2}},
	}, fixtureNow)
	if !errors.Is(err, ErrTimeout) {
		t.Fatalf("error = %v, want ErrTimeout", err)
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	for _, r := range got.Results {
		if r.Action != ActionQueued {
			t.Fatalf("result = %+v, want queued", r)
		}
	}
	if h.promptN != 0 {
		t.Fatalf("Prompt calls = %d, want 0 on gate timeout", h.promptN)
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on gate timeout")
	}
}

func TestRunDryRunDoesNotCallPrompt(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	if _, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow); err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if h.promptN != 0 {
		t.Fatalf("Prompt calls = %d, want 0 on dry-run", h.promptN)
	}
}

func liveCfg(t *testing.T) (config.Config, FileStore) {
	t.Helper()
	cfg := config.Defaults()
	cfg.DryRun = false
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	cfg.StateFile = store.Path
	return cfg, store
}

func TestRunRecordsEveryInputPR(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
		PRs: []string{"acme/widgets#123", "acme/missing#1"},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	actions := map[string]string{}
	for _, r := range got.Results {
		actions[r.Repo+"#"+itoa(r.Number)] = r.Action
	}
	if actions["acme/widgets#123"] != ActionWouldDispatch {
		t.Fatalf("actions = %v", actions)
	}
	if actions["acme/missing#1"] != ActionSkippedNotFound {
		t.Fatalf("actions = %v", actions)
	}
}

func goldenScanDoc() scan.Document {
	addr := fixtureEligiblePR()
	addr.Repo = "acme/gizmos"
	addr.Number = 50
	addr.Unaddressed = false
	addr.BlockingComments = nil

	draft := fixtureEligiblePR()
	draft.Number = 200
	draft.IsDraft = true

	noTab := fixtureEligiblePR()
	noTab.Number = 201
	noTab.Tab = nil

	noPane := fixtureEligiblePR()
	noPane.Number = 202
	noPane.Tab.PaneID = nil

	busy := fixtureEligiblePR()
	busy.Number = 203
	busy.Tab.AgentStatus = "working"

	deduped := fixtureEligiblePR()
	deduped.Number = 204
	deduped.BlockingComments[0].CommentID = "PRRC_old"

	return scan.Document{PRs: []scan.PR{
		addr, fixtureEligiblePR(), draft, noTab, noPane, busy, deduped,
	}}
}

func itoa(n int) string {
	return strconv.Itoa(n)
}
