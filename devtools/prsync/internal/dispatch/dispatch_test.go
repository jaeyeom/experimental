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
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

var update *bool

func TestMain(m *testing.M) {
	update = flag.Bool("update", false, "update golden files")
	os.Exit(m.Run())
}

var fixtureNow = time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

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

func TestRunDryRunCIFixRendersCIFixTemplate(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
		Hint:  "job X failed at target Y; key error: boom",
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
	if !strings.Contains(prompt, "Fix the failing CI") {
		t.Fatalf("missing CI-fix body: %q", prompt)
	}
	if !strings.Contains(prompt, "job X failed at target Y; key error: boom") {
		t.Fatalf("missing hint: %q", prompt)
	}
	if strings.Contains(prompt, "unresolved review comments") {
		t.Fatalf("used comment template: %q", prompt)
	}
	if strings.Contains(prompt, "--force-with-lease") {
		t.Fatalf("used rebase template: %q", prompt)
	}
}

func TestRunDryRunMergeQueueFixRendersPrompt(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.Base = "main"
	commentURL := "https://github.com/acme/widgets/pull/123#issuecomment-9"
	pr.Actions = []scan.Action{{
		Name:       scan.ActionMergeQueueFix,
		EventID:    "EVT",
		Class:      "our-code",
		CommentURL: commentURL,
	}}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch", got.Results)
	}
	prompt := got.Results[0].RenderedPrompt
	if !strings.Contains(prompt, commentURL) {
		t.Fatalf("missing RCA comment URL: %q", prompt)
	}
	if !strings.Contains(prompt, "fix the cause") {
		t.Fatalf("missing fix-the-cause: %q", prompt)
	}
	if !strings.Contains(prompt, "When the ejection is a merge conflict, rebase onto origin/main, regenerate lockfiles, and push.") {
		t.Fatalf("missing conflict instructions: %q", prompt)
	}
	if strings.Contains(prompt, "unresolved review comments") {
		t.Fatalf("used comment template: %q", prompt)
	}
	if strings.Contains(prompt, "--force-with-lease") {
		t.Fatalf("used rebase template: %q", prompt)
	}
}

func TestRunDryRunCIFixIgnoresMergeQueueAction(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.Actions = []scan.Action{{
		Name:       scan.ActionMergeQueueFix,
		CommentURL: "https://github.com/acme/widgets/pull/123#issuecomment-9",
	}}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch", got.Results)
	}
	if !strings.Contains(got.Results[0].RenderedPrompt, "Fix the failing CI") {
		t.Fatalf("CI-fix ignored the flag: %q", got.Results[0].RenderedPrompt)
	}
}

func TestRunDryRunPromptSuffixEndsEveryPromptKind(t *testing.T) {
	t.Parallel()

	addressed := fixtureEligiblePR()
	addressed.Unaddressed = false
	addressed.BlockingComments = nil
	mergeQueue := addressed
	mergeQueue.Actions = []scan.Action{{
		Name:       scan.ActionMergeQueueFix,
		CommentURL: "https://github.com/acme/widgets/pull/123#issuecomment-9",
	}}
	tests := []struct {
		name string
		pr   scan.PR
		req  Request
		body string
	}{
		{name: "comment", pr: fixtureEligiblePR(), body: "unresolved review comments"},
		{name: "rebase", pr: addressed, req: Request{Rebase: true}, body: "--force-with-lease"},
		{name: "ci-fix", pr: addressed, req: Request{CIFix: true}, body: "Fix the failing CI"},
		{name: "merge-queue-fix", pr: mergeQueue, body: "fix the cause"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			cfg := config.Defaults()
			cfg.PromptSuffix = "NOTICE: x"
			store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
			h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
			req := tt.req
			req.Doc = scan.Document{PRs: []scan.PR{tt.pr}}
			got, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
			if err != nil {
				t.Fatalf("Run() unexpected error: %v", err)
			}
			if len(got.Results) != 1 || got.Results[0].Action != ActionWouldDispatch {
				t.Fatalf("results = %+v, want would_dispatch", got.Results)
			}
			prompt := got.Results[0].RenderedPrompt
			if !strings.Contains(prompt, tt.body) {
				t.Fatalf("wrong prompt kind, missing %q: %q", tt.body, prompt)
			}
			if !strings.HasSuffix(prompt, "\n\nNOTICE: x") {
				t.Fatalf("prompt does not end with suffix: %q", prompt)
			}
			if strings.HasSuffix(prompt, "\n\n\nNOTICE: x") {
				t.Fatalf("suffix not separated by exactly one blank line: %q", prompt)
			}
		})
	}
}

func TestRunLivePromptSuffixChangeStillDeduped(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.PromptSuffix = "NOTICE: x"
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	req := Request{Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}}}
	got, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}

	cfg.PromptSuffix = "NOTICE: y"
	got2, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionDispatchedUnresolved {
		t.Fatalf("second results = %+v, want dispatched_unresolved", got2.Results)
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls after suffix change = %d, want 1", h.promptN)
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

func TestRunLivePromptMatchedIdleIsDispatched(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{
			Status: herdr.PromptMatched,
			Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "idle"},
		}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched (herdr --wait matched idle/done)", got.Results)
	}
	if h.waitN != 0 {
		t.Fatalf("Wait calls = %d, want 0 (do not agent wait after a matched prompt)", h.waitN)
	}
	if h.sincePrompt != settleDebouncePolls {
		t.Fatalf("post-prompt AgentList calls = %d, want %d (debounce terminal idle)", h.sincePrompt, settleDebouncePolls)
	}
}

func TestRunLivePromptTimeoutIdleIsDispatched(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{
		lists:     [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts:   []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched (timeout snapshot idle/done)", got.Results)
	}
	if h.waitN != 0 {
		t.Fatalf("Wait calls = %d, want 0 (do not agent wait from idle)", h.waitN)
	}
	if h.sincePrompt != settleDebouncePolls {
		t.Fatalf("post-prompt AgentList calls = %d, want %d (debounce terminal idle)", h.sincePrompt, settleDebouncePolls)
	}
}

func TestRunLivePromptTimeoutWorkingWaitsThenDispatched(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{
		lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: [][]herdr.Agent{
			{workingAgent("w2:pC", "w2:tC")},
			{idleAgent("w2:pC", "w2:tC")},
			{idleAgent("w2:pC", "w2:tC")},
			{idleAgent("w2:pC", "w2:tC")},
		},
		prompts: []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
		waits: []herdr.PromptOutcome{{
			Status: herdr.PromptMatched,
			Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "idle"},
		}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched after agent wait", got.Results)
	}
	if h.waitN != 1 {
		t.Fatalf("Wait calls = %d, want 1", h.waitN)
	}
	if !slices.Equal(h.sawWaitUntil, []string{"idle", "done"}) {
		t.Fatalf("wait until = %v, want [idle done] (blocked not added)", h.sawWaitUntil)
	}
	if h.lastWaitTimeout <= 0 || h.lastWaitTimeout > cfg.DispatchTimeout {
		t.Fatalf("wait timeout = %s, want remaining time within dispatch_timeout_ms", h.lastWaitTimeout)
	}
}

func TestRunLivePromptTimeoutWorkingRemainingExpiredTimesOut(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.DispatchTimeout = 0
	h := &scriptHerdr{
		lists:     [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: [][]herdr.Agent{{workingAgent("w2:pC", "w2:tC")}},
		prompts:   []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if !errors.Is(err, ErrSettleTimeout) {
		t.Fatalf("error = %v, want ErrSettleTimeout (no remaining time)", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedTimeout {
		t.Fatalf("results = %+v, want dispatched_timeout", got.Results)
	}
	if h.waitN != 0 {
		t.Fatalf("Wait calls = %d, want 0 when remaining time is exhausted", h.waitN)
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
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionDispatchedUnresolved {
		t.Fatalf("second results = %+v, want dispatched_unresolved", got2.Results)
	}
	if got2.Results[0].Detail != "recorded at 2026-01-01T09:00:00Z" {
		t.Fatalf("detail = %q, want recorded time", got2.Results[0].Detail)
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls after dedupe = %d, want 1", h.promptN)
	}
}

func TestRunLiveMergeQueueFixWritesSHA(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.HeadSHA = "abc123def456"
	pr.Mergeable = "CONFLICTING"
	pr.Actions = []scan.Action{{
		Name:       scan.ActionMergeQueueFix,
		EventID:    "EVT",
		Class:      "our-code",
		CommentURL: "https://github.com/acme/widgets/pull/123#issuecomment-9",
	}}
	req := Request{Doc: scan.Document{PRs: []scan.PR{pr}}}
	got, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}
	if !strings.Contains(got.Results[0].RenderedPrompt, "fix the cause") {
		t.Fatalf("live merge-queue-fix used wrong template: %q", got.Results[0].RenderedPrompt)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.DedupedMergeQueueFix("acme/widgets#123", "abc123def456") {
		t.Fatalf("state after merge-queue-fix = %#v", st)
	}
	if st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("merge-queue-fix must not record comment ids: %#v", st)
	}
	if st.DedupedHead("acme/widgets#123", "abc123def456") || st.DedupedCIFix("acme/widgets#123", "abc123def456") {
		t.Fatalf("merge-queue-fix must not record other SHAs: %#v", st)
	}

	got2, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionSkippedDeduped {
		t.Fatalf("second results = %+v, want skipped_deduped while still conflicting", got2.Results)
	}
	if h.promptN != 1 {
		t.Fatalf("Prompt calls after SHA dedupe = %d, want 1", h.promptN)
	}

	pr.HeadSHA = "fff000aaa111"
	req.Doc.PRs[0] = pr
	got3, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("third Run() unexpected error: %v", err)
	}
	if len(got3.Results) != 1 || got3.Results[0].Action != ActionDispatched {
		t.Fatalf("changed SHA results = %+v, want dispatched", got3.Results)
	}
	if h.promptN != 2 {
		t.Fatalf("Prompt calls after SHA change = %d, want 2", h.promptN)
	}

	got4, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   req.Doc,
		Force: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("force Run() unexpected error: %v", err)
	}
	if len(got4.Results) != 1 || got4.Results[0].Action != ActionDispatched {
		t.Fatalf("force results = %+v, want dispatched", got4.Results)
	}
	if h.promptN != 3 {
		t.Fatalf("Prompt calls after force = %d, want 3", h.promptN)
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

func TestRunLiveRebaseRetriesWhenStillBehind(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.HeadSHA = "abc123def456"
	pr.MergeStateStatus = "BEHIND"
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

	got2, err := Run(context.Background(), h, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Rebase: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionDispatched {
		t.Fatalf("second results = %+v, want dispatched (still behind)", got2.Results)
	}
	if h.promptN != 2 {
		t.Fatalf("Prompt calls after behind retry = %d, want 2", h.promptN)
	}
}

func TestRunLiveRebaseForceRedispatches(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.HeadSHA = "abc123def456"
	pr.MergeStateStatus = "CLEAN"
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

	got3, err := Run(context.Background(), h, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Rebase: true,
		Force:  true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("force Run() unexpected error: %v", err)
	}
	if len(got3.Results) != 1 || got3.Results[0].Action != ActionDispatched {
		t.Fatalf("force results = %+v, want dispatched", got3.Results)
	}
	if h.promptN != 2 {
		t.Fatalf("Prompt calls after force = %d, want 2", h.promptN)
	}
}

func TestRunLiveCIFixWritesSHA(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.HeadSHA = "abc123def456"
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}
	if !strings.Contains(got.Results[0].RenderedPrompt, "Fix the failing CI") {
		t.Fatalf("live CI-fix used wrong template: %q", got.Results[0].RenderedPrompt)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.DedupedCIFix("acme/widgets#123", "abc123def456") {
		t.Fatalf("state after CI-fix = %#v", st)
	}
	if st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("CI-fix must not record comment ids: %#v", st)
	}
	if st.DedupedHead("acme/widgets#123", "abc123def456") {
		t.Fatalf("CI-fix must not record rebase head SHA: %#v", st)
	}

	got2, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
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
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
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

func TestRunLiveCIFixRetriesWhenStillFailing(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.HeadSHA = "abc123def456"
	pr.CIState = "failing"
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}

	got2, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionDispatched {
		t.Fatalf("second results = %+v, want dispatched (still failing)", got2.Results)
	}
	if h.promptN != 2 {
		t.Fatalf("Prompt calls after failing retry = %d, want 2", h.promptN)
	}
}

func TestRunLiveCIFixForceRedispatches(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.HeadSHA = "abc123def456"
	pr.CIState = "green"
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}

	got2, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("second Run() unexpected error: %v", err)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionSkippedDeduped {
		t.Fatalf("second results = %+v, want skipped_deduped", got2.Results)
	}

	got3, err := Run(context.Background(), h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr}},
		CIFix: true,
		Force: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("force Run() unexpected error: %v", err)
	}
	if len(got3.Results) != 1 || got3.Results[0].Action != ActionDispatched {
		t.Fatalf("force results = %+v, want dispatched", got3.Results)
	}
	if h.promptN != 2 {
		t.Fatalf("Prompt calls after force = %d, want 2", h.promptN)
	}
}

func TestRunLiveRebaseBlockedStopsAdvancing(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.WaitUntil = []string{"idle", "done", "blocked"}
	pr1 := fixtureEligiblePR()
	pr1.Unaddressed = false
	pr1.BlockingComments = nil
	pr2 := fixtureEligiblePR()
	pr2.Number = 124
	pr2.Unaddressed = false
	pr2.BlockingComments = nil
	pr2.HeadSHA = "bbb222"
	h := &scriptHerdr{
		lists:     [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: blockedSettlePost("w2:pC", "w2:tC"),
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

func TestRunLiveStallNeverStartsTimesOut(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.DispatchTimeout = 50 * time.Millisecond // this test waits for the settle to time out
	h := &scriptHerdr{
		lists:   [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		prompts: []herdr.PromptOutcome{{Status: herdr.PromptStalled}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if !errors.Is(err, ErrSettleTimeout) {
		t.Fatalf("error = %v, want ErrSettleTimeout (never picked up)", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedTimeout {
		t.Fatalf("results = %+v, want dispatched_timeout (never picked up)", got.Results)
	}
	if got.Results[0].PaneID == "" || got.Results[0].RenderedPrompt == "" {
		t.Fatalf("timeout item missing pane/prompt: %+v", got.Results[0])
	}
	if h.waitN != 0 {
		t.Fatalf("Wait calls = %d, want 0 (stalled is never-started, not a cue to wait)", h.waitN)
	}
	if h.sincePrompt != 0 {
		t.Fatalf("post-prompt AgentList calls = %d, want 0 (do not poll after stall)", h.sincePrompt)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("state after never-started timeout = %#v", st)
	}
}

func TestRunLiveWaitTimeoutStillWorkingTimesOut(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.DispatchTimeout = 50 * time.Millisecond // this test waits for the settle to time out
	h := &scriptHerdr{
		lists:     [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: [][]herdr.Agent{{workingAgent("w2:pC", "w2:tC")}, {workingAgent("w2:pC", "w2:tC")}},
		prompts:   []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
		waits:     []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
	}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if !errors.Is(err, ErrSettleTimeout) {
		t.Fatalf("error = %v, want ErrSettleTimeout", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedTimeout {
		t.Fatalf("results = %+v, want dispatched_timeout", got.Results)
	}
	if h.waitN != 1 {
		t.Fatalf("Wait calls = %d, want 1", h.waitN)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile() error = %v", err)
	}
	if !st.Deduped("acme/widgets#123", []string{"PRRC_widget"}) {
		t.Fatalf("state after wait timeout = %#v", st)
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
	cfg.WaitUntil = []string{"idle", "done", "blocked"}
	h := &scriptHerdr{
		lists:     [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: blockedSettlePost("w2:pC", "w2:tC"),
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
	cfg.WaitUntil = []string{"idle", "done", "blocked"}
	pr2 := fixtureEligiblePR()
	pr2.Number = 124
	pr2.BlockingComments = []scan.Comment{{CommentID: "PRRC_second"}}
	h := &scriptHerdr{
		lists:     [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: blockedSettlePost("w2:pC", "w2:tC"),
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
	cfg.WaitUntil = []string{"idle", "done", "blocked"}
	pr := fixtureEligiblePR()
	h := &scriptHerdr{
		lists:     [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}},
		postLists: blockedSettlePost("w2:pC", "w2:tC"),
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

	h.postLists = [][]herdr.Agent{
		{seqAgent("w2:pC", "w2:tC", "working", 2)},
		{seqAgent("w2:pC", "w2:tC", "idle", 3)},
		{seqAgent("w2:pC", "w2:tC", "idle", 3)},
		{seqAgent("w2:pC", "w2:tC", "idle", 3)},
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
	cfg.GateTimeout = 50 * time.Millisecond // this test waits for the gate to time out
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
	if got.Results[0].Action != ActionGateTimeout || got.Results[0].Number != 123 {
		t.Fatalf("first = %+v, want gate_timeout #123", got.Results[0])
	}
	if !strings.Contains(got.Results[0].Detail, "w2:tX") {
		t.Fatalf("first detail = %q, want waited-on tab w2:tX", got.Results[0].Detail)
	}
	if got.Results[1].Action != ActionQueued || got.Results[1].Number != 124 {
		t.Fatalf("second = %+v, want queued #124", got.Results[1])
	}
	if h.promptN != 0 {
		t.Fatalf("Prompt calls = %d, want 0 on gate timeout", h.promptN)
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on gate timeout")
	}
}

func TestRunLiveDirtyCheckoutIsGateUnsafe(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.Checkout = gitRepo(t)
	cfg.GateTimeout = time.Hour
	cfg.DispatchTimeout = time.Hour
	writeTestFile(t, filepath.Join(cfg.Checkout, "staged.txt"), "x\n")
	runGit(t, cfg.Checkout, "add", "staged.txt")
	pr2 := fixtureEligiblePR()
	pr2.Number = 124
	pr3 := fixtureEligiblePR()
	pr3.Number = 125
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR(), pr2, pr3}},
	}, fixtureNow)
	if !errors.Is(err, ErrDirtyTree) {
		t.Fatalf("error = %v, want ErrDirtyTree", err)
	}
	if len(got.Results) != 3 {
		t.Fatalf("results = %+v, want 3", got.Results)
	}
	if got.Results[0].Action != ActionGateUnsafe || got.Results[0].Number != 123 || got.Results[0].Detail != "dirty tree: 1 files" {
		t.Fatalf("first = %+v, want gate_unsafe #123 with dirty tree detail", got.Results[0])
	}
	for _, r := range got.Results[1:] {
		if r.Action != ActionQueued {
			t.Fatalf("later = %+v, want queued", r)
		}
	}
	if h.promptN != 0 {
		t.Fatalf("Prompt calls = %d, want 0 on a dirty checkout", h.promptN)
	}
	if _, err := os.Stat(store.Path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state file written on a dirty checkout")
	}
}

func TestRunDryRunDirtyCheckoutAnnotatesWouldDispatch(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.Checkout = gitRepo(t)
	writeTestFile(t, filepath.Join(cfg.Checkout, "a.txt"), "changed\n")
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionWouldDispatch || got.Results[0].Detail != "dirty tree: 1 files" {
		t.Fatalf("results = %+v, want would_dispatch with dirty tree detail", got.Results)
	}
	if h.promptN != 0 {
		t.Fatalf("Prompt calls = %d, want 0 on dry-run", h.promptN)
	}
}

func TestRunLiveBlockedOtherTabGateTimeout(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.GateTimeout = 50 * time.Millisecond // this test waits for the gate to time out
	cfg.ConcurrencyIgnoreBlocked = false
	blocked := fixtureEligiblePR()
	blocked.Number = 99
	blocked.Tab.TabID = "w2:tBlocked"
	blockedPane := "w2:pBlocked"
	blocked.Tab.PaneID = &blockedPane
	blocked.Tab.Label = "PROJ-99"
	blocked.Tab.AgentStatus = "blocked"
	target := fixtureEligiblePR()
	h := &scriptHerdr{lists: [][]herdr.Agent{{
		blockedAgent("w2:pBlocked", "w2:tBlocked"),
		idleAgent("w2:pC", "w2:tC"),
	}}}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{blocked, target}},
		PRs: []string{"acme/widgets#123"},
	}, fixtureNow)
	if !errors.Is(err, ErrTimeout) {
		t.Fatalf("error = %v, want ErrTimeout", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionGateTimeout {
		t.Fatalf("results = %+v, want gate_timeout", got.Results)
	}
	detail := got.Results[0].Detail
	if !strings.Contains(detail, "w2:tBlocked") {
		t.Fatalf("detail = %q, want waited-on tab", detail)
	}
	if !strings.Contains(detail, "PROJ-99") {
		t.Fatalf("detail = %q, want tab label", detail)
	}
	if !strings.Contains(detail, "blocked awaiting your input") {
		t.Fatalf("detail = %q, want blocked awaiting your input", detail)
	}
	if !strings.Contains(detail, "acme/widgets#123") {
		t.Fatalf("detail = %q, want target PR", detail)
	}
	if h.promptN != 0 {
		t.Fatalf("Prompt calls = %d, want 0 (do not inject while another tab is blocked)", h.promptN)
	}
	if h.n < 2 {
		t.Fatalf("AgentList calls = %d, want polling until timeout", h.n)
	}
}

func TestRunDryRunStripsBoilerplateButStillDispatches(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{regexp.MustCompile(`(?m)^Reply with /apply\.$`)}
	pr := fixtureEligiblePR()
	pr.BlockingComments[0].Body = "Fix the nil check.\n\nReply with /apply."
	pr.BlockingComments = append(pr.BlockingComments, scan.Comment{
		ThreadID:  "PRRT_bot",
		CommentID: "PRRC_bot",
		Author:    "review-bot",
		Path:      "src/widget.go",
		URL:       "https://github.com/acme/widgets/pull/123#discussion_r2",
		Body:      "Reply with /apply.",
	})
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch", got.Results)
	}
	prompt := got.Results[0].RenderedPrompt
	if !strings.Contains(prompt, "Fix the nil check.") {
		t.Fatalf("stripped prose missing: %q", prompt)
	}
	if strings.Contains(prompt, "Reply with /apply.") {
		t.Fatalf("boilerplate still in dry-run prompt: %q", prompt)
	}
	botURL := "https://github.com/acme/widgets/pull/123#discussion_r2"
	if !strings.Contains(prompt, "(boilerplate only; see "+botURL+")") {
		t.Fatalf("empty comment dropped: %q", prompt)
	}
	if !strings.Contains(prompt, "reviewer-login") || !strings.Contains(prompt, "review-bot") {
		t.Fatalf("comment no longer listed: %q", prompt)
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
	// Generous: a passing gate or a settled prompt returns at once, and
	// Bazel runs these tests under load, where the fake herdr
	// subprocess can take longer than 50ms to answer.
	cfg.GateTimeout = 5 * time.Second
	cfg.DispatchTimeout = 5 * time.Second
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

func TestUnresolvedCommentDedupeExpires(t *testing.T) {
	t.Parallel()

	t.Run("ttl", func(t *testing.T) {
		t.Parallel()
		cfg, store := liveCfg(t)
		cfg.DedupeTTL = 24 * time.Hour
		h := idleScript()
		pr := fixtureEligiblePR()
		req := Request{Doc: scan.Document{PRs: []scan.PR{pr}}}

		got := mustDispatchRun(t, h, store, cfg, req, fixtureNow)
		if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
			t.Fatalf("first = %+v, want dispatched", got.Results)
		}
		entry := mustLoadState(t, store)["acme/widgets#123"]
		if !slices.Equal(entry.DispatchedCommentIDs, []string{"PRRC_widget"}) {
			t.Fatalf("ids = %v, want PRRC_widget", entry.DispatchedCommentIDs)
		}
		if entry.DispatchedAt != "2026-01-01T09:00:00Z" || entry.DedupeScanCount != 0 {
			t.Fatalf("entry = %#v", entry)
		}

		inside := fixtureNow.Add(time.Hour)
		got = mustDispatchRun(t, h, store, cfg, req, inside)
		if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedUnresolved {
			t.Fatalf("inside window = %+v, want dispatched_unresolved", got.Results)
		}
		if got.Results[0].Detail != "recorded at 2026-01-01T09:00:00Z" {
			t.Fatalf("detail = %q, want recorded time", got.Results[0].Detail)
		}
		if h.promptN != 1 {
			t.Fatalf("prompts inside window = %d, want 1", h.promptN)
		}
		doc := scan.Document{PRs: []scan.PR{pr}}
		MarkUnresolved(&doc, mustLoadState(t, store), cfg, inside)
		if doc.PRs[0].Dispatch != ActionDispatchedUnresolved {
			t.Fatalf("scan marker = %q, want dispatched_unresolved", doc.PRs[0].Dispatch)
		}

		later := fixtureNow.Add(24 * time.Hour)
		got = mustDispatchRun(t, h, store, cfg, req, later)
		if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
			t.Fatalf("after ttl = %+v, want dispatched", got.Results)
		}
		if h.promptN != 2 {
			t.Fatalf("prompts after ttl = %d, want 2", h.promptN)
		}
		entry = mustLoadState(t, store)["acme/widgets#123"]
		if entry.DispatchedAt != later.UTC().Format(time.RFC3339) || entry.DedupeScanCount != 0 {
			t.Fatalf("refreshed entry = %#v", entry)
		}
	})

	t.Run("scans", func(t *testing.T) {
		t.Parallel()
		cfg, store := liveCfg(t)
		cfg.DedupeTTL = 24 * time.Hour
		cfg.DedupeScans = 2
		h := idleScript()
		pr := fixtureEligiblePR()
		req := Request{Doc: scan.Document{PRs: []scan.PR{pr}}}
		mustDispatchRun(t, h, store, cfg, req, fixtureNow)
		mustUnresolved(t, h, store, cfg, req, fixtureNow)
		mustUnresolved(t, h, store, cfg, req, fixtureNow)
		if h.promptN != 1 {
			t.Fatalf("prompts during scans = %d, want 1", h.promptN)
		}
		if got := mustLoadState(t, store)["acme/widgets#123"].DedupeScanCount; got != 2 {
			t.Fatalf("scan count = %d, want 2", got)
		}
		later := fixtureNow.Add(time.Hour)
		got := mustDispatchRun(t, h, store, cfg, req, later)
		if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
			t.Fatalf("after scans = %+v, want dispatched", got.Results)
		}
		entry := mustLoadState(t, store)["acme/widgets#123"]
		if entry.DedupeScanCount != 0 || entry.DispatchedAt != later.UTC().Format(time.RFC3339) {
			t.Fatalf("refreshed entry = %#v", entry)
		}
	})

	t.Run("new comment id", func(t *testing.T) {
		t.Parallel()
		cfg, store := liveCfg(t)
		cfg.DedupeTTL = 24 * time.Hour
		h := idleScript()
		pr := fixtureEligiblePR()
		req := Request{Doc: scan.Document{PRs: []scan.PR{pr}}}
		mustDispatchRun(t, h, store, cfg, req, fixtureNow)
		pr.BlockingComments[0].CommentID = "PRRC_new"
		req.Doc.PRs[0] = pr
		got := mustDispatchRun(t, h, store, cfg, req, fixtureNow.Add(time.Hour))
		if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
			t.Fatalf("new id = %+v, want dispatched", got.Results)
		}
		entry := mustLoadState(t, store)["acme/widgets#123"]
		if !slices.Equal(entry.DispatchedCommentIDs, []string{"PRRC_new"}) || entry.DedupeScanCount != 0 {
			t.Fatalf("entry = %#v", entry)
		}
	})

	t.Run("rebase", func(t *testing.T) {
		t.Parallel()
		assertModeStaysDeduped(t, func(pr *scan.PR) Request {
			pr.Unaddressed = false
			pr.BlockingComments = nil
			pr.HeadSHA = "abc123def456"
			pr.MergeStateStatus = "CLEAN"
			return Request{Doc: scan.Document{PRs: []scan.PR{*pr}}, Rebase: true}
		})
	})

	t.Run("ci-fix", func(t *testing.T) {
		t.Parallel()
		assertModeStaysDeduped(t, func(pr *scan.PR) Request {
			pr.Unaddressed = false
			pr.BlockingComments = nil
			pr.HeadSHA = "abc123def456"
			pr.CIState = "green"
			return Request{Doc: scan.Document{PRs: []scan.PR{*pr}}, CIFix: true}
		})
	})

	t.Run("merge-queue-fix", func(t *testing.T) {
		t.Parallel()
		assertModeStaysDeduped(t, func(pr *scan.PR) Request {
			pr.Unaddressed = false
			pr.BlockingComments = nil
			pr.HeadSHA = "abc123def456"
			pr.Actions = []scan.Action{{
				Name:       scan.ActionMergeQueueFix,
				EventID:    "EVT",
				Class:      "our-code",
				CommentURL: "https://github.com/acme/widgets/pull/123#issuecomment-9",
			}}
			return Request{Doc: scan.Document{PRs: []scan.PR{*pr}}}
		})
	})
}

func idleScript() *scriptHerdr {
	return &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
}

func mustDispatchRun(t *testing.T, h *scriptHerdr, store FileStore, cfg config.Config, req Request, now time.Time) Document {
	t.Helper()
	got, err := Run(context.Background(), h, store, cfg, req, now)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	return got
}

func mustLoadState(t *testing.T, store FileStore) State {
	t.Helper()
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatalf("LoadFile: %v", err)
	}
	return st
}

func mustUnresolved(t *testing.T, h *scriptHerdr, store FileStore, cfg config.Config, req Request, now time.Time) {
	t.Helper()
	got := mustDispatchRun(t, h, store, cfg, req, now)
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatchedUnresolved {
		t.Fatalf("results = %+v, want dispatched_unresolved", got.Results)
	}
}

func assertModeStaysDeduped(t *testing.T, build func(*scan.PR) Request) {
	t.Helper()
	cfg, store := liveCfg(t)
	h := idleScript()
	pr := fixtureEligiblePR()
	req := build(&pr)
	got := mustDispatchRun(t, h, store, cfg, req, fixtureNow)
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("first = %+v, want dispatched", got.Results)
	}
	got = mustDispatchRun(t, h, store, cfg, req, fixtureNow)
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedDeduped {
		t.Fatalf("second = %+v, want skipped_deduped", got.Results)
	}
	if h.promptN != 1 {
		t.Fatalf("prompts = %d, want 1", h.promptN)
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
