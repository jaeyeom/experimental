package dispatch

import (
	"context"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func addressedPR() scan.PR {
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	return pr
}

func dryRunOne(t *testing.T, cfg config.Config, req Request) Item {
	t.Helper()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	got, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 {
		t.Fatalf("results = %+v, want one", got.Results)
	}
	return got.Results[0]
}

func TestRunDryRunMergeRendersMergeTemplate(t *testing.T) {
	t.Parallel()

	got := dryRunOne(t, config.Defaults(), Request{
		Doc:   scan.Document{PRs: []scan.PR{addressedPR()}},
		Merge: true,
	})
	if got.Action != ActionWouldDispatch {
		t.Fatalf("result = %+v, want would_dispatch (merge skips the comment gate)", got)
	}
	if got.Kind != KindMerge {
		t.Fatalf("kind = %q, want %q", got.Kind, KindMerge)
	}
	prompt := got.RenderedPrompt
	for _, want := range []string{
		"PR #123 (https://github.com/acme/widgets/pull/123)",
		"Check out fix-widget",
		"Merge origin/main into fix-widget. Do not rebase.",
		"Push normally. Do not force-push.",
	} {
		if !strings.Contains(prompt, want) {
			t.Fatalf("merge prompt missing %q: %q", want, prompt)
		}
	}
	if strings.Contains(prompt, "--force-with-lease") {
		t.Fatalf("merge prompt force-pushes: %q", prompt)
	}
	if regexp.MustCompile(`(?m)^\d+\. Rebase`).MatchString(prompt) {
		t.Fatalf("merge prompt has a rebase step: %q", prompt)
	}
	if strings.Contains(prompt, "{") {
		t.Fatalf("unreplaced placeholder: %q", prompt)
	}
}

func TestRunDryRunMergePromptTemplateFromConfig(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.MergePromptTemplate = "custom merge #{number} onto {base}"
	got := dryRunOne(t, cfg, Request{Doc: scan.Document{PRs: []scan.PR{addressedPR()}}, Merge: true})
	if got.RenderedPrompt != "custom merge #123 onto main" {
		t.Fatalf("prompt = %q", got.RenderedPrompt)
	}
}

func TestRunDryRunMergeAppendsPromptSuffix(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.PromptSuffix = "NOTICE: x"
	got := dryRunOne(t, cfg, Request{Doc: scan.Document{PRs: []scan.PR{addressedPR()}}, Merge: true})
	if !strings.HasSuffix(got.RenderedPrompt, "Do not touch other PRs.\n\nNOTICE: x") {
		t.Fatalf("suffix not appended after merge prompt: %q", got.RenderedPrompt)
	}
}

func TestRunDryRunKindOnEveryResult(t *testing.T) {
	t.Parallel()

	mergeQueue := addressedPR()
	mergeQueue.Actions = []scan.Action{{Name: scan.ActionMergeQueueFix, CommentURL: "https://example.com/rca"}}
	tests := []struct {
		name string
		pr   scan.PR
		req  Request
		want string
	}{
		{name: "comment", pr: fixtureEligiblePR(), want: KindComment},
		{name: "comment skipped", pr: addressedPR(), want: KindComment},
		{name: "rebase", pr: addressedPR(), req: Request{Rebase: true}, want: KindRebase},
		{name: "merge", pr: addressedPR(), req: Request{Merge: true}, want: KindMerge},
		{name: "ci-fix", pr: addressedPR(), req: Request{CIFix: true}, want: KindCIFix},
		{name: "merge-queue-fix", pr: mergeQueue, want: KindMergeQueueFix},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			req := tt.req
			req.Doc = scan.Document{PRs: []scan.PR{tt.pr}}
			if got := dryRunOne(t, config.Defaults(), req); got.Kind != tt.want {
				t.Fatalf("kind = %q, want %q (%+v)", got.Kind, tt.want, got)
			}
		})
	}
}

func TestRunDryRunDispatchCatchupSwitchesMergeQueueConflictSentence(t *testing.T) {
	t.Parallel()

	pr := addressedPR()
	pr.Actions = []scan.Action{{Name: scan.ActionMergeQueueFix, CommentURL: "https://example.com/rca"}}
	req := Request{Doc: scan.Document{PRs: []scan.PR{pr}}}

	def := dryRunOne(t, config.Defaults(), req).RenderedPrompt
	if !strings.Contains(def, "rebase onto origin/main, regenerate lockfiles, and push.") {
		t.Fatalf("default merge-queue-fix prompt lost rebase wording: %q", def)
	}

	cfg := config.Defaults()
	cfg.DispatchCatchup = "merge"
	got := dryRunOne(t, cfg, req).RenderedPrompt
	if !strings.Contains(got, "merge origin/main into fix-widget, regenerate lockfiles, and push normally. Do not rebase or force-push.") {
		t.Fatalf("merge catch-up wording missing: %q", got)
	}
	if strings.Contains(got, "rebase onto") {
		t.Fatalf("merge catch-up still asks for a rebase: %q", got)
	}
	if !strings.Contains(got, "https://example.com/rca") {
		t.Fatalf("missing RCA comment url: %q", got)
	}
}

func TestRunDryRunDispatchCatchupKeepsExplicitRebase(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DispatchCatchup = "merge"
	got := dryRunOne(t, cfg, Request{Doc: scan.Document{PRs: []scan.PR{addressedPR()}}, Rebase: true})
	if got.Kind != KindRebase || !strings.Contains(got.RenderedPrompt, "--force-with-lease") {
		t.Fatalf("--rebase under dispatch_catchup=merge = %+v", got)
	}
}

func TestRunLiveMergeDedupeUsesOwnField(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	pr := addressedPR()
	pr.HeadSHA = "abc123def456"
	pr.MergeStateStatus = "CLEAN"
	run := func(req Request) Item {
		t.Helper()
		req.Doc = scan.Document{PRs: []scan.PR{pr}}
		got, err := Run(context.Background(), h, store, cfg, req, fixtureNow)
		if err != nil {
			t.Fatalf("Run() unexpected error: %v", err)
		}
		if len(got.Results) != 1 {
			t.Fatalf("results = %+v", got.Results)
		}
		return got.Results[0]
	}

	if got := run(Request{Merge: true}); got.Action != ActionDispatched || got.Kind != KindMerge {
		t.Fatalf("first merge = %+v, want dispatched merge", got)
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatal(err)
	}
	entry := st["acme/widgets#123"]
	if entry.DispatchedMergeSHA != "abc123def456" || entry.DispatchedHeadSHA != "" {
		t.Fatalf("state after merge = %#v, want only dispatched_merge_sha", entry)
	}

	got := run(Request{Merge: true})
	if got.Action != ActionSkippedDeduped || got.Detail != "recorded at abc123def456, merge_state=CLEAN" {
		t.Fatalf("second merge = %+v, want skipped_deduped", got)
	}

	if got := run(Request{Rebase: true}); got.Action != ActionDispatched {
		t.Fatalf("rebase after merge = %+v, want dispatched (own field)", got)
	}

	pr.MergeStateStatus = "BEHIND"
	if got := run(Request{Merge: true}); got.Action != ActionDispatched {
		t.Fatalf("merge while BEHIND = %+v, want dispatched", got)
	}
	pr.MergeStateStatus = "CLEAN"
	if got := run(Request{Merge: true, Force: true}); got.Action != ActionDispatched {
		t.Fatalf("forced merge = %+v, want dispatched", got)
	}
	if h.promptN != 4 {
		t.Fatalf("Prompt calls = %d, want 4", h.promptN)
	}
}

func TestRebaseSuffixConflict(t *testing.T) {
	t.Parallel()

	tests := []struct {
		suffix string
		want   string
	}{
		{suffix: "", want: ""},
		{suffix: "NOTICE: be brief", want: ""},
		{suffix: "Merge the base branch and push normally.", want: "merge"},
		{suffix: "Please DO NOT FORCE-PUSH.", want: "do not force-push"},
		{suffix: "no force pushes here", want: "no force"},
		{suffix: "don't force push", want: "don't force push"},
		{suffix: "emerged", want: ""},
	}
	for _, tt := range tests {
		if got := RebaseSuffixConflict(tt.suffix); got != tt.want {
			t.Errorf("RebaseSuffixConflict(%q) = %q, want %q", tt.suffix, got, tt.want)
		}
	}
}
