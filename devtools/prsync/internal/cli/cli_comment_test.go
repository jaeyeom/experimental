package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestCommentBadPR(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--pr", "not-a-pr", "--body", "please retry"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "invalid --pr") {
		t.Fatalf("stderr = %q", stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout = %q, want empty", stdout.String())
	}
}

func TestCommentPRAndAll(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--pr", "acme/widgets#1", "--all", "--body", "please retry"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "cannot combine --pr and --all") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestCommentRequiresBody(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--pr", "acme/widgets#1"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "--body") {
		t.Fatalf("stderr = %q, want --body requirement", stderr.String())
	}
}

func TestCommentEmptyBody(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--body", "  "}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "--body must not be empty") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestCommentUnknownFlag(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--not-a-flag"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "unknown flag") {
		t.Fatalf("stderr = %q, want unknown flag", stderr.String())
	}
}

func TestCommentStdinJSONDryRun(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "comment")
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", sentinel)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")

	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--pr", "acme/widgets#123", "--body", "please retry"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(got.Results) != 1 {
		t.Fatalf("results = %+v", got.Results)
	}
	if got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("action = %q, want would_dispatch", got.Results[0].Action)
	}
	if got.Results[0].RenderedPrompt != "please retry" {
		t.Fatalf("rendered_prompt = %q, want please retry", got.Results[0].RenderedPrompt)
	}
	if got.Results[0].PaneID != "" {
		t.Fatalf("pane_id = %q, want empty", got.Results[0].PaneID)
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("gh pr comment was invoked on dry-run")
	}
}

func TestCommentStdinOffMachineNoTab(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"herdr_bin=" + herdrBin,
		"author=alice",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")
	doc := stdinEligibleDoc()
	doc.PRs[0].Tab = nil
	raw := mustScanJSON(t, doc)
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "nudge"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch without a tab", got.Results)
	}
	if got.Results[0].RenderedPrompt != "nudge" {
		t.Fatalf("rendered_prompt = %q, want nudge", got.Results[0].RenderedPrompt)
	}
}

func TestCommentGoPostsViaGH(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "comment")
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", sentinel)
	promptSentinel := filepath.Join(t.TempDir(), "prompt")
	t.Setenv("HERDR_FAKE_PROMPT_SENTINEL", promptSentinel)
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "working")
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")

	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "please retry", "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if got.DryRun {
		t.Fatal("dry_run = true, want false")
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}
	if got.Results[0].RenderedPrompt != "please retry" {
		t.Fatalf("rendered_prompt = %q, want please retry", got.Results[0].RenderedPrompt)
	}
	body, err := os.ReadFile(sentinel) //nolint:gosec // test sentinel
	if err != nil {
		t.Fatalf("gh pr comment was not invoked: %v", err)
	}
	gotArgs := strings.TrimSpace(string(body))
	if !strings.Contains(gotArgs, "comment 123") || !strings.Contains(gotArgs, "--repo acme/widgets") || !strings.Contains(gotArgs, "--body please retry") {
		t.Fatalf("gh args = %q", gotArgs)
	}
	if _, err := os.Stat(promptSentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("herdr agent prompt was invoked")
	}
}

func TestCommentDoesNotNeedHerdr(t *testing.T) {
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=prsync-test-missing-gh",
		"herdr_bin=prsync-test-missing-herdr",
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")
	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "please retry"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch without herdr", got.Results)
	}
}

func TestCommentAllScopesToScanDoc(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"herdr_bin=" + herdrBin,
		"author=alice",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")
	doc := stdinEligibleDoc()
	extra := doc.PRs[0]
	extra.Repo = "acme/gizmos"
	extra.Number = 50
	extra.Tab = nil
	doc.PRs = append(doc.PRs, extra)
	raw := mustScanJSON(t, doc)
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--all", "--config", cfgPath, "--body", "hello"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	for _, item := range got.Results {
		if item.Action != dispatch.ActionWouldDispatch || item.RenderedPrompt != "hello" {
			t.Fatalf("item = %+v, want would_dispatch hello", item)
		}
	}
	if got.Results[0].Repo != "acme/gizmos" || got.Results[1].Repo != "acme/widgets" {
		t.Fatalf("results = %+v, want gizmos then widgets", got.Results)
	}
}

func TestCommentInternalScanDryRun(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "comment")
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", sentinel)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--config", cfgPath, "--body", "please retry"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("results = %+v", got.Results)
	}
	if got.Results[0].Repo != "acme/widgets" || got.Results[0].Number != 123 {
		t.Fatalf("result = %+v, want acme/widgets#123", got.Results[0])
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("gh pr comment was invoked on dry-run")
	}
}

func TestCommentNotFoundPR(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"herdr_bin=" + herdrBin,
		"author=alice",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")
	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{
		"comment", "--stdin", "--config", cfgPath, "--body", "please retry",
		"--pr", "acme/widgets#123",
		"--pr", "acme/missing#9",
	}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	actions := map[string]string{}
	for _, r := range got.Results {
		actions[r.Repo] = r.Action
	}
	if actions["acme/missing"] != dispatch.ActionSkippedNotFound {
		t.Fatalf("actions = %v", actions)
	}
	if actions["acme/widgets"] != dispatch.ActionWouldDispatch {
		t.Fatalf("actions = %v", actions)
	}
}

func TestCommentGoThenDeduped(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "comment")
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", sentinel)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
	}, "\n")+"\n")

	doc := stdinEligibleDoc()
	doc.PRs[0].HeadSHA = "abc123def456"
	raw := mustScanJSON(t, doc)

	restore := swapStdin(t, string(raw))
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "/ci", "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("first --go exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if got.DryRun {
		t.Fatal("dry_run = true, want false")
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("first results = %+v, want dispatched", got.Results)
	}
	if _, err := os.Stat(statePath); err != nil {
		t.Fatalf("state_file missing after --go: %v", err)
	}
	if err := os.Remove(sentinel); err != nil {
		t.Fatalf("remove sentinel: %v", err)
	}

	restore = swapStdin(t, string(raw))
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "/ci", "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("second --go exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got = decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("second results = %+v, want skipped_deduped", got.Results)
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("gh pr comment was invoked on deduped re-run")
	}

	restore = swapStdin(t, string(raw))
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "/ci"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("dry-run exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got = decodeDispatch(t, stdout.Bytes())
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("dry-run results = %+v, want skipped_deduped", got.Results)
	}
}

func TestCommentGoNewSHAPostsAgain(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "comment")
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", sentinel)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
	}, "\n")+"\n")

	doc := stdinEligibleDoc()
	doc.PRs[0].HeadSHA = "abc123def456"
	raw := mustScanJSON(t, doc)
	restore := swapStdin(t, string(raw))
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "/ci", "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("first --go exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	if err := os.Remove(sentinel); err != nil {
		t.Fatalf("remove sentinel: %v", err)
	}

	doc.PRs[0].HeadSHA = "fff000aaa111"
	raw = mustScanJSON(t, doc)
	restore = swapStdin(t, string(raw))
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "/ci", "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("changed SHA --go exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("changed SHA results = %+v, want dispatched", got.Results)
	}
	if _, err := os.Stat(sentinel); err != nil {
		t.Fatalf("gh pr comment was not invoked after SHA change: %v", err)
	}
}

func TestCommentAllowDuplicatePostsAgain(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "comment")
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", sentinel)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
	}, "\n")+"\n")

	doc := stdinEligibleDoc()
	doc.PRs[0].HeadSHA = "abc123def456"
	raw := mustScanJSON(t, doc)

	restore := swapStdin(t, string(raw))
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "/ci", "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("first --go exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	if err := os.Remove(sentinel); err != nil {
		t.Fatalf("remove sentinel: %v", err)
	}

	restore = swapStdin(t, string(raw))
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--stdin", "--config", cfgPath, "--body", "/ci", "--go", "--allow-duplicate"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("--allow-duplicate exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("--allow-duplicate results = %+v, want dispatched", got.Results)
	}
	if _, err := os.Stat(sentinel); err != nil {
		t.Fatalf("gh pr comment was not invoked with --allow-duplicate: %v", err)
	}
}

func TestCommentRepostWaivesFlagValidation(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name string
		args []string
		want string
	}{
		{name: "with body", args: []string{"comment", "--repost-waives", "--body", "hi"}, want: "cannot combine --repost-waives and --body"},
		{name: "with allow-duplicate", args: []string{"comment", "--repost-waives", "--allow-duplicate"}, want: "cannot combine --repost-waives and --allow-duplicate"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			var stdout, stderr bytes.Buffer
			code := Execute(context.Background(), tc.args, &stdout, &stderr, executor.NewBasicExecutor())
			if code != ExitUsage || !strings.Contains(stderr.String(), tc.want) {
				t.Fatalf("code=%d stderr=%q, want ExitUsage with %q", code, stderr.String(), tc.want)
			}
		})
	}
}

func TestScanAndCommentRepostWaivesEndToEnd(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not found")
	}

	// Build a real local temporary git repo with a clean base merge.
	repoDir := t.TempDir()
	runGitTest := func(args ...string) string {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", repoDir}, args...)...) //nolint:gosec // test helper
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
		return strings.TrimSpace(string(out))
	}
	writeRepoFile := func(name, content string) {
		t.Helper()
		if err := os.WriteFile(filepath.Join(repoDir, name), []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	runGitTest("init", "-q", "-b", "main")
	runGitTest("config", "user.email", "test@example.com")
	runGitTest("config", "user.name", "Test User")
	runGitTest("remote", "add", "origin", "https://github.com/acme/widgets.git")
	writeRepoFile("base.txt", "base0\n")
	runGitTest("add", ".")
	runGitTest("commit", "-q", "-m", "base0")

	runGitTest("checkout", "-q", "-b", "fix-widget")
	writeRepoFile("feat.txt", "feat1\n")
	runGitTest("add", "feat.txt")
	runGitTest("commit", "-q", "-m", "feat1")
	oldHead := runGitTest("rev-parse", "HEAD")

	runGitTest("checkout", "-q", "main")
	writeRepoFile("base.txt", "base1\n")
	runGitTest("add", "base.txt")
	runGitTest("commit", "-q", "-m", "base1")
	baseTip := runGitTest("rev-parse", "HEAD")

	runGitTest("checkout", "-q", "fix-widget")
	runGitTest("merge", "-q", "--no-ff", "--no-edit", "main")
	newHead := runGitTest("rev-parse", "HEAD")

	ghBin, herdrBin := fixtureBins(t)
	t.Chdir(repoDir)

	t0 := time.Date(2026, 2, 1, 10, 0, 0, 0, time.UTC)
	waiveBody := "/waive\n  known flake in ci/flake\nsee ticket PROJ-123\n"
	fixtureDir := t.TempDir()
	searchFile := filepath.Join(fixtureDir, "search.json")
	viewFile := filepath.Join(fixtureDir, "view.json")
	prViewFile := filepath.Join(fixtureDir, "pr-view.json")
	timelineFile := filepath.Join(fixtureDir, "timeline.json")

	searchPayload := map[string]any{
		"data": map[string]any{
			"search": map[string]any{
				"nodes": []any{
					map[string]any{
						"number":           123,
						"title":            "[PROJ-123] Fix the widget",
						"url":              "https://github.com/acme/widgets/pull/123",
						"state":            "OPEN",
						"baseRefName":      "main",
						"headRefName":      "fix-widget",
						"headRefOid":       newHead,
						"updatedAt":        "2026-02-01T11:00:00Z",
						"mergeable":        "MERGEABLE",
						"mergeStateStatus": "CLEAN",
						"isDraft":          false,
						"reviewDecision":   "APPROVED",
						"reviewRequests":   map[string]any{"nodes": []any{}},
					},
				},
			},
		},
	}
	viewPayload := map[string]any{
		"data": map[string]any{
			"repository": map[string]any{
				"pullRequest": map[string]any{
					"statusCheckRollup": map[string]any{
						"contexts": map[string]any{
							"nodes": []any{
								map[string]any{"name": "ci/flake", "status": "COMPLETED", "conclusion": "FAILURE"},
							},
						},
					},
				},
			},
		},
	}
	prViewPayload := map[string]any{
		"statusCheckRollup": []any{
			map[string]any{"name": "ci/flake", "status": "COMPLETED", "conclusion": "FAILURE"},
		},
	}
	timelinePayload := map[string]any{
		"data": map[string]any{
			"repository": map[string]any{
				"pullRequest": map[string]any{
					"comments": map[string]any{
						"pageInfo": map[string]any{"hasNextPage": false, "endCursor": ""},
						"nodes": []any{
							map[string]any{
								"id":         "IC_1",
								"databaseId": 777,
								"author":     map[string]any{"login": "maintainer"},
								"body":       waiveBody,
								"createdAt":  t0.Add(5 * time.Minute).Format(time.RFC3339),
								"url":        "https://github.com/acme/widgets/pull/123#issuecomment-777",
							},
						},
					},
					"commits": map[string]any{
						"pageInfo": map[string]any{"hasNextPage": false, "endCursor": ""},
						"nodes": []any{
							map[string]any{"commit": map[string]any{
								"oid": oldHead, "committedDate": t0.Format(time.RFC3339), "pushedDate": t0.Format(time.RFC3339),
								"parents": map[string]any{"nodes": []any{}},
							}},
							map[string]any{"commit": map[string]any{
								"oid": newHead, "committedDate": t0.Add(10 * time.Minute).Format(time.RFC3339), "pushedDate": t0.Add(10 * time.Minute).Format(time.RFC3339),
								"parents": map[string]any{"nodes": []any{map[string]any{"oid": oldHead}, map[string]any{"oid": baseTip}}},
							}},
						},
					},
					"timelineItems": map[string]any{
						"pageInfo": map[string]any{"hasNextPage": false, "endCursor": ""},
						"nodes":    []any{},
					},
				},
			},
		},
	}
	for p, v := range map[string]any{
		searchFile:   searchPayload,
		viewFile:     viewPayload,
		prViewFile:   prViewPayload,
		timelineFile: timelinePayload,
	} {
		data, err := json.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, data, 0o600); err != nil {
			t.Fatal(err)
		}
	}

	bodyFile := filepath.Join(t.TempDir(), "posted-body.txt")
	t.Setenv("GH_FAKE_OPEN_PR_SEARCH_FILE", searchFile)
	t.Setenv("GH_FAKE_OPEN_PR_VIEW_FILE", viewFile)
	t.Setenv("GH_FAKE_PR_VIEW_FILE", prViewFile)
	t.Setenv("GH_FAKE_WAIVE_TIMELINE_FILE", timelineFile)
	t.Setenv("GH_FAKE_COMMENT_BODY_FILE", bodyFile)
	t.Setenv("GH_FAKE_COMMENT_URL", "https://github.com/acme/widgets/pull/123#issuecomment-888")

	statePath := filepath.Join(t.TempDir(), "state.json")
	ledgerPath := filepath.Join(t.TempDir(), "ledger.jsonl")
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
		"dispatch_ledger_file=" + ledgerPath,
		"waive_comment_prefix=/waive",
		"waive_authors=maintainer",
		"waive_checks=ci/flake",
		"waive_repost=true",
		"waive_max_per_pr=3",
	}, "\n")+"\n")

	// 1. Scan reports JSON + text without writing anything.
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath, "--json"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("scan exit = %d, stderr=%q", code, stderr.String())
	}
	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("decode scan: %v", err)
	}
	if doc.PRs[0].StaleWaive == nil || doc.PRs[0].StaleWaive.Class != scan.WaiveClassBaseOnly {
		t.Fatalf("stale_waive = %+v", doc.PRs[0].StaleWaive)
	}
	if !slices.Equal(doc.PRs[0].StaleWaive.FailingChecks, []string{"ci/flake"}) {
		t.Fatalf("failing_checks = %v, want [ci/flake]", doc.PRs[0].StaleWaive.FailingChecks)
	}
	if !strings.Contains(stderr.String(), "base-only") || !strings.Contains(stderr.String(), oldHead) || !strings.Contains(stderr.String(), newHead) || !strings.Contains(stderr.String(), "failing=ci/flake") {
		t.Fatalf("scan stderr = %q", stderr.String())
	}
	if _, err := os.Stat(bodyFile); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("scan posted comment")
	}
	if _, err := os.Stat(ledgerPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("scan wrote ledger")
	}

	// 2. Comment dry-run prints what would be posted and where, without posting.
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--repost-waives", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("dry-run comment exit = %d, stderr=%q", code, stderr.String())
	}
	dry := decodeDispatch(t, stdout.Bytes())
	if !dry.DryRun || len(dry.Results) != 1 || dry.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("dry results = %+v", dry)
	}
	if dry.Results[0].RenderedPrompt != waiveBody {
		t.Fatalf("dry rendered_prompt = %q, want %q", dry.Results[0].RenderedPrompt, waiveBody)
	}
	if !strings.Contains(stderr.String(), "acme/widgets#123") || !strings.Contains(stderr.String(), waiveBody) {
		t.Fatalf("dry stderr = %q", stderr.String())
	}
	if _, err := os.Stat(bodyFile); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("dry-run comment posted via gh")
	}

	// 3. Comment --go posts byte-for-byte copy and writes ledger.
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--repost-waives", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("live comment exit = %d, stderr=%q", code, stderr.String())
	}
	live := decodeDispatch(t, stdout.Bytes())
	if live.DryRun || len(live.Results) != 1 || live.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("live results = %+v", live)
	}
	postedBytes, err := os.ReadFile(bodyFile) //nolint:gosec // test sentinel
	if err != nil {
		t.Fatalf("read posted body: %v", err)
	}
	if string(postedBytes) != waiveBody {
		t.Fatalf("posted bytes = %q, want byte-identical %q", string(postedBytes), waiveBody)
	}

	// 4. Ledger CLI shows the re-post with PR, old head, new head, and comment ID.
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"ledger", "--config", cfgPath, "--pr", "acme/widgets#123", "--kind", ledger.KindWaiveRepost}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("ledger text exit = %d, stderr=%q", code, stderr.String())
	}
	for _, want := range []string{"acme/widgets#123", ledger.KindWaiveRepost, oldHead, newHead, "888"} {
		if !strings.Contains(stdout.String(), want) {
			t.Fatalf("ledger text = %q, missing %q", stdout.String(), want)
		}
	}

	// 5. Re-running --go dedupes on the same head (even if state.json is deleted).
	_ = os.Remove(statePath)
	_ = os.Remove(bodyFile)
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--repost-waives", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("rerun comment exit = %d, stderr=%q", code, stderr.String())
	}
	rerun := decodeDispatch(t, stdout.Bytes())
	if len(rerun.Results) != 1 || rerun.Results[0].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("rerun results = %+v, want skipped_deduped", rerun.Results)
	}
	if _, err := os.Stat(bodyFile); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("deduped rerun posted via gh")
	}

	// 6. When waive_max_per_pr=1 and 1 repost is already in the ledger, a new base-only head hits the cap.
	runGitTest("checkout", "-q", "main")
	writeRepoFile("base.txt", "base2\n")
	runGitTest("add", "base.txt")
	runGitTest("commit", "-q", "-m", "base2")
	baseTip2 := runGitTest("rev-parse", "HEAD")

	runGitTest("checkout", "-q", "fix-widget")
	runGitTest("merge", "-q", "--no-ff", "--no-edit", "main")
	newHead2 := runGitTest("rev-parse", "HEAD")

	searchPayload["data"].(map[string]any)["search"].(map[string]any)["nodes"].([]any)[0].(map[string]any)["headRefOid"] = newHead2
	searchPayload["data"].(map[string]any)["search"].(map[string]any)["nodes"].([]any)[0].(map[string]any)["updatedAt"] = "2026-02-01T12:00:00Z"
	prNodes := timelinePayload["data"].(map[string]any)["repository"].(map[string]any)["pullRequest"].(map[string]any)
	commitsMap := prNodes["commits"].(map[string]any)
	commitsMap["nodes"] = append(commitsMap["nodes"].([]any), map[string]any{"commit": map[string]any{
		"oid": newHead2, "committedDate": t0.Add(20 * time.Minute).Format(time.RFC3339), "pushedDate": t0.Add(20 * time.Minute).Format(time.RFC3339),
		"parents": map[string]any{"nodes": []any{map[string]any{"oid": newHead}, map[string]any{"oid": baseTip2}}},
	}})
	for p, v := range map[string]any{searchFile: searchPayload, timelineFile: timelinePayload} {
		data, err := json.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, data, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	cappedCfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
		"dispatch_ledger_file=" + ledgerPath,
		"waive_comment_prefix=/waive",
		"waive_authors=maintainer",
		"waive_checks=ci/flake",
		"waive_repost=true",
		"waive_max_per_pr=1",
	}, "\n")+"\n")
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--repost-waives", "--go", "--config", cappedCfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("capped comment exit = %d, stderr=%q", code, stderr.String())
	}
	capped := decodeDispatch(t, stdout.Bytes())
	if len(capped.Results) != 1 || capped.Results[0].Action != dispatch.ActionSkippedWaiveCap {
		t.Fatalf("capped results = %+v, want skipped_waive_cap", capped.Results)
	}
	if _, err := os.Stat(bodyFile); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("capped run posted via gh")
	}
}

func TestScanAndCommentAuthorChangesNeverReposted(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not found")
	}

	repoDir := t.TempDir()
	runGitTest := func(args ...string) string {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", repoDir}, args...)...) //nolint:gosec // test helper
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
		return strings.TrimSpace(string(out))
	}
	writeRepoFile := func(name, content string) {
		t.Helper()
		if err := os.WriteFile(filepath.Join(repoDir, name), []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	runGitTest("init", "-q", "-b", "main")
	runGitTest("config", "user.email", "test@example.com")
	runGitTest("config", "user.name", "Test User")
	runGitTest("remote", "add", "origin", "https://github.com/acme/widgets.git")
	writeRepoFile("base.txt", "base0\n")
	runGitTest("add", ".")
	runGitTest("commit", "-q", "-m", "base0")

	runGitTest("checkout", "-q", "-b", "fix-widget")
	writeRepoFile("feat.txt", "feat1\n")
	runGitTest("add", "feat.txt")
	runGitTest("commit", "-q", "-m", "feat1")
	oldHead := runGitTest("rev-parse", "HEAD")

	runGitTest("checkout", "-q", "main")
	writeRepoFile("base.txt", "base1\n")
	runGitTest("add", "base.txt")
	runGitTest("commit", "-q", "-m", "base1")
	baseTip := runGitTest("rev-parse", "HEAD")

	runGitTest("checkout", "-q", "fix-widget")
	runGitTest("merge", "-q", "--no-ff", "--no-edit", "main")
	mergeHead := runGitTest("rev-parse", "HEAD")

	writeRepoFile("feat.txt", "feat2\n")
	runGitTest("add", "feat.txt")
	runGitTest("commit", "-q", "-m", "author commit after merge")
	newHead := runGitTest("rev-parse", "HEAD")

	ghBin, herdrBin := fixtureBins(t)
	t.Chdir(repoDir)

	t0 := time.Date(2026, 2, 1, 10, 0, 0, 0, time.UTC)
	fixtureDir := t.TempDir()
	searchFile := filepath.Join(fixtureDir, "search.json")
	prViewFile := filepath.Join(fixtureDir, "pr-view.json")
	timelineFile := filepath.Join(fixtureDir, "timeline.json")

	searchPayload := map[string]any{
		"data": map[string]any{
			"search": map[string]any{
				"nodes": []any{
					map[string]any{
						"number":           123,
						"title":            "[PROJ-123] Fix the widget",
						"url":              "https://github.com/acme/widgets/pull/123",
						"state":            "OPEN",
						"baseRefName":      "main",
						"headRefName":      "fix-widget",
						"headRefOid":       newHead,
						"updatedAt":        "2026-02-01T11:00:00Z",
						"mergeable":        "MERGEABLE",
						"mergeStateStatus": "CLEAN",
						"isDraft":          false,
						"reviewDecision":   "APPROVED",
						"reviewRequests":   map[string]any{"nodes": []any{}},
					},
				},
			},
		},
	}
	prViewPayload := map[string]any{
		"statusCheckRollup": []any{
			map[string]any{"name": "ci/flake", "status": "COMPLETED", "conclusion": "FAILURE"},
		},
	}
	timelinePayload := map[string]any{
		"data": map[string]any{
			"repository": map[string]any{
				"pullRequest": map[string]any{
					"comments": map[string]any{
						"pageInfo": map[string]any{"hasNextPage": false, "endCursor": ""},
						"nodes": []any{
							map[string]any{
								"id":         "IC_1",
								"databaseId": 777,
								"author":     map[string]any{"login": "maintainer"},
								"body":       "/waive\nknown flake\n",
								"createdAt":  t0.Add(5 * time.Minute).Format(time.RFC3339),
								"url":        "https://github.com/acme/widgets/pull/123#issuecomment-777",
							},
						},
					},
					"commits": map[string]any{
						"pageInfo": map[string]any{"hasNextPage": false, "endCursor": ""},
						"nodes": []any{
							map[string]any{"commit": map[string]any{
								"oid": oldHead, "committedDate": t0.Format(time.RFC3339), "pushedDate": t0.Format(time.RFC3339),
								"parents": map[string]any{"nodes": []any{}},
							}},
							map[string]any{"commit": map[string]any{
								"oid": mergeHead, "committedDate": t0.Add(10 * time.Minute).Format(time.RFC3339), "pushedDate": t0.Add(10 * time.Minute).Format(time.RFC3339),
								"parents": map[string]any{"nodes": []any{map[string]any{"oid": oldHead}, map[string]any{"oid": baseTip}}},
							}},
							map[string]any{"commit": map[string]any{
								"oid": newHead, "committedDate": t0.Add(15 * time.Minute).Format(time.RFC3339), "pushedDate": t0.Add(15 * time.Minute).Format(time.RFC3339),
								"parents": map[string]any{"nodes": []any{map[string]any{"oid": mergeHead}}},
							}},
						},
					},
					"timelineItems": map[string]any{
						"pageInfo": map[string]any{"hasNextPage": false, "endCursor": ""},
						"nodes":    []any{},
					},
				},
			},
		},
	}
	for p, v := range map[string]any{searchFile: searchPayload, prViewFile: prViewPayload, timelineFile: timelinePayload} {
		data, err := json.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, data, 0o600); err != nil {
			t.Fatal(err)
		}
	}

	sentinel := filepath.Join(t.TempDir(), "comment")
	t.Setenv("GH_FAKE_OPEN_PR_SEARCH_FILE", searchFile)
	t.Setenv("GH_FAKE_PR_VIEW_FILE", prViewFile)
	t.Setenv("GH_FAKE_WAIVE_TIMELINE_FILE", timelineFile)
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", sentinel)

	statePath := filepath.Join(t.TempDir(), "state.json")
	ledgerPath := filepath.Join(t.TempDir(), "ledger.jsonl")
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
		"dispatch_ledger_file=" + ledgerPath,
		"waive_comment_prefix=/waive",
		"waive_authors=maintainer",
		"waive_checks=ci/flake",
		"waive_repost=true",
	}, "\n")+"\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath, "--json"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("scan exit = %d, stderr=%q", code, stderr.String())
	}
	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("decode scan: %v", err)
	}
	if doc.PRs[0].StaleWaive == nil || doc.PRs[0].StaleWaive.Class != scan.WaiveClassAuthorChanges {
		t.Fatalf("stale_waive = %+v, want author changes", doc.PRs[0].StaleWaive)
	}
	if !strings.Contains(stderr.String(), "class=author changes") {
		t.Fatalf("scan stderr = %q, want class=author changes", stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"comment", "--repost-waives", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("comment exit = %d, stderr=%q", code, stderr.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedAuthorChanges {
		t.Fatalf("results = %+v, want skipped_author_changes", got.Results)
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("gh pr comment was invoked for author changes")
	}
}
