package cli

import (
	"bytes"
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
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

func TestCommentRejectsCIFlag(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"comment", "--ci"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "unknown flag") || !strings.Contains(stderr.String(), "--ci") {
		t.Fatalf("stderr = %q, want unknown flag --ci", stderr.String())
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
