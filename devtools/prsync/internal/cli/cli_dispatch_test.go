package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestPeekScanJSONLeadingWhitespace(t *testing.T) {
	t.Parallel()

	raw, err := json.Marshal(scan.Document{
		GeneratedAt:       "2026-01-01T09:00:00Z",
		Author:            "alice",
		Repos:             []string{"acme/widgets"},
		PRs:               []scan.PR{{Repo: "acme/widgets", Number: 123, Unaddressed: true}},
		InaccessibleRepos: []string{},
		Warnings:          []string{},
	})
	if err != nil {
		t.Fatal(err)
	}
	doc, isJSON, err := peekScanJSON(strings.NewReader("  \n" + string(raw)))
	if err != nil {
		t.Fatalf("peekScanJSON() error = %v", err)
	}
	if !isJSON {
		t.Fatal("isJSON = false, want true")
	}
	if doc.Author != "alice" || len(doc.PRs) != 1 || doc.PRs[0].Number != 123 {
		t.Fatalf("doc = %+v", doc)
	}
}

func TestPeekScanJSONDecodeError(t *testing.T) {
	t.Parallel()

	_, isJSON, err := peekScanJSON(strings.NewReader("  \n{not-json"))
	if err == nil {
		t.Fatal("error = nil, want decode error")
	}
	if !isJSON {
		t.Fatal("isJSON = false, want true so CLI fails loud")
	}
}

func TestPeekScanJSONNonJSONFallsBack(t *testing.T) {
	t.Parallel()

	_, isJSON, err := peekScanJSON(strings.NewReader("not json"))
	if err != nil {
		t.Fatalf("error = %v", err)
	}
	if isJSON {
		t.Fatal("isJSON = true, want false (internal scan)")
	}
}

func TestPeekScanJSONEmpty(t *testing.T) {
	t.Parallel()

	_, isJSON, err := peekScanJSON(strings.NewReader(""))
	if err != nil {
		t.Fatalf("error = %v", err)
	}
	if isJSON {
		t.Fatal("empty stdin should fall back to internal scan")
	}
}

func TestStdinIsTTY(t *testing.T) {
	t.Parallel()

	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		r.Close()
		w.Close()
	})
	if stdinIsTTY(r) {
		t.Fatal("pipe must not be a TTY")
	}
	if !stdinIsTTY(nil) {
		t.Fatal("nil stdin treated as TTY (internal scan)")
	}
}

func TestDispatchBadPR(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--pr", "not-a-pr"}, &stdout, &stderr, executor.NewBasicExecutor())
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

func TestDispatchPRAndAll(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--pr", "acme/widgets#1", "--all"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "cannot combine --pr and --all") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestDispatchStdinJSONDryRun(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	sentinel := filepath.Join(t.TempDir(), "prompt")
	t.Setenv("HERDR_FAKE_PROMPT_SENTINEL", sentinel)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
	}, "\n")+"\n")

	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, "  \n"+string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
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
	if got.Results[0].RenderedPrompt == "" {
		t.Fatal("rendered_prompt empty")
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("agent prompt was invoked")
	}
	if _, err := os.Stat(statePath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state_file was written on dry-run")
	}
}

func TestDispatchStdinDecodeError(t *testing.T) {
	restore := swapStdin(t, "  \n{not json")
	defer restore()
	cfgPath := writeScanConfig(t, "author=alice\nrepos=acme/widgets\nstate_file="+filepath.Join(t.TempDir(), "s.json")+"\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "stdin scan JSON") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestDispatchRebaseAddressedWouldDispatch(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"herdr_bin=" + herdrBin,
		"author=alice",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")
	doc := stdinEligibleDoc()
	doc.PRs[0].Unaddressed = false
	doc.PRs[0].BlockingComments = nil
	doc.PRs[0].Head = "fix-widget"
	doc.PRs[0].Base = "main"
	raw := mustScanJSON(t, doc)
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--rebase", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch", got.Results)
	}
	if !strings.Contains(got.Results[0].RenderedPrompt, "Check out fix-widget") {
		t.Fatalf("rendered_prompt = %q", got.Results[0].RenderedPrompt)
	}
	if strings.Contains(got.Results[0].RenderedPrompt, "unresolved review comments") {
		t.Fatalf("used comment template: %q", got.Results[0].RenderedPrompt)
	}
}

func TestDispatchAddressedWithoutRebaseIsSkipped(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"herdr_bin=" + herdrBin,
		"author=alice",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")
	doc := stdinEligibleDoc()
	doc.PRs[0].Unaddressed = false
	doc.PRs[0].BlockingComments = nil
	raw := mustScanJSON(t, doc)
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedAddressed {
		t.Fatalf("results = %+v, want skipped_addressed", got.Results)
	}
}

func TestDispatchNotFoundPR(t *testing.T) {
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
		"dispatch", "--config", cfgPath,
		"--pr", "acme/widgets#123",
		"--pr", "acme/missing#9",
	}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2 (never drop)", len(got.Results))
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

func TestDispatchDoesNotInvokePromptOnInternalScan(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "prompt")
	t.Setenv("HERDR_FAKE_PROMPT_SENTINEL", sentinel)
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + filepath.Join(t.TempDir(), "state.json"),
	}, "\n")+"\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("results = %+v", got.Results)
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("agent prompt was invoked")
	}
}

func TestDispatchHerdrMissing(t *testing.T) {
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
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	if !strings.Contains(stderr.String(), "herdr") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestDispatchCorruptState(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	if err := os.WriteFile(statePath, []byte("{"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfgPath := writeScanConfig(t, strings.Join([]string{
		"herdr_bin=" + herdrBin,
		"author=alice",
		"state_file=" + statePath,
	}, "\n")+"\n")
	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitUsage, stderr.String(), stdout.String())
	}
}

func stdinEligibleDoc() scan.Document {
	line := 42
	id := "PROJ-123"
	pane := "w2:pC"
	return scan.Document{
		GeneratedAt: "2026-01-01T09:00:00Z",
		Author:      "alice",
		Repos:       []string{"acme/widgets"},
		PRs: []scan.PR{{
			Repo:        "acme/widgets",
			Number:      123,
			Title:       "[PROJ-123] Fix the widget",
			URL:         "https://github.com/acme/widgets/pull/123",
			Identifier:  &id,
			Unaddressed: true,
			BlockingComments: []scan.Comment{{
				ThreadID:  "PRRT_widget",
				CommentID: "PRRC_widget",
				Author:    "reviewer-login",
				Path:      "src/widget.go",
				Line:      &line,
				URL:       "https://github.com/acme/widgets/pull/123#discussion_r1",
				Body:      "This should handle the nil case.",
			}},
			Tab: &scan.Tab{
				TabID: "w2:tC", PaneID: &pane, WorkspaceID: "w2",
				Label: "PROJ-123", AgentStatus: "idle",
			},
		}},
		InaccessibleRepos: []string{},
		Warnings:          []string{},
	}
}

func mustScanJSON(t *testing.T, doc scan.Document) []byte {
	t.Helper()
	raw, err := json.Marshal(doc)
	if err != nil {
		t.Fatal(err)
	}
	return raw
}

func decodeDispatch(t *testing.T, raw []byte) dispatch.Document {
	t.Helper()
	var got dispatch.Document
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("json: %v\n%s", err, raw)
	}
	return got
}

func swapStdin(t *testing.T, body string) (restore func()) {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	old := os.Stdin
	os.Stdin = r
	done := make(chan struct{})
	go func() {
		_, _ = io.WriteString(w, body)
		_ = w.Close()
		close(done)
	}()
	return func() {
		<-done
		os.Stdin = old
		_ = r.Close()
	}
}
