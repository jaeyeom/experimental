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
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestDispatchGoThenDeduped(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinEligibleDoc())

	restore := swapStdin(t, string(raw))
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
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

	restore = swapStdin(t, string(raw))
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("second --go exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got = decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("second results = %+v, want skipped_deduped", got.Results)
	}
}

func TestDispatchGoStallDoesNotWriteState(t *testing.T) {
	t.Setenv("HERDR_FAKE_PROMPT", "stall")
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedStalled {
		t.Fatalf("results = %+v, want skipped_stalled", got.Results)
	}
	if _, err := os.Stat(statePath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state_file written on stall")
	}
}

func TestDispatchGoDoneSettlementIsDispatched(t *testing.T) {
	statusFile := filepath.Join(t.TempDir(), "status")
	if err := os.WriteFile(statusFile, []byte("idle\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("HERDR_FAKE_STATUS_FILE", statusFile)
	t.Setenv("HERDR_FAKE_SETTLE", "done")
	t.Setenv("HERDR_FAKE_PROMPT", "done")
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)

	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("results = %+v, want dispatched (done settlement)", got.Results)
	}
	if got.Results[0].Action == dispatch.ActionDispatchedTimeout {
		t.Fatal("done settlement must not be dispatched_timeout")
	}
	settled, err := os.ReadFile(statusFile) //nolint:gosec // test status file
	if err != nil {
		t.Fatal(err)
	}
	if strings.TrimSpace(string(settled)) != "done" {
		t.Fatalf("status file = %q, want done", settled)
	}
}

func TestDispatchGoBlockedStopsBatch(t *testing.T) {
	t.Setenv("HERDR_FAKE_PROMPT", "blocked")
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinTwoEligibleDoc())
	restore := swapStdin(t, string(raw))

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2\n%s", len(got.Results), stdout.String())
	}
	if got.Results[0].Action != dispatch.ActionDispatchedBlocked || got.Results[0].Number != 123 {
		t.Fatalf("first = %+v, want dispatched_blocked #123", got.Results[0])
	}
	if got.Results[1].Action != dispatch.ActionQueued || got.Results[1].Number != 124 {
		t.Fatalf("second = %+v, want queued #124", got.Results[1])
	}
	if _, err := os.Stat(statePath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state_file written on blocked send")
	}

	restore = swapStdin(t, string(raw))
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("second --go exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got = decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 2 {
		t.Fatalf("second len(results) = %d, want 2\n%s", len(got.Results), stdout.String())
	}
	if got.Results[0].Action == dispatch.ActionSkippedDeduped {
		t.Fatal("blocked PR must not be skipped_deduped on re-run")
	}
	if got.Results[0].Action != dispatch.ActionDispatchedBlocked || got.Results[0].Number != 123 {
		t.Fatalf("second first = %+v, want dispatched_blocked #123", got.Results[0])
	}
}

func TestDispatchGoHerdrTimeoutWritesState(t *testing.T) {
	t.Setenv("HERDR_FAKE_PROMPT", "timeout")
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatchedTimeout {
		t.Fatalf("results = %+v, want dispatched_timeout", got.Results)
	}
	if _, err := os.Stat(statePath); err != nil {
		t.Fatalf("state_file missing after herdr timeout: %v", err)
	}
}

func TestDispatchGoMidLoopFatalPartialJSON(t *testing.T) {
	countFile := filepath.Join(t.TempDir(), "prompt-count")
	t.Setenv("HERDR_FAKE_PROMPT_SEQ", "success,unparseable")
	t.Setenv("HERDR_FAKE_PROMPT_COUNT", countFile)
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinTwoEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2\n%s", len(got.Results), stdout.String())
	}
	if got.Results[0].Action != dispatch.ActionDispatched || got.Results[1].Action != dispatch.ActionFailed {
		t.Fatalf("results = %+v, want dispatched + failed", got.Results)
	}
	st, err := dispatch.LoadFile(statePath)
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

func TestDispatchGoGateTimeoutExit4(t *testing.T) {
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "working")
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinTwoEligibleDoc())
	restore := swapStdin(t, string(raw))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitGateTimeout {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitGateTimeout, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	for _, r := range got.Results {
		if r.Action != dispatch.ActionQueued {
			t.Fatalf("result = %+v, want queued", r)
		}
	}
	if _, err := os.Stat(statePath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state_file written on gate timeout")
	}
}

func writeLiveConfig(t *testing.T, ghBin, herdrBin, statePath string) string {
	t.Helper()
	return writeScanConfig(t, strings.Join([]string{
		"gh_bin=" + ghBin,
		"herdr_bin=" + herdrBin,
		"author=alice",
		"repos=acme/widgets",
		"state_file=" + statePath,
		"gate_poll_ms=1",
		"gate_timeout_ms=50",
		"dispatch_timeout_ms=1000",
	}, "\n")+"\n")
}

func stdinTwoEligibleDoc() scan.Document {
	doc := stdinEligibleDoc()
	second := doc.PRs[0]
	second.Number = 124
	second.URL = "https://github.com/acme/widgets/pull/124"
	pane := "w2:pD"
	tab := *second.Tab
	tab.PaneID = &pane
	tab.TabID = "w2:tD"
	second.Tab = &tab
	second.BlockingComments = []scan.Comment{{
		ThreadID:  "PRRT_second",
		CommentID: "PRRC_second",
		Author:    "reviewer-login",
		Path:      "src/other.go",
		URL:       "https://github.com/acme/widgets/pull/124#discussion_r2",
		Body:      "Second comment.",
	}}
	doc.PRs = append(doc.PRs, second)
	return doc
}
