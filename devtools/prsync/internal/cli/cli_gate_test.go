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
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestGateHelpListsDefaultIgnoreProcs(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--help"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q", code, stderr.String())
	}
	help := strings.Join(strings.Fields(stdout.String()), " ")
	for _, want := range []string{
		"replaces the defaults",
		"gate_lease auto (default), off, or required",
		"agentbus lease status --path <checkout> --json",
		"prsync claims nothing; the dispatched agent claims",
		strings.Join(config.Defaults().GateIgnoreProcs, ","),
	} {
		if !strings.Contains(help, want) {
			t.Fatalf("gate --help missing %q:\n%s", want, stdout.String())
		}
	}
}

func TestGateSafeWhenIdle(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if !got.Safe {
		t.Fatalf("safe = false, want true; busy=%v", got.Busy)
	}
	if got.Busy == nil {
		t.Fatal("busy = null, want []")
	}
}

func TestGateSafeWhenBlockedIsIgnored(t *testing.T) {
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "blocked")
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\nconcurrency_ignore_blocked=true\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if !got.Safe {
		t.Fatalf("safe = false, want true; busy=%v", got.Busy)
	}
	if len(got.Busy) != 0 {
		t.Fatalf("busy = %+v, want none", got.Busy)
	}
}

func TestGateBusyOnPendingDecideItem(t *testing.T) {
	t.Setenv("PRSYNC_FAKE_DECIDE_OUT", `[{"id":"D-17","state":"answered","notifyPane":"w2:pC"}]`)
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUnsafe {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if len(got.Busy) != 1 {
		t.Fatalf("busy = %+v, want one tab", got.Busy)
	}
	b := got.Busy[0]
	if b.PaneID != "w2:pC" || b.Status != "idle" || b.Signal != dispatch.SignalPendingQuestion || b.Detail != "answered item D-17" {
		t.Fatalf("busy = %+v, want idle w2:pC pending-question answered item D-17", b)
	}
	if !strings.Contains(stdout.String(), `"signal": "pending-question"`) {
		t.Fatalf("stdout = %s, want the signal field", stdout.String())
	}

	cfgPath = writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\ngate_pending_question=off\n")
	stdout.Reset()
	if code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor()); code != ExitOK {
		t.Fatalf("gate_pending_question=off exit = %d, want 0; stdout=%q", code, stdout.String())
	}
}

func TestGateBusyWhenBlocked(t *testing.T) {
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "blocked")
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUnsafe {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if got.Safe {
		t.Fatal("safe = true, want false while an agent is blocked")
	}
	if len(got.Busy) != 1 || got.Busy[0].PaneID != "w2:pC" || got.Busy[0].TabID != "w2:tC" {
		t.Fatalf("busy = %+v, want [{w2:pC w2:tC}]", got.Busy)
	}
	if b := got.Busy[0]; b.Status != "blocked" || b.Signal != dispatch.SignalBlocked || b.Detail != "blocked awaiting input" {
		t.Fatalf("busy = %+v, want blocked with signal blocked and detail blocked awaiting input", b)
	}
}

func TestGateBusyWhenWorking(t *testing.T) {
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "working")
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUnsafe {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if got.Safe {
		t.Fatal("safe = true, want false")
	}
	if len(got.Busy) != 1 || got.Busy[0].PaneID != "w2:pC" || got.Busy[0].TabID != "w2:tC" {
		t.Fatalf("busy = %+v", got.Busy)
	}
	if got.Busy[0].Status != "working" {
		t.Fatalf("busy status = %q, want working", got.Busy[0].Status)
	}
}

func TestGateUnsetPaneIDDoesNotCallPaneCurrent(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "")
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "working")
	t.Setenv("HERDR_FAKE_PANE_ID", "w2:pC")
	sentinel := filepath.Join(t.TempDir(), "pane-current")
	t.Setenv("HERDR_FAKE_PANE_CURRENT_SENTINEL", sentinel)

	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUnsafe {
		t.Fatalf("exit = %d, want %d (must not exclude focused pane), stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("pane current was called (sentinel %s exists), err=%v", sentinel, err)
	}
	got := decodeGate(t, stdout.Bytes())
	if got.Safe || len(got.Busy) != 1 || got.Busy[0].PaneID != "w2:pC" {
		t.Fatalf("result = %+v, want busy w2:pC", got)
	}
}

func TestGateSelfExcludesRunnerPane(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "w2:pC")
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "working")
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if !got.Safe {
		t.Fatalf("safe = false, want true after self-exclusion; busy=%v", got.Busy)
	}
}

func TestGateManagedIgnoresUnmatchedWorking(t *testing.T) {
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "working")
	t.Setenv("HERDR_FAKE_TAB_ID", "w2:tOther")
	t.Setenv("HERDR_FAKE_PANE_ID", "w2:pX")
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\nconcurrency_wait_on=managed\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if !got.Safe {
		t.Fatalf("safe = false, want true; managed must ignore unmatched; busy=%v", got.Busy)
	}
}

func TestGateManagedBusyWhenMatchedWorking(t *testing.T) {
	t.Setenv("HERDR_FAKE_AGENT_STATUS", "working")
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\nconcurrency_wait_on=managed\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUnsafe {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if got.Safe {
		t.Fatal("safe = true, want false when matched tab is working")
	}
}

func TestGateHerdrMissing(t *testing.T) {
	t.Parallel()

	cfgPath := writeScanConfig(t, "gh_bin=prsync-test-missing-gh\nherdr_bin=prsync-test-missing-herdr\nauthor=alice\nrepos=acme/widgets\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	if !strings.Contains(stderr.String(), "herdr") {
		t.Fatalf("stderr = %q, want herdr missing", stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout = %q, want empty on precondition", stdout.String())
	}
}

func TestGateHerdrUnsupported(t *testing.T) {
	tests := []struct {
		name    string
		version string
		want    string
	}{
		{name: "old", version: "herdr 0.7.9", want: "older than 0.8"},
		{name: "unparseable", version: "herdr bananas", want: "cannot parse herdr version"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("HERDR_FAKE_VERSION", tc.version)
			ghBin, herdrBin := fixtureBins(t)
			cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")
			var stdout, stderr bytes.Buffer
			code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
			if code != ExitPrecondition {
				t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
			}
			if !strings.Contains(stderr.String(), tc.want) {
				t.Fatalf("stderr = %q, want %q", stderr.String(), tc.want)
			}
		})
	}
}

func TestGateAnyDoesNotNeedGH(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin=prsync-test-missing-gh\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0 (any does not call gh), stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
}

func TestGateManagedNeedsGH(t *testing.T) {
	_, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin=prsync-test-missing-gh\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\nconcurrency_wait_on=managed\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	if !strings.Contains(stderr.String(), "gh binary not found") {
		t.Fatalf("stderr = %q, want gh binary not found", stderr.String())
	}
}

func TestGateMissingConfig(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", "/no/such/prsync.config"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
}

func TestGateDirtyCheckoutIsUnsafe(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")
	chdirDirtyCheckout(t)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUnsafe {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
	}
	got := decodeGate(t, stdout.Bytes())
	if got.Safe || got.Reason != "dirty tree: 1 files" || len(got.Busy) != 0 {
		t.Fatalf("result = %+v, want unsafe dirty tree with no busy agent", got)
	}
}

func TestDispatchGoDirtyCheckoutExitsUnsafe(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinEligibleDoc())
	chdirDirtyCheckout(t)

	restore := swapStdin(t, string(raw))
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitUnsafe {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionGateUnsafe || got.Results[0].Detail != "dirty tree: 1 files" {
		t.Fatalf("results = %+v, want gate_unsafe with dirty tree detail", got.Results)
	}
	if _, err := os.Stat(statePath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("state_file written on a dirty checkout")
	}
}

// chdirDirtyCheckout makes the working directory an acme/widgets checkout
// with one staged file.
func chdirDirtyCheckout(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not installed")
	}
	dir := t.TempDir()
	git := func(args ...string) {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", dir}, args...)...) //nolint:gosec // G204: test git helper
		cmd.Env = append(os.Environ(), "GIT_CONFIG_GLOBAL="+os.DevNull, "GIT_CONFIG_NOSYSTEM=1")
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
	}
	git("init", "-q")
	git("remote", "add", "origin", "git@github.com:acme/widgets.git")
	writeFile(t, filepath.Join(dir, "staged.txt"), "x\n")
	git("add", "staged.txt")
	t.Chdir(dir)
}

func decodeGate(t *testing.T, raw []byte) dispatch.Result {
	t.Helper()
	var got dispatch.Result
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("json: %v\n%s", err, raw)
	}
	return got
}
