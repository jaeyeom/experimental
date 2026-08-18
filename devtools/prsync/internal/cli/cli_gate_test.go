package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	executor "github.com/jaeyeom/go-cmdexec"
)

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

func decodeGate(t *testing.T, raw []byte) dispatch.Result {
	t.Helper()
	var got dispatch.Result
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("json: %v\n%s", err, raw)
	}
	return got
}
