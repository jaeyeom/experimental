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
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/loop"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/policy"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestCLILoopRunsMaxRoundsAndDoctorReportsState(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	ghBin, herdrBin := fixtureBins(t)
	stateDir := t.TempDir()
	statePath := filepath.Join(stateDir, "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{
		"loop", "--config", cfgPath, "--max-rounds", "2", "--dry-run",
	}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("loop exit = %d, want 0, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	if len(lines) != 2 {
		t.Fatalf("round lines = %d, want 2:\n%s", len(lines), stdout.String())
	}
	var r1, r2 loop.RoundResult
	if err := json.Unmarshal([]byte(lines[0]), &r1); err != nil {
		t.Fatalf("decode round 1: %v", err)
	}
	if err := json.Unmarshal([]byte(lines[1]), &r2); err != nil {
		t.Fatalf("decode round 2: %v", err)
	}
	if r1.Round != 1 || r2.Round != 2 || !r2.Dispatch.DryRun {
		t.Fatalf("rounds = %+v / %+v", r1, r2)
	}

	// After clean exit, doctor must report loop not running.
	docOut := runDoctorJSON(t, cfgPath)
	loopChk := findDoctorCheck(t, docOut, "loop")
	if loopChk.Status != toolcli.StatusOK || loopChk.Detail != loop.StateNotRunning {
		t.Fatalf("doctor after stop = %+v, want not running", loopChk)
	}
}

func TestCLIDoctorRunningAndStaleLoopStates(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	_, herdrBin := fixtureBins(t)
	stateDir := t.TempDir()
	cfgPath := writeScanConfig(t, "herdr_bin="+herdrBin+"\nstate_file="+filepath.Join(stateDir, "state.json")+"\n")
	hbPath := loop.HeartbeatPath(stateDir, cfgPath)

	// Write a fresh heartbeat for a live PID.
	if err := loop.WriteHeartbeat(hbPath, loop.Heartbeat{
		PID:        1, // init/launchd PID 1 is alive
		ConfigPath: cfgPath,
		Round:      5,
		State:      "running",
		StartedAt:  time.Now().UTC().Add(-10 * time.Minute),
		UpdatedAt:  time.Now().UTC(),
	}); err != nil {
		t.Fatal(err)
	}
	docOut := runDoctorJSON(t, cfgPath)
	loopChk := findDoctorCheck(t, docOut, "loop")
	// Note: under restricted macOS sandbox Kill(1, 0) returns EPERM -> ProcessAlive returns true!
	if loopChk.Status != toolcli.StatusOK || !strings.HasPrefix(loopChk.Detail, loop.StateRunning) {
		t.Fatalf("doctor running = %+v, want running", loopChk)
	}

	// Now make the heartbeat stale (old timestamp).
	if err := loop.WriteHeartbeat(hbPath, loop.Heartbeat{
		PID:        1,
		ConfigPath: cfgPath,
		Round:      5,
		State:      "running",
		StartedAt:  time.Now().UTC().Add(-2 * time.Hour),
		UpdatedAt:  time.Now().UTC().Add(-time.Hour),
	}); err != nil {
		t.Fatal(err)
	}
	docOut = runDoctorJSON(t, cfgPath)
	loopChk = findDoctorCheck(t, docOut, "loop")
	if loopChk.Status != toolcli.StatusWarn || !strings.HasPrefix(loopChk.Detail, loop.StateStale) {
		t.Fatalf("doctor stale = %+v, want warn/stale heartbeat", loopChk)
	}
}

func TestCLILoopEnsureHealthyExitsFastAndStaleRestarts(t *testing.T) {
	stateDir := t.TempDir()
	cfgPath := writeScanConfig(t, "state_file="+filepath.Join(stateDir, "state.json")+"\n")
	hbPath := loop.HeartbeatPath(stateDir, cfgPath)

	origSpawner := ensureSpawner
	origAlive := ensureIsAlive
	t.Cleanup(func() {
		ensureSpawner = origSpawner
		ensureIsAlive = origAlive
	})

	spawned := 0
	var lastArgs []string
	ensureSpawner = func(args []string) (int, error) {
		spawned++
		lastArgs = append([]string(nil), args...)
		return 9876, nil
	}
	ensureIsAlive = func(pid int) bool { return pid == 4242 }

	// 1. Fresh heartbeat + alive PID -> exit 0, no spawn.
	if err := loop.WriteHeartbeat(hbPath, loop.Heartbeat{
		PID:       4242,
		Round:     2,
		State:     "running",
		StartedAt: time.Now().UTC().Add(-time.Minute),
		UpdatedAt: time.Now().UTC(),
	}); err != nil {
		t.Fatal(err)
	}
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"loop", "--ensure", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK || spawned != 0 {
		t.Fatalf("healthy ensure code=%d spawned=%d stderr=%q", code, spawned, stderr.String())
	}

	// 2. Stale heartbeat -> restarts detached forwarding --config (without --ensure).
	if err := loop.WriteHeartbeat(hbPath, loop.Heartbeat{
		PID:       4242,
		Round:     2,
		State:     "running",
		StartedAt: time.Now().UTC().Add(-2 * time.Hour),
		UpdatedAt: time.Now().UTC().Add(-time.Hour),
	}); err != nil {
		t.Fatal(err)
	}
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"loop", "--ensure", "--config", cfgPath, "--parallel", "3", "--pause", "5s"}, &stdout, &stderr, executor.NewBasicExecutor())
	// Note: cfg missing gate_lease=required with --parallel 3 must refuse!
	if code != ExitUsage {
		t.Fatalf("ensure with --parallel 3 without gate_lease=required code=%d, want %d", code, ExitUsage)
	}

	cfgRequired := writeScanConfig(t, "gate_lease=required\nstate_file="+filepath.Join(stateDir, "state.json")+"\n")
	hbRequired := loop.HeartbeatPath(stateDir, cfgRequired)
	if err := loop.WriteHeartbeat(hbRequired, loop.Heartbeat{
		PID:       4242,
		Round:     2,
		State:     "running",
		StartedAt: time.Now().UTC().Add(-2 * time.Hour),
		UpdatedAt: time.Now().UTC().Add(-time.Hour),
	}); err != nil {
		t.Fatal(err)
	}
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"loop", "--ensure", "--config", cfgRequired, "--parallel", "3", "--pause", "5s"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK || spawned != 1 {
		t.Fatalf("stale ensure code=%d spawned=%d stderr=%q", code, spawned, stderr.String())
	}
	joined := strings.Join(lastArgs, " ")
	if !strings.HasPrefix(joined, "loop ") || strings.Contains(joined, "--ensure") || !strings.Contains(joined, "--parallel 3") || !strings.Contains(joined, "--pause 5s") {
		t.Fatalf("forwarded args = %v", lastArgs)
	}
}

func TestCLILoopRefusesSecondInstance(t *testing.T) {
	stateDir := t.TempDir()
	cfgPath := writeScanConfig(t, "state_file="+filepath.Join(stateDir, "state.json")+"\n")
	lock, err := loop.AcquireLock(loop.LockPath(stateDir, cfgPath))
	if err != nil {
		t.Fatal(err)
	}
	defer func() { _ = lock.Close() }()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"loop", "--config", cfgPath, "--max-rounds", "1"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("second loop code=%d, want %d, stderr=%q", code, ExitPrecondition, stderr.String())
	}
	if !strings.Contains(stderr.String(), "already running") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestCLILoopBotRereviewDryRunAndLiveDedupe(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	ghBin, herdrBin := fixtureBins(t)
	stateDir := t.TempDir()
	statePath := filepath.Join(stateDir, "state.json")
	snapPath := filepath.Join(t.TempDir(), "bot-snap.json")
	commentSentinel := filepath.Join(t.TempDir(), "comment.txt")
	t.Setenv("GH_FAKE_BOT_REREVIEW_FILE", snapPath)
	t.Setenv("GH_FAKE_COMMENT_SENTINEL", commentSentinel)
	t.Setenv("GH_FAKE_COMMENT_URL", "https://github.com/acme/widgets/pull/123#issuecomment-777")

	snapJSON := `{
		"data":{"repository":{"pullRequest":{
			"commits":{"nodes":[{"commit":{"oid":"abc123def456","committedDate":"2026-06-01T10:00:00Z","pushedDate":"2026-06-01T10:05:00Z"}}]},
			"reviews":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"author":{"login":"coderabbitai"},"state":"COMMENTED","submittedAt":"2026-06-01T09:50:00Z"}]},
			"reviewThreads":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"id":"t1","isResolved":true,"comments":{"nodes":[{"author":{"login":"coderabbitai"}}]}}]},
			"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}
		}}}
	}`
	if err := os.WriteFile(snapPath, []byte(snapJSON), 0o600); err != nil {
		t.Fatal(err)
	}

	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath,
		"bot_rereview=auto",
		"bot_rereview_login=coderabbitai[bot]",
		"bot_rereview_body=@coderabbitai review",
	)

	// 1. --dry-run must preview without posting or writing ledger.
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{
		"loop", "--config", cfgPath, "--max-rounds", "1", "--dry-run",
	}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("dry-run loop exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var dryRound loop.RoundResult
	if err := json.Unmarshal([]byte(strings.TrimSpace(stdout.String())), &dryRound); err != nil {
		t.Fatalf("decode dry round: %v\n%s", err, stdout.String())
	}
	if len(dryRound.BotRereview) != 1 || dryRound.BotRereview[0].Decision != policy.DecisionDryRun {
		t.Fatalf("dryRound.BotRereview = %+v", dryRound.BotRereview)
	}
	if _, err := os.Stat(commentSentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("dry-run loop posted comment")
	}

	// 2. Live 2-round loop posts on round 1 and dedupes on round 2.
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{
		"loop", "--config", cfgPath, "--max-rounds", "2",
	}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("live loop exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	if len(lines) != 2 {
		t.Fatalf("round lines = %d, want 2:\n%s", len(lines), stdout.String())
	}
	var r1, r2 loop.RoundResult
	if err := json.Unmarshal([]byte(lines[0]), &r1); err != nil {
		t.Fatalf("decode round 1: %v", err)
	}
	if err := json.Unmarshal([]byte(lines[1]), &r2); err != nil {
		t.Fatalf("decode round 2: %v", err)
	}
	if len(r1.BotRereview) != 1 || r1.BotRereview[0].Decision != policy.DecisionDispatch {
		t.Fatalf("r1.BotRereview = %+v, want dispatch", r1.BotRereview)
	}
	if len(r2.BotRereview) != 1 || r2.BotRereview[0].Decision != policy.DecisionSkip || r2.BotRereview[0].Reason != "already_dispatched" {
		t.Fatalf("r2.BotRereview = %+v, want skip/already_dispatched", r2.BotRereview)
	}
	rawComment, err := os.ReadFile(commentSentinel)
	if err != nil {
		t.Fatalf("read comment sentinel: %v", err)
	}
	if !strings.Contains(string(rawComment), "@coderabbitai review") {
		t.Fatalf("comment args = %q", string(rawComment))
	}
}

func runDoctorJSON(t *testing.T, cfgPath string) toolcli.DoctorOutput {
	t.Helper()
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"doctor", "--offline", "--json", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	var out toolcli.DoctorOutput
	if err := json.Unmarshal(stdout.Bytes(), &out); err != nil {
		t.Fatalf("decode doctor json (exit %d): %v\nstderr=%q\nstdout=%q", code, err, stderr.String(), stdout.String())
	}
	return out
}

func findDoctorCheck(t *testing.T, out toolcli.DoctorOutput, name string) toolcli.CheckResult {
	t.Helper()
	for _, c := range out.Checks {
		if c.Name == name {
			return c
		}
	}
	t.Fatalf("doctor output missing %q check: %+v", name, out.Checks)
	return toolcli.CheckResult{}
}
