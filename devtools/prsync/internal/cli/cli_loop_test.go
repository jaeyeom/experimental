package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
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

func runSingleLoopRound(t *testing.T, cfgPath string, extraArgs ...string) loop.RoundResult {
	t.Helper()
	args := append([]string{"loop", "--config", cfgPath, "--max-rounds", "1"}, extraArgs...)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), args, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("loop %v exit = %d, want 0, stderr=%q stdout=%q", args, code, stderr.String(), stdout.String())
	}
	var res loop.RoundResult
	if err := json.Unmarshal([]byte(strings.TrimSpace(stdout.String())), &res); err != nil {
		t.Fatalf("decode round result: %v\nstdout=%q", err, stdout.String())
	}
	return res
}

func findJudgmentReason(res loop.RoundResult, repo string, pr int, reason string) (loop.JudgmentItem, bool) {
	for _, it := range res.NeedsJudgment {
		if it.Repo == repo && it.PR == pr && it.Reason == reason {
			return it, true
		}
	}
	return loop.JudgmentItem{}, false
}

func setLoopTestPRFixtures(t *testing.T, mergeable, reviewDecision, ciConclusion string, hasRequests bool) {
	t.Helper()
	reqNodes := `[]`
	if hasRequests {
		reqNodes = `[{"requestedReviewer":{"__typename":"User","login":"reviewer-login"}}]`
	}
	searchBody := fmt.Sprintf(`{
  "data": {
    "search": {
      "nodes": [
        {
          "number": 123,
          "title": "[PROJ-123] Fix the widget",
          "url": "https://github.com/acme/widgets/pull/123",
          "state": "OPEN",
          "baseRefName": "main",
          "headRefName": "fix-widget",
          "headRefOid": "abc123def456",
          "updatedAt": "2026-01-01T10:00:00Z",
          "mergeable": %q,
          "mergeStateStatus": "CLEAN",
          "isDraft": false,
          "reviewDecision": %q,
          "reviewRequests": {"nodes": %s}
        }
      ]
    }
  }
}`, mergeable, reviewDecision, reqNodes)
	prViewBody := fmt.Sprintf(`{
  "number": 123,
  "title": "[PROJ-123] Fix the widget",
  "url": "https://github.com/acme/widgets/pull/123",
  "state": "OPEN",
  "baseRefName": "main",
  "headRefName": "fix-widget",
  "headRefOid": "abc123def456",
  "mergeable": %q,
  "mergeStateStatus": "CLEAN",
  "isDraft": false,
  "reviewDecision": %q,
  "reviewRequests": %s,
  "latestReviews": [],
  "statusCheckRollup": [{"name": "ci", "status": "COMPLETED", "conclusion": %q}]
}`, mergeable, reviewDecision, reqNodes, ciConclusion)
	dir := t.TempDir()
	searchPath := filepath.Join(dir, "search.json")
	prViewPath := filepath.Join(dir, "pr-view.json")
	writeFile(t, searchPath, searchBody)
	writeFile(t, prViewPath, prViewBody)
	t.Setenv("GH_FAKE_OPEN_PR_SEARCH_FILE", searchPath)
	t.Setenv("GH_FAKE_PR_VIEW_FILE", prViewPath)
}

func TestCLILoopNeedsJudgmentAllReasons(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	ghBin, herdrBin := fixtureBins(t)

	// 1. ci_failing_unclassified (present when failing with no inbox/ledger, absent with inbox or ledger).
	t.Run("ci_failing_unclassified", func(t *testing.T) {
		t.Setenv("GH_FAKE_REVIEW_THREADS_FILE", emptyReviewThreadsFile(t))
		setLoopTestPRFixtures(t, "MERGEABLE", "APPROVED", "FAILURE", false)
		stateDir := t.TempDir()
		statePath := filepath.Join(stateDir, "state.json")
		cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)

		res := runSingleLoopRound(t, cfgPath, "--dry-run")
		if _, ok := findJudgmentReason(res, "acme/widgets", 123, loop.ReasonCIFailingUnclassified); !ok {
			t.Fatalf("missing ci_failing_unclassified: %+v", res.NeedsJudgment)
		}
		if res.NeedsJudgmentCount != len(res.NeedsJudgment) {
			t.Fatalf("needs_judgment_count = %d, len = %d", res.NeedsJudgmentCount, len(res.NeedsJudgment))
		}

		// Adding a matching ci_fix_inbox entry suppresses ci_failing_unclassified.
		inboxDir := filepath.Join(stateDir, "ci_fix_inbox")
		writeInboxFile(t, inboxDir, "123.json", `{"repo":"acme/widgets","pr":123,"head_sha":"abc123def456","hint":"test flake"}`+"\n")
		resWithInbox := runSingleLoopRound(t, cfgPath, "--dry-run")
		if _, ok := findJudgmentReason(resWithInbox, "acme/widgets", 123, loop.ReasonCIFailingUnclassified); ok {
			t.Fatalf("ci_failing_unclassified should be suppressed by inbox: %+v", resWithInbox.NeedsJudgment)
		}

		// Removing inbox and recording a ci-fix ledger entry also suppresses it.
		if err := os.Remove(filepath.Join(inboxDir, "123.json")); err != nil {
			t.Fatal(err)
		}
		lgr := &ledger.Store{Path: filepath.Join(stateDir, "ledger.jsonl")}
		if err := lgr.Append(ledger.Entry{
			PR: "acme/widgets#123", HeadSHA: "abc123def456", Kind: dispatch.KindCIFix, Outcome: ledger.OutcomeDispatched, Time: time.Now().UTC(),
		}); err != nil {
			t.Fatal(err)
		}
		resWithLedger := runSingleLoopRound(t, cfgPath, "--dry-run")
		if _, ok := findJudgmentReason(resWithLedger, "acme/widgets", 123, loop.ReasonCIFailingUnclassified); ok {
			t.Fatalf("ci_failing_unclassified should be suppressed by ledger: %+v", resWithLedger.NeedsJudgment)
		}
	})

	// 2. no_tab and ambiguous_tab.
	t.Run("no_tab_and_ambiguous_tab", func(t *testing.T) {
		t.Setenv("GH_FAKE_REVIEW_THREADS_FILE", emptyReviewThreadsFile(t))
		setLoopTestPRFixtures(t, "MERGEABLE", "REVIEW_REQUIRED", "SUCCESS", false)

		noTabFile := filepath.Join(t.TempDir(), "tabs-empty.json")
		writeFile(t, noTabFile, `{"result":{"tabs":[]}}`)
		t.Setenv("HERDR_FAKE_TAB_LIST", noTabFile)

		cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state.json"))
		resNoTab := runSingleLoopRound(t, cfgPath, "--dry-run")
		if _, ok := findJudgmentReason(resNoTab, "acme/widgets", 123, loop.ReasonNoTab); !ok {
			t.Fatalf("missing no_tab: %+v", resNoTab.NeedsJudgment)
		}
		if _, ok := findJudgmentReason(resNoTab, "acme/widgets", 123, loop.ReasonAmbiguousTab); ok {
			t.Fatalf("unexpected ambiguous_tab: %+v", resNoTab.NeedsJudgment)
		}

		ambTabFile := filepath.Join(t.TempDir(), "tabs-ambiguous.json")
		writeFile(t, ambTabFile, `{"result":{"tabs":[{"tab_id":"w2:t1","workspace_id":"w2","label":"PROJ-123","agent_status":"idle","pane_count":1},{"tab_id":"w3:t2","workspace_id":"w3","label":"PROJ-123","agent_status":"idle","pane_count":1}]}}`)
		t.Setenv("HERDR_FAKE_TAB_LIST", ambTabFile)

		resAmb := runSingleLoopRound(t, cfgPath, "--dry-run")
		if _, ok := findJudgmentReason(resAmb, "acme/widgets", 123, loop.ReasonAmbiguousTab); !ok {
			t.Fatalf("missing ambiguous_tab: %+v", resAmb.NeedsJudgment)
		}
		if _, ok := findJudgmentReason(resAmb, "acme/widgets", 123, loop.ReasonNoTab); ok {
			t.Fatalf("ambiguous_tab must not also emit no_tab: %+v", resAmb.NeedsJudgment)
		}
	})

	// 3. tab_blocked (with minutes).
	t.Run("tab_blocked_with_minutes", func(t *testing.T) {
		t.Setenv("GH_FAKE_REVIEW_THREADS_FILE", emptyReviewThreadsFile(t))
		setLoopTestPRFixtures(t, "MERGEABLE", "REVIEW_REQUIRED", "SUCCESS", false)
		t.Setenv("HERDR_FAKE_AGENT_STATUS", "blocked")

		stateDir := t.TempDir()
		cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(stateDir, "state.json"))
		// Seed blocked state 14 minutes ago to verify persisted minutes reporting.
		blockedPath := loop.BlockedStatePath(stateDir, cfgPath)
		writeFile(t, blockedPath, fmt.Sprintf(`{"w2:tC":%q}`+"\n", time.Now().UTC().Add(-14*time.Minute).Format(time.RFC3339)))

		res := runSingleLoopRound(t, cfgPath, "--dry-run")
		it, ok := findJudgmentReason(res, "acme/widgets", 123, loop.ReasonTabBlocked)
		if !ok || it.Minutes == nil || *it.Minutes < 14 {
			t.Fatalf("tab_blocked = %+v (ok=%v), want minutes >= 14", it, ok)
		}
	})

	// 4. lease_holder_blocked.
	t.Run("lease_holder_blocked", func(t *testing.T) {
		checkout := setupGitRepo(t, false)
		leaseStatus := filepath.Join(t.TempDir(), "lease.json")
		writeFile(t, leaseStatus, fmt.Sprintf(`{"checkout":"local:%s","state":"held","holder":{"handle":"herdr:local/s/w9:p9","name":"pm/other"}}`+"\n", checkout))
		installFakeAgentbus(t, true, leaseStatus)

		setLoopTestPRFixtures(t, "MERGEABLE", "REVIEW_REQUIRED", "SUCCESS", false)
		stateDir := t.TempDir()
		cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(stateDir, "state.json"),
			"gate_lease=required",
			"gate_timeout_ms=5",
			"gate_poll_ms=1",
		)

		res := runSingleLoopRound(t, cfgPath)
		it, ok := findJudgmentReason(res, "acme/widgets", 123, loop.ReasonLeaseHolderBlocked)
		if !ok || !strings.Contains(it.Detail, "pm/other") {
			t.Fatalf("lease_holder_blocked = %+v (ok=%v), want pm/other", it, ok)
		}
	})

	// 5. merge_ready.
	t.Run("merge_ready", func(t *testing.T) {
		t.Setenv("GH_FAKE_REVIEW_THREADS_FILE", emptyReviewThreadsFile(t))
		setLoopTestPRFixtures(t, "MERGEABLE", "APPROVED", "SUCCESS", false)

		cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state.json"))
		res := runSingleLoopRound(t, cfgPath, "--dry-run")
		if _, ok := findJudgmentReason(res, "acme/widgets", 123, loop.ReasonMergeReady); !ok {
			t.Fatalf("missing merge_ready: %+v", res.NeedsJudgment)
		}

		// Changes requested must not trigger merge_ready.
		setLoopTestPRFixtures(t, "MERGEABLE", "CHANGES_REQUESTED", "SUCCESS", false)
		resNotReady := runSingleLoopRound(t, cfgPath, "--dry-run")
		if _, ok := findJudgmentReason(resNotReady, "acme/widgets", 123, loop.ReasonMergeReady); ok {
			t.Fatalf("unexpected merge_ready when changes requested: %+v", resNotReady.NeedsJudgment)
		}
	})

	// 6. rereview_due when R5 (bot_rereview) is off, absent when R5 is auto.
	t.Run("rereview_due", func(t *testing.T) {
		t.Setenv("GH_FAKE_REVIEW_THREADS_FILE", emptyReviewThreadsFile(t))
		setLoopTestPRFixtures(t, "MERGEABLE", "REVIEW_REQUIRED", "SUCCESS", false)
		snapPath := filepath.Join(t.TempDir(), "bot-snap.json")
		writeFile(t, snapPath, `{
			"data":{"repository":{"pullRequest":{
				"commits":{"nodes":[{"commit":{"oid":"abc123def456","committedDate":"2026-06-01T10:00:00Z","pushedDate":"2026-06-01T10:05:00Z"}}]},
				"reviews":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"author":{"login":"coderabbitai"},"state":"COMMENTED","submittedAt":"2026-06-01T09:50:00Z"}]},
				"reviewThreads":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"id":"t1","isResolved":true,"comments":{"nodes":[{"author":{"login":"coderabbitai"}}]}}]},
				"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}
			}}}
		}`)
		t.Setenv("GH_FAKE_BOT_REREVIEW_FILE", snapPath)
		commentSentinel := filepath.Join(t.TempDir(), "comment.txt")
		t.Setenv("GH_FAKE_COMMENT_SENTINEL", commentSentinel)

		cfgOff := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state-off.json"),
			"bot_rereview=off",
			"bot_rereview_login=coderabbitai[bot]",
			"bot_rereview_body=@coderabbitai review",
		)
		resOff := runSingleLoopRound(t, cfgOff)
		if _, ok := findJudgmentReason(resOff, "acme/widgets", 123, loop.ReasonRereviewDue); !ok {
			t.Fatalf("missing rereview_due when R5 is off: %+v", resOff.NeedsJudgment)
		}
		if _, err := os.Stat(commentSentinel); !errors.Is(err, fs.ErrNotExist) {
			t.Fatal("rereview_due check posted a comment while bot_rereview=off")
		}

		cfgAuto := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state-auto.json"),
			"bot_rereview=auto",
			"bot_rereview_login=coderabbitai[bot]",
			"bot_rereview_body=@coderabbitai review",
		)
		resAuto := runSingleLoopRound(t, cfgAuto)
		if _, ok := findJudgmentReason(resAuto, "acme/widgets", 123, loop.ReasonRereviewDue); ok {
			t.Fatalf("rereview_due must not appear when R5 is auto: %+v", resAuto.NeedsJudgment)
		}
	})
}
