package dispatch

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func writeInboxJSON(t *testing.T, inboxDir, name string, payload any) string {
	t.Helper()
	if err := os.MkdirAll(inboxDir, 0o700); err != nil {
		t.Fatal(err)
	}
	data, err := json.MarshalIndent(payload, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(inboxDir, name)
	if err := os.WriteFile(path, append(data, '\n'), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func writeInboxRaw(t *testing.T, inboxDir, name, raw string) string {
	t.Helper()
	if err := os.MkdirAll(inboxDir, 0o700); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(inboxDir, name)
	if err := os.WriteFile(path, []byte(raw), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func assertFileExists(t *testing.T, path string, want bool) {
	t.Helper()
	_, err := os.Stat(path)
	got := err == nil
	if got != want {
		t.Fatalf("exists(%s) = %v (err=%v), want %v", path, got, err, want)
	}
}

func inboxTestPR(headSHA, agentStatus, ciState string) scan.PR {
	pr := fixtureEligiblePR()
	pr.Unaddressed = false
	pr.BlockingComments = nil
	pr.State = "open"
	pr.HeadSHA = headSHA
	pr.CIState = ciState
	pr.Tab.AgentStatus = agentStatus
	return pr
}

func TestCIFixInboxMatchingHeadSendsOnceAndDedupes(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.Policy.CIFix = config.PolicyAuto
	inboxDir := cfg.CIFixInboxDir()
	lgr := &ledger.Store{Path: cfg.ResolvedLedgerFile()}

	writeInboxJSON(t, inboxDir, "01-req.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      123,
		HeadSHA: "sha111",
		Hint:    "TestWidget failed with nil pointer",
		Writer:  "ci-judge",
		Time:    fixtureNow,
	})
	// Second request for the same head in the same batch must dedupe via ledger.
	writeInboxJSON(t, inboxDir, "02-dup.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      123,
		HeadSHA: "sha111",
		Hint:    "duplicate hint",
		Writer:  "ci-judge",
		Time:    fixtureNow.Add(time.Second),
	})

	h := idleScript()
	pr := inboxTestPR("sha111", "idle", "failing")
	got, err := ProcessCIFixInbox(context.Background(), h, store, cfg, InboxOptions{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Ledger: lgr,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("ProcessCIFixInbox() error = %v", err)
	}
	if h.promptN != 1 {
		t.Fatalf("prompts sent = %d, want 1", h.promptN)
	}
	if len(got.Results) != 2 {
		t.Fatalf("results = %+v, want 2", got.Results)
	}
	if !strings.Contains(got.Results[0].RenderedPrompt, "TestWidget failed with nil pointer") {
		t.Fatalf("prompt missing hint: %q", got.Results[0].RenderedPrompt)
	}
	if got.Results[0].Action != ActionDispatched || got.Results[0].InboxFile != "01-req.json" {
		t.Fatalf("result[0] = %+v, want dispatched", got.Results[0])
	}
	if got.Results[1].Action != ActionSkippedDeduped || got.Results[1].InboxFile != "02-dup.json" {
		t.Fatalf("result[1] = %+v, want skipped_deduped", got.Results[1])
	}

	// Both files moved out of inbox into done/ with recorded outcomes.
	assertFileExists(t, filepath.Join(inboxDir, "01-req.json"), false)
	assertFileExists(t, filepath.Join(inboxDir, "02-dup.json"), false)

	doneData, err := os.ReadFile(filepath.Join(inboxDir, "done", "01-req.json"))
	if err != nil {
		t.Fatalf("read done/01-req.json: %v", err)
	}
	var rec InboxRecord
	if err := json.Unmarshal(doneData, &rec); err != nil {
		t.Fatalf("decode done record: %v", err)
	}
	if rec.Outcome != ActionDispatched || rec.ObservedHeadSHA != "sha111" || rec.Hint != "TestWidget failed with nil pointer" {
		t.Fatalf("done record = %+v", rec)
	}

	dupData, err := os.ReadFile(filepath.Join(inboxDir, "done", "02-dup.json"))
	if err != nil {
		t.Fatalf("read done/02-dup.json: %v", err)
	}
	var dupRec InboxRecord
	if err := json.Unmarshal(dupData, &dupRec); err != nil {
		t.Fatalf("decode dup done record: %v", err)
	}
	if dupRec.Outcome != ActionSkippedDeduped {
		t.Fatalf("dup done record = %+v, want skipped_deduped", dupRec)
	}

	// Even a later request in a subsequent round with failing CI dedupes via ledger.
	writeInboxJSON(t, inboxDir, "03-later.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      123,
		HeadSHA: "sha111",
		Hint:    "still failing on same head",
	})
	got2, err := ProcessCIFixInbox(context.Background(), h, store, cfg, InboxOptions{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Ledger: lgr,
	}, fixtureNow.Add(time.Minute))
	if err != nil {
		t.Fatalf("second round error = %v", err)
	}
	if h.promptN != 1 {
		t.Fatalf("prompts after ledger dedupe = %d, want 1", h.promptN)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionSkippedDeduped {
		t.Fatalf("second round results = %+v, want skipped_deduped", got2.Results)
	}
	assertFileExists(t, filepath.Join(inboxDir, "done", "03-later.json"), true)
}

func TestCIFixInboxChangedHeadOrClosedMovesToStaleUnsent(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.Policy.CIFix = config.PolicyAuto
	inboxDir := cfg.CIFixInboxDir()
	lgr := &ledger.Store{Path: cfg.ResolvedLedgerFile()}

	writeInboxJSON(t, inboxDir, "changed-head.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      123,
		HeadSHA: "oldsha",
		Hint:    "old failure",
	})
	writeInboxJSON(t, inboxDir, "missing-pr.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      999,
		HeadSHA: "somesha",
		Hint:    "closed PR",
	})

	h := idleScript()
	pr := inboxTestPR("newsha", "idle", "failing")
	got, err := ProcessCIFixInbox(context.Background(), h, store, cfg, InboxOptions{
		Doc:    scan.Document{PRs: []scan.PR{pr}},
		Ledger: lgr,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("ProcessCIFixInbox() error = %v", err)
	}
	if h.promptN != 0 {
		t.Fatalf("prompts sent = %d, want 0", h.promptN)
	}
	if len(got.Results) != 2 || got.Results[0].Action != ActionSkippedStale || got.Results[1].Action != ActionSkippedStale {
		t.Fatalf("results = %+v, want both skipped_stale", got.Results)
	}
	assertFileExists(t, filepath.Join(inboxDir, "changed-head.json"), false)
	assertFileExists(t, filepath.Join(inboxDir, "missing-pr.json"), false)
	assertFileExists(t, filepath.Join(inboxDir, "stale", "changed-head.json"), true)
	assertFileExists(t, filepath.Join(inboxDir, "stale", "missing-pr.json"), true)

	entries, err := lgr.Load()
	if err != nil || len(entries) != 0 {
		t.Fatalf("ledger entries = %+v, err=%v, want empty", entries, err)
	}
}

func TestCIFixInboxBusyTabWaitsForNextRound(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.Policy.CIFix = config.PolicyAuto
	inboxDir := cfg.CIFixInboxDir()
	lgr := &ledger.Store{Path: cfg.ResolvedLedgerFile()}

	reqPath := writeInboxJSON(t, inboxDir, "busy.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      123,
		HeadSHA: "sha111",
		Hint:    "flake or real failure",
	})

	h := idleScript()
	// Round 1: tab is working -> stays in inbox, nothing sent.
	got1, err := ProcessCIFixInbox(context.Background(), h, store, cfg, InboxOptions{
		Doc:    scan.Document{PRs: []scan.PR{inboxTestPR("sha111", "working", "failing")}},
		Ledger: lgr,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("round 1 error = %v", err)
	}
	if h.promptN != 0 {
		t.Fatalf("round 1 prompts = %d, want 0", h.promptN)
	}
	if len(got1.Results) != 1 || got1.Results[0].Action != ActionSkippedBusy {
		t.Fatalf("round 1 results = %+v, want skipped_busy", got1.Results)
	}
	assertFileExists(t, reqPath, true)

	// Round 2: tab is idle -> dispatches and moves to done/.
	got2, err := ProcessCIFixInbox(context.Background(), h, store, cfg, InboxOptions{
		Doc:    scan.Document{PRs: []scan.PR{inboxTestPR("sha111", "idle", "failing")}},
		Ledger: lgr,
	}, fixtureNow.Add(time.Minute))
	if err != nil {
		t.Fatalf("round 2 error = %v", err)
	}
	if h.promptN != 1 {
		t.Fatalf("round 2 prompts = %d, want 1", h.promptN)
	}
	if len(got2.Results) != 1 || got2.Results[0].Action != ActionDispatched {
		t.Fatalf("round 2 results = %+v, want dispatched", got2.Results)
	}
	assertFileExists(t, reqPath, false)
	assertFileExists(t, filepath.Join(inboxDir, "done", "busy.json"), true)
}

func TestCIFixInboxMalformedReportedAndMovedToBadIgnoresTempAndDirs(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.Policy.CIFix = config.PolicyAuto
	inboxDir := cfg.CIFixInboxDir()
	lgr := &ledger.Store{Path: cfg.ResolvedLedgerFile()}

	// Dotfiles, non-json files, and subdirectories must be ignored.
	tmpFile := writeInboxRaw(t, inboxDir, ".tmp-12345.json", "{incomplete")
	otherFile := writeInboxRaw(t, inboxDir, "notes.txt", "ignore me")
	if err := os.MkdirAll(filepath.Join(inboxDir, "done"), 0o700); err != nil {
		t.Fatal(err)
	}
	// Malformed JSON and missing-required-field requests must move to bad/ and be reported.
	badJSON := writeInboxRaw(t, inboxDir, "01-syntax.json", "{not valid json")
	missingHint := writeInboxJSON(t, inboxDir, "02-no-hint.json", map[string]any{
		"repo":     "acme/widgets",
		"pr":       123,
		"head_sha": "sha111",
	})
	validReq := writeInboxJSON(t, inboxDir, "03-good.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      123,
		HeadSHA: "sha111",
		Hint:    "real failure",
	})

	var statusBuf bytes.Buffer
	ctx := WithStatusWriter(context.Background(), &statusBuf)
	h := idleScript()
	got, err := ProcessCIFixInbox(ctx, h, store, cfg, InboxOptions{
		Doc:    scan.Document{PRs: []scan.PR{inboxTestPR("sha111", "idle", "failing")}},
		Ledger: lgr,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("ProcessCIFixInbox() error = %v", err)
	}
	if len(got.Results) != 3 {
		t.Fatalf("results = %+v, want 3", got.Results)
	}
	if got.Results[0].Action != ActionBadRequest || got.Results[0].InboxFile != "01-syntax.json" {
		t.Fatalf("result[0] = %+v, want bad_request", got.Results[0])
	}
	if got.Results[1].Action != ActionBadRequest || got.Results[1].InboxFile != "02-no-hint.json" {
		t.Fatalf("result[1] = %+v, want bad_request", got.Results[1])
	}
	if got.Results[2].Action != ActionDispatched || got.Results[2].InboxFile != "03-good.json" {
		t.Fatalf("result[2] = %+v, want dispatched", got.Results[2])
	}
	if !strings.Contains(statusBuf.String(), "01-syntax.json") || !strings.Contains(statusBuf.String(), "02-no-hint.json") {
		t.Fatalf("status output missing malformed reports: %q", statusBuf.String())
	}

	assertFileExists(t, badJSON, false)
	assertFileExists(t, missingHint, false)
	assertFileExists(t, validReq, false)
	assertFileExists(t, filepath.Join(inboxDir, "bad", "01-syntax.json"), true)
	assertFileExists(t, filepath.Join(inboxDir, "bad", "02-no-hint.json"), true)
	assertFileExists(t, filepath.Join(inboxDir, "done", "03-good.json"), true)
	// Temp dotfile and non-json remain untouched.
	assertFileExists(t, tmpFile, true)
	assertFileExists(t, otherFile, true)
}

func TestCIFixInboxPolicyOffAndDryRunDoNotSendOrMutate(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name       string
		policyMode string
		cfgDryRun  bool
		wantAction string
	}{
		{name: "policy off", policyMode: config.PolicyOff, cfgDryRun: false, wantAction: ActionSkippedPolicyOff},
		{name: "policy dry-run", policyMode: config.PolicyDryRun, cfgDryRun: false, wantAction: ActionWouldDispatch},
		{name: "cfg dry-run with policy auto", policyMode: config.PolicyAuto, cfgDryRun: true, wantAction: ActionWouldDispatch},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			cfg, store := liveCfg(t)
			cfg.DryRun = tc.cfgDryRun
			cfg.Policy.CIFix = tc.policyMode
			inboxDir := cfg.CIFixInboxDir()
			lgr := &ledger.Store{Path: cfg.ResolvedLedgerFile()}

			reqPath := writeInboxJSON(t, inboxDir, "req.json", InboxRequest{
				Repo:    "acme/widgets",
				PR:      123,
				HeadSHA: "sha111",
				Hint:    "preview hint",
			})

			h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
			got, err := ProcessCIFixInbox(context.Background(), h, store, cfg, InboxOptions{
				Doc:    scan.Document{PRs: []scan.PR{inboxTestPR("sha111", "idle", "failing")}},
				Ledger: lgr,
			}, fixtureNow)
			if err != nil {
				t.Fatalf("ProcessCIFixInbox() error = %v", err)
			}
			if h.promptN != 0 {
				t.Fatalf("prompts = %d, want 0", h.promptN)
			}
			if len(got.Results) != 1 || got.Results[0].Action != tc.wantAction {
				t.Fatalf("results = %+v, want %q", got.Results, tc.wantAction)
			}
			if tc.wantAction == ActionWouldDispatch && !strings.Contains(got.Results[0].RenderedPrompt, "preview hint") {
				t.Fatalf("dry-run rendered_prompt missing hint: %q", got.Results[0].RenderedPrompt)
			}
			// File stays untouched in inbox, and neither state nor ledger is written.
			assertFileExists(t, reqPath, true)
			assertFileExists(t, store.Path, false)
			assertFileExists(t, lgr.Path, false)
		})
	}
}

func TestCIFixInboxGateTimeoutLeavesFileInInbox(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.Policy.CIFix = config.PolicyAuto
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 5 * time.Millisecond
	inboxDir := cfg.CIFixInboxDir()
	lgr := &ledger.Store{Path: cfg.ResolvedLedgerFile()}

	reqPath := writeInboxJSON(t, inboxDir, "gate-busy.json", InboxRequest{
		Repo:    "acme/widgets",
		PR:      123,
		HeadSHA: "sha111",
		Hint:    "wait for gate",
	})

	// PR tab is idle, but another tab (w9:p9) is working so the gate times out.
	h := &scriptHerdr{
		lists: [][]herdr.Agent{
			{idleAgent("w2:pC", "w2:tC"), workingAgent("w9:p9", "w9:t9")},
		},
	}
	clk := &fakeClock{now: fixtureNow}
	slp := &fakeSleeper{clock: clk}
	got, err := ProcessCIFixInbox(context.Background(), h, store, cfg, InboxOptions{
		Doc:     scan.Document{PRs: []scan.PR{inboxTestPR("sha111", "idle", "failing")}},
		Ledger:  lgr,
		Clock:   clk,
		Sleeper: slp,
	}, fixtureNow)
	if err == nil {
		t.Fatal("expected gate timeout error, got nil")
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionGateTimeout {
		t.Fatalf("results = %+v, want gate_timeout", got.Results)
	}
	assertFileExists(t, reqPath, true)
	assertFileExists(t, lgr.Path, false)
}
