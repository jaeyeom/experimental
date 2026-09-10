package dispatch

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"log/slog"
	"path/filepath"
	"slices"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func TestRunLiveLogsDispatchLifecycle(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	buf, ctx := withLog(t)
	got, err := Run(ctx, h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}

	recs := logRecords(t, buf)
	if !hasLog(recs, "gate_wait_start") {
		t.Fatalf("missing gate_wait_start in %v", logMsgs(recs))
	}
	end, ok := logByMsg(recs, "gate_wait_end")
	if !ok {
		t.Fatalf("missing gate_wait_end in %v", logMsgs(recs))
	}
	if end["safe"] != true {
		t.Fatalf("gate_wait_end.safe = %v, want true", end["safe"])
	}
	send, ok := logByMsg(recs, "dispatch_send")
	if !ok {
		t.Fatalf("missing dispatch_send in %v", logMsgs(recs))
	}
	if send["pane_id"] != "w2:pC" {
		t.Fatalf("dispatch_send.pane_id = %v, want w2:pC", send["pane_id"])
	}
	if send["repo"] != "acme/widgets" || send["number"] != float64(123) {
		t.Fatalf("dispatch_send repo/number = %v %v", send["repo"], send["number"])
	}
	status, ok := logByMsg(recs, "agent_status")
	if !ok {
		t.Fatalf("missing agent_status transition in %v", logMsgs(recs))
	}
	if status["from"] != "working" || status["to"] != "idle" {
		t.Fatalf("agent_status = %v, want working -> idle", status)
	}
	settle, ok := logByMsg(recs, "settle")
	if !ok {
		t.Fatalf("missing settle in %v", logMsgs(recs))
	}
	if settle["agent_status"] != "idle" || settle["decision"] != ActionDispatched {
		t.Fatalf("settle = %v, want idle / dispatched", settle)
	}
	result, ok := logByMsg(recs, "result")
	if !ok {
		t.Fatalf("missing result in %v", logMsgs(recs))
	}
	if result["action"] != ActionDispatched {
		t.Fatalf("result.action = %v, want dispatched", result["action"])
	}
}

func TestRunLogsDedupeOutcome(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	pr := fixtureEligiblePR()
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	if _, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow); err != nil {
		t.Fatalf("seed Run() unexpected error: %v", err)
	}

	buf, ctx := withLog(t)
	got, err := Run(ctx, h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{pr}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedDeduped {
		t.Fatalf("results = %+v, want skipped_deduped", got.Results)
	}
	recs := logRecords(t, buf)
	result, ok := logByMsg(recs, "result")
	if !ok {
		t.Fatalf("missing result in %v", logMsgs(recs))
	}
	if result["action"] != ActionSkippedDeduped {
		t.Fatalf("result.action = %v, want skipped_deduped", result["action"])
	}
	if hasLog(recs, "dispatch_send") {
		t.Fatalf("deduped skip logged dispatch_send: %v", logMsgs(recs))
	}
}

func TestRunLiveLogsBlockedGateTimeout(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	h := &scriptHerdr{lists: [][]herdr.Agent{{blockedAgent("w2:pX", "w2:tX")}}}
	buf, ctx := withLog(t)
	got, err := Run(ctx, h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if !errors.Is(err, ErrTimeout) {
		t.Fatalf("error = %v, want ErrTimeout", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionGateTimeout {
		t.Fatalf("results = %+v, want gate_timeout", got.Results)
	}
	recs := logRecords(t, buf)
	end, ok := logByMsg(recs, "gate_wait_end")
	if !ok {
		t.Fatalf("missing gate_wait_end in %v", logMsgs(recs))
	}
	if end["tab_id"] != "w2:tX" || end["agent_status"] != "blocked" {
		t.Fatalf("gate_wait_end = %v, want tab_id w2:tX agent_status blocked", end)
	}
	result, ok := logByMsg(recs, "result")
	if !ok {
		t.Fatalf("missing result in %v", logMsgs(recs))
	}
	if result["action"] != ActionGateTimeout {
		t.Fatalf("result.action = %v, want gate_timeout", result["action"])
	}
	if hasLog(recs, "dispatch_send") {
		t.Fatalf("blocked gate timeout logged dispatch_send: %v", logMsgs(recs))
	}
}

func TestRunDryRunLogsWouldDispatchWithoutGateWait(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	buf, ctx := withLog(t)
	got, err := Run(ctx, h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if got.Results[0].Action != ActionWouldDispatch {
		t.Fatalf("action = %q, want would_dispatch", got.Results[0].Action)
	}
	recs := logRecords(t, buf)
	if hasLog(recs, "gate_wait_start") {
		t.Fatalf("dry-run logged gate wait: %v", logMsgs(recs))
	}
	result, ok := logByMsg(recs, "result")
	if !ok {
		t.Fatalf("missing result in %v", logMsgs(recs))
	}
	if result["action"] != ActionWouldDispatch {
		t.Fatalf("result.action = %v, want would_dispatch", result["action"])
	}
}

func TestWaitForSettleLogsTimeoutDecision(t *testing.T) {
	t.Parallel()

	baseline := seqAgent("w2:pC", "w2:tC", "idle", 1)
	h := &scriptHerdr{lists: [][]herdr.Agent{{baseline}}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}
	buf, ctx := withLog(t)
	_, err := waitForSettle(ctx, h, "w2:pC", baseline, []string{"idle", "done"}, 50*time.Millisecond, time.Millisecond, clock, sleeper)
	if err == nil {
		t.Fatal("waitForSettle() error = nil, want timeout")
	}
	recs := logRecords(t, buf)
	settle, ok := logByMsg(recs, "settle")
	if !ok {
		t.Fatalf("missing settle in %v", logMsgs(recs))
	}
	if settle["decision"] != ActionDispatchedTimeout {
		t.Fatalf("settle.decision = %v, want dispatched_timeout", settle["decision"])
	}
}

func withLog(t *testing.T) (*bytes.Buffer, context.Context) {
	t.Helper()
	var buf bytes.Buffer
	h := slog.NewJSONHandler(&buf, &slog.HandlerOptions{Level: slog.LevelDebug})
	return &buf, runlog.WithLogger(context.Background(), slog.New(h))
}

func logRecords(t *testing.T, buf *bytes.Buffer) []map[string]any {
	t.Helper()
	var recs []map[string]any
	dec := json.NewDecoder(bytes.NewReader(buf.Bytes()))
	for {
		var rec map[string]any
		if err := dec.Decode(&rec); err != nil {
			if err == io.EOF {
				break
			}
			t.Fatalf("decode log: %v\n%s", err, buf.String())
		}
		recs = append(recs, rec)
	}
	return recs
}

func logMsgs(recs []map[string]any) []string {
	out := make([]string, 0, len(recs))
	for _, r := range recs {
		if s, ok := r["msg"].(string); ok {
			out = append(out, s)
		}
	}
	return out
}

func hasLog(recs []map[string]any, msg string) bool {
	return slices.Contains(logMsgs(recs), msg)
}

func logByMsg(recs []map[string]any, msg string) (map[string]any, bool) {
	for _, r := range recs {
		if r["msg"] == msg {
			return r, true
		}
	}
	return nil, false
}
