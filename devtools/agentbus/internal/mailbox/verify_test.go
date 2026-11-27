package mailbox

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lastturn"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

type verifyWorld struct {
	dir    string
	clock  *fakeNow
	reg    *registry.Registry
	ad     *fake.Adapter
	box    *Mailbox
	log    *eventlog.Log
	turns  map[string][]lastturn.Turn
	agents []adapter.Agent
}

func newVerifyWorld(t *testing.T) *verifyWorld {
	t.Helper()
	dir := t.TempDir()
	clock := &fakeNow{t: time.Date(2026, 11, 18, 7, 0, 0, 0, time.UTC)}
	agents := []adapter.Agent{
		{Handle: holdFrom, Kind: "claude", Status: "idle", TabLabel: "sender-tab", StateSeq: 1},
		{Handle: holdTo, Kind: "claude", Status: "idle", TabLabel: "worker-tab", StateSeq: 10},
		{Handle: "herdr:local/codex/w1:p9", Kind: "codex", Status: "idle", TabLabel: "codex-tab", StateSeq: 1},
	}
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			holdFrom:                  adapter.StateIdle,
			holdTo:                    adapter.StateIdle,
			"herdr:local/codex/w1:p9": adapter.StateIdle,
		},
		Result: map[string]adapter.Result{
			holdFrom:                  adapter.ResultAccepted,
			holdTo:                    adapter.ResultAccepted,
			"herdr:local/codex/w1:p9": adapter.ResultAccepted,
		},
		PaneHandles: map[string]string{
			"sender": holdFrom,
			"worker": holdTo,
			"codex":  "herdr:local/codex/w1:p9",
		},
		InventoryAgents: agents,
	}
	log := eventlog.Open(dir)
	reg, err := registry.Open(dir, []adapter.Adapter{ad}, log)
	if err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=sender"}, "ops/sender", "sending"); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=worker"}, "ops/x", "working"); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=codex"}, "ops/codex", "codex"); err != nil {
		t.Fatal(err)
	}
	box, err := Open(dir, reg, []adapter.Adapter{ad}, log, 0)
	if err != nil {
		t.Fatal(err)
	}
	box.now = clock.now
	w := &verifyWorld{
		dir:    dir,
		clock:  clock,
		reg:    reg,
		ad:     ad,
		box:    box,
		log:    log,
		turns:  map[string][]lastturn.Turn{holdTo: {}},
		agents: agents,
	}
	box.SetTranscript(func(ag adapter.Agent) ([]lastturn.Turn, bool) {
		turns, ok := w.turns[ag.Handle]
		return turns, ok
	})
	return w
}

func (w *verifyWorld) sendTo(t *testing.T, to, body string) Result {
	t.Helper()
	res, err := w.box.Send(context.Background(), []string{"HERDR_PANE_ID=sender"}, 1, to, body, Options{})
	if err != nil {
		t.Fatalf("Send(%s): %v", to, err)
	}
	return res
}

func (w *verifyWorld) flush(t *testing.T, handle string) {
	t.Helper()
	if err := w.box.flushHandle(context.Background(), handle); err != nil {
		t.Fatalf("flushHandle: %v", err)
	}
}

func TestVerifyDeliveryAuthErrorAndOutageHold(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	res := w.sendTo(t, "ops/x", "first task")
	if res.State != stateDelivered {
		t.Fatalf("initial state = %q, want %q", res.State, stateDelivered)
	}

	// Simulate Claude Code replying with synthetic Login expired.
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + res.ID + " from ops/sender → ops/x]\nfirst task"},
		lastturn.Turn{Role: "assistant", Text: "Login expired · Please run /login", IsAPIErrorMessage: true, Model: "<synthetic>"},
	)
	w.flush(t, holdTo)

	rep := w.box.Report(context.Background(), res.ID)
	if rep.State != stateDeliveredUnprocessed || rep.Reason != ReasonAuth {
		t.Fatalf("Report = %+v", rep)
	}
	status, err := w.box.Status(context.Background(), res.ID)
	if err != nil || !strings.Contains(status, "state: delivered-unprocessed") || !strings.Contains(status, "reason: auth") {
		t.Fatalf("Status = %q, err=%v", status, err)
	}
	evs, err := w.log.Read(eventlog.Filter{Event: eventlog.EventUnprocessed})
	if err != nil || len(evs) != 1 || !strings.Contains(evs[0], `"reason":"auth"`) || !strings.Contains(evs[0], res.ID) {
		t.Fatalf("unprocessed events = %+v, err=%v", evs, err)
	}

	// Sender notified once per outage, using text free of auth error patterns.
	outageNotices := textsWith(w.ad.Texts, "ops/x is paused waiting for authentication")
	if outageNotices != 1 {
		t.Fatalf("outage notices = %d, texts=%q", outageNotices, w.ad.Texts)
	}

	// Later message to ops/x is held as target login expired and NOT pasted.
	beforeTexts := len(w.ad.Texts)
	heldRes := w.sendTo(t, "ops/x", "second task")
	if heldRes.State != "held: "+HoldLoginExpired {
		t.Fatalf("second message state = %q, want held: %s", heldRes.State, HoldLoginExpired)
	}
	if len(w.ad.Texts) != beforeTexts {
		t.Fatalf("pasted while login expired: %q", w.ad.Texts[beforeTexts:])
	}

	// A second auth-error verification doesn't notify the sender again.
	w.flush(t, holdTo)
	if got := textsWith(w.ad.Texts, "ops/x is paused waiting for authentication"); got != 1 {
		t.Fatalf("notified twice in same outage: %q", w.ad.Texts)
	}

	// Simulate user logging back in and a normal turn succeeding.
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "hi"},
		lastturn.Turn{Role: "assistant", Text: "Ready to help!"},
	)
	w.flush(t, holdTo)

	heldRep := w.box.Report(context.Background(), heldRes.ID)
	if heldRep.State != stateDelivered {
		t.Fatalf("held message after successful turn = %+v, want delivered", heldRep)
	}
	if textsWith(w.ad.Texts, "second task") != 1 {
		t.Fatalf("held message was not pasted after recovery: %q", w.ad.Texts)
	}
}

func TestVerifyDeliveryNoTurnWithinWindow(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	res := w.sendTo(t, "ops/x", "hello?")
	w.clock.add(61 * time.Second)
	w.flush(t, holdTo)

	rep := w.box.Report(context.Background(), res.ID)
	if rep.State != stateDeliveredUnprocessed || rep.Reason != ReasonNoTurn {
		t.Fatalf("Report = %+v, want delivered-unprocessed/no-turn", rep)
	}
}

func TestVerifyDeliveryNormalReplyAndNoTranscriptKind(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	// Normal reply keeps delivered.
	r1 := w.sendTo(t, "ops/x", "normal")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r1.ID + "]\nnormal"},
		lastturn.Turn{Role: "assistant", Text: "Working on it."},
	)
	w.clock.add(5 * time.Second)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), r1.ID); rep.State != stateDelivered {
		t.Fatalf("normal reply report = %+v", rep)
	}

	// Kind without transcript keeps delivered even after window.
	r2 := w.sendTo(t, "ops/codex", "to codex")
	w.clock.add(2 * time.Minute)
	w.flush(t, "herdr:local/codex/w1:p9")
	if rep := w.box.Report(context.Background(), r2.ID); rep.State != stateDelivered {
		t.Fatalf("codex report = %+v", rep)
	}
}

func TestReplayFiltersOrderDryRunAndAtMostOnce(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	r1 := w.sendTo(t, "ops/x", "msg-1")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r1.ID + "]\nmsg-1"},
		lastturn.Turn{Role: "assistant", Text: "Login expired · Please run /login"},
	)
	w.flush(t, holdTo)

	// Release outage and send a second message that gets no-turn.
	if _, err := w.box.ReleaseHeld(context.Background(), "ops/x"); err != nil {
		t.Fatal(err)
	}
	w.clock.add(5 * time.Minute)
	r2 := w.sendTo(t, "ops/x", "msg-2")
	w.clock.add(61 * time.Second)
	w.flush(t, holdTo)

	// Dry-run sends nothing.
	before := len(w.ad.Texts)
	dry, err := w.box.Replay(context.Background(), ReplayOptions{DryRun: true})
	if err != nil || len(dry) != 2 || dry[0].OriginalID != r1.ID || dry[1].OriginalID != r2.ID {
		t.Fatalf("dry run = %+v, err=%v", dry, err)
	}
	if len(w.ad.Texts) != before {
		t.Fatalf("dry run sent messages: %q", w.ad.Texts[before:])
	}

	// Filter by reason=no-turn.
	noTurnOnly, err := w.box.Replay(context.Background(), ReplayOptions{Reason: ReasonNoTurn})
	if err != nil || len(noTurnOnly) != 1 || noTurnOnly[0].OriginalID != r2.ID {
		t.Fatalf("reason filter = %+v, err=%v", noTurnOnly, err)
	}
	if textsWith(w.ad.Texts, "resend of "+r2.ID) != 1 {
		t.Fatalf("replayed text missing resend marker: %q", w.ad.Texts)
	}

	// Replaying again without --force skips r2 and replays only r1.
	rest, err := w.box.Replay(context.Background(), ReplayOptions{To: "ops/x"})
	if err != nil || len(rest) != 1 || rest[0].OriginalID != r1.ID {
		t.Fatalf("second replay = %+v, err=%v", rest, err)
	}

	// Nothing left to replay without --force.
	none, err := w.box.Replay(context.Background(), ReplayOptions{})
	if err != nil || len(none) != 0 {
		t.Fatalf("expected 0 remaining, got %+v, err=%v", none, err)
	}

	// --force replays both in original order.
	forced, err := w.box.Replay(context.Background(), ReplayOptions{Force: true})
	if err != nil || len(forced) != 2 || forced[0].OriginalID != r1.ID || forced[1].OriginalID != r2.ID {
		t.Fatalf("forced replay = %+v, err=%v", forced, err)
	}

	// JSON report check for delivered-unprocessed.
	raw, _ := json.Marshal(w.box.Report(context.Background(), r1.ID))
	if !strings.Contains(string(raw), `"state":"delivered-unprocessed"`) || !strings.Contains(string(raw), `"reason":"auth"`) {
		t.Fatalf("unexpected report json: %s", raw)
	}
}

func TestStaleComposerNoticeOncePerTabAndInStatus(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}
	w.ad.ComposerText = "Once fixed, resend your message or type something longer than forty chars"
	w.ad.Composer = func(h string) (bool, string, error) {
		if h == holdTo {
			return false, adapter.HoldDraft, nil
		}
		return true, "", nil
	}
	tenMin := 10 * time.Minute
	w.box.SetStaleComposerAfter(&tenMin)
	w.box.SetHoldNoticeAfter(&tenMin)

	m1 := w.sendTo(t, "ops/x", "first")
	m2 := w.sendTo(t, "ops/x", "second")
	if !strings.HasPrefix(m1.State, "held:") {
		t.Fatalf("m1 state = %q", m1.State)
	}
	w.clock.add(10*time.Minute + time.Second)
	w.flush(t, holdTo)

	wantSub := `held by composer text in worker-tab: "Once fixed, resend your message or type "`
	if got := textsWith(w.ad.Texts, wantSub); got != 1 {
		t.Fatalf("stale composer notices = %d, want 1; texts=%q", got, w.ad.Texts)
	}
	status, err := w.box.Status(context.Background(), m1.ID)
	if err != nil || !strings.Contains(status, wantSub) {
		t.Fatalf("Status(%s) = %q, err=%v", m1.ID, status, err)
	}
	_ = m2
}

func TestVerifyDeliveryProseMentioningAuthErrorDoesNotStartOutage(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	// Normal model reply mentioning login expired and quoting the outage notice.
	r1 := w.sendTo(t, "ops/x", "status check")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r1.ID + "]\nstatus check"},
		lastturn.Turn{Role: "assistant", Model: "claude-opus-4-6", Text: "the other tab's login expired, run the login command there"},
	)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), r1.ID); rep.State != stateDelivered {
		t.Fatalf("prose reply report = %+v, want delivered", rep)
	}

	// Even without model metadata, prose mentioning a pattern does not match.
	r2 := w.sendTo(t, "ops/x", "another check")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r2.ID + "]\nanother check"},
		lastturn.Turn{Role: "assistant", Text: "Please run /login in the worker tab if it says Login expired."},
	)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), r2.ID); rep.State != stateDelivered {
		t.Fatalf("metadata-less prose reply report = %+v, want delivered", rep)
	}

	// Synthetic non-auth API error (e.g. 500/rate limit) does not trigger auth outage.
	r3 := w.sendTo(t, "ops/x", "third check")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r3.ID + "]\nthird check"},
		lastturn.Turn{Role: "assistant", IsAPIErrorMessage: true, Model: "<synthetic>", Text: "API Error: 500 Internal Server Error"},
	)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), r3.ID); rep.State != stateDelivered {
		t.Fatalf("synthetic non-auth API error report = %+v, want delivered", rep)
	}

	// Follow-up message to idle prompt is delivered immediately without holding.
	r4 := w.sendTo(t, "ops/x", "idle prompt follow-up")
	if r4.State != stateDelivered {
		t.Fatalf("follow-up state = %q, want %q", r4.State, stateDelivered)
	}
}

func TestVerifyDeliveryExactKnownAuthErrorStartsOutageWithoutMetadata(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	r1 := w.sendTo(t, "ops/x", "task")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r1.ID + "]\ntask"},
		lastturn.Turn{Role: "assistant", Text: "  Login expired · Please run /login\n"},
	)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), r1.ID); rep.State != stateDeliveredUnprocessed || rep.Reason != ReasonAuth {
		t.Fatalf("exact known auth error report = %+v, want delivered-unprocessed/auth", rep)
	}
	r2 := w.sendTo(t, "ops/x", "held task")
	if r2.State != "held: "+HoldLoginExpired {
		t.Fatalf("follow-up state = %q, want held: %s", r2.State, HoldLoginExpired)
	}
}

func TestOutageRecoveryWhenLatestTurnIsNormalAndDeliversInOrder(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	// Simulate an outage recorded on disk (e.g. from before upgrade or when
	// TurnsAtOutage already equals len(turns)) while the latest assistant turn
	// is actually a normal reply.
	w.turns[holdTo] = []lastturn.Turn{
		{Role: "user", Text: "earlier"},
		{Role: "assistant", Model: "claude-opus-4-6", Text: "Normal reply mentioning login expired"},
	}
	w.box.mu.Lock()
	w.box.authOutage[holdTo] = authOutageRecord{
		Since:         w.clock.now().UTC(),
		TurnsAtOutage: len(w.turns[holdTo]),
		RetryAt:       w.clock.now().UTC().Add(time.Hour),
	}
	w.box.mu.Unlock()

	// A tab at an idle prompt whose last turn is normal is not held.
	r1 := w.sendTo(t, "ops/x", "direct message")
	if r1.State != stateDelivered {
		t.Fatalf("idle tab with normal last turn state = %q, want %q", r1.State, stateDelivered)
	}

	// Now create a real outage with held messages m2, m3, m4, then recover via
	// a normal latest turn (even if TurnsAtOutage is stale or new turn appended).
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r1.ID + "]\ndirect message"},
		lastturn.Turn{Role: "assistant", IsAPIErrorMessage: true, Model: "<synthetic>", Text: "Login expired · Please run /login"},
	)
	w.flush(t, holdTo)

	m2 := w.sendTo(t, "ops/x", "order-1")
	m3 := w.sendTo(t, "ops/x", "order-2")
	m4 := w.sendTo(t, "ops/x", "order-3")
	if m2.State != "held: "+HoldLoginExpired {
		t.Fatalf("m2 state = %q, want held", m2.State)
	}

	// Latest turn becomes normal (e.g. user chatted or stale outage cleared).
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "manual prompt"},
		lastturn.Turn{Role: "assistant", Model: "claude-opus-4-6", Text: "All good now."},
	)
	w.flush(t, holdTo)

	for _, id := range []string{m2.ID, m3.ID, m4.ID} {
		if rep := w.box.Report(context.Background(), id); rep.State != stateDelivered {
			t.Fatalf("message %s after recovery = %+v, want delivered", id, rep)
		}
	}
	idx1 := textIndex(w.ad.Texts, "order-1")
	idx2 := textIndex(w.ad.Texts, "order-2")
	idx3 := textIndex(w.ad.Texts, "order-3")
	if idx1 < 0 || idx2 <= idx1 || idx3 <= idx2 {
		t.Fatalf("out-of-order delivery: idx=(%d,%d,%d) texts=%q", idx1, idx2, idx3, w.ad.Texts)
	}
}

func TestOutageBackoffRetryRecoversAfterSlashLogin(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}

	r1 := w.sendTo(t, "ops/x", "initial")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r1.ID + "]\ninitial"},
		lastturn.Turn{Role: "assistant", IsAPIErrorMessage: true, Model: "<synthetic>", Text: "Login expired · Please run /login"},
	)
	w.flush(t, holdTo)

	m2 := w.sendTo(t, "ops/x", "probe-1")
	m3 := w.sendTo(t, "ops/x", "queued-behind")
	before := len(w.ad.Texts)

	// Before backoff elapses, flushing does not paste either held message.
	w.clock.add(30 * time.Second)
	w.flush(t, holdTo)
	if len(w.ad.Texts) != before {
		t.Fatalf("pasted before backoff: %q", w.ad.Texts[before:])
	}

	// After first backoff (1m), only m2 is probed; m3 remains held until m2 verifies.
	w.clock.add(31 * time.Second)
	w.flush(t, holdTo)
	if textsWith(w.ad.Texts[before:], "probe-1") != 1 || textsWith(w.ad.Texts[before:], "queued-behind") != 0 {
		t.Fatalf("expected only probe-1 pasted on backoff, got %q", w.ad.Texts[before:])
	}
	if rep := w.box.Report(context.Background(), m3.ID); rep.State != "held" {
		t.Fatalf("m3 while probe-1 pending = %+v, want held", rep)
	}

	// If probe-1 still gets an auth error, backoff doubles and sender is not re-notified.
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + m2.ID + "]\nprobe-1"},
		lastturn.Turn{Role: "assistant", IsAPIErrorMessage: true, Model: "<synthetic>", Text: "Login expired · Please run /login"},
	)
	w.flush(t, holdTo)
	if got := textsWith(w.ad.Texts, "ops/x is paused waiting for authentication"); got != 1 {
		t.Fatalf("outage notices = %d, want 1", got)
	}
	if rep := w.box.Report(context.Background(), m2.ID); rep.State != stateDeliveredUnprocessed || rep.Reason != ReasonAuth {
		t.Fatalf("failed probe m2 = %+v", rep)
	}

	// After 1m (less than 2m doubled backoff), m3 is still held.
	before2 := len(w.ad.Texts)
	w.clock.add(61 * time.Second)
	w.flush(t, holdTo)
	if len(w.ad.Texts) != before2 {
		t.Fatalf("pasted before doubled backoff: %q", w.ad.Texts[before2:])
	}

	// User runs /login (no transcript turn added); after 2m backoff, m3 probes and succeeds.
	m4 := w.sendTo(t, "ops/x", "after-probe")
	w.clock.add(60 * time.Second)
	w.flush(t, holdTo)
	if textsWith(w.ad.Texts[before2:], "queued-behind") != 1 || textsWith(w.ad.Texts[before2:], "after-probe") != 0 {
		t.Fatalf("expected only m3 probe pasted, got %q", w.ad.Texts[before2:])
	}

	// Normal assistant reply to m3 clears the outage and delivers m4.
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + m3.ID + "]\nqueued-behind"},
		lastturn.Turn{Role: "assistant", Model: "claude-opus-4-6", Text: "Handled."},
	)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), m4.ID); rep.State != stateDelivered {
		t.Fatalf("m4 after probe success = %+v, want delivered", rep)
	}
	if textsWith(w.ad.Texts, "after-probe") != 1 {
		t.Fatalf("m4 was not delivered: %q", w.ad.Texts)
	}
}

func TestOutageNoticeDoesNotMatchDefaultAuthPatterns(t *testing.T) {
	notice := outageNoticeText("ops/x")
	if matchesAuthError(notice, DefaultAuthErrorPatterns) {
		t.Fatalf("matchesAuthError(%q) = true, want false", notice)
	}
	if matchesAuthErrorSubstring(notice, DefaultAuthErrorPatterns) {
		t.Fatalf("matchesAuthErrorSubstring(%q) = true, want false", notice)
	}
	if isAuthErrorTurn(lastturn.Turn{Role: "assistant", Text: notice}, DefaultAuthErrorPatterns) {
		t.Fatalf("isAuthErrorTurn(normal, %q) = true, want false", notice)
	}
	if isAuthErrorTurn(lastturn.Turn{Role: "assistant", IsAPIErrorMessage: true, Model: "<synthetic>", Text: notice}, DefaultAuthErrorPatterns) {
		t.Fatalf("isAuthErrorTurn(synthetic, %q) = true, want false", notice)
	}
}

func TestCustomAuthErrorPatternsAndProbeTimeoutBackoff(t *testing.T) {
	w := newVerifyWorld(t)
	w.ad.PaneHandles = map[string]string{"sender": holdFrom}
	w.box.SetAuthErrorPatterns([]string{"Custom auth failure"})

	// Prose containing custom pattern does not start an outage.
	r1 := w.sendTo(t, "ops/x", "prose")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r1.ID + "]\nprose"},
		lastturn.Turn{Role: "assistant", Text: "I saw Custom auth failure in the logs earlier."},
	)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), r1.ID); rep.State != stateDelivered {
		t.Fatalf("prose with custom pattern = %+v, want delivered", rep)
	}

	// Exact whole-turn match with custom pattern starts an outage.
	r2 := w.sendTo(t, "ops/x", "trigger")
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + r2.ID + "]\ntrigger"},
		lastturn.Turn{Role: "assistant", Text: "Custom auth failure"},
	)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), r2.ID); rep.State != stateDeliveredUnprocessed || rep.Reason != ReasonAuth {
		t.Fatalf("exact custom pattern = %+v, want delivered-unprocessed/auth", rep)
	}

	// Queue two messages; first probes after 1m and times out (no-turn).
	m1 := w.sendTo(t, "ops/x", "probe-timeout")
	m2 := w.sendTo(t, "ops/x", "wait-behind-timeout")
	w.clock.add(61 * time.Second)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), m1.ID); rep.State != stateDelivered {
		t.Fatalf("m1 probe state = %+v, want delivered", rep)
	}
	w.clock.add(61 * time.Second)
	w.flush(t, holdTo)
	if rep := w.box.Report(context.Background(), m1.ID); rep.State != stateDeliveredUnprocessed || rep.Reason != ReasonNoTurn {
		t.Fatalf("m1 after timeout = %+v, want no-turn", rep)
	}
	// After probe timeout, backoff doubled (2m) so m2 stays held at +61s.
	before := len(w.ad.Texts)
	w.clock.add(61 * time.Second)
	w.flush(t, holdTo)
	if len(w.ad.Texts) != before {
		t.Fatalf("m2 pasted before doubled backoff after probe timeout: %q", w.ad.Texts[before:])
	}
	_ = m2
}

func textIndex(texts []string, sub string) int {
	for i, s := range texts {
		if strings.Contains(s, sub) {
			return i
		}
	}
	return -1
}
