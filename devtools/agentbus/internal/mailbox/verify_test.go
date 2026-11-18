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

	// Simulate Claude Code replying with Login expired.
	w.turns[holdTo] = append(w.turns[holdTo],
		lastturn.Turn{Role: "user", Text: "[agentbus " + res.ID + " from ops/sender → ops/x]\nfirst task"},
		lastturn.Turn{Role: "assistant", Text: "Login expired - Please run /login"},
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

	// Sender notified once per outage.
	outageNotices := textsWith(w.ad.Texts, "ops/x is not processing messages (login expired)")
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
	if got := textsWith(w.ad.Texts, "ops/x is not processing messages (login expired)"); got != 1 {
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
		lastturn.Turn{Role: "assistant", Text: "Login expired - Please run /login"},
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
