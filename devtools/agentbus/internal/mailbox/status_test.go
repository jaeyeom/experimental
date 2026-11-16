package mailbox

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

func TestHeldReasonIsOneOfAFixedSet(t *testing.T) {
	for raw, want := range map[string]string{
		"unreachable": HeldTransportUnreachable,
		"machine m2 unreachable: host unreachable": HeldTransportUnreachable,
		adapter.HoldDraft:                          HeldComposerBusy,
		adapter.HoldDraftUnknown:                   HeldComposerBusy,
		adapter.HoldScrolled:                       HeldComposerBusy,
		adapter.HoldFocused:                        HeldComposerBusy,
		string(adapter.StateBlocked):               HeldTargetNotReady,
		string(adapter.StateNoAgent):               HeldTargetNotReady,
		string(adapter.StateUnknown):               HeldTargetNotReady,
		string(adapter.StateGone):                  HeldTargetNotReady,
		"waiting for an earlier message m-1":       HeldEarlierMessage,
		HoldHibernated:                             HeldHibernated,
		HoldWaking:                                 HeldHibernated,
		adapter.HoldAmbiguous:                      HeldUnconfirmed,
		adapter.HoldAmbiguousUnverified:            HeldUnconfirmed,
		"something new":                            HeldOther,
	} {
		if got := heldReason(raw); got != want {
			t.Errorf("heldReason(%q) = %q, want %q", raw, got, want)
		}
	}
}

func TestReportOfAHeldThenDeliveredMessage(t *testing.T) {
	w := newHoldWorld(t)
	w.ad.Composer = func(string) (bool, string, error) { return false, adapter.HoldDraft, nil }
	res := w.send(t, "hello")
	w.flush(t)
	got := w.box.Report(context.Background(), res.ID)
	stored := w.clock.now().UTC().Format(timeLayout)
	if got != (Report{ID: res.ID, State: "held", HeldReason: HeldComposerBusy, StoredAt: stored}) {
		t.Fatalf("held report = %+v", got)
	}

	w.ad.Composer = nil
	w.clock.add(90 * time.Second)
	got = w.box.Report(context.Background(), res.ID)
	delivered := w.clock.now().UTC().Format(timeLayout)
	if got != (Report{ID: res.ID, State: "delivered", StoredAt: stored, DeliveredAt: delivered}) {
		t.Fatalf("delivered report = %+v", got)
	}
}

func TestReportOfAFailedWakeHasItsError(t *testing.T) {
	w := newWakeWorld(t)
	w.waker.err = errors.New("relaunched agent not ready within 1m0s: agent is working")
	res := w.send(t, "wake up")
	w.box.wakes.Wait()
	got := w.box.Report(context.Background(), res.ID)
	if got.State != "failed" || got.HeldReason != "" || got.Error != "wake failed: relaunched agent not ready within 1m0s: agent is working" || got.StoredAt == "" || got.DeliveredAt != "" {
		t.Fatalf("report = %+v", got)
	}
}

func TestReportOfAnUnknownID(t *testing.T) {
	w := newHoldWorld(t)
	if got := w.box.Report(context.Background(), "m-nope"); got != (Report{ID: "m-nope", State: "unknown"}) {
		t.Fatalf("report = %+v", got)
	}
}
