package mailbox

import (
	"context"
	"errors"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
)

// The held_reason values of a Report. Every hold maps to one of them.
const (
	HeldTransportUnreachable = "transport unreachable"
	HeldComposerBusy         = "composer busy"
	HeldTargetNotReady       = "target not ready"
	HeldEarlierMessage       = "earlier message held"
	HeldHibernated           = "hibernated"
	HeldUnconfirmed          = "delivery unconfirmed"
	HeldOther                = "other"
)

// Report is the status --json object of one message. Fields that do not
// apply are empty and omitted. Times use timeLayout, which is RFC3339.
type Report struct {
	ID          string `json:"id"`
	State       string `json:"state"`
	HeldReason  string `json:"held_reason,omitempty"` //nolint:tagliatelle // status --json shape
	Reason      string `json:"reason,omitempty"`
	StoredAt    string `json:"stored_at,omitempty"`    //nolint:tagliatelle // status --json shape
	DeliveredAt string `json:"delivered_at,omitempty"` //nolint:tagliatelle // status --json shape
	Error       string `json:"error,omitempty"`
}

// Report flushes the message's queue, like Status, and reports it under
// id as asked. State is queued, held, delivered, failed, expired, or
// unknown. A message whose wake failed is failed: it stays queued and
// the wake is retried, and Error says why the last one failed.
func (m *Mailbox) Report(ctx context.Context, id string) Report {
	msg, err := m.flushed(ctx, id)
	switch {
	case errors.Is(err, errUnknownMessage):
		return Report{ID: id, State: "unknown"}
	case err != nil:
		// The flush failed, or the client id names more than one message.
		var getErr error
		if msg, getErr = m.get(id); getErr != nil {
			return Report{ID: id, State: "unknown", Error: getErr.Error()}
		}
	}
	r := Report{ID: id, StoredAt: ReportTime(msg.SentAt)}
	if msg.DeliveredAt != nil {
		r.DeliveredAt = ReportTime(*msg.DeliveredAt)
	}
	reason, held := strings.CutPrefix(msg.State, "held: ")
	switch {
	case msg.State == stateDelivered:
		r.State = msg.State
	case msg.State == stateDeliveredUnprocessed:
		r.State = msg.State
		r.Reason = msg.UnprocessedReason
	case msg.State == stateExpired:
		r.State = msg.State
		r.Reason = msg.ExpireReason
	case held && reason == HoldWakeFailed:
		r.State = "failed"
		r.Error = m.wakeError(msg)
	case held:
		r.State = "held"
		r.HeldReason = heldReason(reason)
	default:
		r.State = "queued"
	}
	if err != nil && r.Error == "" {
		r.Error = err.Error()
	}
	return r
}

// ReportTime formats a Report time: RFC3339 in UTC, to the millisecond.
func ReportTime(t time.Time) string {
	return t.UTC().Format(timeLayout)
}

// wakeError is why the last wake of msg's handle failed.
func (m *Mailbox) wakeError(msg message) string {
	if rec, ok := m.reg.Hibernation(msg.Handle); ok && rec.Status == hibernate.StatusWakeFailed && rec.Reason != "" {
		return "wake failed: " + rec.Reason
	}
	return "wake failed"
}

// heldReason maps a stored hold reason to one of the Held* values.
func heldReason(reason string) string {
	switch {
	case unreachableState("held: " + reason):
		return HeldTransportUnreachable
	case strings.HasPrefix(reason, "waiting for an earlier message "):
		return HeldEarlierMessage
	}
	switch reason {
	case adapter.HoldDraft, adapter.HoldDraftUnknown, adapter.HoldScrolled, adapter.HoldFocused:
		return HeldComposerBusy
	case string(adapter.StateBlocked), string(adapter.StateNoAgent), string(adapter.StateUnknown), string(adapter.StateGone), HoldLoginExpired:
		return HeldTargetNotReady
	case HoldHibernated, HoldWaking:
		return HeldHibernated
	case adapter.HoldAmbiguous, adapter.HoldAmbiguousUnverified:
		return HeldUnconfirmed
	default:
		return HeldOther
	}
}
