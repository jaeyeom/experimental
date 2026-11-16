package decide

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"
)

// The agentbus status --json held_reason values decide names.
const (
	HeldTransportUnreachable = "transport unreachable"
	HeldComposerBusy         = "composer busy"
)

// DeliveryStatusTimeout bounds one batched `agentbus status --json`.
const DeliveryStatusTimeout = 5 * time.Second

// Mark is the delivery mark of one human message sent to an agent.
// State is OutcomeWaiting, OutcomeDelivered, or OutcomeFailed. Reason
// is the agentbus held_reason while waiting. DeliveredAt is when the
// target got it, and Late how long it waited first. Error is why it
// failed.
type Mark struct {
	State       string
	Reason      string
	DeliveredAt time.Time
	Late        time.Duration
	Error       string
}

// MessageMark is the mark of msg on it: its latest delivery record. A
// record from before Delivery.Message belongs to the answer message.
// A message with no record, such as an agent message, has no mark.
func MessageMark(it *Item, msg Message) (Mark, bool) {
	if it == nil || msg.Author != "human" || msg.Time.IsZero() {
		return Mark{}, false
	}
	for i := len(it.Delivery) - 1; i >= 0; i-- {
		d := it.Delivery[i]
		if d.State() == OutcomeResend || !deliveryFor(it, d, msg.Time) {
			continue
		}
		return markOf(d), true
	}
	return Mark{}, false
}

func deliveryFor(it *Item, d Delivery, at time.Time) bool {
	if !d.Message.IsZero() {
		return d.Message.Equal(at)
	}
	return !it.AnsweredAt.IsZero() && at.Equal(it.AnsweredAt) && !d.At.Before(it.AnsweredAt)
}

func markOf(d Delivery) Mark {
	m := Mark{State: d.State(), Reason: d.Reason, Error: d.Error}
	if m.State == OutcomeDelivered {
		m.DeliveredAt = d.DeliveredAt
		if m.DeliveredAt.IsZero() {
			m.DeliveredAt = d.At
		}
		if late := m.DeliveredAt.Sub(d.At); late > 0 {
			m.Late = late
		}
	}
	return m
}

// String is the mark as the thread shows it, with times in loc:
// "waiting: bus down", "delivered 14:05, 3 min late", "failed: <error>".
func (m Mark) String(loc *time.Location) string {
	switch m.State {
	case OutcomeWaiting:
		if reason := heldText(m.Reason); reason != "" {
			return "waiting: " + reason
		}
		return "waiting"
	case OutcomeDelivered:
		if loc == nil {
			loc = time.Local
		}
		text := "delivered " + m.DeliveredAt.In(loc).Format("15:04")
		if mins := int(m.Late.Round(time.Minute) / time.Minute); mins > 0 {
			text += fmt.Sprintf(", %d min late", mins)
		}
		return text
	case OutcomeFailed:
		if m.Error == "" {
			return "failed"
		}
		return "failed: " + m.Error
	}
	return m.State
}

// heldText is a held_reason in the thread's words.
func heldText(reason string) string {
	switch reason {
	case HeldTransportUnreachable:
		return "bus down"
	case HeldComposerBusy:
		return "target composer busy"
	case "other":
		return ""
	}
	return reason
}

// sendHeldReason is the held_reason of a held agentbus send, from its
// "held: <reason>" state. The held exit, an unreachable transport, is
// bus down. A reason decide does not sort is left for status to name.
func sendHeldReason(stdout string, runErr error) string {
	reason, held := strings.CutPrefix(strings.TrimSpace(busSendState(stdout)), "held: ")
	if !held {
		return ""
	}
	var exit interface{ ExitCode() int }
	switch {
	case strings.Contains(reason, "unreachable"), errors.As(runErr, &exit) && exit.ExitCode() == 3:
		return HeldTransportUnreachable
	case strings.Contains(reason, "draft"), reason == "pane scrolled up", reason == "pane focused":
		return HeldComposerBusy
	}
	return ""
}

// Pending reports whether agentbus may still change d: it is waiting,
// or failed while agentbus retries it. Only a bus delivery is pending.
func (d Delivery) Pending() bool {
	if !strings.HasPrefix(d.Target, BusPanePrefix) {
		return false
	}
	return d.State() == OutcomeWaiting || (d.Outcome == OutcomeFailed && d.Retrying)
}

// HasPending reports whether any delivery on it is pending.
func HasPending(it *Item) bool {
	for _, d := range it.Delivery {
		if d.Pending() && statusID(it, d) != "" {
			return true
		}
	}
	return false
}

// statusID is the id agentbus status knows d by: its message id, or for
// a legacy answer record the answer's delivery key, its client id.
func statusID(it *Item, d Delivery) string {
	if d.BusID != "" {
		return d.BusID
	}
	if d.Compose || !d.Message.IsZero() || d.At.Before(it.AnsweredAt) {
		return ""
	}
	return DeliveryKey(it)
}

// busReport is one line of `agentbus status --json`.
type busReport struct {
	ID          string `json:"id"`
	State       string `json:"state"`
	HeldReason  string `json:"held_reason"`  //nolint:tagliatelle // agentbus status --json shape
	DeliveredAt string `json:"delivered_at"` //nolint:tagliatelle // agentbus status --json shape
	Error       string `json:"error"`
}

// CheckDeliveries asks agentbus about every pending delivery in one
// `agentbus status --json <id>...` and stores what it reports. With
// nothing pending it runs nothing. It reports whether a record changed.
func (l *Ledger) CheckDeliveries(ctx context.Context, run AgentbusRunner) (bool, error) {
	items, err := l.List()
	if err != nil {
		return false, err
	}
	ids := pendingIDs(items)
	if len(ids) == 0 {
		return false, nil
	}
	if run == nil {
		run = ExecAgentbus
	}
	ctx, cancel := context.WithTimeout(ctx, DeliveryStatusTimeout)
	defer cancel()
	out, runErr := run(ctx, append([]string{"status", "--json"}, ids...))
	reports := parseReports(string(out))
	if len(reports) == 0 {
		if runErr == nil {
			runErr = fmt.Errorf("agentbus status: no report in %q", strings.TrimSpace(string(out)))
		}
		return false, fmt.Errorf("agentbus status: %w", runErr)
	}
	changed := false
	err = l.locked(true, func(st *store) error {
		for i := range st.Items {
			if applyReports(&st.Items[i], reports) {
				changed = true
			}
		}
		return nil
	})
	if err != nil {
		return false, err
	}
	return changed, nil
}

// pendingIDs is the status id of each pending delivery, once each.
func pendingIDs(items []Item) []string {
	var ids []string
	seen := map[string]bool{}
	for i := range items {
		for _, d := range items[i].Delivery {
			id := statusID(&items[i], d)
			if d.Pending() && id != "" && !seen[id] {
				seen[id] = true
				ids = append(ids, id)
			}
		}
	}
	return ids
}

// applyReports applies reports to it's pending deliveries and reports
// whether one changed.
func applyReports(it *Item, reports map[string]busReport) bool {
	changed := false
	for j := range it.Delivery {
		d := &it.Delivery[j]
		r, ok := reports[statusID(it, *d)]
		if ok && d.Pending() && applyReport(d, r) {
			changed = true
		}
	}
	return changed
}

func parseReports(out string) map[string]busReport {
	reports := map[string]busReport{}
	sc := bufio.NewScanner(strings.NewReader(out))
	sc.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for sc.Scan() {
		var r busReport
		if err := json.Unmarshal(sc.Bytes(), &r); err != nil || r.ID == "" || r.State == "" {
			continue
		}
		reports[r.ID] = r
	}
	return reports
}

// applyReport moves d to what agentbus reports. queued and held are
// waiting. delivered keeps when. failed is a wake agentbus retries.
// expired and unknown are final failures. It reports a change.
func applyReport(d *Delivery, r busReport) bool {
	before := *d
	switch r.State {
	case "delivered":
		d.Outcome, d.Reason, d.Error, d.Retrying = OutcomeDelivered, "", "", false
		d.DeliveredAt = time.Now().UTC()
		if t, err := time.Parse(time.RFC3339Nano, r.DeliveredAt); err == nil {
			d.DeliveredAt = t.UTC()
		}
	case "queued":
		d.Outcome, d.Reason, d.Error, d.Retrying = OutcomeWaiting, "", "", false
	case "held":
		d.Outcome, d.Reason, d.Error, d.Retrying = OutcomeWaiting, r.HeldReason, "", false
	case "failed":
		d.Outcome, d.Reason, d.Retrying = OutcomeFailed, "", true
		d.Error = nonEmpty(r.Error, "wake failed")
	case "expired":
		d.Outcome, d.Reason, d.Retrying = OutcomeFailed, "", false
		d.Error = nonEmpty(r.Error, "expired before delivery")
	case "unknown":
		d.Outcome, d.Reason, d.Retrying = OutcomeFailed, "", false
		d.Error = nonEmpty(r.Error, "agentbus does not know this message")
	default:
		return false
	}
	return *d != before
}

func nonEmpty(s, fallback string) string {
	if s != "" {
		return s
	}
	return fallback
}
