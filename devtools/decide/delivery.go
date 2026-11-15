package decide

import (
	"fmt"
	"io"
	"time"
)

const (
	// OutcomeDelivered is an attempt the target accepted.
	OutcomeDelivered = "delivered"
	// OutcomeFailed is a definite delivery failure.
	OutcomeFailed = "failed"
	// OutcomeQueued is an agentbus hold. The bus delivers one copy later.
	OutcomeQueued = "delivered (queued)"
	// OutcomeResend is the one follow-up of an answer the owner had not taken up.
	OutcomeResend = "resend"

	// MaxDeliveryAttempts is the first send plus retries of one answer.
	MaxDeliveryAttempts = 3
)

// Delivery is one attempt to deliver an answer.
type Delivery struct {
	At      time.Time `json:"at"`
	Target  string    `json:"target"`
	Outcome string    `json:"outcome"`
	Error   string    `json:"error,omitempty"`
}

// DeliveryKey is the stable retry key for item's current answer,
// <id>:<answeredAt> in UTC. A new answer has a new key.
func DeliveryKey(item *Item) string {
	if item == nil {
		return ""
	}
	return item.ID + ":" + item.AnsweredAt.UTC().Format(time.RFC3339Nano)
}

// DeliveryBackoff is how long to wait after attempts failures before
// the next try. The first retry waits 5s. The second waits 20s.
func DeliveryBackoff(attempts int) time.Duration {
	if attempts <= 1 {
		return 5 * time.Second
	}
	return 20 * time.Second
}

// Undelivered is the error on an answered item whose latest attempt
// for this answer failed. A delivered or queued attempt is not undelivered.
func Undelivered(item *Item) (string, bool) {
	attempts := attemptsSinceAnswer(item)
	if len(attempts) == 0 {
		return "", false
	}
	last := attempts[len(attempts)-1]
	if last.Outcome != OutcomeFailed {
		return "", false
	}
	return last.Error, true
}

// DeliverAnswer notifies item and appends one delivery record.
// A definite failure is recorded and returned. A held agentbus target
// is recorded as queued and is not an error. An empty local template
// records nothing.
func (l *Ledger) DeliverAnswer(item *Item, out io.Writer) (Item, error) {
	if item == nil {
		return Item{}, fmt.Errorf("decide item is nil")
	}
	target, outcome, nerr := notifyAttempt(item, out)
	if outcome == "" && nerr == nil {
		stored, err := l.Get(item.ID)
		if err != nil {
			return Item{}, err
		}
		return stored, nil
	}
	msg := ""
	if nerr != nil {
		msg = nerr.Error()
	}
	updated, err := l.appendDelivery(item.ID, Delivery{
		At:      l.clock().UTC(),
		Target:  target,
		Outcome: outcome,
		Error:   msg,
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nerr //nolint:wrapcheck // the notify error is already wrapped
}

// RetryDue delivers again for answered items whose latest attempt failed,
// once the backoff has elapsed, up to MaxDeliveryAttempts.
func (l *Ledger) RetryDue(now time.Time, out io.Writer) error {
	items, err := l.List()
	if err != nil {
		return err
	}
	var first error
	for i := range items {
		if !deliveryDue(&items[i], now) {
			continue
		}
		if _, err := l.DeliverAnswer(&items[i], out); err != nil && first == nil {
			first = err
		}
	}
	return first //nolint:wrapcheck // DeliverAnswer already names the failure
}

func (l *Ledger) appendDelivery(id string, d Delivery) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		d.At = d.At.UTC()
		it.Delivery = append(it.Delivery, d)
		switch d.Outcome {
		case OutcomeFailed:
			it.NotifyError = d.Error
		case OutcomeResend:
			it.NotifyError = d.Error
		case OutcomeDelivered, OutcomeQueued:
			it.NotifyError = ""
		}
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

func deliveryDue(it *Item, now time.Time) bool {
	if it == nil || it.Kind != KindRelay || it.State != StateAnswered {
		return false
	}
	attempts := attemptsSinceAnswer(it)
	n := len(attempts)
	if n == 0 || n >= MaxDeliveryAttempts {
		return false
	}
	last := attempts[n-1]
	if last.Outcome != OutcomeFailed {
		return false
	}
	return !now.Before(last.At.Add(DeliveryBackoff(n)))
}

func attemptsSinceAnswer(it *Item) []Delivery {
	if it == nil {
		return nil
	}
	if it.AnsweredAt.IsZero() {
		return append([]Delivery(nil), it.Delivery...)
	}
	var out []Delivery
	for _, d := range it.Delivery {
		if !d.At.Before(it.AnsweredAt) {
			out = append(out, d)
		}
	}
	return out
}
