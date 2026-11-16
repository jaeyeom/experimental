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
	// OutcomeWaiting is a message agentbus stored but has not delivered
	// yet. It is never shown as delivered.
	OutcomeWaiting = "waiting"
	// OutcomeLegacyQueued is how records before waiting stored a bus
	// hold. It reads as OutcomeWaiting.
	OutcomeLegacyQueued = "delivered (queued)"
	// OutcomeResend is the one follow-up of an answer the owner had not taken up.
	OutcomeResend = "resend"

	// MaxDeliveryAttempts is the first send plus retries of one answer.
	MaxDeliveryAttempts = 3
)

// Delivery is one attempt to deliver a human message to an agent: an
// answer, a reply, or the first note of a new thread.
// Message is the Time of the thread message it carries. A record from
// before that field is an answer delivery. Compose marks a reply or a
// new thread, which RetryDue does not retry. BusID is the agentbus
// message id that send printed, or the client id for an answer. Reason
// is the agentbus held_reason while waiting. DeliveredAt is when the
// target got it. Retrying marks a failure agentbus is still retrying.
type Delivery struct {
	At          time.Time `json:"at"`
	Target      string    `json:"target"`
	Outcome     string    `json:"outcome"`
	Error       string    `json:"error,omitempty"`
	Message     time.Time `json:"message,omitzero"`
	Compose     bool      `json:"compose,omitempty"`
	BusID       string    `json:"busId,omitempty"`
	Reason      string    `json:"reason,omitempty"`
	DeliveredAt time.Time `json:"deliveredAt,omitzero"`
	Retrying    bool      `json:"retrying,omitempty"`
}

// State is the delivery state: OutcomeWaiting, OutcomeDelivered,
// OutcomeFailed, or OutcomeResend. A legacy queued record is waiting.
func (d Delivery) State() string {
	if d.Outcome == OutcomeLegacyQueued {
		return OutcomeWaiting
	}
	return d.Outcome
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
// for this answer failed. A delivered or waiting attempt is not undelivered.
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
// is recorded as waiting and is not an error. An empty local template
// records nothing. The record is for the answer message and keeps the
// agentbus message id.
func (l *Ledger) DeliverAnswer(item *Item, out io.Writer) (Item, error) {
	if item == nil {
		return Item{}, fmt.Errorf("decide item is nil")
	}
	d, nerr := notifyAttempt(item, out)
	if d.Outcome == "" && nerr == nil {
		stored, err := l.Get(item.ID)
		if err != nil {
			return Item{}, err
		}
		return stored, nil
	}
	d.At = l.clock().UTC()
	d.Message = item.AnsweredAt
	updated, err := l.appendDelivery(item.ID, d)
	if err != nil {
		return Item{}, err
	}
	return updated, nerr //nolint:wrapcheck // the notify error is already wrapped
}

// RecordCompose stores d as the delivery of the human message at
// msgAt on id, a reply or the first note of a new thread. The time is
// now. A failure is also id's notifyError; any other outcome clears it.
func (l *Ledger) RecordCompose(id string, msgAt time.Time, d Delivery) (Item, error) {
	d.At = l.clock().UTC()
	d.Message = msgAt.UTC()
	d.Compose = true
	return l.appendDelivery(id, d)
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
		case OutcomeDelivered, OutcomeWaiting:
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
	if last.Outcome != OutcomeFailed || last.Retrying {
		return false
	}
	return !now.Before(last.At.Add(DeliveryBackoff(n)))
}

// attemptsSinceAnswer is the answer deliveries of the current answer.
// A reply or new-thread record is not one.
func attemptsSinceAnswer(it *Item) []Delivery {
	if it == nil {
		return nil
	}
	var out []Delivery
	for _, d := range it.Delivery {
		if d.Compose {
			continue
		}
		if it.AnsweredAt.IsZero() || !d.At.Before(it.AnsweredAt) {
			out = append(out, d)
		}
	}
	return out
}
