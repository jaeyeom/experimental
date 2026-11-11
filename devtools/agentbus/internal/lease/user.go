package lease

import (
	"errors"
	"fmt"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// PauseAskText is the check-in a holder gets when the user pauses.
func PauseAskText(checkout string) string {
	return "[lease] the user asked for " + checkout + "; please finish up and release"
}

// PausedNote is the note the user gets when a queued pause is granted.
func PausedNote(checkout string) string {
	return "agentbus lease: your pause of " + checkout + " is granted; the checkout is yours. Give it back with: agentbus lease resume"
}

// Pause gives the checkout to the user. A free checkout, or one the
// user already holds, becomes paused at once, with no deadline. While
// an agent holds it, the user goes to the front of the queue and the
// holder is asked to finish up: Reply.Asked names it, its bid ends now,
// and the ask counts as its check-in, so the normal check-in rules
// follow. A.With are handles that may edit under the pause; they are
// added to those already shared with.
func (t *Table) Pause(a Args) (Reply, error) {
	return t.update(a.Checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		h := rec.Holder
		var r Reply
		var events []eventlog.Event
		switch {
		case h == nil && rec.State == StateNeedsAdopt:
			return Reply{}, nil, fmt.Errorf("%s needs adopt; run agentbus lease adopt instead", a.Checkout)
		case h == nil:
			rec.State = StatePaused
			rec.Holder = &holder{Handle: UserHolder, Since: now}
			r.Result = ResultPaused
			events = append(events, eventlog.Event{Event: eventlog.EventLeasePause, Actor: UserHolder, Detail: "paused"})
		case h.Handle == UserHolder:
			if rec.State != StateHeld && rec.State != StatePaused {
				return Reply{}, nil, fmt.Errorf("%s is %s; finish that first", a.Checkout, rec.State)
			}
			if rec.State == StateHeld {
				rec.State = StatePaused
				events = append(events, eventlog.Event{Event: eventlog.EventLeasePause, Actor: UserHolder, Detail: "paused"})
			}
			r.Result = ResultPaused
		default:
			r.Result = ResultQueued
			if rec.find(UserHolder) < 0 {
				t.doc.Seq++
				r.Asked = h.Handle
				events = queuePause(rec, now, t.doc.Seq)
			}
			r.Position = rec.find(UserHolder) + 1
		}
		return r, append(events, addWith(rec, a.With)...), nil
	})
}

// queuePause puts the user at the front of rec's queue and asks the
// holder to release: its bid ends now, and the ask is its check-in.
func queuePause(rec *record, now time.Time, seq uint64) []eventlog.Event {
	h := rec.Holder
	rec.Queue = append(rec.Queue, waiter{Handle: UserHolder, Since: now, Seq: seq})
	rec.sortQueue()
	if h.Deadline.IsZero() || h.Deadline.After(now) {
		h.Deadline = now
	}
	h.CheckedIn, h.SilentSince = now, time.Time{}
	return []eventlog.Event{
		{Event: eventlog.EventLeasePause, Actor: UserHolder, Detail: "queued, position 1; asked " + h.Handle + " to release"},
		{Event: eventlog.EventLeaseCheckIn, Actor: h.Handle, Detail: "the user asked for the checkout"},
	}
}

// addWith shares the user's pause with each handle not yet shared with.
func addWith(rec *record, with []string) []eventlog.Event {
	var events []eventlog.Event
	for _, w := range with {
		if w == "" || w == UserHolder || slices.Contains(rec.With, w) {
			continue
		}
		rec.With = append(rec.With, w)
		events = append(events, eventlog.Event{Event: eventlog.EventLeaseWith, Actor: UserHolder, Detail: "shared with " + w})
	}
	return events
}

// Unshare takes a.With off the list of panes that may edit under the
// user's pause.
func (t *Table) Unshare(a Args) (Reply, error) {
	if len(a.With) == 0 {
		return Reply{}, errors.New("name a pane or agent to unshare")
	}
	return t.update(a.Checkout, func(rec *record, _ time.Time) (Reply, []eventlog.Event, error) {
		var events []eventlog.Event
		for _, w := range a.With {
			i := slices.Index(rec.With, w)
			if i < 0 {
				return Reply{}, nil, fmt.Errorf("%s is not shared with %s", a.Checkout, w)
			}
			rec.With = slices.Delete(rec.With, i, i+1)
			events = append(events, eventlog.Event{Event: eventlog.EventLeaseUnshare, Actor: UserHolder, Detail: "unshared with " + w})
		}
		if len(rec.With) == 0 {
			rec.With = nil
		}
		return Reply{Result: ResultUnshared}, events, nil
	})
}

// BeginResume marks the user's hold releasing while the CLI runs the
// git steps. A user still queued withdraws the pause instead: Result is
// ResultLeft and the CLI runs no git steps.
func (t *Table) BeginResume(a Args) (Reply, error) {
	return t.update(a.Checkout, func(rec *record, _ time.Time) (Reply, []eventlog.Event, error) {
		if i := rec.find(UserHolder); i >= 0 {
			rec.Queue = slices.Delete(rec.Queue, i, i+1)
			if len(rec.Queue) == 0 {
				rec.Queue = nil
			}
			rec.With = nil
			ev := eventlog.Event{Event: eventlog.EventLeaseResume, Actor: UserHolder, Detail: "withdrew the queued pause"}
			return Reply{Result: ResultLeft}, []eventlog.Event{ev}, nil
		}
		if err := userHolds(rec, a.Checkout); err != nil {
			return Reply{}, nil, err
		}
		rec.State = StateReleasing
		return Reply{Result: ResultReleasing}, nil, nil
	})
}

// AbortResume puts a resume that could not finish back to paused.
func (t *Table) AbortResume(a Args) (Reply, error) {
	return t.update(a.Checkout, func(rec *record, _ time.Time) (Reply, []eventlog.Event, error) {
		if err := userHolds(rec, a.Checkout); err != nil {
			return Reply{}, nil, err
		}
		if rec.State == StateReleasing {
			rec.State = StatePaused
		}
		return Reply{Result: ResultPaused}, nil, nil
	})
}

// Resume ends the user's hold like Release: the list of panes shared
// with clears and the next live waiter is granted.
func (t *Table) Resume(a Args, gone func(handle string) bool) (Reply, error) {
	return t.update(a.Checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		if err := userHolds(rec, a.Checkout); err != nil {
			return Reply{}, nil, err
		}
		detail := "resumed"
		if a.AsIs {
			detail += " as-is"
		}
		if a.PullError != "" {
			detail += "; pull failed"
		}
		r := Reply{Result: ResultResumed, PullError: a.PullError}
		events := []eventlog.Event{{Event: eventlog.EventLeaseResume, Actor: UserHolder, Detail: detail, Reason: a.PullError}}
		rec.With = nil
		events = append(events, grantNext(rec, now, gone, &r)...)
		return r, events, nil
	})
}

// userHolds refuses unless the user holds rec as paused, held (after
// adopt), or releasing.
func userHolds(rec *record, checkout string) error {
	if rec.Holder == nil || rec.Holder.Handle != UserHolder {
		return fmt.Errorf("the user does not hold %s", checkout)
	}
	switch rec.State {
	case StatePaused, StateHeld, StateReleasing:
		return nil
	default:
		return fmt.Errorf("%s is %s; finish that first", checkout, rec.State)
	}
}

// sharedWith reports whether caller may edit under the user's pause. A
// queued pause granted by takeover is held, not paused, once the user
// adopts; its list applies too.
func (r *record) sharedWith(caller string) bool {
	return (r.State == StatePaused || r.State == StateHeld) && r.Holder != nil && r.Holder.Handle == UserHolder && slices.Contains(r.With, caller)
}

// notHolder is the error for caller doing what only the holder may.
func notHolder(rec *record, caller, verb, checkout string) error {
	if rec.Holder != nil && rec.Holder.Handle == UserHolder && slices.Contains(rec.With, caller) {
		return fmt.Errorf("the user holds %s and shares it with you; only the user can %s it", checkout, verb)
	}
	return fmt.Errorf("only the holder can %s %s", verb, checkout)
}
