package lease

import (
	"cmp"
	"fmt"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// Pane is what herdr reports about a holder's pane, reduced to what the
// soft deadline needs.
type Pane string

// Pane states. Busy is working or blocked (mid-turn or waiting on a
// prompt). Idle is idle, done or no-agent. Unknown means the machine
// could not be reached.
const (
	PaneBusy    Pane = "busy"
	PaneIdle    Pane = "idle"
	PaneGone    Pane = "gone"
	PaneUnknown Pane = "unknown"
)

// Default soft-deadline timing.
const (
	DefaultCheckInterval = 2 * time.Minute
	DefaultCheckinRepeat = 15 * time.Minute
	DefaultIdleGrace     = 5 * time.Minute
)

// Timing is the soft-deadline timing. Interval is how often an overrun
// holder is looked at and how far a busy one is extended. Repeat is the
// least time between check-ins to a busy holder. Grace is how long a
// checked-in holder may stay idle and silent before takeover.
type Timing struct {
	Interval time.Duration
	Repeat   time.Duration
	Grace    time.Duration
}

// WithDefaults fills each zero field with its default.
func (tm Timing) WithDefaults() Timing {
	if tm.Interval <= 0 {
		tm.Interval = DefaultCheckInterval
	}
	if tm.Repeat <= 0 {
		tm.Repeat = DefaultCheckinRepeat
	}
	if tm.Grace <= 0 {
		tm.Grace = DefaultIdleGrace
	}
	return tm
}

// Watch names a holder the daemon should look at.
type Watch struct {
	Checkout string
	Holder   string
}

// Decision is what Observe asks the daemon to do. CheckIn is a message
// for the holder. TakeOver is why the holder should be taken over.
type Decision struct {
	CheckIn  string
	TakeOver string
}

// CheckInText is the message a holder gets when its bid ends.
func CheckInText(checkout string) string {
	return "[lease] your bid on " + checkout + " ended. Run 'agentbus lease extend --for <d>' or 'agentbus lease release'."
}

// Watched lists the agent holders whose pane state Observe needs: those
// with a waiter queued, and those past their deadline who have not been
// checked in on yet. The user, and a holder already marked for
// takeover, are never watched.
func (t *Table) Watched() []Watch {
	t.mu.Lock()
	defer t.mu.Unlock()
	now := t.now()
	var out []Watch
	for k, rec := range t.doc.Leases {
		h := rec.Holder
		if rec.State != StateHeld || h == nil || h.Handle == UserHolder || !h.TakeoverAt.IsZero() {
			continue
		}
		if len(rec.Queue) > 0 || (!now.Before(h.Deadline) && h.CheckedIn.IsZero()) {
			out = append(out, Watch{Checkout: k, Holder: h.Handle})
		}
	}
	slices.SortFunc(out, func(a, b Watch) int { return cmp.Compare(a.Checkout, b.Checkout) })
	return out
}

// Observe applies the soft-deadline rules to w's holder, given its pane
// state p read outside the table lock. It does nothing when w is stale
// (the holder changed or the lease is not held). Timers are persisted,
// so a restart resumes them.
func (t *Table) Observe(w Watch, p Pane, tm Timing) (Decision, error) {
	tm = tm.WithDefaults()
	return t.decide(w.Checkout, func(rec *record, now time.Time) (Decision, []eventlog.Event) {
		h := rec.Holder
		if rec.State != StateHeld || h == nil || h.Handle != w.Holder || h.Handle == UserHolder || !h.TakeoverAt.IsZero() {
			return Decision{}, nil
		}
		waiting := len(rec.Queue) > 0
		if p == PaneGone && waiting {
			h.TakeoverAt = now
			return Decision{TakeOver: "pane gone"}, nil
		}
		if now.Before(h.Deadline) {
			return Decision{}, nil
		}
		o := &observation{h: h, checkout: w.Checkout, now: now, tm: tm}
		if h.CheckedIn.IsZero() {
			o.checkIn("bid ended at " + h.Deadline.Format(time.RFC3339))
		}
		// With nobody waiting: one check-in, then leave the holder be.
		if waiting {
			o.pane(p)
		}
		return o.d, o.events
	})
}

// observation is one Observe of an overrun holder.
type observation struct {
	h        *holder
	checkout string
	now      time.Time
	tm       Timing
	d        Decision
	events   []eventlog.Event
}

func (o *observation) checkIn(detail string) {
	o.h.CheckedIn = o.now
	o.d.CheckIn = CheckInText(o.checkout)
	o.events = append(o.events, eventlog.Event{Event: eventlog.EventLeaseCheckIn, Actor: o.h.Handle, Detail: detail})
}

// pane applies the pane state of an overrun holder with a waiter queued.
func (o *observation) pane(p Pane) {
	h, now := o.h, o.now
	switch p {
	case PaneBusy:
		h.SilentSince = time.Time{}
		h.Deadline = now.Add(o.tm.Interval)
		o.events = append(o.events, eventlog.Event{Event: eventlog.EventLeaseAutoExtend, Actor: h.Handle, Detail: "until " + h.Deadline.Format(time.RFC3339), Reason: "pane busy"})
		if o.d.CheckIn == "" && now.Sub(h.CheckedIn) >= o.tm.Repeat {
			o.checkIn("still busy " + Short(now.Sub(h.CheckedIn).Round(time.Second)) + " after the last check-in")
		}
	case PaneIdle, PaneGone:
		if h.SilentSince.IsZero() {
			h.SilentSince = now
		}
		if silent := now.Sub(h.SilentSince); silent >= o.tm.Grace {
			h.TakeoverAt = now
			o.d.TakeOver = fmt.Sprintf("idle and silent for %s after the check-in", Short(silent.Round(time.Second)))
		}
	default:
		// The machine is unreachable: never take over, and the silence
		// has to be seen again from the start.
		h.SilentSince = time.Time{}
	}
}

// decide runs change on a known checkout through update.
func (t *Table) decide(checkout string, change func(*record, time.Time) (Decision, []eventlog.Event)) (Decision, error) {
	var d Decision
	_, err := t.update(checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		var events []eventlog.Event
		d, events = change(rec, now)
		return Reply{}, events, nil
	})
	return d, err
}
