package lease

import (
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// BackupRefPrefix is where adopt saves a taken-over checkout's
// leftovers. The tool never deletes these refs.
const BackupRefPrefix = "refs/lease-backup/"

// Takeover is what TakeOver did.
type Takeover struct {
	// Done is false when the watch was stale and nothing changed.
	Done bool
	// From is the holder taken over; FromHeld is true when it held the
	// checkout (held or releasing), false when it was an adopter that
	// never adopted.
	From     string
	FromHeld bool
	// To is the new adopter, or "" when nobody live was waiting.
	To string
	// BackupRef is where adopt will save the leftovers.
	BackupRef string
	// Skipped are waiters dropped because their pane was gone.
	Skipped []string
	// Lease is the checkout after the takeover.
	Lease Status
}

// BackupRef names the ref adopt saves leftovers to: the UTC time and the
// old holder's name or handle, made safe for a ref name.
func BackupRef(at time.Time, who string) string {
	var b strings.Builder
	for _, r := range strings.TrimSpace(who) {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9', r == '_', r == '+', r == '=':
			b.WriteRune(r)
		default:
			b.WriteByte('-')
		}
	}
	name := strings.Trim(b.String(), "-")
	for strings.Contains(name, "--") {
		name = strings.ReplaceAll(name, "--", "-")
	}
	if name == "" {
		name = "unknown"
	}
	return BackupRefPrefix + at.UTC().Format("20060102T150405Z") + "-" + name
}

// TakenOverText is the message the old holder gets on takeover.
func TakenOverText(checkout, ref string) string {
	return "[lease] taken over from you: " + checkout + "; your uncommitted work will be saved to " + ref
}

// AdoptWakeText is the message the new adopter gets on takeover.
func AdoptWakeText(checkout, from, ref string) string {
	return "[lease] granted " + checkout + " as needs-adopt, taken over from " + from +
		". Run 'agentbus lease adopt' first: it saves any leftovers to " + ref +
		" and resets the checkout. Release with: agentbus lease release"
}

// NeedsAdoptMark is in every NeedsAdoptNote.
const NeedsAdoptMark = " and needs adopt "

// NeedsAdoptNote is the note the user gets when a checkout needs adopt.
func NeedsAdoptNote(tk Takeover) string {
	who := "nobody is waiting; the next claim gets it, or run 'agentbus lease adopt' from a terminal"
	switch tk.To {
	case "":
	case UserHolder:
		who = "for your pause; run 'agentbus lease adopt' from a terminal"
	default:
		who = "assigned to " + tk.To
	}
	return "agentbus lease: " + tk.Lease.Checkout + " was taken over from " + tk.From +
		NeedsAdoptMark + "(" + who + "). Leftovers will be saved to " + tk.BackupRef + "."
}

// TakeOver hands w's checkout from its holder to the first waiter whose
// pane is not gone, in state needs-adopt, dropping the gone waiters.
// The holder may be held, releasing, or an adopter in needs-adopt. With
// nobody left it stays needs-adopt with no holder, and the next claim
// gets it. gone runs under the table lock, so it must answer from what
// the caller already knows. A stale watch changes nothing.
func (t *Table) TakeOver(w Watch, reason string, gone func(handle string) bool) (Takeover, error) {
	var tk Takeover
	r, err := t.update(w.Checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		h := rec.Holder
		if h == nil || h.Handle != w.Holder || h.Handle == UserHolder || !takeable(rec.State) {
			return Reply{}, nil, nil
		}
		tk = Takeover{Done: true, From: h.Handle, FromHeld: rec.State != StateNeedsAdopt}
		if tk.FromHeld || rec.BackupRef == "" {
			who := t.names(h.Handle)
			if who == "" {
				who = h.Handle
			}
			rec.BackupRef = BackupRef(now, who)
		}
		tk.BackupRef = rec.BackupRef
		rec.State, rec.Holder = StateNeedsAdopt, nil
		events := []eventlog.Event{{Event: eventlog.EventLeaseTakeover, Actor: tk.From, Detail: "taken over; backup " + rec.BackupRef, Reason: reason}}
		for len(rec.Queue) > 0 {
			next := rec.Queue[0]
			rec.Queue = rec.Queue[1:]
			if next.Handle != UserHolder && gone(next.Handle) {
				tk.Skipped = append(tk.Skipped, next.Handle)
				events = append(events, eventlog.Event{Event: eventlog.EventLeaseSkip, Actor: next.Handle, Detail: "skipped and dropped from the queue", Reason: "pane gone"})
				continue
			}
			rec.Holder = next.holder(now)
			tk.To = next.Handle
			events = append(events, eventlog.Event{Event: eventlog.EventLeaseGrant, Actor: next.Handle, Detail: grantDetail(rec)})
			break
		}
		if len(rec.Queue) == 0 {
			rec.Queue = nil
		}
		if tk.To == "" {
			events[0].Detail += "; nobody waiting, unassigned"
		}
		return Reply{}, events, nil
	})
	if err != nil {
		return Takeover{}, err
	}
	if tk.Done {
		tk.Lease = *r.Lease
	}
	return tk, nil
}

// BeginAdopt starts caller's adopt of a needs-adopt checkout and
// returns the backup ref to save the leftovers to. Only the holder, the
// assigned adopter, may adopt, except the user, who may always adopt:
// the user then holds the checkout and an agent adopter goes back to
// the front of the queue.
func (t *Table) BeginAdopt(caller string, a Args) (Reply, error) {
	if err := checkCaller(caller); err != nil {
		return Reply{}, err
	}
	return t.update(a.Checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		if rec.State != StateNeedsAdopt {
			return Reply{}, nil, fmt.Errorf("%s does not need adopt; it is %s", a.Checkout, rec.State)
		}
		var events []eventlog.Event
		switch {
		case rec.Holder != nil && rec.Holder.Handle == caller:
		case caller == UserHolder:
			if h := rec.Holder; h != nil {
				// Seq 0 puts the adopter ahead of every normal waiter.
				rec.Queue = append(rec.Queue, waiter{Handle: h.Handle, For: h.For, Why: h.Why, Since: h.Since})
				rec.sortQueue()
			}
			rec.Queue = deleteWaiter(rec.Queue, UserHolder)
			rec.Holder = &holder{Handle: UserHolder, Since: now}
			events = append(events, eventlog.Event{Event: eventlog.EventLeaseGrant, Actor: UserHolder, Detail: "needs-adopt; the user adopts"})
		case rec.Holder == nil:
			return Reply{}, nil, fmt.Errorf("nobody holds %s; claim it first with: agentbus lease claim --for <duration>", a.Checkout)
		default:
			return Reply{}, nil, fmt.Errorf("%s needs adopt by %s; claim it first with: agentbus lease claim --for <duration>", a.Checkout, rec.Holder.Handle)
		}
		if rec.BackupRef == "" {
			rec.BackupRef = BackupRef(now, "")
		}
		return Reply{Result: ResultAdopting}, events, nil
	})
}

// Adopt finishes caller's adopt: the CLI saved the leftovers to Backups
// and reset the checkout. The lease becomes held by caller.
func (t *Table) Adopt(caller string, a Args) (Reply, error) {
	if err := checkCaller(caller); err != nil {
		return Reply{}, err
	}
	return t.update(a.Checkout, func(rec *record, _ time.Time) (Reply, []eventlog.Event, error) {
		if rec.State != StateNeedsAdopt || rec.Holder == nil || rec.Holder.Handle != caller {
			return Reply{}, nil, fmt.Errorf("only the adopter of a needs-adopt checkout can adopt %s", a.Checkout)
		}
		rec.State, rec.BackupRef = StateHeld, ""
		rec.Holder.answered()
		detail := "adopted; nothing to save"
		if len(a.Backups) > 0 {
			detail = "adopted; saved to " + strings.Join(a.Backups, ", ")
		}
		ev := eventlog.Event{Event: eventlog.EventLeaseAdopt, Actor: caller, Detail: detail}
		if len(a.Forced) > 0 {
			ev.Reason = "forced past: " + strings.Join(a.Forced, ", ")
		}
		return Reply{Result: ResultAdopted}, []eventlog.Event{ev}, nil
	})
}

// takeable is a state whose holder can be taken over. A crashed release
// stays releasing, and must not block the queue.
func takeable(s State) bool {
	return s == StateHeld || s == StateReleasing || s == StateNeedsAdopt
}

func deleteWaiter(q []waiter, handle string) []waiter {
	out := q[:0]
	for _, w := range q {
		if w.Handle != handle {
			out = append(out, w)
		}
	}
	return out
}
