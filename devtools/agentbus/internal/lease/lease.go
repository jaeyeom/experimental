// Package lease keeps the agentbus workdir leases: who may change a
// checkout right now, until when, and who is waiting. The daemon owns
// one Table. Git and process inspection happen in the CLI on the
// checkout's machine; the Table only keeps state, persists it, and logs
// every change.
package lease

import (
	"cmp"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// State is a checkout's lease state.
type State string

// Lease states, part of the stable status shape. Releasing lasts while
// the holder's release runs its git steps. NeedsAdopt follows a
// takeover, or a claim of a dirty free checkout, until adopt resets the
// checkout. Paused is the user's hold, set by pause.
const (
	StateFree       State = "free"
	StateHeld       State = "held"
	StatePaused     State = "paused"
	StateReleasing  State = "releasing"
	StateNeedsAdopt State = "needs-adopt"
)

// UserHolder is the special holder that stands for the user. It queues
// ahead of every agent.
const UserHolder = "user"

// Results a Reply carries.
const (
	ResultGranted  = "granted"
	ResultQueued   = "queued"
	ResultExtended = "extended"
	ResultLeft     = "left"
	ResultStatus   = "status"
	ResultAllowed  = "allowed"
	ResultDenied   = "denied"
	// ResultReleasing: the holder's git steps are running.
	ResultReleasing = "releasing"
	ResultReleased  = "released"
	ResultHeld      = "held"
	// ResultAdopting: the adopter's process check and cleanup run.
	ResultAdopting = "adopting"
	ResultAdopted  = "adopted"
	// ResultPaused: the user holds the checkout.
	ResultPaused   = "paused"
	ResultResumed  = "resumed"
	ResultUnshared = "unshared"
)

// stateFile is the persisted table in the daemon state directory.
const stateFile = "leases.json"

// fileVersion is the persisted format version.
const fileVersion = 1

// Args is the lease part of a daemon request. The CLI fills it on the
// checkout's machine.
type Args struct {
	// Checkout is the checkout key, <machine>:<abs git top-level>.
	Checkout string `json:"checkout,omitempty"`
	// For is the time bid, a Go duration such as 40m.
	For    string `json:"for,omitempty"`
	Urgent bool   `json:"urgent,omitempty"`
	Why    string `json:"why,omitempty"`
	// All asks status for every checkout the daemon has seen.
	All bool `json:"all,omitempty"`
	// Tree is what the CLI found in the checkout's working tree: empty
	// when clean, else why it needs adopt (dirty, rebase in progress).
	Tree string `json:"tree,omitempty"`
	// PullError is why release's git pull failed, when it did. The
	// release goes ahead; the error is logged with it.
	PullError string `json:"pullError,omitempty"`
	// Backups are the refs adopt saved the leftovers to.
	Backups []string `json:"backups,omitempty"`
	// Forced names the processes adopt --force went past.
	Forced []string `json:"forced,omitempty"`
	// With are the handles pause shares the user's hold with, or unshare
	// takes off.
	With []string `json:"with,omitempty"`
	// AsIs is resume --as-is: the git steps were skipped.
	AsIs bool `json:"asIs,omitempty"`
	// Hook marks a check from the PreToolUse lease hook. The daemon logs
	// its denials, with Tool and Target, the hook's tool call.
	Hook bool `json:"hook,omitempty"`
	// Tool is the hook's tool name, such as Edit or Bash.
	Tool string `json:"tool,omitempty"`
	// Target is what the tool call would change: a file path relative to
	// the checkout root, or a git subcommand such as "git switch".
	Target string `json:"target,omitempty"`
}

// Reply is the lease part of a daemon response.
type Reply struct {
	// Caller is the handle the daemon stamped on the request.
	Caller string `json:"caller,omitempty"`
	Result string `json:"result,omitempty"`
	// Position is the caller's 1-based place in the queue when queued.
	Position int `json:"position,omitempty"`
	// Lease is the checkout the request named, after the request.
	Lease *Status `json:"lease,omitempty"`
	// Leases is the status op's answer.
	Leases []Status `json:"leases,omitempty"`
	// Granted is the waiter a release handed the checkout to.
	Granted string `json:"granted,omitempty"`
	// Skipped are the waiters a release dropped because their pane was
	// gone.
	Skipped []string `json:"skipped,omitempty"`
	// PullError echoes a release's failed pull.
	PullError string `json:"pullError,omitempty"`
	// Takeover is the daemon's effective lease_takeover on a status
	// reply: "log" or "on". An older daemon leaves it empty.
	Takeover TakeoverMode `json:"takeover,omitempty"`
	// Asked is the holder a pause asked to finish up and release.
	Asked string `json:"asked,omitempty"`
	// Waiting is the notice a claim that newly queued sends the holder.
	// The daemon sends it; it is not part of the reply on the wire.
	Waiting *Notice `json:"-"`
}

// Notice is an agentbus message for To.
type Notice struct {
	To   string
	Text string
}

// Status is the stable `lease status --json` shape other tools read.
type Status struct {
	Checkout  string       `json:"checkout"`
	State     State        `json:"state"`
	Holder    *HolderView  `json:"holder,omitempty"`
	Queue     []WaiterView `json:"queue"`
	BackupRef string       `json:"backup_ref,omitempty"` //nolint:tagliatelle // stable status shape
	// With are the agent panes that may edit under the user's pause.
	With []string `json:"with,omitempty"`
}

// HolderView is the holder in Status. Times are RFC3339.
type HolderView struct {
	Handle   string `json:"handle"`
	Name     string `json:"name"`
	Since    string `json:"since"`
	Deadline string `json:"deadline"`
	Why      string `json:"why"`
}

// WaiterView is one queue entry in Status.
type WaiterView struct {
	Handle string `json:"handle"`
	Name   string `json:"name"`
	Urgent bool   `json:"urgent"`
	For    string `json:"for"`
	Since  string `json:"since"`
}

// Table is every lease the daemon knows, persisted in its state
// directory. All methods are safe for concurrent use.
type Table struct {
	mu    sync.Mutex
	dir   string
	doc   file
	log   EventLog
	now   func() time.Time
	names func(handle string) string
	// skips are the skipped waiting notices logged per checkout, kept in
	// memory only.
	skips map[string]*skipSet
}

// EventLog is where a Table logs its events.
type EventLog interface {
	Append(e eventlog.Event) error
}

type file struct {
	Version int `json:"version"`
	// Seq is the last arrival number given to a claim. It orders the
	// queue and keeps growing across restarts.
	Seq    uint64             `json:"seq"`
	Leases map[string]*record `json:"leases"`
}

// record is one checkout. A free checkout keeps its record, so status
// --all lists every checkout ever named.
type record struct {
	State     State    `json:"state"`
	Holder    *holder  `json:"holder,omitempty"`
	Queue     []waiter `json:"queue,omitempty"`
	BackupRef string   `json:"backup_ref,omitempty"` //nolint:tagliatelle // persisted format
	// With are the handles the user's pause is shared with. It is kept
	// while the user waits in the queue and cleared on resume.
	With []string `json:"with,omitempty"`
}

type holder struct {
	Handle string    `json:"handle"`
	Since  time.Time `json:"since"`
	// Deadline is the soft deadline. Zero means none (the user).
	Deadline time.Time     `json:"deadline,omitzero"`
	For      time.Duration `json:"for,omitempty"`
	Why      string        `json:"why,omitempty"`
	// CheckedIn is when the last check-in was sent since the holder last
	// claimed or extended. Zero means none yet.
	CheckedIn time.Time `json:"checkedIn,omitzero"`
	// SilentSince is when the holder was first seen idle, with no extend
	// or release, after a check-in. Zero means the silence timer is off.
	SilentSince time.Time `json:"silentSince,omitzero"`
	// TakeoverAt is when the daemon decided to take over. Zero means not
	// yet; it is decided once.
	TakeoverAt time.Time `json:"takeoverAt,omitzero"`
	// WouldTakeOverAt is when a takeover was decided but only logged,
	// because lease_takeover is "log". Zero means not logged yet; it is
	// logged once per holder until the holder answers.
	WouldTakeOverAt time.Time `json:"wouldTakeOverAt,omitzero"`
}

// answered resets the check-in timers: the holder answered with a claim
// or an extend.
func (h *holder) answered() {
	h.CheckedIn, h.SilentSince, h.TakeoverAt, h.WouldTakeOverAt = time.Time{}, time.Time{}, time.Time{}, time.Time{}
}

type waiter struct {
	Handle string        `json:"handle"`
	Urgent bool          `json:"urgent,omitempty"`
	For    time.Duration `json:"for"`
	Why    string        `json:"why,omitempty"`
	Since  time.Time     `json:"since"`
	Seq    uint64        `json:"seq"`
}

// Open loads the table from dir. A missing file is an empty table. now
// is the clock; names returns the registered name of a handle, or "".
func Open(dir string, log EventLog, now func() time.Time, names func(handle string) string) (*Table, error) {
	if now == nil {
		now = time.Now
	}
	if names == nil {
		names = func(string) string { return "" }
	}
	doc, err := load(dir)
	if err != nil {
		return nil, err
	}
	return &Table{dir: dir, doc: doc, log: log, now: now, names: names, skips: map[string]*skipSet{}}, nil
}

// Claim grants a free checkout to caller or queues caller. It never
// blocks. Claiming again as holder or waiter updates For and Why (and
// Urgent for a waiter) without losing the place.
func (t *Table) Claim(caller string, a Args) (Reply, error) {
	if err := checkCaller(caller); err != nil {
		return Reply{}, err
	}
	d, err := bid(a.For)
	if err != nil {
		return Reply{}, err
	}
	return t.update(a.Checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		claim := eventlog.Event{Event: eventlog.EventLeaseClaim, Actor: caller, Detail: claimDetail(d, a)}
		switch {
		case rec.Holder == nil:
			// A checkout taken over with nobody waiting stays needs-adopt
			// for the next claim.
			if rec.State != StateNeedsAdopt {
				rec.State = StateHeld
			}
			if a.Tree != "" {
				rec.State = StateNeedsAdopt
			}
			rec.Holder = &holder{Handle: caller, Since: now, Deadline: now.Add(d), For: d, Why: a.Why}
			grant := eventlog.Event{Event: eventlog.EventLeaseGrant, Actor: caller, Detail: grantDetail(rec)}
			if a.Tree != "" {
				grant.Reason = a.Tree
			}
			return Reply{Result: ResultGranted}, []eventlog.Event{claim, grant}, nil
		case rec.Holder.Handle == caller:
			rec.Holder.Deadline = now.Add(d)
			rec.Holder.For = d
			rec.Holder.Why = a.Why
			rec.Holder.answered()
			return Reply{Result: ResultGranted}, []eventlog.Event{claim}, nil
		}
		r := Reply{Result: ResultQueued}
		events := []eventlog.Event{claim}
		var skip string
		if i := rec.find(caller); i >= 0 {
			rec.Queue[i].For = d
			rec.Queue[i].Why = a.Why
			rec.Queue[i].Urgent = a.Urgent
			skip = skipRepeat
		} else {
			t.doc.Seq++
			rec.Queue = append(rec.Queue, waiter{Handle: caller, Urgent: a.Urgent, For: d, Why: a.Why, Since: now, Seq: t.doc.Seq})
			if r.Waiting, skip = t.waitingNotice(rec, a.Checkout, caller, d, a.Why, now); r.Waiting != nil {
				events = append(events, eventlog.Event{Event: eventlog.EventLeaseNotifyWaiting, Actor: rec.Holder.Handle, Detail: fmt.Sprintf("waiter %s; %d waiting", caller, len(rec.Queue))})
			}
		}
		if caller != UserHolder && skip != "" && t.firstSkip(a.Checkout, rec.Holder, caller, skip) {
			events = append(events, eventlog.Event{Event: eventlog.EventLeaseNotifySkipped, Actor: caller, Detail: fmt.Sprintf("holder %s; reason %s; queue %d", rec.Holder.Handle, skip, len(rec.Queue))})
		}
		rec.sortQueue()
		r.Position = rec.find(caller) + 1
		events[0].Detail += fmt.Sprintf("; queued, position %d", r.Position)
		return r, events, nil
	})
}

// Extend moves the holder's soft deadline to now plus For.
func (t *Table) Extend(caller string, a Args) (Reply, error) {
	if err := checkCaller(caller); err != nil {
		return Reply{}, err
	}
	d, err := bid(a.For)
	if err != nil {
		return Reply{}, err
	}
	return t.update(a.Checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		if rec.Holder == nil || rec.Holder.Handle != caller {
			return Reply{}, nil, notHolder(rec, caller, "extend the lease on", a.Checkout)
		}
		rec.Holder.Deadline = now.Add(d)
		rec.Holder.For = d
		rec.Holder.answered()
		ev := eventlog.Event{Event: eventlog.EventLeaseExtend, Actor: caller, Detail: "until " + rec.Holder.Deadline.Format(time.RFC3339)}
		return Reply{Result: ResultExtended}, []eventlog.Event{ev}, nil
	})
}

// Leave takes caller out of the queue. The holder releases instead.
func (t *Table) Leave(caller string, a Args) (Reply, error) {
	if err := checkCaller(caller); err != nil {
		return Reply{}, err
	}
	return t.update(a.Checkout, func(rec *record, _ time.Time) (Reply, []eventlog.Event, error) {
		if rec.Holder != nil && rec.Holder.Handle == caller {
			return Reply{}, nil, fmt.Errorf("you hold %s; release it instead of leaving the queue", a.Checkout)
		}
		i := rec.find(caller)
		if i < 0 {
			return Reply{}, nil, fmt.Errorf("you are not in the queue for %s", a.Checkout)
		}
		rec.Queue = slices.Delete(rec.Queue, i, i+1)
		ev := eventlog.Event{Event: eventlog.EventLeaseLeave, Actor: caller, Detail: fmt.Sprintf("left the queue at position %d", i+1)}
		return Reply{Result: ResultLeft}, []eventlog.Event{ev}, nil
	})
}

// Check answers whether caller may change the checkout now: allowed
// only for the holder of a held lease and for the panes the user's
// pause is shared with.
func (t *Table) Check(caller string, a Args) (Reply, error) {
	return t.update(a.Checkout, func(rec *record, _ time.Time) (Reply, []eventlog.Event, error) {
		if caller != "" && rec.State == StateHeld && rec.Holder != nil && rec.Holder.Handle == caller {
			return Reply{Result: ResultAllowed}, nil, nil
		}
		if caller != "" && rec.sharedWith(caller) {
			return Reply{Result: ResultAllowed}, nil, nil
		}
		return Reply{Result: ResultDenied}, nil, nil
	})
}

// Status reports one checkout, or with All every checkout seen, sorted
// by key.
func (t *Table) Status(a Args) (Reply, error) {
	if !a.All {
		r, err := t.update(a.Checkout, func(*record, time.Time) (Reply, []eventlog.Event, error) {
			return Reply{Result: ResultStatus}, nil, nil
		})
		if err != nil {
			return Reply{}, err
		}
		r.Leases = []Status{*r.Lease}
		r.Lease = nil
		return r, nil
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	keys := make([]string, 0, len(t.doc.Leases))
	for k := range t.doc.Leases {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	out := make([]Status, 0, len(keys))
	for _, k := range keys {
		out = append(out, t.view(k, t.doc.Leases[k]))
	}
	return Reply{Result: ResultStatus, Leases: out}, nil
}

// BeginRelease marks the holder's lease releasing while the CLI runs
// the git steps. Calling it again while releasing is allowed, so a
// release cut short can be run again.
func (t *Table) BeginRelease(caller string, a Args) (Reply, error) {
	return t.holderUpdate(caller, a, func(rec *record) Reply {
		rec.State = StateReleasing
		return Reply{Result: ResultReleasing}
	})
}

// AbortRelease puts a releasing lease back to held when the git steps
// could not leave the checkout on the default branch.
func (t *Table) AbortRelease(caller string, a Args) (Reply, error) {
	return t.holderUpdate(caller, a, func(rec *record) Reply {
		if rec.State == StateReleasing {
			rec.State = StateHeld
		}
		return Reply{Result: ResultHeld}
	})
}

// Release ends caller's hold and grants the checkout to the first
// waiter whose pane is not gone, dropping the gone ones. With nobody
// left the checkout is free. gone runs under the table lock, so it must
// answer from what the caller already knows.
func (t *Table) Release(caller string, a Args, gone func(handle string) bool) (Reply, error) {
	if err := checkCaller(caller); err != nil {
		return Reply{}, err
	}
	return t.update(a.Checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		if rec.Holder == nil || rec.Holder.Handle != caller {
			return Reply{}, nil, notHolder(rec, caller, "release", a.Checkout)
		}
		r := Reply{Result: ResultReleased, PullError: a.PullError}
		events := []eventlog.Event{{Event: eventlog.EventLeaseRelease, Actor: caller, Detail: "released", Reason: a.PullError}}
		if a.PullError != "" {
			events[0].Detail = "released; pull failed"
		}
		events = append(events, grantNext(rec, now, gone, &r)...)
		return r, events, nil
	})
}

// grantNext frees rec and grants it to the first waiter whose pane is
// not gone, dropping the gone ones into r.Skipped. The user, who has no
// pane, is granted a pause. With nobody left the checkout is free.
func grantNext(rec *record, now time.Time, gone func(handle string) bool, r *Reply) []eventlog.Event {
	var events []eventlog.Event
	rec.State, rec.Holder = StateFree, nil
	for len(rec.Queue) > 0 {
		w := rec.Queue[0]
		rec.Queue = rec.Queue[1:]
		if w.Handle != UserHolder && gone(w.Handle) {
			r.Skipped = append(r.Skipped, w.Handle)
			events = append(events, eventlog.Event{Event: eventlog.EventLeaseSkip, Actor: w.Handle, Detail: "skipped and dropped from the queue", Reason: "pane gone"})
			continue
		}
		rec.State = StateHeld
		if w.Handle == UserHolder {
			rec.State = StatePaused
		}
		rec.Holder = w.holder(now)
		r.Granted = w.Handle
		events = append(events, eventlog.Event{Event: eventlog.EventLeaseGrant, Actor: w.Handle, Detail: grantDetail(rec)})
		break
	}
	if len(rec.Queue) == 0 {
		rec.Queue = nil
	}
	return events
}

// Reasons a lease_notify_skipped event gives.
const (
	skipUserHolder   = "user-holder"
	skipPastDeadline = "past-deadline"
	skipRepeat       = "repeat"
)

// waitingNotice is the message that tells rec's holder caller newly
// queued behind it, or nil and why it was skipped. Only an agent holding
// a held lease with time left is told: the user is not, and a holder
// past its deadline gets the check-in instead. The user's own queued
// claim asks the holder already.
func (t *Table) waitingNotice(rec *record, checkout, caller string, d time.Duration, why string, now time.Time) (*Notice, string) {
	h := rec.Holder
	switch {
	case caller == UserHolder:
		return nil, ""
	case h.Handle == UserHolder:
		return nil, skipUserHolder
	case rec.State != StateHeld:
		return nil, ""
	case !now.Before(h.Deadline):
		return nil, skipPastDeadline
	}
	who := cmp.Or(t.names(caller), caller)
	paren := "for " + Short(d)
	if why != "" {
		paren = why + ", " + paren
	}
	text := fmt.Sprintf("[lease] %d waiting for %s: %s (%s). Release when you can: agentbus lease release", len(rec.Queue), checkout, who, paren)
	return &Notice{To: h.Handle, Text: text}, ""
}

// skipSet is the (waiter, reason) pairs logged as skipped notices while
// one holder keeps a checkout.
type skipSet struct {
	holder string
	since  time.Time
	seen   map[[2]string]bool
}

// firstSkip reports whether the skipped notice for waiter and reason is
// the first one while h holds checkout, and remembers it. A new holder
// starts a fresh set. The caller holds t.mu.
func (t *Table) firstSkip(checkout string, h *holder, waiter, reason string) bool {
	s := t.skips[checkout]
	if s == nil || s.holder != h.Handle || !s.since.Equal(h.Since) {
		s = &skipSet{holder: h.Handle, since: h.Since, seen: map[[2]string]bool{}}
		t.skips[checkout] = s
	}
	k := [2]string{waiter, reason}
	if s.seen[k] {
		return false
	}
	s.seen[k] = true
	return true
}

// holder is w granted at now. The user gets no deadline.
func (w waiter) holder(now time.Time) *holder {
	if w.Handle == UserHolder {
		return &holder{Handle: UserHolder, Since: now}
	}
	return &holder{Handle: w.Handle, Since: now, Deadline: now.Add(w.For), For: w.For, Why: w.Why}
}

// holderUpdate runs change when caller holds the checkout.
func (t *Table) holderUpdate(caller string, a Args, change func(*record) Reply) (Reply, error) {
	if err := checkCaller(caller); err != nil {
		return Reply{}, err
	}
	return t.update(a.Checkout, func(rec *record, _ time.Time) (Reply, []eventlog.Event, error) {
		if rec.Holder == nil || rec.Holder.Handle != caller {
			return Reply{}, nil, notHolder(rec, caller, "release", a.Checkout)
		}
		return change(rec), nil, nil
	})
}

// WakeText is the agentbus message a new holder gets on grant.
func WakeText(checkout string, h HolderView) string {
	out := "[lease] granted " + checkout
	if ts, err := time.Parse(time.RFC3339, h.Deadline); err == nil {
		out += " until " + ts.Local().Format("15:04")
	}
	if h.Why != "" {
		out += " (for: " + h.Why + ")"
	}
	return out + ". Release with: agentbus lease release"
}

// update runs change on the checkout's record under the lock, records a
// checkout seen for the first time, persists any change, and logs the
// events change returns. A failed save or change leaves the table as it
// was.
func (t *Table) update(checkout string, change func(*record, time.Time) (Reply, []eventlog.Event, error)) (Reply, error) {
	if err := checkKey(checkout); err != nil {
		return Reply{}, err
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	old, seen := t.doc.Leases[checkout]
	rec := &record{State: StateFree}
	if seen {
		rec = old.clone()
	}
	seq := t.doc.Seq
	r, events, err := change(rec, t.now().UTC())
	if err != nil {
		t.doc.Seq = seq
		return Reply{}, err
	}
	if !seen || !rec.equal(old) {
		t.doc.Leases[checkout] = rec
		if err := t.save(); err != nil {
			if seen {
				t.doc.Leases[checkout] = old
			} else {
				delete(t.doc.Leases, checkout)
			}
			t.doc.Seq = seq
			return Reply{}, err
		}
	}
	for _, ev := range events {
		ev.Name = checkout
		// A skipped notice that cannot be logged does not change the answer.
		if err := t.log.Append(ev); err != nil && ev.Event != eventlog.EventLeaseNotifySkipped {
			return Reply{}, fmt.Errorf("log %s: %w", ev.Event, err)
		}
	}
	view := t.view(checkout, rec)
	r.Lease = &view
	return r, nil
}

func (t *Table) view(checkout string, rec *record) Status {
	s := Status{Checkout: checkout, State: rec.State, Queue: []WaiterView{}, BackupRef: rec.BackupRef, With: slices.Clone(rec.With)}
	if h := rec.Holder; h != nil {
		s.Holder = &HolderView{Handle: h.Handle, Name: t.names(h.Handle), Since: stamp(h.Since), Deadline: stamp(h.Deadline), Why: h.Why}
	}
	for _, w := range rec.Queue {
		s.Queue = append(s.Queue, WaiterView{Handle: w.Handle, Name: t.names(w.Handle), Urgent: w.Urgent, For: Short(w.For), Since: stamp(w.Since)})
	}
	return s
}

// rank orders the queue: the user, then urgent claims, then the rest.
func (w waiter) rank() int {
	switch {
	case w.Handle == UserHolder:
		return 0
	case w.Urgent:
		return 1
	default:
		return 2
	}
}

func (r *record) sortQueue() {
	slices.SortStableFunc(r.Queue, func(a, b waiter) int {
		return cmp.Or(cmp.Compare(a.rank(), b.rank()), cmp.Compare(a.Seq, b.Seq))
	})
}

func (r *record) find(handle string) int {
	return slices.IndexFunc(r.Queue, func(w waiter) bool { return w.Handle == handle })
}

func (r *record) clone() *record {
	c := *r
	if r.Holder != nil {
		h := *r.Holder
		c.Holder = &h
	}
	c.Queue = slices.Clone(r.Queue)
	c.With = slices.Clone(r.With)
	return &c
}

func (r *record) equal(o *record) bool {
	a, _ := json.Marshal(r)
	b, _ := json.Marshal(o)
	return string(a) == string(b)
}

// Short renders d without trailing zero units: 40m, 1h, 1h30m.
func Short(d time.Duration) string {
	s := d.String()
	if strings.HasSuffix(s, "m0s") {
		s = strings.TrimSuffix(s, "0s")
	}
	if strings.HasSuffix(s, "h0m") {
		s = strings.TrimSuffix(s, "0m")
	}
	return s
}

func stamp(ts time.Time) string {
	if ts.IsZero() {
		return ""
	}
	return ts.UTC().Format(time.RFC3339)
}

func claimDetail(d time.Duration, a Args) string {
	parts := []string{"for " + Short(d)}
	if a.Urgent {
		parts = append(parts, "urgent")
	}
	if a.Why != "" {
		parts = append(parts, "why: "+a.Why)
	}
	return strings.Join(parts, "; ")
}

func grantDetail(rec *record) string {
	detail := string(rec.State)
	if !rec.Holder.Deadline.IsZero() {
		detail += " until " + rec.Holder.Deadline.Format(time.RFC3339)
	}
	return detail
}

func bid(raw string) (time.Duration, error) {
	if strings.TrimSpace(raw) == "" {
		return 0, errors.New("--for is required, such as --for 40m")
	}
	d, err := time.ParseDuration(strings.TrimSpace(raw))
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("--for %q must be a positive duration, such as 40m", raw)
	}
	return d, nil
}

func checkCaller(caller string) error {
	if caller == "" {
		return errors.New("the lease needs a caller handle")
	}
	return nil
}

// checkKey accepts <machine>:<absolute path>.
func checkKey(key string) error {
	machine, path, ok := strings.Cut(key, ":")
	if !ok || machine == "" || strings.Contains(machine, "/") || !filepath.IsAbs(path) || filepath.Clean(path) != path {
		return fmt.Errorf("checkout %q must be <machine>:<absolute path>", key)
	}
	return nil
}

func load(dir string) (file, error) {
	doc := file{Version: fileVersion, Leases: map[string]*record{}}
	data, err := os.ReadFile(filepath.Join(dir, stateFile))
	if errors.Is(err, os.ErrNotExist) {
		return doc, nil
	}
	if err != nil {
		return file{}, fmt.Errorf("read leases: %w", err)
	}
	if err := json.Unmarshal(data, &doc); err != nil {
		return file{}, fmt.Errorf("decode leases: %w", err)
	}
	if doc.Version != fileVersion {
		return file{}, fmt.Errorf("leases file version %d, want %d", doc.Version, fileVersion)
	}
	if doc.Leases == nil {
		doc.Leases = map[string]*record{}
	}
	return doc, nil
}

// save writes the table to a temp file and renames it into place, so a
// crash never leaves a half-written file. The caller holds t.mu.
func (t *Table) save() error {
	data, err := json.MarshalIndent(t.doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode leases: %w", err)
	}
	data = append(data, '\n')
	if err := os.MkdirAll(t.dir, 0o755); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	tmp, err := os.CreateTemp(t.dir, ".leases-*.tmp")
	if err != nil {
		return fmt.Errorf("write leases: %w", err)
	}
	_, werr := tmp.Write(data)
	cerr := tmp.Close()
	if werr == nil && cerr == nil {
		if err := os.Rename(tmp.Name(), filepath.Join(t.dir, stateFile)); err != nil {
			_ = os.Remove(tmp.Name())
			return fmt.Errorf("replace leases: %w", err)
		}
		return nil
	}
	_ = os.Remove(tmp.Name())
	return fmt.Errorf("write leases: %w", errors.Join(werr, cerr))
}
