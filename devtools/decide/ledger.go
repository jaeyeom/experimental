// Package decide stores decision items in a local ledger.
//
// The default directory is $XDG_STATE_HOME/decide, or ~/.local/state/decide
// when XDG_STATE_HOME is unset. Open takes an explicit directory so tests
// and a later config path can override the default.
package decide

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"sort"
	"strings"
	"time"

	"github.com/google/renameio/v2"
	"golang.org/x/sys/unix"
)

const (
	// KindRelay is a choice among options.
	KindRelay = "relay"
	// KindTypeIn is text the operator types somewhere else.
	KindTypeIn = "type-in"

	// StateOpen is waiting for a human.
	StateOpen = "open"
	// StateAnswered has an answer.
	StateAnswered = "answered"
	// StateDone is finished.
	StateDone = "done"
	// StateDoneUnread is done for the agent but still in the user's
	// inbox: an agent's done came while the user had not read a message.
	// The user's archive or answer takes it out.
	StateDoneUnread = "done-unread"
	// StateWithdrawn was withdrawn. Its id is never reused.
	StateWithdrawn = "withdrawn"

	// ViaUI is an answer given in the inbox TUI or Emacs.
	ViaUI = "ui"
	// ViaCLI is an answer given with the decide command line.
	ViaCLI = "cli"

	ledgerName  = "ledger.json"
	changesName = "changes.json"
	lockName    = "ledger.lock"
)

// ErrNotFound is returned when an id is not in the ledger.
var ErrNotFound = errors.New("decide item not found")

// ErrNewerMessage is returned when archive's seen time is older than a
// message on the item. The item stays open.
var ErrNewerMessage = errors.New("new message arrived: not archived")

// Option is one choice on a relay item. At most one option is recommended.
type Option struct {
	Key         string `json:"key"`
	Label       string `json:"label"`
	Recommended bool   `json:"recommended"`
}

// Message is one entry in a thread. Author is "human" or an agent name.
// Options are present only when that message asks for a choice.
// OptionKey and OptionLabel are set on a message that answers with an option.
// Via is ViaUI or ViaCLI on a human message, as in Answer.Via. It is empty
// on an agent message and on a message stored before via existed, which
// does not count as ViaUI.
// By is who sent the message from the command line: the caller's first
// agentbus name, else its herdr tab label, else its pane handle. It is
// empty when unknown. Author stays the thread owner on an agent reply.
type Message struct {
	Author      string     `json:"author"`
	Time        time.Time  `json:"time"`
	WrittenAt   *time.Time `json:"writtenAt,omitempty"`
	ReceivedAt  *time.Time `json:"receivedAt,omitempty"`
	Text        string     `json:"text"`
	Verbatim    string     `json:"verbatim,omitempty"`
	Options     []Option   `json:"options,omitempty"`
	OptionKey   string     `json:"optionKey,omitempty"`
	OptionLabel string     `json:"optionLabel,omitempty"`
	Via         string     `json:"via,omitempty"`
	By          string     `json:"by,omitempty"`
}

// Sender is who to show for this message: By when it is set, else Author.
// The item owner stays in the item header and is not repeated here.
func (m Message) Sender() string {
	if m.By != "" {
		return m.By
	}
	return m.Author
}

// Answer is set only when an item has been answered.
// Value is the option key or the free text. Key and Label are set for an
// option answer and keep the label the option had when it was picked.
// Via is ViaUI or ViaCLI. It is empty on an answer stored before via
// existed, which does not count as ViaUI.
type Answer struct {
	Value      string     `json:"value"`
	Key        string     `json:"key,omitempty"`
	Label      string     `json:"label,omitempty"`
	AnsweredBy string     `json:"answeredBy"`
	Via        string     `json:"via,omitempty"`
	WrittenAt  *time.Time `json:"writtenAt,omitempty"`
	ReceivedAt *time.Time `json:"receivedAt,omitempty"`
}

// CheckVia reports an error unless via is ViaUI or ViaCLI.
func CheckVia(via string) error {
	if via != ViaUI && via != ViaCLI {
		return fmt.Errorf("decide via %q must be %s or %s", via, ViaUI, ViaCLI)
	}
	return nil
}

// Item is one stored decision, kept as a thread of messages.
type Item struct {
	ID          string     `json:"id"`
	Question    string     `json:"question"`
	Verbatim    string     `json:"verbatim,omitempty"`
	Messages    []Message  `json:"messages,omitempty"`
	Options     []Option   `json:"options,omitempty"`
	FreeText    bool       `json:"freeText"`
	Owner       string     `json:"owner"`
	Links       []string   `json:"links,omitempty"`
	Deadline    *time.Time `json:"deadline,omitempty"`
	Kind        string     `json:"kind"`
	Class       string     `json:"class,omitempty"`
	Parent      string     `json:"parent,omitempty"`
	State       string     `json:"state"`
	CreatedAt   time.Time  `json:"createdAt,omitempty"`
	AnsweredAt  time.Time  `json:"answeredAt,omitempty"`
	DoneAt      time.Time  `json:"doneAt,omitempty"`
	WithdrawnAt time.Time  `json:"withdrawnAt,omitempty"`
	Answer      *Answer    `json:"answer,omitempty"`
	Unread      bool       `json:"unread"`
	// ReadAt is when the item was last marked read. A mark before
	// AnsweredAt has not seen this answer.
	ReadAt *time.Time `json:"readAt,omitempty"`
	// FollowUp is why a stale answer was not re-sent. Empty means none.
	// A new answer clears it.
	FollowUp   string `json:"followUp,omitempty"`
	NotifyPane string `json:"notifyPane,omitempty"`
	// NotifyMachine is the herdr machine NotifyPane is on, for an item
	// asked in hub mode from another machine. Empty is this machine.
	NotifyMachine string `json:"notifyMachine,omitempty"`
	// NotifyClosed is set when reassign clears the pane, so nothing is
	// routed. An empty NotifyPane without it still uses the config pane.
	NotifyClosed bool `json:"notifyClosed,omitempty"`
	// Worker is the agent working on the item for its owner, set by
	// decide assign. The owner does not change. WorkerNotifyPane is where
	// a reply routed to the worker goes, agentbus:<Worker> by default, and
	// WorkerNotifyMachine is the herdr machine of a pane id there.
	Worker              string `json:"worker,omitempty"`
	WorkerNotifyPane    string `json:"workerNotifyPane,omitempty"`
	WorkerNotifyMachine string `json:"workerNotifyMachine,omitempty"`
	TypeText            string `json:"typeText,omitempty"`
	Where               string `json:"where,omitempty"`
	Reason              string `json:"reason,omitempty"`
	Outcome             string `json:"outcome,omitempty"`
	// ForwardedFrom is the item id a forwarded thread was started from.
	ForwardedFrom string `json:"forwardedFrom,omitempty"`
	// BusMessage is the agentbus message id of a note sent to a bus name.
	BusMessage string `json:"busMessage,omitempty"`
	// NotifyError is why the last notification was not delivered.
	// Empty means it was delivered, or no notification ran.
	NotifyError string `json:"notifyError,omitempty"`
	// DeliveryNotice is the once-per-delivery attention marker shown in
	// the list and thread while a message is late or has failed.
	DeliveryNotice string `json:"deliveryNotice,omitempty"`
	// Delivery is each attempt to deliver an answer, oldest first.
	Delivery []Delivery `json:"delivery,omitempty"`
	// SnoozedUntil hides the item until that time. SnoozedAt is when the
	// snooze was set. A new message on the thread clears both.
	SnoozedUntil *time.Time `json:"snoozedUntil,omitempty"`
	SnoozedAt    *time.Time `json:"snoozedAt,omitempty"`
	// OwnerRepliesOnly refuses decide reply and decide send --id unless
	// the sender is Owner. A human reply from the inbox and an answer
	// are still allowed.
	OwnerRepliesOnly bool `json:"ownerRepliesOnly,omitempty"`
	// Supersedes is the id of the older item this one replaces, and
	// SupersededBy the id of the newer item that replaces this one. Both
	// are set together by Supersede.
	Supersedes   string `json:"supersedes,omitempty"`
	SupersededBy string `json:"supersededBy,omitempty"`
	// Tags are optional labels on the item (such as "off-line").
	Tags []string `json:"tags,omitempty"`
}

// LastMessageAt is when the newest message in the thread was sent, or
// CreatedAt when the thread has no newer message.
func (it *Item) LastMessageAt() time.Time {
	last := it.CreatedAt
	for _, m := range it.Messages {
		if m.Time.After(last) {
			last = m.Time
		}
	}
	return last
}

// MarshalJSON adds lastMessageAt, so JSON readers can see the sort key.
func (it Item) MarshalJSON() ([]byte, error) {
	type plain Item
	data, err := json.Marshal(struct {
		plain
		LastMessageAt time.Time `json:"lastMessageAt"`
	}{plain(it), it.LastMessageAt()})
	if err != nil {
		return nil, fmt.Errorf("marshal decide item %s: %w", it.ID, err)
	}
	return data, nil
}

// Ledger is the on-disk decision store for one directory.
type Ledger struct {
	dir string
	now func() time.Time
}

// maxChangeLog is how many recent ledger changes are kept for incremental sync.
const maxChangeLog = 1000

// LedgerChange records one item mutation at monotonic sequence Seq.
type LedgerChange struct {
	Seq     int64  `json:"seq"`
	ID      string `json:"id"`
	Item    *Item  `json:"item,omitempty"`
	Deleted bool   `json:"deleted,omitempty"`
}

type changeLog struct {
	Seq     int64          `json:"seq"`
	Changes []LedgerChange `json:"changes,omitempty"`
}

type store struct {
	Next  int    `json:"next"`
	Items []Item `json:"items"`
}

// DefaultDir returns ${XDG_STATE_HOME:-~/.local/state}/decide.
func DefaultDir() (string, error) {
	if v := os.Getenv("XDG_STATE_HOME"); v != "" {
		return filepath.Join(v, "decide"), nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("home dir: %w", err)
	}
	return filepath.Join(home, ".local", "state", "decide"), nil
}

// Open creates dir if needed and returns a ledger stored there.
func Open(dir string) (*Ledger, error) {
	if dir == "" {
		return nil, fmt.Errorf("decide directory is empty")
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("create decide directory: %w", err)
	}
	return &Ledger{dir: dir, now: time.Now}, nil
}

// SetClock replaces the clock used for timestamps and snooze wake-ups.
// A nil clock is time.Now.
func (l *Ledger) SetClock(now func() time.Time) {
	if now == nil {
		now = time.Now
	}
	l.now = now
}

// Create stores item and assigns the next id. Ids start at D-001 and are
// never reused. An empty state becomes open. A zero CreatedAt becomes now.
func (l *Ledger) Create(item *Item) (Item, error) {
	if item == nil {
		return Item{}, fmt.Errorf("decide item is nil")
	}
	stored := cloneItem(item)
	if err := validate(&stored); err != nil {
		return Item{}, err
	}
	if !isUICreated(&stored) {
		limits, err := loadShapeLimits()
		if err != nil {
			return Item{}, err
		}
		if err := CheckShape(stored.Question, limits); err != nil {
			return Item{}, err
		}
		if err := checkOptionsShape(stored.Options, limits); err != nil {
			return Item{}, err
		}
	}
	if stored.Class == "" {
		stored.Class = ClassDecision
	}
	if stored.State == "" {
		stored.State = StateOpen
	}
	if stored.CreatedAt.IsZero() {
		stored.CreatedAt = l.clock()
	}
	normalizeTimes(&stored)
	ensureMessages(&stored)
	err := l.locked(true, func(st *store) error {
		if stored.Parent != "" {
			parent, err := findItem(st, stored.Parent)
			if err != nil {
				return fmt.Errorf("parent %s: %w", stored.Parent, err)
			}
			stored.Parent = parent.ID
		}
		stored.ID = st.allocID(stored.Class)
		st.Items = append(st.Items, stored)
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return stored, nil
}

// Get returns the item with id.
func (l *Ledger) Get(id string) (Item, error) {
	var found Item
	err := l.locked(false, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		found = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return found, nil
}

// MarkRead clears unread on id and keeps every other field.
func (l *Ledger) MarkRead(id string) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		it.Unread = false
		now := l.clock().UTC()
		it.ReadAt = &now
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// AddTag adds tag to id if it is not already present.
func (l *Ledger) AddTag(id, tag string) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if !slices.Contains(it.Tags, tag) {
			it.Tags = append(it.Tags, tag)
		}
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// List returns every stored item in storage order.
func (l *Ledger) List() ([]Item, error) {
	var items []Item
	err := l.locked(false, func(st *store) error {
		items = append([]Item(nil), st.Items...)
		return nil
	})
	if err != nil {
		return nil, err
	}
	if items == nil {
		items = []Item{}
	}
	return items, nil
}

// ChangesSince returns the ledger changes after since, or a full resync
// when since is 0, ahead of the ledger, or older than the retained log.
func (l *Ledger) ChangesSince(since int64) (seq int64, changes []LedgerChange, items []Item, resync bool, err error) {
	err = l.locked(false, func(st *store) error {
		cl, cerr := loadChanges(filepath.Join(l.dir, changesName))
		if cerr != nil {
			return cerr
		}
		seq = cl.Seq
		if since == 0 || since > cl.Seq {
			resync = true
			items = cloneItems(st.Items)
			return nil
		}
		if since == cl.Seq {
			return nil
		}
		if len(cl.Changes) == 0 || since < cl.Changes[0].Seq-1 {
			resync = true
			items = cloneItems(st.Items)
			return nil
		}
		for _, ch := range cl.Changes {
			if ch.Seq > since {
				c := ch
				if ch.Item != nil {
					cloned := cloneItem(ch.Item)
					c.Item = &cloned
				}
				changes = append(changes, c)
			}
		}
		return nil
	})
	return seq, changes, items, resync, err
}

func cloneItems(src []Item) []Item {
	out := make([]Item, len(src))
	for i := range src {
		out[i] = cloneItem(&src[i])
	}
	return out
}

// Withdraw marks id withdrawn, stores reason, and keeps the record.
// The id is not reused. A withdrawn, done, or done-unread item is refused.
func (l *Ledger) Withdraw(id, reason string) (Item, error) {
	reason = strings.TrimSpace(reason)
	if reason == "" {
		return Item{}, fmt.Errorf("decide reason is empty")
	}
	updated, err := l.finish(id, StateWithdrawn, reason)
	if err != nil {
		return Item{}, err
	}
	l.clearItemDrafts(updated.ID)
	return updated, nil
}

// Done is an agent's done: DoneVia with ViaCLI.
func (l *Ledger) Done(id, outcome string) (Item, error) {
	return l.DoneVia(id, outcome, ViaCLI)
}

// DoneVia marks id done and stores outcome. A withdrawn or done item is
// refused. via is ViaUI for the user's own done from an inbox front end,
// which closes the item at once and does not check message times. On a
// done-unread item, the outcome "archived" keeps the agent's outcome.
//
// ViaCLI is an agent's done. An agent cannot remove an unread item from
// the user's inbox: when the item is unread, or an agent message is newer
// than both readAt and the last human message, the item becomes
// done-unread. It stays in the inbox, marked unread, with the outcome
// appended to the thread as the owner's closing note. A done-unread item
// done again re-checks, and the same outcome adds no second note.
func (l *Ledger) DoneVia(id, outcome, via string) (Item, error) {
	outcome = strings.TrimSpace(outcome)
	if outcome == "" {
		return Item{}, fmt.Errorf("decide outcome is empty")
	}
	if err := CheckVia(via); err != nil {
		return Item{}, err
	}
	if err := checkAuthorShape(outcome, via); err != nil {
		return Item{}, err
	}
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.EffectiveClass() == ClassNote {
			return fmt.Errorf("decide note %s can only be closed with archive", id)
		}
		if it.EffectiveClass() == ClassDecision && via == ViaCLI && outcomeLineCount(outcome) > 2 {
			return fmt.Errorf("decide decision done outcome must be 1 or 2 lines")
		}
		if it.State == StateWithdrawn || it.State == StateDone {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		now := l.clock().UTC()
		applyDone(it, outcome, via, now)
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	l.clearItemDrafts(updated.ID)
	return updated, nil
}

func applyDone(it *Item, outcome, via string, now time.Time) {
	switch {
	case via == ViaUI:
		if it.State != StateDoneUnread || outcome != "archived" {
			it.Outcome = outcome
		}
		if it.State != StateDoneUnread {
			it.DoneAt = now
		}
		it.State = StateDone
		clearSnooze(it)
	case it.State == StateDoneUnread && it.Outcome == outcome:
	case hasUnread(it):
		ensureMessages(it)
		it.Messages = append(it.Messages, Message{
			Author: it.Owner,
			Time:   now,
			Text:   "done: " + outcome,
		})
		it.State = StateDoneUnread
		it.Unread = true
		it.DoneAt = now
		it.Outcome = outcome
		wakeOnMessage(it)
	default:
		it.State = StateDone
		clearSnooze(it)
		it.DoneAt = now
		it.Outcome = outcome
	}
}

// hasUnread reports whether it has a message the user has not read: the
// item is marked unread, or an agent message is newer than both readAt
// and the last human message. A human message reads what came before
// it. A system note is not an agent message.
func hasUnread(it *Item) bool {
	if it.Unread {
		return true
	}
	ensureMessages(it)
	var seen time.Time
	if it.ReadAt != nil {
		seen = *it.ReadAt
	}
	for _, m := range it.Messages {
		if m.Author == "human" && m.Time.After(seen) {
			seen = m.Time
		}
	}
	for _, m := range it.Messages {
		if m.Author != "human" && m.Author != "system" && m.Time.After(seen) {
			return true
		}
	}
	return false
}

// Archive marks id done with outcome archived when every message is at
// or before seen. A message newer than seen returns ErrNewerMessage and
// leaves the item open. The check and the write share one lock.
// A done-unread item keeps the agent's outcome and done time.
func (l *Ledger) Archive(id string, seen time.Time) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.State == StateWithdrawn || it.State == StateDone {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		for _, msg := range it.Messages {
			if msg.Time.After(seen) {
				return ErrNewerMessage
			}
		}
		if it.State != StateDoneUnread {
			it.DoneAt = l.clock().UTC()
			it.Outcome = "archived"
		}
		it.State = StateDone
		clearSnooze(it)
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	l.clearItemDrafts(updated.ID)
	return updated, nil
}

// Answer records value on a relay item, sets answeredBy to human, and
// stores via, which must be ViaUI or ViaCLI.
// value is an option key, or free text when the item allows it.
// Free text is stored as given, including quotes and punctuation.
// A type-in, withdrawn, or done item is refused.
func (l *Ledger) Answer(id, value, via string) (Item, error) {
	return l.AnswerAt(id, value, via, time.Time{})
}

// AnswerAt is Answer with an optional client writtenAt timestamp. When
// writtenAt is non-zero, both WrittenAt and ReceivedAt (hub clock) are stored.
func (l *Ledger) AnswerAt(id, value, via string, writtenAt time.Time) (Item, error) {
	if strings.TrimSpace(value) == "" {
		return Item{}, fmt.Errorf("decide answer is empty")
	}
	if err := CheckVia(via); err != nil {
		return Item{}, err
	}
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.Kind == KindTypeIn {
			return fmt.Errorf("decide item %s is type-in", id)
		}
		if it.EffectiveClass() == ClassNote {
			return fmt.Errorf("decide note %s takes no answer", id)
		}
		if it.State == StateWithdrawn || it.State == StateDone {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		stored := value
		if it.EffectiveClass() == ClassFact {
			stored = strings.TrimSpace(value)
		} else if !it.FreeText {
			stored = strings.TrimSpace(value)
			if !hasOption(it, stored) {
				return fmt.Errorf("decide answer %q is not an option", stored)
			}
		}
		// Capture a legacy thread before replacing the answer. Otherwise the
		// rebuild treats the new answer as the previous one and drops it.
		ensureMessages(it)
		receivedAt := l.clock().UTC()
		it.State = StateAnswered
		it.AnsweredAt = receivedAt
		it.FollowUp = ""
		it.Answer = &Answer{Value: stored, AnsweredBy: "human", Via: via}
		msg := Message{
			Author: "human",
			Time:   receivedAt,
			Text:   stored,
			Via:    via,
		}
		if !writtenAt.IsZero() {
			w := writtenAt.UTC()
			r := receivedAt
			it.Answer.WrittenAt = &w
			it.Answer.ReceivedAt = &r
			msg.WrittenAt = &w
			msg.ReceivedAt = &r
		}
		if label, ok := optionLabel(it.Options, stored); ok {
			it.Answer.Key, it.Answer.Label = stored, label
			msg.OptionKey, msg.OptionLabel = stored, label
			msg.Text = keyLabel(stored, label)
		}
		it.Messages = append(it.Messages, msg)
		wakeOnMessage(it)
		updated = *it
		noteApprovalOnParent(st, it, stored, receivedAt)
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// StartThread stores a human compose addressed to owner.
// pane is the owner's notify pane. text is the first message and the question.
// via is stored on the message, as in Answer.
func (l *Ledger) StartThread(owner, pane, text, via string) (Item, error) {
	return l.ForwardThreadVerbatim("", owner, pane, text, "", via)
}

// StartThreadVerbatim is StartThread with an optional verbatim block.
func (l *Ledger) StartThreadVerbatim(owner, pane, text, verbatim, via string) (Item, error) {
	return l.ForwardThreadVerbatim("", owner, pane, text, verbatim, via)
}

// ForwardThread is StartThread for a note forwarded from item from.
// An empty from starts a plain thread.
func (l *Ledger) ForwardThread(from, owner, pane, text, via string) (Item, error) {
	return l.ForwardThreadVerbatimAt(from, owner, pane, text, "", via, time.Time{})
}

// ForwardThreadVerbatim is ForwardThread with an optional verbatim block.
func (l *Ledger) ForwardThreadVerbatim(from, owner, pane, text, verbatim, via string) (Item, error) {
	return l.ForwardThreadVerbatimAt(from, owner, pane, text, verbatim, via, time.Time{})
}

// ForwardThreadAt is ForwardThread with an optional client writtenAt timestamp.
func (l *Ledger) ForwardThreadAt(from, owner, pane, text, via string, writtenAt time.Time) (Item, error) {
	return l.ForwardThreadVerbatimAt(from, owner, pane, text, "", via, writtenAt)
}

// ForwardThreadVerbatimAt is ForwardThreadVerbatim with an optional client writtenAt timestamp.
func (l *Ledger) ForwardThreadVerbatimAt(from, owner, pane, text, verbatim, via string, writtenAt time.Time) (Item, error) {
	if strings.TrimSpace(owner) == "" {
		return Item{}, fmt.Errorf("decide owner is empty")
	}
	if strings.TrimSpace(text) == "" && strings.TrimSpace(verbatim) == "" {
		return Item{}, fmt.Errorf("decide text is empty")
	}
	if err := CheckVia(via); err != nil {
		return Item{}, err
	}
	now := l.clock().UTC()
	msg := Message{
		Author:   "human",
		Time:     now,
		Text:     text,
		Verbatim: verbatim,
		Via:      via,
	}
	if !writtenAt.IsZero() {
		w := writtenAt.UTC()
		r := now
		msg.WrittenAt = &w
		msg.ReceivedAt = &r
	}
	return l.Create(&Item{
		Question:      text,
		Verbatim:      verbatim,
		Owner:         owner,
		Kind:          KindRelay,
		NotifyPane:    pane,
		CreatedAt:     now,
		ForwardedFrom: from,
		Messages:      []Message{msg},
	})
}

// SetNotifyError stores why id's last notification was not delivered.
// An empty message clears a previous failure.
func (l *Ledger) SetNotifyError(id, message string) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		it.NotifyError = message
		wasMarked := it.DeliveryNotice != ""
		if message != "" {
			it.DeliveryNotice = "not delivered: " + message
			it.Tags = addTagValue(it.Tags, TagUndelivered)
			if !wasMarked {
				it.Unread = true
			}
		} else {
			it.DeliveryNotice = ""
			it.Tags = removeTagValue(it.Tags, TagUndelivered)
		}
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

func addTagValue(tags []string, tag string) []string {
	if slices.Contains(tags, tag) {
		return tags
	}
	return append(tags, tag)
}

func removeTagValue(tags []string, tag string) []string {
	return slices.DeleteFunc(tags, func(v string) bool { return v == tag })
}

// SetBusMessage stores the agentbus message id that delivered id.
func (l *Ledger) SetBusMessage(id, msg string) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		it.BusMessage = msg
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// Delete removes id. It is for a thread whose first note was never
// delivered. The id is not reused.
func (l *Ledger) Delete(id string) error {
	var canonicalID string
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		canonicalID = it.ID
		i := slices.IndexFunc(st.Items, func(cur Item) bool { return cur.ID == canonicalID })
		st.Items = slices.Delete(st.Items, i, i+1)
		return nil
	})
	if err != nil {
		return err
	}
	l.clearItemDrafts(canonicalID)
	return nil
}

func (l *Ledger) clearItemDrafts(id string) {
	ds, err := OpenDraftStore(l.dir)
	if err != nil {
		return
	}
	_ = ds.Delete(ReplyDraftKey(id), "")
	_ = ds.Delete(AnswerDraftKey(id), "")
}

// Compose appends a human message on id.
// An open relay question also stores text as the answer, unchanged,
// including quotes and newlines, and marks the thread answered by a human.
// That includes a question created without free text.
// An answered, done, or notification thread keeps its answer and state.
// A withdrawn thread is refused. text is stored unchanged.
// via is stored on the message and with an answer, as in Answer.
// by is who sent it, as in Message.By, and may be empty.
func (l *Ledger) Compose(id, text, via, by string) (Item, error) {
	return l.ComposeVerbatimAt(id, text, "", via, by, time.Time{})
}

// ComposeVerbatim appends a human message on id with an optional verbatim block.
func (l *Ledger) ComposeVerbatim(id, text, verbatim, via, by string) (Item, error) {
	return l.ComposeVerbatimAt(id, text, verbatim, via, by, time.Time{})
}

// ComposeAt is Compose with an optional client writtenAt timestamp.
func (l *Ledger) ComposeAt(id, text, via, by string, writtenAt time.Time) (Item, error) {
	return l.ComposeVerbatimAt(id, text, "", via, by, writtenAt)
}

// ComposeVerbatimAt is ComposeVerbatim with an optional client writtenAt timestamp.
func (l *Ledger) ComposeVerbatimAt(id, text, verbatim, via, by string, writtenAt time.Time) (Item, error) {
	if strings.TrimSpace(text) == "" && strings.TrimSpace(verbatim) == "" {
		return Item{}, fmt.Errorf("decide text is empty")
	}
	if err := CheckVia(via); err != nil {
		return Item{}, err
	}
	if strings.TrimSpace(text) != "" {
		if err := checkAuthorShape(text, via); err != nil {
			return Item{}, err
		}
	}
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.State == StateWithdrawn {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		ensureMessages(it)
		now := l.clock().UTC()
		msg := Message{
			Author:   "human",
			Time:     now,
			Text:     text,
			Verbatim: verbatim,
			Via:      via,
			By:       by,
		}
		if !writtenAt.IsZero() {
			w := writtenAt.UTC()
			r := now
			msg.WrittenAt = &w
			msg.ReceivedAt = &r
		}
		it.Messages = append(it.Messages, msg)
		if openQuestion(it) {
			ans := text
			if ans == "" {
				ans = verbatim
			}
			it.State = StateAnswered
			it.AnsweredAt = now
			it.Answer = &Answer{Value: ans, AnsweredBy: "human", Via: via}
			if !writtenAt.IsZero() {
				w := writtenAt.UTC()
				r := now
				it.Answer.WrittenAt = &w
				it.Answer.ReceivedAt = &r
			}
		}
		wakeOnMessage(it)
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// RejectForeignSender reports when by may not post on an owner-only item.
// The sender is the owner when by equals Owner. An empty by is not the owner.
// A human inbox reply does not call this, and neither does an answer.
func (it *Item) RejectForeignSender(by string) error {
	if it == nil || !it.OwnerRepliesOnly || by == it.Owner {
		return nil
	}
	return fmt.Errorf("item %s accepts replies only from its owner %s; message the owner instead", it.ID, it.Owner)
}

// Reply appends an agent message. See ReplyVerbatim.
func (l *Ledger) Reply(id, by, text string, options []Option, freeText bool) (Item, error) {
	return l.ReplyVerbatim(id, by, text, "", options, freeText)
}

// ReplyVerbatim appends an agent message with an optional verbatim quote
// block that is stored byte for byte without shape checks. The author's own
// text and option labels are shape-checked.
// The author is the thread owner, and by, which may be empty, is who wrote it,
// as in Message.By. Options replace the pending options and freeText. A reply
// with no options keeps them while the item is open, and clears options only
// when reopening an answered or done item.
// An answered, done, or done-unread thread returns to the inbox. A withdrawn
// thread is refused.
// OwnerRepliesOnly refuses a sender other than the owner.
func (l *Ledger) ReplyVerbatim(id, by, text, verbatim string, options []Option, freeText bool) (Item, error) {
	if strings.TrimSpace(text) == "" {
		return Item{}, fmt.Errorf("decide text is empty")
	}
	if err := oneRecommended(options); err != nil {
		return Item{}, err
	}
	limits, err := loadShapeLimits()
	if err != nil {
		return Item{}, err
	}
	if err := CheckShape(text, limits); err != nil {
		return Item{}, err
	}
	if err := checkOptionsShape(options, limits); err != nil {
		return Item{}, err
	}
	var updated Item
	err = l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.State == StateWithdrawn {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		if err := checkReplyClass(it, options, freeText); err != nil {
			return err
		}
		if err := it.RejectForeignSender(by); err != nil {
			return err
		}
		ensureMessages(it)
		msg := Message{
			Author:   it.Owner,
			Time:     l.clock().UTC(),
			Text:     text,
			Verbatim: verbatim,
			By:       by,
		}
		if len(options) > 0 {
			msg.Options = append([]Option(nil), options...)
			it.Options = append([]Option(nil), options...)
			it.FreeText = freeText
		} else if it.State == StateAnswered || it.State == StateDone || it.State == StateDoneUnread {
			it.Options = nil
		}
		it.Messages = append(it.Messages, msg)
		it.State = StateOpen
		it.Unread = true
		wakeOnMessage(it)
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// Retag changes the class of id to newClass, updates its ID prefix, and
// records a system event in the thread with the old and new class.
func (l *Ledger) Retag(id, newClass, by string) (Item, error) {
	canonical, err := ParseClass(newClass)
	if err != nil {
		return Item{}, err
	}
	var updated Item
	err = l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		oldClass := it.EffectiveClass()
		num, ok := parseID(it.ID)
		if !ok {
			return fmt.Errorf("decide item %s has invalid id", it.ID)
		}
		it.Class = canonical
		it.ID = FormatID(canonical, num)
		if oldClass != canonical {
			ensureMessages(it)
			it.Messages = append(it.Messages, Message{
				Author: "system",
				Time:   l.clock().UTC(),
				Text:   retagNote(oldClass, canonical, by),
				By:     by,
			})
		}
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

func retagNote(oldClass, newClass, by string) string {
	note := "retagged: " + oldClass + " → " + newClass
	if by != "" {
		note += " (by " + by + ")"
	}
	return note
}

// Prune removes closed non-approval items older than olderThan.
// Approvals are kept forever and never pruned.
func (l *Ledger) Prune(olderThan time.Duration) (int, error) {
	return l.PruneBefore(l.clock().UTC().Add(-olderThan))
}

// PruneBefore removes done or withdrawn non-approval items closed before cutoff.
// Approvals are kept forever and never pruned.
func (l *Ledger) PruneBefore(cutoff time.Time) (int, error) {
	var removed int
	err := l.locked(true, func(st *store) error {
		kept := st.Items[:0]
		for i := range st.Items {
			it := st.Items[i]
			if shouldPrune(&it, cutoff) {
				removed++
				continue
			}
			kept = append(kept, it)
		}
		st.Items = kept
		return nil
	})
	if err != nil {
		return 0, err
	}
	return removed, nil
}

func shouldPrune(it *Item, cutoff time.Time) bool {
	if it.EffectiveClass() == ClassApproval {
		return false
	}
	var closedAt time.Time
	switch it.State {
	case StateDone:
		closedAt = it.DoneAt
	case StateWithdrawn:
		closedAt = it.WithdrawnAt
	default:
		return false
	}
	if closedAt.IsZero() {
		closedAt = it.CreatedAt
	}
	return closedAt.Before(cutoff)
}

func outcomeLineCount(outcome string) int {
	normalized := strings.ReplaceAll(strings.TrimSpace(outcome), "\r\n", "\n")
	if normalized == "" {
		return 0
	}
	count := 0
	for _, line := range strings.Split(normalized, "\n") {
		if strings.TrimSpace(line) != "" {
			count++
		}
	}
	return count
}

// Reassign is an owner or notify-pane change. SetOwner and SetPane
// choose which fields change. Pane may be empty when SetPane is set,
// which clears routing. Machine is the herdr machine of a pane id and
// is ignored for an empty pane or an agentbus name. By is who ran it
// and may be empty.
type Reassign struct {
	Owner    string
	SetOwner bool
	Pane     string
	SetPane  bool
	Machine  string
	By       string
}

// Reassign moves id to a new owner, notify pane, or both.
// A done, done-unread, or withdrawn item is refused. The thread gains one system
// message. Earlier messages and the answer stay as they were.
func (l *Ledger) Reassign(id string, change Reassign) (Item, error) {
	if !change.SetOwner && !change.SetPane {
		return Item{}, fmt.Errorf("decide reassign requires an owner or a notify pane")
	}
	owner := strings.TrimSpace(change.Owner)
	if change.SetOwner && owner == "" {
		return Item{}, fmt.Errorf("decide owner is empty")
	}
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.State == StateDone || it.State == StateDoneUnread || it.State == StateWithdrawn {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		ensureMessages(it)
		oldOwner, oldPane := it.Owner, it.NotifyPane
		if change.SetOwner {
			it.Owner = owner
		}
		if change.SetPane {
			applyNotifyPane(it, change.Pane, change.Machine)
		}
		it.Messages = append(it.Messages, Message{
			Author: "system",
			Time:   l.clock().UTC(),
			Text:   reassignNote(oldOwner, it.Owner, oldPane, it.NotifyPane, change.By, change.SetOwner, change.SetPane),
			By:     change.By,
		})
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// applyNotifyPane stores pane and its machine. An empty pane clears
// routing. An agentbus name is not on a machine.
func applyNotifyPane(it *Item, pane, machine string) {
	pane = strings.TrimSpace(pane)
	it.NotifyPane = pane
	it.NotifyMachine = ""
	it.NotifyClosed = pane == ""
	if pane == "" || strings.HasPrefix(pane, BusPanePrefix) {
		return
	}
	it.NotifyMachine = strings.TrimSpace(machine)
}

// reassignNote is the system message for one reassign.
func reassignNote(oldOwner, newOwner, oldPane, newPane, by string, setOwner, setPane bool) string {
	var parts []string
	if setOwner {
		parts = append(parts, "owner "+oldOwner+" → "+newOwner)
	}
	if setPane {
		parts = append(parts, "notify pane "+oldPane+" → "+newPane)
	}
	note := "reassigned: " + strings.Join(parts, "; ")
	if by != "" {
		note += " (by " + by + ")"
	}
	return note
}

// openQuestion is a relay thread still waiting for a human answer.
// A notification has no options and does not allow free text.
func openQuestion(it *Item) bool {
	return it.Kind == KindRelay && it.State == StateOpen && (len(it.Options) > 0 || it.FreeText)
}

// Select returns items in state that are not snoozed, in Sort order.
// An empty state means open. Open is the inbox, which also holds
// done-unread items.
func Select(items []Item, state string) ([]Item, error) {
	if state == "" {
		state = StateOpen
	}
	switch state {
	case StateOpen, StateAnswered, StateDone, StateDoneUnread, StateWithdrawn:
	default:
		return nil, fmt.Errorf("decide state %q is invalid", state)
	}
	out := make([]Item, 0)
	for _, it := range items {
		if InState(&it, state) && !it.Snoozed() {
			out = append(out, it)
		}
	}
	Sort(out)
	return out, nil
}

// InState reports whether it belongs in the list for state. The open
// list, the inbox, also holds done-unread items.
func InState(it *Item, state string) bool {
	return it.State == state || (state == StateOpen && it.State == StateDoneUnread)
}

// Sort orders items by soonest deadline, then no deadline, then newest
// LastMessageAt. Equal times break toward the higher id.
func Sort(items []Item) {
	sort.SliceStable(items, func(i, j int) bool {
		return before(&items[i], &items[j])
	})
}

func (l *Ledger) clock() time.Time {
	if l.now == nil {
		return time.Now()
	}
	return l.now()
}

func (l *Ledger) locked(write bool, fn func(*store) error) (err error) {
	f, err := l.acquire()
	if err != nil {
		return err
	}
	defer func() {
		relErr := release(f)
		if err == nil {
			err = relErr
		}
	}()
	st, err := load(filepath.Join(l.dir, ledgerName))
	if err != nil {
		return err
	}
	before := snapshotItems(st.Items)
	migrated := migrateStore(st)
	if wakeStore(st, l.clock()) {
		migrated = true
	}
	if migrated {
		if err = save(l.dir, st); err != nil {
			return err
		}
		if err = l.saveDelta(before, st.Items); err != nil {
			return err
		}
		before = snapshotItems(st.Items)
	}
	if err = fn(st); err != nil {
		return err
	}
	if !write {
		return nil
	}
	if err = save(l.dir, st); err != nil {
		return err
	}
	return l.saveDelta(before, st.Items)
}

func (l *Ledger) saveDelta(before map[string]Item, after []Item) error {
	cl, err := loadChanges(filepath.Join(l.dir, changesName))
	if err != nil {
		return err
	}
	if !recordChanges(cl, before, after) {
		return nil
	}
	return saveChanges(l.dir, cl)
}

func snapshotItems(items []Item) map[string]Item {
	snap := make(map[string]Item, len(items))
	for i := range items {
		snap[items[i].ID] = cloneItem(&items[i])
	}
	return snap
}

func recordChanges(cl *changeLog, before map[string]Item, after []Item) bool {
	changed := false
	seen := make(map[string]bool, len(after))
	for i := range after {
		it := &after[i]
		seen[it.ID] = true
		prev, ok := before[it.ID]
		if !ok || !reflect.DeepEqual(prev, *it) {
			cl.Seq++
			cloned := cloneItem(it)
			cl.Changes = append(cl.Changes, LedgerChange{Seq: cl.Seq, ID: it.ID, Item: &cloned})
			changed = true
		}
	}
	for id := range before {
		if !seen[id] {
			cl.Seq++
			cl.Changes = append(cl.Changes, LedgerChange{Seq: cl.Seq, ID: id, Deleted: true})
			changed = true
		}
	}
	if len(cl.Changes) > maxChangeLog {
		cl.Changes = cl.Changes[len(cl.Changes)-maxChangeLog:]
	}
	return changed
}

func loadChanges(path string) (*changeLog, error) {
	data, err := os.ReadFile(path) //nolint:gosec // operator-local state file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return &changeLog{}, nil
		}
		return nil, fmt.Errorf("read decide changes: %w", err)
	}
	var cl changeLog
	if err := json.Unmarshal(data, &cl); err != nil {
		return nil, fmt.Errorf("decode decide changes: %w", err)
	}
	return &cl, nil
}

func saveChanges(dir string, cl *changeLog) error {
	data, err := json.Marshal(cl)
	if err != nil {
		return fmt.Errorf("encode decide changes: %w", err)
	}
	data = append(data, '\n')
	path := filepath.Join(dir, changesName)
	if err := renameio.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write decide changes: %w", err)
	}
	return nil
}

func (l *Ledger) acquire() (*os.File, error) {
	path := filepath.Join(l.dir, lockName)
	//nolint:gosec // operator-local state file
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open decide lock: %w", err)
	}
	if err := flock(f, unix.LOCK_EX); err != nil {
		_ = f.Close()
		return nil, err
	}
	return f, nil
}

func release(f *os.File) error {
	uerr := flock(f, unix.LOCK_UN)
	cerr := f.Close()
	if uerr != nil {
		return uerr
	}
	if cerr != nil {
		return fmt.Errorf("close decide lock: %w", cerr)
	}
	return nil
}

func flock(f *os.File, how int) error {
	if err := unix.Flock(int(f.Fd()), how); err != nil { //nolint:gosec // G115: a file descriptor fits in int
		op := "lock"
		if how == unix.LOCK_UN {
			op = "unlock"
		}
		return fmt.Errorf("%s decide ledger: %w", op, err)
	}
	return nil
}

func load(path string) (*store, error) {
	data, err := os.ReadFile(path) //nolint:gosec // operator-local state file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return &store{Next: 1, Items: []Item{}}, nil
		}
		return nil, fmt.Errorf("read decide ledger: %w", err)
	}
	var st store
	if err := json.Unmarshal(data, &st); err != nil {
		return nil, fmt.Errorf("decode decide ledger: %w", err)
	}
	if st.Items == nil {
		st.Items = []Item{}
	}
	if st.Next < 1 {
		st.Next = 1
	}
	for _, it := range st.Items {
		n, ok := parseID(it.ID)
		if ok && n >= st.Next {
			st.Next = n + 1
		}
	}
	return &st, nil
}

func save(dir string, st *store) error {
	if st.Items == nil {
		st.Items = []Item{}
	}
	data, err := json.MarshalIndent(st, "", "  ")
	if err != nil {
		return fmt.Errorf("encode decide ledger: %w", err)
	}
	data = append(data, '\n')
	path := filepath.Join(dir, ledgerName)
	if err := renameio.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write decide ledger: %w", err)
	}
	return nil
}

func (st *store) allocID(class string) string {
	id := FormatID(class, st.Next)
	st.Next++
	return id
}

func (l *Ledger) finish(id, state, text string) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.State == StateWithdrawn || it.State == StateDone || it.State == StateDoneUnread {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		it.State = state
		clearSnooze(it)
		now := l.clock().UTC()
		switch state {
		case StateWithdrawn:
			it.WithdrawnAt = now
			it.Reason = text
		case StateDone:
			it.DoneAt = now
			it.Outcome = text
		default:
			return fmt.Errorf("decide state %q is invalid", state)
		}
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

func findItem(st *store, id string) (*Item, error) {
	for i := range st.Items {
		if st.Items[i].ID == id {
			return &st.Items[i], nil
		}
	}
	if n, ok := parseID(id); ok {
		for i := range st.Items {
			if itemN, itemOK := parseID(st.Items[i].ID); itemOK && itemN == n {
				return &st.Items[i], nil
			}
		}
	}
	return nil, fmt.Errorf("%w: %s", ErrNotFound, id)
}

func hasOption(it *Item, key string) bool {
	_, ok := optionLabel(it.Options, key)
	return ok
}

func optionLabel(options []Option, key string) (string, bool) {
	for _, opt := range options {
		if opt.Key == key {
			return opt.Label, true
		}
	}
	return "", false
}

func keyLabel(key, label string) string {
	if label == "" {
		return key
	}
	return key + ": " + label
}

// AnswerLabel is the label of the option it was answered with, or "".
// An answer stored without a label looks the key up in the options of the
// latest message at or before the answer, then in the item options.
func AnswerLabel(it *Item) string {
	a := it.Answer
	if a == nil {
		return ""
	}
	if a.Key != "" {
		return a.Label
	}
	for i := len(it.Messages) - 1; i >= 0; i-- {
		msg := it.Messages[i]
		if len(msg.Options) == 0 || msg.Time.After(it.AnsweredAt) {
			continue
		}
		if label, ok := optionLabel(msg.Options, a.Value); ok {
			return label
		}
		break
	}
	label, _ := optionLabel(it.Options, a.Value)
	return label
}

// AnswerText renders the answer of it as "key: label" for an option answer,
// or as the stored value. It is "" when it has no answer.
func AnswerText(it *Item) string {
	if it.Answer == nil {
		return ""
	}
	return keyLabel(it.Answer.Value, AnswerLabel(it))
}

func before(a, b *Item) bool {
	switch {
	case a.Deadline != nil && b.Deadline != nil:
		if !a.Deadline.Equal(*b.Deadline) {
			return a.Deadline.Before(*b.Deadline)
		}
		return newer(a, b)
	case a.Deadline != nil:
		return true
	case b.Deadline != nil:
		return false
	default:
		return newer(a, b)
	}
}

func newer(a, b *Item) bool {
	at, bt := a.LastMessageAt(), b.LastMessageAt()
	if !at.Equal(bt) {
		return at.After(bt)
	}
	ai, _ := parseID(a.ID)
	bi, _ := parseID(b.ID)
	return ai > bi
}

func validate(item *Item) error {
	if _, err := ClassPrefix(item.Class); err != nil {
		return err
	}
	switch item.Kind {
	case KindRelay, KindTypeIn:
	default:
		return fmt.Errorf("decide kind %q is invalid", item.Kind)
	}
	switch item.State {
	case "", StateOpen, StateAnswered, StateDone, StateWithdrawn:
	default:
		return fmt.Errorf("decide state %q is invalid", item.State)
	}
	if err := validateClass(item); err != nil {
		return err
	}
	if err := oneRecommended(item.Options); err != nil {
		return err
	}
	for i := range item.Messages {
		if err := oneRecommended(item.Messages[i].Options); err != nil {
			return err
		}
	}
	return nil
}

func validateClass(item *Item) error {
	switch item.EffectiveClass() {
	case ClassApproval:
		if strings.TrimSpace(item.Verbatim) == "" {
			return fmt.Errorf("decide approval requires a verbatim payload")
		}
		if len(item.Options) == 0 {
			return fmt.Errorf("decide approval requires options")
		}
		return checkApprovalOptions(item.Options)
	case ClassNote:
		if len(item.Options) > 0 || item.FreeText {
			return fmt.Errorf("decide note cannot have options or free-text")
		}
	}
	return nil
}

func checkReplyClass(it *Item, options []Option, freeText bool) error {
	switch it.EffectiveClass() {
	case ClassNote:
		if len(options) > 0 || freeText {
			return fmt.Errorf("decide note cannot have options or free-text")
		}
	case ClassApproval:
		if len(options) > 0 {
			return checkApprovalOptions(options)
		}
	}
	return nil
}

func checkApprovalOptions(opts []Option) error {
	for _, opt := range opts {
		if !isApprovalOptionKey(opt.Key) {
			return fmt.Errorf("decide approval option key %q must be yes, no, or edit", opt.Key)
		}
	}
	return nil
}

func oneRecommended(opts []Option) error {
	recommended := 0
	for _, opt := range opts {
		if opt.Recommended {
			recommended++
		}
	}
	if recommended > 1 {
		return fmt.Errorf("decide item has %d recommended options", recommended)
	}
	return nil
}

func cloneItem(src *Item) Item {
	item := *src
	if src.Options != nil {
		item.Options = append([]Option(nil), src.Options...)
	}
	if src.Links != nil {
		item.Links = append([]string(nil), src.Links...)
	}
	if src.Tags != nil {
		item.Tags = append([]string(nil), src.Tags...)
	}
	if src.Deadline != nil {
		t := *src.Deadline
		item.Deadline = &t
	}
	if src.Answer != nil {
		a := *src.Answer
		item.Answer = &a
	}
	item.SnoozedUntil = cloneTime(src.SnoozedUntil)
	item.SnoozedAt = cloneTime(src.SnoozedAt)
	item.ReadAt = cloneTime(src.ReadAt)
	if src.Messages != nil {
		item.Messages = append([]Message(nil), src.Messages...)
		for i := range item.Messages {
			if src.Messages[i].Options != nil {
				item.Messages[i].Options = append([]Option(nil), src.Messages[i].Options...)
			}
		}
	}
	if src.Delivery != nil {
		item.Delivery = append([]Delivery(nil), src.Delivery...)
	}
	return item
}

func cloneTime(t *time.Time) *time.Time {
	if t == nil {
		return nil
	}
	c := *t
	return &c
}

func normalizeTimes(item *Item) {
	item.CreatedAt = item.CreatedAt.UTC()
	item.AnsweredAt = item.AnsweredAt.UTC()
	item.DoneAt = item.DoneAt.UTC()
	item.WithdrawnAt = item.WithdrawnAt.UTC()
	if item.Deadline != nil {
		t := item.Deadline.UTC()
		item.Deadline = &t
	}
	if item.ReadAt != nil {
		t := item.ReadAt.UTC()
		item.ReadAt = &t
	}
	for i := range item.Messages {
		item.Messages[i].Time = item.Messages[i].Time.UTC()
	}
	for i := range item.Delivery {
		item.Delivery[i].At = item.Delivery[i].At.UTC()
	}
}

// ensureMessages fills a thread from the stored ask and answer.
// A thread that already has messages is left as stored.
func ensureMessages(it *Item) {
	if len(it.Messages) > 0 {
		return
	}
	msg := Message{
		Author:   it.Owner,
		Time:     it.CreatedAt.UTC(),
		Text:     it.Question,
		Verbatim: it.Verbatim,
	}
	if len(it.Options) > 0 {
		msg.Options = append([]Option(nil), it.Options...)
	}
	it.Messages = []Message{msg}
	if it.Answer == nil {
		return
	}
	author := it.Answer.AnsweredBy
	if author == "" {
		author = "human"
	}
	it.Messages = append(it.Messages, Message{
		Author: author,
		Time:   it.AnsweredAt.UTC(),
		Text:   it.Answer.Value,
		Via:    it.Answer.Via,
	})
}

// migrateStore gives every item without messages a thread.
// It reports whether the ledger changed.
func migrateStore(st *store) bool {
	changed := false
	for i := range st.Items {
		if len(st.Items[i].Messages) > 0 {
			continue
		}
		ensureMessages(&st.Items[i])
		changed = true
	}
	return changed
}

func isUICreated(item *Item) bool {
	return len(item.Messages) == 1 && item.Messages[0].Author == "human" && item.Messages[0].Via == ViaUI
}
