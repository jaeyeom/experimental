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
	"slices"
	"sort"
	"strconv"
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
	// StateWithdrawn was withdrawn. Its id is never reused.
	StateWithdrawn = "withdrawn"

	ledgerName = "ledger.json"
	lockName   = "ledger.lock"
)

// ErrNotFound is returned when an id is not in the ledger.
var ErrNotFound = errors.New("decide item not found")

// Option is one choice on a relay item. At most one option is recommended.
type Option struct {
	Key         string `json:"key"`
	Label       string `json:"label"`
	Recommended bool   `json:"recommended"`
}

// Message is one entry in a thread. Author is "human" or an agent name.
// Options are present only when that message asks for a choice.
type Message struct {
	Author  string    `json:"author"`
	Time    time.Time `json:"time"`
	Text    string    `json:"text"`
	Options []Option  `json:"options,omitempty"`
}

// Answer is set only when an item has been answered.
type Answer struct {
	Value      string `json:"value"`
	AnsweredBy string `json:"answeredBy"`
}

// Item is one stored decision, kept as a thread of messages.
type Item struct {
	ID          string     `json:"id"`
	Question    string     `json:"question"`
	Messages    []Message  `json:"messages,omitempty"`
	Options     []Option   `json:"options,omitempty"`
	FreeText    bool       `json:"freeText"`
	Owner       string     `json:"owner"`
	Links       []string   `json:"links,omitempty"`
	Deadline    *time.Time `json:"deadline,omitempty"`
	Kind        string     `json:"kind"`
	State       string     `json:"state"`
	CreatedAt   time.Time  `json:"createdAt,omitempty"`
	AnsweredAt  time.Time  `json:"answeredAt,omitempty"`
	DoneAt      time.Time  `json:"doneAt,omitempty"`
	WithdrawnAt time.Time  `json:"withdrawnAt,omitempty"`
	Answer      *Answer    `json:"answer,omitempty"`
	Unread      bool       `json:"unread"`
	NotifyPane  string     `json:"notifyPane,omitempty"`
	TypeText    string     `json:"typeText,omitempty"`
	Where       string     `json:"where,omitempty"`
	Reason      string     `json:"reason,omitempty"`
	Outcome     string     `json:"outcome,omitempty"`
	// ForwardedFrom is the item id a forwarded thread was started from.
	ForwardedFrom string `json:"forwardedFrom,omitempty"`
	// BusMessage is the agentbus message id of a note sent to a bus name.
	BusMessage string `json:"busMessage,omitempty"`
}

// Ledger is the on-disk decision store for one directory.
type Ledger struct {
	dir string
	now func() time.Time
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
	if stored.State == "" {
		stored.State = StateOpen
	}
	if stored.CreatedAt.IsZero() {
		stored.CreatedAt = l.clock()
	}
	normalizeTimes(&stored)
	ensureMessages(&stored)
	err := l.locked(true, func(st *store) error {
		stored.ID = st.allocID()
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
		for _, it := range st.Items {
			if it.ID == id {
				found = it
				return nil
			}
		}
		return fmt.Errorf("%w: %s", ErrNotFound, id)
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

// Withdraw marks id withdrawn, stores reason, and keeps the record.
// The id is not reused. A withdrawn or done item is refused.
func (l *Ledger) Withdraw(id, reason string) (Item, error) {
	reason = strings.TrimSpace(reason)
	if reason == "" {
		return Item{}, fmt.Errorf("decide reason is empty")
	}
	return l.finish(id, StateWithdrawn, reason)
}

// Done marks id done and stores outcome. A withdrawn or done item is refused.
func (l *Ledger) Done(id, outcome string) (Item, error) {
	outcome = strings.TrimSpace(outcome)
	if outcome == "" {
		return Item{}, fmt.Errorf("decide outcome is empty")
	}
	return l.finish(id, StateDone, outcome)
}

// Answer records value on a relay item and sets answeredBy to human.
// value is an option key, or free text when the item allows it.
// Free text is stored as given, including quotes and punctuation.
// A type-in, withdrawn, or done item is refused.
func (l *Ledger) Answer(id, value string) (Item, error) {
	if strings.TrimSpace(value) == "" {
		return Item{}, fmt.Errorf("decide answer is empty")
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
		if it.State == StateWithdrawn || it.State == StateDone {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		stored := value
		if !it.FreeText {
			stored = strings.TrimSpace(value)
			if !hasOption(it, stored) {
				return fmt.Errorf("decide answer %q is not an option", stored)
			}
		}
		// Capture a legacy thread before replacing the answer. Otherwise the
		// rebuild treats the new answer as the previous one and drops it.
		ensureMessages(it)
		it.State = StateAnswered
		it.AnsweredAt = l.clock().UTC()
		it.Answer = &Answer{Value: stored, AnsweredBy: "human"}
		it.Messages = append(it.Messages, Message{
			Author: "human",
			Time:   it.AnsweredAt,
			Text:   stored,
		})
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// StartThread stores a human compose addressed to owner.
// pane is the owner's notify pane. text is the first message and the question.
func (l *Ledger) StartThread(owner, pane, text string) (Item, error) {
	return l.ForwardThread("", owner, pane, text)
}

// ForwardThread is StartThread for a note forwarded from item from.
// An empty from starts a plain thread.
func (l *Ledger) ForwardThread(from, owner, pane, text string) (Item, error) {
	if strings.TrimSpace(owner) == "" {
		return Item{}, fmt.Errorf("decide owner is empty")
	}
	if strings.TrimSpace(text) == "" {
		return Item{}, fmt.Errorf("decide text is empty")
	}
	now := l.clock().UTC()
	return l.Create(&Item{
		Question:      text,
		Owner:         owner,
		Kind:          KindRelay,
		NotifyPane:    pane,
		CreatedAt:     now,
		ForwardedFrom: from,
		Messages: []Message{{
			Author: "human",
			Time:   now,
			Text:   text,
		}},
	})
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
	return l.locked(true, func(st *store) error {
		i := slices.IndexFunc(st.Items, func(it Item) bool { return it.ID == id })
		if i < 0 {
			return fmt.Errorf("%w: %s", ErrNotFound, id)
		}
		st.Items = slices.Delete(st.Items, i, i+1)
		return nil
	})
}

// Compose appends a human message on id.
// An open relay question also stores text as the answer, unchanged,
// including quotes and newlines, and marks the thread answered by a human.
// That includes a question created without free text.
// An answered, done, or notification thread keeps its answer and state.
// A withdrawn thread is refused. text is stored unchanged.
func (l *Ledger) Compose(id, text string) (Item, error) {
	if strings.TrimSpace(text) == "" {
		return Item{}, fmt.Errorf("decide text is empty")
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
		it.Messages = append(it.Messages, Message{
			Author: "human",
			Time:   now,
			Text:   text,
		})
		if openQuestion(it) {
			it.State = StateAnswered
			it.AnsweredAt = now
			it.Answer = &Answer{Value: text, AnsweredBy: "human"}
		}
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// Reply appends an agent message. The author is the thread owner.
// No options leaves a notification: the thread is open, unread, and has no
// pending options. Options return it to the inbox as needing an answer.
// freeText is stored only when options are present. text is stored unchanged.
// An answered or done thread returns to the inbox. A withdrawn thread is refused.
func (l *Ledger) Reply(id, text string, options []Option, freeText bool) (Item, error) {
	if strings.TrimSpace(text) == "" {
		return Item{}, fmt.Errorf("decide text is empty")
	}
	if err := oneRecommended(options); err != nil {
		return Item{}, err
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
		msg := Message{
			Author: it.Owner,
			Time:   l.clock().UTC(),
			Text:   text,
		}
		if len(options) > 0 {
			msg.Options = append([]Option(nil), options...)
			it.Options = append([]Option(nil), options...)
			it.FreeText = freeText
		} else {
			it.Options = nil
		}
		it.Messages = append(it.Messages, msg)
		it.State = StateOpen
		it.Unread = true
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// openQuestion is a relay thread still waiting for a human answer.
// A notification has no options and does not allow free text.
func openQuestion(it *Item) bool {
	return it.Kind == KindRelay && it.State == StateOpen && (len(it.Options) > 0 || it.FreeText)
}

// Select returns items in state. Deadline order is soonest first, then items
// with no deadline, then newest created time. An empty state means open.
func Select(items []Item, state string) ([]Item, error) {
	if state == "" {
		state = StateOpen
	}
	switch state {
	case StateOpen, StateAnswered, StateDone, StateWithdrawn:
	default:
		return nil, fmt.Errorf("decide state %q is invalid", state)
	}
	out := make([]Item, 0)
	for _, it := range items {
		if it.State == state {
			out = append(out, it)
		}
	}
	Sort(out)
	return out, nil
}

// Sort orders items by soonest deadline, then no deadline, then newest
// created time. Equal times break toward the higher id.
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
	migrated := migrateStore(st)
	if migrated {
		if err = save(l.dir, st); err != nil {
			return err
		}
	}
	if err = fn(st); err != nil {
		return err
	}
	if !write {
		return nil
	}
	return save(l.dir, st)
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

func (st *store) allocID() string {
	id := fmt.Sprintf("D-%03d", st.Next)
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
		if it.State == StateWithdrawn || it.State == StateDone {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		it.State = state
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
	return nil, fmt.Errorf("%w: %s", ErrNotFound, id)
}

func hasOption(it *Item, key string) bool {
	for _, opt := range it.Options {
		if opt.Key == key {
			return true
		}
	}
	return false
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
	if !a.CreatedAt.Equal(b.CreatedAt) {
		return a.CreatedAt.After(b.CreatedAt)
	}
	ai, _ := parseID(a.ID)
	bi, _ := parseID(b.ID)
	return ai > bi
}

func parseID(id string) (int, bool) {
	nStr, ok := strings.CutPrefix(id, "D-")
	if !ok || nStr == "" {
		return 0, false
	}
	n, err := strconv.Atoi(nStr)
	if err != nil || n < 1 {
		return 0, false
	}
	return n, true
}

func validate(item *Item) error {
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
	if src.Deadline != nil {
		t := *src.Deadline
		item.Deadline = &t
	}
	if src.Answer != nil {
		a := *src.Answer
		item.Answer = &a
	}
	if src.Messages != nil {
		item.Messages = append([]Message(nil), src.Messages...)
		for i := range item.Messages {
			if src.Messages[i].Options != nil {
				item.Messages[i].Options = append([]Option(nil), src.Messages[i].Options...)
			}
		}
	}
	return item
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
	for i := range item.Messages {
		item.Messages[i].Time = item.Messages[i].Time.UTC()
	}
}

// ensureMessages fills a thread from the stored ask and answer.
// A thread that already has messages is left as stored.
func ensureMessages(it *Item) {
	if len(it.Messages) > 0 {
		return
	}
	msg := Message{
		Author: it.Owner,
		Time:   it.CreatedAt.UTC(),
		Text:   it.Question,
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
