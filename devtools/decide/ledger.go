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

// Answer is set only when an item has been answered.
type Answer struct {
	Value      string `json:"value"`
	AnsweredBy string `json:"answeredBy"`
}

// Item is one stored decision.
type Item struct {
	ID          string     `json:"id"`
	Question    string     `json:"question"`
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

// List returns every stored item.
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

// Withdraw marks id withdrawn and keeps the record. The id is not reused.
func (l *Ledger) Withdraw(id string) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		for i := range st.Items {
			if st.Items[i].ID != id {
				continue
			}
			st.Items[i].State = StateWithdrawn
			st.Items[i].WithdrawnAt = l.clock().UTC()
			updated = st.Items[i]
			return nil
		}
		return fmt.Errorf("%w: %s", ErrNotFound, id)
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
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
	recommended := 0
	for _, opt := range item.Options {
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
}
