// Package outbox stores hub-mode sends on the client until the hub accepts them.
package outbox

import (
	"crypto/rand"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"

	"golang.org/x/sys/unix"
)

const (
	// KindSend is a queued send.
	KindSend = "send"
	// KindReply is a queued reply.
	KindReply = "reply"
	// KindBypass is a queued SendMessage bypass record.
	KindBypass = "bypass"

	// StatePending is still waiting for the hub.
	StatePending = "pending"
	// StateDelivered is a hub accept, kept so status can name the message.
	StateDelivered = "delivered"
	// StateExpired was dropped because its ttl elapsed.
	StateExpired = "expired"

	// StaleAfter is how long a pending entry waits before doctor and ls flag it.
	StaleAfter = 24 * time.Hour
)

// Entry is one queued message. The file is the durable copy.
type Entry struct {
	Kind       string    `json:"kind"`
	Args       []string  `json:"args,omitempty"`
	Stdin      string    `json:"stdin,omitempty"`
	Handle     string    `json:"handle,omitempty"`
	ClientID   string    `json:"clientId"`
	EnqueuedAt time.Time `json:"enqueuedAt"`
	Attempts   int       `json:"attempts"`
	TTL        string    `json:"ttl,omitempty"`
	LastError  string    `json:"lastError,omitempty"`
	NextAt     time.Time `json:"nextAt,omitempty"`
	State      string    `json:"state"`
	MsgID      string    `json:"msgId,omitempty"`
	HubState   string    `json:"hubState,omitempty"`
	Target     string    `json:"target,omitempty"`
	Body       string    `json:"body,omitempty"`
	Session    string    `json:"session,omitempty"`
}

// LogEvent is one client log line waiting to be appended on the hub.
type LogEvent struct {
	Time        time.Time `json:"time"`
	Event       string    `json:"event"`
	Actor       string    `json:"actor"`
	Name        string    `json:"name,omitempty"`
	MsgID       string    `json:"msg,omitempty"`
	Detail      string    `json:"detail,omitempty"`
	Reason      string    `json:"reason,omitempty"`
	TargetState string    `json:"targetState,omitempty"`
	Resolved    string    `json:"resolved,omitempty"`
}

// Box is the outbox under a state directory.
type Box struct {
	Dir string
}

// Open returns the outbox rooted at stateDir/outbox.
func Open(stateDir string) Box {
	return Box{Dir: filepath.Join(stateDir, "outbox")}
}

func (b Box) pendingDir() string { return filepath.Join(b.Dir, "pending") }
func (b Box) doneDir() string    { return filepath.Join(b.Dir, "done") }
func (b Box) spoolDir() string   { return filepath.Join(b.Dir, "spool") }

// Enqueue writes e as a new pending file. A client id already pending is
// left as it is and returned.
func (b Box) Enqueue(e Entry) (Entry, error) {
	if e.ClientID == "" {
		return Entry{}, errors.New("outbox entry needs a client id")
	}
	if e.State == "" {
		e.State = StatePending
	}
	if err := os.MkdirAll(b.pendingDir(), 0o700); err != nil {
		return Entry{}, fmt.Errorf("create outbox: %w", err)
	}
	if prev, ok, err := b.read(b.pendingDir(), e.ClientID); err != nil {
		return Entry{}, err
	} else if ok {
		return prev, nil
	}
	if err := writeAtomic(b.pendingDir(), fileName(e.ClientID), e); err != nil {
		return Entry{}, err
	}
	return e, nil
}

// Put replaces the pending file for e.ClientID.
func (b Box) Put(e Entry) error {
	if err := os.MkdirAll(b.pendingDir(), 0o700); err != nil {
		return fmt.Errorf("create outbox: %w", err)
	}
	if err := writeAtomic(b.pendingDir(), fileName(e.ClientID), e); err != nil {
		return err
	}
	return nil
}

// Finish removes a pending entry and keeps it under done for status.
func (b Box) Finish(e Entry) error {
	if err := os.MkdirAll(b.doneDir(), 0o700); err != nil {
		return fmt.Errorf("create outbox: %w", err)
	}
	if err := writeAtomic(b.doneDir(), fileName(e.ClientID), e); err != nil {
		return err
	}
	if err := os.Remove(filepath.Join(b.pendingDir(), fileName(e.ClientID))); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("remove pending entry: %w", err)
	}
	return nil
}

// Drop removes a pending entry and does not keep a done record.
func (b Box) Drop(clientID string) error {
	err := os.Remove(filepath.Join(b.pendingDir(), fileName(clientID)))
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("remove pending entry: %w", err)
	}
	return nil
}

// Get returns the pending entry, or else the done record.
func (b Box) Get(clientID string) (Entry, bool, error) {
	if e, ok, err := b.read(b.pendingDir(), clientID); err != nil || ok {
		return e, ok, err
	}
	return b.read(b.doneDir(), clientID)
}

// Pending returns pending entries in enqueue order.
func (b Box) Pending() ([]Entry, error) {
	return readAll(b.pendingDir())
}

// PendingHandle returns one sender's pending entries in enqueue order.
func (b Box) PendingHandle(handle string) ([]Entry, error) {
	all, err := b.Pending()
	if err != nil {
		return nil, err
	}
	var out []Entry
	for _, e := range all {
		if e.Handle == handle {
			out = append(out, e)
		}
	}
	return out, nil
}

// HasWork reports whether a flush has a pending message or a spooled log line.
func (b Box) HasWork() (bool, error) {
	n, err := countFiles(b.pendingDir())
	if err != nil || n > 0 {
		return n > 0, err
	}
	n, err = countFiles(b.spoolDir())
	return n > 0, err
}

// Spool appends one log line to forward on the next flush.
func (b Box) Spool(ev LogEvent) error {
	if err := os.MkdirAll(b.spoolDir(), 0o700); err != nil {
		return fmt.Errorf("create outbox spool: %w", err)
	}
	name, err := spoolName()
	if err != nil {
		return err
	}
	return writeAtomic(b.spoolDir(), name, ev)
}

// SpoolEvents returns spooled log lines in write order.
func (b Box) SpoolEvents() ([]LogEvent, error) {
	names, err := jsonNames(b.spoolDir())
	if err != nil {
		return nil, err
	}
	slices.Sort(names)
	out := make([]LogEvent, 0, len(names))
	for _, name := range names {
		var ev LogEvent
		if err := readJSON(filepath.Join(b.spoolDir(), name), &ev); err != nil {
			return nil, err
		}
		out = append(out, ev)
	}
	return out, nil
}

// ClearSpool removes the spooled log lines.
func (b Box) ClearSpool() error {
	names, err := jsonNames(b.spoolDir())
	if err != nil {
		return err
	}
	for _, name := range names {
		if err := os.Remove(filepath.Join(b.spoolDir(), name)); err != nil && !errors.Is(err, os.ErrNotExist) {
			return fmt.Errorf("remove spooled log: %w", err)
		}
	}
	return nil
}

// Stale reports a pending entry that has waited longer than StaleAfter.
// It is not deleted.
func Stale(e Entry, now time.Time) bool {
	return e.State == StatePending && now.Sub(e.EnqueuedAt) > StaleAfter
}

// StatusLine is the text of `agentbus status` for a local outbox entry.
func StatusLine(e Entry, now time.Time) string {
	switch e.State {
	case StateDelivered:
		return "delivered → " + e.MsgID
	case StateExpired:
		return "expired"
	default:
		if e.Attempts > 0 && e.LastError != "" {
			wait := e.NextAt.Sub(now)
			if wait < 0 {
				wait = 0
			}
			return fmt.Sprintf("retrying (attempt %d, next in %s, last error %s)", e.Attempts, wait.Round(time.Second), e.LastError)
		}
		return "queued"
	}
}

// flock is per process. held tracks directories this process already locks
// so a second flush in-process skips the way a second process does.
var (
	heldMu sync.Mutex
	held   = map[string]struct{}{}
)

// TryLock takes a non-blocking exclusive flock on the outbox directory.
// ok is false when another flusher holds it. The caller skips in that case.
func (b Box) TryLock() (func(), bool, error) {
	if err := os.MkdirAll(b.Dir, 0o700); err != nil {
		return nil, false, fmt.Errorf("create outbox: %w", err)
	}
	heldMu.Lock()
	if _, ok := held[b.Dir]; ok {
		heldMu.Unlock()
		return nil, false, nil
	}
	held[b.Dir] = struct{}{}
	heldMu.Unlock()

	f, err := os.Open(b.Dir) //nolint:gosec // the outbox directory under the state dir
	if err != nil {
		unhold(b.Dir)
		return nil, false, fmt.Errorf("open outbox: %w", err)
	}
	got, err := tryLock(f)
	if err != nil || !got {
		_ = f.Close()
		unhold(b.Dir)
		return nil, false, err
	}
	return func() {
		_ = f.Close()
		unhold(b.Dir)
	}, true, nil
}

func unhold(dir string) {
	heldMu.Lock()
	delete(held, dir)
	heldMu.Unlock()
}

func tryLock(f *os.File) (bool, error) {
	for {
		err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB) //nolint:gosec // G115: a file descriptor fits in int
		switch {
		case err == nil:
			return true, nil
		case errors.Is(err, unix.EWOULDBLOCK) || errors.Is(err, unix.EAGAIN):
			return false, nil
		case errors.Is(err, unix.EINTR):
			continue
		default:
			return false, fmt.Errorf("flock %s: %w", f.Name(), err)
		}
	}
}

func (b Box) read(dir, clientID string) (Entry, bool, error) {
	var e Entry
	err := readJSON(filepath.Join(dir, fileName(clientID)), &e)
	if errors.Is(err, os.ErrNotExist) {
		return Entry{}, false, nil
	}
	if err != nil {
		return Entry{}, false, err
	}
	return e, true, nil
}

func readAll(dir string) ([]Entry, error) {
	names, err := jsonNames(dir)
	if err != nil {
		return nil, err
	}
	out := make([]Entry, 0, len(names))
	for _, name := range names {
		var e Entry
		if err := readJSON(filepath.Join(dir, name), &e); err != nil {
			return nil, err
		}
		out = append(out, e)
	}
	slices.SortFunc(out, func(a, c Entry) int {
		if a.EnqueuedAt.Before(c.EnqueuedAt) {
			return -1
		}
		if c.EnqueuedAt.Before(a.EnqueuedAt) {
			return 1
		}
		return strings.Compare(a.ClientID, c.ClientID)
	})
	return out, nil
}

func countFiles(dir string) (int, error) {
	names, err := jsonNames(dir)
	return len(names), err
}

func jsonNames(dir string) ([]string, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil
		}
		return nil, fmt.Errorf("read outbox: %w", err)
	}
	var names []string
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".json") {
			continue
		}
		names = append(names, entry.Name())
	}
	return names, nil
}

func fileName(clientID string) string {
	return strings.ReplaceAll(clientID, ":", "_") + ".json"
}

func spoolName() (string, error) {
	var buf [8]byte
	if _, err := rand.Read(buf[:]); err != nil {
		return "", fmt.Errorf("name spooled log: %w", err)
	}
	return fmt.Sprintf("%d-%x.json", time.Now().UnixNano(), buf), nil
}

func writeAtomic(dir, name string, doc any) error {
	data, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode outbox entry: %w", err)
	}
	data = append(data, '\n')
	tmp, err := os.CreateTemp(dir, ".tmp-*")
	if err != nil {
		return fmt.Errorf("write outbox: %w", err)
	}
	tmpName := tmp.Name()
	defer os.Remove(tmpName)
	if _, err := tmp.Write(data); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("write outbox: %w", err)
	}
	if err := tmp.Sync(); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("write outbox: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("write outbox: %w", err)
	}
	if err := os.Rename(tmpName, filepath.Join(dir, name)); err != nil {
		return fmt.Errorf("write outbox: %w", err)
	}
	d, err := os.Open(dir) //nolint:gosec // the outbox directory under the state dir
	if err != nil {
		return fmt.Errorf("write outbox: %w", err)
	}
	err = d.Sync()
	_ = d.Close()
	if err != nil {
		return fmt.Errorf("write outbox: %w", err)
	}
	return nil
}

func readJSON(path string, dest any) error {
	f, err := os.Open(path) //nolint:gosec // a file this box wrote under the state dir
	if err != nil {
		return fmt.Errorf("read outbox: %w", err)
	}
	defer f.Close()
	if err := json.NewDecoder(f).Decode(dest); err != nil && !errors.Is(err, io.EOF) {
		return fmt.Errorf("read outbox: %w", err)
	}
	return nil
}
