// Package mailbox stores agentbus messages and stamps the sender.
// The sender cannot choose from. Names do not expire.
package mailbox

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

const (
	mailboxFile = "messages.json"
	// defaultInline is the builder-chosen paste limit, in bytes.
	defaultInline = 4096
	// previewLen is the builder-chosen preview size, in bytes.
	previewLen = 64
	timeLayout = "2006-01-02T15:04:05.000Z07:00"
)

// Result is the id and delivery state of a send or reply.
type Result struct {
	ID    string
	State string
}

// Options controls one send. A zero TTL does not expire.
// Wait blocks until the message is delivered or expired.
type Options struct {
	TTL  time.Duration
	Wait bool
}

type message struct {
	ID          string     `json:"id"`
	From        string     `json:"from"`
	To          string     `json:"to"`
	Re          string     `json:"re,omitempty"`
	SentAt      time.Time  `json:"sentAt"`
	Body        string     `json:"body"`
	Replyable   bool       `json:"replyable"`
	State       string     `json:"state"`
	Handle      string     `json:"handle"`
	Deadline    *time.Time `json:"deadline,omitempty"`
	DeliveredAt *time.Time `json:"deliveredAt,omitempty"`
	ClosedAt    *time.Time `json:"closedAt,omitempty"`
}

type file struct {
	Messages []message `json:"messages"`
}

// Mailbox persists messages in the daemon state directory.
// The queue is FIFO per resolved handle. A held front blocks later
// messages to that handle.
type Mailbox struct {
	dir      string
	reg      *registry.Registry
	log      *eventlog.Log
	inline   int
	now      func() time.Time
	mu       sync.Mutex
	cv       *sync.Cond
	msgs     map[string]message
	queue    map[string][]string
	inflight map[string]bool
	adapters []adapter.Adapter
	broken   map[string]string
	prep     sync.Once
	prepErr  error
}

// Open loads messages. A missing file is an empty mailbox.
// inline <= 0 uses defaultInline.
func Open(dir string, reg *registry.Registry, adapters []adapter.Adapter, log *eventlog.Log, inline int) (*Mailbox, error) {
	if inline <= 0 {
		inline = defaultInline
	}
	msgs, err := load(dir)
	if err != nil {
		return nil, err
	}
	box := &Mailbox{
		dir:      dir,
		reg:      reg,
		log:      log,
		inline:   inline,
		now:      time.Now,
		msgs:     msgs,
		queue:    rebuild(msgs),
		inflight: map[string]bool{},
		adapters: adapters,
	}
	box.cv = sync.NewCond(&box.mu)
	return box, nil
}

// Send stores a message and queues it for the resolved handle.
// target is a logical name or a raw handle. from is stamped here.
func (m *Mailbox) Send(ctx context.Context, env []string, peerPID int, target, body string, opt Options) (Result, error) {
	if body == "" {
		return Result{}, errors.New("message body is required")
	}
	from, replyable := m.stamp(ctx, env, peerPID)
	handle, err := m.reg.Resolve(target)
	if err != nil {
		if logErr := m.logEvent(eventlog.Event{
			Event:  eventlog.EventRejected,
			Actor:  from,
			Name:   target,
			Detail: "unknown target",
			Reason: err.Error(),
		}); logErr != nil {
			return Result{}, logErr
		}
		return Result{}, fmt.Errorf("resolve target: %w", err)
	}
	msg, err := m.newMessage(from, target, "", body, replyable, handle)
	if err != nil {
		return Result{}, err
	}
	return m.enqueue(ctx, msg, opt)
}

// Reply sends to the original from and sets re to id.
// A message with no reply address is refused.
func (m *Mailbox) Reply(ctx context.Context, env []string, peerPID int, id, body string) (Result, error) {
	if body == "" {
		return Result{}, errors.New("message body is required")
	}
	orig, err := m.get(id)
	if err != nil {
		return Result{}, err
	}
	if !orig.Replyable {
		return Result{}, errors.New("no reply address")
	}
	from, replyable := m.stamp(ctx, env, peerPID)
	handle, err := m.reg.Resolve(orig.From)
	if err != nil {
		return Result{}, fmt.Errorf("resolve reply target: %w", err)
	}
	msg, err := m.newMessage(from, orig.From, id, body, replyable, handle)
	if err != nil {
		return Result{}, err
	}
	return m.enqueue(ctx, msg, Options{})
}

func (m *Mailbox) newMessage(from, to, re, body string, replyable bool, handle string) (message, error) {
	id, err := newID()
	if err != nil {
		return message{}, err
	}
	return message{
		ID:        id,
		From:      from,
		To:        to,
		Re:        re,
		SentAt:    m.now().UTC(),
		Body:      body,
		Replyable: replyable,
		Handle:    handle,
	}, nil
}

func (m *Mailbox) preview(body string) bool {
	return len(body) > m.inline || unsafeBody(body)
}

// stamp reads the caller from env. A herdr pane uses its registered
// name when it has one, otherwise its raw handle. Anyone else is
// anon:<host-pid> and is not replyable.
func (m *Mailbox) stamp(ctx context.Context, env []string, peerPID int) (string, bool) {
	for _, ad := range m.adapters {
		handle, err := ad.Identify(ctx, env)
		if err != nil || !strings.HasPrefix(handle, "herdr:") {
			continue
		}
		names := m.reg.NamesOf(handle)
		if len(names) == 0 {
			return handle, true
		}
		return strings.Join(names, ", "), true
	}
	return anon(peerPID), false
}

func anon(pid int) string {
	host, err := os.Hostname()
	if err != nil || strings.TrimSpace(host) == "" {
		host = "unknown"
	}
	return fmt.Sprintf("anon:%s-%d", host, pid)
}

func newID() (string, error) {
	var buf [4]byte
	if _, err := rand.Read(buf[:]); err != nil {
		return "", fmt.Errorf("message id: %w", err)
	}
	return "m-" + hex.EncodeToString(buf[:]), nil
}

func (m *Mailbox) get(id string) (message, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	msg, ok := m.msgs[id]
	if !ok {
		return message{}, fmt.Errorf("unknown message %s", id)
	}
	return msg, nil
}

func (m *Mailbox) logEvent(e eventlog.Event) error {
	if m.log == nil {
		return nil
	}
	if err := m.log.Append(e); err != nil {
		return fmt.Errorf("log %s: %w", e.Event, err)
	}
	return nil
}

func load(dir string) (map[string]message, error) {
	data, err := os.ReadFile(filepath.Join(dir, mailboxFile))
	if errors.Is(err, os.ErrNotExist) {
		return map[string]message{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read messages: %w", err)
	}
	var doc file
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("decode messages: %w", err)
	}
	out := make(map[string]message, len(doc.Messages))
	for _, msg := range doc.Messages {
		out[msg.ID] = msg
	}
	return out, nil
}

func (m *Mailbox) save() error {
	doc := file{Messages: make([]message, 0, len(m.msgs))}
	for _, msg := range m.msgs {
		doc.Messages = append(doc.Messages, msg)
	}
	sort.Slice(doc.Messages, func(i, j int) bool {
		if !doc.Messages[i].SentAt.Equal(doc.Messages[j].SentAt) {
			return doc.Messages[i].SentAt.Before(doc.Messages[j].SentAt)
		}
		return doc.Messages[i].ID < doc.Messages[j].ID
	})
	data, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode messages: %w", err)
	}
	data = append(data, '\n')
	tmp := filepath.Join(m.dir, ".messages.json.tmp")
	if err := os.WriteFile(tmp, data, 0o600); err != nil {
		return fmt.Errorf("write messages: %w", err)
	}
	if err := os.Rename(tmp, filepath.Join(m.dir, mailboxFile)); err != nil {
		return fmt.Errorf("replace messages: %w", err)
	}
	return nil
}
