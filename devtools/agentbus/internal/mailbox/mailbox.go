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
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lastturn"
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
// Duplicate is set when this client id was already accepted.
// Unreachable is set when the message is stored but the transport cannot
// be reached. AnonHerdr is set when a herdr pane was stamped anon because
// herdr could not identify it.
type Result struct {
	ID          string
	State       string
	Duplicate   bool
	Unreachable bool
	AnonHerdr   bool
}

// Options controls one send. A zero TTL does not expire.
// Wait blocks until the message is delivered or expired.
// ClientID empty keeps the old accept-every-call behavior.
// ReplyFirst answers after the precheck and leaves the paste until
// DeliverDeferred. Wait ignores it and blocks until the message settles.
type Options struct {
	TTL         time.Duration
	Wait        bool
	ClientID    string
	ReplyFirst  bool
	ExpectReply bool
}

type message struct {
	ID   string `json:"id"`
	From string `json:"from"`
	// FromNames are the sender's names when it was stamped. From is
	// their display join; reply targets come from this list.
	FromNames   []string  `json:"fromNames,omitempty"`
	To          string    `json:"to"`
	Re          string    `json:"re,omitempty"`
	SentAt      time.Time `json:"sentAt"`
	Body        string    `json:"body"`
	Replyable   bool      `json:"replyable"`
	ExpectReply bool      `json:"expectReply,omitempty"`
	State       string    `json:"state"`
	Handle      string    `json:"handle"`
	// ClientID is the caller's retry key. Empty means the send was not idempotent.
	ClientID string `json:"clientId,omitempty"`
	// SenderHandle is the stable caller handle. From is only the display name.
	SenderHandle string     `json:"senderHandle,omitempty"`
	Deadline     *time.Time `json:"deadline,omitempty"`
	HeldAt       *time.Time `json:"heldAt,omitempty"`
	DeliveredAt  *time.Time `json:"deliveredAt,omitempty"`
	ClosedAt     *time.Time `json:"closedAt,omitempty"`
	// HoldNotice is set once a long-hold notice was claimed for this message.
	HoldNotice bool `json:"holdNoticeSent,omitempty"`
	// GoneSince is when every adapter began to report a raw handle
	// target gone. Any other answer clears it.
	GoneSince *time.Time `json:"goneSince,omitempty"`
	// ExpireReason is why an expired message closed: ExpireTTL or ExpireGone.
	ExpireReason string `json:"expireReason,omitempty"`
	// UnprocessedReason is why a delivered message was marked
	// delivered-unprocessed: "auth" or "no-turn".
	UnprocessedReason string `json:"unprocessedReason,omitempty"`
	// ResendOf is the original message id when this message is a replay.
	ResendOf string `json:"resendOf,omitempty"`
	// ReplayedAt is when this delivered-unprocessed message was replayed.
	ReplayedAt *time.Time `json:"replayedAt,omitempty"`
	// VerifyPending is true while post-delivery transcript verification is running.
	VerifyPending      bool       `json:"verifyPending,omitempty"`
	VerifyDeadline     *time.Time `json:"verifyDeadline,omitempty"`
	VerifyStatus       string     `json:"verifyStatus,omitempty"`
	VerifyStateSeq     int64      `json:"verifyStateSeq,omitempty"`
	VerifyStateChanged bool       `json:"verifyStateChanged,omitempty"`
	// ComposerText / ComposerTextSince / ComposerTabLabel track unchanged
	// composer text during HoldDraft holds.
	ComposerText        string     `json:"composerText,omitempty"`
	ComposerTextSince   *time.Time `json:"composerTextSince,omitempty"`
	ComposerTabLabel    string     `json:"composerTabLabel,omitempty"`
	StaleComposerNotice bool       `json:"staleComposerNoticeSent,omitempty"`
}

type authOutageRecord struct {
	Since         time.Time       `json:"since"`
	TurnsAtOutage int             `json:"turnsAtOutage"`
	Noticed       map[string]bool `json:"noticed,omitempty"`
	Failures      int             `json:"failures,omitempty"`
	RetryAt       time.Time       `json:"retryAt,omitempty"`
	ProbeMsgID    string          `json:"probeMsgId,omitempty"`
}

type file struct {
	Messages   []message                   `json:"messages"`
	AuthOutage map[string]authOutageRecord `json:"authOutage,omitempty"`
}

// Mailbox persists messages in the daemon state directory.
// The queue is FIFO per resolved handle. A held front blocks later
// messages to that handle.
type Mailbox struct {
	dir    string
	reg    *registry.Registry
	log    *eventlog.Log
	inline int
	now    func() time.Time
	mu     sync.Mutex
	cv     *sync.Cond
	msgs   map[string]message
	// byClient maps a sender handle and client id to the message id.
	// The entry is stored on the message, so it lasts as long as that
	// message is retained. Messages are not pruned, which is past the
	// 7-day floor for a client id whose message is already gone.
	byClient map[dedupeKey]string
	queue    map[string][]string
	inflight map[string]bool
	flushing map[string]bool
	adapters []adapter.Adapter
	// waker relaunches hibernated agents; nil holds their messages.
	waker           Waker
	wakeModel       string
	wakeModelByName []config.WakeModelRule
	// waking is the wake in flight per handle, and wakes every wake
	// goroutine.
	waking     map[string]*wakeCall
	wakes      sync.WaitGroup
	deliveries sync.WaitGroup
	// deferred handles are pasted by DeliverDeferred, after the reply.
	deferred []string
	// holdPaste keeps a ReplyFirst message in the queue until replyFirst
	// has snapshotted its answer. A concurrent flush does not paste it.
	holdPaste map[string]bool
	// pasting is set while Deliver is inside the adapter, so a send can
	// return once a slow paste is underway.
	pasting map[string]bool
	// pasteBound limits one paste after the caller context ends.
	// Zero uses defaultPasteBound.
	pasteBound time.Duration
	// noticeAfter is how long a hold waits before one sender notice.
	// Zero with noticeOff false uses defaultHoldNotice. noticeOff sends none.
	noticeAfter time.Duration
	noticeOff   bool
	// goneAfter is how long a raw handle target stays gone before its
	// message expires. Nil uses defaultGoneAfter.
	goneAfter *time.Duration
	// transcript reads an agent's transcript turns for post-delivery verification.
	transcript func(adapter.Agent) ([]lastturn.Turn, bool)
	// verifyWithin bounds post-delivery verification. Zero with verifyOff false
	// uses defaultVerifyWithin. verifyOff disables verification.
	verifyWithin time.Duration
	verifyOff    bool
	// authPatterns identify an assistant auth-error reply (substring-matched
	// on synthetic/API-error turns, whole-turn matched otherwise). Nil uses
	// DefaultAuthErrorPatterns.
	authPatterns []string
	// staleComposerAfter is how long unchanged composer text holds a message
	// before notifying the sender once per tab.
	staleComposerAfter time.Duration
	staleComposerOff   bool
	// hibernateWarn is how recent a delivery must be for hibernate to
	// warn. Nil uses defaultHibernateWarn; zero turns the warning off.
	hibernateWarn *time.Duration
	// authOutage tracks handles currently held due to an auth error.
	authOutage map[string]authOutageRecord
	// staleComposerNoticed tracks handle -> composerText already noticed.
	staleComposerNoticed map[string]string
	// expectReplySenders are sender name/handle globs whose delivered
	// messages expect a reply unless already replied to.
	expectReplySenders []string
}

// Open loads messages. A missing file is an empty mailbox.
// inline <= 0 uses defaultInline.
func Open(dir string, reg *registry.Registry, adapters []adapter.Adapter, log *eventlog.Log, inline int) (*Mailbox, error) {
	if inline <= 0 {
		inline = defaultInline
	}
	msgs, outage, err := loadWithOutage(dir)
	if err != nil {
		return nil, err
	}
	if outage == nil {
		outage = map[string]authOutageRecord{}
	}
	box := &Mailbox{
		dir:                  dir,
		reg:                  reg,
		log:                  log,
		inline:               inline,
		now:                  time.Now,
		msgs:                 msgs,
		byClient:             indexClients(msgs),
		queue:                rebuild(msgs),
		inflight:             map[string]bool{},
		flushing:             map[string]bool{},
		holdPaste:            map[string]bool{},
		pasting:              map[string]bool{},
		adapters:             adapters,
		waking:               map[string]*wakeCall{},
		authOutage:           outage,
		staleComposerNoticed: map[string]string{},
	}
	box.cv = sync.NewCond(&box.mu)
	if reg != nil {
		reg.OnHolderChange = box.MoveName
	}
	return box, nil
}

// SetNow overrides the clock the mailbox reads. Nil restores time.Now.
func (m *Mailbox) SetNow(fn func() time.Time) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if fn == nil {
		m.now = time.Now
		return
	}
	m.now = fn
}

// Send stores a message and queues it for the resolved handle.
// target is a logical name or a raw handle. from is stamped here.
func (m *Mailbox) Send(ctx context.Context, env []string, peerPID int, target, body string, opt Options) (Result, error) {
	if body == "" {
		return Result{}, errors.New("message body is required")
	}
	from, sender, names, replyable, anonHerdr := m.stamp(ctx, env, peerPID)
	return m.send(ctx, from, sender, names, replyable, anonHerdr, target, body, opt)
}

// Post queues a message the daemon itself sends, such as a lease grant.
// from names the sender; the message has no reply address.
func (m *Mailbox) Post(ctx context.Context, from, target, body string) (Result, error) {
	if body == "" {
		return Result{}, errors.New("message body is required")
	}
	return m.send(ctx, from, "", nil, false, false, target, body, Options{})
}

func (m *Mailbox) send(ctx context.Context, from, sender string, names []string, replyable, anonHerdr bool, target, body string, opt Options) (Result, error) {
	if err := ValidClientID(opt.ClientID); err != nil {
		return Result{}, err
	}
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
		return Result{}, fmt.Errorf("resolve target: %w", unknownTarget{err})
	}
	msg, err := m.newMessage(from, target, "", body, replyable, handle)
	if err != nil {
		return Result{}, err
	}
	msg.ClientID = opt.ClientID
	msg.SenderHandle = sender
	msg.FromNames = names
	msg.ExpectReply = opt.ExpectReply
	res, err := m.enqueue(ctx, msg, opt)
	if err != nil {
		return Result{}, err
	}
	res.AnonHerdr = anonHerdr
	return res, nil
}

// Reply sends to the original from and sets re to id.
// A message with no reply address is refused.
func (m *Mailbox) Reply(ctx context.Context, env []string, peerPID int, id, body string, opt Options) (Result, error) {
	if body == "" {
		return Result{}, errors.New("message body is required")
	}
	if err := ValidClientID(opt.ClientID); err != nil {
		return Result{}, err
	}
	orig, err := m.get(id)
	if err != nil {
		return Result{}, err
	}
	if !orig.Replyable {
		return Result{}, errors.New("no reply address")
	}
	from, sender, names, replyable, anonHerdr := m.stamp(ctx, env, peerPID)
	to, handle, err := m.replyTarget(ctx, orig)
	if err != nil {
		return Result{}, fmt.Errorf("resolve reply target: %w", unknownTarget{err})
	}
	msg, err := m.newMessage(from, to, id, body, replyable, handle)
	if err != nil {
		return Result{}, err
	}
	msg.ClientID = opt.ClientID
	msg.SenderHandle = sender
	msg.FromNames = names
	msg.ExpectReply = opt.ExpectReply
	res, err := m.enqueue(ctx, msg, opt)
	if err != nil {
		return Result{}, err
	}
	res.AnonHerdr = anonHerdr
	return res, nil
}

// replyTarget picks where a reply to orig goes: the first of the
// sender's names that resolves, else the sender's handle when it is
// alive. It returns the name (or handle) to use as To, and the handle.
// Messages stored before FromNames existed split the joined From.
func (m *Mailbox) replyTarget(ctx context.Context, orig message) (string, string, error) {
	names := orig.FromNames
	if len(names) == 0 {
		names = strings.Split(orig.From, ", ")
	}
	for _, name := range names {
		if handle, err := m.reg.Resolve(name); err == nil {
			return name, handle, nil
		}
	}
	if h := orig.SenderHandle; h != "" {
		state, _, _, err := m.observe(ctx, m.adapters, h)
		if err != nil || state != adapter.StateGone {
			return h, h, nil
		}
	}
	return "", "", fmt.Errorf("tried %s: none is registered and the sender handle is gone",
		strings.Join(names, ", "))
}

// unknownTarget is a resolve failure. It keeps the resolver's text and
// matches ErrUnknownTarget.
type unknownTarget struct{ err error }

func (e unknownTarget) Error() string { return e.err.Error() }

func (e unknownTarget) Unwrap() error { return e.err }

func (e unknownTarget) Is(target error) bool { return target == ErrUnknownTarget }

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
// stamp returns the display name, the stable sender handle, and whether
// the caller can be replied to. The dedupe key uses the handle, because
// the display name changes when the pane's names change.
func (m *Mailbox) stamp(ctx context.Context, env []string, peerPID int) (string, string, []string, bool, bool) {
	for _, ad := range m.adapters {
		handle, err := ad.Identify(ctx, env)
		if err != nil || handle == "" {
			continue
		}
		// A replayed anon handle carries the original pid. Recomputing
		// it from this process would miss the client-id dedupe.
		if strings.HasPrefix(handle, "anon:") {
			return handle, handle, nil, false, false
		}
		if !strings.HasPrefix(handle, "herdr:") {
			continue
		}
		names := m.reg.NamesOf(handle)
		if len(names) == 0 {
			return handle, handle, nil, true, false
		}
		return strings.Join(names, ", "), handle, names, true, false
	}
	who := anon(peerPID)
	return who, who, nil, false, m.anonBecauseHerdr(ctx, env)
}

// anonBecauseHerdr reports a herdr pane whose Identify failed because
// herdr itself could not be asked. A pane herdr can list is not this case.
func (m *Mailbox) anonBecauseHerdr(ctx context.Context, env []string) bool {
	if envValue(env, "HERDR_PANE_ID") == "" {
		return false
	}
	for _, ad := range m.adapters {
		if ad.Name() != "herdr" {
			continue
		}
		if err := ad.SelfTest(ctx); err != nil {
			return true
		}
	}
	return false
}

func envValue(env []string, key string) string {
	prefix := key + "="
	for _, entry := range env {
		if value, ok := strings.CutPrefix(entry, prefix); ok {
			return value
		}
	}
	return ""
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
	return m.lookupLocked(id)
}

// lookupLocked finds id as a message id, or as a client id when it
// contains a colon. The caller holds m.mu.
func (m *Mailbox) lookupLocked(id string) (message, error) {
	if msg, ok := m.msgs[id]; ok {
		return msg, nil
	}
	if !strings.Contains(id, ":") {
		return message{}, fmt.Errorf("%w %s", errUnknownMessage, id)
	}
	var found message
	n := 0
	for _, msg := range m.msgs {
		if msg.ClientID != id {
			continue
		}
		n++
		found = msg
	}
	switch n {
	case 1:
		return found, nil
	case 0:
		return message{}, fmt.Errorf("%w %s", errUnknownMessage, id)
	default:
		return message{}, fmt.Errorf("client id %s matches more than one message", id)
	}
}

// errUnknownMessage is the lookup error for an id no message has.
var errUnknownMessage = errors.New("unknown message")

type dedupeKey struct {
	sender   string
	clientID string
}

func indexClients(msgs map[string]message) map[dedupeKey]string {
	out := make(map[dedupeKey]string, len(msgs))
	for _, msg := range msgs {
		if msg.ClientID == "" || msg.SenderHandle == "" {
			continue
		}
		out[dedupeKey{sender: msg.SenderHandle, clientID: msg.ClientID}] = msg.ID
	}
	return out
}

// ValidClientID checks a client-generated retry key.
// Empty is valid and means the call is not idempotent.
// A set id is at most 64 characters from [A-Za-z0-9._:-] and contains a colon.
func ValidClientID(id string) error {
	if id == "" {
		return nil
	}
	if len(id) > 64 || !strings.Contains(id, ":") || !clientIDChars(id) {
		return fmt.Errorf("client id %q must be at most 64 characters from [A-Za-z0-9._:-] and contain a colon", id)
	}
	return nil
}

func clientIDChars(id string) bool {
	for _, r := range id {
		switch {
		case r >= 'A' && r <= 'Z', r >= 'a' && r <= 'z', r >= '0' && r <= '9':
		case r == '.' || r == '_' || r == ':' || r == '-':
		default:
			return false
		}
	}
	return true
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

// UnverifiedHolds lists messages whose paste may have landed and whose
// screen could not be checked. Each line is
// "ambiguous-unverified: <id> to <to>". A missing file is empty.
func UnverifiedHolds(dir string) ([]string, error) {
	msgs, err := load(dir)
	if err != nil {
		return nil, err
	}
	want := "held: " + adapter.HoldAmbiguousUnverified
	pending := make([]message, 0)
	for _, msg := range msgs {
		if msg.State == want {
			pending = append(pending, msg)
		}
	}
	sort.Slice(pending, func(i, j int) bool {
		if !pending[i].SentAt.Equal(pending[j].SentAt) {
			return pending[i].SentAt.Before(pending[j].SentAt)
		}
		return pending[i].ID < pending[j].ID
	})
	lines := make([]string, 0, len(pending))
	for _, msg := range pending {
		lines = append(lines, fmt.Sprintf("ambiguous-unverified: %s to %s", msg.ID, msg.To))
	}
	return lines, nil
}

func load(dir string) (map[string]message, error) {
	msgs, _, err := loadWithOutage(dir)
	return msgs, err
}

func loadWithOutage(dir string) (map[string]message, map[string]authOutageRecord, error) {
	data, err := os.ReadFile(filepath.Join(dir, mailboxFile))
	if errors.Is(err, os.ErrNotExist) {
		return map[string]message{}, nil, nil
	}
	if err != nil {
		return nil, nil, fmt.Errorf("read messages: %w", err)
	}
	var doc file
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, nil, fmt.Errorf("decode messages: %w", err)
	}
	out := make(map[string]message, len(doc.Messages))
	for _, msg := range doc.Messages {
		out[msg.ID] = msg
	}
	return out, doc.AuthOutage, nil
}

func (m *Mailbox) save() error {
	doc := file{
		Messages:   make([]message, 0, len(m.msgs)),
		AuthOutage: m.authOutage,
	}
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
