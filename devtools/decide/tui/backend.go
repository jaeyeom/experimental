package tui

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// Backend is the inbox's access to a ledger and the side effects that
// belong where the ledger lives: notifications, compose sends, the
// status board, and herdr lookups of owner panes. LocalBackend is the
// ledger on this machine. A hub backend sends each call to the hub.
type Backend interface {
	// List is every item in the ledger, snoozed or not.
	List() ([]decide.Item, error)
	MarkRead(id string) (decide.Item, error)
	// Answer records value with via ui and runs the notify command of a
	// relay item. A failed notify is a *NotifyError; the answer stands.
	Answer(id, value string) (decide.Item, error)
	// Reply appends a human message with via ui and sends the thread
	// note to the agents route names, as decide.ReplyTargets resolves
	// them. An empty route is the owner. A failed send is a *NotifyError.
	Reply(id, text, route string) (decide.Item, error)
	Done(id, outcome string) (decide.Item, error)
	// Archive marks a notification done with outcome archived when no
	// message is newer than seen.
	Archive(id string, seen time.Time) (decide.Item, error)
	Snooze(id string, until time.Time) (decide.Item, error)
	Unsnooze(id string) (decide.Item, error)
	Status() (StatusBoard, error)
	// Send opens a thread to tg, forwarded from item forward when that
	// is not empty, and delivers text to tg.
	Send(forward string, tg Target, text string) error
	// OwnerPane is it's stored owner pane before herdr is asked, with
	// Machine empty for the machine the inbox runs on.
	OwnerPane(it *decide.Item) decide.OwnerLocation
	// Locate asks herdr where it's owner pane lives now.
	Locate(ctx context.Context, it *decide.Item) decide.OwnerLocation
	// Focus brings loc's tab to the front in this machine's herdr and
	// returns the status line text.
	Focus(ctx context.Context, loc decide.OwnerLocation) string
}

// StatusBoard is the status view's agents and resources.
type StatusBoard struct {
	Agents    []decide.AgentStatus    `json:"agents"`
	Resources []decide.ResourceStatus `json:"resources"`
}

// ErrUnreachable marks a backend call that failed because the hub
// could not be reached. The inbox keeps its view and says so.
var ErrUnreachable = errors.New("hub unreachable")

// SyncStatus reports the hub replica's connection state and last sync time.
type SyncStatus struct {
	Connected bool
	SyncedAt  time.Time
}

// ReplicaBackend is a backend that serves reads from a local replica
// and reports its sync freshness.
type ReplicaBackend interface {
	SyncStatus() SyncStatus
}

// OutboxStatus is the state of one queued write on a hub client.
type OutboxStatus string

const (
	OutboxPending OutboxStatus = "pending"
	OutboxHeld    OutboxStatus = "held"
	OutboxFailed  OutboxStatus = "failed"
)

// OutboxEntry is one queued write on a hub client.
type OutboxEntry struct {
	ID                string       `json:"id"`
	Op                string       `json:"op"`
	ItemID            string       `json:"itemId,omitempty"`
	Text              string       `json:"text,omitempty"`
	Route             string       `json:"route,omitempty"`
	Until             *time.Time   `json:"until,omitempty"`
	Seen              *time.Time   `json:"seen,omitempty"`
	To                string       `json:"to,omitempty"`
	Bus               bool         `json:"bus,omitempty"`
	Forward           string       `json:"forward,omitempty"`
	Name              string       `json:"name,omitempty"`
	Choice            string       `json:"choice,omitempty"`
	WrittenAt         time.Time    `json:"writtenAt"`
	Status            OutboxStatus `json:"status"`
	Error             string       `json:"error,omitempty"`
	HoldReason        string       `json:"holdReason,omitempty"`
	BaseState         string       `json:"baseState,omitempty"`
	BaseMessageCount  int          `json:"baseMessageCount,omitempty"`
	BaseLastMessageAt time.Time    `json:"baseLastMessageAt,omitempty"`
	BaseAnswered      bool         `json:"baseAnswered,omitempty"`
	Force             bool         `json:"force,omitempty"`
}

// OutboxBackend is a backend that queues writes locally on a hub client.
type OutboxBackend interface {
	OutboxEnabled() bool
	Outbox() []OutboxEntry
	EditOutbox(id, text string) error
	CancelOutbox(id string) error
	SendOutbox(id string) error
	DismissFailedOutbox(id string) error
	SendOutboxAsMessage(id string) error
}

// DraftBackend is an optional backend capability for persisting unsent drafts.
type DraftBackend interface {
	ListDrafts() ([]decide.Draft, error)
	PutDraft(d decide.Draft, baseRev int64) (decide.Draft, bool, error)
	DeleteDraft(key, conflictID string) error
}

// OrgTreeResult is the contacts org tree JSON returned by a hub backend.
// Fallback is true when the hub does not support the org-tree op and the
// client should fall back to local orgctl with Note.
type OrgTreeResult struct {
	JSON     []byte
	Fallback bool
	Note     string
}

// OrgTreeBackend is an optional backend capability for fetching the
// contacts org tree from the hub or its local replica cache.
type OrgTreeBackend interface {
	OrgTree() (OrgTreeResult, error)
}

// PanelsBackend is an optional backend capability for reading and
// subscribing to dashboard panels without timer polling.
type PanelsBackend interface {
	Panels() ([]decide.PanelEntry, error)
	WatchPanels(ctx context.Context, emit func([]decide.PanelEntry)) error
}

// FlagBackend is an optional backend capability for reading configured flag
// reasons and flagging an agent message or item.
type FlagBackend interface {
	Reasons() ([]decide.Reason, error)
	Flag(itemID string, msgIndex *int, reasons []string, note string) (decide.Event, error)
}

// AsksBackend is an optional backend capability for reading and closing tracked asks.
type AsksBackend interface {
	AsksEnabled() (bool, error)
	ListAsks() ([]decide.AskRecord, error)
	CloseAsk(askID, status, reason string) (decide.AskRecord, error)
}

// unreachableStatus is the status line while the hub is unreachable.
const unreachableStatus = "hub unreachable; retrying"

// NotifyError is a write that was stored whose notification failed.
type NotifyError struct {
	Err error
}

func (e *NotifyError) Error() string { return e.Err.Error() }

func (e *NotifyError) Unwrap() error { return e.Err }

// LocalBackend is the ledger in Dir on this machine. RunSend, BusRun,
// and Herdr replace decide.RunSend, decide.ExecAgentbus, and
// decide.ExecHerdr. Machine is this machine's herdr label.
type LocalBackend struct {
	Dir               string
	Now               func() time.Time
	SendTemplate      string
	LateDeliveryAfter time.Duration
	RunSend           func([]string) error
	BusRun            BusRunner
	Herdr             decide.HerdrCall
	Checkouts         []decide.ConfigCheckout
	Machine           string
}

// ledgerSync controls whether LocalBackend ledgers fsync atomic writes.
// Tests disable it in TestMain while preserving atomic replacement.
var ledgerSync = true //nolint:pkgvar // TestMain sets false in tui tests

func (b *LocalBackend) ledger() (*decide.Ledger, error) {
	led, err := decide.Open(b.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide ledger: %w", err)
	}
	led.SetSync(ledgerSync)
	if b.Now != nil {
		led.SetClock(b.Now)
	}
	return led, nil
}

func (b *LocalBackend) now() time.Time {
	if b.Now != nil {
		return b.Now()
	}
	return time.Now()
}

func (b *LocalBackend) busRunner() BusRunner {
	if b.BusRun != nil {
		return b.BusRun
	}
	return decide.ExecAgentbus
}

// with opens the ledger and runs fn on it.
func (b *LocalBackend) with(fn func(*decide.Ledger) (decide.Item, error)) (decide.Item, error) {
	led, err := b.ledger()
	if err != nil {
		return decide.Item{}, err
	}
	return fn(led)
}

// FollowDue retries failed deliveries that are due and follows up stale
// answers on this ledger. Each pass locks the ledger per write, and a
// re-send is claimed under the lock, so passes from several processes
// send it once.
func (b *LocalBackend) FollowDue() error {
	led, err := b.ledger()
	if err != nil {
		return err
	}
	rerr := led.RetryDue(b.now(), io.Discard)
	if err := led.FollowStale(b.now(), io.Discard, b.Herdr); err != nil {
		return fmt.Errorf("follow stale answers: %w", err)
	}
	if rerr != nil {
		return fmt.Errorf("retry deliveries: %w", rerr)
	}
	return nil
}

// FollowLoop runs FollowDue at once and then every every until ctx
// ends. report, when not nil, gets each pass's error.
func (b *LocalBackend) FollowLoop(ctx context.Context, every time.Duration, report func(error)) {
	pass := func() {
		if err := b.FollowDue(); err != nil && report != nil {
			report(err)
		}
	}
	pass()
	tick := time.NewTicker(every)
	defer tick.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
			pass()
		}
	}
}

// DeliveryChecker is a backend that can refresh the delivery marks of
// messages agentbus has not delivered yet, where the ledger lives.
type DeliveryChecker interface {
	// CheckDeliveries runs one batched `agentbus status --json` for the
	// pending deliveries, or nothing when none is pending.
	CheckDeliveries(ctx context.Context) error
}

// CheckDeliveries implements DeliveryChecker.
func (b *LocalBackend) CheckDeliveries(ctx context.Context) error {
	led, err := b.ledger()
	if err != nil {
		return err
	}
	_, err = led.CheckDeliveries(ctx, b.busRunner())
	return err //nolint:wrapcheck // CheckDeliveries names agentbus status
}

// List implements Backend.
func (b *LocalBackend) List() ([]decide.Item, error) {
	led, err := b.ledger()
	if err != nil {
		return nil, err
	}
	threshold := b.LateDeliveryAfter
	if threshold <= 0 {
		threshold = decide.DefaultLateDeliveryAfter
	}
	if _, err := led.MarkLateDeliveries(threshold); err != nil {
		return nil, fmt.Errorf("mark late deliveries: %w", err)
	}
	_ = led.RetryDue(b.now(), io.Discard)
	_ = led.FollowStale(b.now(), io.Discard, b.Herdr)
	all, err := led.List()
	if err != nil {
		return nil, fmt.Errorf("list decide ledger: %w", err)
	}
	return all, nil
}

// Get is one item.
func (b *LocalBackend) Get(id string) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Get(id) })
}

// MarkRead implements Backend.
func (b *LocalBackend) MarkRead(id string) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) {
		it, err := led.MarkRead(id)
		if err != nil {
			return decide.Item{}, fmt.Errorf("mark decide item read: %w", err)
		}
		return it, nil
	})
}

// Answer implements Backend.
func (b *LocalBackend) Answer(id, value string) (decide.Item, error) {
	return b.AnswerAt(id, value, time.Time{})
}

// AnswerAt is Answer with an optional client writtenAt timestamp.
func (b *LocalBackend) AnswerAt(id, value string, writtenAt time.Time) (decide.Item, error) {
	updated, err := b.with(func(led *decide.Ledger) (decide.Item, error) {
		return led.AnswerAt(id, value, decide.ViaUI, writtenAt)
	})
	if err != nil {
		return decide.Item{}, err
	}
	if updated.Kind == decide.KindRelay {
		led, lerr := b.ledger()
		if lerr != nil {
			return updated, lerr
		}
		updated, err = led.DeliverAnswer(&updated, io.Discard)
		if err != nil {
			return updated, &NotifyError{Err: err}
		}
	}
	return updated, nil
}

// Reply implements Backend.
func (b *LocalBackend) Reply(id, text, route string) (decide.Item, error) {
	return b.ReplyAt(id, text, route, time.Time{})
}

// ReplyAt is Reply with an optional client writtenAt timestamp.
func (b *LocalBackend) ReplyAt(id, text, route string, writtenAt time.Time) (decide.Item, error) {
	if route != "" {
		if err := decide.CheckRoute(route); err != nil {
			return decide.Item{}, err //nolint:wrapcheck // CheckRoute names the route
		}
	}
	updated, err := b.with(func(led *decide.Ledger) (decide.Item, error) {
		return led.ComposeAt(id, text, decide.ViaUI, "", writtenAt)
	})
	if err != nil {
		return decide.Item{}, err
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		err = fmt.Errorf("load decide config: %w", err)
		updated.NotifyError = b.recordNotifyError(updated.ID, err)
		return updated, &NotifyError{Err: err}
	}
	targets := decide.ReplyTargets(&updated, cfg, route)
	if len(targets) == 0 {
		updated.NotifyError = b.recordNotifyError(updated.ID, nil)
		return updated, nil
	}
	var first error
	for _, tg := range targets {
		d, err := b.notifyThread(&updated, cfg, tg, text)
		if d.Outcome != "" {
			updated = b.recordCompose(updated, d)
		} else {
			updated.NotifyError = b.recordNotifyError(updated.ID, err)
		}
		if err != nil && first == nil {
			first = err
		}
	}
	if first != nil {
		return updated, &NotifyError{Err: first}
	}
	return updated, nil
}

// recordCompose stores d as the delivery of item's newest message, the
// one just sent. A ledger error keeps item as it was.
func (b *LocalBackend) recordCompose(item decide.Item, d decide.Delivery) decide.Item {
	if len(item.Messages) == 0 {
		return item
	}
	at := item.Messages[len(item.Messages)-1].Time
	got, err := b.with(func(led *decide.Ledger) (decide.Item, error) { return led.RecordCompose(item.ID, at, d) })
	if err != nil {
		return item
	}
	return got
}

// recordNotifyError stores err on id and returns the stored text.
// A nil err clears a previous failure.
func (b *LocalBackend) recordNotifyError(id string, err error) string {
	message := ""
	if err != nil {
		message = err.Error()
	}
	if _, serr := b.with(func(led *decide.Ledger) (decide.Item, error) {
		return led.SetNotifyError(id, message)
	}); serr != nil {
		return message
	}
	return message
}

// notifyThread runs the compose command so tg's pane sees the thread
// header, the original text, and the reply hint. A pane on another
// machine is reached through `--machine`. The attempt comes back as a
// Delivery; one with no outcome never reached a send.
func (b *LocalBackend) notifyThread(item *decide.Item, cfg decide.Config, tg decide.ReplyTarget, text string) (decide.Delivery, error) {
	pane, machine := tg.Pane, tg.Machine
	if strings.TrimSpace(pane) == "" {
		return decide.Delivery{}, fmt.Errorf("decide compose pane is empty")
	}
	target := decide.ParseTarget(pane, machine)
	switch target.Scheme {
	case decide.SchemePull:
		return decide.Delivery{}, nil
	case decide.SchemeAgentbus:
		return decide.SendBus(context.Background(), b.busRunner(), target.Address, decide.DeliveryNote(item.ID, "human", text)) //nolint:wrapcheck // SendBus returns agentbus's error, which the status line prints
	case "", decide.SchemeHerdr:
	default:
		return decide.NotifyItemVerbatim(item, pane, machine, text, "") //nolint:wrapcheck // NotifyItemVerbatim already wraps delivery errors
	}
	template := b.SendTemplate
	if strings.TrimSpace(template) == "" {
		template = cfg.SendTemplate
	}
	wire := target.Address
	if machine != "" {
		wire = target.Address + "@" + machine
	}
	argv := decide.MachineArgv(decide.SendArgv(template, target.Address, decide.DeliveryNote(item.ID, "human", text)), machine)
	if err := b.runSend(argv); err != nil {
		err = fmt.Errorf("decide send: %w", err)
		return decide.Delivery{Target: wire, Outcome: decide.OutcomeFailed, Error: err.Error()}, err
	}
	return decide.Delivery{Target: wire, Outcome: decide.OutcomeDelivered}, nil
}

func (b *LocalBackend) runSend(argv []string) error {
	if b.RunSend != nil {
		return b.RunSend(argv)
	}
	return decide.RunSend(argv) //nolint:wrapcheck // RunSend names the send in its errors
}

// Done implements Backend. It is the user's own done, so it closes the
// item at once, even when a message is unread.
func (b *LocalBackend) Done(id, outcome string) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) { return led.DoneVia(id, outcome, decide.ViaUI) })
}

// Archive implements Backend.
func (b *LocalBackend) Archive(id string, seen time.Time) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Archive(id, seen) })
}

// ArchiveNotes archives every open note with no message after seen.
func (b *LocalBackend) ArchiveNotes(seen time.Time) (int, error) {
	led, err := b.ledger()
	if err != nil {
		return 0, err
	}
	n, err := led.ArchiveNotes(seen)
	if err != nil {
		return n, fmt.Errorf("archive decide notes: %w", err)
	}
	return n, nil
}

// Withdraw withdraws item id with reason.
func (b *LocalBackend) Withdraw(id, reason string) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Withdraw(id, reason) })
}

// Snooze implements Backend.
func (b *LocalBackend) Snooze(id string, until time.Time) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Snooze(id, until) })
}

// Unsnooze implements Backend.
func (b *LocalBackend) Unsnooze(id string) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Unsnooze(id) })
}

// Status implements Backend. Resources show the agentbus lease of
// their configured checkouts.
func (b *LocalBackend) Status() (StatusBoard, error) {
	board, err := decide.OpenStatus(b.Dir)
	if err != nil {
		return StatusBoard{}, fmt.Errorf("open decide status: %w", err)
	}
	agents, err := board.Agents()
	if err != nil {
		return StatusBoard{}, fmt.Errorf("read decide status: %w", err)
	}
	resources, err := board.Resources()
	if err != nil {
		return StatusBoard{}, fmt.Errorf("read decide status: %w", err)
	}
	resources = decide.ResourceView(context.Background(), resources, b.Checkouts, b.busRunner(), b.now())
	return StatusBoard{Agents: agents, Resources: resources}, nil
}

// Send implements Backend. A configured target gets the send template.
// A bus name, and a pane that starts with agentbus:, gets `agentbus send`.
// A failed send drops the thread it opened.
func (b *LocalBackend) Send(forward string, tg Target, text string) error {
	return b.SendAt(forward, tg, text, time.Time{})
}

// SendAt is Send with an optional client writtenAt timestamp.
func (b *LocalBackend) SendAt(forward string, tg Target, text string, writtenAt time.Time) error {
	if tg.Bus {
		return b.sendBus(forward, tg, text)
	}
	if rec, ok := decide.AgentbusPaneRecipient(tg.Pane); ok {
		tg.Name = rec.Name
		tg.Pane = rec.Pane
		return b.sendBus(forward, tg, text)
	}
	// The thread id has to exist before the pane note is built.
	led, err := b.ledger()
	if err != nil {
		return err
	}
	item, err := led.ForwardThreadAt(forward, tg.Name, tg.Pane, text, decide.ViaUI, writtenAt)
	if err != nil {
		return fmt.Errorf("store decide thread: %w", err)
	}
	var d decide.Delivery
	if scheme := decide.ParseTarget(tg.Pane, "").Scheme; scheme != "" && scheme != decide.SchemeHerdr {
		d, err = decide.NotifyItemVerbatim(&item, tg.Pane, "", text, "")
	} else {
		err = b.runSend(decide.SendArgv(b.SendTemplate, tg.Pane, deliveryNote(item.ID, forward, text)))
		if err == nil {
			d = decide.Delivery{Target: tg.Pane, Outcome: decide.OutcomeDelivered}
		}
	}
	if err != nil {
		if derr := led.Delete(item.ID); derr != nil {
			return fmt.Errorf("%w; %w", err, derr)
		}
		return err
	}
	if _, err := led.RecordCompose(item.ID, item.Messages[0].Time, d); err != nil {
		return fmt.Errorf("record delivery: %w", err)
	}
	if err := decide.AppendSend(b.Dir, tg.Name, tg.Pane, text); err != nil {
		return fmt.Errorf("record decide send: %w", err)
	}
	return nil
}

func (b *LocalBackend) sendBus(forward string, tg Target, text string) error {
	led, err := b.ledger()
	if err != nil {
		return err
	}
	_, err = decide.OpenBusThread(led, b.Dir, forward, tg.Name, text, decide.ViaUI, b.busRunner())
	return err //nolint:wrapcheck // OpenBusThread names the failure, and a bus error is the status line
}

// deliveryNote is the pane note for a new thread, naming the item it
// was forwarded from when there is one.
func deliveryNote(id, forward, text string) string {
	if forward != "" {
		return decide.ForwardNote(id, forward, "human", text)
	}
	return decide.DeliveryNote(id, "human", text)
}

// OwnerPane implements Backend.
func (b *LocalBackend) OwnerPane(it *decide.Item) decide.OwnerLocation {
	return decide.OwnerPane(it, b.Machine)
}

// Locate implements Backend.
func (b *LocalBackend) Locate(ctx context.Context, it *decide.Item) decide.OwnerLocation {
	return decide.LocationResolver{Run: b.Herdr}.Resolve(ctx, []decide.OwnerLocation{b.OwnerPane(it)})[0]
}

// Focus implements Backend.
func (b *LocalBackend) Focus(ctx context.Context, loc decide.OwnerLocation) string {
	return decide.LocationResolver{Run: b.Herdr}.Focus(ctx, loc)
}

// Panels implements PanelsBackend.
func (b *LocalBackend) Panels() ([]decide.PanelEntry, error) {
	store, err := decide.OpenPanels(b.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide panels: %w", err)
	}
	if b.Now != nil {
		store.SetClock(b.Now)
	}
	panels, err := store.List()
	if err != nil {
		return nil, fmt.Errorf("list decide panels: %w", err)
	}
	return panels, nil
}

// WatchPanels implements PanelsBackend.
func (b *LocalBackend) WatchPanels(ctx context.Context, emit func([]decide.PanelEntry)) error {
	return decide.WatchPanels(ctx, b.Dir, b.Now, emit) //nolint:wrapcheck // WatchPanels names the failure
}

func (b *LocalBackend) draftStore() (*decide.DraftStore, error) {
	ds, err := decide.OpenDraftStore(b.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide drafts: %w", err)
	}
	if b.Now != nil {
		ds.SetClock(b.Now)
	}
	return ds, nil
}

// ListDrafts implements DraftBackend.
func (b *LocalBackend) ListDrafts() ([]decide.Draft, error) {
	ds, err := b.draftStore()
	if err != nil {
		return nil, err
	}
	_, drafts, err := ds.List()
	if err != nil {
		return nil, fmt.Errorf("list decide drafts: %w", err)
	}
	return drafts, nil
}

// PutDraft implements DraftBackend.
func (b *LocalBackend) PutDraft(d decide.Draft, baseRev int64) (decide.Draft, bool, error) {
	ds, err := b.draftStore()
	if err != nil {
		return decide.Draft{}, false, err
	}
	if d.UpdatedBy == "" {
		d.UpdatedBy = b.Machine
	}
	saved, conflict, err := ds.Put(d, baseRev)
	if err != nil {
		return decide.Draft{}, false, fmt.Errorf("put decide draft: %w", err)
	}
	return saved, conflict, nil
}

// DeleteDraft implements DraftBackend.
func (b *LocalBackend) DeleteDraft(key, conflictID string) error {
	ds, err := b.draftStore()
	if err != nil {
		return err
	}
	if err := ds.Delete(key, conflictID); err != nil {
		return fmt.Errorf("delete decide draft: %w", err)
	}
	return nil
}

// Reasons implements FlagBackend.
func (b *LocalBackend) Reasons() ([]decide.Reason, error) {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return nil, fmt.Errorf("load decide config: %w", err)
	}
	return cfg.EffectiveReasons(), nil
}

// Flag implements FlagBackend.
func (b *LocalBackend) Flag(itemID string, msgIndex *int, reasons []string, note string) (decide.Event, error) {
	led, err := b.ledger()
	if err != nil {
		return decide.Event{}, err
	}
	return led.FlagItem(itemID, msgIndex, reasons, note, decide.ViaUI) //nolint:wrapcheck // FlagItem already wraps errors
}

// AsksEnabled implements AsksBackend.
func (b *LocalBackend) AsksEnabled() (bool, error) {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return false, fmt.Errorf("load decide config: %w", err)
	}
	return cfg.Asks.Enabled, nil
}

// ListAsks implements AsksBackend.
func (b *LocalBackend) ListAsks() ([]decide.AskRecord, error) {
	led, err := b.ledger()
	if err != nil {
		return nil, err
	}
	return led.ListAsks() //nolint:wrapcheck // ListAsks already wraps errors
}

// CloseAsk implements AsksBackend.
func (b *LocalBackend) CloseAsk(askID, status, reason string) (decide.AskRecord, error) {
	led, err := b.ledger()
	if err != nil {
		return decide.AskRecord{}, err
	}
	return led.CloseAsk(askID, status, reason, decide.Human, decide.ViaUI) //nolint:wrapcheck // CloseAsk already wraps errors
}
