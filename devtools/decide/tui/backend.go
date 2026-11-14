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
	// Reply appends a human message with via ui and sends the owner
	// pane the thread note. A failed send is a *NotifyError.
	Reply(id, text string) (decide.Item, error)
	Done(id, outcome string) (decide.Item, error)
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
	Dir          string
	Now          func() time.Time
	SendTemplate string
	RunSend      func([]string) error
	BusRun       BusRunner
	Herdr        decide.HerdrCall
	Checkouts    []decide.ConfigCheckout
	Machine      string
}

func (b *LocalBackend) ledger() (*decide.Ledger, error) {
	led, err := decide.Open(b.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide ledger: %w", err)
	}
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

// List implements Backend.
func (b *LocalBackend) List() ([]decide.Item, error) {
	led, err := b.ledger()
	if err != nil {
		return nil, err
	}
	_ = led.RetryDue(b.now(), io.Discard)
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
	updated, err := b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Answer(id, value, decide.ViaUI) })
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
func (b *LocalBackend) Reply(id, text string) (decide.Item, error) {
	updated, err := b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Compose(id, text, decide.ViaUI, "") })
	if err != nil {
		return decide.Item{}, err
	}
	if err := b.notifyThread(&updated, text); err != nil {
		updated.NotifyError = b.recordNotifyError(updated.ID, err)
		return updated, &NotifyError{Err: err}
	}
	updated.NotifyError = b.recordNotifyError(updated.ID, nil)
	return updated, nil
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

// notifyThread runs the compose command so the owner pane sees the
// thread header, the original text, and the reply hint. A pane on
// another machine is reached through `--machine`.
func (b *LocalBackend) notifyThread(item *decide.Item, text string) error {
	if item.NotifyClosed {
		return nil
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fmt.Errorf("load decide config: %w", err)
	}
	pane, machine := decide.NotifyTarget(item, cfg)
	if strings.TrimSpace(pane) == "" {
		return fmt.Errorf("decide compose pane is empty")
	}
	if name, ok := strings.CutPrefix(pane, busPanePrefix); ok {
		_, err := decide.SendBus(context.Background(), b.busRunner(), name, decide.DeliveryNote(item.ID, "human", text))
		return err //nolint:wrapcheck // SendBus returns agentbus's error, which the status line prints
	}
	template := b.SendTemplate
	if strings.TrimSpace(template) == "" {
		template = cfg.SendTemplate
	}
	argv := decide.MachineArgv(decide.SendArgv(template, pane, decide.DeliveryNote(item.ID, "human", text)), machine)
	if err := b.runSend(argv); err != nil {
		return fmt.Errorf("decide send: %w", err)
	}
	return nil
}

func (b *LocalBackend) runSend(argv []string) error {
	if b.RunSend != nil {
		return b.RunSend(argv)
	}
	return decide.RunSend(argv) //nolint:wrapcheck // RunSend names the send in its errors
}

// Done implements Backend.
func (b *LocalBackend) Done(id, outcome string) (decide.Item, error) {
	return b.with(func(led *decide.Ledger) (decide.Item, error) { return led.Done(id, outcome) })
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

// Send implements Backend. A configured target gets the send template;
// a bus name gets `agentbus send`, and a failed bus send drops the
// thread it opened.
func (b *LocalBackend) Send(forward string, tg Target, text string) error {
	if tg.Bus {
		return b.sendBus(forward, tg, text)
	}
	// The thread id has to exist before the pane note is built.
	led, err := b.ledger()
	if err != nil {
		return err
	}
	item, err := led.ForwardThread(forward, tg.Name, tg.Pane, text, decide.ViaUI)
	if err != nil {
		return fmt.Errorf("store decide thread: %w", err)
	}
	if err := b.runSend(decide.SendArgv(b.SendTemplate, tg.Pane, deliveryNote(item.ID, forward, text))); err != nil {
		return err
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
