package mailbox

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// Hold reasons of a message to a hibernated agent the daemon wakes.
const (
	HoldWaking     = "waking"
	HoldWakeFailed = "wake failed"
	HoldPaneGone   = "pane gone"
)

// Wake backoff: the first retry waits wakeBackoff, and each further
// failure doubles it up to wakeBackoffMax.
const (
	wakeBackoff    = time.Minute
	wakeBackoffMax = time.Hour
)

// wakeActor is the actor of the wake events.
const wakeActor = "agentbus"

// Waker relaunches a hibernated agent in its pane from its record and
// returns once the agent waits for input. A *hibernate.PaneReusedError
// means the pane runs something else and nothing was launched.
type Waker interface {
	Wake(ctx context.Context, rec hibernate.Record) error
}

// ModelWaker is an optional Waker extension that accepts a resolved
// wake model (empty or "keep" preserves the agent's original model).
type ModelWaker interface {
	WakeWithModel(ctx context.Context, rec hibernate.Record, model string) error
}

// Errors of a Wake that starts no relaunch: the target names no agent,
// or its agent is not hibernated. Send and Reply also return
// ErrUnknownTarget when the target names no agent.
var (
	ErrUnknownTarget = errors.New("unknown target")
	ErrNotHibernated = errors.New("not hibernated")
)

// wakeCall is one wake in flight. err is set before done closes.
type wakeCall struct {
	done chan struct{}
	err  error
}

// SetWaker makes a delivery to a hibernated agent wake it first. Without
// a waker such a message is held as hibernated. Call it before the
// mailbox is used.
func (m *Mailbox) SetWaker(w Waker) {
	m.waker = w
}

// SetWakeModel configures the default wake model and per-name rules.
// Call it before the mailbox is used.
func (m *Mailbox) SetWakeModel(defaultModel string, byName []config.WakeModelRule) {
	m.wakeModel = defaultModel
	m.wakeModelByName = slices.Clone(byName)
}

// Wake relaunches target's hibernated agent now, with no message, and
// waits for the outcome. It ignores the backoff of a failed wake and
// joins a wake already in flight. It returns target's handle. A target
// that names no agent is ErrUnknownTarget, and one whose agent runs is
// ErrNotHibernated; neither starts a wake. An agent still stopping is
// neither: it is about to be hibernated.
func (m *Mailbox) Wake(ctx context.Context, target string) (string, error) {
	return m.WakeWithModel(ctx, target, "")
}

// WakeWithModel is Wake with an optional per-call model override.
func (m *Mailbox) WakeWithModel(ctx context.Context, target, model string) (string, error) {
	handle, err := m.reg.Resolve(target)
	if err != nil {
		return "", fmt.Errorf("resolve %s: %w: %w", target, ErrUnknownTarget, err)
	}
	rec, ok := m.reg.Hibernation(handle)
	switch {
	case !ok:
		return handle, fmt.Errorf("%s is %w", target, ErrNotHibernated)
	case rec.Status == hibernate.StatusStopFailed:
		return handle, fmt.Errorf("%s is %w: its record is %s", target, ErrNotHibernated, rec.Status)
	case !hibernate.Asleep(rec.Status):
		return handle, fmt.Errorf("%s is still %s; wake it once it is hibernated", target, rec.Status)
	case m.waker == nil:
		return handle, errors.New("this daemon has no waker")
	}
	call := m.startWake(ctx, rec, model)
	select {
	case <-call.done:
		return handle, call.err
	case <-ctx.Done():
		return handle, fmt.Errorf("wake %s: %w", target, ctx.Err())
	}
}

// wakeOutcome holds a message to a hibernated agent, starting a wake
// unless one is in flight or a failed wake's backoff has not ended.
func (m *Mailbox) wakeOutcome(ctx context.Context, rec hibernate.Record) outcome {
	const target = HoldHibernated
	if m.waker == nil {
		return outcome{reason: HoldHibernated, target: target}
	}
	m.mu.Lock()
	_, inFlight := m.waking[rec.Handle]
	m.mu.Unlock()
	if !inFlight && rec.Status == hibernate.StatusPaneGone {
		return outcome{reason: HoldPaneGone, target: target}
	}
	if !inFlight && rec.Status == hibernate.StatusWakeFailed && m.now().Before(rec.RetryAt) {
		return outcome{reason: HoldWakeFailed, target: target}
	}
	m.startWake(ctx, rec, "")
	return outcome{reason: HoldWaking, target: target}
}

// startWake starts a wake of rec's agent, or returns the one in flight:
// a handle never has two. When it ends, the handle's queue is flushed,
// so messages sent meanwhile go out in order.
func (m *Mailbox) startWake(ctx context.Context, rec hibernate.Record, override string) *wakeCall {
	handle := rec.Handle
	m.mu.Lock()
	if call, ok := m.waking[handle]; ok {
		m.mu.Unlock()
		return call
	}
	call := &wakeCall{done: make(chan struct{})}
	m.waking[handle] = call
	m.mu.Unlock()
	m.wakes.Go(func() {
		call.err = m.runWake(ctx, rec, override)
		m.mu.Lock()
		delete(m.waking, handle)
		m.mu.Unlock()
		close(call.done)
		if err := m.flushHandle(ctx, handle); err != nil {
			_ = m.logEvent(eventlog.Event{
				Event:  eventlog.EventAdapterError,
				Actor:  wakeActor,
				Detail: "queue flush after a wake failed",
				Reason: err.Error(),
			})
		}
	})
	return call
}

// runWake relaunches rec's agent and records the outcome: a woken agent
// loses its record, so its names are active again; a reused pane loses
// it too and is not launched into; a failed wake keeps it as
// wake-failed with a backoff. The senders of queued messages hear of a
// failure or a reused pane.
func (m *Mailbox) runWake(ctx context.Context, rec hibernate.Record, override string) error {
	handle := rec.Handle
	name := m.agentName(handle)
	resolved, usedModel, origModel := m.prepareWakeModel(rec, name, override)
	err := m.invokeWaker(ctx, rec, resolved)
	if err != nil && ctx.Err() != nil {
		// Shutdown cut the wake short; the record stays as it was.
		return fmt.Errorf("wake %s: %w", name, err)
	}
	var reused *hibernate.PaneReusedError
	var gone *hibernate.PaneGoneError
	switch {
	case errors.As(err, &reused):
		if clearErr := m.reg.ClearHibernation(handle); clearErr != nil {
			return fmt.Errorf("%w; drop the record: %w", err, clearErr)
		}
		_ = m.logEvent(eventlog.Event{Event: eventlog.EventWakeSkipped, Actor: wakeActor, Name: name, Detail: "pane reused; record dropped", Reason: err.Error()})
		m.notifyQueued(ctx, handle, fmt.Sprintf("[agentbus] %s was hibernated, but %s; it was not relaunched and its record was dropped; your message is queued", name, err))
		return fmt.Errorf("not relaunched: %w; the record was dropped", err)
	case errors.As(err, &gone):
		if _, recErr := m.reg.HoldPaneGone(handle, err.Error()); recErr != nil {
			return fmt.Errorf("%w; mark the record: %w", err, recErr)
		}
		_ = m.logEvent(eventlog.Event{Event: eventlog.EventWakeHeld, Actor: wakeActor, Name: name, Detail: "pane gone; messages held for a new holder", Reason: err.Error()})
		m.notifyQueuedEach(ctx, handle, func(id string) string {
			return fmt.Sprintf("[agentbus] %s is hibernated and its pane is gone; message %s waits for a new holder (agentbus forget/release %s to clear)", name, id, name)
		})
		return fmt.Errorf("wake %s: %w", name, err)
	case err != nil:
		failures := rec.Failures + 1
		if _, recErr := m.reg.FailWake(handle, err.Error(), m.now().Add(wakeDelay(failures))); recErr != nil {
			return fmt.Errorf("%w; mark the record: %w", err, recErr)
		}
		detail := fmt.Sprintf("failure %d", failures)
		if md := wakeModelDetail(usedModel, origModel); md != "" {
			detail += " (" + md + ")"
		}
		_ = m.logEvent(eventlog.Event{Event: eventlog.EventWakeFailed, Actor: wakeActor, Name: name, Detail: detail, Reason: err.Error()})
		m.notifyQueued(ctx, handle, fmt.Sprintf("[agentbus] %s is hibernated and failed to wake: %s; your message is queued", name, err))
		return fmt.Errorf("wake %s: %w", name, err)
	}
	if err := m.reg.ClearHibernation(handle); err != nil {
		return fmt.Errorf("woke %s, but the record could not be cleared: %w", name, err)
	}
	detail := "relaunched session " + rec.Session
	if md := wakeModelDetail(usedModel, origModel); md != "" {
		detail += " (" + md + ")"
	}
	_ = m.logEvent(eventlog.Event{Event: eventlog.EventWake, Actor: wakeActor, Name: name, Detail: detail})
	return nil
}

func (m *Mailbox) prepareWakeModel(rec hibernate.Record, name, override string) (resolved, used, orig string) {
	resolved = m.resolveWakeModel(rec, override)
	if resolved != "" && resolved != hibernate.WakeModelKeep && !hibernate.SupportsWakeModel(rec.Kind) {
		_ = m.logEvent(eventlog.Event{
			Event:  eventlog.EventWakeModelIgnored,
			Actor:  wakeActor,
			Name:   name,
			Detail: fmt.Sprintf("wake model %q ignored for %s agent", resolved, rec.Kind),
		})
		resolved = ""
	}
	orig = hibernate.ArgvModel(rec.Kind, rec.Argv)
	used = orig
	if resolved != "" && resolved != hibernate.WakeModelKeep {
		used = resolved
	}
	return resolved, used, orig
}

func (m *Mailbox) invokeWaker(ctx context.Context, rec hibernate.Record, model string) error {
	if mw, ok := m.waker.(ModelWaker); ok {
		return mw.WakeWithModel(ctx, rec, model) //nolint:wrapcheck // waker errors are wrapped by runWake
	}
	return m.waker.Wake(ctx, rec) //nolint:wrapcheck // waker errors are wrapped by runWake
}

func (m *Mailbox) resolveWakeModel(rec hibernate.Record, override string) string {
	if override != "" {
		return override
	}
	keys := append([]string{rec.Handle}, m.reg.NamesOf(rec.Handle)...)
	keys = append(keys, rec.Names...)
	for _, rule := range m.wakeModelByName {
		re := procinfo.Glob(rule.Pattern, true)
		if slices.ContainsFunc(keys, re.MatchString) {
			return rule.Model
		}
	}
	return m.wakeModel
}

func wakeModelDetail(used, orig string) string {
	switch {
	case used != "" && orig != "" && used != orig:
		return fmt.Sprintf("model %s, was %s", used, orig)
	case used != "":
		return "model " + used
	default:
		return ""
	}
}

// wakeDelay is the backoff after the nth failed wake in a row.
func wakeDelay(failures int) time.Duration {
	d := wakeBackoff
	for i := 1; i < failures && d < wakeBackoffMax; i++ {
		d *= 2
	}
	return min(d, wakeBackoffMax)
}

// agentName is handle's first name, else handle.
func (m *Mailbox) agentName(handle string) string {
	if names := m.reg.NamesOf(handle); len(names) > 0 {
		return names[0]
	}
	return handle
}

// notifyQueued tells each sender of a message queued for handle, once.
func (m *Mailbox) notifyQueued(ctx context.Context, handle, text string) {
	m.notifyQueuedEach(ctx, handle, func(string) string { return text })
}

// notifyQueuedEach is notifyQueued with text built from the id of the
// sender's first queued message.
func (m *Mailbox) notifyQueuedEach(ctx context.Context, handle string, text func(id string) string) {
	m.mu.Lock()
	type note struct{ id, from string }
	var notes []note
	seen := map[string]bool{}
	for _, id := range m.queue[handle] {
		from := m.msgs[id].From
		if !seen[from] {
			seen[from] = true
			notes = append(notes, note{id: id, from: from})
		}
	}
	m.mu.Unlock()
	for _, n := range notes {
		m.notifySender(ctx, n.id, n.from, text(n.id)+"\n", "wake notice not sent")
	}
}

// wakeStatus is the waiting line of agentbus status for a message held
// for a wake, or empty.
func (m *Mailbox) wakeStatus(msg message) string {
	switch strings.TrimPrefix(msg.State, "held: ") {
	case HoldWaking:
		return "waiting: wake in progress\n"
	case HoldWakeFailed:
		line := "waiting: wake failed"
		if rec, ok := m.reg.Hibernation(msg.Handle); ok && rec.Status == hibernate.StatusWakeFailed {
			line += fmt.Sprintf(": %s; next try after %s", rec.Reason, rec.RetryAt.UTC().Format(timeLayout))
		}
		return line + "\n"
	case HoldPaneGone:
		return "waiting: pane gone; held for a new holder of the name (agentbus forget to clear)\n"
	case HoldHibernated:
		return "waiting: hibernated\n"
	default:
		return ""
	}
}
