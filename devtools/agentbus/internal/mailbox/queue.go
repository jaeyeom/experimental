package mailbox

import (
	"context"
	"fmt"
	"slices"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

const (
	stateDelivered = "delivered"
	stateExpired   = "expired"
	// defaultPoll matches the registry inventory interval.
	defaultPoll = 2 * time.Second
	waitTick    = 15 * time.Millisecond
)

// HoldHibernated is the hold reason of a message to a hibernated agent.
const HoldHibernated = "hibernated"

type outcome struct {
	accepted bool
	tried    bool
	reason   string
	target   string
}

// Poll delivers queued mail immediately, then again on each tick.
// A non-positive interval uses defaultPoll. An empty queue logs nothing.
// Before returning, Poll waits for the flushes it started; they run under
// ctx, so its end cuts off their remote calls.
func (m *Mailbox) Poll(ctx context.Context, interval time.Duration) {
	if interval <= 0 {
		interval = defaultPoll
	}
	var flushes sync.WaitGroup
	defer flushes.Wait()
	m.flushQuiet(ctx, &flushes)
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			m.flushQuiet(ctx, &flushes)
		}
	}
}

func (m *Mailbox) enqueue(ctx context.Context, msg message, opt Options) (Result, error) {
	if opt.TTL > 0 {
		deadline := m.now().UTC().Add(opt.TTL)
		msg.Deadline = &deadline
	}
	if err := m.store(msg); err != nil {
		return Result{}, err
	}
	if err := m.flushHandle(ctx, msg.Handle); err != nil {
		return Result{}, err
	}
	if !opt.Wait {
		return m.resultOf(msg.ID)
	}
	return m.wait(ctx, msg.ID, msg.Handle)
}

func (m *Mailbox) wait(ctx context.Context, id, handle string) (Result, error) {
	for {
		if err := m.flushHandle(ctx, handle); err != nil {
			return Result{}, err
		}
		result, err := m.resultOf(id)
		if err != nil {
			return Result{}, err
		}
		if terminal(result.State) || ctx.Err() != nil {
			return result, nil
		}
		timer := time.NewTimer(waitTick)
		select {
		case <-ctx.Done():
			timer.Stop()
			return m.resultOf(id)
		case <-timer.C:
		}
	}
}

func (m *Mailbox) store(msg message) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.msgs[msg.ID] = msg
	m.queue[msg.Handle] = append(m.queue[msg.Handle], msg.ID)
	if err := m.logEvent(eventlog.Event{
		Event:  eventlog.EventSend,
		Actor:  msg.From,
		MsgID:  msg.ID,
		Name:   msg.To,
		Detail: "stored",
	}); err != nil {
		return err
	}
	return m.save()
}

func (m *Mailbox) resultOf(id string) (Result, error) {
	msg, err := m.get(id)
	if err != nil {
		return Result{}, err
	}
	return Result{ID: msg.ID, State: msg.State}, nil
}

// flushQuiet starts a flush for each machine that is not already
// flushing, and does not wait for it. A slow or hung machine holds up only
// its own handles: the next tick skips it and flushes the others.
// Each flush is added to flushes. Errors are logged.
func (m *Mailbox) flushQuiet(ctx context.Context, flushes *sync.WaitGroup) {
	for machine, handles := range byMachine(m.handles()) {
		if !m.startFlush(machine) {
			continue
		}
		flushes.Go(func() {
			defer m.endFlush(machine)
			for _, handle := range handles {
				if ctx.Err() != nil {
					return
				}
				if err := m.flushHandle(ctx, handle); err != nil {
					_ = m.logEvent(eventlog.Event{
						Event:  eventlog.EventAdapterError,
						Actor:  "agentbus",
						Detail: "queue flush failed",
						Reason: err.Error(),
					})
				}
			}
		})
	}
}

func (m *Mailbox) startFlush(machine string) bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.flushing[machine] {
		return false
	}
	m.flushing[machine] = true
	return true
}

func (m *Mailbox) endFlush(machine string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	delete(m.flushing, machine)
}

// byMachine groups handles by the part before the first "/", such as
// herdr:local or herdr:m2. Order within a group is kept.
func byMachine(handles []string) map[string][]string {
	groups := map[string][]string{}
	for _, handle := range handles {
		machine, _, _ := strings.Cut(handle, "/")
		groups[machine] = append(groups[machine], handle)
	}
	return groups
}

func (m *Mailbox) handles() []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	out := make([]string, 0, len(m.queue))
	for handle := range m.queue {
		out = append(out, handle)
	}
	sort.Strings(out)
	return out
}

func (m *Mailbox) flushHandle(ctx context.Context, handle string) error {
	if handle == "" {
		return nil
	}
	for {
		if ctx.Err() != nil {
			return nil
		}
		cont, err := m.step(ctx, handle)
		if err != nil || !cont {
			return err
		}
	}
}

// step attempts the front message.
// The mailbox lock is not held across State, Deliver, or the expiry notice.
func (m *Mailbox) step(ctx context.Context, handle string) (bool, error) {
	m.mu.Lock()
	id, ok := m.front(handle)
	if !ok {
		m.mu.Unlock()
		return false, nil
	}
	if m.inflight[id] {
		for m.inflight[id] {
			m.cv.Wait()
		}
		m.mu.Unlock()
		return true, nil
	}
	msg := m.msgs[id]
	if m.expired(msg) {
		cont, err := m.finishExpire(ctx, handle, msg)
		return cont, err
	}
	m.inflight[id] = true
	m.mu.Unlock()

	outcome := m.attempt(ctx, msg)

	m.mu.Lock()
	delete(m.inflight, id)
	m.cv.Broadcast()
	if !m.stillFront(handle, id) {
		m.mu.Unlock()
		return true, nil
	}
	if outcome.accepted {
		err := m.markDelivered(id)
		m.mu.Unlock()
		if err != nil {
			return false, err
		}
		return true, nil
	}
	if m.expired(m.msgs[id]) {
		return m.finishExpire(ctx, handle, m.msgs[id])
	}
	if err := m.holdFront(id, outcome); err != nil {
		m.mu.Unlock()
		return false, err
	}
	err := m.holdTail(handle)
	m.mu.Unlock()
	if err != nil {
		return false, err
	}
	return false, nil
}

// Requeue puts a delivered message back in its queue, in send order.
// The prompt hook calls it when Claude Code blocked a submit that merged
// the message into a user draft. The input box still holds both, so the
// message waits as a draft hold. A message that is still queued is left
// as it is. actor is who reported the merge.
func (m *Mailbox) Requeue(id, actor string) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	for m.inflight[id] {
		m.cv.Wait()
	}
	msg, ok := m.msgs[id]
	switch {
	case !ok:
		return fmt.Errorf("unknown message %s", id)
	case msg.State == stateExpired:
		return fmt.Errorf("message %s expired", id)
	case msg.State != stateDelivered:
		return nil
	}
	now := m.now().UTC()
	msg.State = "held: " + adapter.HoldDraft
	msg.HeldAt = &now
	msg.DeliveredAt = nil
	msg.ClosedAt = nil
	m.msgs[id] = msg
	m.insert(msg)
	if err := m.logEvent(eventlog.Event{
		Event:  eventlog.EventRequeued,
		Actor:  actor,
		MsgID:  msg.ID,
		Name:   msg.To,
		Detail: "prompt blocked: merged into a user draft",
	}); err != nil {
		return err
	}
	return m.save()
}

// insert queues msg for its handle in send order.
func (m *Mailbox) insert(msg message) {
	ids := m.queue[msg.Handle]
	at := len(ids)
	for i, other := range ids {
		if messageBefore(msg, m.msgs[other]) {
			at = i
			break
		}
	}
	m.queue[msg.Handle] = slices.Insert(ids, at, msg.ID)
}

func (m *Mailbox) finishExpire(ctx context.Context, handle string, msg message) (bool, error) {
	m.pop(handle)
	closed, err := m.markExpired(msg)
	m.mu.Unlock()
	if err != nil {
		return false, err
	}
	m.notifyExpired(ctx, closed)
	return true, nil
}

func (m *Mailbox) attempt(ctx context.Context, msg message) outcome {
	if err := m.prepare(ctx); err != nil {
		return outcome{reason: "unreachable", target: "unreachable"}
	}
	if m.reg.Hibernated(msg.Handle) {
		// Its pane runs a shell now. The message waits for a wake.
		return outcome{reason: HoldHibernated, target: HoldHibernated}
	}
	state, ad, err := m.observe(ctx, msg.Handle)
	if err != nil || ad == nil || !canDeliver(state) {
		return m.holdOutcome(state, err)
	}
	if reason, held := composerHold(ctx, ad, msg.Handle); held {
		return outcome{reason: reason, target: string(state)}
	}
	text := render(msg, m.preview(msg.Body))
	// Re-read right before the paste: a draft that appeared since the
	// check means the user is typing. The race narrows but cannot close.
	if reason, held := composerHold(ctx, ad, msg.Handle); held {
		return outcome{reason: reason, target: string(state)}
	}
	_ = m.logEvent(eventlog.Event{
		Event:       eventlog.EventDeliverAttempt,
		Actor:       msg.From,
		MsgID:       msg.ID,
		Name:        msg.To,
		Detail:      "composer-empty-at-check",
		TargetState: string(state),
	})
	result, err := ad.Deliver(ctx, msg.Handle, text)
	if ue := remotecall.AsUnreachable(err); ue != nil {
		return unreachableOutcome(ue)
	}
	if err != nil {
		return outcome{tried: true, reason: err.Error(), target: string(state)}
	}
	switch result {
	case adapter.ResultAccepted:
		return outcome{accepted: true, tried: true, target: string(state)}
	case adapter.ResultRefusedRetryable:
		return outcome{tried: true, reason: "transport refused", target: string(adapter.StateBlocked)}
	default:
		return outcome{tried: true, reason: "transport failed", target: string(state)}
	}
}

// composerHold reports a hold reason when the composer is not empty.
// A composer that cannot be read counts as not empty. A draft hold is
// not a tried delivery, so it never counts as a retry.
func composerHold(ctx context.Context, ad adapter.Adapter, handle string) (string, bool) {
	empty, reason, err := ad.ComposerEmpty(ctx, handle)
	switch ue := remotecall.AsUnreachable(err); {
	case ue != nil:
		return ue.Error(), true
	case err != nil || (!empty && reason == ""):
		return adapter.HoldDraftUnknown, true
	case !empty:
		return reason, true
	default:
		return "", false
	}
}

// unreachableOutcome holds a message for a machine that cannot be
// reached. It is not a tried delivery, so it never counts as a retry.
func unreachableOutcome(ue *remotecall.UnreachableError) outcome {
	return outcome{reason: ue.Error(), target: "unreachable"}
}

func (m *Mailbox) holdOutcome(state adapter.State, err error) outcome {
	if ue := remotecall.AsUnreachable(err); ue != nil {
		return unreachableOutcome(ue)
	}
	if err != nil {
		return outcome{reason: "unreachable", target: "unreachable"}
	}
	return outcome{reason: holdReason(state), target: string(state)}
}

func (m *Mailbox) observe(ctx context.Context, handle string) (adapter.State, adapter.Adapter, error) {
	var fallback adapter.State
	var sawErr error
	for _, item := range m.adapters {
		if m.isBroken(item.Name()) {
			continue
		}
		state, err := item.State(ctx, handle)
		if err != nil {
			sawErr = err
			continue
		}
		if state == adapter.StateGone || state == adapter.StateUnknown {
			fallback = prefer(fallback, state)
			continue
		}
		return state, item, nil
	}
	if fallback == "" && sawErr != nil {
		return "", nil, sawErr
	}
	if fallback == "" {
		fallback = adapter.StateGone
	}
	return fallback, nil, nil
}

func prefer(current, next adapter.State) adapter.State {
	if current == "" || next == adapter.StateUnknown {
		return next
	}
	return current
}

func canDeliver(state adapter.State) bool {
	switch state {
	case adapter.StateIdle, adapter.StateWorking, adapter.StateDone:
		return true
	default:
		return false
	}
}

func holdReason(state adapter.State) string {
	switch state {
	case adapter.StateBlocked, adapter.StateNoAgent, adapter.StateUnknown, adapter.StateGone:
		return string(state)
	default:
		if state == "" {
			return string(adapter.StateGone)
		}
		return string(state)
	}
}

func (m *Mailbox) prepare(ctx context.Context) error {
	m.prep.Do(func() {
		m.prepErr = m.runSelfTests(ctx)
	})
	return m.prepErr
}

func (m *Mailbox) runSelfTests(ctx context.Context) error {
	broken := map[string]string{}
	for _, item := range m.adapters {
		err := item.SelfTest(ctx)
		if err == nil {
			continue
		}
		broken[item.Name()] = err.Error()
		if logErr := m.logEvent(eventlog.Event{
			Event:  eventlog.EventAdapterError,
			Actor:  "agentbus",
			Name:   item.Name(),
			Detail: "self-test failed",
			Reason: err.Error(),
		}); logErr != nil {
			return fmt.Errorf("log adapter_error: %w", logErr)
		}
	}
	m.mu.Lock()
	m.broken = broken
	m.mu.Unlock()
	return nil
}

func (m *Mailbox) isBroken(name string) bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	_, ok := m.broken[name]
	return ok
}

func (m *Mailbox) markDelivered(id string) error {
	msg := m.msgs[id]
	now := m.now().UTC()
	msg.State = stateDelivered
	msg.DeliveredAt = &now
	msg.ClosedAt = &now
	m.msgs[id] = msg
	m.pop(msg.Handle)
	detail := "accepted"
	if msg.HeldAt != nil {
		detail += " after hold of " + waitString(now.Sub(*msg.HeldAt))
	}
	if err := m.logEvent(eventlog.Event{
		Event:  eventlog.EventDelivered,
		Actor:  msg.From,
		MsgID:  msg.ID,
		Name:   msg.To,
		Detail: detail,
	}); err != nil {
		return err
	}
	return m.save()
}

func (m *Mailbox) markExpired(msg message) (message, error) {
	now := m.now().UTC()
	msg.State = stateExpired
	msg.ClosedAt = &now
	m.msgs[msg.ID] = msg
	if err := m.logEvent(eventlog.Event{
		Event:  eventlog.EventExpired,
		Actor:  msg.From,
		MsgID:  msg.ID,
		Name:   msg.To,
		Detail: "waited " + waitString(now.Sub(msg.SentAt)),
		Reason: "ttl",
	}); err != nil {
		return msg, err
	}
	if err := m.save(); err != nil {
		return msg, err
	}
	return msg, nil
}

func (m *Mailbox) holdFront(id string, outcome outcome) error {
	msg := m.msgs[id]
	next := "held: " + outcome.reason
	prev := msg.State
	msg.State = next
	m.markHeld(&msg)
	m.msgs[id] = msg
	switch {
	case outcome.tried && strings.HasPrefix(prev, "held:"):
		if err := m.logChange(msg, eventlog.EventRetry, outcome); err != nil {
			return err
		}
	case prev != next:
		if err := m.logChange(msg, eventlog.EventHeld, outcome); err != nil {
			return err
		}
	default:
		return nil
	}
	return m.save()
}

// markHeld records when the hold started, so delivery can log its duration.
func (m *Mailbox) markHeld(msg *message) {
	if msg.HeldAt == nil {
		now := m.now().UTC()
		msg.HeldAt = &now
	}
}

func (m *Mailbox) logChange(msg message, name string, outcome outcome) error {
	return m.logEvent(eventlog.Event{
		Event:       name,
		Actor:       msg.From,
		MsgID:       msg.ID,
		Name:        msg.To,
		Detail:      "waited " + waitString(m.now().Sub(msg.SentAt)),
		Reason:      outcome.reason,
		TargetState: outcome.target,
	})
}

func (m *Mailbox) holdTail(handle string) error {
	ids := m.queue[handle]
	if len(ids) < 2 {
		return nil
	}
	changed := false
	for _, id := range ids[1:] {
		msg := m.msgs[id]
		const next = "held: waiting for an earlier message"
		if msg.State == next {
			continue
		}
		msg.State = next
		m.markHeld(&msg)
		m.msgs[id] = msg
		changed = true
		if err := m.logEvent(eventlog.Event{
			Event:       eventlog.EventHeld,
			Actor:       msg.From,
			MsgID:       msg.ID,
			Name:        msg.To,
			Detail:      "waited " + waitString(m.now().Sub(msg.SentAt)),
			Reason:      "waiting for an earlier message",
			TargetState: "queued",
		}); err != nil {
			return err
		}
	}
	if !changed {
		return nil
	}
	return m.save()
}

// notifyExpired tells a reachable sender directly. It does not queue
// the notice, and it skips anonymous or unreachable senders.
func (m *Mailbox) notifyExpired(ctx context.Context, msg message) {
	if strings.HasPrefix(msg.From, "anon:") {
		return
	}
	handle, err := m.reg.Resolve(msg.From)
	if err != nil {
		return
	}
	if err := m.prepare(ctx); err != nil {
		return
	}
	state, ad, err := m.observe(ctx, handle)
	if err != nil || ad == nil || !canDeliver(state) {
		return
	}
	if reason, held := composerHold(ctx, ad, handle); held {
		_ = m.logEvent(eventlog.Event{
			Event:  eventlog.EventDeliverAttempt,
			Actor:  "agentbus",
			MsgID:  msg.ID,
			Name:   msg.From,
			Detail: "expiry notice not sent",
			Reason: reason,
		})
		return
	}
	text := fmt.Sprintf("agentbus: message %s to %s expired\n", msg.ID, msg.To)
	_, _ = ad.Deliver(ctx, handle, text)
}

func (m *Mailbox) front(handle string) (string, bool) {
	ids := m.queue[handle]
	if len(ids) == 0 {
		return "", false
	}
	return ids[0], true
}

func (m *Mailbox) stillFront(handle, id string) bool {
	front, ok := m.front(handle)
	return ok && front == id
}

func (m *Mailbox) pop(handle string) {
	ids := m.queue[handle]
	if len(ids) <= 1 {
		delete(m.queue, handle)
		return
	}
	m.queue[handle] = ids[1:]
}

func (m *Mailbox) expired(msg message) bool {
	return msg.Deadline != nil && !m.now().Before(*msg.Deadline)
}

func terminal(state string) bool {
	return state == stateDelivered || state == stateExpired
}

func rebuild(msgs map[string]message) map[string][]string {
	pending := make([]message, 0, len(msgs))
	for _, msg := range msgs {
		if terminal(msg.State) {
			continue
		}
		pending = append(pending, msg)
	}
	sort.Slice(pending, func(i, j int) bool {
		return messageBefore(pending[i], pending[j])
	})
	queue := map[string][]string{}
	for _, msg := range pending {
		queue[msg.Handle] = append(queue[msg.Handle], msg.ID)
	}
	return queue
}

func waitString(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	return d.Round(time.Millisecond).String()
}
