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
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

const (
	stateDelivered = "delivered"
	stateExpired   = "expired"
	// stateQueued is the send and reply answer when the paste has not
	// run. The stored state becomes delivered once the paste is accepted.
	stateQueued = "queued for delivery"
	// defaultPoll matches the registry inventory interval.
	defaultPoll = 2 * time.Second
	// defaultPasteBound is how long one paste may run after shutdown.
	defaultPasteBound = 5 * time.Second
	// defaultHoldNotice is how long a hold waits before one sender notice.
	defaultHoldNotice = 10 * time.Minute
	waitTick          = 15 * time.Millisecond
)

// HoldHibernated is the hold reason of a message to a hibernated agent.
const HoldHibernated = "hibernated"

type outcome struct {
	accepted bool
	tried    bool
	reason   string
	target   string
	// detail, when set, is the delivered log detail instead of "accepted".
	detail string
	// deferred means the precheck passed and the paste has not run.
	deferred bool
}

// Poll delivers queued mail immediately, then again on each tick.
// A non-positive interval uses defaultPoll. An empty queue logs nothing.
// Before returning, Poll waits for the flushes it started. A paste runs
// under its own bound, so shutdown does not cut it off; Poll still waits
// for that flush before it returns.
func (m *Mailbox) Poll(ctx context.Context, interval time.Duration) {
	if interval <= 0 {
		interval = defaultPoll
	}
	var flushes sync.WaitGroup
	defer m.wakes.Wait()
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
	hold := opt.ReplyFirst && !opt.Wait
	dup, err := m.store(msg, hold)
	if err != nil {
		return Result{}, err
	}
	if dup.Duplicate {
		return dup, nil
	}
	if hold {
		return m.replyFirst(ctx, msg)
	}
	if err := m.flushHandle(ctx, msg.Handle); err != nil {
		return Result{}, err
	}
	if !opt.Wait {
		return m.resultOf(msg.ID)
	}
	return m.wait(ctx, msg.ID, msg.Handle)
}

// replyFirst runs the precheck and returns. The paste waits for
// DeliverDeferred, so a slow paste does not hold the reply.
func (m *Mailbox) replyFirst(ctx context.Context, msg message) (Result, error) {
	defer m.releaseHoldPaste(msg.ID)
	if err := m.precheckHandle(ctx, msg.Handle); err != nil {
		return Result{}, err
	}
	res, err := m.resultOf(msg.ID)
	if err != nil {
		return Result{}, err
	}
	if res.State == "" {
		if err := m.markQueued(msg.ID); err != nil {
			return Result{}, err
		}
		res.State = stateQueued
	}
	m.noteDeferred(msg.Handle)
	return res, nil
}

// releaseHoldPaste lets a later flush paste id. Call it only after the
// send answer is copied.
func (m *Mailbox) releaseHoldPaste(id string) {
	m.mu.Lock()
	delete(m.holdPaste, id)
	m.mu.Unlock()
}

// precheckHandle applies holds and expiry, and stops before a paste.
func (m *Mailbox) precheckHandle(ctx context.Context, handle string) error {
	for {
		if ctx.Err() != nil {
			return nil
		}
		cont, err := m.step(ctx, handle, false)
		if err != nil || !cont {
			return err
		}
	}
}

// DeliverDeferred pastes what replyFirst left queued. Call it after the
// response is written. A flush error is logged; the reply already left.
func (m *Mailbox) DeliverDeferred(ctx context.Context) {
	m.mu.Lock()
	handles := m.deferred
	m.deferred = nil
	m.mu.Unlock()
	seen := map[string]bool{}
	for _, handle := range handles {
		if seen[handle] || handle == "" {
			continue
		}
		seen[handle] = true
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
}

func (m *Mailbox) noteDeferred(handle string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.deferred = append(m.deferred, handle)
}

// markQueued records that the paste has not run yet.
func (m *Mailbox) markQueued(id string) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	msg, ok := m.msgs[id]
	if !ok || msg.State != "" {
		return nil
	}
	msg.State = stateQueued
	m.msgs[id] = msg
	return m.save()
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

func (m *Mailbox) store(msg message, holdPaste bool) (Result, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if msg.ClientID != "" {
		dup, err := m.absorbLocked(msg)
		if err != nil || dup.Duplicate {
			return dup, err
		}
	}
	m.msgs[msg.ID] = msg
	if msg.ClientID != "" && msg.SenderHandle != "" {
		m.byClient[dedupeKey{sender: msg.SenderHandle, clientID: msg.ClientID}] = msg.ID
	}
	m.queue[msg.Handle] = append(m.queue[msg.Handle], msg.ID)
	if holdPaste {
		if m.holdPaste == nil {
			m.holdPaste = map[string]bool{}
		}
		m.holdPaste[msg.ID] = true
	}
	if err := m.logEvent(eventlog.Event{
		Event:  eventlog.EventSend,
		Actor:  msg.From,
		MsgID:  msg.ID,
		Name:   msg.To,
		Detail: "stored",
	}); err != nil {
		return Result{}, err
	}
	if err := m.save(); err != nil {
		return Result{}, err
	}
	return Result{}, nil
}

// absorbLocked returns the earlier accept when this sender already used
// msg.ClientID. A different body, recipient, or parent is a conflict
// and stores nothing. The caller holds m.mu.
func (m *Mailbox) absorbLocked(msg message) (Result, error) {
	id, ok := m.byClient[dedupeKey{sender: msg.SenderHandle, clientID: msg.ClientID}]
	if !ok {
		return Result{}, nil
	}
	prev, exists := m.msgs[id]
	if !exists {
		return Result{}, nil
	}
	if prev.Body != msg.Body || prev.To != msg.To || prev.Re != msg.Re {
		return Result{}, fmt.Errorf("client id %s conflicts with message %s", msg.ClientID, prev.ID)
	}
	if err := m.logEvent(eventlog.Event{
		Event:  eventlog.EventDedupe,
		Actor:  msg.From,
		MsgID:  prev.ID,
		Name:   prev.To,
		Detail: msg.ClientID,
	}); err != nil {
		return Result{}, err
	}
	return Result{ID: prev.ID, State: prev.State, Duplicate: true}, nil
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
		cont, err := m.step(ctx, handle, true)
		if err != nil || !cont {
			return err
		}
	}
}

// leaveForReply reports that this flush should stop. The front is missing,
// or it is a ReplyFirst message whose answer is not snapshotted yet.
// The caller holds m.mu.
func (m *Mailbox) leaveForReply(id string, ok, deliver bool) bool {
	if !ok {
		return true
	}
	return deliver && m.holdPaste[id]
}

// step attempts the front message.
// The mailbox lock is not held across State, Deliver, or the expiry notice.
// deliver false stops once the precheck says the front is ready to paste.
func (m *Mailbox) step(ctx context.Context, handle string, deliver bool) (bool, error) {
	m.mu.Lock()
	id, ok := m.front(handle)
	if m.leaveForReply(id, ok, deliver) {
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

	outcome := m.attempt(ctx, msg, deliver)
	if !outcome.deferred {
		m.noteLiveness(handle, outcome)
	}

	m.mu.Lock()
	delete(m.inflight, id)
	m.cv.Broadcast()
	if outcome.deferred {
		err := m.markFrontQueued(handle, id)
		m.mu.Unlock()
		return false, err
	}
	if !m.stillFront(handle, id) {
		m.mu.Unlock()
		return true, nil
	}
	if outcome.accepted {
		err := m.markDelivered(id, outcome.detail)
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
	if err := m.maybeHoldNotices(ctx, handle); err != nil {
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

// noteLiveness feeds a delivery result to the list liveness cache: an
// accepted paste means alive, and an unreachable machine unreachable.
func (m *Mailbox) noteLiveness(handle string, o outcome) {
	switch {
	case o.accepted:
		m.reg.ObserveLiveness(handle, "alive")
	case o.target == "unreachable":
		m.reg.ObserveLiveness(handle, "unreachable")
	}
}

func (m *Mailbox) attempt(ctx context.Context, msg message, deliver bool) outcome {
	if err := m.prepare(ctx); err != nil {
		return outcome{reason: "unreachable", target: "unreachable"}
	}
	if rec, ok := m.reg.Hibernation(msg.Handle); ok && hibernate.Asleep(rec.Status) {
		// Its pane runs a shell now. The message waits for a wake.
		return m.wakeOutcome(ctx, rec)
	}
	state, ad, err := m.observe(ctx, msg.Handle)
	if err != nil || ad == nil || !canDeliver(state) {
		return m.holdOutcome(state, err)
	}
	if ambiguousHold(msg.State) {
		if !deliver {
			// The screen check is part of the paste, so the reply is not held up.
			return outcome{reason: strings.TrimPrefix(msg.State, "held: "), target: string(state)}
		}
		if o, done := confirmPaste(ctx, ad, msg, state); done {
			return o
		}
	}
	if reason, held := composerHold(ctx, ad, msg.Handle); held {
		return outcome{reason: reason, target: string(state)}
	}
	if !deliver {
		return outcome{deferred: true}
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
	return m.deliverOutcome(ctx, ad, msg.Handle, text, state)
}

// SetPasteBound limits one paste. Zero uses defaultPasteBound.
// Call it before the mailbox delivers anything.
func (m *Mailbox) SetPasteBound(d time.Duration) {
	m.pasteBound = d
}

func (m *Mailbox) pasteLimit() time.Duration {
	if m.pasteBound > 0 {
		return m.pasteBound
	}
	return defaultPasteBound
}

// pasteContext detaches a paste from ctx's cancel and bounds it.
// ok is false when ctx is already done, so shutdown starts no new paste.
func (m *Mailbox) pasteContext(ctx context.Context) (context.Context, context.CancelFunc, bool) {
	if ctx.Err() != nil {
		return ctx, func() {}, false
	}
	paste, cancel := context.WithTimeout(context.WithoutCancel(ctx), m.pasteLimit())
	return paste, cancel, true
}

// ambiguousHold is a paste that may already be on screen.
func ambiguousHold(state string) bool {
	switch state {
	case "held: " + adapter.HoldAmbiguous, "held: " + adapter.HoldAmbiguousUnverified:
		return true
	default:
		return false
	}
}

// confirmPaste looks for this message before a retry paste.
// done is false when the text is not on screen and a paste may proceed.
func confirmPaste(ctx context.Context, ad adapter.Adapter, msg message, state adapter.State) (outcome, bool) {
	probe, ok := adapter.AsPasteConfirmer(ad)
	if !ok {
		return unverifiedOutcome(state), true
	}
	recent, composer, unreadable, err := probe.Probe(ctx, msg.Handle, "[agentbus "+msg.ID)
	if err != nil {
		return unverifiedOutcome(state), true
	}
	if hasEnvelope(recent, msg.ID) {
		return outcome{accepted: true, tried: true, detail: "confirmed-after-ambiguous", target: string(state)}, true
	}
	if unreadable {
		return unverifiedOutcome(state), true
	}
	if hasEnvelope(composer, msg.ID) {
		return submitPaste(ctx, probe, msg.Handle, state)
	}
	if strings.TrimSpace(composer) != "" {
		return outcome{reason: adapter.HoldDraft, target: string(state)}, true
	}
	return outcome{}, false
}

func unverifiedOutcome(state adapter.State) outcome {
	return outcome{reason: adapter.HoldAmbiguousUnverified, target: string(state)}
}

// submitPaste presses Enter for a paste that is already in the composer.
func submitPaste(ctx context.Context, probe adapter.PasteConfirmer, handle string, state adapter.State) (outcome, bool) {
	if err := probe.Submit(ctx, handle); err != nil {
		return outcome{tried: true, reason: adapter.HoldAmbiguous, target: string(state)}, true
	}
	return outcome{accepted: true, tried: true, detail: "submitted-after-ambiguous", target: string(state)}, true
}

// hasEnvelope reports whether text contains this message's envelope header.
// The id must end at a space, a closing bracket, or a newline, so m-1 does
// not match m-10.
func hasEnvelope(text, id string) bool {
	if id == "" || text == "" {
		return false
	}
	needle := "[agentbus " + id
	rest := text
	for {
		i := strings.Index(rest, needle)
		if i < 0 {
			return false
		}
		after := i + len(needle)
		if after >= len(rest) {
			return true
		}
		switch rest[after] {
		case ' ', ']', '\n', '\r':
			return true
		}
		rest = rest[after:]
	}
}

// deliverOutcome classifies one paste. The paste outlives ctx so
// shutdown can drain it, and it stops at the paste bound. A canceled
// ctx does not start a new paste. A transport error or a failed result
// is ambiguous: the text may already be on screen. A retryable refusal
// is definite, and so is an unreachable machine.
func (m *Mailbox) deliverOutcome(ctx context.Context, ad adapter.Adapter, handle, text string, state adapter.State) outcome {
	paste, cancel, ok := m.pasteContext(ctx)
	if !ok {
		return outcome{deferred: true}
	}
	defer cancel()
	result, err := ad.Deliver(paste, handle, text)
	if ue := remotecall.AsUnreachable(err); ue != nil {
		return unreachableOutcome(ue)
	}
	if err != nil || ambiguousResult(result) {
		return outcome{tried: true, reason: adapter.HoldAmbiguous, target: string(state)}
	}
	if result == adapter.ResultRefusedRetryable {
		return outcome{tried: true, reason: "transport refused", target: string(adapter.StateBlocked)}
	}
	return outcome{accepted: true, tried: true, target: string(state)}
}

func ambiguousResult(result adapter.Result) bool {
	return result != adapter.ResultAccepted && result != adapter.ResultRefusedRetryable
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

func (m *Mailbox) markDelivered(id, detail string) error {
	msg := m.msgs[id]
	now := m.now().UTC()
	msg.State = stateDelivered
	msg.DeliveredAt = &now
	msg.ClosedAt = &now
	m.msgs[id] = msg
	m.pop(msg.Handle)
	if detail == "" {
		detail = "accepted"
		if msg.HeldAt != nil {
			detail += " after hold of " + waitString(now.Sub(*msg.HeldAt))
		}
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
	reason := "waiting for an earlier message " + ids[0]
	next := "held: " + reason
	changed := false
	for _, id := range ids[1:] {
		msg := m.msgs[id]
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
			Reason:      reason,
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

// SetHoldNoticeAfter sets how long a hold waits before one notice to
// the sender. Nil uses 10 minutes. A duration of zero or less sends none.
func (m *Mailbox) SetHoldNoticeAfter(d *time.Duration) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if d == nil {
		m.noticeAfter = defaultHoldNotice
		m.noticeOff = false
		return
	}
	if *d <= 0 {
		m.noticeOff = true
		return
	}
	m.noticeOff = false
	m.noticeAfter = *d
}

// holdNotice is one sender notice claimed for a message past the threshold.
type holdNotice struct {
	id     string
	from   string
	to     string
	reason string
}

// maybeHoldNotices sends at most one notice per message that has been
// held for the configured duration. The caller does not hold m.mu.
func (m *Mailbox) maybeHoldNotices(ctx context.Context, handle string) error {
	due, err := m.claimHoldNotices(handle)
	if err != nil {
		return err
	}
	for _, item := range due {
		m.sendHoldNotice(ctx, item)
	}
	return nil
}

// claimHoldNotices marks due messages so a later poll does not notice
// them again. A save failure puts the marks back.
func (m *Mailbox) claimHoldNotices(handle string) ([]holdNotice, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.noticeOff {
		return nil, nil
	}
	after := m.noticeAfter
	if after <= 0 {
		after = defaultHoldNotice
	}
	now := m.now()
	var due []holdNotice
	var marked []string
	for _, id := range m.queue[handle] {
		msg := m.msgs[id]
		if msg.HoldNotice || msg.HeldAt == nil || now.Sub(*msg.HeldAt) < after {
			continue
		}
		msg.HoldNotice = true
		m.msgs[id] = msg
		marked = append(marked, id)
		due = append(due, holdNotice{
			id:     msg.ID,
			from:   msg.From,
			to:     msg.To,
			reason: strings.TrimPrefix(msg.State, "held: "),
		})
	}
	if len(due) == 0 {
		return nil, nil
	}
	if err := m.save(); err != nil {
		for _, id := range marked {
			msg := m.msgs[id]
			msg.HoldNotice = false
			m.msgs[id] = msg
		}
		return nil, err
	}
	return due, nil
}

// sendHoldNotice tells the sender once. An anonymous sender is only logged.
func (m *Mailbox) sendHoldNotice(ctx context.Context, n holdNotice) {
	if strings.HasPrefix(n.from, "anon:") {
		_ = m.logEvent(eventlog.Event{
			Event:  eventlog.EventHoldNoticeSkipped,
			Actor:  "agentbus",
			MsgID:  n.id,
			Name:   n.to,
			Detail: "anonymous sender",
			Reason: n.reason,
		})
		return
	}
	text := fmt.Sprintf("agentbus: message %s to %s held: %s\nagentbus status %s\n", n.id, n.to, n.reason, n.id)
	m.notifySender(ctx, n.id, n.from, text, "hold notice not sent")
}

// notifyExpired tells a reachable sender directly. It does not queue
// the notice, and it skips anonymous or unreachable senders.
func (m *Mailbox) notifyExpired(ctx context.Context, msg message) {
	text := fmt.Sprintf("agentbus: message %s to %s expired\n", msg.ID, msg.To)
	m.notifySender(ctx, msg.ID, msg.From, text, "expiry notice not sent")
}

// notifySender pastes text to from, the sender of message id, when it
// is a reachable agent with an empty composer. It does not queue the
// notice. unsent is the deliver_attempt detail when the composer holds
// it back.
func (m *Mailbox) notifySender(ctx context.Context, id, from, text, unsent string) {
	if strings.HasPrefix(from, "anon:") {
		return
	}
	handle, err := m.reg.Resolve(from)
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
			MsgID:  id,
			Name:   from,
			Detail: unsent,
			Reason: reason,
		})
		return
	}
	paste, cancel, ok := m.pasteContext(ctx)
	if !ok {
		return
	}
	defer cancel()
	_, _ = ad.Deliver(paste, handle, text)
}

// markFrontQueued stores stateQueued on a front the precheck will paste.
// The caller holds m.mu. A message that is no longer front is left alone.
func (m *Mailbox) markFrontQueued(handle, id string) error {
	if !m.stillFront(handle, id) {
		return nil
	}
	msg := m.msgs[id]
	if msg.State != "" {
		return nil
	}
	msg.State = stateQueued
	m.msgs[id] = msg
	return m.save()
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
