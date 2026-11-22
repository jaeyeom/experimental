package mailbox

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lastturn"
)

// DefaultAuthErrorPatterns are case-insensitive substrings matched against
// an assistant reply to detect an expired login.
var DefaultAuthErrorPatterns = []string{
	"Login expired",
	"Please run /login",
	"Not logged in",
	"Invalid API key",
}

// SetTranscript sets the function used to read an agent's transcript turns
// for post-delivery verification.
func (m *Mailbox) SetTranscript(fn func(adapter.Agent) ([]lastturn.Turn, bool)) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.transcript = fn
}

// SetVerifyWithin sets how long post-delivery verification waits for a
// turn. Nil uses 60s; zero or negative disables verification.
func (m *Mailbox) SetVerifyWithin(d *time.Duration) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if d == nil {
		m.verifyWithin = defaultVerifyWithin
		m.verifyOff = false
		return
	}
	if *d <= 0 {
		m.verifyOff = true
		return
	}
	m.verifyOff = false
	m.verifyWithin = *d
}

// SetAuthErrorPatterns sets the case-insensitive substrings that identify
// an auth error reply. Nil uses DefaultAuthErrorPatterns.
func (m *Mailbox) SetAuthErrorPatterns(patterns []string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.authPatterns = append([]string(nil), patterns...)
	if patterns == nil {
		m.authPatterns = nil
	}
}

// SetStaleComposerAfter sets how long unchanged composer text holds a
// message before notifying the sender once per tab. Nil uses 10m; zero
// or negative disables the notice.
func (m *Mailbox) SetStaleComposerAfter(d *time.Duration) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if d == nil {
		m.staleComposerAfter = defaultStaleComposerAfter
		m.staleComposerOff = false
		return
	}
	if *d <= 0 {
		m.staleComposerOff = true
		return
	}
	m.staleComposerOff = false
	m.staleComposerAfter = *d
}

// SetHibernateWarnWithin sets how recent a delivery must be for
// hibernating its agent to log a warning. Nil uses 60s; zero or
// negative turns the warning off.
func (m *Mailbox) SetHibernateWarnWithin(d *time.Duration) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.hibernateWarn = d
}

// RecentDeliveries are the ids of the messages delivered to handle
// within the hibernate warning window, oldest first.
func (m *Mailbox) RecentDeliveries(handle string) []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	window := defaultHibernateWarn
	if m.hibernateWarn != nil {
		window = *m.hibernateWarn
	}
	if window <= 0 {
		return nil
	}
	cutoff := m.now().Add(-window)
	var recent []message
	for _, msg := range m.msgs {
		if msg.Handle == handle && msg.DeliveredAt != nil && !msg.DeliveredAt.Before(cutoff) {
			recent = append(recent, msg)
		}
	}
	slices.SortFunc(recent, func(a, b message) int { return a.DeliveredAt.Compare(*b.DeliveredAt) })
	ids := make([]string, 0, len(recent))
	for _, msg := range recent {
		ids = append(ids, msg.ID)
	}
	return ids
}

func (m *Mailbox) verifyWithinDuration() time.Duration {
	if m.verifyWithin > 0 {
		return m.verifyWithin
	}
	return defaultVerifyWithin
}

func (m *Mailbox) staleComposerEnabled() bool {
	return !m.staleComposerOff
}

func (m *Mailbox) staleComposerAfterDuration() time.Duration {
	if m.staleComposerAfter > 0 {
		return m.staleComposerAfter
	}
	return defaultStaleComposerAfter
}

func (m *Mailbox) authErrorPatterns() []string {
	if m.authPatterns != nil {
		return m.authPatterns
	}
	return DefaultAuthErrorPatterns
}

func (m *Mailbox) isAuthOutage(handle string) bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	_, ok := m.authOutage[handle]
	return ok
}

func (m *Mailbox) draftHoldOutcome(ctx context.Context, ad adapter.Adapter, handle, reason string, state adapter.State) outcome {
	o := outcome{reason: reason, target: string(state)}
	if reason != adapter.HoldDraft {
		return o
	}
	if r, ok := adapter.AsComposerReader(ad); ok {
		if text, label, rok, err := r.ComposerInfo(ctx, handle); err == nil && rok {
			o.composerText = text
			o.composerTabLabel = label
		}
	}
	return o
}

func (m *Mailbox) updateComposerTrack(msg *message, o outcome) {
	if o.reason != adapter.HoldDraft || o.composerText == "" {
		msg.ComposerText = ""
		msg.ComposerTextSince = nil
		msg.ComposerTabLabel = ""
		msg.StaleComposerNotice = false
		return
	}
	now := m.now().UTC()
	if msg.ComposerText != o.composerText || msg.ComposerTextSince == nil {
		msg.ComposerText = o.composerText
		msg.ComposerTextSince = &now
		msg.StaleComposerNotice = false
		if m.staleComposerNoticed[msg.Handle] != o.composerText {
			delete(m.staleComposerNoticed, msg.Handle)
		}
	}
	if o.composerTabLabel != "" {
		msg.ComposerTabLabel = o.composerTabLabel
	}
}

func (m *Mailbox) staleComposerDetail(msg message) string {
	if msg.State != "held: "+adapter.HoldDraft || msg.ComposerText == "" || msg.ComposerTextSince == nil {
		return ""
	}
	if !m.staleComposerEnabled() {
		return ""
	}
	if m.now().Sub(*msg.ComposerTextSince) < m.staleComposerAfterDuration() {
		return ""
	}
	label := msg.ComposerTabLabel
	if label == "" {
		label = msg.To
	}
	return fmt.Sprintf("held by composer text in %s: %q", label, firstNRunes(msg.ComposerText, 40))
}

func firstNRunes(s string, n int) string {
	if n <= 0 {
		return ""
	}
	count := 0
	for i := range s {
		if count == n {
			return s[:i]
		}
		count++
	}
	return s
}

func (m *Mailbox) maybeStaleComposerNotice(ctx context.Context, handle string) {
	type staleItem struct {
		id     string
		from   string
		to     string
		detail string
	}
	m.mu.Lock()
	if !m.staleComposerEnabled() {
		m.mu.Unlock()
		return
	}
	var item *staleItem
	for _, id := range m.queue[handle] {
		msg := m.msgs[id]
		detail := m.staleComposerDetail(msg)
		if detail == "" || msg.StaleComposerNotice {
			continue
		}
		if m.staleComposerNoticed[handle] == msg.ComposerText {
			msg.StaleComposerNotice = true
			m.msgs[id] = msg
			continue
		}
		msg.StaleComposerNotice = true
		m.msgs[id] = msg
		m.staleComposerNoticed[handle] = msg.ComposerText
		item = &staleItem{id: msg.ID, from: msg.From, to: msg.To, detail: detail}
		break
	}
	if item != nil {
		_ = m.save()
	}
	m.mu.Unlock()
	if item == nil {
		return
	}
	if strings.HasPrefix(item.from, "anon:") {
		_ = m.logEvent(eventlog.Event{
			Event:  eventlog.EventHoldNoticeSkipped,
			Actor:  "agentbus",
			MsgID:  item.id,
			Name:   item.to,
			Detail: "anonymous sender",
			Reason: item.detail,
		})
		return
	}
	text := fmt.Sprintf("agentbus: message %s to %s %s\nagentbus status %s\n", item.id, item.to, item.detail, item.id)
	m.notifySender(ctx, item.id, item.from, text, "hold notice not sent")
}

func (m *Mailbox) attachVerifyInfo(ctx context.Context, handle string, o *outcome) {
	m.mu.Lock()
	fn := m.transcript
	off := m.verifyOff
	m.mu.Unlock()
	if fn == nil || off {
		return
	}
	ag, found := m.agentForHandle(ctx, handle)
	if !found {
		return
	}
	if _, ok := fn(ag); !ok {
		return
	}
	o.verifyPending = true
	o.verifyStatus = ag.Status
	o.verifyStateSeq = ag.StateSeq
}

func (m *Mailbox) agentForHandle(ctx context.Context, handle string) (adapter.Agent, bool) {
	ads, err := m.reg.Usable(ctx)
	if err != nil {
		return adapter.Agent{}, false
	}
	for _, ad := range ads {
		agents, err := ad.Inventory(ctx)
		if err != nil {
			continue
		}
		for _, ag := range agents {
			if ag.Handle == handle {
				return ag, true
			}
		}
	}
	return adapter.Agent{}, false
}

type outageNotice struct {
	id   string
	from string
	to   string
	text string
}

func (m *Mailbox) checkVerifications(ctx context.Context, handle string) error {
	m.mu.Lock()
	fn := m.transcript
	_, inOutage := m.authOutage[handle]
	var pendingIDs []string
	for _, msg := range m.msgs {
		if msg.Handle == handle && msg.VerifyPending {
			pendingIDs = append(pendingIDs, msg.ID)
		}
	}
	m.mu.Unlock()
	if fn == nil || (len(pendingIDs) == 0 && !inOutage) {
		return nil
	}
	ag, found := m.agentForHandle(ctx, handle)
	var turns []lastturn.Turn
	var hasTranscript bool
	if found {
		turns, hasTranscript = fn(ag)
	}

	m.mu.Lock()
	now := m.now().UTC()
	changed := false
	var notices []outageNotice

	for _, id := range pendingIDs {
		c, n := m.verifyOneLocked(handle, id, ag, found, turns, hasTranscript, now)
		if c {
			changed = true
		}
		if n != nil {
			notices = append(notices, *n)
		}
	}
	if m.checkOutageRecoveryLocked(handle, turns, hasTranscript) {
		changed = true
	}

	var saveErr error
	if changed {
		saveErr = m.save()
	}
	m.mu.Unlock()

	for _, n := range notices {
		m.notifySender(ctx, n.id, n.from, n.text, "outage notice not sent")
	}
	return saveErr
}

func (m *Mailbox) verifyOneLocked(handle, id string, ag adapter.Agent, found bool, turns []lastturn.Turn, hasTranscript bool, now time.Time) (bool, *outageNotice) {
	msg, ok := m.msgs[id]
	if !ok || !msg.VerifyPending {
		return false, nil
	}
	patterns := m.authErrorPatterns()
	if hasTranscript {
		if userIdx := lastUserTurnWithID(turns, msg.ID); userIdx >= 0 {
			if reply, hasReply := firstAssistantAfter(turns, userIdx); hasReply {
				return true, m.finishReplyVerifyLocked(handle, msg, reply, patterns, len(turns), now)
			}
		}
	}
	changed := false
	if found && (ag.Status != msg.VerifyStatus || ag.StateSeq != msg.VerifyStateSeq) && !msg.VerifyStateChanged {
		msg.VerifyStateChanged = true
		m.msgs[id] = msg
		changed = true
	}
	if msg.VerifyDeadline != nil && !now.Before(*msg.VerifyDeadline) {
		m.finishTimeoutVerifyLocked(msg, turns, hasTranscript)
		changed = true
	}
	return changed, nil
}

func (m *Mailbox) finishReplyVerifyLocked(handle string, msg message, reply string, patterns []string, turnCount int, now time.Time) *outageNotice {
	msg.VerifyPending = false
	if !matchesAuthError(reply, patterns) {
		m.msgs[msg.ID] = msg
		delete(m.authOutage, handle)
		return nil
	}
	msg.State = stateDeliveredUnprocessed
	msg.UnprocessedReason = ReasonAuth
	m.msgs[msg.ID] = msg
	_ = m.logEvent(eventlog.Event{
		Event:       eventlog.EventUnprocessed,
		Actor:       "agentbus",
		MsgID:       msg.ID,
		Name:        msg.To,
		Detail:      "assistant replied with auth error",
		Reason:      ReasonAuth,
		TargetState: stateDeliveredUnprocessed,
	})
	if n, ok := m.recordAuthOutageLocked(handle, msg, turnCount, now); ok {
		return &n
	}
	return nil
}

func (m *Mailbox) finishTimeoutVerifyLocked(msg message, turns []lastturn.Turn, hasTranscript bool) {
	msg.VerifyPending = false
	if !msg.VerifyStateChanged && (!hasTranscript || lastUserTurnWithID(turns, msg.ID) < 0) {
		msg.State = stateDeliveredUnprocessed
		msg.UnprocessedReason = ReasonNoTurn
		_ = m.logEvent(eventlog.Event{
			Event:       eventlog.EventUnprocessed,
			Actor:       "agentbus",
			MsgID:       msg.ID,
			Name:        msg.To,
			Detail:      "no turn within verification window",
			Reason:      ReasonNoTurn,
			TargetState: stateDeliveredUnprocessed,
		})
	}
	m.msgs[msg.ID] = msg
}

func (m *Mailbox) checkOutageRecoveryLocked(handle string, turns []lastturn.Turn, hasTranscript bool) bool {
	outage, active := m.authOutage[handle]
	if !active || !hasTranscript || len(turns) <= outage.TurnsAtOutage {
		return false
	}
	patterns := m.authErrorPatterns()
	for _, turn := range turns[outage.TurnsAtOutage:] {
		if turn.Role == "assistant" && !matchesAuthError(turn.Text, patterns) {
			delete(m.authOutage, handle)
			return true
		}
	}
	return false
}

func (m *Mailbox) recordAuthOutageLocked(handle string, msg message, turnCount int, now time.Time) (outageNotice, bool) {
	rec, exists := m.authOutage[handle]
	if !exists {
		rec = authOutageRecord{
			Since:         now,
			TurnsAtOutage: turnCount,
			Noticed:       map[string]bool{},
		}
	} else {
		rec.TurnsAtOutage = turnCount
		if rec.Noticed == nil {
			rec.Noticed = map[string]bool{}
		}
	}
	var notice outageNotice
	send := false
	if msg.From != "" && !strings.HasPrefix(msg.From, "anon:") && !rec.Noticed[msg.From] {
		rec.Noticed[msg.From] = true
		target := msg.To
		if target == "" {
			target = handle
		}
		notice = outageNotice{
			id:   msg.ID,
			from: msg.From,
			to:   msg.To,
			text: fmt.Sprintf("%s is not processing messages (login expired)\n", target),
		}
		send = true
	}
	m.authOutage[handle] = rec
	return notice, send
}

func lastUserTurnWithID(turns []lastturn.Turn, id string) int {
	for i := len(turns) - 1; i >= 0; i-- {
		if turns[i].Role == "user" && strings.Contains(turns[i].Text, id) {
			return i
		}
	}
	return -1
}

func firstAssistantAfter(turns []lastturn.Turn, userIdx int) (string, bool) {
	for i := userIdx + 1; i < len(turns); i++ {
		if turns[i].Role == "assistant" {
			return turns[i].Text, true
		}
	}
	return "", false
}

func matchesAuthError(text string, patterns []string) bool {
	lower := strings.ToLower(text)
	for _, p := range patterns {
		if p != "" && strings.Contains(lower, strings.ToLower(p)) {
			return true
		}
	}
	return false
}

// ReleaseHeld clears any login-expired hold on target and flushes its queue.
func (m *Mailbox) ReleaseHeld(ctx context.Context, target string) (int, error) {
	handle, err := m.resolveTarget(target)
	if err != nil {
		return 0, err
	}
	m.mu.Lock()
	delete(m.authOutage, handle)
	released := 0
	for _, id := range m.queue[handle] {
		msg := m.msgs[id]
		if msg.State == "held: "+HoldLoginExpired {
			msg.State = stateQueued
			m.msgs[id] = msg
			released++
		}
	}
	if err := m.save(); err != nil {
		m.mu.Unlock()
		return 0, err
	}
	m.mu.Unlock()
	if err := m.flushHandle(ctx, handle); err != nil {
		return released, err
	}
	return released, nil
}

func (m *Mailbox) resolveTarget(target string) (string, error) {
	if handle, err := m.reg.Resolve(target); err == nil {
		return handle, nil
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	for _, msg := range m.msgs {
		if msg.To == target || msg.Handle == target {
			return msg.Handle, nil
		}
	}
	return "", fmt.Errorf("unknown target %q", target)
}

// ReplayOptions filters which delivered-unprocessed messages Replay resends.
type ReplayOptions struct {
	To     string
	Since  time.Time
	Reason string
	DryRun bool
	Force  bool
}

// ReplayEntry describes one replayed (or dry-run) message.
type ReplayEntry struct {
	OriginalID string `json:"originalId"`
	NewID      string `json:"newId,omitempty"`
	To         string `json:"to"`
	Reason     string `json:"reason"`
	State      string `json:"state,omitempty"`
}

// Replay resends delivered-unprocessed messages matching opt in original order.
func (m *Mailbox) Replay(ctx context.Context, opt ReplayOptions) ([]ReplayEntry, error) {
	if opt.Reason != "" && opt.Reason != ReasonAuth && opt.Reason != ReasonNoTurn {
		return nil, fmt.Errorf("invalid reason %q: want %s or %s", opt.Reason, ReasonAuth, ReasonNoTurn)
	}
	var resolvedTo string
	if opt.To != "" {
		if h, err := m.reg.Resolve(opt.To); err == nil {
			resolvedTo = h
		}
	}
	m.mu.Lock()
	candidates := m.filterReplayCandidatesLocked(opt, resolvedTo)
	if opt.DryRun {
		m.mu.Unlock()
		return dryRunEntries(candidates), nil
	}
	now := m.now().UTC()
	for _, orig := range candidates {
		orig.ReplayedAt = &now
		m.msgs[orig.ID] = orig
	}
	if err := m.save(); err != nil {
		m.mu.Unlock()
		return nil, err
	}
	m.mu.Unlock()
	return m.executeReplay(ctx, candidates)
}

func (m *Mailbox) filterReplayCandidatesLocked(opt ReplayOptions, resolvedTo string) []message {
	var candidates []message
	for _, msg := range m.msgs {
		if !matchesReplayFilter(msg, opt, resolvedTo) {
			continue
		}
		candidates = append(candidates, msg)
	}
	sortMessages(candidates)
	return candidates
}

func matchesReplayFilter(msg message, opt ReplayOptions, resolvedTo string) bool {
	if msg.State != stateDeliveredUnprocessed {
		return false
	}
	if !opt.Force && msg.ReplayedAt != nil {
		return false
	}
	if opt.To != "" && msg.To != opt.To && (resolvedTo == "" || msg.Handle != resolvedTo) {
		return false
	}
	if !opt.Since.IsZero() && msg.SentAt.Before(opt.Since) {
		return false
	}
	if opt.Reason != "" && msg.UnprocessedReason != opt.Reason {
		return false
	}
	return true
}

func dryRunEntries(candidates []message) []ReplayEntry {
	entries := make([]ReplayEntry, 0, len(candidates))
	for _, msg := range candidates {
		entries = append(entries, ReplayEntry{
			OriginalID: msg.ID,
			To:         msg.To,
			Reason:     msg.UnprocessedReason,
		})
	}
	return entries
}

func (m *Mailbox) executeReplay(ctx context.Context, candidates []message) ([]ReplayEntry, error) {
	entries := make([]ReplayEntry, 0, len(candidates))
	for _, orig := range candidates {
		id, err := newID()
		if err != nil {
			return entries, err
		}
		target := orig.To
		if target == "" {
			target = orig.Handle
		}
		handle, err := m.reg.Resolve(target)
		if err != nil {
			handle = orig.Handle
		}
		msg := message{
			ID:           id,
			From:         orig.From,
			To:           orig.To,
			Re:           orig.Re,
			ResendOf:     orig.ID,
			SentAt:       m.now().UTC(),
			Body:         orig.Body,
			Replyable:    orig.Replyable,
			Handle:       handle,
			SenderHandle: orig.SenderHandle,
		}
		res, err := m.enqueue(ctx, msg, Options{})
		if err != nil {
			return entries, err
		}
		entries = append(entries, ReplayEntry{
			OriginalID: orig.ID,
			NewID:      res.ID,
			To:         orig.To,
			Reason:     orig.UnprocessedReason,
			State:      res.State,
		})
	}
	return entries, nil
}
