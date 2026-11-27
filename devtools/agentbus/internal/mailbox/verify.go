package mailbox

import (
	"context"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lastturn"
)

const (
	knownSyntheticAuthError = "Login expired · Please run /login"
	authOutageBackoff       = time.Minute
	authOutageBackoffMax    = time.Hour
	authMatchLineMaxRunes   = 120
)

// DefaultAuthErrorPatterns identify an assistant auth-error reply.
// They match substrings on synthetic/API-error turns, or the whole trimmed
// turn when transcript metadata is absent.
var DefaultAuthErrorPatterns = []string{
	knownSyntheticAuthError,
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

// SetAuthErrorPatterns sets the patterns that identify an auth-error reply
// (substring-matched on synthetic/API-error turns, whole-turn matched when
// transcript metadata is absent). Nil uses DefaultAuthErrorPatterns.
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

func (m *Mailbox) holdForAuthOutage(handle string, deliver bool) (bool, string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	rec, ok := m.authOutage[handle]
	if !ok {
		return false, ""
	}
	if !deliver {
		return true, rec.HeldDetail
	}
	if rec.ProbeMsgID != "" {
		if probe, exists := m.msgs[rec.ProbeMsgID]; exists && probe.VerifyPending {
			return true, rec.HeldDetail
		}
	}
	if m.now().Before(rec.RetryAt) {
		return true, rec.HeldDetail
	}
	return false, ""
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
	fn, inOutage, pendingIDs := m.collectVerificationLocked(handle)
	if !inOutage && (fn == nil || len(pendingIDs) == 0) {
		return nil
	}
	ag, found := m.agentForHandle(ctx, handle)
	var turns []lastturn.Turn
	var hasTranscript bool
	if found && fn != nil {
		turns, hasTranscript = fn(ag)
	}

	m.mu.Lock()
	now := m.now().UTC()
	changed, notices := m.applyVerificationsLocked(handle, pendingIDs, ag, found, turns, hasTranscript, now)
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

func (m *Mailbox) collectVerificationLocked(handle string) (func(adapter.Agent) ([]lastturn.Turn, bool), bool, []string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	_, inOutage := m.authOutage[handle]
	var pendingIDs []string
	for _, msg := range m.msgs {
		if msg.Handle == handle && msg.VerifyPending {
			pendingIDs = append(pendingIDs, msg.ID)
		}
	}
	return m.transcript, inOutage, pendingIDs
}

func (m *Mailbox) applyVerificationsLocked(handle string, pendingIDs []string, ag adapter.Agent, found bool, turns []lastturn.Turn, hasTranscript bool, now time.Time) (bool, []outageNotice) {
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
	if m.checkOutageRecoveryLocked(handle, ag, found, turns, hasTranscript) {
		changed = true
	}
	return changed, notices
}

func (m *Mailbox) verifyOneLocked(handle, id string, ag adapter.Agent, found bool, turns []lastturn.Turn, hasTranscript bool, now time.Time) (bool, *outageNotice) {
	msg, ok := m.msgs[id]
	if !ok || !msg.VerifyPending {
		return false, nil
	}
	patterns := m.authErrorPatterns()
	if hasTranscript {
		if userIdx := lastUserTurnWithID(turns, msg.ID); userIdx >= 0 {
			if reply, replyIdx, hasReply := firstAssistantAfter(turns, userIdx); hasReply {
				return true, m.finishReplyVerifyLocked(handle, msg, ag, found, reply, replyIdx, patterns, len(turns), now)
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
		m.finishTimeoutVerifyLocked(handle, msg, turns, hasTranscript, now)
		changed = true
	}
	return changed, nil
}

func (m *Mailbox) finishReplyVerifyLocked(handle string, msg message, ag adapter.Agent, found bool, reply lastturn.Turn, replyIdx int, patterns []string, turnCount int, now time.Time) *outageNotice {
	msg.VerifyPending = false
	match, ok := matchAuthErrorTurn(reply, replyIdx, patterns)
	if !ok {
		m.msgs[msg.ID] = msg
		if _, active := m.authOutage[handle]; active {
			m.clearAuthOutageLocked(handle, "new_non_error_turn", fmt.Sprintf("probe reply succeeded at %s", formatTurnPosition(reply, replyIdx)))
		}
		return nil
	}
	heldDetail := formatAuthHeldDetail(match)
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
		HeldDetail:  heldDetail,
		TargetState: stateDeliveredUnprocessed,
	})
	if n, ok := m.recordAuthOutageLocked(handle, msg, ag, found, reply.Text, heldDetail, turnCount, now); ok {
		return &n
	}
	return nil
}

func (m *Mailbox) finishTimeoutVerifyLocked(handle string, msg message, turns []lastturn.Turn, hasTranscript bool, now time.Time) {
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
	if rec, ok := m.authOutage[handle]; ok && rec.ProbeMsgID == msg.ID {
		rec.ProbeMsgID = ""
		rec.Failures++
		rec.RetryAt = now.Add(authOutageDelay(rec.Failures))
		m.authOutage[handle] = rec
	}
}

func (m *Mailbox) checkOutageRecoveryLocked(handle string, ag adapter.Agent, found bool, turns []lastturn.Turn, hasTranscript bool) bool {
	rec, active := m.authOutage[handle]
	if !active {
		return false
	}
	activity, detail := m.outageRecoveryActivityLocked(rec, ag, found, turns, hasTranscript)
	if activity == "" {
		return false
	}
	m.clearAuthOutageLocked(handle, activity, detail)
	return true
}

func (m *Mailbox) outageRecoveryActivityLocked(rec authOutageRecord, ag adapter.Agent, found bool, turns []lastturn.Turn, hasTranscript bool) (activity, detail string) {
	if hasTranscript {
		if activity, detail := transcriptRecoveryActivity(rec, turns, m.authErrorPatterns()); activity != "" {
			return activity, detail
		}
	}
	if !found || m.probePendingLocked(rec) {
		return "", ""
	}
	return paneWorkingActivity(rec, ag)
}

func transcriptRecoveryActivity(rec authOutageRecord, turns []lastturn.Turn, patterns []string) (string, string) {
	lastTurn, lastIdx, ok := lastAssistantTurnWithIndex(turns)
	if !ok || isAuthErrorTurn(lastTurn, patterns) {
		return "", ""
	}
	pos := formatTurnPosition(lastTurn, lastIdx)
	if lastIdx >= rec.TurnsAtOutage {
		if hasUserTurnInRange(turns, rec.TurnsAtOutage, lastIdx) {
			return "new_non_error_turn", "new non-error turn at " + pos
		}
		return "new_assistant_output", "new assistant output at " + pos
	}
	if rec.LastAssistantText != "" && lastTurn.Text != rec.LastAssistantText {
		return "new_assistant_output", "assistant output updated at " + pos
	}
	return "new_non_error_turn", "latest assistant turn is normal at " + pos
}

func paneWorkingActivity(rec authOutageRecord, ag adapter.Agent) (string, string) {
	workingStatus := ag.Status == string(adapter.StateWorking)
	statusChanged := rec.OutageStatus != string(adapter.StateWorking) || (ag.StateSeq != 0 && ag.StateSeq != rec.OutageStateSeq)
	if workingStatus && statusChanged {
		return "pane_working", fmt.Sprintf("pane state changed to working (status=%s seq=%d)", ag.Status, ag.StateSeq)
	}
	if ag.Title != "" && ag.Title != rec.OutageTitle && isWorkingTitle(ag.Title) {
		return "pane_working", fmt.Sprintf("pane title changed to %q", trimSingleLine(ag.Title, authMatchLineMaxRunes))
	}
	return "", ""
}

func (m *Mailbox) probePendingLocked(rec authOutageRecord) bool {
	if rec.ProbeMsgID == "" {
		return false
	}
	probe, exists := m.msgs[rec.ProbeMsgID]
	return exists && probe.VerifyPending
}

func (m *Mailbox) clearAuthOutageLocked(handle, activity, detail string) {
	rec, ok := m.authOutage[handle]
	if !ok {
		return
	}
	delete(m.authOutage, handle)
	name := rec.TargetName
	if name == "" {
		name = m.agentName(handle)
	}
	var frontID string
	for _, id := range m.queue[handle] {
		if frontID == "" {
			frontID = id
		}
		msg := m.msgs[id]
		if msg.State == "held: "+HoldLoginExpired {
			msg.State = stateQueued
			msg.HeldDetail = ""
			m.msgs[id] = msg
		}
	}
	_ = m.logEvent(eventlog.Event{
		Event:  eventlog.EventOutageCleared,
		Actor:  "agentbus",
		MsgID:  frontID,
		Name:   name,
		Detail: detail,
		Reason: activity,
	})
}

func hasUserTurnInRange(turns []lastturn.Turn, start, end int) bool {
	if start < 0 {
		start = 0
	}
	for i := start; i < end && i < len(turns); i++ {
		if turns[i].Role == "user" {
			return true
		}
	}
	return false
}

func isWorkingTitle(title string) bool {
	for _, r := range title {
		if r >= 0x2801 && r <= 0x28ff {
			return true
		}
	}
	return strings.Contains(strings.ToLower(title), "working")
}

func (m *Mailbox) recordAuthOutageLocked(handle string, msg message, ag adapter.Agent, found bool, replyText, heldDetail string, turnCount int, now time.Time) (outageNotice, bool) {
	target := msg.To
	if target == "" {
		target = m.agentName(handle)
	}
	rec, exists := m.authOutage[handle]
	if !exists {
		rec = authOutageRecord{
			Since:         now,
			TurnsAtOutage: turnCount,
			Noticed:       map[string]bool{},
			Failures:      1,
		}
	} else {
		rec.TurnsAtOutage = turnCount
		rec.Failures++
		rec.ProbeMsgID = ""
		if rec.Noticed == nil {
			rec.Noticed = map[string]bool{}
		}
	}
	rec.HeldDetail = heldDetail
	rec.TargetName = target
	rec.LastAssistantText = replyText
	if found {
		rec.OutageStatus = ag.Status
		rec.OutageStateSeq = ag.StateSeq
		rec.OutageTitle = ag.Title
	}
	rec.RetryAt = now.Add(authOutageDelay(rec.Failures))
	var notice outageNotice
	send := false
	if msg.From != "" && !strings.HasPrefix(msg.From, "anon:") && !rec.Noticed[msg.From] {
		rec.Noticed[msg.From] = true
		notice = outageNotice{
			id:   msg.ID,
			from: msg.From,
			to:   msg.To,
			text: outageNoticeText(target),
		}
		send = true
	}
	m.authOutage[handle] = rec
	return notice, send
}

func outageNoticeText(target string) string {
	return fmt.Sprintf("%s is paused waiting for authentication\n", target)
}

func authOutageDelay(failures int) time.Duration {
	return expBackoff(authOutageBackoff, authOutageBackoffMax, failures)
}

func lastUserTurnWithID(turns []lastturn.Turn, id string) int {
	for i := len(turns) - 1; i >= 0; i-- {
		if turns[i].Role == "user" && strings.Contains(turns[i].Text, id) {
			return i
		}
	}
	return -1
}

type authErrorMatch struct {
	Pattern  string
	Line     string
	Position string
}

func firstAssistantAfter(turns []lastturn.Turn, userIdx int) (lastturn.Turn, int, bool) {
	for i := userIdx + 1; i < len(turns); i++ {
		if turns[i].Role == "assistant" {
			return turns[i], i, true
		}
	}
	return lastturn.Turn{}, -1, false
}

func lastAssistantTurnWithIndex(turns []lastturn.Turn) (lastturn.Turn, int, bool) {
	for i := len(turns) - 1; i >= 0; i-- {
		if turns[i].Role == "assistant" {
			return turns[i], i, true
		}
	}
	return lastturn.Turn{}, -1, false
}

func isAuthErrorTurn(turn lastturn.Turn, patterns []string) bool {
	_, ok := matchAuthErrorTurn(turn, -1, patterns)
	return ok
}

func matchAuthErrorTurn(turn lastturn.Turn, turnIdx int, patterns []string) (authErrorMatch, bool) {
	if len(patterns) == 0 {
		return authErrorMatch{}, false
	}
	if strings.EqualFold(strings.TrimSpace(turn.Text), knownSyntheticAuthError) {
		pat := knownSyntheticAuthError
		for _, p := range patterns {
			if p = strings.TrimSpace(p); strings.EqualFold(p, knownSyntheticAuthError) {
				pat = p
				break
			}
		}
		return authErrorMatch{
			Pattern:  pat,
			Line:     trimSingleLine(turn.Text, authMatchLineMaxRunes),
			Position: formatTurnPosition(turn, turnIdx),
		}, true
	}
	if turn.IsAPIErrorMessage || turn.Model == "<synthetic>" {
		if pat, line, ok := matchAuthErrorSubstring(turn.Text, patterns); ok {
			return authErrorMatch{
				Pattern:  pat,
				Line:     trimSingleLine(line, authMatchLineMaxRunes),
				Position: formatTurnPosition(turn, turnIdx),
			}, true
		}
		return authErrorMatch{}, false
	}
	if turn.Model != "" {
		return authErrorMatch{}, false
	}
	if pat, ok := matchWholeAuthError(turn.Text, patterns); ok {
		return authErrorMatch{
			Pattern:  pat,
			Line:     trimSingleLine(turn.Text, authMatchLineMaxRunes),
			Position: formatTurnPosition(turn, turnIdx),
		}, true
	}
	return authErrorMatch{}, false
}

func matchesAuthError(text string, patterns []string) bool {
	_, ok := matchWholeAuthError(text, patterns)
	return ok
}

func matchWholeAuthError(text string, patterns []string) (string, bool) {
	trimmed := strings.TrimSpace(text)
	if trimmed == "" {
		return "", false
	}
	for _, p := range patterns {
		p = strings.TrimSpace(p)
		if p != "" && strings.EqualFold(trimmed, p) {
			return p, true
		}
	}
	return "", false
}

func matchesAuthErrorSubstring(text string, patterns []string) bool {
	_, _, ok := matchAuthErrorSubstring(text, patterns)
	return ok
}

func matchAuthErrorSubstring(text string, patterns []string) (string, string, bool) {
	lower := strings.ToLower(text)
	for _, p := range patterns {
		if p == "" {
			continue
		}
		lp := strings.ToLower(p)
		if !strings.Contains(lower, lp) {
			continue
		}
		for _, line := range strings.Split(text, "\n") {
			if strings.Contains(strings.ToLower(line), lp) {
				return p, line, true
			}
		}
		return p, text, true
	}
	return "", "", false
}

func trimSingleLine(s string, maxRunes int) string {
	s = strings.NewReplacer("\r", " ", "\n", " ", "\t", " ").Replace(s)
	s = strings.TrimSpace(s)
	if maxRunes > 0 && utf8.RuneCountInString(s) > maxRunes {
		return firstNRunes(s, maxRunes) + "…"
	}
	return s
}

func formatTurnPosition(turn lastturn.Turn, turnIdx int) string {
	if turn.Source != "" && turn.Line > 0 {
		if turnIdx >= 0 {
			return fmt.Sprintf("transcript %s:%d (turn %d)", filepath.Base(turn.Source), turn.Line, turnIdx+1)
		}
		return fmt.Sprintf("transcript %s:%d", filepath.Base(turn.Source), turn.Line)
	}
	if turnIdx >= 0 {
		return fmt.Sprintf("transcript turn %d", turnIdx+1)
	}
	return "transcript"
}

func formatAuthHeldDetail(m authErrorMatch) string {
	return fmt.Sprintf("pattern %q matched %q at %s", m.Pattern, m.Line, m.Position)
}

// ReleaseHeld clears any login-expired hold on target and flushes its queue.
func (m *Mailbox) ReleaseHeld(ctx context.Context, target string) (int, error) {
	handle, err := m.resolveTarget(target)
	if err != nil {
		return 0, err
	}
	m.mu.Lock()
	released := 0
	for _, id := range m.queue[handle] {
		msg := m.msgs[id]
		if msg.State == "held: "+HoldLoginExpired {
			msg.State = stateQueued
			msg.HeldDetail = ""
			m.msgs[id] = msg
			released++
		}
	}
	m.clearAuthOutageLocked(handle, "manual_release", "cleared by release-held")
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
