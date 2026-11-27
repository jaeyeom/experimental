package mailbox

import (
	"cmp"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// HeldFilter filters held messages returned by HeldMessages.
type HeldFilter struct {
	OlderThan  time.Duration
	Reasons    []string
	NotReasons []string
	To         string
}

// HeldMessage describes one message currently in a held state.
type HeldMessage struct {
	ID         string `json:"id"`
	From       string `json:"from"`
	To         string `json:"to"`
	Reason     string `json:"reason"`
	ReasonCode string `json:"reason_code"`           //nolint:tagliatelle // CLI JSON shape
	HeldDetail string `json:"held_detail,omitempty"` //nolint:tagliatelle // CLI JSON shape
	HeldSince  string `json:"held_since"`            //nolint:tagliatelle // CLI JSON shape
	Age        string `json:"age"`
	AgeSeconds int64  `json:"age_seconds"` //nolint:tagliatelle // CLI JSON shape
}

// AskFilter filters unanswered asks returned by Asks.
type AskFilter struct {
	Unanswered time.Duration
	From       string
	To         string
}

// Ask describes one delivered message expecting a reply that has none yet.
type Ask struct {
	ID         string `json:"id"`
	From       string `json:"from"`
	To         string `json:"to"`
	Sent       string `json:"sent"`
	Age        string `json:"age"`
	AgeSeconds int64  `json:"age_seconds"` //nolint:tagliatelle // CLI JSON shape
	FirstLine  string `json:"first_line"`  //nolint:tagliatelle // CLI JSON shape
	Rule       string `json:"rule"`
}

// SetExpectReplySenders sets sender names, handles, or globs whose delivered
// messages expect a reply unless already replied to.
func (m *Mailbox) SetExpectReplySenders(patterns []string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.expectReplySenders = slices.Clone(patterns)
}

func (m *Mailbox) resolveOptional(target string) string {
	if target == "" || m.reg == nil {
		return ""
	}
	if h, err := m.reg.Resolve(target); err == nil {
		return h
	}
	return ""
}

// HeldMessages returns held messages matching f, ordered longest-held first.
func (m *Mailbox) HeldMessages(f HeldFilter) []HeldMessage {
	resolvedTo := m.resolveOptional(f.To)
	m.mu.Lock()
	now := m.now().UTC()
	type item struct {
		since time.Time
		msg   HeldMessage
	}
	var items []item
	for _, msg := range m.msgs {
		hm, since, ok := matchHeldMessage(msg, f, resolvedTo, now)
		if !ok {
			continue
		}
		items = append(items, item{since: since, msg: hm})
	}
	m.mu.Unlock()
	slices.SortFunc(items, func(a, b item) int {
		return cmp.Or(a.since.Compare(b.since), cmp.Compare(a.msg.ID, b.msg.ID))
	})
	out := make([]HeldMessage, len(items))
	for i, it := range items {
		out[i] = it.msg
	}
	return out
}

func matchHeldMessage(msg message, f HeldFilter, resolvedTo string, now time.Time) (HeldMessage, time.Time, bool) {
	rawReason, held := strings.CutPrefix(msg.State, "held: ")
	if !held {
		return HeldMessage{}, time.Time{}, false
	}
	since := heldSince(msg)
	age := max(now.Sub(since), 0)
	if f.OlderThan > 0 && age < f.OlderThan {
		return HeldMessage{}, time.Time{}, false
	}
	if !matchesRecipient(msg, f.To, resolvedTo) {
		return HeldMessage{}, time.Time{}, false
	}
	code := HeldReasonCode(rawReason)
	if (len(f.Reasons) > 0 && !slices.Contains(f.Reasons, code)) || slices.Contains(f.NotReasons, code) {
		return HeldMessage{}, time.Time{}, false
	}
	return HeldMessage{
		ID:         msg.ID,
		From:       msg.From,
		To:         msg.To,
		Reason:     rawReason,
		ReasonCode: code,
		HeldDetail: msg.HeldDetail,
		HeldSince:  ReportTime(since),
		Age:        waitString(age),
		AgeSeconds: int64(age / time.Second),
	}, since, true
}

func heldSince(msg message) time.Time {
	if msg.HeldAt != nil {
		return *msg.HeldAt
	}
	return msg.SentAt
}

func matchesRecipient(msg message, to, resolvedTo string) bool {
	if to == "" {
		return true
	}
	return msg.To == to || msg.Handle == to || (resolvedTo != "" && msg.Handle == resolvedTo)
}

// HeldReasonCode maps a stored hold reason text to a stable reason code.
func HeldReasonCode(reason string) string {
	switch {
	case unreachableState("held: " + reason):
		return "unreachable"
	case strings.HasPrefix(reason, "waiting for an earlier message "):
		return "ordering"
	}
	if code, ok := composerHoldCode(reason); ok {
		return code
	}
	if code, ok := lifecycleHoldCode(reason); ok {
		return code
	}
	return "other"
}

func composerHoldCode(reason string) (string, bool) {
	switch reason {
	case adapter.HoldDraft:
		return "draft", true
	case adapter.HoldDraftUnknown:
		return "draft-unknown", true
	case adapter.HoldScrolled:
		return "scrolled", true
	case adapter.HoldFocused:
		return "focused", true
	case adapter.HoldAmbiguous:
		return "ambiguous", true
	case adapter.HoldAmbiguousUnverified:
		return "ambiguous-unverified", true
	case "transport refused":
		return "transport-refused", true
	default:
		return "", false
	}
}

func lifecycleHoldCode(reason string) (string, bool) {
	switch reason {
	case HoldLoginExpired:
		return "login-expired", true
	case HoldHibernated:
		return "hibernated", true
	case HoldWaking:
		return "waking", true
	case HoldWakeFailed:
		return "wake-failed", true
	case HoldPaneGone:
		return "pane-gone", true
	case HoldParked:
		return "parked", true
	case string(adapter.StateBlocked), string(adapter.StateNoAgent), string(adapter.StateUnknown), string(adapter.StateGone):
		return reason, true
	default:
		return "", false
	}
}

// Asks returns delivered messages expecting a reply that have none matching f,
// ordered oldest first.
func (m *Mailbox) Asks(f AskFilter) []Ask {
	resolvedFrom := m.resolveOptional(f.From)
	resolvedTo := m.resolveOptional(f.To)
	m.mu.Lock()
	now := m.now().UTC()
	replied := m.repliedIDsLocked()
	senders := slices.Clone(m.expectReplySenders)
	type askItem struct {
		sent time.Time
		ask  Ask
	}
	var items []askItem
	for _, msg := range m.msgs {
		ask, ok := matchAsk(msg, f, resolvedFrom, resolvedTo, replied, senders, now)
		if !ok {
			continue
		}
		items = append(items, askItem{sent: msg.SentAt, ask: ask})
	}
	m.mu.Unlock()
	slices.SortFunc(items, func(a, b askItem) int {
		return cmp.Or(a.sent.Compare(b.sent), cmp.Compare(a.ask.ID, b.ask.ID))
	})
	out := make([]Ask, len(items))
	for i, it := range items {
		out[i] = it.ask
	}
	return out
}

func (m *Mailbox) repliedIDsLocked() map[string]bool {
	replied := map[string]bool{}
	for _, msg := range m.msgs {
		if msg.Re != "" && msg.State != stateExpired {
			replied[msg.Re] = true
		}
	}
	return replied
}

func matchAsk(msg message, f AskFilter, resolvedFrom, resolvedTo string, replied map[string]bool, senders []string, now time.Time) (Ask, bool) {
	if msg.State != stateDelivered || replied[msg.ID] {
		return Ask{}, false
	}
	if !matchesSenderFilter(msg, f.From, resolvedFrom) || !matchesRecipient(msg, f.To, resolvedTo) {
		return Ask{}, false
	}
	age := max(now.Sub(msg.SentAt), 0)
	if f.Unanswered > 0 && age < f.Unanswered {
		return Ask{}, false
	}
	rule, ok := askRule(msg, senders)
	if !ok {
		return Ask{}, false
	}
	return Ask{
		ID:         msg.ID,
		From:       msg.From,
		To:         msg.To,
		Sent:       ReportTime(msg.SentAt),
		Age:        waitString(age),
		AgeSeconds: int64(age / time.Second),
		FirstLine:  firstLine(msg.Body),
		Rule:       rule,
	}, true
}

func matchesSenderFilter(msg message, from, resolvedFrom string) bool {
	if from == "" {
		return true
	}
	return msg.From == from || msg.SenderHandle == from || slices.Contains(msg.FromNames, from) ||
		(resolvedFrom != "" && msg.SenderHandle == resolvedFrom)
}

func askRule(msg message, senderPatterns []string) (string, bool) {
	if msg.ExpectReply {
		return "explicit", true
	}
	if msg.From == "agentbus" {
		return "", false
	}
	if strings.HasSuffix(strings.TrimSpace(msg.Body), "?") {
		return "question-mark", true
	}
	if matchesSenderPatterns(msg, senderPatterns) {
		return "sender-skill", true
	}
	return "", false
}

func matchesSenderPatterns(msg message, patterns []string) bool {
	if len(patterns) == 0 {
		return false
	}
	keys := make([]string, 0, 2+len(msg.FromNames))
	if msg.From != "" {
		keys = append(keys, msg.From)
	}
	if msg.SenderHandle != "" {
		keys = append(keys, msg.SenderHandle)
	}
	keys = append(keys, msg.FromNames...)
	for _, pat := range patterns {
		re := procinfo.Glob(pat, true)
		if slices.ContainsFunc(keys, re.MatchString) {
			return true
		}
	}
	return false
}

func firstLine(body string) string {
	for _, line := range strings.Split(body, "\n") {
		trimmed := strings.TrimSpace(strings.TrimRight(line, "\r"))
		if trimmed != "" {
			return trimmed
		}
	}
	return ""
}
