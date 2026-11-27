package decide

import (
	"slices"
	"sort"
	"strings"
	"time"
)

// AnswerClassification describes one classified human answer or reply.
type AnswerClassification struct {
	Type           string
	AskingAgent    string
	RawAskingAgent string
	AskedAt        time.Time
	AnsweredAt     time.Time
}

// AnswerStatsOptions filters the answer statistics report.
type AnswerStatsOptions struct {
	Since    *time.Time
	Until    *time.Time
	Agent    string
	Resolver *NameResolver
}

// AnswerStatsRow summarizes one asking agent's answer breakdown.
type AnswerStatsRow struct {
	Agent                  string   `json:"agent"`
	Aliases                []string `json:"aliases,omitempty"`
	Option                 int      `json:"option"`
	FreeTextOverOptions    int      `json:"freeTextOverOptions"`
	FreeTextOnly           int      `json:"freeTextOnly"`
	Unknown                int      `json:"unknown"`
	FreeTextShare          float64  `json:"freeTextShare"`
	Total                  int      `json:"total"`
	OptionIDs              []string `json:"optionIds,omitempty"`
	FreeTextOverOptionsIDs []string `json:"freeTextOverOptionsIds,omitempty"`
	FreeTextOnlyIDs        []string `json:"freeTextOnlyIds,omitempty"`
	UnknownIDs             []string `json:"unknownIds,omitempty"`
}

func utcTimePtr(t time.Time) *time.Time {
	if t.IsZero() {
		return nil
	}
	u := t.UTC()
	return &u
}

func itemRawOwner(it *Item) string {
	if it.RawOwner != "" {
		return it.RawOwner
	}
	return it.Owner
}

func itemAskingAgent(it *Item) string {
	if it.AskingAgent != "" {
		return it.AskingAgent
	}
	return it.Owner
}

func resolveAgentWithConfig(name string, it *Item) string {
	name = strings.TrimSpace(name)
	if name == "" {
		return ""
	}
	cfg, err := LoadConfig()
	if err != nil {
		return name
	}
	return NewNameResolver(cfg, nil).Resolve(name, it)
}

func msgAsker(it *Item, m Message) (canon, raw string) {
	if m.By != "" {
		return resolveAgentWithConfig(m.By, nil), m.By
	}
	raw = itemRawOwner(it)
	canon = it.AskingAgent
	if canon == "" {
		canon = resolveAgentWithConfig(raw, it)
	}
	return canon, raw
}

func prepareAnswerMeta(it *Item, stored string) (answerType, label string, isOption bool, askingAgent, rawAskingAgent string, askedAt time.Time) {
	askingAgent, rawAskingAgent, askedAt = pendingAsker(it)
	answerType = AnswerTypeFreeTextOnly
	label, isOption = optionLabel(it.Options, stored)
	switch {
	case isOption:
		answerType = AnswerTypeOption
	case len(it.Options) > 0:
		answerType = AnswerTypeFreeTextOverOptions
	}
	return answerType, label, isOption, askingAgent, rawAskingAgent, askedAt
}

func newAnswerRecord(stored, label string, isOption bool, via, answerType, askingAgent, rawAskingAgent string, askedAt, receivedAt, writtenAt time.Time) *Answer {
	ans := &Answer{
		Value:          stored,
		AnsweredBy:     Human,
		Via:            via,
		AnswerType:     answerType,
		AskingAgent:    askingAgent,
		RawAskingAgent: rawAskingAgent,
		AskedAt:        utcTimePtr(askedAt),
		AnsweredAt:     utcTimePtr(receivedAt),
	}
	if !writtenAt.IsZero() {
		w := writtenAt.UTC()
		r := receivedAt
		ans.WrittenAt = &w
		ans.ReceivedAt = &r
	}
	if isOption {
		ans.Key, ans.Label = stored, label
	}
	return ans
}

func newAnswerMessage(stored, label string, isOption bool, via, answerType, askingAgent, rawAskingAgent string, askedAt, receivedAt, writtenAt time.Time) Message {
	msg := Message{
		Author:         Human,
		Time:           receivedAt,
		Text:           stored,
		Via:            via,
		AnswerType:     answerType,
		AskingAgent:    askingAgent,
		RawAskingAgent: rawAskingAgent,
		AskedAt:        utcTimePtr(askedAt),
	}
	if !writtenAt.IsZero() {
		w := writtenAt.UTC()
		r := receivedAt
		msg.WrittenAt = &w
		msg.ReceivedAt = &r
	}
	if isOption {
		msg.OptionKey, msg.OptionLabel = stored, label
		msg.Text = keyLabel(stored, label)
	}
	return msg
}

// pendingAsker returns the canonical asking agent, raw name as filed, and
// question time for an open question on it. When options are pending, it uses
// the newest message with options; otherwise it uses the newest agent message.
// It falls back to Owner and CreatedAt.
func pendingAsker(it *Item) (string, string, time.Time) {
	if len(it.Options) > 0 {
		for i := len(it.Messages) - 1; i >= 0; i-- {
			m := it.Messages[i]
			if len(m.Options) > 0 {
				canon, raw := msgAsker(it, m)
				return canon, raw, m.Time.UTC()
			}
		}
	} else {
		for i := len(it.Messages) - 1; i >= 0; i-- {
			m := it.Messages[i]
			if m.Author != Human && m.Author != "system" {
				canon, raw := msgAsker(it, m)
				return canon, raw, m.Time.UTC()
			}
		}
	}
	raw := itemRawOwner(it)
	canon := it.AskingAgent
	if canon == "" {
		canon = resolveAgentWithConfig(raw, it)
	}
	return canon, raw, it.CreatedAt.UTC()
}

// ClassifyMessage returns the classification of human message idx on it,
// using recorded fields when present or deriving from legacy fields.
// Non-human messages and thread-opening human messages (idx == 0 without
// AnswerType) return a zero classification.
func ClassifyMessage(it *Item, idx int) AnswerClassification {
	if it == nil || idx < 0 || idx >= len(it.Messages) {
		return AnswerClassification{}
	}
	m := it.Messages[idx]
	if m.Author != Human {
		return AnswerClassification{}
	}
	if m.AnswerType != "" {
		return recordedMessageClassification(it, m)
	}
	if idx == 0 {
		return AnswerClassification{}
	}
	return deriveLegacyMessageClassification(it, idx)
}

func recordedMessageClassification(it *Item, m Message) AnswerClassification {
	agent := m.AskingAgent
	raw := m.RawAskingAgent
	if m.AnswerType != AnswerTypeReplyNoQuestion {
		if agent == "" {
			agent = itemAskingAgent(it)
		}
		if raw == "" {
			if m.AskingAgent == "" || (it.RawOwner != "" && (m.AskingAgent == it.AskingAgent || m.AskingAgent == it.Owner)) {
				raw = itemRawOwner(it)
			} else {
				raw = agent
			}
		}
	}
	var askedAt time.Time
	if m.AskedAt != nil {
		askedAt = m.AskedAt.UTC()
	}
	return AnswerClassification{
		Type:           m.AnswerType,
		AskingAgent:    agent,
		RawAskingAgent: raw,
		AskedAt:        askedAt,
		AnsweredAt:     m.Time.UTC(),
	}
}

// ClassifyAnswer returns the classification of it.Answer, using recorded
// fields when present or deriving from legacy fields.
func ClassifyAnswer(it *Item) AnswerClassification {
	if it == nil || it.Answer == nil {
		return AnswerClassification{}
	}
	if it.Answer.AnswerType != "" {
		return recordedAnswerClassification(it)
	}
	if len(it.Messages) > 0 {
		if idx := answerMessageIndex(it); idx >= 0 {
			return ClassifyMessage(it, idx)
		}
	}
	return deriveUnmessagedAnswer(it)
}

func recordedAnswerClassification(it *Item) AnswerClassification {
	askedAt := it.CreatedAt.UTC()
	if it.Answer.AskedAt != nil {
		askedAt = it.Answer.AskedAt.UTC()
	}
	answeredAt := it.AnsweredAt.UTC()
	if it.Answer.AnsweredAt != nil {
		answeredAt = it.Answer.AnsweredAt.UTC()
	}
	agent := it.Answer.AskingAgent
	raw := it.Answer.RawAskingAgent
	if it.Answer.AnswerType != AnswerTypeReplyNoQuestion {
		if agent == "" {
			agent = itemAskingAgent(it)
		}
		if raw == "" {
			if it.Answer.AskingAgent == "" || (it.RawOwner != "" && (it.Answer.AskingAgent == it.AskingAgent || it.Answer.AskingAgent == it.Owner)) {
				raw = itemRawOwner(it)
			} else {
				raw = agent
			}
		}
	}
	return AnswerClassification{
		Type:           it.Answer.AnswerType,
		AskingAgent:    agent,
		RawAskingAgent: raw,
		AskedAt:        askedAt,
		AnsweredAt:     answeredAt,
	}
}

func deriveUnmessagedAnswer(it *Item) AnswerClassification {
	answeredAt := it.AnsweredAt.UTC()
	askedAt := it.CreatedAt.UTC()
	agent := itemAskingAgent(it)
	raw := itemRawOwner(it)
	typ := AnswerTypeUnknown
	switch {
	case it.Answer.Key != "":
		typ = AnswerTypeOption
	case len(it.Options) > 0:
		if _, ok := optionLabel(it.Options, strings.TrimSpace(it.Answer.Value)); ok && it.Answer.Via == "" {
			typ = AnswerTypeOption
		} else {
			typ = AnswerTypeFreeTextOverOptions
		}
	case it.FreeText && it.Kind == KindRelay:
		typ = AnswerTypeFreeTextOnly
	}
	return AnswerClassification{Type: typ, AskingAgent: agent, RawAskingAgent: raw, AskedAt: askedAt, AnsweredAt: answeredAt}
}

type legacyThreadState struct {
	pendingOpts []Option
	hasPending  bool
	answered    bool
	askAgent    string
	rawAgent    string
	askAt       time.Time
}

func (st *legacyThreadState) stepAgent(it *Item, pm Message, isFirst bool) {
	setAsker := func() {
		if pm.By != "" {
			st.askAgent = pm.By
			st.rawAgent = pm.By
		} else {
			st.askAgent = itemAskingAgent(it)
			st.rawAgent = itemRawOwner(it)
		}
		st.askAt = pm.Time.UTC()
	}
	if len(pm.Options) > 0 {
		st.pendingOpts = pm.Options
		st.hasPending = true
		st.answered = false
		setAsker()
		return
	}
	if isFirst || st.answered || !st.hasPending || len(st.pendingOpts) == 0 {
		st.pendingOpts = nil
		st.hasPending = true
		st.answered = false
		setAsker()
	}
}

func legacyStateBefore(it *Item, idx int) legacyThreadState {
	var st legacyThreadState
	for j := 0; j < idx; j++ {
		pm := it.Messages[j]
		switch {
		case pm.Author == "system":
		case pm.Author != Human:
			st.stepAgent(it, pm, j == 0)
		case j > 0:
			st.answered = true
		}
	}
	if !hasAnyMessageOptions(it.Messages) && !st.answered && st.hasPending && len(st.pendingOpts) == 0 && len(it.Options) > 0 {
		st.pendingOpts = it.Options
	}
	if st.askAgent == "" {
		st.askAgent = itemAskingAgent(it)
		st.rawAgent = itemRawOwner(it)
	}
	if st.askAt.IsZero() {
		st.askAt = it.CreatedAt.UTC()
	}
	return st
}

func hasAnyMessageOptions(msgs []Message) bool {
	for i := range msgs {
		if len(msgs[i].Options) > 0 {
			return true
		}
	}
	return false
}

func hasLaterAgentMessage(msgs []Message, idx int) bool {
	for j := idx + 1; j < len(msgs); j++ {
		pm := msgs[j]
		if pm.Author != Human && pm.Author != "system" {
			return true
		}
	}
	return false
}

func matchesPendingOptionLegacy(it *Item, m Message, pendingOpts []Option, matchesAnswer bool) bool {
	if m.Via != "" {
		return false
	}
	if _, ok := optionLabel(pendingOpts, strings.TrimSpace(m.Text)); ok {
		return true
	}
	if matchesAnswer {
		if _, ok := optionLabel(pendingOpts, strings.TrimSpace(it.Answer.Value)); ok {
			return true
		}
	}
	return false
}

func deriveLegacyMessageClassification(it *Item, idx int) AnswerClassification {
	m := it.Messages[idx]
	answeredAt := m.Time.UTC()
	st := legacyStateBefore(it, idx)
	hasLaterAgent := hasLaterAgentMessage(it.Messages, idx)
	matchesAnswer := messageMatchesAnswer(it, idx, hasLaterAgent)

	if m.OptionKey != "" || (matchesAnswer && it.Answer.Key != "") {
		return AnswerClassification{Type: AnswerTypeOption, AskingAgent: st.askAgent, RawAskingAgent: st.rawAgent, AskedAt: st.askAt, AnsweredAt: answeredAt}
	}
	if len(st.pendingOpts) > 0 && !st.answered {
		if matchesPendingOptionLegacy(it, m, st.pendingOpts, matchesAnswer) {
			return AnswerClassification{Type: AnswerTypeOption, AskingAgent: st.askAgent, RawAskingAgent: st.rawAgent, AskedAt: st.askAt, AnsweredAt: answeredAt}
		}
		return AnswerClassification{Type: AnswerTypeFreeTextOverOptions, AskingAgent: st.askAgent, RawAskingAgent: st.rawAgent, AskedAt: st.askAt, AnsweredAt: answeredAt}
	}
	if st.answered || it.EffectiveClass() == ClassNote || it.Kind == KindTypeIn {
		return AnswerClassification{Type: AnswerTypeReplyNoQuestion, AnsweredAt: answeredAt}
	}
	if !hasLaterAgent && len(it.Options) == 0 && it.Kind == KindRelay {
		if it.FreeText {
			return AnswerClassification{Type: AnswerTypeFreeTextOnly, AskingAgent: st.askAgent, RawAskingAgent: st.rawAgent, AskedAt: st.askAt, AnsweredAt: answeredAt}
		}
		if it.Answer == nil {
			return AnswerClassification{Type: AnswerTypeReplyNoQuestion, AnsweredAt: answeredAt}
		}
	}
	return AnswerClassification{Type: AnswerTypeUnknown, AskingAgent: st.askAgent, RawAskingAgent: st.rawAgent, AskedAt: st.askAt, AnsweredAt: answeredAt}
}

func messageMatchesAnswer(it *Item, idx int, hasLaterAgent bool) bool {
	if it.Answer == nil {
		return false
	}
	m := it.Messages[idx]
	if !it.AnsweredAt.IsZero() {
		return m.Time.Equal(it.AnsweredAt)
	}
	if m.OptionKey != "" && m.OptionKey == it.Answer.Value {
		return true
	}
	if m.Text == it.Answer.Value {
		return true
	}
	if it.Answer.Key != "" && m.Text == keyLabel(it.Answer.Key, it.Answer.Label) {
		return true
	}
	return !hasLaterAgent && idx == lastHumanMessageIndex(it)
}

func answerMessageIndex(it *Item) int {
	if it.Answer == nil {
		return -1
	}
	for i := len(it.Messages) - 1; i >= 0; i-- {
		if it.Messages[i].Author != Human {
			continue
		}
		if messageMatchesAnswer(it, i, hasLaterAgentMessage(it.Messages, i)) {
			return i
		}
	}
	return -1
}

func lastHumanMessageIndex(it *Item) int {
	for i := len(it.Messages) - 1; i >= 0; i-- {
		if it.Messages[i].Author == Human {
			return i
		}
	}
	return -1
}

type answerBucket struct {
	row     AnswerStatsRow
	aliases map[string]bool
	optSeen map[string]bool
	ftoSeen map[string]bool
	ftSeen  map[string]bool
	unkSeen map[string]bool
}

type answerAggregator struct {
	opts       AnswerStatsOptions
	buckets    map[string]*answerBucket
	rawToCanon map[string]string
	ambigRaw   map[string]bool
}

func (a *answerAggregator) bucketFor(agent string) *answerBucket {
	b, ok := a.buckets[agent]
	if !ok {
		b = &answerBucket{
			row:     AnswerStatsRow{Agent: agent},
			aliases: map[string]bool{},
			optSeen: map[string]bool{},
			ftoSeen: map[string]bool{},
			ftSeen:  map[string]bool{},
			unkSeen: map[string]bool{},
		}
		a.buckets[agent] = b
	}
	return b
}

func addUniqueID(ids *[]string, seen map[string]bool, id string) {
	if id == "" || seen[id] {
		return
	}
	seen[id] = true
	*ids = append(*ids, id)
}

func (a *answerAggregator) inWindow(at time.Time) bool {
	if (a.opts.Since != nil || a.opts.Until != nil) && at.IsZero() {
		return false
	}
	if a.opts.Since != nil && at.Before(*a.opts.Since) {
		return false
	}
	if a.opts.Until != nil && at.After(*a.opts.Until) {
		return false
	}
	return true
}

func (a *answerAggregator) resolveAskingAgent(it *Item, c AnswerClassification) (canon, raw string) {
	canon = strings.TrimSpace(c.AskingAgent)
	raw = strings.TrimSpace(c.RawAskingAgent)
	if raw == "" {
		raw = canon
	}
	if canon == "" {
		return AnswerTypeUnknown, ""
	}
	if a.opts.Resolver != nil {
		// Only let it.NotifyPane identify the item's owner/askingAgent, not a different worker's Message.By.
		var itemCtx *Item
		if it != nil && (raw == it.Owner || raw == it.RawOwner || canon == it.Owner || canon == it.AskingAgent) {
			itemCtx = it
		}
		canon = a.opts.Resolver.Resolve(canon, itemCtx)
	}
	return canon, raw
}

func (a *answerAggregator) noteRawMapping(raw, canon string) {
	if raw == "" || canon == "" || raw == canon || a.ambigRaw[raw] {
		return
	}
	if prev, ok := a.rawToCanon[raw]; ok && prev != canon {
		delete(a.rawToCanon, raw)
		a.ambigRaw[raw] = true
		return
	}
	a.rawToCanon[raw] = canon
}

func (a *answerAggregator) record(it *Item, c AnswerClassification) {
	if c.Type == "" || c.Type == AnswerTypeReplyNoQuestion {
		return
	}
	agent, raw := a.resolveAskingAgent(it, c)
	if agent != AnswerTypeUnknown {
		a.noteRawMapping(raw, agent)
	}
	if !a.inWindow(c.AnsweredAt) {
		return
	}
	b := a.bucketFor(agent)
	if raw != "" && raw != agent {
		b.aliases[raw] = true
	}
	id := ""
	if it != nil {
		id = it.ID
	}
	switch c.Type {
	case AnswerTypeOption:
		b.row.Option++
		addUniqueID(&b.row.OptionIDs, b.optSeen, id)
	case AnswerTypeFreeTextOverOptions:
		b.row.FreeTextOverOptions++
		addUniqueID(&b.row.FreeTextOverOptionsIDs, b.ftoSeen, id)
	case AnswerTypeFreeTextOnly:
		b.row.FreeTextOnly++
		addUniqueID(&b.row.FreeTextOnlyIDs, b.ftSeen, id)
	default:
		b.row.Unknown++
		addUniqueID(&b.row.UnknownIDs, b.unkSeen, id)
	}
}

func (a *answerAggregator) matchesAgentFilter(r AnswerStatsRow, wantRaw, wantCanon string) bool {
	if wantRaw == "" {
		return true
	}
	if r.Agent == wantCanon || r.Agent == wantRaw {
		return true
	}
	return slices.Contains(r.Aliases, wantRaw)
}

func (a *answerAggregator) rows() []AnswerStatsRow {
	wantRaw := strings.TrimSpace(a.opts.Agent)
	wantCanon := wantRaw
	if wantRaw != "" {
		if a.opts.Resolver != nil {
			wantCanon = a.opts.Resolver.Resolve(wantRaw, nil)
		}
		if wantCanon == wantRaw {
			if c, ok := a.rawToCanon[wantRaw]; ok {
				wantCanon = c
			}
		}
	}
	rows := make([]AnswerStatsRow, 0, len(a.buckets))
	for _, b := range a.buckets {
		r := b.row
		if len(b.aliases) > 0 {
			r.Aliases = make([]string, 0, len(b.aliases))
			for alias := range b.aliases {
				r.Aliases = append(r.Aliases, alias)
			}
			sort.Strings(r.Aliases)
		}
		if !a.matchesAgentFilter(r, wantRaw, wantCanon) {
			continue
		}
		r.Total = r.Option + r.FreeTextOverOptions + r.FreeTextOnly + r.Unknown
		if withOpts := r.Option + r.FreeTextOverOptions; withOpts > 0 {
			r.FreeTextShare = float64(r.FreeTextOverOptions) / float64(withOpts)
		}
		rows = append(rows, r)
	}
	sort.Slice(rows, func(i, j int) bool {
		if rows[i].FreeTextShare != rows[j].FreeTextShare {
			return rows[i].FreeTextShare > rows[j].FreeTextShare
		}
		if rows[i].FreeTextOverOptions != rows[j].FreeTextOverOptions {
			return rows[i].FreeTextOverOptions > rows[j].FreeTextOverOptions
		}
		if rows[i].Total != rows[j].Total {
			return rows[i].Total > rows[j].Total
		}
		return rows[i].Agent < rows[j].Agent
	})
	return rows
}

// ComputeAnswerStats aggregates answer types per asking agent over items.
func ComputeAnswerStats(items []Item, opts AnswerStatsOptions) []AnswerStatsRow {
	agg := answerAggregator{
		opts:       opts,
		buckets:    map[string]*answerBucket{},
		rawToCanon: map[string]string{},
		ambigRaw:   map[string]bool{},
	}
	for i := range items {
		it := &items[i]
		if len(it.Messages) == 0 {
			if it.Answer != nil {
				agg.record(it, ClassifyAnswer(it))
			}
			continue
		}
		for j := range it.Messages {
			if it.Messages[j].Author == Human {
				agg.record(it, ClassifyMessage(it, j))
			}
		}
	}
	return agg.rows()
}
