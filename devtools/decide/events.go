package decide

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
)

const (
	eventsName = "events.jsonl"

	// EventChoice logs a human answer or free-text reply on an item that offered choices.
	EventChoice = "choice"
	// EventCause logs one or more causes explaining why choices did not fit.
	EventCause = "cause"
	// EventFlag logs a flag on an agent message or item from the inbox.
	EventFlag = "flag"
	// EventGood logs a positive thumbs-up record on an agent message or item from the inbox.
	EventGood = "good"
	// EventAskOpen logs a new ask record created from a choice or free-text answer.
	EventAskOpen = "ask_open"
	// EventAskDone logs closing an ask as done.
	EventAskDone = "ask_done"
	// EventAskDrop logs closing an ask as dropped.
	EventAskDrop = "ask_drop"

	// MisfitFlagOptionsDidNotFit marks free-text replies on items with choices.
	MisfitFlagOptionsDidNotFit = "options did not fit"
)

// CauseAnnotation is a cause annotation joined onto its target choice event when querying.
type CauseAnnotation struct {
	EventID       string    `json:"eventId"`
	Time          time.Time `json:"time"`
	Who           string    `json:"who"`
	Via           string    `json:"via,omitempty"`
	Reasons       []string  `json:"reasons"`
	Note          string    `json:"note,omitempty"`
	EarlierItemID string    `json:"earlierItemId,omitempty"`
}

// Event is one immutable append-only entry in events.jsonl.
type Event struct {
	ID              string            `json:"id"`
	Kind            string            `json:"kind"`
	Time            time.Time         `json:"time"`
	ItemID          string            `json:"itemId"`
	Who             string            `json:"who"`
	Via             string            `json:"via,omitempty"`
	AskingAgent     string            `json:"askingAgent,omitempty"`
	RawAskingAgent  string            `json:"rawAskingAgent,omitempty"`
	ChoiceKey       string            `json:"choiceKey,omitempty"`
	ChoiceLabel     string            `json:"choiceLabel,omitempty"`
	ReplyText       string            `json:"replyText,omitempty"`
	Options         []Option          `json:"options,omitempty"`
	Misfit          bool              `json:"misfit,omitempty"`
	MisfitFlag      string            `json:"misfitFlag,omitempty"`
	TargetEventID   string            `json:"targetEventId,omitempty"`
	Reasons         []string          `json:"reasons,omitempty"`
	Note            string            `json:"note,omitempty"`
	EarlierItemID   string            `json:"earlierItemId,omitempty"`
	MessageIndex    *int              `json:"messageIndex,omitempty"`
	MessageTime     *time.Time        `json:"messageTime,omitempty"`
	MessageText     string            `json:"messageText,omitempty"`
	SendingAgent    string            `json:"sendingAgent,omitempty"`
	RawSendingAgent string            `json:"rawSendingAgent,omitempty"`
	AskID           string            `json:"askId,omitempty"`
	AskPerson       string            `json:"askPerson,omitempty"`
	AskWhat         string            `json:"askWhat,omitempty"`
	AskLinks        []string          `json:"askLinks,omitempty"`
	Causes          []CauseAnnotation `json:"causes,omitempty"`
}

// EventFilter filters events returned by Ledger.QueryEvents.
type EventFilter struct {
	Kind   string
	Misfit bool
	Flags  bool
	Good   bool
	Agent  string
	Cause  string
}

// AgentEventGroup groups filtered events under one canonical agent name.
type AgentEventGroup struct {
	Agent  string  `json:"agent"`
	Events []Event `json:"events"`
}

// AgentName returns the canonical agent associated with e, falling back to raw names.
func (e Event) AgentName() string {
	for _, cand := range []string{e.SendingAgent, e.AskingAgent, e.RawSendingAgent, e.RawAskingAgent} {
		if strings.TrimSpace(cand) != "" {
			return cand
		}
	}
	return "unknown"
}

func formatEventID(n int) string {
	return fmt.Sprintf("EV-%03d", n)
}

func parseEventID(id string) (int, bool) {
	trimmed := strings.TrimSpace(id)
	if trimmed == "" {
		return 0, false
	}
	numStr := trimmed
	if prefix, rest, hasDash := strings.Cut(trimmed, "-"); hasDash {
		if !strings.EqualFold(prefix, "EV") || rest == "" {
			return 0, false
		}
		numStr = rest
	}
	n, err := strconv.Atoi(numStr)
	if err != nil || n < 1 {
		return 0, false
	}
	return n, true
}

func nextEventSequences(events []Event) (nextEv, nextAsk int) {
	nextEv, nextAsk = 1, 1
	for _, ev := range events {
		if n, ok := parseEventID(ev.ID); ok && n >= nextEv {
			nextEv = n + 1
		}
		if ev.AskID != "" {
			if n, ok := parseAskID(ev.AskID); ok && n >= nextAsk {
				nextAsk = n + 1
			}
		}
	}
	return nextEv, nextAsk
}

func readEvents(dir string) ([]Event, error) {
	path := filepath.Join(dir, eventsName)
	f, err := os.Open(path) //nolint:gosec // operator-local state file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return []Event{}, nil
		}
		return nil, fmt.Errorf("open decide events: %w", err)
	}
	defer f.Close()

	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 64<<10), 16<<20)
	events := make([]Event, 0)
	lineNo := 0
	for sc.Scan() {
		lineNo++
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		var ev Event
		if err := json.Unmarshal([]byte(line), &ev); err != nil {
			return nil, fmt.Errorf("decode decide event line %d: %w", lineNo, err)
		}
		events = append(events, ev)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("read decide events: %w", err)
	}
	return events, nil
}

func appendEvents(dir string, events []Event) error {
	if len(events) == 0 {
		return nil
	}
	path := filepath.Join(dir, eventsName)
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600) //nolint:gosec // operator-local state file
	if err != nil {
		return fmt.Errorf("open decide events: %w", err)
	}
	var writeErr error
	for _, ev := range events {
		ev.Time = ev.Time.UTC()
		ev.Causes = nil // stored separately as EventCause lines so the log stays strictly append-only
		line, err := json.Marshal(ev)
		if err != nil {
			writeErr = fmt.Errorf("encode decide event %s: %w", ev.ID, err)
			break
		}
		if _, err := f.Write(append(line, '\n')); err != nil {
			writeErr = fmt.Errorf("write decide event %s: %w", ev.ID, err)
			break
		}
	}
	if syncErr := f.Sync(); writeErr == nil && syncErr != nil {
		writeErr = fmt.Errorf("sync decide events: %w", syncErr)
	}
	if closeErr := f.Close(); writeErr == nil && closeErr != nil {
		writeErr = fmt.Errorf("close decide events: %w", closeErr)
	}
	return writeErr
}

func cloneEvent(ev Event) Event {
	if ev.Options != nil {
		ev.Options = cloneOptions(ev.Options)
	}
	if ev.Reasons != nil {
		ev.Reasons = append([]string(nil), ev.Reasons...)
	}
	if ev.AskLinks != nil {
		ev.AskLinks = append([]string(nil), ev.AskLinks...)
	}
	if ev.MessageIndex != nil {
		v := *ev.MessageIndex
		ev.MessageIndex = &v
	}
	ev.MessageTime = cloneTime(ev.MessageTime)
	if ev.Causes != nil {
		causes := make([]CauseAnnotation, len(ev.Causes))
		for i, c := range ev.Causes {
			c.Reasons = append([]string(nil), c.Reasons...)
			causes[i] = c
		}
		ev.Causes = causes
	}
	return ev
}

// buildChoiceAndAskEvents creates the choice event (and optional ask_open event
// when cfg.Asks.Enabled is true and the chosen option declares an ask) under
// the ledger lock.
func buildChoiceAndAskEvents(
	dir string,
	cfg Config,
	it *Item,
	when time.Time,
	who, via string,
	askingAgent, rawAskingAgent string,
	choiceKey, choiceLabel, replyText string,
	misfit bool,
	chosenOpt *Option,
) ([]Event, error) {
	if len(it.Options) == 0 {
		return nil, nil
	}
	existing, err := readEvents(dir)
	if err != nil {
		return nil, err
	}
	nextEv, nextAsk := nextEventSequences(existing)
	choiceEv := Event{
		ID:             formatEventID(nextEv),
		Kind:           EventChoice,
		Time:           when.UTC(),
		ItemID:         it.ID,
		Who:            who,
		Via:            via,
		AskingAgent:    askingAgent,
		RawAskingAgent: rawAskingAgent,
		ChoiceKey:      choiceKey,
		ChoiceLabel:    choiceLabel,
		ReplyText:      replyText,
		Options:        cloneOptions(it.Options),
		Misfit:         misfit,
	}
	if misfit {
		choiceEv.MisfitFlag = MisfitFlagOptionsDidNotFit
	}
	out := []Event{choiceEv}
	if cfg.Asks.Enabled && chosenOpt != nil && strings.TrimSpace(chosenOpt.Ask) != "" {
		person, what, err := ParseAskSpec(chosenOpt.Ask)
		if err != nil {
			return nil, err
		}
		nextEv++
		askEv := Event{
			ID:             formatEventID(nextEv),
			Kind:           EventAskOpen,
			Time:           when.UTC(),
			ItemID:         it.ID,
			Who:            who,
			Via:            via,
			AskingAgent:    askingAgent,
			RawAskingAgent: rawAskingAgent,
			TargetEventID:  choiceEv.ID,
			ChoiceKey:      chosenOpt.Key,
			ChoiceLabel:    chosenOpt.Label,
			AskID:          formatAskID(nextAsk),
			AskPerson:      person,
			AskWhat:        what,
			AskLinks:       append([]string(nil), chosenOpt.AskLinks...),
		}
		out = append(out, askEv)
	}
	return out, nil
}

// ListRawEvents returns every raw event from events.jsonl in append order.
func (l *Ledger) ListRawEvents() ([]Event, error) {
	var events []Event
	err := l.locked(false, func(*store) error {
		var rerr error
		events, rerr = readEvents(l.dir)
		return rerr
	})
	if err != nil {
		return nil, err
	}
	out := make([]Event, len(events))
	for i, ev := range events {
		out[i] = cloneEvent(ev)
	}
	return out, nil
}

// AddCause appends an optional cause annotation event referencing eventID
// without modifying the original event in events.jsonl.
func (l *Ledger) AddCause(eventID string, reasons []string, note, who, via string) (Event, error) {
	return l.AddCauseWith(eventID, reasons, note, "", who, via)
}

// AddCauseWith appends an optional cause annotation event referencing eventID
// with an optional earlierItemID for escalation reasons.
func (l *Ledger) AddCauseWith(eventID string, reasons []string, note, earlierItemID, who, via string) (Event, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return Event{}, err
	}
	if via == "" {
		via = ViaCLI
	}
	if err := CheckVia(via); err != nil {
		return Event{}, err
	}
	catalog := cfg.EffectiveReasons()
	canonReasons, trimmedNote, err := ValidateReasonSelection(catalog, reasons, note)
	if err != nil {
		return Event{}, err
	}
	trimmedEarlier, err := ValidateEarlierItemID(catalog, canonReasons, earlierItemID)
	if err != nil {
		return Event{}, err
	}
	if strings.TrimSpace(who) == "" {
		who = Human
	}
	var created Event
	err = l.lockedWithEvents(false, func(st *store) (bool, []Event, error) {
		existing, err := readEvents(l.dir)
		if err != nil {
			return false, nil, err
		}
		target, ok := findEvent(existing, eventID)
		if !ok {
			return false, nil, fmt.Errorf("decide event %s not found", eventID)
		}
		if target.Kind != EventChoice {
			return false, nil, fmt.Errorf("decide event %s (%s) is not a choice event", target.ID, target.Kind)
		}
		resolvedEarlier := resolveOptionalItemID(st, trimmedEarlier)
		nextEv, _ := nextEventSequences(existing)
		created = Event{
			ID:             formatEventID(nextEv),
			Kind:           EventCause,
			Time:           l.clock().UTC(),
			ItemID:         target.ItemID,
			Who:            strings.TrimSpace(who),
			Via:            via,
			AskingAgent:    target.AskingAgent,
			RawAskingAgent: target.RawAskingAgent,
			TargetEventID:  target.ID,
			Reasons:        canonReasons,
			Note:           trimmedNote,
			EarlierItemID:  resolvedEarlier,
		}
		return false, []Event{created}, nil
	})
	if err != nil {
		return Event{}, err
	}
	return cloneEvent(created), nil
}

// FlagItem appends a flag event against itemID (and an agent message on it).
// When msgIdx is non-nil, that message must exist and be an agent message.
// When msgIdx is nil, the newest agent message on itemID is selected, or the
// item itself when no agent message exists yet.
func (l *Ledger) FlagItem(itemID string, msgIdx *int, reasons []string, note, via string) (Event, error) {
	return l.FlagItemWith(itemID, msgIdx, reasons, note, "", via)
}

// FlagItemWith appends a flag event against itemID (and an agent message on it)
// with an optional earlierItemID for escalation reasons.
func (l *Ledger) FlagItemWith(itemID string, msgIdx *int, reasons []string, note, earlierItemID, via string) (Event, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return Event{}, err
	}
	if via == "" {
		via = ViaUI
	}
	if err := CheckVia(via); err != nil {
		return Event{}, err
	}
	catalog := cfg.EffectiveReasons()
	canonReasons, trimmedNote, err := ValidateReasonSelection(catalog, reasons, note)
	if err != nil {
		return Event{}, err
	}
	trimmedEarlier, err := ValidateEarlierItemID(catalog, canonReasons, earlierItemID)
	if err != nil {
		return Event{}, err
	}
	var created Event
	err = l.lockedWithEvents(false, func(st *store) (bool, []Event, error) {
		it, err := findItem(st, itemID)
		if err != nil {
			return false, nil, err
		}
		snapshot := cloneItem(it)
		ensureMessages(&snapshot)
		idx, msgTime, msgText, canonAgent, rawAgent, err := resolveFlagTarget(&snapshot, msgIdx)
		if err != nil {
			return false, nil, err
		}
		resolvedEarlier := resolveOptionalItemID(st, trimmedEarlier)
		existing, err := readEvents(l.dir)
		if err != nil {
			return false, nil, err
		}
		nextEv, _ := nextEventSequences(existing)
		created = Event{
			ID:              formatEventID(nextEv),
			Kind:            EventFlag,
			Time:            l.clock().UTC(),
			ItemID:          snapshot.ID,
			Who:             Human,
			Via:             via,
			SendingAgent:    canonAgent,
			RawSendingAgent: rawAgent,
			MessageIndex:    idx,
			MessageTime:     msgTime,
			MessageText:     msgText,
			Reasons:         canonReasons,
			Note:            trimmedNote,
			EarlierItemID:   resolvedEarlier,
		}
		return false, []Event{created}, nil
	})
	if err != nil {
		return Event{}, err
	}
	return cloneEvent(created), nil
}

// GoodItem appends a thumbs-up (good-message) event against itemID (and an
// agent message on it) with an optional note.
func (l *Ledger) GoodItem(itemID string, msgIdx *int, note, via string) (Event, error) {
	if via == "" {
		via = ViaUI
	}
	if err := CheckVia(via); err != nil {
		return Event{}, err
	}
	trimmedNote := strings.TrimSpace(note)
	var created Event
	err := l.lockedWithEvents(false, func(st *store) (bool, []Event, error) {
		it, err := findItem(st, itemID)
		if err != nil {
			return false, nil, err
		}
		snapshot := cloneItem(it)
		ensureMessages(&snapshot)
		idx, msgTime, msgText, canonAgent, rawAgent, err := resolveFlagTarget(&snapshot, msgIdx)
		if err != nil {
			return false, nil, err
		}
		existing, err := readEvents(l.dir)
		if err != nil {
			return false, nil, err
		}
		nextEv, _ := nextEventSequences(existing)
		created = Event{
			ID:              formatEventID(nextEv),
			Kind:            EventGood,
			Time:            l.clock().UTC(),
			ItemID:          snapshot.ID,
			Who:             Human,
			Via:             via,
			SendingAgent:    canonAgent,
			RawSendingAgent: rawAgent,
			MessageIndex:    idx,
			MessageTime:     msgTime,
			MessageText:     msgText,
			Note:            trimmedNote,
		}
		return false, []Event{created}, nil
	})
	if err != nil {
		return Event{}, err
	}
	return cloneEvent(created), nil
}

func resolveOptionalItemID(st *store, trimmed string) string {
	if trimmed == "" || st == nil {
		return trimmed
	}
	if it, err := findItem(st, trimmed); err == nil {
		return it.ID
	}
	return trimmed
}

func resolveFlagTarget(it *Item, msgIdx *int) (idxPtr *int, msgTime *time.Time, msgText, canonAgent, rawAgent string, err error) {
	if msgIdx != nil {
		idx := *msgIdx
		if idx < 0 || idx >= len(it.Messages) {
			return nil, nil, "", "", "", fmt.Errorf("decide message index %d out of range on %s", idx, it.ID)
		}
		m := it.Messages[idx]
		if m.Author == Human || m.Author == "system" {
			return nil, nil, "", "", "", fmt.Errorf("decide message %d on %s is not an agent message", idx, it.ID)
		}
		canon, raw := msgAsker(it, m)
		return &idx, utcTimePtr(m.Time), ComposeNoteText(m.Text, m.Verbatim), canon, raw, nil
	}
	for i := len(it.Messages) - 1; i >= 0; i-- {
		m := it.Messages[i]
		if m.Author == Human || m.Author == "system" {
			continue
		}
		idx := i
		canon, raw := msgAsker(it, m)
		return &idx, utcTimePtr(m.Time), ComposeNoteText(m.Text, m.Verbatim), canon, raw, nil
	}
	raw := itemRawOwner(it)
	canon := itemAskingAgent(it)
	if canon != "" {
		canon = resolveAgentWithConfig(canon, it)
	}
	created := utcTimePtr(it.CreatedAt)
	return nil, created, ComposeNoteText(it.Question, it.Verbatim), canon, raw, nil
}

// QueryEvents returns events matching filter, newest first, with EventCause
// annotations materialized onto their target choice events.
func (l *Ledger) QueryEvents(filter EventFilter) ([]Event, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return nil, err
	}
	rawEvents, err := l.ListRawEvents()
	if err != nil {
		return nil, err
	}
	return FilterEvents(rawEvents, filter, cfg), nil
}

// FilterEvents joins cause annotations onto choice events, filters by filter,
// and returns matching events newest first.
func FilterEvents(rawEvents []Event, filter EventFilter, cfg Config) []Event {
	joined := attachEventCauses(rawEvents)
	resolver := NewNameResolver(cfg, nil)
	wantAgent := strings.TrimSpace(filter.Agent)
	wantCanonAgent := wantAgent
	if wantAgent != "" {
		wantCanonAgent = resolver.Resolve(wantAgent, nil)
	}
	out := make([]Event, 0)
	for _, ev := range joined {
		if eventMatchesFilter(ev, filter, wantAgent, wantCanonAgent, resolver) {
			out = append(out, ev)
		}
	}
	sortEventsNewestFirst(out)
	return out
}

func attachEventCauses(rawEvents []Event) []Event {
	joined := make([]Event, len(rawEvents))
	byID := make(map[string]int, len(rawEvents))
	for i, ev := range rawEvents {
		joined[i] = cloneEvent(ev)
		byID[ev.ID] = i
	}
	for _, ev := range joined {
		if ev.Kind != EventCause {
			continue
		}
		if idx, ok := byID[ev.TargetEventID]; ok {
			joined[idx].Causes = append(joined[idx].Causes, CauseAnnotation{
				EventID:       ev.ID,
				Time:          ev.Time,
				Who:           ev.Who,
				Via:           ev.Via,
				Reasons:       append([]string(nil), ev.Reasons...),
				Note:          ev.Note,
				EarlierItemID: ev.EarlierItemID,
			})
		}
	}
	return joined
}

func eventMatchesFilter(ev Event, filter EventFilter, wantRaw, wantCanon string, resolver NameResolver) bool {
	if filter.Misfit && (ev.Kind != EventChoice || !ev.Misfit) {
		return false
	}
	if filter.Flags && ev.Kind != EventFlag {
		return false
	}
	if filter.Good && ev.Kind != EventGood {
		return false
	}
	if filter.Kind != "" && ev.Kind != filter.Kind {
		return false
	}
	if wantRaw != "" && !eventMatchesAgent(ev, wantRaw, wantCanon, resolver) {
		return false
	}
	if cause := strings.TrimSpace(filter.Cause); cause != "" && !eventMatchesCause(ev, cause) {
		return false
	}
	return true
}

func sortEventsNewestFirst(out []Event) {
	sort.SliceStable(out, func(i, j int) bool {
		if !out[i].Time.Equal(out[j].Time) {
			return out[i].Time.After(out[j].Time)
		}
		ni, oki := parseEventID(out[i].ID)
		nj, okj := parseEventID(out[j].ID)
		if oki && okj && ni != nj {
			return ni > nj
		}
		return out[i].ID > out[j].ID
	})
}

func eventMatchesAgent(ev Event, wantRaw, wantCanon string, resolver NameResolver) bool {
	cands := []string{ev.AskingAgent, ev.RawAskingAgent, ev.SendingAgent, ev.RawSendingAgent}
	for _, c := range cands {
		c = strings.TrimSpace(c)
		if c == "" {
			continue
		}
		if strings.EqualFold(c, wantRaw) || strings.EqualFold(c, wantCanon) {
			return true
		}
		if strings.EqualFold(resolver.Resolve(c, nil), wantCanon) {
			return true
		}
	}
	return false
}

func eventMatchesCause(ev Event, reasonID string) bool {
	for _, r := range ev.Reasons {
		if strings.EqualFold(r, reasonID) {
			return true
		}
	}
	for _, c := range ev.Causes {
		for _, r := range c.Reasons {
			if strings.EqualFold(r, reasonID) {
				return true
			}
		}
	}
	return false
}

// GroupEventsByAgent groups events by canonical agent, sorted by descending
// count then alphabetically by agent name.
func GroupEventsByAgent(events []Event) []AgentEventGroup {
	byAgent := make(map[string][]Event)
	for _, ev := range events {
		agent := ev.AgentName()
		byAgent[agent] = append(byAgent[agent], ev)
	}
	groups := make([]AgentEventGroup, 0, len(byAgent))
	for agent, list := range byAgent {
		groups = append(groups, AgentEventGroup{Agent: agent, Events: list})
	}
	sort.Slice(groups, func(i, j int) bool {
		if len(groups[i].Events) != len(groups[j].Events) {
			return len(groups[i].Events) > len(groups[j].Events)
		}
		return groups[i].Agent < groups[j].Agent
	})
	return groups
}

func findEvent(events []Event, id string) (Event, bool) {
	trimmed := strings.TrimSpace(id)
	for _, ev := range events {
		if strings.EqualFold(ev.ID, trimmed) {
			return ev, true
		}
	}
	if n, ok := parseEventID(trimmed); ok {
		for _, ev := range events {
			if en, eok := parseEventID(ev.ID); eok && en == n {
				return ev, true
			}
		}
	}
	return Event{}, false
}
