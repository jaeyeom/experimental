package decide

import (
	"fmt"
	"slices"
	"sort"
	"strconv"
	"strings"
	"time"
)

// Ask lifecycle states.
const (
	AskStatusOpen    = "open"
	AskStatusDone    = "done"
	AskStatusDropped = "dropped"
)

// AskRecord represents an ask opened when a human picks an ask-marked option
// (or records one via `decide ask add` after a free-text answer).
type AskRecord struct {
	ID          string     `json:"id"`
	ItemID      string     `json:"itemId"`
	EventID     string     `json:"eventId,omitempty"`
	ChoiceKey   string     `json:"choiceKey,omitempty"`
	ChoiceLabel string     `json:"choiceLabel,omitempty"`
	Person      string     `json:"person"`
	What        string     `json:"what"`
	Links       []string   `json:"links,omitempty"`
	Owner       string     `json:"owner"`
	RawOwner    string     `json:"rawOwner,omitempty"`
	Status      string     `json:"status"`
	CreatedAt   time.Time  `json:"createdAt"`
	ClosedAt    *time.Time `json:"closedAt,omitempty"`
	ClosedBy    string     `json:"closedBy,omitempty"`
	CloseReason string     `json:"closeReason,omitempty"`
}

// ParseAskSpec parses `<person>: <what>` into non-empty person and what parts.
func ParseAskSpec(spec string) (person, what string, err error) {
	p, w, ok := strings.Cut(strings.TrimSpace(spec), ":")
	if !ok {
		return "", "", fmt.Errorf("decide ask spec %q must be \"<person>: <what>\"", spec)
	}
	p = strings.TrimSpace(p)
	w = strings.TrimSpace(w)
	if p == "" || w == "" {
		return "", "", fmt.Errorf("decide ask spec %q must be \"<person>: <what>\"", spec)
	}
	return p, w, nil
}

func formatAskID(n int) string {
	return fmt.Sprintf("ASK-%03d", n)
}

func parseAskID(id string) (int, bool) {
	trimmed := strings.TrimSpace(id)
	if trimmed == "" {
		return 0, false
	}
	numStr := trimmed
	if prefix, rest, hasDash := strings.Cut(trimmed, "-"); hasDash {
		if !strings.EqualFold(prefix, "ASK") || rest == "" {
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

// MaterializeAsks builds AskRecords from events in append order.
func MaterializeAsks(events []Event) []AskRecord {
	asks := make([]AskRecord, 0)
	byID := make(map[string]int)
	for _, ev := range events {
		switch ev.Kind {
		case EventAskOpen:
			rec := AskRecord{
				ID:          ev.AskID,
				ItemID:      ev.ItemID,
				EventID:     ev.TargetEventID,
				ChoiceKey:   ev.ChoiceKey,
				ChoiceLabel: ev.ChoiceLabel,
				Person:      ev.AskPerson,
				What:        ev.AskWhat,
				Links:       append([]string(nil), ev.AskLinks...),
				Owner:       ev.AskingAgent,
				RawOwner:    ev.RawAskingAgent,
				Status:      AskStatusOpen,
				CreatedAt:   ev.Time.UTC(),
			}
			byID[rec.ID] = len(asks)
			asks = append(asks, rec)
		case EventAskDone, EventAskDrop:
			idx, ok := byID[ev.AskID]
			if !ok {
				continue
			}
			status := AskStatusDone
			if ev.Kind == EventAskDrop {
				status = AskStatusDropped
			}
			closedAt := ev.Time.UTC()
			asks[idx].Status = status
			asks[idx].ClosedAt = &closedAt
			asks[idx].ClosedBy = ev.Who
			asks[idx].CloseReason = ev.Note
		}
	}
	return asks
}

// SortAsks orders asks with open asks first, then closed, oldest createdAt first.
func SortAsks(asks []AskRecord) {
	sort.SliceStable(asks, func(i, j int) bool {
		openI := asks[i].Status == AskStatusOpen
		openJ := asks[j].Status == AskStatusOpen
		if openI != openJ {
			return openI
		}
		if !asks[i].CreatedAt.Equal(asks[j].CreatedAt) {
			return asks[i].CreatedAt.Before(asks[j].CreatedAt)
		}
		ni, oki := parseAskID(asks[i].ID)
		nj, okj := parseAskID(asks[j].ID)
		if oki && okj && ni != nj {
			return ni < nj
		}
		return asks[i].ID < asks[j].ID
	})
}

// OpenAskCount returns how many asks in slice have status AskStatusOpen.
func OpenAskCount(asks []AskRecord) int {
	n := 0
	for i := range asks {
		if asks[i].Status == AskStatusOpen {
			n++
		}
	}
	return n
}

func cloneAsk(a AskRecord) AskRecord {
	if a.Links != nil {
		a.Links = append([]string(nil), a.Links...)
	}
	a.ClosedAt = cloneTime(a.ClosedAt)
	return a
}

// ListAsks returns all asks in SortAsks order (open first, oldest first).
func (l *Ledger) ListAsks() ([]AskRecord, error) {
	raw, err := l.ListRawEvents()
	if err != nil {
		return nil, err
	}
	asks := MaterializeAsks(raw)
	SortAsks(asks)
	return asks, nil
}

// GetAsk finds an ask record by ID (exact or short numeric ID).
func (l *Ledger) GetAsk(id string) (AskRecord, error) {
	asks, err := l.ListAsks()
	if err != nil {
		return AskRecord{}, err
	}
	rec, err := findAsk(asks, id)
	if err != nil {
		return AskRecord{}, err
	}
	return cloneAsk(*rec), nil
}

func findAsk(asks []AskRecord, id string) (*AskRecord, error) {
	trimmed := strings.TrimSpace(id)
	for i := range asks {
		if strings.EqualFold(asks[i].ID, trimmed) {
			return &asks[i], nil
		}
	}
	if n, ok := parseAskID(trimmed); ok {
		for i := range asks {
			if an, aok := parseAskID(asks[i].ID); aok && an == n {
				return &asks[i], nil
			}
		}
	}
	return nil, fmt.Errorf("decide ask %s not found", id)
}

// AddAsk manually opens an ask record on itemID (for free-text answers).
// Refuses when asks are disabled in config.
func (l *Ledger) AddAsk(itemID, spec string, links []string, eventID, via string) (AskRecord, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return AskRecord{}, err
	}
	if !cfg.Asks.Enabled {
		return AskRecord{}, fmt.Errorf("decide asks are disabled in config (set asks.enabled = true)")
	}
	person, what, err := ParseAskSpec(spec)
	if err != nil {
		return AskRecord{}, err
	}
	if via == "" {
		via = ViaCLI
	}
	if err := CheckVia(via); err != nil {
		return AskRecord{}, err
	}
	cleanedLinks := cleanNonEmptyStrings(links)

	var created AskRecord
	err = l.lockedWithEvents(false, func(st *store) (bool, []Event, error) {
		it, err := findItem(st, itemID)
		if err != nil {
			return false, nil, err
		}
		existing, err := readEvents(l.dir)
		if err != nil {
			return false, nil, err
		}
		targetEv, err := resolveAskChoiceEvent(existing, it.ID, eventID)
		if err != nil {
			return false, nil, err
		}

		canonOwner, rawOwner := resolveAskOwner(it, targetEv)
		nextEv, nextAsk := nextEventSequences(existing)
		now := l.clock().UTC()
		targetID := ""
		if targetEv != nil {
			targetID = targetEv.ID
		}
		askEv := Event{
			ID:             formatEventID(nextEv),
			Kind:           EventAskOpen,
			Time:           now,
			ItemID:         it.ID,
			Who:            Human,
			Via:            via,
			AskingAgent:    canonOwner,
			RawAskingAgent: rawOwner,
			TargetEventID:  targetID,
			AskID:          formatAskID(nextAsk),
			AskPerson:      person,
			AskWhat:        what,
			AskLinks:       cleanedLinks,
		}
		created = AskRecord{
			ID:        askEv.AskID,
			ItemID:    it.ID,
			EventID:   targetID,
			Person:    person,
			What:      what,
			Links:     append([]string(nil), cleanedLinks...),
			Owner:     canonOwner,
			RawOwner:  rawOwner,
			Status:    AskStatusOpen,
			CreatedAt: now,
		}
		return false, []Event{askEv}, nil
	})
	if err != nil {
		return AskRecord{}, err
	}
	return cloneAsk(created), nil
}

func resolveAskChoiceEvent(existing []Event, itemID, eventID string) (*Event, error) {
	if strings.TrimSpace(eventID) != "" {
		ev, ok := findEvent(existing, eventID)
		if !ok {
			return nil, fmt.Errorf("decide event %s not found", eventID)
		}
		if ev.Kind != EventChoice {
			return nil, fmt.Errorf("decide event %s (%s) is not a choice event", ev.ID, ev.Kind)
		}
		if ev.ItemID != itemID {
			return nil, fmt.Errorf("decide event %s belongs to %s, not %s", ev.ID, ev.ItemID, itemID)
		}
		return &ev, nil
	}
	for i := len(existing) - 1; i >= 0; i-- {
		if existing[i].Kind == EventChoice && existing[i].ItemID == itemID {
			ev := existing[i]
			return &ev, nil
		}
	}
	return nil, nil
}

func resolveAskOwner(it *Item, ev *Event) (canon, raw string) {
	if ev != nil && (ev.AskingAgent != "" || ev.RawAskingAgent != "") {
		canon = ev.AskingAgent
		raw = ev.RawAskingAgent
		if canon == "" {
			canon = resolveAgentWithConfig(raw, it)
		}
		return canon, raw
	}
	canon, raw, _ = pendingAsker(it)
	return canon, raw
}

func cleanNonEmptyStrings(in []string) []string {
	var out []string
	for _, s := range in {
		if t := strings.TrimSpace(s); t != "" {
			out = append(out, t)
		}
	}
	return out
}

func normalizeCloseAskParams(status, reason, by, via string) (trimmedReason, actor, outVia string, err error) {
	if status != AskStatusDone && status != AskStatusDropped {
		return "", "", "", fmt.Errorf("decide ask status %q must be done or dropped", status)
	}
	trimmedReason = strings.TrimSpace(reason)
	if trimmedReason == "" {
		return "", "", "", fmt.Errorf("decide ask close reason is empty")
	}
	outVia = via
	if outVia == "" {
		outVia = ViaCLI
	}
	if err := CheckVia(outVia); err != nil {
		return "", "", "", err
	}
	actor = strings.TrimSpace(by)
	if outVia == ViaUI || actor == "" || strings.EqualFold(actor, Human) {
		actor = Human
	}
	return trimmedReason, actor, outVia, nil
}

// CloseAsk marks askID done or dropped with reason, writes the ask close event,
// and appends a system note to the original item thread in the same locked write.
func (l *Ledger) CloseAsk(id, status, reason, by, via string) (AskRecord, error) {
	trimmedReason, actor, via, err := normalizeCloseAskParams(status, reason, by, via)
	if err != nil {
		return AskRecord{}, err
	}

	var updated AskRecord
	err = l.lockedWithEvents(true, func(st *store) (bool, []Event, error) {
		existing, err := readEvents(l.dir)
		if err != nil {
			return false, nil, err
		}
		asks := MaterializeAsks(existing)
		ask, err := findAsk(asks, id)
		if err != nil {
			return false, nil, err
		}
		if ask.Status != AskStatusOpen {
			return false, nil, fmt.Errorf("decide ask %s is already %s", ask.ID, ask.Status)
		}
		var it *Item
		if found, ferr := findItem(st, ask.ItemID); ferr == nil {
			it = found
		}
		if err := checkCloseAskAuth(ask, it, actor); err != nil {
			return false, nil, err
		}
		now := l.clock().UTC()
		evKind := EventAskDone
		if status == AskStatusDropped {
			evKind = EventAskDrop
		}
		nextEv, _ := nextEventSequences(existing)
		closeEv := Event{
			ID:             formatEventID(nextEv),
			Kind:           evKind,
			Time:           now,
			ItemID:         ask.ItemID,
			Who:            actor,
			Via:            via,
			AskingAgent:    ask.Owner,
			RawAskingAgent: ask.RawOwner,
			AskID:          ask.ID,
			Note:           trimmedReason,
		}
		writeLedger := false
		if it != nil {
			ensureMessages(it)
			it.Messages = append(it.Messages, Message{
				Author: "system",
				Time:   now,
				By:     actor,
				Text:   askCloseThreadNote(ask, status, actor, trimmedReason),
			})
			writeLedger = true
		}
		res := cloneAsk(*ask)
		res.Status = status
		res.ClosedAt = &now
		res.ClosedBy = actor
		res.CloseReason = trimmedReason
		updated = res
		return writeLedger, []Event{closeEv}, nil
	})
	if err != nil {
		return AskRecord{}, err
	}
	return cloneAsk(updated), nil
}

func checkCloseAskAuth(ask *AskRecord, it *Item, actor string) error {
	if actor == Human {
		return nil
	}
	candidates := []string{ask.Owner, ask.RawOwner}
	if it != nil {
		candidates = append(candidates, it.Owner, it.RawOwner, it.AskingAgent)
	}
	if slices.Contains(candidates, actor) {
		return nil
	}
	if resolved := resolveAgentWithConfig(actor, it); resolved != "" && slices.Contains(candidates, resolved) {
		return nil
	}
	ownerLabel := ask.Owner
	if ownerLabel == "" && it != nil {
		ownerLabel = it.Owner
	}
	return fmt.Errorf("decide ask %s may only be closed by %s or human (got %s)", ask.ID, ownerLabel, actor)
}

func askCloseThreadNote(ask *AskRecord, status, actor, reason string) string {
	return fmt.Sprintf("ask %s (%s: %s) %s by %s: %s", ask.ID, ask.Person, ask.What, status, actor, reason)
}
