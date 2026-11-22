package decide

import (
	"fmt"
	"strings"
	"time"
)

// Inbox groups, in display order.
const (
	// GroupApprovals holds approvals and the threads waiting on one.
	GroupApprovals = "approvals"
	// GroupNeedsYou holds the other items that need the user.
	GroupNeedsYou = "needs-you"
	// GroupRequests holds requests and inbound asks.
	GroupRequests = "requests"
	// GroupNotes holds notes.
	GroupNotes = "notes"
)

// InboxEntry is one row of the grouped inbox. Item is the row's item. An
// approval filed with --parent is nested under its parent: the parent is
// the entry, ApprovalWaiting is set, and Approvals holds the open cards.
type InboxEntry struct {
	Group           string
	Item            Item
	Approvals       []Item
	ApprovalWaiting bool
}

func groupOf(class string) string {
	switch class {
	case ClassApproval:
		return GroupApprovals
	case ClassRequest, ClassInbound:
		return GroupRequests
	case ClassNote:
		return GroupNotes
	}
	return GroupNeedsYou
}

// InboxEntries arranges items (an already selected inbox) into groups:
// approvals with their parents, other items that need the user, requests
// and inbound, then notes. Rows inside a group keep Sort order, so tasks
// run by due date, soonest first. An open approval whose parent is in
// items is nested under it and has no row of its own. A non-empty classes
// keeps only entries of those classes; a parent with a waiting approval
// also matches "approval".
func InboxEntries(items []Item, classes []string) []InboxEntry {
	sorted := append([]Item(nil), items...)
	Sort(sorted)
	present := map[string]bool{}
	for i := range sorted {
		present[sorted[i].ID] = true
	}
	cards := map[string][]Item{}
	nested := map[string]bool{}
	for i := range sorted {
		it := &sorted[i]
		if it.EffectiveClass() == ClassApproval && it.State == StateOpen && present[it.Parent] {
			cards[it.Parent] = append(cards[it.Parent], *it)
			nested[it.ID] = true
		}
	}
	want := map[string]bool{}
	for _, c := range classes {
		want[c] = true
	}
	byGroup := map[string][]InboxEntry{}
	for i := range sorted {
		it := &sorted[i]
		if nested[it.ID] {
			continue
		}
		e := InboxEntry{Group: groupOf(it.EffectiveClass()), Item: *it}
		if c := cards[it.ID]; len(c) > 0 {
			e.Group, e.Approvals, e.ApprovalWaiting = GroupApprovals, c, true
		}
		if len(want) > 0 && !want[it.EffectiveClass()] && (!e.ApprovalWaiting || !want[ClassApproval]) {
			continue
		}
		byGroup[e.Group] = append(byGroup[e.Group], e)
	}
	var out []InboxEntry
	for _, g := range []string{GroupApprovals, GroupNeedsYou, GroupRequests, GroupNotes} {
		out = append(out, byGroup[g]...)
	}
	return out
}

// ApprovalCard returns the newest open approval filed under parentID.
func ApprovalCard(items []Item, parentID string) (Item, bool) {
	var best *Item
	for i := range items {
		it := &items[i]
		if it.Parent != parentID || it.EffectiveClass() != ClassApproval || it.State != StateOpen {
			continue
		}
		if best == nil || newer(it, best) {
			best = it
		}
	}
	if best == nil {
		return Item{}, false
	}
	return *best, true
}

// BadgeOf is the class badge shown on a row: the class prefix letter.
func BadgeOf(it *Item) string {
	p, err := ClassPrefix(it.EffectiveClass())
	if err != nil {
		return "D"
	}
	return p
}

// UnreadCount counts items with something unread. Notes are excluded.
func UnreadCount(items []Item) int {
	n := 0
	for i := range items {
		it := items[i]
		if it.EffectiveClass() != ClassNote && hasUnread(&it) {
			n++
		}
	}
	return n
}

// ApprovalKeyOption maps the list quick keys y, n and e to an approval
// option key.
func ApprovalKeyOption(r rune) (string, bool) {
	switch r {
	case 'y':
		return "yes", true
	case 'n':
		return "no", true
	case 'e':
		return "edit", true
	}
	return "", false
}

// AnswerCard answers the open approval card filed under parentID, as
// answering it by id does, so the parent gets the one-line result.
func (l *Ledger) AnswerCard(parentID, value, via string) (Item, error) {
	items, err := l.List()
	if err != nil {
		return Item{}, err
	}
	parent, err := l.Get(parentID)
	if err != nil {
		return Item{}, err
	}
	card, ok := ApprovalCard(items, parent.ID)
	if !ok {
		return Item{}, fmt.Errorf("decide item %s has no open approval", parent.ID)
	}
	return l.Answer(card.ID, value, via)
}

// approvalLine is the one line a parent thread gets when its approval is
// answered.
func approvalLine(card *Item, key string, at time.Time) string {
	switch key {
	case "yes":
		return fmt.Sprintf("%s approved, posted %s", card.ID, at.Local().Format("15:04"))
	case "no":
		return card.ID + " declined"
	}
	return card.ID + " answered " + key
}

// ArchiveNotes archives every open note with no message after seen and
// returns how many it archived.
func (l *Ledger) ArchiveNotes(seen time.Time) (int, error) {
	items, err := l.List()
	if err != nil {
		return 0, err
	}
	n := 0
	for i := range items {
		it := &items[i]
		if it.EffectiveClass() != ClassNote || !InState(it, StateOpen) {
			continue
		}
		if _, err := l.Archive(it.ID, seen); err != nil {
			if err == ErrNewerMessage { //nolint:errorlint // sentinel returned unwrapped
				continue
			}
			return n, err
		}
		n++
	}
	return n, nil
}

// Proposal is one proposed class change from the backfill.
type Proposal struct {
	ID     string `json:"id"`
	From   string `json:"from"`
	To     string `json:"to"`
	Reason string `json:"reason"`
}

// ProposeBackfill proposes a class for each item that is still a
// decision, by owner and pattern. Anything uncertain stays a decision and
// is not listed. It never changes items.
func ProposeBackfill(items []Item) []Proposal {
	var out []Proposal
	for i := range items {
		it := &items[i]
		if it.EffectiveClass() != ClassDecision || it.State == StateWithdrawn {
			continue
		}
		to, reason := classifyLegacy(it)
		if to == "" {
			continue
		}
		out = append(out, Proposal{ID: it.ID, From: ClassDecision, To: to, Reason: reason})
	}
	return out
}

func classifyLegacy(it *Item) (class, reason string) {
	q := strings.ToLower(it.Question)
	noChoice := len(it.Options) == 0 && !it.FreeText
	switch {
	case len(it.Messages) > 0 && it.Messages[0].Author == "human" && noChoice:
		return ClassRequest, "thread the user started"
	case strings.TrimSpace(it.Verbatim) != "" || strings.Contains(q, "as written"):
		return ClassApproval, "asks to post text as written"
	case it.Kind == KindRelay && noChoice:
		return ClassNote, "report with no options"
	case strings.Contains(q, "install"):
		return ClassNote, "install report"
	}
	return "", ""
}

// ApplyBackfill retags every proposal, recording a retag event on each
// item, and returns how many it changed.
func (l *Ledger) ApplyBackfill(props []Proposal, by string) (int, error) {
	n := 0
	for _, p := range props {
		if _, err := l.Retag(p.ID, p.To, by); err != nil {
			return n, fmt.Errorf("retag %s: %w", p.ID, err)
		}
		n++
	}
	return n, nil
}

// noteApprovalOnParent posts the one-line result of an answered approval
// to its parent thread.
func noteApprovalOnParent(st *store, card *Item, key string, at time.Time) {
	if card.EffectiveClass() != ClassApproval || card.Parent == "" {
		return
	}
	parent, err := findItem(st, card.Parent)
	if err != nil {
		return
	}
	ensureMessages(parent)
	parent.Messages = append(parent.Messages, Message{
		Author: "system",
		Time:   at,
		Text:   approvalLine(card, key, at),
	})
}
