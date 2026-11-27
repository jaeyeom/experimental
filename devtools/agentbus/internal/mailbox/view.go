package mailbox

import (
	"context"
	"fmt"
	"slices"
	"strings"
)

// Show prints one stored message.
func (m *Mailbox) Show(id string) (string, error) {
	msg, err := m.get(id)
	if err != nil {
		return "", err
	}
	return formatMessage(msg), nil
}

// Recipient is the name or handle message id was sent to.
func (m *Mailbox) Recipient(id string) (string, error) {
	msg, err := m.get(id)
	if err != nil {
		return "", err
	}
	return msg.To, nil
}

// Answer is one reply to a message. Addressed is a reply from the name
// the message was sent to, or from whoever holds that name now.
type Answer struct {
	ID        string
	From      string
	Text      string
	Addressed bool
}

// Replies returns the direct replies to message id, oldest first.
func (m *Mailbox) Replies(id string) ([]Answer, error) {
	m.mu.Lock()
	orig, err := m.lookupLocked(id)
	var kids []message
	for _, msg := range m.msgs {
		if err == nil && msg.Re == orig.ID {
			kids = append(kids, msg)
		}
	}
	m.mu.Unlock()
	if err != nil {
		return nil, err
	}
	sortMessages(kids)
	holder, herr := m.reg.Resolve(orig.To)
	out := make([]Answer, 0, len(kids))
	for _, k := range kids {
		addressed := slices.Contains(strings.Split(k.From, ", "), orig.To) ||
			(herr == nil && k.SenderHandle != "" && k.SenderHandle == holder)
		out = append(out, Answer{ID: k.ID, From: k.From, Text: k.Body, Addressed: addressed})
	}
	return out, nil
}

// Thread prints the reply chain that contains id, root first.
func (m *Mailbox) Thread(id string) (string, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	start, err := m.lookupLocked(id)
	if err != nil {
		return "", err
	}
	root := start.ID
	seen := map[string]bool{}
	for m.msgs[root].Re != "" && !seen[root] {
		parent, ok := m.msgs[m.msgs[root].Re]
		if !ok {
			break
		}
		seen[root] = true
		root = parent.ID
	}
	var parts []string
	m.walk(root, map[string]bool{}, &parts)
	return strings.Join(parts, "\n"), nil
}

func (m *Mailbox) walk(id string, seen map[string]bool, parts *[]string) {
	if seen[id] {
		return
	}
	msg, ok := m.msgs[id]
	if !ok {
		return
	}
	seen[id] = true
	*parts = append(*parts, formatMessage(msg))
	var kids []message
	for _, candidate := range m.msgs {
		if candidate.Re == id {
			kids = append(kids, candidate)
		}
	}
	sortMessages(kids)
	for _, kid := range kids {
		m.walk(kid.ID, seen, parts)
	}
}

// Status flushes the message's queue, then prints its state and how long it waited.
func (m *Mailbox) Status(ctx context.Context, id string) (string, error) {
	msg, err := m.flushed(ctx, id)
	if err != nil {
		return "", err
	}
	client := ""
	if msg.ClientID != "" {
		client = "client_id: " + msg.ClientID + "\n"
	}
	extra := ""
	if msg.State == stateDeliveredUnprocessed && msg.UnprocessedReason != "" {
		extra += "reason: " + msg.UnprocessedReason + "\n"
	}
	if msg.HeldDetail != "" && strings.HasPrefix(msg.State, "held: ") {
		extra += "held_detail: " + msg.HeldDetail + "\n"
	}
	if detail := m.staleComposerDetail(msg); detail != "" {
		extra += "detail: " + detail + "\n"
	}
	return fmt.Sprintf("id: %s\nstate: %s\n%s%s%swaited: %s\n", msg.ID, msg.State, client, extra, m.wakeStatus(msg), m.waited(msg)), nil
}

// flushed flushes the queue of message id, then reads it again.
func (m *Mailbox) flushed(ctx context.Context, id string) (message, error) {
	msg, err := m.get(id)
	if err != nil {
		return message{}, err
	}
	if err := m.flushHandle(ctx, msg.Handle); err != nil {
		return message{}, err
	}
	return m.get(id)
}

// Held lists queued messages in send order, with how long each has waited.
func (m *Mailbox) Held() []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	pending := make([]message, 0)
	for _, msg := range m.msgs {
		if terminal(msg.State) {
			continue
		}
		pending = append(pending, msg)
	}
	sortMessages(pending)
	lines := make([]string, 0, len(pending))
	for _, msg := range pending {
		lines = append(lines, fmt.Sprintf("held %s to %s waited %s", msg.ID, msg.To, m.waited(msg)))
	}
	return lines
}

func (m *Mailbox) waited(msg message) string {
	end := m.now()
	switch {
	case msg.ClosedAt != nil:
		end = *msg.ClosedAt
	case msg.DeliveredAt != nil:
		end = *msg.DeliveredAt
	}
	return waitString(end.Sub(msg.SentAt))
}

func formatMessage(msg message) string {
	var b strings.Builder
	fmt.Fprintf(&b, "id: %s\n", msg.ID)
	if msg.ClientID != "" {
		fmt.Fprintf(&b, "client_id: %s\n", msg.ClientID)
	}
	fmt.Fprintf(&b, "from: %s\nto: %s\n", msg.From, msg.To)
	if msg.Re != "" {
		fmt.Fprintf(&b, "re: %s\n", msg.Re)
	}
	if msg.ResendOf != "" {
		fmt.Fprintf(&b, "resend_of: %s\n", msg.ResendOf)
	}
	fmt.Fprintf(&b, "sent_at: %s\nbody:\n%s\n", msg.SentAt.UTC().Format(timeLayout), msg.Body)
	return b.String()
}

func sortMessages(msgs []message) {
	for i := 1; i < len(msgs); i++ {
		item := msgs[i]
		j := i
		for j > 0 && messageBefore(item, msgs[j-1]) {
			msgs[j] = msgs[j-1]
			j--
		}
		msgs[j] = item
	}
}

func messageBefore(a, b message) bool {
	if !a.SentAt.Equal(b.SentAt) {
		return a.SentAt.Before(b.SentAt)
	}
	return a.ID < b.ID
}
