package mailbox

import (
	"context"
	"fmt"
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

// Thread prints the reply chain that contains id, root first.
func (m *Mailbox) Thread(id string) (string, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if _, ok := m.msgs[id]; !ok {
		return "", fmt.Errorf("unknown message %s", id)
	}
	root := id
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
	msg, err := m.get(id)
	if err != nil {
		return "", err
	}
	if err := m.flushHandle(ctx, msg.Handle); err != nil {
		return "", err
	}
	msg, err = m.get(id)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("id: %s\nstate: %s\n%swaited: %s\n", msg.ID, msg.State, m.wakeStatus(msg), m.waited(msg)), nil
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
	fmt.Fprintf(&b, "id: %s\nfrom: %s\nto: %s\n", msg.ID, msg.From, msg.To)
	if msg.Re != "" {
		fmt.Fprintf(&b, "re: %s\n", msg.Re)
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
