package tui

import (
	"context"
	"log/slog"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// defaultDeliveryEvery is how often pending delivery marks are checked.
const defaultDeliveryEvery = 10 * time.Second

// deliveryTickMsg starts the next delivery check.
type deliveryTickMsg struct{}

// deliveryMsg is the result of one background delivery check.
type deliveryMsg struct{ err error }

// anyPending reports whether a loaded item has a delivery agentbus may
// still change.
func (m *Model) anyPending() bool {
	for i := range m.allItems {
		if decide.HasPending(&m.allItems[i]) {
			return true
		}
	}
	return false
}

// startDeliveryPoll schedules the first check when a delivery is
// pending and no poll runs yet. Nothing pending schedules nothing.
func (m *Model) startDeliveryPoll() tea.Cmd {
	if m.deliveryPolling || !m.anyPending() {
		return nil
	}
	if _, ok := m.store().(DeliveryChecker); !ok {
		return nil
	}
	m.deliveryPolling = true
	return m.deliveryTick()
}

func (m *Model) deliveryTick() tea.Cmd {
	every := m.deliveryEvery
	if every <= 0 {
		every = defaultDeliveryEvery
	}
	return tea.Tick(every, func(time.Time) tea.Msg { return deliveryTickMsg{} })
}

// deliveryCheck runs one batched status check off the update loop. In
// hub mode the backend runs it on the hub.
func (m *Model) deliveryCheck() tea.Cmd {
	checker, ok := m.store().(DeliveryChecker)
	if !ok {
		return nil
	}
	return func() tea.Msg {
		return deliveryMsg{err: checker.CheckDeliveries(context.Background())}
	}
}

// updateDelivery runs a check on its tick while a delivery is pending,
// and stops the poll once none is. A finished check waits for the next
// tick; the ledger reload shows what it stored. It reports whether msg
// was a delivery message.
func (m *Model) updateDelivery(msg tea.Msg) (tea.Cmd, bool) {
	switch msg := msg.(type) {
	case deliveryTickMsg:
		if !m.anyPending() {
			m.deliveryPolling = false
			return nil, true
		}
		return m.deliveryCheck(), true
	case deliveryMsg:
		if msg.err != nil && !m.deliveryLogged {
			m.deliveryLogged = true
			slog.Debug("decide: delivery check failed", "err", msg.err)
		}
		return m.deliveryTick(), true
	}
	return nil, false
}

// messageMark is the delivery mark shown after msg's header, or empty.
func (m *Model) messageMark(it *decide.Item, msg decide.Message) string {
	mark, ok := decide.MessageMark(it, msg)
	if !ok {
		return ""
	}
	return mark.String(m.zone())
}
