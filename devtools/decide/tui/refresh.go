package tui

import (
	"fmt"
	"reflect"
	"slices"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// refreshDetail updates the open item in place from a fresh ledger read,
// whatever the folder. Drafts, focus, and the search highlight are left
// alone. When the view was at the thread's end it follows a new message.
// Otherwise the scroll and selected message stay put and the footer
// counts the new messages below.
func (m *Model) refreshDetail(all []decide.Item) {
	if m.detail == nil {
		return
	}
	i := slices.IndexFunc(all, func(it decide.Item) bool { return it.ID == m.detail.ID })
	if i < 0 {
		if !m.detailGone {
			m.detailGone = true
			m.status = "item no longer in ledger"
		}
		return
	}
	m.detailGone = false
	fresh := all[i]
	old := m.detail
	if reflect.DeepEqual(*old, fresh) {
		return
	}
	follow := m.atThreadEnd()
	m.detail = &fresh
	if fresh.State != old.State {
		m.status = "item " + fresh.State + " elsewhere"
	}
	m.message = sameMessage(old.Messages, fresh.Messages, m.message)
	added := len(fresh.Messages) - len(old.Messages)
	if added <= 0 {
		return
	}
	if follow {
		m.message = len(fresh.Messages) - 1
		m.newMessages = 0
		m.scrollToMessage()
		return
	}
	m.newMessages += added
}

// atThreadEnd reports whether the last message is selected or the view
// shows the bottom of the open item.
func (m *Model) atThreadEnd() bool {
	if m.message >= len(m.detail.Messages)-1 {
		return true
	}
	lines := m.detailLines(m.detailInnerWidth(m.contentWidth()))
	_, limit := m.layoutBudget()
	return threadEnd(len(lines), m.scroll, limit)
}

// threadEnd reports whether a view of n lines scrolled to scroll with
// limit visible rows shows the last line. clipLines clamps the same way.
func threadEnd(n, scroll, limit int) bool {
	return limit <= 0 || scroll >= n-limit
}

// sameMessage is the index in fresh of old[index], so the selection
// follows the message rather than its position.
func sameMessage(old, fresh []decide.Message, index int) int {
	if index >= 0 && index < len(old) {
		want := old[index]
		if i := slices.IndexFunc(fresh, func(msg decide.Message) bool {
			return msg.Author == want.Author && msg.Time.Equal(want.Time) && msg.Text == want.Text
		}); i >= 0 {
			return i
		}
	}
	return max(min(index, len(fresh)-1), 0)
}

// footerStatus is the status text with the new-message marker in front.
func (m *Model) footerStatus() string {
	if m.newMessages == 0 {
		return m.status
	}
	marker := fmt.Sprintf("↓ %d new message", m.newMessages)
	if m.newMessages > 1 {
		marker += "s"
	}
	if m.status == "" {
		return marker
	}
	return marker + "  " + m.status
}
