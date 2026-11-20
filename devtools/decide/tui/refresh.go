package tui

import (
	"fmt"
	"reflect"
	"slices"
	"strings"
	"time"

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
	limit, _ := m.detailLayout(m.contentWidth())
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

// loadingStatus is the status line until the first list arrives.
const loadingStatus = "Loading…"

// footerStatus is the status text with the new-message marker in front
// and any hub replica freshness text, or Loading… until the first list arrives.
func (m *Model) footerStatus() string {
	if m.loading && m.status == "" {
		return loadingStatus
	}
	status := m.status
	if fresh := m.freshnessStatus(); fresh != "" {
		switch {
		case status == "":
			status = fresh
		case !strings.Contains(status, "unreachable"):
			status += "  " + fresh
		}
	}
	if m.newMessages == 0 {
		return status
	}
	marker := fmt.Sprintf("↓ %d new message", m.newMessages)
	if m.newMessages > 1 {
		marker += "s"
	}
	if status == "" {
		return marker
	}
	return marker + "  " + status
}

func (m *Model) freshnessStatus() string {
	rb, ok := m.store().(ReplicaBackend)
	if !ok {
		return ""
	}
	st := rb.SyncStatus()
	if st.Connected {
		if st.SyncedAt.IsZero() {
			return ""
		}
		return formatSyncedAgo(m.now().Sub(st.SyncedAt))
	}
	if st.SyncedAt.IsZero() {
		return unreachableStatus
	}
	return "hub unreachable; showing copy from " + st.SyncedAt.In(m.zone()).Format("15:04")
}

func formatSyncedAgo(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	secs := int(d / time.Second)
	switch {
	case secs < 60:
		return fmt.Sprintf("synced %ds ago", secs)
	case secs < 3600:
		return fmt.Sprintf("synced %dm ago", secs/60)
	case secs < 86400:
		return fmt.Sprintf("synced %dh ago", secs/3600)
	default:
		return fmt.Sprintf("synced %dd ago", secs/86400)
	}
}
