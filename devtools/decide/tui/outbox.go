package tui

import (
	"fmt"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func (m *Model) outboxBackend() (OutboxBackend, bool) {
	ob, ok := m.store().(OutboxBackend)
	if !ok || !ob.OutboxEnabled() {
		return nil, false
	}
	return ob, true
}

func (m *Model) hasOutbox() bool {
	_, ok := m.outboxBackend()
	return ok
}

func (m *Model) outboxNavIndex() int {
	return len(Folders) + 2
}

func (m *Model) outboxStaleThreshold() time.Duration {
	if m.OutboxStaleAfter > 0 {
		return m.OutboxStaleAfter
	}
	return decide.DefaultOutboxStaleAfter
}

func (m *Model) showOutbox() error {
	if !m.hasOutbox() {
		m.status = "unknown go-to key"
		return nil
	}
	m.composing = false
	m.answerFocus = false
	m.replyFocus = false
	m.replySend = false
	m.detail = nil
	m.mode = modeOutbox
	m.scroll = 0
	m.clampOutboxCursor()
	return nil
}

func (m *Model) outboxEntries() []OutboxEntry {
	ob, ok := m.outboxBackend()
	if !ok {
		return nil
	}
	entries := ob.Outbox()
	q := strings.ToLower(m.listQuery())
	if q == "" {
		return entries
	}
	var filtered []OutboxEntry
	for _, e := range entries {
		if strings.Contains(strings.ToLower(outboxSearchText(e)), q) {
			filtered = append(filtered, e)
		}
	}
	return filtered
}

func outboxSearchText(e OutboxEntry) string {
	return strings.Join([]string{e.ID, e.Op, e.ItemID, e.To, e.Text, e.HoldReason, e.Error}, " ")
}

func (m *Model) clampOutboxCursor() {
	n := len(m.outboxEntries())
	if n == 0 {
		m.outboxCursor = 0
		return
	}
	if m.outboxCursor < 0 {
		m.outboxCursor = 0
	}
	if m.outboxCursor >= n {
		m.outboxCursor = n - 1
	}
}

func (m *Model) moveOutbox(delta int) {
	n := len(m.outboxEntries())
	if n == 0 {
		m.outboxCursor = 0
		return
	}
	m.outboxCursor += delta
	m.clampOutboxCursor()
}

func (m *Model) selectedOutbox() (OutboxEntry, bool) {
	entries := m.outboxEntries()
	if m.outboxCursor < 0 || m.outboxCursor >= len(entries) {
		return OutboxEntry{}, false
	}
	return entries[m.outboxCursor], true
}

func (m *Model) outboxMail(pane int) []screenLine {
	if m.width <= 0 {
		return stackRegions(0, m.sidebarLines(), m.outboxScreen(0))
	}
	if m.narrow() {
		return stackRegions(pane, []screenLine{m.folderTabLine(pane)}, m.outboxScreen(pane))
	}
	sidebar := m.sidebarLines()
	sideW, mainW := listColumnWidths(maxWidth(sidebar), pane)
	cols := [][]screenLine{
		fitColumnLines(sidebar, sideW),
		m.outboxScreen(mainW),
	}
	return zipFixed(cols, []int{sideW, mainW})
}

func (m *Model) outboxScreen(width int) []screenLine {
	entries := m.outboxEntries()
	if len(entries) == 0 {
		lines := []screenLine{{text: m.emptyText("outbox empty")}}
		lines = m.withFilterHeader(lines)
		if width > 0 {
			return fitColumnLines(lines, width)
		}
		return lines
	}
	now := m.now()
	loc := m.zone()
	lines := make([]screenLine, 0, len(entries))
	for i, e := range entries {
		prefix := "  "
		if i == m.outboxCursor {
			prefix = "> "
		}
		target := e.ItemID
		if target == "" {
			target = e.To
		}
		if target == "" {
			target = e.Name
		}
		if target == "" {
			target = "-"
		}
		state := outboxEntryStateText(e)
		preview := strings.ReplaceAll(strings.TrimSpace(e.Text), "\n", " ")
		if preview == "" {
			preview = e.Choice
		}
		line := fmt.Sprintf("%s%-6s  %-8s  %-6s  %-8s  %s", prefix, e.ID, e.Op, target, stamp(e.WrittenAt, now, loc), state)
		if preview != "" {
			line += "  " + preview
		}
		lines = append(lines, screenLine{text: line})
	}
	_, limit := m.layoutBudget()
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	lines = m.withFilterHeader(lines)
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

func outboxEntryStateText(e OutboxEntry) string {
	switch e.Status {
	case OutboxHeld:
		return "held: " + e.HoldReason
	case OutboxFailed:
		return lipgloss.NewStyle().Foreground(lipgloss.Color("9")).Render("failed: " + e.Error)
	default:
		return "in outbox"
	}
}

func (m *Model) itemOutboxEntries(itemID string) []OutboxEntry {
	ob, ok := m.outboxBackend()
	if !ok || itemID == "" {
		return nil
	}
	var out []OutboxEntry
	for _, e := range ob.Outbox() {
		if e.ItemID == itemID {
			out = append(out, e)
		}
	}
	return out
}

func (m *Model) outboxThreadLines(it *decide.Item, width int, now time.Time, loc *time.Location) []screenLine {
	entries := m.itemOutboxEntries(it.ID)
	if len(entries) == 0 {
		return nil
	}
	var lines []screenLine
	for _, e := range entries {
		lines = append(lines, screenLine{text: plainText(strings.Repeat("─", max(width, 8)))})
		hdr := fmt.Sprintf("human  %s  %s", stamp(e.WrittenAt, now, loc), outboxEntryStateText(e))
		lines = append(lines, screenLine{text: hdr})
		body := e.Text
		if body == "" {
			switch e.Op {
			case "archive":
				body = "(archive)"
			case "unsnooze":
				body = "(unsnooze)"
			case "snooze":
				if e.Until != nil {
					body = "(snooze until " + stamp(*e.Until, now, loc) + ")"
				}
			}
		}
		if body != "" {
			for _, l := range questionLines(body, max(width-2, 1), nil, false) {
				lines = append(lines, indentLine(l))
			}
		}
		if e.Status == OutboxHeld {
			lines = append(lines, indentLine(screenLine{text: plainText("[s] send  [e] edit  [x] cancel")}))
		}
	}
	return lines
}

func (m *Model) openNavFolder(i int) error {
	if m.hasOutbox() && i == m.outboxNavIndex() {
		return m.showOutbox()
	}
	return m.showFolder(i)
}

func (m *Model) restoreFailedOutbox() {
	ob, ok := m.outboxBackend()
	if !ok {
		return
	}
	if m.restoredFailed == nil {
		m.restoredFailed = make(map[string]bool)
	}
	for _, e := range ob.Outbox() {
		if e.Status == OutboxFailed && !m.restoredFailed[e.ID] {
			m.restoreFailedEntry(e)
		}
	}
}

func (m *Model) restoreFailedEntry(e OutboxEntry) {
	onDetail := m.detail != nil && m.detail.ID == e.ItemID
	switch e.Op {
	case "reply":
		if onDetail && m.reply == "" {
			m.reply = e.Text
			m.replyField.SetValue(e.Text)
			m.restoredFailed[e.ID] = true
		}
	case "answer":
		if onDetail && m.draft == "" {
			m.draft = e.Text
			m.free.SetValue(e.Text)
			m.restoredFailed[e.ID] = true
		}
	case "done":
		if onDetail && m.draft == "" {
			m.draft = e.Text
			m.outcome.SetValue(e.Text)
			m.restoredFailed[e.ID] = true
		}
	case "send":
		if m.note == "" {
			m.note = e.Text
			m.pullNote()
			m.restoredFailed[e.ID] = true
		}
	}
}

func (m *Model) outboxStatus() string {
	ob, ok := m.outboxBackend()
	if !ok {
		return ""
	}
	entries := ob.Outbox()
	if len(entries) == 0 {
		return ""
	}
	now := m.now()
	thresh := m.outboxStaleThreshold()
	var failed, held, stale int
	for _, e := range entries {
		switch e.Status {
		case OutboxFailed:
			failed++
		case OutboxHeld:
			held++
		case OutboxPending:
			if now.Sub(e.WrittenAt) >= thresh {
				stale++
			}
		}
	}
	if failed > 0 {
		return fmt.Sprintf("outbox: %d failed", failed)
	}
	if held > 0 {
		return fmt.Sprintf("outbox: %d held", held)
	}
	if stale > 0 {
		return fmt.Sprintf("outbox: %d pending (>%s)", stale, formatShortDuration(thresh))
	}
	return ""
}

func formatShortDuration(d time.Duration) string {
	if d%time.Minute == 0 {
		return fmt.Sprintf("%dm", int(d/time.Minute))
	}
	return d.String()
}

func (m *Model) sendOutboxByID(ob OutboxBackend, id string) {
	if err := ob.SendOutbox(id); err != nil {
		m.status = err.Error()
		return
	}
	m.status = "sending " + id
}

func (m *Model) cancelOutboxByID(ob OutboxBackend, id string) {
	if err := ob.CancelOutbox(id); err != nil {
		m.status = err.Error()
		return
	}
	m.status = "cancelled " + id
	m.clampOutboxCursor()
}

func (m *Model) openSelectedOutbox(e OutboxEntry) error {
	if e.ItemID != "" {
		return m.openItemByID(e.ItemID)
	}
	m.editOutboxEntry(e)
	return nil
}

func (m *Model) handleOutboxKey(s decide.KeyStroke) (bool, error) {
	if s.Ctrl || s.Alt || !m.hasOutbox() {
		return false, nil
	}
	switch s.Key {
	case "j", "down":
		m.moveOutbox(1)
		return true, nil
	case "k", "up":
		m.moveOutbox(-1)
		return true, nil
	case "u", "esc":
		m.backToList()
		return true, nil
	}
	return m.handleOutboxEntryKey(s.Key)
}

func (m *Model) handleOutboxEntryKey(key string) (bool, error) {
	e, ok := m.selectedOutbox()
	if !ok {
		return false, nil
	}
	ob, _ := m.outboxBackend()
	switch key {
	case "enter", "o":
		return false, m.openSelectedOutbox(e)
	case "s":
		if e.Status == OutboxHeld {
			m.sendOutboxByID(ob, e.ID)
		}
	case "e":
		m.editOutboxEntry(e)
	case "x", "d":
		m.cancelOutboxByID(ob, e.ID)
	default:
		return false, nil
	}
	return true, nil
}

func (m *Model) openItemByID(id string) error {
	for _, it := range m.allItems {
		if it.ID == id {
			cp := it
			m.detail = &cp
			m.mode = modeDetail
			m.scroll = 0
			m.message = max(len(cp.Messages)-1, 0)
			m.newMessages = 0
			m.detailGone = false
			m.restoreFailedOutbox()
			return nil
		}
	}
	m.status = "item " + id + " not found"
	return nil
}

func (m *Model) editOutboxEntry(e OutboxEntry) {
	if e.ItemID != "" && (m.detail == nil || m.detail.ID != e.ItemID) {
		_ = m.openItemByID(e.ItemID)
	}
	m.outboxEditing = e.ID
	switch e.Op {
	case "reply":
		m.reply = e.Text
		m.replyField.SetValue(e.Text)
		m.replyFocus = true
		m.replyField.Focus()
	case "answer":
		m.draft = e.Text
		m.free.SetValue(e.Text)
		m.answerFocus = true
		m.free.Focus()
	case "done":
		m.draft = e.Text
		m.outcome.SetValue(e.Text)
		m.mode = modeOutcome
		m.outcome.Focus()
	case "send":
		m.focusCompose()
		m.note = e.Text
		m.composeFocus = focusText
		m.pullNote()
		m.noteField.Focus()
	}
}

func targetItemOutboxEntry(entries []OutboxEntry) OutboxEntry {
	for _, e := range entries {
		if e.Status == OutboxHeld || e.Status == OutboxFailed {
			return e
		}
	}
	return entries[len(entries)-1]
}

func (m *Model) handleDetailOutboxKey(s decide.KeyStroke) (bool, error) {
	if s.Ctrl || s.Alt || m.detail == nil {
		return false, nil
	}
	ob, ok := m.outboxBackend()
	if !ok {
		return false, nil
	}
	entries := m.itemOutboxEntries(m.detail.ID)
	if len(entries) == 0 {
		return false, nil
	}
	target := targetItemOutboxEntry(entries)
	switch s.Key {
	case "s":
		if target.Status == OutboxHeld {
			m.sendOutboxByID(ob, target.ID)
			return true, nil
		}
	case "e":
		if target.Status == OutboxHeld || target.Status == OutboxFailed {
			m.editOutboxEntry(target)
			return true, nil
		}
	case "x":
		m.cancelOutboxByID(ob, target.ID)
		return true, nil
	}
	return false, nil
}

func (m *Model) requestQuit() (bool, error) {
	if ob, ok := m.outboxBackend(); ok {
		if n := len(ob.Outbox()); n > 0 {
			m.confirmKind = confirmQuit
			word := "entry"
			if n != 1 {
				word = "entries"
			}
			m.status = fmt.Sprintf("outbox has %d pending %s; quit anyway? y/n", n, word)
			return false, nil
		}
	}
	return true, nil
}
