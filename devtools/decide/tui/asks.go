package tui

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func (m *Model) asksBackend() (AsksBackend, bool) {
	ab, ok := m.store().(AsksBackend)
	return ab, ok
}

func (m *Model) hasAsks() bool {
	return m.asksOn
}

func (m *Model) asksNavIndex() int {
	idx := len(Folders) + 2
	if m.hasOutbox() {
		idx++
	}
	return idx
}

func (m *Model) refreshAsks() {
	ab, ok := m.asksBackend()
	if !ok {
		return
	}
	if !m.asksOnExplicit {
		if on, err := ab.AsksEnabled(); err == nil {
			m.asksOn = on
		}
	}
	if !m.asksOn {
		m.asks = nil
		return
	}
	if asks, err := ab.ListAsks(); err == nil {
		m.asks = asks
	}
	m.clampAskCursor()
}

func (m *Model) showAsks() error {
	if !m.hasAsks() {
		m.status = "unknown go-to key"
		return nil
	}
	m.flushAllDrafts()
	m.composing = false
	m.answerFocus = false
	m.replyFocus = false
	m.replySend = false
	m.detail = nil
	m.askClosing = ""
	m.askCloseBuf = ""
	m.askCloseErr = ""
	m.mode = modeAsks
	m.scroll = 0
	m.refreshAsks()
	m.clampAskCursor()
	m.status = "asks"
	return nil
}

func (m *Model) visibleAsks() []decide.AskRecord {
	q := strings.ToLower(m.listQuery())
	if q == "" && m.agentFilter == "" {
		return m.asks
	}
	out := make([]decide.AskRecord, 0, len(m.asks))
	for _, a := range m.asks {
		if m.agentFilter != "" && !strings.EqualFold(a.Owner, m.agentFilter) && !strings.EqualFold(a.RawOwner, m.agentFilter) {
			continue
		}
		if q != "" {
			hay := strings.ToLower(strings.Join([]string{
				a.ID, a.ItemID, a.Person, a.What, a.Owner, a.Status, a.CloseReason, strings.Join(a.Links, " "),
			}, " "))
			if !strings.Contains(hay, q) {
				continue
			}
		}
		out = append(out, a)
	}
	return out
}

func (m *Model) clampAskCursor() {
	n := len(m.visibleAsks())
	if n == 0 {
		m.askCursor = 0
		return
	}
	if m.askCursor < 0 {
		m.askCursor = 0
	}
	if m.askCursor >= n {
		m.askCursor = n - 1
	}
}

func (m *Model) moveAsks(delta int) {
	n := len(m.visibleAsks())
	if n == 0 {
		m.askCursor = 0
		return
	}
	m.askCursor += delta
	m.clampAskCursor()
}

func (m *Model) selectedAsk() (decide.AskRecord, bool) {
	asks := m.visibleAsks()
	if m.askCursor < 0 || m.askCursor >= len(asks) {
		return decide.AskRecord{}, false
	}
	return asks[m.askCursor], true
}

func (m *Model) handleAsksKey(s decide.KeyStroke) (bool, error) {
	if s.Ctrl || s.Alt {
		return false, nil
	}
	switch s.Key {
	case "j", "down":
		m.moveAsks(1)
		return true, nil
	case "k", "up":
		m.moveAsks(-1)
		return true, nil
	case "home":
		m.askCursor = 0
		return true, nil
	case "G", "end":
		m.askCursor = max(len(m.visibleAsks())-1, 0)
		return true, nil
	case "u", "esc":
		m.backToList()
		return true, nil
	case "enter", "o":
		if a, ok := m.selectedAsk(); ok {
			return true, m.openItemByID(a.ItemID)
		}
		return true, nil
	case "d":
		m.beginAskClose(decide.AskStatusDone)
		return true, nil
	case "x":
		m.beginAskClose(decide.AskStatusDropped)
		return true, nil
	}
	return false, nil
}

func (m *Model) beginAskClose(status string) {
	a, ok := m.selectedAsk()
	if !ok {
		return
	}
	if a.Status != decide.AskStatusOpen {
		m.status = fmt.Sprintf("ask %s is already %s", a.ID, a.Status)
		return
	}
	m.askClosing = status
	m.askCloseID = a.ID
	m.askCloseBuf = ""
	m.askCloseErr = ""
}

func (m *Model) askCloseKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.askClosing = ""
		m.askCloseErr = ""
		return false, nil
	case '\r':
		reason := strings.TrimSpace(m.askCloseBuf)
		if reason == "" {
			m.askCloseErr = "reason is required"
			return false, nil
		}
		ab, ok := m.asksBackend()
		if !ok {
			m.askCloseErr = "asks backend unavailable"
			return false, nil
		}
		closed, err := ab.CloseAsk(m.askCloseID, m.askClosing, reason)
		if err != nil {
			m.askCloseErr = err.Error()
			return false, nil
		}
		m.askClosing = ""
		m.askCloseErr = ""
		m.status = fmt.Sprintf("%s %s", closed.ID, closed.Status)
		return false, m.Reload()
	case '\b', '\x7f':
		m.askCloseBuf = dropLastRune(m.askCloseBuf)
		m.askCloseErr = ""
	default:
		if r >= ' ' {
			m.askCloseBuf += string(r)
			m.askCloseErr = ""
		}
	}
	return false, nil
}

func (m *Model) askCloseLines() []screenLine {
	verb := "mark done"
	if m.askClosing == decide.AskStatusDropped {
		verb = "drop"
	}
	lines := []screenLine{
		{text: plainText(fmt.Sprintf("%s ask %s", verb, m.askCloseID))},
		{text: plainText("> reason: " + withCursor(m.askCloseBuf, len([]rune(m.askCloseBuf))))},
	}
	if m.askCloseErr != "" {
		lines = append(lines, screenLine{text: plainText(m.askCloseErr)})
	}
	return boxLines(lines)
}

func (m *Model) asksMail(pane int) []screenLine {
	if m.width <= 0 {
		return stackRegions(0, m.sidebarLines(), m.asksScreen(0))
	}
	if m.narrow() {
		return stackRegions(pane, []screenLine{m.folderTabLine(pane)}, m.asksScreen(pane))
	}
	sidebar := m.sidebarLines()
	sideW, mainW := listColumnWidths(maxWidth(sidebar), pane)
	cols := [][]screenLine{
		fitColumnLines(sidebar, sideW),
		m.asksScreen(mainW),
	}
	return zipFixed(cols, []int{sideW, mainW})
}

func (m *Model) asksScreen(width int) []screenLine {
	asks := m.visibleAsks()
	if len(asks) == 0 {
		lines := []screenLine{{text: m.emptyText("no asks")}}
		lines = m.withFilterHeader(lines)
		if width > 0 {
			return fitColumnLines(lines, width)
		}
		return lines
	}
	now := m.now()
	loc := m.zone()
	lines := make([]screenLine, 0, len(asks)*2)
	for i, a := range asks {
		prefix := "  "
		if i == m.askCursor {
			prefix = "> "
		}
		line := fmt.Sprintf("%s%-7s  %-7s  %-6s  %-10s  %s: %s",
			prefix, a.ID, a.Status, a.ItemID, stamp(a.CreatedAt, now, loc), a.Person, a.What)
		if a.Owner != "" {
			line += "  (" + a.Owner + ")"
		}
		if len(a.Links) > 0 {
			line += "  [" + strings.Join(a.Links, ", ") + "]"
		}
		lines = append(lines, screenLine{text: line})
		if a.CloseReason != "" {
			lines = append(lines, screenLine{text: fmt.Sprintf("    %s by %s: %s", a.Status, a.ClosedBy, a.CloseReason)})
		}
	}
	_, limit := m.layoutBudget()
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	lines = m.withFilterHeader(lines)
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

func (m *Model) asksHintLine() string {
	var parts []string
	appendHintIf(&parts, m.goToHintLabel())
	parts = append(parts, "j/k move", "enter open thread", "d done", "x drop", "u list")
	appendHintIf(&parts, m.helpHintLabel())
	return strings.Join(parts, "  ")
}

func (m *Model) asksPanelData() json.RawMessage {
	openCount := decide.OpenAskCount(m.asks)
	rows := []decide.PanelListRow{
		{
			Label: "open",
			Value: fmt.Sprintf("%d", openCount),
			State: func() string {
				if openCount > 0 {
					return decide.PanelStateWarn
				}
				return decide.PanelStateOK
			}(),
		},
		{
			Label: "total",
			Value: fmt.Sprintf("%d", len(m.asks)),
			State: decide.PanelStateOK,
		},
	}
	for _, a := range m.asks {
		if a.Status != decide.AskStatusOpen {
			continue
		}
		rows = append(rows, decide.PanelListRow{
			Label: a.ID + " " + a.Person,
			Value: a.What,
		})
		if len(rows) >= 6 {
			break
		}
	}
	raw, _ := json.Marshal(decide.PanelListData{Rows: rows})
	return raw
}
