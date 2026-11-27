package tui

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func (m *Model) flagBackend() (FlagBackend, bool) {
	fb, ok := m.store().(FlagBackend)
	return fb, ok
}

// beginFlag opens the reason picker popup for the current item/message.
func (m *Model) beginFlag() {
	fb, ok := m.flagBackend()
	if !ok {
		m.status = "flagging is not supported by this backend"
		return
	}
	it, ok := m.selectedFlagItem()
	if !ok {
		return
	}
	reasons, err := fb.Reasons()
	if err != nil {
		m.status = err.Error()
		return
	}
	if len(reasons) == 0 {
		reasons = decide.DefaultReasons()
	}

	cp := it
	if len(cp.Messages) == 0 {
		cp.Messages = []decide.Message{{
			Author:   cp.Owner,
			Time:     cp.CreatedAt,
			Text:     cp.Question,
			Verbatim: cp.Verbatim,
		}}
	}
	msgIdx, agent, preview := resolveTUIFlagTarget(&cp, m.mode == modeDetail, m.message)

	m.flagging = true
	m.flagItemID = cp.ID
	m.flagMsgIdx = msgIdx
	m.flagAgent = agent
	m.flagPreview = preview
	m.flagReasons = reasons
	m.flagSelected = make(map[string]bool)
	m.flagNote = ""
	m.flagCursor = 0
	m.flagErr = ""
}

func (m *Model) selectedFlagItem() (decide.Item, bool) {
	if m.mode == modeDetail && m.detail != nil {
		return *m.detail, true
	}
	return m.selectedItem()
}

func resolveTUIFlagTarget(it *decide.Item, isDetail bool, detailMsg int) (msgIdx *int, agent, preview string) {
	if isDetail && detailMsg >= 0 && detailMsg < len(it.Messages) {
		msg := it.Messages[detailMsg]
		if msg.Author != decide.Human && msg.Author != "system" {
			idx := detailMsg
			return &idx, msg.Sender(), messagePreview(decide.ComposeNoteText(msg.Text, msg.Verbatim))
		}
	}
	for i := len(it.Messages) - 1; i >= 0; i-- {
		msg := it.Messages[i]
		if msg.Author == decide.Human || msg.Author == "system" {
			continue
		}
		idx := i
		return &idx, msg.Sender(), messagePreview(decide.ComposeNoteText(msg.Text, msg.Verbatim))
	}
	return nil, it.Owner, messagePreview(decide.ComposeNoteText(it.Question, it.Verbatim))
}

func messagePreview(text string) string {
	first, _, _ := strings.Cut(strings.TrimSpace(text), "\n")
	runes := []rune(first)
	if len(runes) > 64 {
		return string(runes[:61]) + "..."
	}
	return first
}

func (m *Model) moveFlagCursor(delta int) {
	total := len(m.flagReasons) + 2 // reasons + note row + Submit row
	if total <= 0 {
		m.flagCursor = 0
		return
	}
	m.flagCursor = max(min(m.flagCursor+delta, total-1), 0)
}

func (m *Model) cycleFlagCursor(reverse bool) {
	noteIdx := len(m.flagReasons)
	submitIdx := noteIdx + 1
	if reverse {
		switch {
		case m.flagCursor > noteIdx:
			m.flagCursor = noteIdx
		case m.flagCursor == noteIdx:
			m.flagCursor = max(noteIdx-1, 0)
		default:
			m.flagCursor = submitIdx
		}
		return
	}
	switch {
	case m.flagCursor < noteIdx:
		m.flagCursor = noteIdx
	case m.flagCursor == noteIdx:
		m.flagCursor = submitIdx
	default:
		m.flagCursor = 0
	}
}

func (m *Model) flagOverlayKeyMsg(msg tea.KeyMsg) bool {
	if !m.flagging {
		return false
	}
	switch msg.Type {
	case tea.KeyUp:
		m.moveFlagCursor(-1)
		return true
	case tea.KeyDown:
		m.moveFlagCursor(1)
		return true
	case tea.KeyTab:
		m.cycleFlagCursor(false)
		return true
	case tea.KeyShiftTab:
		m.cycleFlagCursor(true)
		return true
	}
	if m.isSendKey(msg) {
		m.submitFlag()
		return true
	}
	return false
}

func (m *Model) flagKey(r rune) (bool, error) {
	noteIdx := len(m.flagReasons)
	submitIdx := noteIdx + 1

	switch r {
	case '\x1b':
		m.flagging = false
		return false, nil
	case '\t':
		m.cycleFlagCursor(false)
		return false, nil
	}

	if m.flagCursor == noteIdx {
		return m.flagNoteKey(r)
	}
	if m.flagCursor == submitIdx {
		return m.flagSubmitRowKey(r)
	}

	switch r {
	case 'j':
		m.moveFlagCursor(1)
	case 'k':
		m.moveFlagCursor(-1)
	case ' ':
		m.toggleCurrentFlagReason()
	case '\r':
		m.flagReasonEnter(noteIdx)
	}
	return false, nil
}

func (m *Model) flagSubmitRowKey(r rune) (bool, error) {
	switch r {
	case 'k':
		m.moveFlagCursor(-1)
	case 'j':
		m.moveFlagCursor(1)
	case '\r', ' ':
		m.submitFlag()
	}
	return false, nil
}

func (m *Model) flagReasonEnter(noteIdx int) {
	if len(m.selectedFlagReasons()) > 0 {
		m.submitFlag()
		return
	}
	m.toggleCurrentFlagReason()
	if m.flagCursor >= 0 && m.flagCursor < len(m.flagReasons) && m.flagReasons[m.flagCursor].RequireNote && strings.TrimSpace(m.flagNote) == "" {
		m.flagCursor = noteIdx
	}
}

func (m *Model) flagNoteKey(r rune) (bool, error) {
	switch r {
	case '\r':
		m.submitFlag()
	case '\b', '\x7f':
		m.flagNote = dropLastRune(m.flagNote)
		m.flagErr = ""
	default:
		if r >= ' ' {
			m.flagNote += string(r)
			m.flagErr = ""
		}
	}
	return false, nil
}

func (m *Model) toggleCurrentFlagReason() {
	if m.flagCursor < 0 || m.flagCursor >= len(m.flagReasons) {
		return
	}
	id := m.flagReasons[m.flagCursor].ID
	m.flagSelected[id] = !m.flagSelected[id]
	m.flagErr = ""
}

func (m *Model) selectedFlagReasons() []string {
	var selected []string
	for _, r := range m.flagReasons {
		if m.flagSelected[r.ID] {
			selected = append(selected, r.ID)
		}
	}
	return selected
}

func (m *Model) submitFlag() {
	fb, ok := m.flagBackend()
	if !ok {
		m.flagErr = "flagging is not supported"
		return
	}
	selected := m.selectedFlagReasons()
	canon, note, err := decide.ValidateReasonSelection(m.flagReasons, selected, m.flagNote)
	if err != nil {
		m.flagErr = err.Error()
		if len(selected) > 0 && strings.TrimSpace(m.flagNote) == "" {
			m.flagCursor = len(m.flagReasons)
		}
		return
	}
	if _, err := fb.Flag(m.flagItemID, m.flagMsgIdx, canon, note); err != nil {
		m.flagErr = err.Error()
		return
	}
	m.flagging = false
	m.status = fmt.Sprintf("flagged %s (%s)", m.flagItemID, strings.Join(canon, ", "))
}

func (m *Model) flagLines() []screenLine {
	header := fmt.Sprintf("flag %s", m.flagItemID)
	if m.flagAgent != "" {
		header += " (agent: " + m.flagAgent + ")"
	}
	lines := []screenLine{{text: plainText(header)}}
	if m.flagPreview != "" {
		lines = append(lines, screenLine{text: plainText("  " + m.flagPreview)})
	}
	lastCat := ""
	for i, r := range m.flagReasons {
		if r.Category != lastCat {
			lastCat = r.Category
			lines = append(lines, screenLine{text: plainText("  [" + r.Category + "]")})
		}
		cursor := "  "
		if i == m.flagCursor {
			cursor = "> "
		}
		box := "[ ]"
		if m.flagSelected[r.ID] {
			box = "[x]"
		}
		req := ""
		if r.RequireNote {
			req = " *"
		}
		lines = append(lines, screenLine{
			text: plainText(fmt.Sprintf("%s%s %s — %s%s", cursor, box, r.ID, r.Label, req)),
		})
	}
	noteIdx := len(m.flagReasons)
	notePrefix := "  "
	if m.flagCursor == noteIdx {
		notePrefix = "> "
	}
	noteVal := m.flagNote
	if m.flagCursor == noteIdx {
		noteVal = withCursor(m.flagNote, len([]rune(m.flagNote)))
	}
	lines = append(lines, screenLine{text: plainText(notePrefix + "note: " + noteVal)})

	submitIdx := noteIdx + 1
	submitPrefix := "  "
	if m.flagCursor == submitIdx {
		submitPrefix = "> "
	}
	lines = append(lines, screenLine{text: plainText(submitPrefix + "[Submit]")})
	if m.flagErr != "" {
		lines = append(lines, screenLine{text: plainText(m.flagErr)})
	}
	return boxLines(lines)
}

const flagHint = "space toggle  j/k/tab move  enter submit  esc cancel"
