package tui

import (
	"fmt"
	"strings"
	"time"

	"github.com/charmbracelet/bubbles/textarea"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

const sendsName = decide.SendsName

// Target is a named pane the compose box can send to. A bus target is
// an agentbus name: Pane is its handle, When is its --when text, and
// Dead marks a holder agentbus reports dead.
type Target struct {
	Name string
	Pane string
	Bus  bool
	When string
	Dead bool
	// Live is the bus liveness word: alive, dead, hibernated, or unknown.
	Live string
	// LiveAge is how long before the bus list agentbus observed Live;
	// nil when it never did.
	LiveAge *time.Duration
	// Fav marks a starred name, listed first.
	Fav bool
}

// onBus reports a target delivered with `agentbus send`: a bus name, or
// a pane that starts with agentbus:, as LocalBackend.Send routes it.
func (tg Target) onBus() bool {
	_, ok := decide.AgentbusPaneRecipient(tg.Pane)
	return tg.Bus || ok
}

type sendRecord struct {
	Target string `json:"target"`
	Pane   string `json:"pane"`
	Text   string `json:"text"`
}

// ShiftEnter inserts a newline while the compose text is focused.
func (m *Model) ShiftEnter() {
	if !m.composing || m.composeFocus != focusText {
		return
	}
	m.insertNoteNewline()
}

// ShiftTab moves compose focus back from [Send] to the text to the recipient.
func (m *Model) ShiftTab() {
	if m.replySend && !m.help {
		m.prevReplyButton()
		return
	}
	if m.help || m.answerFocus || !m.composing {
		return
	}
	if m.composeFocus == focusText {
		m.note = m.noteField.Value()
		m.noteField.Blur()
	}
	if m.composeFocus > focusRecipient {
		m.composeFocus--
	}
	if m.composeFocus == focusText {
		m.pullNote()
		m.noteField.Focus()
	}
}

func (m *Model) composeKey(r rune) (bool, error) {
	if m.composeRestorePrompt {
		switch r {
		case 'r', 'R':
			m.restoreComposeDraft()
			return false, nil
		case 'd', 'D':
			m.discardComposeDraft()
			return false, nil
		}
	}
	if m.composeFocus == focusSend && m.hasDraftConflict(composerNote) {
		switch r {
		case 'k', 'K':
			m.resolveConflict(composerNote, conflictKeepCurrent)
			return false, nil
		case 'u', 'U':
			m.resolveConflict(composerNote, conflictUseOther)
			return false, nil
		case 'm', 'M':
			m.resolveConflict(composerNote, conflictMergeBoth)
			return false, nil
		}
	}
	if r == '\t' {
		m.advanceCompose()
		return false, nil
	}
	if m.composeFocus == focusRecipient && m.pickerKey(r) {
		return false, nil
	}
	switch r {
	case '\x1b':
		m.blurCompose()
		return false, nil
	case '\r', '\n':
		return m.composeEnter()
	}
	m.typeRune(r)
	return false, nil
}

// sendComposer sends the draft of composer kind: Tab then Enter in the
// note and reply composers, Enter in the others.
func (m *Model) sendComposer(kind composerKind) tea.Cmd {
	switch kind {
	case composerNote:
		m.advanceCompose()
	case composerReply:
		m.focusReplySend()
	}
	return m.applyRune('\r')
}

func (m *Model) composeEnter() (bool, error) {
	switch m.composeFocus {
	case focusText:
		m.insertNoteNewline()
		return false, nil
	case focusSend:
		return false, m.sendNote()
	default:
		return false, nil
	}
}

func (m *Model) advanceCompose() {
	if m.composeFocus == focusText {
		m.note = m.noteField.Value()
		m.noteField.Blur()
	}
	if m.composeFocus < focusSend {
		m.composeFocus++
	}
	if m.composeFocus == focusText {
		m.pullNote()
		m.noteField.Focus()
	}
}

func (m *Model) pickerArrow(delta int) {
	if m.composing && m.composeFocus == focusRecipient {
		m.moveTarget(delta)
	}
}

func (m *Model) sendNote() error {
	if strings.TrimSpace(m.note) == "" {
		return nil
	}
	tg, ok := m.selectedTarget()
	if !ok {
		m.status = "no compose target"
		return nil
	}
	if m.outboxEditing != "" {
		if ob, ok := m.outboxBackend(); ok {
			if err := ob.EditOutbox(m.outboxEditing, m.note); err != nil {
				m.status = err.Error()
				return nil
			}
			m.status = "updated " + m.outboxEditing
			m.outboxEditing = ""
			m.clearNote()
			return nil
		}
	}
	if m.askBusDown(tg.onBus(), m.sendNote) {
		return nil
	}
	if err := m.store().Send(m.forward, tg, m.note); err != nil {
		m.status = err.Error()
		return nil
	}
	if m.forward == "" && m.outboxEditing == "" {
		m.deleteDraft(composerNote)
	}
	m.clearNote()
	m.status = "sent to " + tg.Name
	return nil
}

// forwardItem opens compose on the viewed item with its context quoted.
// The cursor waits on the empty line after the quote.
func (m *Model) forwardItem() {
	if m.detail == nil {
		return
	}
	m.focusCompose()
	m.composeRestorePrompt = false
	m.forward = m.detail.ID
	m.note = forwardPrefill(m.detail, m.message, m.now(), m.zone())
	m.composeFocus = focusText
	m.pullNote()
	m.noteField.Focus()
}

// forwardQuoteLimit caps the quoted message so the draft stays editable.
const forwardQuoteLimit = 2000

// forwardPrefill quotes the question, message msg (or the latest when msg
// is out of range), and the first link of it.
func forwardPrefill(it *decide.Item, msg int, now time.Time, loc *time.Location) string {
	var b strings.Builder
	fmt.Fprintf(&b, "Forwarded from %s (owner: %s)\n", it.ID, it.Owner)
	line, rest, _ := strings.Cut(it.Question, "\n")
	question := clipRunes(line, 200)
	if question != line || strings.TrimSpace(rest) != "" {
		question += "…"
	}
	quote(&b, question)
	if n := len(it.Messages); n > 0 {
		if msg < 0 || msg >= n {
			msg = n - 1
		}
		m := it.Messages[msg]
		quote(&b, strings.TrimSpace(m.Sender()+" "+stamp(m.Time, now, loc)))
		text := m.Text
		if clipped := clipRunes(text, forwardQuoteLimit); clipped != text {
			text = clipped + "…"
		}
		quote(&b, text)
	}
	if links := itemLinks(it); len(links) > 0 {
		b.WriteString("Link: " + links[0] + "\n")
	}
	return b.String()
}

// quote writes each line of text prefixed with "> ".
func quote(b *strings.Builder, text string) {
	for _, line := range strings.Split(text, "\n") {
		b.WriteString("> " + line + "\n")
	}
}

// clipRunes keeps the first n runes of s.
func clipRunes(s string, n int) string {
	r := []rune(s)
	if len(r) <= n {
		return s
	}
	return string(r[:n])
}

// sendArgv builds the compose command. Tests compare the inbox argv
// with the shared helper.
func sendArgv(template, pane, text string) []string {
	return decide.SendArgv(template, pane, text)
}

func (m *Model) composeBody() []screenLine {
	width := m.contentWidth()
	if !m.framed() || width < 2 {
		return m.composeStack(width)
	}
	banner := m.composeRestoreLines(width)
	banner = append(banner, m.conflictBannerLines(composerNote, width)...)
	budget := m.height - 3 - len(banner)
	if budget < 3 {
		budget = 3
	}
	pickerLines, sel := m.pickerLines()
	av, reserve := m.pickerAvatar(width - 2)
	pickerBox, textLines := composeBoxHeights(len(pickerLines), budget)
	if reserve {
		pickerBox, textLines = composeBoxHeights(max(len(pickerLines), avatarRows), budget)
		if pickerBox-2 < avatarRows {
			reserve = false
			pickerBox, textLines = composeBoxHeights(len(pickerLines), budget)
		}
	}
	picker := windowLines(pickerLines, sel, pickerBox-2)
	if reserve {
		picker = besideAvatar(av, picker, width-2-avatarColumn)
	}
	textFocused := m.composeFocus == focusText
	noteBox := boxedEditor(m.note, &m.noteField, textFocused, hitNote, width, textLines, &m.noteScroll)
	regions := make([][]screenLine, 0, 4)
	if len(banner) > 0 {
		regions = append(regions, banner)
	}
	regions = append(regions,
		panelBox(picker, width, m.composeFocus == focusRecipient),
		noteBox,
		panelBox([]screenLine{m.sendLine(width - 2)}, width, m.composeFocus == focusSend),
	)
	return stackRegions(width, regions...)
}

// boxedEditor renders a bordered multi-line textarea of width columns and
// height inner text rows. It keeps the cursor line in view using scroll
// (when non-nil) and marks each text row with hitKind.
func boxedEditor(text string, field *textarea.Model, focused bool, hitKind, width, height int, scroll *int) []screenLine {
	wrap := 0
	if width > 0 {
		wrap = composeDraftWidth(width-2, focused)
	}
	lines := fieldLines(text, field, focused, wrap)
	lines, first := windowDraftScroll(lines, height, focused, scroll)
	lines = fieldRows(lines, hitKind, first, 0, wrap)
	return panelBox(lines, width, focused)
}

// composeBoxHeights sizes the recipient and message boxes inside budget rows.
// The send box is the remaining three rows: one label and its border.
func composeBoxHeights(pickerCount, budget int) (pickerBox, textLines int) {
	const sendBox = 3
	pickerBox = pickerCount + 2
	if pickerBox < sendBox {
		pickerBox = sendBox
	}
	textBox := budget - pickerBox - sendBox
	if textBox >= sendBox {
		return pickerBox, textBox - 2
	}
	textBox = sendBox
	pickerBox = budget - textBox - sendBox
	if pickerBox < sendBox {
		pickerBox = sendBox
		textBox = budget - pickerBox - sendBox
	}
	if textBox < sendBox {
		textBox = sendBox
	}
	lines := textBox - 2
	if lines < 1 {
		lines = 1
	}
	return pickerBox, lines
}

// composeDraftWidth leaves one column for the caret when the message is focused.
func composeDraftWidth(inner int, textFocus bool) int {
	if inner < 1 {
		inner = 1
	}
	if textFocus && inner > 1 {
		return inner - 1
	}
	return inner
}

// windowDraftScroll keeps the cursor line within limit visible rows,
// updating scroll only when the cursor moves past the top or bottom of
// the window. An unfocused box shows its top rows.
func windowDraftScroll(draft []screenLine, limit int, focused bool, scroll *int) ([]screenLine, int) {
	if limit < 1 {
		limit = 1
	}
	start := 0
	if len(draft) > limit {
		if !focused {
			if scroll != nil {
				*scroll = 0
			}
		} else {
			cursor := max(cursorLineIndex(draft), 0)
			if scroll != nil {
				start = *scroll
				if cursor < start {
					start = cursor
				} else if cursor >= start+limit {
					start = cursor - limit + 1
				}
				if maxStart := len(draft) - limit; start > maxStart {
					start = max(maxStart, 0)
				}
				if start < 0 {
					start = 0
				}
				*scroll = start
			} else {
				start = max(cursor-limit+1, 0)
			}
		}
		draft = draft[start : start+limit]
	} else if scroll != nil {
		*scroll = 0
	}
	for len(draft) < limit {
		draft = append(draft, screenLine{})
	}
	return draft, start
}

func (m *Model) composeStack(width int) []screenLine {
	banner := m.composeRestoreLines(width)
	banner = append(banner, m.conflictBannerLines(composerNote, width)...)
	picker, _ := m.pickerLines()
	draft := m.draftLines(width)
	send := m.sendLine(width)
	minH := m.composeMinHeight()
	for len(banner)+len(picker)+len(draft)+1 < minH {
		draft = append(draft, screenLine{})
	}
	draft = fieldRows(draft, hitNote, 0, 0, width)
	lines := make([]screenLine, 0, len(banner)+len(picker)+len(draft)+1)
	lines = append(lines, banner...)
	lines = append(lines, picker...)
	lines = append(lines, draft...)
	lines = append(lines, send)
	return lines
}

// windowLines shows limit rows of lines, keeping selected on screen.
func windowLines(lines []screenLine, selected, limit int) []screenLine {
	if limit <= 0 || len(lines) <= limit {
		return lines
	}
	start := selected - limit/2
	if start < 0 {
		start = 0
	}
	if start+limit > len(lines) {
		start = len(lines) - limit
	}
	return append([]screenLine(nil), lines[start:start+limit]...)
}

func (m *Model) composeMinHeight() int {
	if m.height <= 1 {
		return 1
	}
	half := m.height / 2
	if half < 1 {
		return 1
	}
	return half
}

func (m *Model) draftLines(width int) []screenLine {
	focused := m.composing && m.composeFocus == focusText && m.noteField.Focused()
	return fieldLines(m.note, &m.noteField, focused, width)
}

// fieldLines wraps a composer's text to width. When focused it draws the
// caret where the field's cursor is.
func fieldLines(text string, field *textarea.Model, focused bool, width int) []screenLine {
	row, col := -1, 0
	if focused {
		syncField(field, text)
		row = field.Line()
		col = field.LineInfo().CharOffset
	}
	if text == "" {
		if row >= 0 {
			return []screenLine{{text: withCursor("", col), cursor: true}}
		}
		return []screenLine{{text: ""}}
	}
	parts := strings.Split(text, "\n")
	var lines []screenLine
	for i, part := range parts {
		chunks := wrapIndexed(part, width)
		if i != row {
			for _, ch := range chunks {
				lines = append(lines, screenLine{text: ch.text})
			}
			continue
		}
		lines = append(lines, cursorChunks(chunks, col)...)
	}
	if len(lines) == 0 {
		return []screenLine{{}}
	}
	return lines
}

// cursorChunks puts a one-column cursor at display column col of the
// unwrapped line that chunks came from.
func cursorChunks(chunks []textChunk, col int) []screenLine {
	if len(chunks) == 0 {
		return []screenLine{{text: withCursor("", 0), cursor: true}}
	}
	lines := make([]screenLine, len(chunks))
	seen := 0
	placed := false
	for i, ch := range chunks {
		w := lipgloss.Width(ch.text)
		if !placed && col <= seen+w {
			lines[i] = screenLine{text: withCursor(ch.text, col-seen), cursor: true}
			placed = true
			continue
		}
		lines[i] = screenLine{text: ch.text}
		seen += w
	}
	if !placed {
		last := len(lines) - 1
		lines[last].text = withCursor(chunks[last].text, lipgloss.Width(chunks[last].text))
		lines[last].cursor = true
	}
	return lines
}

func (m *Model) sendLine(width int) screenLine {
	useStyles()
	label := m.actionButton("Send", "compose.send", m.composeFocus == focusSend, width)
	rendered := renderButton(label)
	return screenLine{
		text: rendered,
		hits: []hit{{kind: hitSend, x0: 1, x1: lipgloss.Width(rendered)}},
	}
}
