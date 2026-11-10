package tui

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

const sendsName = decide.SendsName

// Target is a named pane the compose box can send to.
type Target struct {
	Name string
	Pane string
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
	switch r {
	case '\x1b':
		m.blurCompose()
		return false, nil
	case '\t':
		m.advanceCompose()
		return false, nil
	case '\r', '\n':
		return m.composeEnter()
	}
	if m.composeFocus == focusRecipient && (r == 'j' || r == 'k') {
		delta := 1
		if r == 'k' {
			delta = -1
		}
		m.moveTarget(delta)
		return false, nil
	}
	m.typeRune(r)
	return false, nil
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

func (m *Model) moveTarget(delta int) {
	n := len(m.Targets)
	if n == 0 {
		return
	}
	next := m.target + delta
	if next < 0 || next >= n {
		return
	}
	m.target = next
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
	if m.target < 0 || m.target >= len(m.Targets) {
		m.status = "no compose target"
		return nil
	}
	tg := m.Targets[m.target]
	text := m.note
	// The thread id has to exist before the pane note is built.
	item, err := m.recordThread(tg, text)
	if err != nil {
		m.status = err.Error()
		return nil
	}
	note := decide.DeliveryNote(item.ID, "human", text)
	if m.forward != "" {
		note = decide.ForwardNote(item.ID, m.forward, "human", text)
	}
	argv := decide.SendArgv(m.SendTemplate, tg.Pane, note)
	send := m.runSend
	if send == nil {
		send = decide.RunSend
	}
	if err := send(argv); err != nil {
		m.status = err.Error()
		return nil
	}
	if err := decide.AppendSend(m.Dir, tg.Name, tg.Pane, text); err != nil {
		m.status = err.Error()
		return nil
	}
	m.clearNote()
	m.status = "sent to " + tg.Name
	return nil
}

func (m *Model) recordThread(tg Target, text string) (decide.Item, error) {
	led, err := decide.Open(m.Dir)
	if err != nil {
		return decide.Item{}, fmt.Errorf("open decide ledger: %w", err)
	}
	item, err := led.ForwardThread(m.forward, tg.Name, tg.Pane, text)
	if err != nil {
		return decide.Item{}, fmt.Errorf("store decide thread: %w", err)
	}
	return item, nil
}

// forwardItem opens compose on the viewed item with its context quoted.
// The cursor waits on the empty line after the quote.
func (m *Model) forwardItem() {
	if m.detail == nil {
		return
	}
	m.focusCompose()
	m.forward = m.detail.ID
	m.note = forwardPrefill(m.detail, m.message)
	m.composeFocus = focusText
	m.pullNote()
	m.noteField.Focus()
}

// forwardQuoteLimit caps the quoted message so the draft stays editable.
const forwardQuoteLimit = 2000

// forwardPrefill quotes the question, message msg (or the latest when msg
// is out of range), and the first link of it.
func forwardPrefill(it *decide.Item, msg int) string {
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
		quote(&b, strings.TrimSpace(m.Author+" "+headerTime(m.Time)))
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
	budget := m.height - 3
	if budget < 3 {
		budget = 3
	}
	pickerLines := m.pickerLines()
	pickerBox, textLines := composeBoxHeights(len(pickerLines), budget)
	picker := windowLines(pickerLines, m.target, pickerBox-2)
	wrap := composeDraftWidth(width-2, m.composeFocus == focusText)
	draft, first := windowDraft(m.draftLines(wrap), textLines)
	draft = fieldRows(draft, hitNote, first, 0, wrap)
	return stackRegions(width,
		panelBox(picker, width, m.composeFocus == focusRecipient),
		panelBox(draft, width, m.composeFocus == focusText),
		panelBox([]screenLine{m.sendLine()}, width, m.composeFocus == focusSend),
	)
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

// windowDraft keeps the caret line on screen and pads the message to limit
// rows. It also returns the index of the first row shown.
func windowDraft(draft []screenLine, limit int) ([]screenLine, int) {
	if limit < 1 {
		limit = 1
	}
	start := 0
	if len(draft) > limit {
		cursor := 0
		for i, line := range draft {
			if strings.Contains(line.text, cursorOn) {
				cursor = i
				break
			}
		}
		start = max(cursor-limit+1, 0)
		draft = draft[start : start+limit]
	}
	for len(draft) < limit {
		draft = append(draft, screenLine{})
	}
	return draft, start
}

func (m *Model) composeStack(width int) []screenLine {
	picker := m.pickerLines()
	draft := m.draftLines(width)
	send := m.sendLine()
	minH := m.composeMinHeight()
	for len(picker)+len(draft)+1 < minH {
		draft = append(draft, screenLine{})
	}
	draft = fieldRows(draft, hitNote, 0, 0, width)
	lines := make([]screenLine, 0, len(picker)+len(draft)+1)
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

func (m *Model) pickerLines() []screenLine {
	if len(m.Targets) == 0 {
		return []screenLine{{text: "(no recipients)"}}
	}
	if m.target < 0 || m.target >= len(m.Targets) {
		m.target = 0
	}
	lines := make([]screenLine, 0, len(m.Targets))
	for i, tg := range m.Targets {
		label := tg.Name
		if i == m.target {
			label = "[" + label + "]"
		}
		if m.composeFocus == focusRecipient && i == m.target {
			label = "> " + label
		}
		lines = append(lines, screenLine{
			text: label,
			hits: []hit{{kind: hitTarget, n: i}},
		})
	}
	return lines
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
		line := ""
		if row >= 0 {
			line = withCursor("", col)
		}
		return []screenLine{{text: line}}
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
		return []screenLine{{text: withCursor("", 0)}}
	}
	lines := make([]screenLine, len(chunks))
	seen := 0
	placed := false
	for i, ch := range chunks {
		w := lipgloss.Width(ch.text)
		if !placed && col <= seen+w {
			lines[i] = screenLine{text: withCursor(ch.text, col-seen)}
			placed = true
			continue
		}
		lines[i] = screenLine{text: ch.text}
		seen += w
	}
	if !placed {
		last := len(lines) - 1
		lines[last].text = withCursor(chunks[last].text, lipgloss.Width(chunks[last].text))
	}
	return lines
}

func (m *Model) sendLine() screenLine {
	useStyles()
	label := "[Send]"
	if m.composeFocus == focusSend {
		label = "> [Send]"
	}
	return screenLine{text: optionButton.Render(label), hits: []hit{{kind: hitSend}}}
}
