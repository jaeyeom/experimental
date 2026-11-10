package tui

const (
	hitNone = iota
	hitFolder
	hitRow
	hitOption
	hitAnswer
	hitSubmit
	hitMarkDone
	hitTarget
	hitCompose
	hitSend
	hitLink
	hitReply
	hitOutcome
	hitNote
)

// hit is one clickable span on a screen row. x0 and x1 are 1-based columns.
// x1 == 0 means the whole row.
//
// A composer row (hitReply, hitOutcome, hitNote) takes the whole row. Its
// n is the visual row of the wrapped text, x0 is where the row starts, pad
// is the label width before the text, and wrap is the wrap width.
type hit struct {
	kind      int
	n         int
	x0, x1    int
	url       string
	pad, wrap int
}

// Click handles one left-button press at 1-based screen coordinates.
func (m *Model) Click(x, y int) error {
	m.View()
	if m.answerFocus && !m.onAnswerRow(x, y) {
		m.answerFocus = false
		m.free.Blur()
		return nil
	}
	field, onField := m.fieldRow(y)
	if m.replyFocus && field.kind != hitReply {
		m.replyFocus = false
		if !onField {
			return nil
		}
	}
	if onField {
		m.clickField(field, x)
		return nil
	}
	if y < 1 || y > len(m.rows) {
		return nil
	}
	for _, h := range m.rows[y-1] {
		if h.x1 != 0 && (x < h.x0 || x > h.x1) {
			continue
		}
		return m.hit(h)
	}
	return nil
}

// fieldRow is the composer hit on screen row y, if any.
func (m *Model) fieldRow(y int) (hit, bool) {
	if y < 1 || y > len(m.rows) {
		return hit{}, false
	}
	for _, h := range m.rows[y-1] {
		if fieldComposer(h.kind) != composerNone {
			return h, true
		}
	}
	return hit{}, false
}

func fieldComposer(kind int) composerKind {
	switch kind {
	case hitReply:
		return composerReply
	case hitOutcome:
		return composerOutcome
	case hitNote:
		return composerNote
	default:
		return composerNone
	}
}

// clickField focuses the composer under h and puts its cursor at screen
// column x. The draft takes the field's value first, so the next sync
// does not reload the field and move the cursor to the end.
func (m *Model) clickField(h hit, x int) {
	switch h.kind {
	case hitReply:
		m.focusReply()
	case hitNote:
		m.answerFocus = false
		m.composeFocus = focusText
	}
	field, text := m.fieldFor(fieldComposer(h.kind))
	syncField(field, *text)
	*text = field.Value()
	line, col := clickCursor(*text, h.wrap, h.n, x-h.x0-h.pad)
	setFieldCursor(field, line, col)
	field.Focus()
}

// fieldRows marks lines as rows of a composer of hit kind. lines[0] shows
// visual row first of the wrapped text.
func fieldRows(lines []screenLine, kind, first, pad, wrap int) []screenLine {
	for i := range lines {
		lines[i].hits = []hit{{kind: kind, n: first + i, x0: 1, x1: 1, pad: pad, wrap: wrap}}
	}
	return lines
}

func (m *Model) onAnswerRow(x, y int) bool {
	return m.onHitRow(x, y, hitAnswer)
}

func (m *Model) onHitRow(x, y, kind int) bool {
	if y < 1 || y > len(m.rows) {
		return false
	}
	for _, h := range m.rows[y-1] {
		if h.kind != kind {
			continue
		}
		if h.x1 == 0 || (x >= h.x0 && x <= h.x1) {
			return true
		}
	}
	return false
}

// Wheel scrolls the list or the open item by one line.
func (m *Model) Wheel(down bool) {
	if m.help {
		return
	}
	if down {
		m.scroll++
		return
	}
	if m.scroll > 0 {
		m.scroll--
	}
}

// scrollPage moves by one viewport of the open item. clipLines stops the
// result at the top and the bottom.
func (m *Model) scrollPage(down bool) {
	if m.help {
		return
	}
	n := m.threadPage()
	if down {
		m.scroll += n
		return
	}
	if m.scroll > n {
		m.scroll -= n
		return
	}
	m.scroll = 0
}

func (m *Model) threadPage() int {
	_, n := m.layoutBudget()
	if n < 1 {
		return 1
	}
	return n
}

func (m *Model) scrollHome() {
	if m.help {
		return
	}
	m.scroll = 0
}

func (m *Model) scrollEnd() {
	if m.help {
		return
	}
	// clipLines clamps this onto the last viewport.
	m.scroll = 1 << 20
}

func (m *Model) hit(h hit) error {
	if m.hitCompose(h) {
		return nil
	}
	m.blurCompose()
	switch h.kind {
	case hitFolder:
		return m.showFolder(h.n)
	case hitRow:
		if h.n < 0 || h.n >= len(m.items) {
			return nil
		}
		m.cursor = h.n
		return m.openRow()
	case hitOption:
		return m.answerOption(h.n)
	case hitAnswer:
		m.focusAnswer()
		return nil
	case hitSubmit:
		return m.submitFreeText()
	case hitMarkDone:
		m.scroll = 0
		m.beginOutcome()
		return nil
	case hitSend:
		return m.sendNote()
	case hitLink:
		return m.openURL(h.url)
	default:
		return nil
	}
}

func (m *Model) hitCompose(h hit) bool {
	switch h.kind {
	case hitTarget:
		m.selectTarget(h.n)
		return true
	case hitCompose:
		m.focusCompose()
		return true
	default:
		return false
	}
}

func (m *Model) showFolder(n int) error {
	if n == len(Folders) {
		return m.showStatus()
	}
	if n < 0 || n >= len(Folders) {
		return nil
	}
	if m.mode != modeList {
		m.backToList()
	}
	m.scroll = 0
	if n == m.folder {
		return nil
	}
	m.folder = n
	m.cursor = 0
	return m.Reload()
}
