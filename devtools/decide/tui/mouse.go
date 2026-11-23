package tui

const (
	hitNone = iota
	hitFolder
	hitRow
	hitOption
	hitAnswer
	hitSubmit
	hitMarkDone
	hitClose
	hitTarget
	hitCompose
	hitSend
	hitLink
	hitReply
	hitOutcome
	hitNote
	hitAvatar
	hitReplySend
	hitReplySendAndArchive
	hitReplyDiscard
	hitReplyFocus
	hitComposeRestore
	hitComposeDiscard
	hitConflictKeep
	hitConflictUse
	hitConflictMerge
	hitTitle
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
	if m.avatarPick {
		return m.clickAvatar(x, y)
	}
	if m.answerFocus && !m.onAnswerRow(x, y) {
		m.answerFocus = false
		m.free.Blur()
		return nil
	}
	field, onField := m.fieldRow(y)
	if m.leaveReply(field, onField, x, y) {
		return nil
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

// leaveReply unfocuses the reply box and its buttons for a click
// elsewhere. It reports true when the click only left the reply box.
func (m *Model) leaveReply(field hit, onField bool, x, y int) bool {
	onReplyBtn := m.onHitRow(x, y, hitReplySend) || m.onHitRow(x, y, hitReplySendAndArchive) || m.onHitRow(x, y, hitReplyDiscard) || m.onHitRow(x, y, hitReplyFocus)
	if !onReplyBtn {
		m.replySend = false
		m.replyBtn = replyBtnSend
	}
	if !m.replyFocus || field.kind == hitReply {
		return false
	}
	m.replyFocus = false
	return !onField && !onReplyBtn
}

// CtrlClick opens the link under 1-based screen coordinates. Off a link
// it does nothing. Each piece of a wrapped link keeps its whole URL.
func (m *Model) CtrlClick(x, y int) {
	m.View()
	if y < 1 || y > len(m.rows) {
		return
	}
	for _, h := range m.rows[y-1] {
		if h.kind == hitLink && x >= h.x0 && x <= h.x1 {
			m.openURL(h.url)
			return
		}
	}
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
	n, _ := m.detailLayout(m.contentWidth())
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
		return m.openNavFolder(h.n)
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
	case hitClose:
		return m.closeSelected()
	case hitSend:
		return m.sendNote()
	case hitLink:
		m.openURL(h.url)
		return nil
	default:
		if handled, err := m.hitReplyButton(h.kind); handled {
			return err
		}
		return nil
	}
}

func (m *Model) hitReplyButton(kind int) (bool, error) {
	switch kind {
	case hitTitle:
		_, err := m.runDetailTop()
		return true, err
	case hitReplySend:
		return true, m.submitReply()
	case hitReplySendAndArchive:
		return true, m.submitReplyAndArchive()
	case hitReplyDiscard:
		m.discardReply()
		return true, nil
	case hitReplyFocus:
		m.focusReply()
		return true, nil
	default:
		return false, nil
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
	case hitComposeRestore:
		m.restoreComposeDraft()
		return true
	case hitComposeDiscard:
		m.discardComposeDraft()
		return true
	case hitConflictKeep:
		m.resolveConflict(composerKind(h.n), conflictKeepCurrent)
		return true
	case hitConflictUse:
		m.resolveConflict(composerKind(h.n), conflictUseOther)
		return true
	case hitConflictMerge:
		m.resolveConflict(composerKind(h.n), conflictMergeBoth)
		return true
	default:
		return false
	}
}

func (m *Model) showFolder(n int) error {
	switch n {
	case len(Folders):
		return m.showStatus()
	case len(Folders) + 1:
		return m.showContacts()
	}
	if n < 0 || n >= len(Folders) {
		return nil
	}
	wasList := m.mode == modeList
	if !wasList {
		m.backToList()
	}
	m.scroll = 0
	if n == m.folder {
		if !wasList {
			return m.reloadLoaded()
		}
		return nil
	}
	m.folder = n
	m.cursor = 0
	return m.reloadLoaded()
}

// reloadLoaded reloads once the first list has arrived. Until then the
// background first list shows the folder.
func (m *Model) reloadLoaded() error {
	if m.loading {
		return nil
	}
	return m.Reload()
}
