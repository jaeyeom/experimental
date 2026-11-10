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
)

// hit is one clickable span on a screen row. x0 and x1 are 1-based columns.
// x1 == 0 means the whole row.
type hit struct {
	kind   int
	n      int
	x0, x1 int
	url    string
}

// Click handles one left-button press at 1-based screen coordinates.
func (m *Model) Click(x, y int) error {
	m.View()
	if m.answerFocus && !m.onAnswerRow(x, y) {
		m.answerFocus = false
		m.free.Blur()
		return nil
	}
	if m.replyFocus && !m.onHitRow(x, y, hitReply) {
		m.replyFocus = false
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
	case hitReply:
		m.focusReply()
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
		if h.n >= 0 && h.n < len(m.Targets) {
			m.target = h.n
		}
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
