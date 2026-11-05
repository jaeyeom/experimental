package tui

import (
	"io"
	"strconv"
	"strings"
	"time"
)

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

	mouseOn   = "\x1b[?1000h\x1b[?1006h"
	mouseOff  = "\x1b[?1000l\x1b[?1006l"
	modifyOn  = "\x1b[>4;2m"
	modifyOff = "\x1b[>4;0m"
)

// hit is one clickable span on a screen row. x0 and x1 are 1-based columns.
// x1 == 0 means the whole row.
type hit struct {
	kind   int
	n      int
	x0, x1 int
}

type termEvent struct {
	key        rune
	x, y       int
	wheel      int
	mouse      bool
	press      bool
	shiftEnter bool
	shiftTab   bool
}

// Click handles one left-button press at 1-based screen coordinates.
func (m *Model) Click(x, y int) error {
	m.View()
	if m.answerFocus && !m.onAnswerRow(x, y) {
		m.answerFocus = false
		m.free.Blur()
		m.draft = m.free.Value()
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
	if y < 1 || y > len(m.rows) {
		return false
	}
	for _, h := range m.rows[y-1] {
		if h.kind != hitAnswer {
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

func parseSGR(seq string) (button, x, y int, press, ok bool) {
	body, found := strings.CutPrefix(seq, "\x1b[<")
	if !found || len(body) < 2 {
		return 0, 0, 0, false, false
	}
	last := body[len(body)-1]
	if last != 'M' && last != 'm' {
		return 0, 0, 0, false, false
	}
	parts := strings.Split(body[:len(body)-1], ";")
	if len(parts) != 3 {
		return 0, 0, 0, false, false
	}
	button, err1 := strconv.Atoi(parts[0])
	x, err2 := strconv.Atoi(parts[1])
	y, err3 := strconv.Atoi(parts[2])
	if err1 != nil || err2 != nil || err3 != nil {
		return 0, 0, 0, false, false
	}
	return button, x, y, last == 'M', true
}

func readEvents(in io.Reader, events chan<- termEvent, readErr chan<- error) {
	buf := make([]byte, 1)
	for {
		n, err := in.Read(buf)
		if n == 1 {
			if buf[0] == 0x1b {
				readEscape(in, events)
			} else {
				events <- termEvent{key: rune(buf[0])}
			}
		}
		if err != nil {
			readErr <- err
			return
		}
	}
}

func readEscape(in io.Reader, events chan<- termEvent) {
	next, ok := readByteWait(in, 50*time.Millisecond)
	if !ok || next != '[' {
		events <- termEvent{key: 0x1b}
		if ok {
			events <- termEvent{key: rune(next)}
		}
		return
	}
	third, ok := readByteWait(in, 50*time.Millisecond)
	if !ok {
		events <- termEvent{key: 0x1b}
		events <- termEvent{key: '['}
		return
	}
	if third != '<' {
		readCSI(in, third, events)
		return
	}
	seq := []byte{0x1b, '[', '<'}
	for len(seq) < 40 {
		b, ok := readByteWait(in, 50*time.Millisecond)
		if !ok {
			break
		}
		seq = append(seq, b)
		if b == 'M' || b == 'm' {
			button, x, y, press, parsed := parseSGR(string(seq))
			if parsed {
				events <- termEvent{x: x, y: y, wheel: wheelOf(button, press), mouse: true, press: press}
			}
			return
		}
	}
	events <- termEvent{key: 0x1b}
}

func readCSI(in io.Reader, first byte, events chan<- termEvent) {
	seq := []byte{first}
	if !csiFinal(first) {
		for len(seq) < 40 {
			b, ok := readByteWait(in, 50*time.Millisecond)
			if !ok {
				break
			}
			seq = append(seq, b)
			if csiFinal(b) {
				break
			}
		}
	}
	if !csiFinal(seq[len(seq)-1]) {
		emitBytes(events, seq)
		return
	}
	switch string(seq) {
	case "13;2u", "27;2;13~":
		events <- termEvent{shiftEnter: true}
	case "Z", "1;2Z", "9;2u", "27;2;9~":
		events <- termEvent{shiftTab: true}
	default:
		emitBytes(events, seq)
	}
}

func csiFinal(b byte) bool {
	return b >= 0x40 && b <= 0x7e
}

func emitBytes(events chan<- termEvent, seq []byte) {
	events <- termEvent{key: 0x1b}
	events <- termEvent{key: '['}
	for _, b := range seq {
		events <- termEvent{key: rune(b)}
	}
}

func wheelOf(button int, press bool) int {
	base := button &^ (4 | 8 | 16 | 32)
	if !press || (base != 64 && base != 65) {
		return 0
	}
	if base == 65 {
		return 1
	}
	return -1
}

func readByteWait(in io.Reader, d time.Duration) (byte, bool) {
	if dl, ok := in.(interface{ SetReadDeadline(time.Time) error }); ok {
		_ = dl.SetReadDeadline(time.Now().Add(d))
		defer func() { _ = dl.SetReadDeadline(time.Time{}) }()
	}
	var buf [1]byte
	n, err := in.Read(buf[:])
	if err != nil || n != 1 {
		return 0, false
	}
	return buf[0], true
}
