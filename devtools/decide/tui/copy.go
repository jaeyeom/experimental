package tui

import (
	"errors"
	"fmt"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// osc52MaxBytes is the OSC 52 payload size above which the inbox does
// not write that sequence. A larger copy uses the clipboard command,
// or says the text is too large.
const osc52MaxBytes = 100 * 1024

// openCopyMenu shows the item-view copy menu.
func (m *Model) openCopyMenu() {
	if m.detail == nil {
		m.status = "nothing to copy"
		return
	}
	m.copyMenu = true
	m.status = ""
}

// copyMenuKey handles the copy menu. Esc closes it. A choice copies and
// closes it.
func (m *Model) copyMenuKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.copyMenu = false
		return false, nil
	case 'm', 'q', 't', 'i', 'a':
		m.copyMenu = false
		m.copyChoice(r)
		return false, nil
	}
	if r >= ' ' {
		m.status = "unknown key, ? for help"
	}
	return false, nil
}

func (m *Model) copyMenuLines() []screenLine {
	rows := []string{
		"copy",
		"m  message",
		"q  question",
		"t  thread",
		"i  id",
		"a  answer",
	}
	lines := make([]screenLine, len(rows))
	for i, row := range rows {
		lines[i] = screenLine{text: plainText(row)}
	}
	return lines
}

// copyChoice copies the piece r names from the open item.
func (m *Model) copyChoice(r rune) {
	it := m.detail
	if it == nil {
		m.status = "nothing to copy"
		return
	}
	switch r {
	case 'm':
		m.copyStored("message", selectedMessageText(it, m.message))
	case 'q':
		m.copyStored("question", it.Question)
	case 't':
		m.copyStored("thread", m.threadCopy(it))
	case 'i':
		m.copyStored("id", it.ID)
	case 'a':
		if it.Answer == nil {
			m.status = "no answer"
			return
		}
		m.copyStored("answer", it.Answer.Value)
	}
}

// copyStored writes text and confirms it. An empty text copies nothing.
func (m *Model) copyStored(kind, text string) {
	if text == "" {
		m.status = "nothing to copy"
		return
	}
	if err := m.writeClipboard(text); err != nil {
		m.status = err.Error()
		return
	}
	m.status = fmt.Sprintf("copied %s (%d chars)", kind, utf8.RuneCountInString(text))
}

// selectedMessageText is the selected message, or the last one when the
// index is outside the thread.
func selectedMessageText(it *decide.Item, index int) string {
	if it == nil || len(it.Messages) == 0 {
		return ""
	}
	if index < 0 || index >= len(it.Messages) {
		index = len(it.Messages) - 1
	}
	return it.Messages[index].Text
}

// threadCopy is the thread as plain text: sender, local time, then the
// stored message, one block per message.
func (m *Model) threadCopy(it *decide.Item) string {
	if it == nil || len(it.Messages) == 0 {
		return ""
	}
	now := m.now()
	loc := m.zone()
	parts := make([]string, len(it.Messages))
	for i, msg := range it.Messages {
		parts[i] = messageCopyBlock(msg, now, loc)
	}
	return strings.Join(parts, "\n\n")
}

func messageCopyBlock(msg decide.Message, now time.Time, loc *time.Location) string {
	head := strings.TrimSpace(msg.Sender() + " " + stamp(msg.Time, now, loc))
	if msg.Text == "" {
		return head
	}
	if head == "" {
		return msg.Text
	}
	return head + "\n" + msg.Text
}

// writeClipboard writes text the way ui.clipboard asks. off writes nothing.
func (m *Model) writeClipboard(text string) error {
	switch m.clipboard {
	case "", decide.ClipboardAuto:
		return m.writeAutoClip(text)
	case decide.ClipboardOSC52:
		return m.writeOSCClip(text)
	case decide.ClipboardCommand:
		return m.runClip(text)
	case decide.ClipboardOff:
		return errors.New("copy is disabled")
	default:
		return fmt.Errorf("ui.clipboard %q must be auto, osc52, command, or off", m.clipboard)
	}
}

// writeAutoClip writes OSC 52 unless the text is over the size limit,
// and also runs the clipboard command.
func (m *Model) writeAutoClip(text string) error {
	wrote := false
	if len(text) <= osc52MaxBytes {
		wrote = m.writeOSC52(text)
	}
	if err := m.runClip(text); err != nil && !wrote {
		if len(text) > osc52MaxBytes {
			return errors.New("text is too large")
		}
		return err
	}
	return nil
}

func (m *Model) writeOSCClip(text string) error {
	if len(text) > osc52MaxBytes {
		return errors.New("text is too large")
	}
	if !m.writeOSC52(text) {
		return errors.New("could not write OSC 52")
	}
	return nil
}

func (m *Model) runClip(text string) error {
	clip := m.copyClip
	if clip == nil {
		clip = copyClipboard
	}
	return clip(text)
}

// togglePlain switches the borderless, unwrapped item view.
func (m *Model) togglePlain() {
	m.plain = !m.plain
	m.scroll = 0
}

// plainScreen is the open item with no border, padding, or soft wrap.
// A line longer than the window runs past the edge.
func (m *Model) plainScreen() []screenLine {
	lines := m.plainItemLines()
	limit := 0
	if m.height > 1 {
		limit = m.height - 1
	}
	var scroll int
	lines, scroll = clipLines(lines, m.scroll, limit)
	m.scroll = scroll
	return lines
}

func (m *Model) plainItemLines() []screenLine {
	it := m.detail
	if it == nil {
		return []screenLine{{text: "(empty)"}}
	}
	if len(it.Messages) == 0 {
		return splitPlain(it.Question)
	}
	now := m.now()
	loc := m.zone()
	var lines []screenLine
	for i, msg := range it.Messages {
		if i > 0 {
			lines = append(lines, screenLine{})
		}
		lines = append(lines, splitPlain(messageCopyBlock(msg, now, loc))...)
	}
	return lines
}

func splitPlain(text string) []screenLine {
	if text == "" {
		return nil
	}
	parts := strings.Split(text, "\n")
	lines := make([]screenLine, len(parts))
	for i, part := range parts {
		lines[i] = screenLine{text: part}
	}
	return lines
}
