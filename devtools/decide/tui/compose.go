package tui

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

const sendsName = "sends.jsonl"

// defaultSendTemplate is the compose command when config sets none.
// {pane} is one argument. The note is appended as its own argument
// and is not interpolated into sh -c.
const defaultSendTemplate = "herdr agent prompt {pane}"

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
	case '\b', '\x7f':
		if m.composeFocus == focusText {
			m.editNote(r)
		}
		return false, nil
	}
	if m.composeFocus == focusRecipient && (r == 'j' || r == 'k') {
		delta := 1
		if r == 'k' {
			delta = -1
		}
		m.moveTarget(delta)
		return false, nil
	}
	if m.composeFocus == focusText && r >= ' ' {
		m.editNote(r)
	}
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
	argv := sendArgv(m.SendTemplate, tg.Pane, m.note)
	send := m.runSend
	if send == nil {
		send = defaultRunSend
	}
	if err := send(argv); err != nil {
		m.status = err.Error()
		return nil
	}
	if err := m.appendSend(tg.Name, tg.Pane, m.note); err != nil {
		m.status = err.Error()
		return nil
	}
	m.clearNote()
	m.status = "sent to " + tg.Name
	return nil
}

// sendArgv builds the compose command. An empty template is
// `herdr agent prompt {pane}` plus the note as its own argument.
// {pane} is replaced in the template arguments. The note is not
// passed through a shell.
func sendArgv(template, pane, text string) []string {
	if strings.TrimSpace(template) == "" {
		template = defaultSendTemplate
	}
	parts := strings.Fields(template)
	for i, part := range parts {
		parts[i] = strings.ReplaceAll(part, "{pane}", pane)
	}
	return append(parts, text)
}

func defaultRunSend(argv []string) error {
	if len(argv) == 0 {
		return fmt.Errorf("decide send: empty command")
	}
	// The note is already its own argument. Operator config chooses the program.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(argv[0], argv[1:]...) //nolint:gosec // G204: the note is its own argument, not a shell string
	cmd.Stdout = io.Discard
	cmd.Stderr = io.Discard
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("decide send: %w", err)
	}
	return nil
}

func (m *Model) appendSend(name, pane, text string) error {
	if err := os.MkdirAll(m.Dir, 0o755); err != nil {
		return fmt.Errorf("decide send history: %w", err)
	}
	//nolint:gosec // G304: history stays under the ledger directory
	f, err := os.OpenFile(filepath.Join(m.Dir, sendsName), os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return fmt.Errorf("decide send history: %w", err)
	}
	enc := json.NewEncoder(f)
	enc.SetEscapeHTML(false)
	werr := enc.Encode(sendRecord{Target: name, Pane: pane, Text: text})
	cerr := f.Close()
	if werr != nil {
		return fmt.Errorf("decide send history: %w", werr)
	}
	if cerr != nil {
		return fmt.Errorf("decide send history: %w", cerr)
	}
	return nil
}

func (m *Model) composeBody() []screenLine {
	picker := m.pickerLines()
	draft := m.draftLines()
	send := m.sendLine()
	minH := m.composeMinHeight()
	for len(picker)+len(draft)+1 < minH {
		draft = append(draft, screenLine{})
	}
	lines := make([]screenLine, 0, len(picker)+len(draft)+1)
	lines = append(lines, picker...)
	lines = append(lines, draft...)
	lines = append(lines, send)
	return lines
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

func (m *Model) draftLines() []screenLine {
	text := m.note
	if text == "" {
		return []screenLine{{}}
	}
	parts := strings.Split(text, "\n")
	lines := make([]screenLine, len(parts))
	for i, part := range parts {
		lines[i] = screenLine{text: part}
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
