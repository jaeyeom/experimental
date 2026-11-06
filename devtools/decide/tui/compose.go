package tui

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"unicode/utf8"
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

// ShiftEnter inserts a newline into a focused compose box and does not send.
func (m *Model) ShiftEnter() {
	if !m.composing {
		return
	}
	m.insertNoteNewline()
}

// ShiftTab selects the previous compose target in the list and while the
// box is focused. It does not insert a character.
func (m *Model) ShiftTab() {
	if m.help || m.answerFocus {
		return
	}
	if m.composing || m.mode == modeList {
		m.cycleTarget(-1)
	}
}

func (m *Model) composeKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.blurCompose()
		return false, nil
	case '\r', '\n':
		return false, m.sendNote()
	case '\b', '\x7f':
		m.editNote(r)
		return false, nil
	}
	if r >= ' ' {
		m.editNote(r)
	}
	return false, nil
}

func (m *Model) cycleTarget(delta int) {
	n := len(m.Targets)
	if n == 0 {
		return
	}
	m.target = (m.target + delta) % n
	if m.target < 0 {
		m.target += n
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

func (m *Model) composeLines() []screenLine {
	lines := m.targetLines()
	lines = append(lines, m.composeLine(), m.sendLine())
	return lines
}

// targetLines is the compose dropdown. The open row is the selected target.
// The other targets are the entries under it.
func (m *Model) targetLines() []screenLine {
	if len(m.Targets) == 0 {
		return []screenLine{{text: "to: ▾ (none)"}}
	}
	if m.target < 0 || m.target >= len(m.Targets) {
		m.target = 0
	}
	lines := make([]screenLine, 0, len(m.Targets))
	selected := m.Targets[m.target].Name
	prefix := "to: ▾ "
	lines = append(lines, screenLine{
		text: prefix + selected,
		hits: []hit{{
			kind: hitTarget,
			n:    m.target,
			x0:   utf8.RuneCountInString(prefix) + 1,
			x1:   utf8.RuneCountInString(prefix + selected),
		}},
	})
	for i, tg := range m.Targets {
		if i == m.target {
			continue
		}
		pad := "  "
		lines = append(lines, screenLine{
			text: pad + tg.Name,
			hits: []hit{{
				kind: hitTarget,
				n:    i,
				x0:   utf8.RuneCountInString(pad) + 1,
				x1:   utf8.RuneCountInString(pad + tg.Name),
			}},
		})
	}
	return lines
}

func (m *Model) sendLine() screenLine {
	useStyles()
	return screenLine{text: optionButton.Render("Send"), hits: []hit{{kind: hitSend}}}
}

func (m *Model) composeLine() screenLine {
	if !m.composing {
		return screenLine{
			text: "compose:",
			hits: []hit{{kind: hitCompose}},
		}
	}
	body := strings.ReplaceAll(m.note, "\n", `\n`)
	if body == "" {
		body = "…"
	}
	return screenLine{
		text: "compose › " + body + "_  Enter send · Esc cancel",
		hits: []hit{{kind: hitCompose}},
	}
}
