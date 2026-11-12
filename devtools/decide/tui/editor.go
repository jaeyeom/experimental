package tui

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"

	"github.com/charmbracelet/bubbles/textarea"
	tea "github.com/charmbracelet/bubbletea"
)

type composerKind int

const (
	composerNone composerKind = iota
	composerReply
	composerAnswer
	composerOutcome
	composerNote
)

// editorDoneMsg is the result of an external edit after the terminal resumes.
type editorDoneMsg struct {
	err error
}

// externalEdit is one editor process. tea.Exec suspends the terminal around Run.
type externalEdit struct {
	m        *Model
	prog     string
	args     []string
	path     string
	original string
	kind     composerKind
	stdin    io.Reader
	stdout   io.Writer
	stderr   io.Writer
}

// Run calls the suspend hook, the editor, and the resume hook.
func (e *externalEdit) Run() error {
	if e.m.suspend != nil {
		e.m.suspend()
	}
	if e.m.resume != nil {
		defer e.m.resume()
	}
	// $VISUAL, $EDITOR, or nano. The operator chooses the program. Arguments are not a shell string.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(e.prog, e.args...) //nolint:gosec // G204: operator editor, arguments are not a shell string
	cmd.Stdin = e.stdin
	cmd.Stdout = e.stdout
	cmd.Stderr = e.stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("run editor: %w", err)
	}
	return nil
}

func (e *externalEdit) SetStdin(r io.Reader)  { e.stdin = r }
func (e *externalEdit) SetStdout(w io.Writer) { e.stdout = w }
func (e *externalEdit) SetStderr(w io.Writer) { e.stderr = w }

func (m *Model) composerFocused() bool {
	_, ok := m.composerKind()
	return ok
}

func (m *Model) composerKind() (composerKind, bool) {
	switch {
	case m.help || m.linkPick || m.avatarPick:
		return composerNone, false
	case m.replyFocus:
		return composerReply, true
	case m.answerFocus:
		return composerAnswer, true
	case m.mode == modeOutcome:
		return composerOutcome, true
	case m.composing && m.composeFocus == focusText:
		return composerNote, true
	default:
		return composerNone, false
	}
}

// fieldFor is the field behind a composer and the draft string it edits.
func (m *Model) fieldFor(kind composerKind) (*textarea.Model, *string) {
	switch kind {
	case composerReply:
		return &m.replyField, &m.reply
	case composerAnswer:
		return &m.free, &m.draft
	case composerOutcome:
		return &m.outcome, &m.draft
	case composerNote:
		return &m.noteField, &m.note
	default:
		return nil, nil
	}
}

// composerField is the focused composer's field and draft, or nil.
func (m *Model) composerField() (*textarea.Model, *string) {
	kind, _ := m.composerKind()
	return m.fieldFor(kind)
}

// editComposerKey applies k to the focused composer. It reports false
// when no composer is focused.
func (m *Model) editComposerKey(k tea.KeyMsg) bool {
	field, text := m.composerField()
	if field == nil {
		return false
	}
	syncField(field, *text)
	field.Focus()
	editField(field, k)
	*text = field.Value()
	return true
}

func (m *Model) composerText(kind composerKind) string {
	field, text := m.fieldFor(kind)
	if field == nil {
		return ""
	}
	syncField(field, *text)
	return *text
}

func (m *Model) setComposerText(kind composerKind, text string) {
	field, draft := m.fieldFor(kind)
	if field == nil {
		return
	}
	*draft = text
	field.SetValue(text)
	field.Focus()
}

// handleEditorKey starts an external edit from a focused composer.
// Ctrl+X then Ctrl+E, or Ctrl+G alone, opens the editor. Any other key
// after Ctrl+X is dropped and is not inserted.
func (m *Model) handleEditorKey(k tea.KeyMsg) (tea.Cmd, bool) {
	if m.editPrefix {
		m.editPrefix = false
		if k.Type == tea.KeyCtrlE && m.composerFocused() {
			return m.startExternalEdit(), true
		}
		return nil, true
	}
	if !m.composerFocused() {
		return nil, false
	}
	switch k.Type {
	case tea.KeyCtrlX:
		m.editPrefix = true
		return nil, true
	case tea.KeyCtrlG:
		return m.startExternalEdit(), true
	default:
		return nil, false
	}
}

func (m *Model) startExternalEdit() tea.Cmd {
	kind, ok := m.composerKind()
	if !ok {
		return nil
	}
	original := m.composerText(kind)
	path, err := writeEditorFile(original)
	if err != nil {
		m.status = "editor failed"
		return nil
	}
	prog, args, err := editorCommand()
	if err != nil {
		_ = os.Remove(path)
		m.status = "editor not found"
		return nil
	}
	edit := &externalEdit{
		m:        m,
		prog:     prog,
		args:     append(append([]string{}, args...), path),
		path:     path,
		original: original,
		kind:     kind,
	}
	m.editor = edit
	return tea.Exec(edit, func(err error) tea.Msg {
		return editorDoneMsg{err: err}
	})
}

func (m *Model) finishExternalEdit(err error) {
	edit := m.editor
	m.editor = nil
	if edit == nil {
		return
	}
	defer os.Remove(edit.path)
	if err != nil {
		return
	}
	body, readErr := os.ReadFile(edit.path) //nolint:gosec // G304: temp file this process created
	if readErr != nil || string(body) == edit.original {
		return
	}
	m.setComposerText(edit.kind, string(body))
}

// editorCommand is $VISUAL, otherwise $EDITOR, otherwise nano.
// The value is split into a program and arguments. It is not a shell command.
func editorCommand() (string, []string, error) {
	line := strings.TrimSpace(os.Getenv("VISUAL"))
	if line == "" {
		line = strings.TrimSpace(os.Getenv("EDITOR"))
	}
	fields := strings.Fields(line)
	if len(fields) == 0 {
		fields = []string{"nano"}
	}
	prog, err := exec.LookPath(fields[0])
	if err != nil {
		return "", nil, fmt.Errorf("find editor: %w", err)
	}
	return prog, fields[1:], nil
}

func writeEditorFile(text string) (string, error) {
	f, err := os.CreateTemp("", "decide-*.md")
	if err != nil {
		return "", fmt.Errorf("create editor file: %w", err)
	}
	path := f.Name()
	_, err = f.WriteString(text)
	cerr := f.Close()
	if err == nil {
		err = cerr
	}
	if err != nil {
		_ = os.Remove(path)
		return "", fmt.Errorf("write editor file: %w", err)
	}
	return path, nil
}
