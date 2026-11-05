// Package tui is the keyboard inbox for decide.
//
// It lists one folder at a time, opens a row into the detail view, and
// reloads the ledger while it is open. Answers and done use the ledger writes.
package tui

import (
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

const (
	boldOn      = "\x1b[1m"
	boldOff     = "\x1b[0m"
	screenClear = "\x1b[H\x1b[2J"

	// DefaultInterval is how often an open inbox reloads the ledger.
	DefaultInterval = 250 * time.Millisecond
)

const (
	modeList = iota
	modeDetail
	modeOutcome
)

// Folders is the inbox order: open, answered, done, withdrawn.
var Folders = []struct {
	Name  string
	State string
}{
	{Name: "Inbox", State: decide.StateOpen},
	{Name: "Answered", State: decide.StateAnswered},
	{Name: "Done", State: decide.StateDone},
	{Name: "Withdrawn", State: decide.StateWithdrawn},
}

// Model is the inbox list and the open row's detail view. Tests drive it
// with Key and Reload.
type Model struct {
	Dir    string
	Now    func() time.Time
	folder int
	cursor int
	items  []decide.Item
	status string
	mode   int
	draft  string
	detail *decide.Item
}

// New returns an inbox positioned on the open folder.
func New(dir string) *Model {
	return &Model{Dir: dir, Now: time.Now}
}

// Reload reads the current folder from disk. The cursor stays on the same id
// when that id is still present.
func (m *Model) Reload() error {
	keep := ""
	if m.cursor >= 0 && m.cursor < len(m.items) {
		keep = m.items[m.cursor].ID
	}
	led, err := m.ledger()
	if err != nil {
		return err
	}
	all, err := led.List()
	if err != nil {
		return fmt.Errorf("list decide ledger: %w", err)
	}
	items, err := decide.Select(all, Folders[m.folder].State)
	if err != nil {
		return fmt.Errorf("select decide folder: %w", err)
	}
	m.items = items
	m.cursor = 0
	for i, it := range items {
		if it.ID == keep {
			m.cursor = i
			break
		}
	}
	return nil
}

// Key applies one key. In the list, h and l switch folders, j and k move,
// Enter opens the row, and q quits. In the detail view, 1 through 9 answer a
// relay option, free text submits on Enter when the item allows it, and e
// asks for an outcome then marks the item done.
func (m *Model) Key(r rune) (bool, error) {
	switch m.mode {
	case modeDetail:
		return m.detailKey(r)
	case modeOutcome:
		return m.outcomeKey(r)
	default:
		return m.listKey(r)
	}
}

func (m *Model) listKey(r rune) (bool, error) {
	switch r {
	case 'q':
		return true, nil
	case 'j':
		if m.cursor < len(m.items)-1 {
			m.cursor++
		}
	case 'k':
		if m.cursor > 0 {
			m.cursor--
		}
	case 'h':
		if m.folder > 0 {
			m.folder--
			m.cursor = 0
			if err := m.Reload(); err != nil {
				return false, err
			}
		}
	case 'l', '\t':
		if m.folder < len(Folders)-1 {
			m.folder++
			m.cursor = 0
			if err := m.Reload(); err != nil {
				return false, err
			}
		}
	case '\r', '\n':
		if err := m.openRow(); err != nil {
			return false, err
		}
	}
	return false, nil
}

func (m *Model) detailKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.backToList()
		return false, nil
	case 'e':
		m.mode = modeOutcome
		m.draft = ""
		m.status = "outcome"
		return false, nil
	case '\r', '\n':
		return false, m.submitFreeText()
	case '\b', '\x7f':
		if m.detail != nil && m.detail.FreeText {
			m.trimDraft()
		}
		return false, nil
	}
	if r >= '1' && r <= '9' {
		return false, m.answerOption(int(r - '1'))
	}
	if r >= ' ' {
		return false, m.typeFree(r)
	}
	return false, nil
}

func (m *Model) outcomeKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.mode = modeDetail
		m.draft = ""
		if m.detail != nil {
			m.status = "opened " + m.detail.ID
		}
		return false, nil
	case '\r', '\n':
		return false, m.submitOutcome()
	case '\b', '\x7f':
		m.trimDraft()
		return false, nil
	}
	if r >= ' ' {
		m.draft += string(r)
		m.status = "outcome"
	}
	return false, nil
}

func (m *Model) openRow() error {
	if len(m.items) == 0 {
		return nil
	}
	id := m.items[m.cursor].ID
	led, err := m.ledger()
	if err != nil {
		return err
	}
	updated, err := led.MarkRead(id)
	if err != nil {
		return fmt.Errorf("mark decide item read: %w", err)
	}
	m.detail = &updated
	m.mode = modeDetail
	m.draft = ""
	m.status = "opened " + id
	return m.Reload()
}

func (m *Model) answerOption(index int) error {
	if m.detail == nil {
		return nil
	}
	if m.detail.Kind != decide.KindRelay {
		m.status = "no answer control"
		return nil
	}
	if index < 0 || index >= len(m.detail.Options) {
		m.status = "no such option"
		return nil
	}
	return m.writeAnswer(m.detail.Options[index].Key)
}

func (m *Model) submitFreeText() error {
	if m.detail == nil || !m.detail.FreeText {
		m.status = "free text is not allowed"
		return nil
	}
	return m.writeAnswer(m.draft)
}

func (m *Model) typeFree(r rune) error {
	if m.detail == nil || !m.detail.FreeText {
		m.status = "free text is not allowed"
		return nil
	}
	m.draft += string(r)
	m.status = ""
	return nil
}

func (m *Model) writeAnswer(value string) error {
	if m.detail == nil {
		return nil
	}
	led, err := m.ledger()
	if err != nil {
		return err
	}
	id := m.detail.ID
	if _, err := led.Answer(id, value); err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "answered " + id
	m.backToList()
	return m.Reload()
}

func (m *Model) submitOutcome() error {
	if m.detail == nil {
		return nil
	}
	led, err := m.ledger()
	if err != nil {
		return err
	}
	id := m.detail.ID
	if _, err := led.Done(id, m.draft); err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "done " + id
	m.backToList()
	return m.Reload()
}

func (m *Model) ledger() (*decide.Ledger, error) {
	led, err := decide.Open(m.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide ledger: %w", err)
	}
	return led, nil
}

func (m *Model) backToList() {
	m.mode = modeList
	m.detail = nil
	m.draft = ""
}

func (m *Model) trimDraft() {
	if m.draft == "" {
		return
	}
	runes := []rune(m.draft)
	m.draft = string(runes[:len(runes)-1])
}

// View renders the folder bar and either the rows or the open detail.
// Unread rows are bold.
func (m *Model) View() string {
	var b strings.Builder
	m.writeFolders(&b)
	if m.mode == modeList {
		m.writeRows(&b)
	} else {
		m.writeDetail(&b)
	}
	if m.status != "" {
		b.WriteString(m.status)
		b.WriteByte('\n')
	}
	return b.String()
}

func (m *Model) writeFolders(b *strings.Builder) {
	for i, folder := range Folders {
		name := folder.Name
		if i == m.folder {
			name = "[" + name + "]"
		}
		if i > 0 {
			b.WriteByte(' ')
		}
		b.WriteString(name)
	}
	b.WriteByte('\n')
}

func (m *Model) writeRows(b *strings.Builder) {
	if len(m.items) == 0 {
		b.WriteString("(empty)\n")
	}
	now := time.Now()
	if m.Now != nil {
		now = m.Now()
	}
	for i, it := range m.items {
		marker := "  "
		if i == m.cursor {
			marker = "> "
		}
		line := marker + rowText(it, now)
		if it.Unread {
			line = boldOn + line + boldOff
		}
		b.WriteString(line)
		b.WriteByte('\n')
	}
}

func (m *Model) writeDetail(b *strings.Builder) {
	if m.detail == nil {
		b.WriteString("(empty)\n")
		return
	}
	it := m.detail
	b.WriteString(it.ID)
	b.WriteByte('\n')
	b.WriteString(it.Question)
	b.WriteByte('\n')
	if it.Kind == decide.KindTypeIn {
		b.WriteString("type: " + it.TypeText + "\n")
		b.WriteString("where: " + it.Where + "\n")
	} else {
		m.writeRelay(b, it)
	}
	if m.mode == modeOutcome {
		b.WriteString("outcome: " + m.draft + "\n")
	}
}

func (m *Model) writeRelay(b *strings.Builder, it *decide.Item) {
	for i, opt := range it.Options {
		if i >= 9 {
			break
		}
		mark := ""
		if opt.Recommended {
			mark = " recommended"
		}
		fmt.Fprintf(b, "%d %s %s%s\n", i+1, opt.Key, opt.Label, mark)
	}
	for _, link := range it.Links {
		b.WriteString("link: " + link + "\n")
	}
	deadline := "-"
	if it.Deadline != nil {
		deadline = it.Deadline.Format(time.RFC3339)
	}
	b.WriteString("deadline: " + deadline + "\n")
	if it.FreeText && m.mode == modeDetail {
		b.WriteString("answer: " + m.draft + "\n")
	}
}

func rowText(it decide.Item, now time.Time) string {
	deadline := "-"
	if it.Deadline != nil {
		deadline = it.Deadline.Format(time.RFC3339)
	}
	question := strings.Join(strings.Fields(it.Question), " ")
	return fmt.Sprintf("%s %s %s %s %s", it.ID, it.Owner, question, age(now.Sub(it.CreatedAt)), deadline)
}

func age(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	switch {
	case d < time.Minute:
		return "now"
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d/time.Minute))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh", int(d/time.Hour))
	default:
		return fmt.Sprintf("%dd", int(d/(24*time.Hour)))
	}
}

// Frame is one screen, cleared before the view.
func Frame(view string) string {
	return screenClear + view
}

// WriteFrame writes one screen.
func WriteFrame(w io.Writer, view string) error {
	if _, err := io.WriteString(w, Frame(view)); err != nil {
		return fmt.Errorf("draw decide inbox: %w", err)
	}
	return nil
}
