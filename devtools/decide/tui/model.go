// Package tui is the keyboard inbox for decide.
//
// It lists one folder at a time, opens a row into the detail view, and
// reloads the ledger while it is open. Answers and done use the ledger writes.
// The compose box sends a note to a configured pane. No pane id is built in.
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
	Dir          string
	Now          func() time.Time
	Targets      []Target
	SendTemplate string
	folder       int
	cursor       int
	items        []decide.Item
	status       string
	mode         int
	draft        string
	detail       *decide.Item
	height       int
	scroll       int
	rows         [][]hit
	target       int
	composing    bool
	help         bool
	note         string
	runSend      func([]string) error
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
// Enter opens the row, ] changes the compose target, c focuses the compose
// box, and q quits. In the detail view, 1 through 9 answer a relay option,
// free text submits on Enter when the item allows it, and e asks for an
// outcome then marks the item done. While the compose box is focused, Enter
// sends the note and other printable keys type into it. ? opens the shortcut
// overlay, and Esc or ? closes it.
func (m *Model) Key(r rune) (bool, error) {
	if m.help {
		return m.helpKey(r)
	}
	if r == '?' {
		m.help = true
		return false, nil
	}
	if m.composing {
		return m.composeKey(r)
	}
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
	if m.listComposeKey(r) {
		return false, nil
	}
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
			m.scroll = 0
			if err := m.Reload(); err != nil {
				return false, err
			}
		}
	case 'l', '\t':
		if m.folder < len(Folders)-1 {
			m.folder++
			m.cursor = 0
			m.scroll = 0
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

func (m *Model) helpKey(r rune) (bool, error) {
	switch r {
	case '\x1b', '?':
		m.help = false
	case 'q':
		return true, nil
	}
	return false, nil
}

func (m *Model) listComposeKey(r rune) bool {
	switch r {
	case 'c':
		m.composing = true
		return true
	case ']':
		m.cycleTarget(1)
		return true
	default:
		return false
	}
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
	m.scroll = 0
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
	m.scroll = 0
}

func (m *Model) trimDraft() {
	if m.draft == "" {
		return
	}
	runes := []rune(m.draft)
	m.draft = string(runes[:len(runes)-1])
}

type screenLine struct {
	text string
	hits []hit
}

// View renders the folder bar and either the rows or the open detail.
// Unread rows are bold. A positive height scrolls the body to that many rows.
func (m *Model) View() string {
	lines := m.window(m.allLines())
	m.rows = make([][]hit, len(lines))
	var b strings.Builder
	for i, line := range lines {
		b.WriteString(line.text)
		b.WriteByte('\n')
		m.rows[i] = line.hits
	}
	return b.String()
}

func (m *Model) allLines() []screenLine {
	var lines []screenLine
	if m.help {
		lines = m.helpLines()
	} else {
		lines = []screenLine{m.folderLine()}
		if m.mode == modeList {
			lines = append(lines, m.rowLines()...)
		} else {
			lines = append(lines, m.detailLines()...)
		}
		lines = append(lines, m.composeLines()...)
	}
	lines = append(lines, screenLine{text: "? help"})
	if m.status != "" {
		lines = append(lines, screenLine{text: m.status})
	}
	return lines
}

func (m *Model) helpLines() []screenLine {
	text := []string{
		"shortcuts",
		"q quit",
		"j/k move    h/l folder    enter open",
		"c compose   ] target      tab next target",
		"enter send  shift+enter newline",
		"1-9 answer  e done        esc back",
		"click folder, row, option, target, or compose",
		"wheel scroll",
		"? or esc close",
	}
	lines := make([]screenLine, len(text))
	for i, line := range text {
		lines[i] = screenLine{text: line}
	}
	return lines
}

func (m *Model) window(lines []screenLine) []screenLine {
	if m.height <= 1 || len(lines) <= m.height {
		if m.height <= 1 {
			return lines
		}
		m.scroll = 0
		return lines
	}
	head := lines[:1]
	rest := lines[1:]
	tailN := m.pinnedTail()
	if tailN > len(rest) {
		tailN = len(rest)
	}
	tail := rest[len(rest)-tailN:]
	rest = rest[:len(rest)-tailN]
	limit := m.height - 1 - len(tail)
	if limit < 1 {
		limit = 1
	}
	if m.scroll > len(rest)-limit {
		m.scroll = len(rest) - limit
	}
	if m.scroll < 0 {
		m.scroll = 0
	}
	end := m.scroll + limit
	if end > len(rest) {
		end = len(rest)
	}
	out := append(append([]screenLine{}, head...), rest[m.scroll:end]...)
	return append(out, tail...)
}

func (m *Model) pinnedTail() int {
	n := 1
	if !m.help {
		n += 2
	}
	if m.status != "" {
		n++
	}
	return n
}

func (m *Model) folderLine() screenLine {
	var b strings.Builder
	hits := make([]hit, 0, len(Folders))
	col := 1
	for i, folder := range Folders {
		name := folder.Name
		if i == m.folder {
			name = "[" + name + "]"
		}
		if i > 0 {
			b.WriteByte(' ')
			col++
		}
		start := col
		b.WriteString(name)
		col += len(name)
		hits = append(hits, hit{kind: hitFolder, n: i, x0: start, x1: col - 1})
	}
	return screenLine{text: b.String(), hits: hits}
}

func (m *Model) rowLines() []screenLine {
	if len(m.items) == 0 {
		return []screenLine{{text: "(empty)"}}
	}
	now := time.Now()
	if m.Now != nil {
		now = m.Now()
	}
	lines := make([]screenLine, 0, len(m.items))
	for i, it := range m.items {
		marker := "  "
		if i == m.cursor {
			marker = "> "
		}
		text := marker + rowText(it, now)
		if it.Unread {
			text = boldOn + text + boldOff
		}
		lines = append(lines, screenLine{text: text, hits: []hit{{kind: hitRow, n: i}}})
	}
	return lines
}

func (m *Model) detailLines() []screenLine {
	if m.detail == nil {
		return []screenLine{{text: "(empty)"}}
	}
	it := m.detail
	lines := []screenLine{{text: it.ID}}
	for _, part := range strings.Split(it.Question, "\n") {
		lines = append(lines, screenLine{text: part})
	}
	if it.Kind == decide.KindTypeIn {
		lines = append(lines,
			screenLine{text: "type: " + it.TypeText},
			screenLine{text: "where: " + it.Where},
			screenLine{text: "mark done", hits: []hit{{kind: hitMarkDone}}},
		)
	} else {
		lines = append(lines, m.relayLines(it)...)
	}
	if m.mode == modeOutcome {
		lines = append(lines, screenLine{text: "outcome: " + m.draft})
	}
	return lines
}

func (m *Model) relayLines(it *decide.Item) []screenLine {
	var lines []screenLine
	for i, opt := range it.Options {
		if i >= 9 {
			break
		}
		mark := ""
		if opt.Recommended {
			mark = " recommended"
		}
		lines = append(lines, screenLine{
			text: fmt.Sprintf("%d %s %s%s", i+1, opt.Key, opt.Label, mark),
			hits: []hit{{kind: hitOption, n: i}},
		})
	}
	for _, link := range it.Links {
		lines = append(lines, screenLine{text: "link: " + link})
	}
	deadline := "-"
	if it.Deadline != nil {
		deadline = it.Deadline.Format(time.RFC3339)
	}
	lines = append(lines, screenLine{text: "deadline: " + deadline})
	if it.FreeText && m.mode == modeDetail {
		lines = append(lines,
			screenLine{text: "answer: " + m.draft, hits: []hit{{kind: hitAnswer}}},
			screenLine{text: "submit", hits: []hit{{kind: hitSubmit}}},
		)
	}
	return lines
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
