// Package tui is the keyboard inbox for decide.
//
// decide inbox is a bubbletea program. The list, folders, and detail view are
// lipgloss. Free text and the type-in outcome field are bubbles text inputs.
// The compose box is a bubbles textarea. It sends a note to a configured pane.
// No pane id is built in.
package tui

import (
	"fmt"
	"io"
	"regexp"
	"strings"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/bubbles/textinput"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/muesli/termenv"
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
	modeStatus
)

const (
	focusRecipient = iota
	focusText
	focusSend
)

// narrowWidth is the smallest measured terminal that keeps the sidebar column.
const narrowWidth = 80

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
	width        int
	counts       []int
	scroll       int
	rows         [][]hit
	target       int
	composing    bool
	composeFocus int
	chord        bool
	chordSeq     int
	help         bool
	answerFocus  bool
	note         string
	runSend      func([]string) error
	interval     time.Duration
	runErr       error
	free         textinput.Model
	outcome      textinput.Model
	noteField    textarea.Model
	agents       []decide.AgentStatus
	resources    []decide.ResourceStatus
}

// New returns an inbox positioned on the open folder.
func New(dir string) *Model {
	return &Model{
		Dir:       dir,
		Now:       time.Now,
		free:      newTextField(),
		outcome:   newTextField(),
		noteField: newNoteField(),
	}
}

func newNoteField() textarea.Model {
	field := textarea.New()
	field.Prompt = ""
	field.ShowLineNumbers = false
	field.Placeholder = ""
	// A wide field keeps the caret column on the logical line. The view
	// wraps separately to the panel width.
	field.SetWidth(4096)
	return field
}

func newTextField() textinput.Model {
	field := textinput.New()
	field.Prompt = ""
	field.CharLimit = 0
	field.Width = 0
	return field
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
	m.counts = make([]int, len(Folders))
	for _, it := range all {
		for i, folder := range Folders {
			if it.State == folder.State {
				m.counts[i]++
			}
		}
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

// Key applies one key. In the list, j and k move, o or Enter opens the row,
// g starts a folder chord, c opens compose, and q quits. A second key of
// i, a, d, or w opens Inbox, Answered, Done, or Withdrawn. In the opened
// item, j and k move to the next and previous item, and u returns to the
// list. 1 through 9 answer a relay option, 0 focuses free text on a relay
// item that allows it, and e asks for an outcome then marks the item done.
// Enter submits free text only while that row is focused. Esc leaves the
// row and keeps the unsent draft. In compose, Tab moves from the recipient
// to the text to [Send], Enter inserts a newline in the text, and Enter on
// [Send] sends. Printable keys type into a focused field. ? opens the
// shortcut overlay only when no text field is focused, and Esc or ? closes
// it.
func (m *Model) Key(r rune) (bool, error) {
	if m.chord {
		return m.finishChord(r)
	}
	if m.help {
		return m.helpKey(r)
	}
	if m.answerFocus {
		return m.answerKey(r)
	}
	if m.mode == modeOutcome {
		return m.outcomeKey(r)
	}
	if m.composing {
		return m.composeKey(r)
	}
	if r == '?' {
		m.help = true
		return false, nil
	}
	switch m.mode {
	case modeDetail:
		return m.detailKey(r)
	case modeStatus:
		return m.statusKey(r)
	default:
		return m.listKey(r)
	}
}

func (m *Model) statusKey(r rune) (bool, error) {
	switch r {
	case 'q':
		return true, nil
	case 'g':
		m.beginChord()
	case 'u':
		m.backToList()
	case 'j':
		m.scroll++
	case 'k':
		if m.scroll > 0 {
			m.scroll--
		}
	}
	return false, nil
}

func (m *Model) showStatus() error {
	m.composing = false
	m.answerFocus = false
	m.mode = modeStatus
	m.scroll = 0
	if err := m.loadStatus(); err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "status"
	return nil
}

func (m *Model) loadStatus() error {
	board, err := decide.OpenStatus(m.Dir)
	if err != nil {
		return err
	}
	agents, err := board.Agents()
	if err != nil {
		return err
	}
	resources, err := board.Resources()
	if err != nil {
		return err
	}
	m.agents = agents
	m.resources = resources
	return nil
}

func (m *Model) beginChord() {
	m.chord = true
	m.chordSeq++
}

func (m *Model) finishChord(r rune) (bool, error) {
	m.chord = false
	switch r {
	case 'i':
		return false, m.showFolder(0)
	case 'a':
		return false, m.showFolder(1)
	case 'd':
		return false, m.showFolder(2)
	case 'w':
		return false, m.showFolder(3)
	case 's':
		return false, m.showStatus()
	default:
		return false, nil
	}
}

func (m *Model) expireChord(seq int) {
	if !m.chord || seq != m.chordSeq {
		return
	}
	m.chord = false
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
	case 'g':
		m.beginChord()
	case 'o', '\r', '\n':
		if err := m.openRow(); err != nil {
			return false, err
		}
	case 'c':
		m.focusCompose()
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

func (m *Model) detailKey(r rune) (bool, error) {
	switch r {
	case '\x1b', 'u':
		m.backToList()
		return false, nil
	case 'j':
		return false, m.openRelative(1)
	case 'k':
		return false, m.openRelative(-1)
	case 'g':
		m.beginChord()
		return false, nil
	case 'c':
		m.focusCompose()
		return false, nil
	case 'e':
		m.beginOutcome()
		return false, nil
	case '0':
		if m.detail != nil && m.detail.FreeText {
			m.focusAnswer()
			return false, nil
		}
	}
	if r >= '1' && r <= '9' {
		return false, m.answerOption(int(r - '1'))
	}
	if r >= ' ' && (m.detail == nil || !m.detail.FreeText) {
		return false, m.typeFree(r)
	}
	return false, nil
}

// answerKey edits the free-text row. Esc leaves the box and keeps the draft.
func (m *Model) answerKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.answerFocus = false
		m.free.Blur()
		m.draft = m.free.Value()
		return false, nil
	case '\r', '\n':
		m.draft = m.free.Value()
		return false, m.submitFreeText()
	case '\b', '\x7f':
		m.editField(&m.free, r)
		return false, nil
	}
	if r >= ' ' {
		m.editField(&m.free, r)
	}
	return false, nil
}

func (m *Model) editField(field *textinput.Model, r rune) {
	updated, _ := field.Update(runeKey(r))
	*field = updated
	m.draft = field.Value()
}

func (m *Model) pullNote() {
	if m.noteField.Value() != m.note {
		m.noteField.SetValue(m.note)
	}
}

func (m *Model) editNote(r rune) {
	m.pullNote()
	if !m.noteField.Focused() {
		m.noteField.Focus()
	}
	updated, _ := m.noteField.Update(runeKey(r))
	m.noteField = updated
	m.note = m.noteField.Value()
}

func (m *Model) focusCompose() {
	m.composing = true
	m.composeFocus = focusRecipient
	m.answerFocus = false
	m.free.Blur()
	m.pullNote()
	m.noteField.Blur()
}

func (m *Model) blurCompose() {
	if m.noteField.Focused() {
		m.note = m.noteField.Value()
	}
	m.composing = false
	m.noteField.Blur()
}

func (m *Model) insertNoteNewline() {
	m.pullNote()
	m.noteField.InsertRune('\n')
	m.note = m.noteField.Value()
}

func (m *Model) clearNote() {
	m.note = ""
	m.composing = false
	m.composeFocus = focusRecipient
	m.noteField.Blur()
	m.noteField.Reset()
}

func (m *Model) focusAnswer() {
	m.answerFocus = true
	m.composing = false
	m.outcome.Blur()
	m.free.SetValue(m.draft)
	m.free.CursorEnd()
	m.free.Focus()
}

func (m *Model) beginOutcome() {
	m.mode = modeOutcome
	m.draft = ""
	m.answerFocus = false
	m.free.Blur()
	m.outcome.SetValue("")
	m.outcome.Focus()
	m.status = "outcome"
}

func (m *Model) outcomeKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.mode = modeDetail
		m.draft = ""
		m.outcome.Blur()
		m.outcome.SetValue("")
		if m.detail != nil {
			m.status = "opened " + m.detail.ID
		}
		return false, nil
	case '\r', '\n':
		m.draft = m.outcome.Value()
		return false, m.submitOutcome()
	case '\b', '\x7f':
		m.editField(&m.outcome, r)
		return false, nil
	}
	if r >= ' ' {
		m.editField(&m.outcome, r)
		m.status = "outcome"
	}
	return false, nil
}

func (m *Model) openRelative(delta int) error {
	next := m.cursor + delta
	if next < 0 || next >= len(m.items) {
		return nil
	}
	m.cursor = next
	return m.openRow()
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
	m.answerFocus = false
	m.resetFields()
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
	updated, err := led.Answer(id, value)
	if err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "answered " + id
	if updated.Kind == decide.KindRelay {
		if err := decide.Notify(&updated, io.Discard); err != nil {
			m.status = err.Error()
		}
	}
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
	m.answerFocus = false
	m.resetFields()
	m.scroll = 0
}

func (m *Model) resetFields() {
	m.free.Blur()
	m.free.SetValue("")
	m.outcome.Blur()
	m.outcome.SetValue("")
}

type screenLine struct {
	text string
	hits []hit
}

// View renders the sidebar, list, detail, compose panel, and status bar.
// Unread rows are bold. A positive height scrolls inside the list or detail.
func (m *Model) View() string {
	lines := m.allLines()
	if m.height > 0 && len(lines) > m.height {
		lines = append([]screenLine(nil), lines[:m.height]...)
	}
	m.rows = make([][]hit, len(lines))
	parts := make([]string, len(lines))
	for i, line := range lines {
		parts[i] = line.text
		m.rows[i] = line.hits
	}
	return strings.Join(parts, "\n")
}

func (m *Model) allLines() []screenLine {
	var lines []screenLine
	if m.help {
		lines = m.helpLines()
		if w := m.contentWidth(); w > 0 {
			lines = fitColumnLines(lines, w)
		}
	} else {
		lines = m.mailLines()
	}
	status := m.statusLine()
	if w := m.contentWidth(); w > 0 {
		status = fitLine(status, w)
	}
	lines = append(lines, status)
	return m.frameLines(lines)
}

// contentWidth is the width inside the outer frame, or the window when the
// frame is not drawn. Zero means the terminal has not been measured.
func (m *Model) contentWidth() int {
	if m.framed() {
		return m.width - 2
	}
	if m.width > 0 {
		return m.width
	}
	return 0
}

func (m *Model) framed() bool {
	return m.width >= 2 && m.height >= 2
}

func (m *Model) narrow() bool {
	return m.width > 0 && m.width < narrowWidth
}

func (m *Model) statusLine() screenLine {
	if m.chord {
		return screenLine{text: "g…"}
	}
	const hint = "gi/ga/gd/gw/gs  o open  u list  c  tab recipient/text/[Send]  ? help"
	if m.status == "" {
		return screenLine{text: hint}
	}
	return screenLine{text: m.status + "  " + hint}
}

func (m *Model) listFocused() bool {
	return m.mode == modeList && !m.composing && !m.answerFocus
}

func (m *Model) detailFocused() bool {
	return m.mode != modeList && m.mode != modeStatus && !m.composing && !m.answerFocus
}

func (m *Model) mailLines() []screenLine {
	pane := m.contentWidth()
	if m.composing {
		return m.composeScreen(pane)
	}
	if m.mode == modeStatus {
		return m.statusScreen(pane)
	}
	if m.mode != modeList {
		return m.openedScreen(pane)
	}
	if m.width <= 0 {
		return m.unmeasuredList()
	}
	if m.narrow() {
		return m.stackedList(pane)
	}
	return m.columnList(pane)
}

func (m *Model) statusScreen(width int) []screenLine {
	now := time.Now()
	if m.Now != nil {
		now = m.Now()
	}
	lines := make([]screenLine, 0, len(m.agents)+len(m.resources))
	if len(m.agents) == 0 && len(m.resources) == 0 {
		lines = append(lines, screenLine{text: "no status"})
	}
	useStyles()
	for _, row := range m.agents {
		text := fmt.Sprintf("%s %s %s %s", row.Name, row.State, age(now.Sub(row.UpdatedAt)), row.Text)
		lines = append(lines, screenLine{text: statusStyle(row.UpdatedAt, now).Render(text)})
	}
	for _, row := range m.resources {
		lines = append(lines, screenLine{text: statusStyle(row.UpdatedAt, now).Render(decide.ResourceLine(row))})
	}
	_, limit := m.layoutBudget()
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

func statusStyle(updated, now time.Time) lipgloss.Style {
	if decide.StatusStale(updated, now) {
		return detailLine
	}
	return plainLine
}

func (m *Model) unmeasuredList() []screenLine {
	sidebar := m.sidebarLines()
	list := m.clipList(m.rowLines(0))
	if m.listFocused() {
		list = boxLines(list)
	}
	return stackRegions(0, sidebar, list)
}

func (m *Model) stackedList(pane int) []screenLine {
	list := m.clipList(m.rowLines(pane))
	list = fitPane(list, pane, m.listFocused())
	return stackRegions(pane, []screenLine{m.folderTabLine(pane)}, list)
}

func (m *Model) columnList(pane int) []screenLine {
	sidebar := m.sidebarLines()
	sideW, listW := listColumnWidths(maxWidth(sidebar), pane)
	listInner := listW
	if m.listFocused() && listW >= 2 {
		listInner = listW - 2
	}
	list := m.clipList(m.rowLines(listInner))
	cols := [][]screenLine{
		fitColumnLines(sidebar, sideW),
		fitPane(list, listW, m.listFocused()),
	}
	return zipFixed(cols, []int{sideW, listW})
}

func (m *Model) clipList(list []screenLine) []screenLine {
	listN, _ := m.layoutBudget()
	var scroll int
	list, scroll = clipLines(list, m.scroll, listN)
	m.scroll = scroll
	return list
}

func (m *Model) openedScreen(width int) []screenLine {
	textW := width
	if (m.detailFocused() || m.answerFocus) && width >= 2 {
		textW = width - 2
	}
	detail := m.detailLines(textW)
	_, detailN := m.layoutBudget()
	var scroll int
	detail, scroll = clipLines(detail, m.scroll, detailN)
	m.scroll = scroll
	if width <= 0 {
		if m.answerFocus {
			return boxAnswer(detail)
		}
		if m.detailFocused() {
			return boxLines(detail)
		}
		return detail
	}
	return m.fitDetail(detail, width)
}

func (m *Model) composeScreen(width int) []screenLine {
	lines := m.composeBody()
	if width <= 0 {
		return lines
	}
	return fitColumnLines(lines, width)
}

func listColumnWidths(sidebar, pane int) (int, int) {
	budget := pane - 1
	if budget < 2 {
		budget = 2
	}
	side := sidebar
	if side < 1 {
		side = 1
	}
	if side > budget-1 {
		side = budget - 1
	}
	return side, budget - side
}

func (m *Model) fitDetail(lines []screenLine, width int) []screenLine {
	switch {
	case m.answerFocus:
		return boxAnswerWithin(lines, width)
	case m.detailFocused():
		return fitPane(lines, width, true)
	default:
		return fitPane(lines, width, false)
	}
}

func (m *Model) folderTabLine(width int) screenLine {
	var b strings.Builder
	hits := make([]hit, 0, len(Folders))
	for i, folder := range Folders {
		if i > 0 {
			b.WriteByte(' ')
		}
		count := 0
		if i < len(m.counts) {
			count = m.counts[i]
		}
		name := folder.Name
		if i == m.folder {
			name = "[" + name + "]"
		}
		label := fmt.Sprintf("%s %d", name, count)
		start := lipgloss.Width(b.String()) + 1
		b.WriteString(label)
		hits = append(hits, hit{
			kind: hitFolder,
			n:    i,
			x0:   start,
			x1:   lipgloss.Width(b.String()),
		})
	}
	return fitLine(screenLine{text: b.String(), hits: hits}, width)
}

func (m *Model) sidebarLines() []screenLine {
	lines := make([]screenLine, len(Folders))
	for i, folder := range Folders {
		count := 0
		if i < len(m.counts) {
			count = m.counts[i]
		}
		name := folder.Name
		if i == m.folder {
			name = "[" + name + "]"
		}
		label := fmt.Sprintf("%s %d", name, count)
		lines[i] = screenLine{
			text: label,
			hits: []hit{{kind: hitFolder, n: i}},
		}
	}
	return lines
}

func (m *Model) layoutBudget() (int, int) {
	if m.height <= 1 {
		return 0, 0
	}
	folderChrome := 0
	if m.mode == modeList && !m.composing {
		folderChrome = len(Folders)
		if m.width > 0 {
			folderChrome = 0
			if m.narrow() {
				folderChrome = 1
			}
		}
	}
	frame := 0
	if m.framed() {
		frame = 2
	}
	chrome := folderChrome + 1 + frame
	box := 0
	if !m.composing && (m.listFocused() || m.detailFocused()) {
		box = 2
	}
	remain := m.height - chrome - box
	if remain < 2 {
		remain = 2
	}
	if m.mode != modeList {
		return 0, remain
	}
	return remain, 0
}

func clipLines(lines []screenLine, scroll, limit int) ([]screenLine, int) {
	if limit <= 0 || len(lines) <= limit {
		return lines, 0
	}
	if scroll < 0 {
		scroll = 0
	}
	maxScroll := len(lines) - limit
	if scroll > maxScroll {
		scroll = maxScroll
	}
	return append([]screenLine(nil), lines[scroll:scroll+limit]...), scroll
}

func stackRegions(width int, cols ...[]screenLine) []screenLine {
	var out []screenLine
	for _, col := range cols {
		for _, line := range col {
			if width > 0 {
				line = fitLine(line, width)
			}
			out = append(out, line)
		}
	}
	return out
}

func zipFixed(cols [][]screenLine, widths []int) []screenLine {
	height := 0
	for _, col := range cols {
		if len(col) > height {
			height = len(col)
		}
	}
	if height == 0 {
		return nil
	}
	out := make([]screenLine, 0, height)
	for row := 0; row < height; row++ {
		var b strings.Builder
		var hits []hit
		colX := 1
		for i, col := range cols {
			if i > 0 {
				b.WriteByte(' ')
				colX++
			}
			var line screenLine
			if row < len(col) {
				line = col[row]
			}
			width := 1
			if i < len(widths) && widths[i] > 0 {
				width = widths[i]
			}
			fitted := fitLine(line, width)
			b.WriteString(fitted.text)
			for _, h := range fitted.hits {
				hits = append(hits, shiftHit(h, colX, width))
			}
			colX += width
		}
		out = append(out, screenLine{text: b.String(), hits: hits})
	}
	return out
}

func maxWidth(lines []screenLine) int {
	w := 1
	for _, line := range lines {
		if n := lipgloss.Width(line.text); n > w {
			w = n
		}
	}
	return w
}

func fitColumnLines(lines []screenLine, width int) []screenLine {
	if width <= 0 {
		return lines
	}
	out := make([]screenLine, len(lines))
	for i, line := range lines {
		out[i] = fitLine(line, width)
	}
	return out
}

func fitPane(lines []screenLine, width int, box bool) []screenLine {
	if len(lines) == 0 {
		return nil
	}
	if width <= 0 {
		if box {
			return boxLines(lines)
		}
		return lines
	}
	if !box || width < 2 {
		return fitColumnLines(lines, width)
	}
	inner := width - 2
	return boxWithin(fitColumnLines(lines, inner), width)
}

func shiftHit(h hit, colX, colW int) hit {
	if h.x1 == 0 {
		h.x0 = colX
		h.x1 = colX + colW - 1
		if h.x1 < h.x0 {
			h.x1 = h.x0
		}
		return h
	}
	h.x0 += colX - 1
	h.x1 += colX - 1
	return h
}

func fitLine(line screenLine, width int) screenLine {
	if width <= 0 {
		return line
	}
	w := lipgloss.Width(line.text)
	if w == width {
		return line
	}
	if w < width {
		line.text += strings.Repeat(" ", width-w)
		return line
	}
	line.text = truncateVisible(line.text, width)
	if len(line.hits) == 0 {
		return line
	}
	hits := append([]hit(nil), line.hits...)
	for i := range hits {
		if hits[i].x1 == 0 {
			continue
		}
		if hits[i].x0 > width {
			hits[i].x0 = width
		}
		if hits[i].x1 > width {
			hits[i].x1 = width
		}
	}
	line.hits = hits
	return line
}

func truncateVisible(text string, width int) string {
	if width <= 0 {
		return ""
	}
	var b strings.Builder
	vis := 0
	limit := width - 1
	if limit < 1 {
		limit = width
	}
	for i := 0; i < len(text); {
		if text[i] == 0x1b && i+1 < len(text) && text[i+1] == '[' {
			j := i + 2
			for j < len(text) && text[j] != 'm' {
				j++
			}
			if j < len(text) {
				j++
			}
			b.WriteString(text[i:j])
			i = j
			continue
		}
		if vis >= limit {
			break
		}
		_, size := utf8.DecodeRuneInString(text[i:])
		if size <= 0 {
			break
		}
		b.WriteString(text[i : i+size])
		vis++
		i += size
	}
	if vis < lipgloss.Width(text) && limit < width {
		b.WriteString("…")
	}
	return b.String()
}

var (
	styleOnce    sync.Once
	unreadRow    lipgloss.Style
	detailLine   lipgloss.Style
	plainLine    lipgloss.Style
	focusMark    lipgloss.Style
	optionButton lipgloss.Style
)

func useStyles() {
	styleOnce.Do(func() {
		// ANSI bold is stable in tests and on a real terminal.
		lipgloss.SetColorProfile(termenv.ANSI)
		unreadRow = lipgloss.NewStyle().Bold(true)
		detailLine = lipgloss.NewStyle().Faint(true)
		plainLine = lipgloss.NewStyle()
		focusMark = lipgloss.NewStyle().Bold(true)
		optionButton = lipgloss.NewStyle().Reverse(true)
	})
}

func boxAnswer(lines []screenLine) []screenLine {
	var out []screenLine
	for _, line := range lines {
		if lineHas(line, hitAnswer) {
			out = append(out, boxLines([]screenLine{line})...)
			continue
		}
		out = append(out, line)
	}
	return out
}

func lineHas(line screenLine, kind int) bool {
	for _, h := range line.hits {
		if h.kind == kind {
			return true
		}
	}
	return false
}

func boxAnswerWithin(lines []screenLine, width int) []screenLine {
	if width < 2 {
		return fitColumnLines(lines, width)
	}
	var out []screenLine
	for _, line := range lines {
		if !lineHas(line, hitAnswer) {
			out = append(out, fitLine(line, width))
			continue
		}
		out = append(out, boxWithin([]screenLine{fitLine(line, width-2)}, width)...)
	}
	return out
}

func panelBox(lines []screenLine, width int, focused bool) []screenLine {
	return boxWithinStyled(lines, width, focused)
}

func boxWithin(lines []screenLine, width int) []screenLine {
	return boxWithinStyled(lines, width, true)
}

func boxWithinStyled(lines []screenLine, width int, focused bool) []screenLine {
	if len(lines) == 0 || width < 2 {
		return boxLines(lines)
	}
	useStyles()
	inner := width - 2
	rule := strings.Repeat("─", inner)
	top := "┌" + rule + "┐"
	bot := "└" + rule + "┘"
	if focused {
		top = focusMark.Render(top)
		bot = focusMark.Render(bot)
	}
	out := []screenLine{{text: top}}
	left, right := "│", "│"
	if focused {
		left = focusMark.Render("│")
		right = focusMark.Render("│")
	}
	for _, line := range lines {
		fitted := fitLine(line, inner)
		hits := fitted.hits
		if len(hits) > 0 {
			hits = append([]hit(nil), hits...)
			for i := range hits {
				if hits[i].x1 == 0 {
					continue
				}
				hits[i].x0++
				hits[i].x1++
			}
		}
		out = append(out, screenLine{
			text: left + fitted.text + right,
			hits: hits,
		})
	}
	out = append(out, screenLine{text: bot})
	return out
}

func (m *Model) frameLines(lines []screenLine) []screenLine {
	if !m.framed() {
		return lines
	}
	innerW := m.width - 2
	innerH := m.height - 2
	body := lines
	if innerH < 1 {
		body = nil
	} else if len(body) > innerH {
		status := body[len(body)-1]
		if innerH == 1 {
			body = []screenLine{status}
		} else {
			body = append(append([]screenLine(nil), body[:innerH-1]...), status)
		}
	}
	for len(body) < innerH {
		body = append(body, screenLine{})
	}
	useStyles()
	rule := strings.Repeat("─", innerW)
	out := make([]screenLine, 0, innerH+2)
	out = append(out, screenLine{text: focusMark.Render("┌" + rule + "┐")})
	for _, line := range body {
		fitted := fitLine(line, innerW)
		hits := fitted.hits
		if len(hits) > 0 {
			hits = append([]hit(nil), hits...)
			for i := range hits {
				if hits[i].x1 == 0 {
					continue
				}
				hits[i].x0++
				hits[i].x1++
			}
		}
		out = append(out, screenLine{
			text: "│" + fitted.text + "│",
			hits: hits,
		})
	}
	out = append(out, screenLine{text: focusMark.Render("└" + rule + "┘")})
	return out
}

func boxLines(lines []screenLine) []screenLine {
	if len(lines) == 0 {
		return nil
	}
	useStyles()
	width := 0
	for _, line := range lines {
		if n := lipgloss.Width(line.text); n > width {
			width = n
		}
	}
	rule := strings.Repeat("─", width)
	out := []screenLine{{text: focusMark.Render("┌" + rule + "┐")}}
	for _, line := range lines {
		pad := width - lipgloss.Width(line.text)
		if pad < 0 {
			pad = 0
		}
		hits := line.hits
		if len(hits) > 0 {
			hits = append([]hit(nil), line.hits...)
			for i := range hits {
				if hits[i].x1 == 0 {
					continue
				}
				hits[i].x0++
				hits[i].x1++
			}
		}
		out = append(out, screenLine{
			text: "│" + line.text + strings.Repeat(" ", pad) + "│",
			hits: hits,
		})
	}
	out = append(out, screenLine{text: focusMark.Render("└" + rule + "┘")})
	return out
}

func (m *Model) helpLines() []screenLine {
	text := []string{
		"shortcuts",
		"q quit",
		"j/k move",
		"g then i inbox, a answered, d done, w withdrawn, s status",
		"o or enter opens    u returns to the list",
		"c compose",
		"tab moves recipient, text, [Send]",
		"shift+tab moves back",
		"enter on text inserts a newline",
		"enter on [Send] sends",
		"esc keeps the draft",
		"1-9 answer  e done        esc back",
		"click folder, row, option, target, or [Send]",
		"wheel scroll",
		"? or esc close",
	}
	lines := make([]screenLine, len(text))
	for i, line := range text {
		lines[i] = screenLine{text: plainText(line)}
	}
	return lines
}

func (m *Model) rowLines(width int) []screenLine {
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
		if width > 0 {
			text = rowTextFitted(it, now, marker, width)
		}
		useStyles()
		if it.Unread {
			text = unreadRow.Render(text)
		} else {
			text = plainLine.Render(text)
		}
		lines = append(lines, screenLine{text: text, hits: []hit{{kind: hitRow, n: i}}})
	}
	return lines
}

func (m *Model) detailLines(width int) []screenLine {
	if m.detail == nil {
		return []screenLine{{text: "(empty)"}}
	}
	it := m.detail
	lines := []screenLine{{text: plainText(it.ID)}}
	lines = append(lines, questionLines(it.Question, width)...)
	if it.Kind == decide.KindTypeIn {
		lines = append(lines,
			screenLine{text: plainText("type: " + it.TypeText)},
			screenLine{text: plainText("where: " + it.Where)},
			screenLine{text: plainText("mark done"), hits: []hit{{kind: hitMarkDone}}},
		)
	} else {
		lines = append(lines, m.relayLines(it, width)...)
	}
	if m.mode == modeOutcome {
		body := m.draft
		if m.outcome.Focused() {
			body = withCursor(m.draft, m.outcome.Position())
		}
		lines = append(lines, screenLine{text: plainText("outcome: " + body)})
	}
	return lines
}

func plainText(s string) string {
	useStyles()
	return detailLine.Render(s)
}

var urlPattern = regexp.MustCompile(`https?://[^[:space:]]+`)

func questionLines(q string, width int) []screenLine {
	parts := strings.Split(q, "\n")
	lines := make([]screenLine, 0, len(parts))
	for _, part := range parts {
		lines = append(lines, linkLines(part, width)...)
	}
	return lines
}

// textChunk is one wrapped piece of an original string.
type textChunk struct {
	text     string
	from, to int
}

// wrapIndexed breaks s into lines of at most width columns.
// A break prefers the last space that fits. A word longer than width
// is split. The returned ranges refer to bytes of s.
func wrapIndexed(s string, width int) []textChunk {
	if width <= 0 || lipgloss.Width(s) <= width {
		return []textChunk{{text: s, from: 0, to: len(s)}}
	}
	var out []textChunk
	i := 0
	for i < len(s) {
		end := i
		lastBreak := -1
		vis := 0
		for end < len(s) {
			r, size := utf8.DecodeRuneInString(s[end:])
			rw := lipgloss.Width(string(r))
			if rw < 1 {
				rw = 1
			}
			if vis+rw > width && end > i {
				break
			}
			if r == ' ' {
				lastBreak = end
			}
			vis += rw
			end += size
		}
		cut := end
		next := end
		if end < len(s) && lastBreak > i {
			cut = lastBreak
			next = lastBreak + 1
		}
		if cut <= i {
			_, size := utf8.DecodeRuneInString(s[i:])
			if size < 1 {
				size = 1
			}
			cut = i + size
			next = cut
		}
		out = append(out, textChunk{text: s[i:cut], from: i, to: cut})
		i = next
	}
	if len(out) == 0 {
		return []textChunk{{from: 0, to: 0}}
	}
	return out
}

func linkLines(part string, width int) []screenLine {
	chunks := wrapIndexed(part, width)
	spans := urlPattern.FindAllStringIndex(part, -1)
	lines := make([]screenLine, 0, len(chunks))
	for _, ch := range chunks {
		lines = append(lines, styleLinkChunk(part, ch, spans))
	}
	return lines
}

func styleLinkChunk(original string, ch textChunk, spans [][]int) screenLine {
	if ch.from >= ch.to {
		return screenLine{text: plainText("")}
	}
	var b strings.Builder
	var hits []hit
	pos := ch.from
	for _, span := range spans {
		if span[1] <= ch.from || span[0] >= ch.to {
			continue
		}
		visStart := span[0]
		if visStart < ch.from {
			visStart = ch.from
		}
		visEnd := span[1]
		if visEnd > ch.to {
			visEnd = ch.to
		}
		if pos < visStart {
			b.WriteString(plainText(original[pos:visStart]))
		}
		frag := original[visStart:visEnd]
		startCol := lipgloss.Width(original[ch.from:visStart]) + 1
		b.WriteString("\x1b[4m")
		b.WriteString(frag)
		b.WriteString("\x1b[24m")
		endCol := startCol + lipgloss.Width(frag) - 1
		if endCol < startCol {
			endCol = startCol
		}
		hits = append(hits, hit{
			kind: hitLink,
			x0:   startCol,
			x1:   endCol,
			url:  original[span[0]:span[1]],
		})
		pos = visEnd
	}
	if pos < ch.to {
		b.WriteString(plainText(original[pos:ch.to]))
	}
	if b.Len() == 0 {
		return screenLine{text: plainText(ch.text)}
	}
	return screenLine{text: b.String(), hits: hits}
}

func (m *Model) relayLines(it *decide.Item, width int) []screenLine {
	var lines []screenLine
	for i, opt := range it.Options {
		if i >= 9 {
			break
		}
		mark := ""
		if opt.Recommended {
			mark = " recommended"
		}
		label := fmt.Sprintf("%d %s %s%s", i+1, opt.Key, opt.Label, mark)
		useStyles()
		for _, ch := range wrapIndexed(label, width) {
			lines = append(lines, screenLine{
				text: optionButton.Render(ch.text),
				hits: []hit{{kind: hitOption, n: i}},
			})
		}
	}
	for _, link := range it.Links {
		lines = append(lines, linkLines(link, width)...)
	}
	deadline := "-"
	if it.Deadline != nil {
		deadline = it.Deadline.Format(time.RFC3339)
	}
	lines = append(lines, screenLine{text: plainText("deadline: " + deadline)})
	if it.FreeText && m.mode == modeDetail {
		lines = append(lines, screenLine{
			text: plainText(m.freeTextLine()),
			hits: []hit{{kind: hitAnswer}},
		})
	}
	return lines
}

func (m *Model) freeTextLine() string {
	body := m.draft
	if body == "" && !m.answerFocus {
		body = "___"
	}
	if m.answerFocus {
		body = withCursor(m.draft, m.free.Position())
	}
	line := "0  answer: " + body
	if m.answerFocus {
		line += " Esc to cancel"
	}
	return line
}

// cursorOn and cursorOff mark one reversed column at the caret.
const (
	cursorOn  = "\x1b[7m"
	cursorOff = "\x1b[27m"
)

// withCursor draws a one-column cursor at the rune index col.
func withCursor(text string, col int) string {
	runes := []rune(text)
	if col < 0 {
		col = 0
	}
	if col >= len(runes) {
		return text + cursorOn + " " + cursorOff
	}
	return string(runes[:col]) + cursorOn + string(runes[col]) + cursorOff + string(runes[col+1:])
}

func rowText(it decide.Item, now time.Time) string {
	question := strings.Join(strings.Fields(it.Question), " ")
	text := fmt.Sprintf("%s %s %s %s", it.ID, question, it.Owner, age(now.Sub(it.CreatedAt)))
	if it.Deadline != nil {
		text += " due " + it.Deadline.Format(time.RFC3339)
	}
	return text
}

// rowTextFitted keeps a long question and a long owner visible by cutting
// each with an ellipsis inside width.
func rowTextFitted(it decide.Item, now time.Time, marker string, width int) string {
	question := strings.Join(strings.Fields(it.Question), " ")
	tail := " " + age(now.Sub(it.CreatedAt))
	if it.Deadline != nil {
		tail += " due " + it.Deadline.Format(time.RFC3339)
	}
	owner := it.Owner
	full := marker + it.ID + " " + question + " " + owner + tail
	if lipgloss.Width(full) <= width {
		return full
	}
	fixed := lipgloss.Width(marker+it.ID+tail) + 2
	remain := width - fixed
	if remain < 2 {
		return truncateVisible(full, width)
	}
	qBudget := remain / 2
	oBudget := remain - qBudget
	qw := lipgloss.Width(question)
	ow := lipgloss.Width(owner)
	if qw < qBudget {
		oBudget += qBudget - qw
		qBudget = qw
	}
	if ow < oBudget {
		qBudget += oBudget - ow
		if qBudget > qw {
			qBudget = qw
		}
		oBudget = ow
	}
	q := question
	if qw > qBudget {
		q = truncateVisible(question, qBudget)
	}
	o := owner
	if ow > oBudget {
		o = truncateVisible(owner, oBudget)
	}
	return marker + it.ID + " " + q + " " + o + tail
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
