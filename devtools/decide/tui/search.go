package tui

import (
	"fmt"
	"regexp"
	"slices"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// searchPattern matches q as literal text, ignoring case. An empty q is nil.
func searchPattern(q string) *regexp.Regexp {
	if q == "" {
		return nil
	}
	return regexp.MustCompile("(?i)" + regexp.QuoteMeta(q))
}

func matchesAny(re *regexp.Regexp, fields ...string) bool {
	for _, field := range fields {
		if re.MatchString(field) {
			return true
		}
	}
	return false
}

// searchText is the lowercase id, owner, question, and every message
// text. NUL separators keep a match inside one field.
func searchText(it *decide.Item) string {
	fields := []string{it.ID, it.Owner, it.Question}
	for _, msg := range it.Messages {
		fields = append(fields, msg.Text)
	}
	return strings.ToLower(strings.Join(fields, "\x00"))
}

// setFolder caches the folder's rows and their search text, then filters
// them. The cursor stays on keep when that id still matches.
func (m *Model) setFolder(items []decide.Item, keep string) {
	m.listTotal = len(items)
	m.folderItems = items
	m.folderText = make([]string, len(items))
	for i := range items {
		m.folderText[i] = searchText(&items[i])
	}
	m.refilter(keep)
}

// listQuery is the search folder counts follow. Typing counts at once,
// including a large list that has not applied the draft yet. A detail
// highlight does not replace the list filter.
func (m *Model) listQuery() string {
	if m.mode != modeDetail && m.searching {
		return m.search
	}
	return m.filter
}

// matchCount is how many cached search texts contain q, using the same
// case-insensitive literal match as the list filter.
func matchCount(texts []string, q string) int {
	needle := strings.ToLower(q)
	n := 0
	for _, text := range texts {
		if strings.Contains(text, needle) {
			n++
		}
	}
	return n
}

// refilter applies m.filter to the cached folder rows.
func (m *Model) refilter(keep string) {
	q := strings.ToLower(m.filter)
	if q == "" {
		m.items = m.folderItems
	} else {
		m.items = make([]decide.Item, 0, len(m.folderItems))
		for i, text := range m.folderText {
			if strings.Contains(text, q) {
				m.items = append(m.items, m.folderItems[i])
			}
		}
	}
	m.cursor = max(slices.IndexFunc(m.items, func(it decide.Item) bool { return it.ID == keep }), 0)
}

func (m *Model) cursorID() string {
	if m.cursor >= 0 && m.cursor < len(m.items) {
		return m.items[m.cursor].ID
	}
	return ""
}

func filterStatus(agents []decide.AgentStatus, resources []decide.ResourceStatus, q string) ([]decide.AgentStatus, []decide.ResourceStatus) {
	re := searchPattern(q)
	if re == nil {
		return agents, resources
	}
	var keptAgents []decide.AgentStatus
	for _, row := range agents {
		if matchesAny(re, row.Name, row.State, row.Text) {
			keptAgents = append(keptAgents, row)
		}
	}
	var keptResources []decide.ResourceStatus
	for _, row := range resources {
		if matchesAny(re, row.Name, row.Availability, decide.ResourceNote(row)) {
			keptResources = append(keptResources, row)
		}
	}
	return keptAgents, keptResources
}

// A list at least searchDebounceItems long re-filters only after typing
// pauses for searchDebounce. Enter always applies the final query.
const (
	searchDebounce      = 75 * time.Millisecond
	searchDebounceItems = 1000
)

// searchTickMsg asks for a debounced filter pass.
type searchTickMsg struct{}

// searchSnapshot is the state from before /, which Esc restores.
type searchSnapshot struct {
	filter, highlight string
	id                string
	scroll            int
	agentCursor       int
}

// searchKey edits the / prompt. Every edit applies the query at once.
// Enter keeps it, and an empty query clears it. Esc restores the state
// from before /.
func (m *Model) searchKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.restoreSearch()
		m.endSearch()
	case '\r', '\n':
		m.applyQuery(m.search)
		m.endSearch()
	case '\b', '\x7f':
		m.search = dropLastRune(m.search)
		m.liveSearch()
	default:
		if r >= ' ' {
			m.search += string(r)
			m.liveSearch()
		}
	}
	return false, nil
}

func (m *Model) beginSearch() {
	m.searching = true
	m.search = ""
	m.prior = searchSnapshot{
		filter:      m.filter,
		highlight:   m.highlight,
		id:          m.cursorID(),
		scroll:      m.scroll,
		agentCursor: m.agentCursor,
	}
}

func (m *Model) endSearch() {
	m.searching = false
	m.search = ""
	m.searchPending = false
}

// liveSearch applies the draft query, or defers it in a large list.
func (m *Model) liveSearch() {
	if m.mode == modeList && len(m.folderItems) >= searchDebounceItems {
		m.searchPending = true
		m.searchArmed = true
		m.searchAt = m.now()
		return
	}
	m.applyQuery(m.search)
}

// searchTick schedules one debounced pass after a deferred keystroke.
func (m *Model) searchTick() tea.Cmd {
	if !m.searchArmed {
		return nil
	}
	m.searchArmed = false
	return tea.Tick(searchDebounce, func(time.Time) tea.Msg { return searchTickMsg{} })
}

// finishDebounce applies a deferred query once typing has paused.
func (m *Model) finishDebounce() {
	if m.searching && m.searchPending && m.now().Sub(m.searchAt) >= searchDebounce {
		m.applyQuery(m.search)
	}
}

// applyQuery highlights q in an open item, or filters the list or status.
func (m *Model) applyQuery(q string) {
	m.searchPending = false
	switch m.mode {
	case modeDetail:
		m.highlight = q
		m.scrollToMatch()
	case modeStatus:
		m.filter = q
		if err := m.loadStatus(); err != nil {
			m.status = err.Error()
		}
		m.scroll = 0
		m.revealAgentCursor()
	default:
		m.filter = q
		m.refilter(m.cursorID())
		m.revealListCursor()
	}
}

// restoreSearch puts back the filter or highlight, cursor, and scroll from
// before /.
func (m *Model) restoreSearch() {
	p := m.prior
	switch m.mode {
	case modeDetail:
		m.highlight = p.highlight
	case modeStatus:
		m.filter = p.filter
		m.agentCursor = p.agentCursor
		if err := m.loadStatus(); err != nil {
			m.status = err.Error()
		}
	default:
		m.filter = p.filter
		m.refilter(p.id)
	}
	m.scroll = p.scroll
}

// scrollToMatch scrolls the open item to the first match at or after the
// scroll from before /, wrapping to the first match above it. No match
// keeps that scroll.
func (m *Model) scrollToMatch() {
	m.scroll = m.prior.scroll
	if m.highlight == "" {
		return
	}
	first := -1
	for i, line := range m.detailLines(m.detailInnerWidth(m.contentWidth())) {
		if !line.match {
			continue
		}
		if i >= m.prior.scroll {
			m.scroll = i
			return
		}
		if first < 0 {
			first = i
		}
	}
	if first >= 0 {
		m.scroll = first
	}
}

func (m *Model) now() time.Time {
	if m.Now != nil {
		return m.Now()
	}
	return time.Now()
}

// zone is the zone stamps use. UTC forces UTC. Otherwise it is the
// clock's zone, which is the machine zone for time.Now.
func (m *Model) zone() *time.Location {
	if m.UTC {
		return time.UTC
	}
	if loc := m.now().Location(); loc != nil {
		return loc
	}
	return time.UTC
}

func (m *Model) searchPrompt() screenLine {
	return screenLine{text: "/" + withCursor(m.search, len([]rune(m.search))) + "  as you type  enter keeps  esc restores"}
}

// filterHeader names the list, the query, and how many rows it keeps.
func (m *Model) filterHeader() (screenLine, bool) {
	if m.filter == "" || m.composing || (m.mode != modeList && m.mode != modeStatus) {
		return screenLine{}, false
	}
	name := "status"
	shown := len(m.agents) + len(m.resources)
	total := m.statusTotal
	if m.mode == modeList {
		name = strings.ToLower(Folders[m.folder].Name)
		shown = len(m.items)
		total = m.listTotal
	}
	return screenLine{text: fmt.Sprintf("%s  /%s  (%d of %d)", name, m.filter, shown, total)}, true
}

// emptyText is the placeholder for an empty list. A filter says no matches.
func (m *Model) emptyText(placeholder string) string {
	if m.filter != "" {
		return "no matches"
	}
	return placeholder
}

func (m *Model) withFilterHeader(lines []screenLine) []screenLine {
	header, ok := m.filterHeader()
	if !ok {
		return lines
	}
	return append([]screenLine{header}, lines...)
}

// highlightCount counts matches in the thread text the item view shows.
func highlightCount(it *decide.Item, re *regexp.Regexp) int {
	if len(it.Messages) == 0 {
		return len(re.FindAllStringIndex(it.Question, -1))
	}
	n := 0
	for _, msg := range it.Messages {
		n += len(re.FindAllStringIndex(msg.Text, -1))
	}
	return n
}

// paintCell renders a list cell in style with each match of re in
// searchMatch. A nil re paints the whole cell in style.
func paintCell(text string, style lipgloss.Style, re *regexp.Regexp) string {
	if re == nil {
		return style.Render(text)
	}
	useStyles()
	var b strings.Builder
	pos := 0
	for _, mt := range re.FindAllStringIndex(text, -1) {
		if pos < mt[0] {
			b.WriteString(style.Render(text[pos:mt[0]]))
		}
		b.WriteString(searchMatch.Render(text[mt[0]:mt[1]]))
		pos = mt[1]
	}
	if pos < len(text) {
		b.WriteString(style.Render(text[pos:]))
	}
	return b.String()
}

// paintMatches renders s[from:to] as plain text with each match in
// searchMatch. matches are byte ranges of s.
func paintMatches(s string, from, to int, matches [][]int) string {
	useStyles()
	var b strings.Builder
	pos := from
	for _, mt := range matches {
		start, end := max(mt[0], pos), min(mt[1], to)
		if start >= end {
			continue
		}
		if pos < start {
			b.WriteString(plainText(s[pos:start]))
		}
		b.WriteString(searchMatch.Render(s[start:end]))
		pos = end
	}
	if pos < to {
		b.WriteString(plainText(s[pos:to]))
	}
	return b.String()
}
