package tui

import (
	"fmt"
	"regexp"
	"strings"

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

// itemMatches checks the id, owner, question, and every message text.
func itemMatches(re *regexp.Regexp, it *decide.Item) bool {
	if matchesAny(re, it.ID, it.Owner, it.Question) {
		return true
	}
	for _, msg := range it.Messages {
		if re.MatchString(msg.Text) {
			return true
		}
	}
	return false
}

func filterItems(items []decide.Item, q string) []decide.Item {
	re := searchPattern(q)
	if re == nil {
		return items
	}
	out := make([]decide.Item, 0, len(items))
	for i := range items {
		if itemMatches(re, &items[i]) {
			out = append(out, items[i])
		}
	}
	return out
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
		if matchesAny(re, row.Name, row.Availability, row.Note) {
			keptResources = append(keptResources, row)
		}
	}
	return keptAgents, keptResources
}

// searchKey edits the / prompt. Enter applies the query, and an empty
// query clears it. Esc closes the prompt and keeps the current query.
func (m *Model) searchKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.searching = false
		m.search = ""
	case '\r', '\n':
		m.searching = false
		q := m.search
		m.search = ""
		return false, m.applySearch(q)
	case '\b', '\x7f':
		m.search = dropLastRune(m.search)
	default:
		if r >= ' ' {
			m.search += string(r)
		}
	}
	return false, nil
}

func (m *Model) beginSearch() {
	m.searching = true
	m.search = ""
}

// applySearch highlights q in an open item, or filters the list or status.
func (m *Model) applySearch(q string) error {
	if m.mode == modeDetail {
		m.highlight = q
		return nil
	}
	m.filter = q
	if m.mode == modeStatus {
		if err := m.loadStatus(); err != nil {
			m.status = err.Error()
		}
		m.scroll = 0
		m.revealAgentCursor()
		return nil
	}
	if err := m.Reload(); err != nil {
		return err
	}
	m.revealListCursor()
	return nil
}

func (m *Model) searchPrompt() screenLine {
	return screenLine{text: "/" + withCursor(m.search, len([]rune(m.search)))}
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
