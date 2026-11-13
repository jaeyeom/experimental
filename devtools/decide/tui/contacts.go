package tui

import (
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
)

// contact is one row of the Contacts folder. OnBus is a name the last
// bus list reported. Offline is a ledger name that list did not report.
type contact struct {
	Name     string
	Liveness string
	OnBus    bool
	Offline  bool
	Open     int
	Last     time.Time
}

func (c contact) liveLabel() string {
	if c.OnBus {
		return c.Liveness
	}
	if c.Offline {
		return "(offline)"
	}
	return ""
}

// showContacts opens the Contacts folder and asks Update to refresh the
// bus list in the background. The ledger rows are already shown.
func (m *Model) showContacts() error {
	m.composing = false
	m.answerFocus = false
	m.replyFocus = false
	m.mode = modeContacts
	m.scroll = 0
	m.contactCursor = 0
	m.rebuildContacts()
	m.busWant = true
	m.status = "contacts"
	return nil
}

// rebuildContacts merges the last ledger read with the last bus list.
// The highlighted name stays put.
func (m *Model) rebuildContacts() {
	prev := ""
	if rows := m.visibleContacts(); m.contactCursor >= 0 && m.contactCursor < len(rows) {
		prev = rows[m.contactCursor].Name
	}
	m.contacts = buildContacts(m.allItems, m.busNames, m.busKnown || m.busNote != "")
	m.contactCursor = 0
	for i, c := range m.visibleContacts() {
		if c.Name == prev {
			m.contactCursor = i
			break
		}
	}
	m.clampContactCursor()
}

// visibleContacts is the contact list, filtered by the / query while
// Contacts is open.
func (m *Model) visibleContacts() []contact {
	if m.filter == "" || m.mode != modeContacts {
		return m.contacts
	}
	q := strings.ToLower(m.filter)
	out := make([]contact, 0, len(m.contacts))
	for _, c := range m.contacts {
		if strings.Contains(strings.ToLower(c.Name), q) || strings.Contains(strings.ToLower(c.liveLabel()), q) {
			out = append(out, c)
		}
	}
	return out
}

// contactSet collects one row per agent name.
type contactSet struct {
	byName map[string]*contact
	order  []string
}

// buildContacts is the union of bus names and ledger owners and senders.
// A name only the ledger has is offline once a bus list has succeeded
// or failed. Before that answer it is not marked. Open items sort
// first, then the newest activity, then the name.
func buildContacts(items []decide.Item, bus []Target, busKnown bool) []contact {
	set := contactSet{byName: map[string]*contact{}}
	set.addBus(bus)
	set.addItems(items)
	return set.list(busKnown)
}

func (s *contactSet) add(name string) *contact {
	if name == "" {
		return nil
	}
	if c, ok := s.byName[name]; ok {
		return c
	}
	c := &contact{Name: name}
	s.byName[name] = c
	s.order = append(s.order, name)
	return c
}

func (s *contactSet) addBus(bus []Target) {
	for _, tg := range bus {
		c := s.add(tg.Name)
		if c == nil {
			continue
		}
		c.OnBus = true
		c.Liveness = tg.Live
	}
}

func (s *contactSet) addItems(items []decide.Item) {
	for i := range items {
		s.addItem(items[i])
	}
}

func (s *contactSet) addItem(it decide.Item) {
	seen := map[string]bool{}
	last := itemActivity(it)
	open := it.State == decide.StateOpen && !it.Snoozed()
	s.note(it.Owner, open, last, seen)
	for _, msg := range it.Messages {
		s.note(msg.Sender(), open, last, seen)
	}
}

func (s *contactSet) note(name string, open bool, last time.Time, seen map[string]bool) {
	if name == "" || seen[name] {
		return
	}
	seen[name] = true
	c := s.add(name)
	if open {
		c.Open++
	}
	if last.After(c.Last) {
		c.Last = last
	}
}

func (s *contactSet) list(busKnown bool) []contact {
	out := make([]contact, 0, len(s.order))
	for _, name := range s.order {
		c := *s.byName[name]
		if busKnown && !c.OnBus {
			c.Offline = true
		}
		out = append(out, c)
	}
	sortContacts(out)
	return out
}

func sortContacts(out []contact) {
	slices.SortStableFunc(out, func(a, b contact) int {
		if (a.Open > 0) != (b.Open > 0) {
			if a.Open > 0 {
				return -1
			}
			return 1
		}
		if !a.Last.Equal(b.Last) {
			if a.Last.After(b.Last) {
				return -1
			}
			return 1
		}
		return strings.Compare(a.Name, b.Name)
	})
}

func itemActivity(it decide.Item) time.Time {
	last := it.CreatedAt
	for _, msg := range it.Messages {
		if msg.Time.After(last) {
			last = msg.Time
		}
	}
	for _, t := range []time.Time{it.AnsweredAt, it.DoneAt, it.WithdrawnAt} {
		if t.After(last) {
			last = t
		}
	}
	if it.SnoozedAt != nil && it.SnoozedAt.After(last) {
		last = *it.SnoozedAt
	}
	return last
}

// itemInvolves reports whether name owns the item or sent a message on it.
func itemInvolves(it decide.Item, name string) bool {
	if name == "" {
		return false
	}
	if it.Owner == name {
		return true
	}
	for _, msg := range it.Messages {
		if msg.Sender() == name {
			return true
		}
	}
	return false
}

func (m *Model) contactsKey(r rune) (bool, error) {
	switch r {
	case 'q':
		return true, nil
	case 'g':
		m.beginChord()
	case 'u':
		m.backToList()
	case 'j':
		m.moveContact(1)
	case 'k':
		m.moveContact(-1)
	case 'G':
		m.jumpList(m.listLen() - 1)
	case '\r', '\n':
		if c, ok := m.selectedContact(); ok {
			m.applyAgentFilter(c.Name)
		}
	case '\x1b':
		m.clearAgentFilter()
	case 'A':
		if c, ok := m.selectedContact(); ok {
			m.openAvatarPick(c.Name)
		}
	case '/':
		m.beginSearch()
	}
	return false, nil
}

func (m *Model) selectedContact() (contact, bool) {
	rows := m.visibleContacts()
	if m.contactCursor < 0 || m.contactCursor >= len(rows) {
		return contact{}, false
	}
	return rows[m.contactCursor], true
}

func (m *Model) moveContact(delta int) {
	n := len(m.visibleContacts())
	if n == 0 {
		return
	}
	m.contactCursor = min(max(m.contactCursor+delta, 0), n-1)
	m.revealContactCursor()
}

func (m *Model) clampContactCursor() {
	n := len(m.visibleContacts())
	if n == 0 || m.contactCursor < 0 {
		m.contactCursor = 0
		return
	}
	if m.contactCursor >= n {
		m.contactCursor = n - 1
	}
}

func (m *Model) revealContactCursor() {
	if len(m.visibleContacts()) == 0 {
		return
	}
	m.clampContactCursor()
	_, limit := m.layoutBudget()
	// The contact table starts with a header row.
	m.scroll = revealRow(m.scroll, m.contactCursor+1, limit)
}

// applyAgentFilter keeps, in every folder, items name owns or sent a
// message on. The current folder updates at once.
func (m *Model) applyAgentFilter(name string) {
	if name == "" {
		return
	}
	m.agentFilter = name
	m.refilter(m.cursorID())
	if m.mode == modeList {
		m.revealListCursor()
	}
}

// clearAgentFilter drops the agent filter. Esc in a list, Status, or
// Contacts does this. A search in progress keeps the search key.
func (m *Model) clearAgentFilter() {
	if m.agentFilter == "" {
		return
	}
	m.agentFilter = ""
	m.refilter(m.cursorID())
	if m.mode == modeList {
		m.revealListCursor()
	}
}

// avatarMark is the Contacts avatar column: the built-in id, "file",
// or "-" when the name has no avatar.
func (m *Model) avatarMark(name string) string {
	if rule, ok := decide.MatchAvatar(m.Avatars, name); ok {
		switch {
		case rule.Hide:
			return "-"
		case rule.Builtin != "":
			return rule.Builtin
		case rule.File != "":
			return "file"
		}
	}
	if m.AvatarDefault {
		return avatar.PickBuiltin(name)
	}
	return "-"
}

func (m *Model) contactsMail(pane int) []screenLine {
	if m.width <= 0 {
		return stackRegions(0, m.sidebarLines(), m.contactsScreen(0))
	}
	if m.narrow() {
		return stackRegions(pane, []screenLine{m.folderTabLine(pane)}, m.contactsScreen(pane))
	}
	sidebar := m.sidebarLines()
	sideW, mainW := listColumnWidths(maxWidth(sidebar), pane)
	cols := [][]screenLine{
		fitColumnLines(sidebar, sideW),
		m.contactsScreen(mainW),
	}
	return zipFixed(cols, []int{sideW, mainW})
}

func (m *Model) contactsScreen(width int) []screenLine {
	rows := m.visibleContacts()
	var lines []screenLine
	if len(rows) == 0 {
		lines = append(lines, screenLine{text: m.emptyText("no contacts")})
	} else {
		useStyles()
		lines = append(lines, m.contactTable(width, m.now(), rows)...)
	}
	_, limit := m.layoutBudget()
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	lines = m.withFilterHeader(lines)
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

func (m *Model) contactTable(width int, now time.Time, rows []contact) []screenLine {
	headers := []string{"avatar", "name", "liveness", "open", "activity"}
	cells := make([][]string, len(rows))
	loc := m.zone()
	for i, row := range rows {
		cells[i] = []string{
			m.avatarMark(row.Name),
			row.Name,
			row.liveLabel(),
			strconv.Itoa(row.Open),
			stamp(row.Last, now, loc),
		}
	}
	return styledStatusRows(headers, cells, width, func(i int) lipgloss.Style {
		if i == m.contactCursor {
			return plainLine.Reverse(true)
		}
		return plainLine
	})
}
