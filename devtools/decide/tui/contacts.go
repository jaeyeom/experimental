package tui

import (
	"context"
	"fmt"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
)

// Contact states. Active is an agentbus name reported alive.
// Unresponsive is registered but not alive and not dead. Dead is gone
// or dead, or known only from the ledger once a bus list has answered.
const (
	stateActive = iota
	stateUnknown
	stateUnresponsive
	stateDead
)

const (
	confirmNone = iota
	confirmHideDead
	confirmRelease
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

// state is the contact's place in the Contacts sort.
func (c contact) state() int {
	if c.OnBus {
		switch c.Liveness {
		case "alive":
			return stateActive
		case "dead", "gone":
			return stateDead
		default:
			return stateUnresponsive
		}
	}
	if c.Offline {
		return stateDead
	}
	return stateUnknown
}

// stateLabel is the state column. Empty means the bus has not answered.
func (c contact) stateLabel() string {
	switch c.state() {
	case stateActive:
		return "active"
	case stateUnresponsive:
		return "unresponsive"
	case stateDead:
		return "dead"
	default:
		return ""
	}
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
// A hidden name that is active again leaves the hidden list. The
// highlighted name stays put.
func (m *Model) rebuildContacts() {
	prev := ""
	if rows := m.visibleContacts(); m.contactCursor >= 0 && m.contactCursor < len(rows) {
		prev = rows[m.contactCursor].Name
	}
	m.contacts = buildContacts(m.allItems, m.busNames, m.busKnown || m.busNote != "")
	m.dropActiveHidden()
	m.contactCursor = 0
	for i, c := range m.visibleContacts() {
		if c.Name == prev {
			m.contactCursor = i
			break
		}
	}
	m.clampContactCursor()
}

// visibleContacts is the contact list. Hidden names stay out unless H
// is showing them. The / query filters while Contacts is open.
func (m *Model) visibleContacts() []contact {
	rows := m.contacts
	if !m.showHidden {
		if hide := m.hiddenSet(); len(hide) > 0 {
			kept := make([]contact, 0, len(rows))
			for _, c := range rows {
				if !hide[c.Name] {
					kept = append(kept, c)
				}
			}
			rows = kept
		}
	}
	if m.filter == "" || m.mode != modeContacts {
		return rows
	}
	q := strings.ToLower(m.filter)
	out := make([]contact, 0, len(rows))
	for _, c := range rows {
		if strings.Contains(strings.ToLower(c.Name), q) || strings.Contains(strings.ToLower(c.liveLabel()), q) {
			out = append(out, c)
		}
	}
	return out
}

// hiddenSet is the hidden names.
func (m *Model) hiddenSet() map[string]bool {
	if len(m.hidden) == 0 {
		return nil
	}
	out := make(map[string]bool, len(m.hidden))
	for _, name := range m.hidden {
		out[name] = true
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
// or failed. Before that answer it is not marked. Rows sort by state
// (active, then not-yet-known, then unresponsive, then dead). Within a
// state, open items sort first, then the newest activity, then the name.
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
		if ra, rb := a.state(), b.state(); ra != rb {
			return ra - rb
		}
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
	if m.contactsHideKey(r) {
		return false, nil
	}
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

// contactsHideKey handles hide, show-hidden, hide-dead, and release.
func (m *Model) contactsHideKey(r rune) bool {
	switch r {
	case 'x':
		m.hideSelected()
	case 'H':
		m.showHidden = !m.showHidden
		if m.showHidden {
			m.status = "showing hidden contacts"
		} else {
			m.status = "hiding hidden contacts"
		}
		m.clampContactCursor()
	case 'X':
		m.askHideDead()
	case 'R':
		m.askRelease()
	default:
		return false
	}
	return true
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
	headers := []string{"avatar", "name", "state", "liveness", "open", "activity"}
	cells := make([][]string, len(rows))
	loc := m.zone()
	for i, row := range rows {
		cells[i] = []string{
			m.avatarMark(row.Name),
			row.Name,
			row.stateLabel(),
			row.liveLabel(),
			strconv.Itoa(row.Open),
			stamp(row.Last, now, loc),
		}
	}
	return styledStatusRows(headers, cells, width, func(i int) lipgloss.Style {
		if i == m.contactCursor {
			return plainLine.Reverse(true)
		}
		switch rows[i].state() {
		case stateDead, stateUnresponsive:
			return plainLine.Faint(true)
		default:
			return plainLine
		}
	})
}

// activeContactMatches counts active contacts, and how many of them the
// current search keeps. An empty query keeps every active contact.
func (m *Model) activeContactMatches() (total, matches int) {
	q := strings.ToLower(m.listQuery())
	for _, c := range m.contacts {
		if c.state() != stateActive {
			continue
		}
		total++
		if q == "" || strings.Contains(strings.ToLower(c.Name), q) || strings.Contains(strings.ToLower(c.liveLabel()), q) {
			matches++
		}
	}
	return total, matches
}

// DeadContactNames lists dead contacts in contact order. release is the
// subset agentbus still lists, so a history-only name is only hidden.
func DeadContactNames(items []decide.Item, bus []Target) (hide, release []string) {
	for _, c := range buildContacts(items, bus, true) {
		if c.state() != stateDead {
			continue
		}
		hide = append(hide, c.Name)
		if c.OnBus {
			release = append(release, c.Name)
		}
	}
	return hide, release
}

// hiddenHub stores the hidden contact list on the hub, as SetAvatar does
// for an avatar rule. A local backend does not implement it.
type hiddenHub interface {
	SetHidden(names []string) (decide.DisplaySettings, error)
}

// confirmKey answers a hide-dead or release prompt.
func (m *Model) confirmKey(r rune) (bool, error) {
	switch r {
	case 'y', 'Y':
		kind := m.confirmKind
		name := m.confirmName
		m.confirmKind = confirmNone
		m.confirmName = ""
		switch kind {
		case confirmHideDead:
			m.hideDead()
		case confirmRelease:
			m.releaseContact(name)
		}
	case 'n', 'N', '\x1b':
		m.confirmKind = confirmNone
		m.confirmName = ""
		m.status = ""
	}
	return false, nil
}

// dropActiveHidden removes hidden names that are active again and stores
// that list. A failed write keeps the previous list.
func (m *Model) dropActiveHidden() {
	if len(m.hidden) == 0 {
		return
	}
	active := map[string]bool{}
	for _, c := range m.contacts {
		if c.state() == stateActive {
			active[c.Name] = true
		}
	}
	keep := make([]string, 0, len(m.hidden))
	dropped := false
	for _, name := range m.hidden {
		if active[name] {
			dropped = true
			continue
		}
		keep = append(keep, name)
	}
	if !dropped {
		return
	}
	if err := m.persistHidden(keep); err != nil {
		m.status = err.Error()
	}
}

// persistHidden stores names on the hub in hub mode, otherwise in this
// machine's config, and keeps the stored list.
func (m *Model) persistHidden(names []string) error {
	if hub, ok := m.backend.(hiddenHub); ok {
		settings, err := hub.SetHidden(names)
		if err != nil {
			return fmt.Errorf("save hidden contacts: %w", err)
		}
		eff, err := decide.MergeDisplay(m.localConfig, settings)
		if err != nil {
			return fmt.Errorf("merge hidden contacts: %w", err)
		}
		m.hidden = append([]string(nil), eff.Config.HiddenContacts...)
		return nil
	}
	stored, err := decide.SetHiddenContacts(names)
	if err != nil {
		return fmt.Errorf("save hidden contacts: %w", err)
	}
	m.hidden = append([]string(nil), stored...)
	return nil
}

// savedNote says where a hide was written.
func (m *Model) savedNote(action string) string {
	if _, ok := m.backend.(hiddenHub); !ok {
		return action
	}
	if _, local := m.localConfig.Keys["hiddenContacts"]; local {
		return action + " (saved on hub; a local hiddenContacts list overrides it here)"
	}
	return action + " (saved on hub)"
}

func (m *Model) hideSelected() {
	c, ok := m.selectedContact()
	if !ok {
		return
	}
	if c.state() == stateActive {
		m.status = fmt.Sprintf("%s is active and stays visible", c.Name)
		return
	}
	if m.hiddenSet()[c.Name] {
		m.status = fmt.Sprintf("%s is already hidden", c.Name)
		return
	}
	names := append(append([]string(nil), m.hidden...), c.Name)
	if err := m.persistHidden(names); err != nil {
		m.status = err.Error()
		return
	}
	m.status = m.savedNote("hid " + c.Name)
	m.rebuildContacts()
}

func (m *Model) deadToHide() []string {
	hide := m.hiddenSet()
	var names []string
	for _, c := range m.contacts {
		if c.state() == stateDead && !hide[c.Name] {
			names = append(names, c.Name)
		}
	}
	return names
}

func (m *Model) askHideDead() {
	if len(m.deadToHide()) == 0 {
		m.status = "no dead contacts"
		return
	}
	m.confirmKind = confirmHideDead
	m.status = "hide every dead contact? y/n"
}

func (m *Model) hideDead() {
	names := m.deadToHide()
	if len(names) == 0 {
		m.status = "no dead contacts"
		return
	}
	all := append(append([]string(nil), m.hidden...), names...)
	if err := m.persistHidden(all); err != nil {
		m.status = err.Error()
		return
	}
	m.status = m.savedNote("hid " + strings.Join(names, ", "))
	m.rebuildContacts()
}

func (m *Model) askRelease() {
	c, ok := m.selectedContact()
	if !ok {
		return
	}
	switch {
	case c.state() == stateActive:
		m.status = fmt.Sprintf("release refused: %s is active", c.Name)
	case c.state() == stateUnresponsive || c.state() == stateUnknown:
		m.status = fmt.Sprintf("release refused: %s is unresponsive", c.Name)
	case !c.OnBus:
		m.status = fmt.Sprintf("release refused: %s is not registered", c.Name)
	default:
		m.confirmKind = confirmRelease
		m.confirmName = c.Name
		m.status = fmt.Sprintf("release %s? y/n", c.Name)
	}
}

func (m *Model) releaseContact(name string) {
	var found contact
	ok := false
	for _, c := range m.contacts {
		if c.Name == name {
			found = c
			ok = true
			break
		}
	}
	if !ok || found.state() != stateDead || !found.OnBus {
		m.status = fmt.Sprintf("release refused: %s", name)
		return
	}
	timeout := m.busTimeout
	if timeout <= 0 {
		timeout = decide.DefaultBusListTimeout
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	out, err := m.busRunner()(ctx, []string{"release", name})
	if err != nil {
		m.status = fmt.Sprintf("release %s: %v", name, err)
		return
	}
	text := strings.TrimSpace(string(out))
	if text == "" {
		text = "ok"
	}
	m.status = fmt.Sprintf("release %s: %s", name, text)
}
