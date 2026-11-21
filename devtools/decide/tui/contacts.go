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
	confirmBusDown
	confirmRoute
	confirmQuit
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
	// Aka are the ledger names that resolved to Name, other than Name.
	Aka []string
	// When is the bus name's --when text. LiveAge is how long before the
	// bus list agentbus observed Liveness; nil when unknown.
	When    string
	LiveAge *time.Duration
	// Answered, Done, and Withdrawn count items in those states.
	Answered, Done, Withdrawn int
	// Waiting and Failed count messages to this agent whose latest
	// delivery is waiting or failed.
	Waiting, Failed int
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
	m.replySend = false
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
	m.contacts = buildContacts(m.allItems, m.busNames, m.busKnown || m.busNote != "", m.aliases)
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

// visibleContacts is the contact list, in tree order while the org
// tree is shown.
func (m *Model) visibleContacts() []contact {
	rows := m.filteredContacts()
	if m.orgActive() {
		rows, _ = orgLayout(m.orgRoot, rows)
	}
	return rows
}

// filteredContacts is the contact list in contact order. Hidden names
// stay out unless H is showing them. The / query filters while
// Contacts is open.
func (m *Model) filteredContacts() []contact {
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
		if c.matchesQuery(q) {
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
// Ledger names resolve to a canonical agent before the rows merge.
// A name only the ledger has is offline once a bus list has succeeded
// or failed. Before that answer it is not marked. Rows sort by state
// (active, then not-yet-known, then unresponsive, then dead). Within a
// state, open items sort first, then the newest activity, then the name.
func buildContacts(items []decide.Item, bus []Target, busKnown bool, aliases map[string]string) []contact {
	set := contactSet{byName: map[string]*contact{}}
	set.addBus(bus)
	set.addItems(items, newResolver(bus, aliases))
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
		c.When = tg.When
		c.LiveAge = tg.LiveAge
	}
}

func (s *contactSet) addItems(items []decide.Item, res nameResolver) {
	for i := range items {
		s.addItem(items[i], res)
	}
}

func (s *contactSet) addItem(it decide.Item, res nameResolver) {
	seen := map[string]bool{}
	tally := itemTally{last: itemActivity(it), open: it.State == decide.StateOpen && !it.Snoozed(), state: it.State}
	s.note(it.Owner, res.canonical(it.Owner, &it), tally, seen)
	for _, msg := range it.Messages {
		name := msg.Sender()
		s.note(name, res.canonical(name, nil), tally, seen)
	}
	if bus, ok := res.notifyBus(it.NotifyPane); ok {
		s.note("", bus, tally, seen)
	}
	s.addDeliveries(&it, res)
}

// itemTally is what one item adds to each contact it involves.
type itemTally struct {
	last  time.Time
	open  bool
	state string
}

func (s *contactSet) note(raw, canon string, tally itemTally, seen map[string]bool) {
	if canon == "" {
		return
	}
	c := s.add(canon)
	if raw != "" && raw != canon {
		c.addAka(raw)
	}
	if seen[canon] {
		return
	}
	seen[canon] = true
	if tally.open {
		c.Open++
	}
	switch tally.state {
	case decide.StateAnswered:
		c.Answered++
	case decide.StateDone:
		c.Done++
	case decide.StateWithdrawn:
		c.Withdrawn++
	}
	if tally.last.After(c.Last) {
		c.Last = tally.last
	}
}

// addDeliveries counts the item's human messages whose latest delivery
// is waiting or failed, under the agent the delivery went to.
func (s *contactSet) addDeliveries(it *decide.Item, res nameResolver) {
	for _, msg := range it.Messages {
		d, ok := decide.MessageDelivery(it, msg)
		if !ok {
			continue
		}
		state := d.State()
		if state != decide.OutcomeWaiting && state != decide.OutcomeFailed {
			continue
		}
		name, ok := res.notifyBus(d.Target)
		if !ok {
			name = res.canonical(d.Target, it)
		}
		c := s.add(name)
		if c == nil {
			continue
		}
		if state == decide.OutcomeWaiting {
			c.Waiting++
		} else {
			c.Failed++
		}
	}
}

func (c *contact) addAka(name string) {
	if name == "" || name == c.Name || slices.Contains(c.Aka, name) {
		return
	}
	c.Aka = append(c.Aka, name)
}

// matchesQuery reports whether q, already lower-case, is in the name,
// an aka, or the liveness label. An empty q matches.
func (c contact) matchesQuery(q string) bool {
	if q == "" {
		return true
	}
	if strings.Contains(strings.ToLower(c.Name), q) || strings.Contains(strings.ToLower(c.liveLabel()), q) {
		return true
	}
	return strings.Contains(strings.ToLower(strings.Join(c.Aka, " ")), q)
}

func (s *contactSet) list(busKnown bool) []contact {
	out := make([]contact, 0, len(s.order))
	for _, name := range s.order {
		c := *s.byName[name]
		slices.Sort(c.Aka)
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

// agentKeeps reports whether it belongs on screen under the agent
// filter. A merged contact matches every name that resolved to it.
func (m *Model) agentKeeps(it decide.Item) bool {
	if m.agentFilter == "" {
		return true
	}
	if itemInvolves(it, m.agentFilter) {
		return true
	}
	res := newResolver(m.busNames, m.aliases)
	if res.canonical(it.Owner, &it) == m.agentFilter {
		return true
	}
	for _, msg := range it.Messages {
		if res.canonical(msg.Sender(), nil) == m.agentFilter {
			return true
		}
	}
	if bus, ok := res.notifyBus(it.NotifyPane); ok && bus == m.agentFilter {
		return true
	}
	return false
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

// composeFromContact opens the compose view on the selected contact.
// The recipient is the canonical name, and the cursor starts in the
// text box. A dead name the bus does not list is still selected, and
// selectTarget keeps its dead-target warning.
func (m *Model) composeFromContact() {
	c, ok := m.selectedContact()
	if !ok {
		return
	}
	m.ensureComposeDead(c)
	m.focusCompose()
	if m.selectName(c.Name) {
		m.selectTarget(m.target)
	}
	m.advanceCompose()
}

// ensureComposeDead adds a dead candidate for a contact the bus does
// not list, so compose can select that name. A name already offered,
// or one the bus has not yet called dead, is left alone.
func (m *Model) ensureComposeDead(c contact) {
	if c.Name == "" || c.state() != stateDead {
		return
	}
	if slices.ContainsFunc(m.candidates(), func(tg Target) bool { return tg.Name == c.Name }) {
		return
	}
	m.composeDead = append(m.composeDead, Target{
		Name: c.Name, Bus: true, Dead: true, Live: "dead",
	})
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
	limit := m.contactListLimit(m.contactsWidth())
	row := m.contactCursor
	if m.orgActive() {
		row = m.orgCursorRow()
	}
	// The contact table starts with a header row, after any note.
	if m.orgNote != "" {
		row++
	}
	m.scroll = revealRow(m.scroll, row+1, limit)
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

// contactsWidth is the width contactsScreen gets, as contactsMail lays
// it out; 0 when the width is unknown.
func (m *Model) contactsWidth() int {
	if m.width <= 0 {
		return 0
	}
	pane := m.contentWidth()
	if m.narrow() {
		return pane
	}
	_, mainW := listColumnWidths(maxWidth(m.sidebarLines()), pane)
	return mainW
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
	if m.aliasPick {
		return m.aliasPickScreen(width)
	}
	rows := m.visibleContacts()
	side, below := m.contactPanelPlace(width)
	listW := width
	if side {
		listW = width - contactPanelWidth - 1
	}
	var lines []screenLine
	if m.orgNote != "" {
		useStyles()
		lines = append(lines, screenLine{text: dimNav.Render(m.orgNote)})
	}
	if len(rows) == 0 {
		lines = append(lines, screenLine{text: m.emptyText("no contacts")})
	} else {
		useStyles()
		lines = append(lines, m.contactTable(listW, m.now(), rows)...)
	}
	limit := m.contactListLimit(width)
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	lines = m.withFilterHeader(lines)
	switch {
	case side:
		panel, _ := clipLines(m.contactPanel(contactPanelWidth), 0, limit)
		lines = zipFixed([][]screenLine{lines, panel}, []int{listW, contactPanelWidth})
	case below:
		lines = append(append(lines, screenLine{}), m.contactPanel(width)...)
	}
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

// contactName is the row label, including a plain aka suffix. The row
// style paints the whole line; a pre-rendered suffix would reset it.
func (m *Model) contactName(row contact) string {
	if len(row.Aka) == 0 {
		return row.Name
	}
	return row.Name + " " + akaSuffix(row)
}

// akaSuffix is the other names a merged row shows.
func akaSuffix(row contact) string {
	return "aka " + strings.Join(row.Aka, ", ")
}

// contactCell is the name column. dimAka is set only for a row with no
// style of its own. A reverse or faint row must not contain a reset,
// or the highlight stops at the aka suffix.
func (m *Model) contactCell(row contact, dimAka bool) string {
	if len(row.Aka) == 0 || !dimAka {
		return m.contactName(row)
	}
	useStyles()
	return row.Name + " " + dimNav.Render(akaSuffix(row))
}

// starredCell is the name column with the favorite mark.
func (m *Model) starredCell(row contact, dimAka bool) string {
	if m.isFavorite(row.Name) {
		return favoriteMark + " " + m.contactCell(row, dimAka)
	}
	return m.contactCell(row, dimAka)
}

// contactRowStyle is the highlight for contact row i. dimAka is true
// when that highlight does not already style the whole line.
func (m *Model) contactRowStyle(row contact, i int) (style lipgloss.Style, dimAka bool) {
	useStyles()
	if i == m.contactCursor {
		return plainLine.Reverse(true), false
	}
	switch row.state() {
	case stateDead, stateUnresponsive:
		return plainLine.Faint(true), false
	default:
		return plainLine, true
	}
}

func (m *Model) contactTable(width int, now time.Time, rows []contact) []screenLine {
	headers := []string{"name", "state", "liveness", "open", "activity"}
	loc := m.zone()
	cell := func(row contact, i int) ([]string, lipgloss.Style) {
		style, dimAka := m.contactRowStyle(row, i)
		return []string{
			m.starredCell(row, dimAka),
			row.stateLabel(),
			row.liveLabel(),
			strconv.Itoa(row.Open),
			stamp(row.Last, now, loc),
		}, style
	}
	var cells [][]string
	var styles []lipgloss.Style
	if m.orgActive() {
		_, tree := orgLayout(m.orgRoot, m.filteredContacts())
		for _, r := range tree {
			if r.contact < 0 {
				cells = append(cells, []string{r.prefix + r.label, "", "", "", ""})
				styles = append(styles, dimNav)
				continue
			}
			c, style := cell(rows[r.contact], r.contact)
			c[0] = r.prefix + c[0]
			cells = append(cells, c)
			styles = append(styles, style)
		}
	} else {
		for i, row := range rows {
			c, style := cell(row, i)
			cells = append(cells, c)
			styles = append(styles, style)
		}
	}
	return styledStatusRows(headers, cells, width, func(i int) lipgloss.Style {
		return styles[i]
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
		if c.matchesQuery(q) {
			matches++
		}
	}
	return total, matches
}

// DeadContactNames lists dead contacts in contact order. release is the
// subset agentbus still lists, so a history-only name is only hidden.
func DeadContactNames(items []decide.Item, bus []Target, aliases map[string]string) (hide, release []string) {
	for _, c := range buildContacts(items, bus, true, aliases) {
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

// confirmKey answers a hide-dead or release prompt, and hands the
// bus-down prompt to busDownKey and the reply route prompt to routeKey.
func (m *Model) confirmKey(r rune) (bool, error) {
	switch m.confirmKind {
	case confirmBusDown:
		return false, m.busDownKey(r)
	case confirmRoute:
		return false, m.routeKey(r)
	}
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
		case confirmQuit:
			return true, nil
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
	m.status = "hide every dead contact? " + buttonLabel("Yes", "y", false, 0) + " " + buttonLabel("No", "n", false, 0)
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
		m.status = fmt.Sprintf("release %s? %s %s", c.Name, buttonLabel("Yes", "y", false, 0), buttonLabel("No", "n", false, 0))
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
