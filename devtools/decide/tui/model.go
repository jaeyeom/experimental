// Package tui is the keyboard inbox for decide.
//
// decide inbox is a bubbletea program. The list, folders, and detail view are
// lipgloss. Every compose box (reply, free text, outcome, and the note
// sent to a configured pane) is a bubbles textarea with one shared Emacs
// keymap. No pane id is built in.
package tui

import (
	"errors"
	"fmt"
	"io"
	"os"
	"regexp"
	"slices"
	"strings"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
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

// snoozedFolder is the index of Snoozed in Folders.
const snoozedFolder = 1

// narrowWidth is the smallest measured terminal that keeps the sidebar column.
const narrowWidth = 80

// Folders is the inbox order: open, snoozed, answered, done, withdrawn.
// Snoozed lists snoozed items in any state. The other folders hide them.
var Folders = []struct {
	Name    string
	State   string
	Snoozed bool
}{
	{Name: "Inbox", State: decide.StateOpen},
	{Name: "Snoozed", Snoozed: true},
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
	reply        string
	replyFocus   bool
	detail       *decide.Item
	height       int
	width        int
	counts       []int
	scroll       int
	message      int
	rows         [][]hit
	target       int
	composing    bool
	composeFocus int
	chord        bool
	chordSeq     int
	editPrefix   bool
	editor       *externalEdit
	suspend      func()
	resume       func()
	help         bool
	answerFocus  bool
	note         string
	forward      string
	runSend      func([]string) error
	runOpen      OpenRunner
	copyClip     func(string) error
	out          io.Writer
	keyLog       io.Writer
	linkPick     bool
	// avatarPick is the avatar picker for avatarOwner. avatarCursor is
	// the entry under the cursor and avatarTop the first row shown.
	avatarPick   bool
	avatarOwner  string
	avatarCursor int
	avatarTop    int
	linkURL      string
	mouseOn      bool
	mouseSync    bool
	interval     time.Duration
	runErr       error
	replyField   textarea.Model
	free         textarea.Model
	outcome      textarea.Model
	noteField    textarea.Model
	agents       []decide.AgentStatus
	resources    []decide.ResourceStatus
	agentCursor  int
	searching    bool
	search       string
	filter       string
	listTotal    int
	statusTotal  int
	highlight    string
	// prior is the state from before /. folderItems and folderText are
	// the unfiltered folder and its lowercase search text, so typing in
	// the prompt filters without reading the ledger. A large folder
	// defers that pass: searchPending marks a query not yet applied,
	// searchAt is its last keystroke, and searchArmed asks Update for a
	// tick.
	prior         searchSnapshot
	folderItems   []decide.Item
	folderText    []string
	searchPending bool
	searchArmed   bool
	searchAt      time.Time
	// newMessages counts messages a live refresh added below the view.
	newMessages int
	detailGone  bool
	// busOn adds agentbus names to the recipients. busNames is the last
	// good listing; the load started at busAt runs in the background
	// while busLoading, bounded by busTimeout. busNote is the failure
	// note last shown. busRun replaces decide.ExecAgentbus.
	busOn      bool
	busRun     BusRunner
	busNames   []Target
	busAt      time.Time
	busLoading bool
	busTimeout time.Duration
	busNote    string
	pickFilter string
	// snoozing shows the snooze popup for snoozeItem. snoozeCustom is
	// the custom row's text and snoozeErr its parse error.
	snoozing     bool
	snoozeItem   decide.Item
	snoozeCursor int
	snoozeCustom string
	snoozeErr    string
	// Avatars, AvatarDefault, and AvatarMono come from decide config.
	// avatarProfile is the terminal's color profile. avatarRender
	// replaces the shared renderer. avatarFailed holds files that did
	// not render.
	Avatars       []decide.AvatarRule
	AvatarDefault bool
	AvatarMono    string
	avatarProfile termenv.Profile
	avatarRender  *avatar.Renderer
	avatarFailed  map[string]bool
	// Checkouts map status resources to lease-guarded checkouts.
	Checkouts []decide.ConfigCheckout
	// Machine is this machine's herdr label. locations caches each
	// item's owner location for the session. locateWant asks Update to
	// look up the open item's owner.
	Machine    string
	locations  map[string]decide.OwnerLocation
	locateWant bool
	// focusWant asks Update to focus this owner location's tab.
	focusWant *decide.OwnerLocation
	// linkCursor is the link picker's row and linkDigits the number
	// typed there so far. openWant asks Update to open
	// a URL with OpenCommand, or the platform opener when it is empty.
	linkCursor  int
	linkDigits  string
	openWant    string
	OpenCommand []string
	// NoHyperlinks drops the OSC 8 hyperlinks around URLs.
	NoHyperlinks bool
	// backend replaces the local ledger in Dir, such as a hub's.
	// asyncReload reloads it off the update loop. hubDown marks a
	// reload that found the hub unreachable.
	backend     Backend
	asyncReload bool
	hubDown     bool
}

// New returns an inbox positioned on the open folder.
func New(dir string) *Model {
	return &Model{
		Dir:        dir,
		Now:        time.Now,
		mouseOn:    true,
		replyField: newComposerField(),
		free:       newComposerField(),
		outcome:    newComposerField(),
		noteField:  newComposerField(),
	}
}

// Reload reads the current folder from disk. The cursor stays on the same id
// when that id is still present.
func (m *Model) Reload() error {
	all, err := m.store().List()
	if err != nil {
		return m.reloadFailed(err)
	}
	return m.applyList(all)
}

// reloadFailed keeps the view when the hub is unreachable and says so.
// Any other error ends the inbox.
func (m *Model) reloadFailed(err error) error {
	if errors.Is(err, ErrUnreachable) {
		m.hubDown = true
		m.status = unreachableStatus
		return nil
	}
	return err
}

// applyList shows a fresh ledger read: the open item, the folder
// counts, and the current folder.
func (m *Model) applyList(all []decide.Item) error {
	if m.hubDown {
		m.hubDown = false
		m.status = "hub reconnected"
	}
	keep := m.cursorID()
	m.refreshDetail(all)
	m.counts = make([]int, len(Folders))
	for _, it := range all {
		for i, folder := range Folders {
			if folder.Snoozed == it.Snoozed() && (folder.Snoozed || it.State == folder.State) {
				m.counts[i]++
			}
		}
	}
	if Folders[m.folder].Snoozed {
		m.setFolder(decide.SelectSnoozed(all), keep)
		return nil
	}
	items, err := decide.Select(all, Folders[m.folder].State)
	if err != nil {
		return fmt.Errorf("select decide folder: %w", err)
	}
	m.setFolder(items, keep)
	return nil
}

// Key applies one key. In the list, j and k move one row and keep that row
// visible. G jumps to the last row. g starts a folder chord, and a second g
// jumps to the first row. o or Enter opens the row, c opens compose, b
// snoozes the row, and q quits. A second key of i, z, a, d, w, or s opens
// Inbox, Snoozed, Answered, Done, Withdrawn, or Status. Status is also a row in the folder list. There, j
// and k highlight an agent row and keep it visible, and c opens compose.
// In the opened item, up, down, j, and k
// scroll one line, page up and page down scroll one page, home and end
// jump to the ends, and G jumps to the end. n and p move between messages. u returns to the
// list, and q quits. b snoozes the item. 1 through 9 answer a relay
// option. 0 focuses the composer.
// Sending it appends a human message. On an open question that text is also
// the answer. In a focused composer, ctrl+x then ctrl+e, or ctrl+g alone,
// opens that draft in an external editor. e or y archives a notification,
// in the list or the item, and d asks for an outcome then marks the item
// done. l opens a link, L copies
// its URL, and Y copies the
// current message. M toggles mouse capture. Enter submits the composer only
// while it is focused. Esc leaves it and keeps the unsent draft. In compose,
// Tab moves from the recipient
// to the text to [Send], Enter inserts a newline in the text, and Enter on
// [Send] sends. Printable keys type into a focused field, and the
// composerKeys Emacs bindings edit it; '\n' (C-j) inserts a newline and
// only '\r' (Enter) submits. Ctrl+Enter, Alt+Enter and C-s send from any focused
// composer; Update handles them before Key. ? opens the
// shortcut overlay only when no text field is focused, and Esc or ? closes
// it. / opens a search prompt that filters a list or highlights matches in
// an open item as you type. Enter keeps the result, an empty Enter clears
// it, and Esc restores the state from before /.
func (m *Model) Key(r rune) (bool, error) {
	if m.chord {
		return m.finishChord(r)
	}
	if m.snoozing {
		return m.snoozeKey(r)
	}
	if m.searching {
		return m.searchKey(r)
	}
	if m.help {
		return m.helpKey(r)
	}
	if m.pickerOpen() {
		return m.popupKey(r)
	}
	if m.answerFocus {
		return m.answerKey(r)
	}
	if m.replyFocus {
		return m.replyKey(r)
	}
	if m.mode == modeOutcome {
		return m.outcomeKey(r)
	}
	if m.composing {
		return m.composeKey(r)
	}
	if r == 'M' {
		m.toggleMouse()
		return false, nil
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
		m.moveAgent(1)
	case 'k':
		m.moveAgent(-1)
	case 'G':
		m.jumpList(m.listLen() - 1)
	case 'c':
		m.composeFromStatus()
	case '/':
		m.beginSearch()
	}
	return false, nil
}

func (m *Model) moveAgent(delta int) {
	n := len(m.agents)
	if n == 0 {
		return
	}
	next := m.agentCursor + delta
	if next < 0 {
		next = 0
	}
	if next >= n {
		next = n - 1
	}
	m.agentCursor = next
	m.revealAgentCursor()
}

func (m *Model) listLen() int {
	if m.mode == modeStatus {
		return len(m.agents)
	}
	return len(m.items)
}

func (m *Model) moveList(delta int) {
	if m.mode == modeStatus {
		m.moveAgent(delta)
		return
	}
	n := len(m.items)
	if n == 0 {
		return
	}
	next := m.cursor + delta
	if next < 0 {
		next = 0
	}
	if next >= n {
		next = n - 1
	}
	m.cursor = next
	m.revealListCursor()
}

func (m *Model) jumpList(index int) {
	n := m.listLen()
	if n == 0 {
		return
	}
	if index < 0 {
		index = 0
	}
	if index >= n {
		index = n - 1
	}
	if m.mode == modeStatus {
		m.agentCursor = index
		m.revealAgentCursor()
		return
	}
	m.cursor = index
	m.revealListCursor()
}

func (m *Model) revealListCursor() {
	n := len(m.items)
	if n == 0 {
		m.scroll = 0
		return
	}
	if m.cursor < 0 {
		m.cursor = 0
	}
	if m.cursor >= n {
		m.cursor = n - 1
	}
	limit, _ := m.layoutBudget()
	m.scroll = revealRow(m.scroll, m.cursor, limit)
}

func (m *Model) revealAgentCursor() {
	if len(m.agents) == 0 {
		return
	}
	m.clampAgentCursor()
	_, limit := m.layoutBudget()
	// The agent table starts with a header row.
	m.scroll = revealRow(m.scroll, m.agentCursor+1, limit)
}

func revealRow(scroll, row, limit int) int {
	if limit <= 0 {
		return scroll
	}
	if row < scroll {
		return row
	}
	if row >= scroll+limit {
		return row - limit + 1
	}
	return scroll
}

func (m *Model) listPage() int {
	var n int
	if m.mode == modeStatus {
		_, n = m.layoutBudget()
	} else {
		n, _ = m.layoutBudget()
	}
	if n < 1 {
		return 1
	}
	return n
}

func (m *Model) composeFromStatus() {
	m.focusCompose()
	if m.agentCursor >= 0 && m.agentCursor < len(m.agents) {
		m.selectName(m.agents[m.agentCursor].Name)
	}
}

func (m *Model) showStatus() error {
	m.composing = false
	m.answerFocus = false
	m.replyFocus = false
	m.mode = modeStatus
	m.scroll = 0
	m.agentCursor = 0
	if err := m.loadStatus(); err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "status"
	return nil
}

func (m *Model) loadStatus() error {
	board, err := m.store().Status()
	if err != nil {
		return err //nolint:wrapcheck // each backend names what failed
	}
	m.statusTotal = len(board.Agents) + len(board.Resources)
	m.agents, m.resources = filterStatus(board.Agents, board.Resources, m.filter)
	m.clampAgentCursor()
	return nil
}

func (m *Model) clampAgentCursor() {
	if len(m.agents) == 0 || m.agentCursor < 0 {
		m.agentCursor = 0
		return
	}
	if m.agentCursor >= len(m.agents) {
		m.agentCursor = len(m.agents) - 1
	}
}

func (m *Model) beginChord() {
	m.chord = true
	m.chordSeq++
}

// goToChord is one second key after g. The table drives finishChord, the
// footer menu, and the ? help line, so the three cannot drift.
type goToChord struct {
	key  rune
	name string
	run  func(m *Model) error
}

var goToChords = []goToChord{
	{'i', "inbox", func(m *Model) error { return m.showFolder(0) }},
	{'z', "snoozed", func(m *Model) error { return m.showFolder(snoozedFolder) }},
	{'a', "answered", func(m *Model) error { return m.showFolder(2) }},
	{'d', "done", func(m *Model) error { return m.showFolder(3) }},
	{'w', "withdrawn", func(m *Model) error { return m.showFolder(4) }},
	{'s', "status", func(m *Model) error { return m.showStatus() }},
	{'g', "top", func(m *Model) error {
		if m.mode == modeList || m.mode == modeStatus {
			m.jumpList(0)
		}
		return nil
	}},
}

// goToMenu is the footer while a g chord is pending. When the full menu is
// wider than width, it keeps only the keys. A width of 0 means unmeasured.
func goToMenu(width int) string {
	names := make([]string, len(goToChords))
	keys := make([]string, len(goToChords))
	for i, c := range goToChords {
		keys[i] = string(c.key)
		names[i] = keys[i] + " " + c.name
	}
	wide := "go to: " + strings.Join(names, " · ") + " · esc cancel"
	if width <= 0 || lipgloss.Width(wide) <= width {
		return wide
	}
	return "go to: " + strings.Join(keys, " ")
}

// goToHelp is the ? help line for the g chords.
func goToHelp() string {
	names := make([]string, len(goToChords))
	for i, c := range goToChords {
		names[i] = string(c.key) + " " + c.name
	}
	return "g then " + strings.Join(names, ", ")
}

func (m *Model) finishChord(r rune) (bool, error) {
	m.chord = false
	if r == '\x1b' {
		return false, nil
	}
	for _, c := range goToChords {
		if c.key == r {
			return false, c.run(m)
		}
	}
	m.status = "unknown go-to key"
	return false, nil
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
		m.moveList(1)
	case 'k':
		m.moveList(-1)
	case 'G':
		m.jumpList(m.listLen() - 1)
	case 'g':
		m.beginChord()
	case 'o', '\r', '\n':
		if err := m.openRow(); err != nil {
			return false, err
		}
	case 'c':
		m.focusCompose()
	case 'e', 'y':
		return false, m.archiveSelected()
	case 'b':
		m.beginSnooze()
	case '/':
		m.beginSearch()
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
	case 'q':
		return true, nil
	case '\x1b', 'u':
		m.backToList()
		return false, nil
	case 'j':
		m.Wheel(true)
		return false, nil
	case 'k':
		m.Wheel(false)
		return false, nil
	case 'n':
		m.moveMessage(1)
		return false, nil
	case 'p':
		m.moveMessage(-1)
		return false, nil
	case 'g':
		m.beginChord()
		return false, nil
	case 'e', 'y':
		return false, m.archiveSelected()
	case 'b':
		m.beginSnooze()
		return false, nil
	case 'l':
		return false, m.openSelectedLink()
	case '/':
		m.beginSearch()
		return false, nil
	case 'G':
		m.scrollEnd()
		return false, nil
	}
	if m.detailOutside(r) || m.detailComposer(r) {
		return false, nil
	}
	return m.detailText(r)
}

// detailOutside runs an item-view key that acts outside the inbox:
// Y copies the message, L the link, o jumps to the owner's tab, and A
// picks the owner's avatar, saved to config.
func (m *Model) detailOutside(r rune) bool {
	switch r {
	case 'A':
		m.beginAvatarPick()
	case 'Y':
		m.copyCurrentMessage()
	case 'L':
		m.copyCurrentLink()
	case 'o':
		m.jumpToOwner()
	default:
		return false
	}
	return true
}

// detailComposer opens the composer an item-view key names: c compose,
// f forward, d outcome, or 0 reply.
func (m *Model) detailComposer(r rune) bool {
	switch r {
	case 'c':
		m.focusCompose()
	case 'f':
		m.forwardItem()
	case 'd':
		m.beginOutcome()
	case '0':
		m.focusReply()
	default:
		return false
	}
	return true
}

// detailText answers a numbered option. Any other printable key is unbound.
func (m *Model) detailText(r rune) (bool, error) {
	if r >= '1' && r <= '9' {
		return false, m.answerOption(int(r - '1'))
	}
	if r >= ' ' {
		m.status = "unknown key, ? for help"
	}
	return false, nil
}

// answerKey edits the free-text row. Esc leaves the box and keeps the draft.
func (m *Model) answerKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.answerFocus = false
		m.free.Blur()
		return false, nil
	case '\r':
		return false, m.submitFreeText()
	}
	m.typeRune(r)
	return false, nil
}

// typeRune edits the focused composer with a printable rune, Backspace,
// or '\n' (C-j), which inserts a newline. Other control runes are ignored.
func (m *Model) typeRune(r rune) {
	if r >= ' ' || r == '\n' || r == '\b' || r == '\x7f' {
		m.editComposerKey(runeKey(r))
	}
}

func (m *Model) pullNote() {
	syncField(&m.noteField, m.note)
}

// focusCompose opens the recipient picker with no filter. It shows the
// bus names loaded so far and never waits for agentbus.
func (m *Model) focusCompose() {
	m.pickFilter = ""
	m.composing = true
	m.composeFocus = focusRecipient
	m.answerFocus = false
	m.free.Blur()
	m.pullNote()
	m.noteField.Blur()
}

// blurCompose leaves the compose box and keeps the draft. A forward's
// draft is discarded with its prefill.
func (m *Model) blurCompose() {
	if m.forward != "" {
		m.clearNote()
		return
	}
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
	m.forward = ""
	m.composing = false
	m.composeFocus = focusRecipient
	m.noteField.Blur()
	m.noteField.Reset()
}

func (m *Model) focusReply() {
	m.replyFocus = true
	m.answerFocus = false
	m.free.Blur()
}

// replyKey edits the reply box. Esc leaves it and keeps the draft.
func (m *Model) replyKey(r rune) (bool, error) {
	switch r {
	case '\x1b':
		m.replyFocus = false
		return false, nil
	case '\r':
		return false, m.submitReply()
	}
	m.typeRune(r)
	return false, nil
}

func dropLastRune(s string) string {
	runes := []rune(s)
	if len(runes) == 0 {
		return ""
	}
	return string(runes[:len(runes)-1])
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
	case '\r':
		return false, m.submitOutcome()
	}
	m.typeRune(r)
	if r >= ' ' {
		m.status = "outcome"
	}
	return false, nil
}

// moveMessage selects the next newer message, or the previous older one.
// The selection stays put at either end. The header then sits at the top
// of the thread area.
func (m *Model) moveMessage(delta int) {
	if m.detail == nil || len(m.detail.Messages) == 0 {
		return
	}
	next := m.message + delta
	if next < 0 {
		next = 0
	}
	last := len(m.detail.Messages) - 1
	if next > last {
		next = last
	}
	m.message = next
	if next == last {
		m.newMessages = 0
	}
	m.scrollToMessage()
}

func (m *Model) scrollToMessage() {
	lines := m.detailLines(m.detailInnerWidth(m.contentWidth()))
	want := m.message + 1
	for i, line := range lines {
		if line.header == want {
			m.scroll = i
			return
		}
	}
}

func (m *Model) detailInnerWidth(width int) int {
	if (m.detailFocused() || m.answerFocus) && width >= 2 {
		return width - 2
	}
	return width
}

func (m *Model) openRow() error {
	if len(m.items) == 0 {
		return nil
	}
	id := m.items[m.cursor].ID
	updated, err := m.store().MarkRead(id)
	if err != nil {
		return m.writeFailed(err)
	}
	m.detail = &updated
	m.locateWant = true
	m.mode = modeDetail
	m.draft = ""
	m.reply = ""
	m.answerFocus = false
	m.replyFocus = false
	m.closeLinkPick()
	m.linkURL = ""
	m.resetFields()
	m.scroll = 0
	m.message = 0
	m.newMessages = 0
	m.detailGone = false
	m.status = "opened " + id
	return m.Reload()
}

// archiveSelected marks the selected notification done, the same result as
// decide done. A thread with pending options still needs an answer.
func (m *Model) archiveSelected() error {
	if m.folder < 0 || m.folder >= len(Folders) || Folders[m.folder].State != decide.StateOpen {
		return nil
	}
	it, ok := m.selectedItem()
	if !ok {
		return nil
	}
	if len(it.Options) > 0 {
		m.status = "needs an answer"
		return nil
	}
	if _, err := m.store().Done(it.ID, "archived"); err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "done " + it.ID
	if m.mode == modeDetail {
		m.backToList()
	}
	return m.Reload()
}

func (m *Model) selectedItem() (decide.Item, bool) {
	if m.mode == modeDetail && m.detail != nil {
		return *m.detail, true
	}
	if m.mode == modeList && m.cursor >= 0 && m.cursor < len(m.items) {
		return m.items[m.cursor], true
	}
	return decide.Item{}, false
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

func (m *Model) writeAnswer(value string) error {
	if m.detail == nil {
		return nil
	}
	id := m.detail.ID
	_, err := m.store().Answer(id, value)
	var notifyErr *NotifyError
	switch {
	case errors.As(err, &notifyErr):
		m.status = err.Error()
	case err != nil:
		m.status = err.Error()
		return nil
	default:
		m.status = "answered " + id
	}
	m.backToList()
	return m.Reload()
}

// submitReply appends a human message, the same result as decide send --id.
// The owner pane receives the thread header, the original text, and the reply hint.
func (m *Model) submitReply() error {
	if m.detail == nil {
		return nil
	}
	text := m.reply
	if strings.TrimSpace(text) == "" {
		return nil
	}
	updated, err := m.store().Reply(m.detail.ID, text)
	var notifyErr *NotifyError
	switch {
	case errors.As(err, &notifyErr):
		m.status = err.Error()
	case err != nil:
		m.status = err.Error()
		return nil
	default:
		m.status = "replied " + updated.ID
	}
	m.detail = &updated
	m.reply = ""
	m.replyFocus = false
	// The new message is last. clipLines clamps this onto that end.
	m.scroll = 1 << 20
	return m.Reload()
}

func (m *Model) submitOutcome() error {
	if m.detail == nil {
		return nil
	}
	id := m.detail.ID
	if _, err := m.store().Done(id, m.draft); err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "done " + id
	m.backToList()
	return m.Reload()
}

// store is the backend the inbox reads and writes: the one Run was
// given, or the local ledger in Dir.
func (m *Model) store() Backend {
	if m.backend != nil {
		return m.backend
	}
	return &LocalBackend{
		Dir:          m.Dir,
		Now:          m.Now,
		SendTemplate: m.SendTemplate,
		RunSend:      m.runSend,
		BusRun:       m.busRun,
		Checkouts:    m.Checkouts,
		Machine:      m.Machine,
	}
}

// writeFailed shows a write the hub could not take on the status line.
// Any other error ends the inbox.
func (m *Model) writeFailed(err error) error {
	if errors.Is(err, ErrUnreachable) {
		m.status = err.Error()
		return nil
	}
	return err
}

func (m *Model) backToList() {
	m.mode = modeList
	m.detail = nil
	m.highlight = ""
	m.draft = ""
	m.reply = ""
	m.answerFocus = false
	m.replyFocus = false
	m.closeLinkPick()
	m.linkURL = ""
	m.resetFields()
	m.scroll = 0
	m.message = 0
	m.newMessages = 0
	m.revealListCursor()
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
	// header is 1-based message index when this line is that message's header.
	header int
	// match reports that the line paints a search match.
	match bool
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
	if m.help || m.snoozing || m.linkPick || m.avatarPick {
		switch {
		case m.help:
			lines = m.helpLines()
		case m.snoozing:
			lines = m.snoozeLines()
		case m.avatarPick:
			lines = m.avatarPickLines()
		default:
			lines = m.linkPickLines()
		}
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
	// The mouse label stays in front. The list and status hints stay the same
	// width, so a 70-column footer still shows "? help". An open item also
	// names e/y archive, 0 reply, f forward, and q quit, and d outcome when it fits.
	// It leaves the folder keys to ? help so a 120-column footer still fits.
	if m.searching {
		return m.searchPrompt()
	}
	label := m.mouseLabel()
	if m.snoozing {
		return screenLine{text: label + snoozeHint}
	}
	if m.linkPick {
		if m.status != "" {
			return screenLine{text: m.status + "  " + label + linkPickHint}
		}
		return screenLine{text: label + linkPickHint}
	}
	if m.avatarPick {
		return screenLine{text: label + avatarPickHint}
	}
	if m.chord {
		return screenLine{text: goToMenu(m.contentWidth())}
	}
	status := m.footerStatus()
	if kind, ok := m.composerKind(); ok {
		hint := label + composerHint(kind)
		if status == "" {
			return screenLine{text: hint}
		}
		return screenLine{text: status + "  " + hint}
	}
	if m.composing && m.composeFocus == focusRecipient {
		hint := label + pickerHint
		if status == "" {
			return screenLine{text: hint}
		}
		return screenLine{text: status + "  " + hint}
	}
	hint := label + "g go to  o open  u list  c  tab/text/[Send]  ? help"
	switch m.mode {
	case modeStatus:
		hint = label + "g go to  u list  c  tab/text/[Send]  ? help"
	case modeDetail, modeOutcome:
		hint = label + "0 reply  f forward  u list  q quit  e/y archive  j/k pgup/pgdn home/end  n/p  ? help"
	}
	text := hint
	if status != "" {
		text = status + "  " + hint
	}
	return screenLine{text: m.withOptionalHints(text)}
}

// withOptionalHints adds d outcome in an open item, then /find, then
// b snooze, then g go to and o owner in an open item, before "? help"
// while each fits, so a narrow footer keeps "? help".
func (m *Model) withOptionalHints(text string) string {
	extras := []string{"/find", "b snooze"}
	switch m.mode {
	case modeStatus:
		extras = extras[:1]
	case modeDetail, modeOutcome:
		extras = append([]string{"d outcome"}, extras...)
		extras = append(extras, "g go to", "o owner")
	}
	for _, extra := range extras {
		wide := strings.TrimSuffix(text, "? help") + extra + "  ? help"
		if w := m.contentWidth(); w <= 0 || lipgloss.Width(wide) <= w {
			text = wide
		}
	}
	return text
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
		return m.statusMail(pane)
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

func (m *Model) statusMail(pane int) []screenLine {
	if m.width <= 0 {
		return stackRegions(0, m.sidebarLines(), m.statusScreen(0))
	}
	if m.narrow() {
		return stackRegions(pane, []screenLine{m.folderTabLine(pane)}, m.statusScreen(pane))
	}
	sidebar := m.sidebarLines()
	sideW, mainW := listColumnWidths(maxWidth(sidebar), pane)
	cols := [][]screenLine{
		fitColumnLines(sidebar, sideW),
		m.statusScreen(mainW),
	}
	return zipFixed(cols, []int{sideW, mainW})
}

func (m *Model) statusScreen(width int) []screenLine {
	now := time.Now()
	if m.Now != nil {
		now = m.Now()
	}
	var lines []screenLine
	if len(m.agents) == 0 && len(m.resources) == 0 {
		lines = append(lines, screenLine{text: m.emptyText("no status")})
	} else {
		useStyles()
		if len(m.agents) > 0 {
			lines = append(lines, m.agentTable(width, now)...)
		}
		if len(m.agents) > 0 && len(m.resources) > 0 {
			lines = append(lines, screenLine{})
		}
		if len(m.resources) > 0 {
			lines = append(lines, m.resourceTable(width, now)...)
		}
	}
	_, limit := m.layoutBudget()
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	lines = m.withFilterHeader(lines)
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

func (m *Model) agentTable(width int, now time.Time) []screenLine {
	headers := []string{"agent", "state", "age", "text"}
	rows := make([][]string, len(m.agents))
	for i, row := range m.agents {
		rows[i] = []string{row.Name, row.State, age(now.Sub(row.UpdatedAt)), row.Text}
	}
	return styledStatusRows(headers, rows, width, func(i int) lipgloss.Style {
		style := statusStyle(m.agents[i].UpdatedAt, now)
		if i == m.agentCursor {
			return style.Reverse(true)
		}
		return style
	})
}

func (m *Model) resourceTable(width int, now time.Time) []screenLine {
	headers := []string{"resource", "free/busy", "age", "note"}
	rows := make([][]string, len(m.resources))
	for i, row := range m.resources {
		rows[i] = []string{row.Name, row.Availability, age(now.Sub(row.UpdatedAt)), decide.ResourceNote(row)}
	}
	return styledStatusRows(headers, rows, width, func(i int) lipgloss.Style {
		return statusStyle(m.resources[i].UpdatedAt, now)
	})
}

func styledStatusRows(headers []string, rows [][]string, width int, style func(int) lipgloss.Style) []screenLine {
	formatted := alignColumns(headers, rows, width)
	lines := make([]screenLine, len(formatted))
	if len(formatted) == 0 {
		return nil
	}
	lines[0] = screenLine{text: formatted[0]}
	for i := 1; i < len(formatted); i++ {
		lines[i] = screenLine{text: style(i - 1).Render(formatted[i])}
	}
	return lines
}

func alignColumns(headers []string, rows [][]string, width int) []string {
	return formatAligned(headers, rows, columnWidths(headers, rows, width))
}

func columnWidths(headers []string, rows [][]string, width int) []int {
	n := len(headers)
	widths := make([]int, n)
	for i, header := range headers {
		widths[i] = lipgloss.Width(header)
		if widths[i] < 1 {
			widths[i] = 1
		}
	}
	for _, row := range rows {
		for i := 0; i < n-1 && i < len(row); i++ {
			if w := lipgloss.Width(row[i]); w > widths[i] {
				widths[i] = w
			}
		}
	}
	if n == 0 {
		return widths
	}
	if width <= 0 {
		widths[n-1] = lastColumnWidth(widths[n-1], rows, n-1)
		return widths
	}
	return fitColumnWidths(widths, width)
}

func lastColumnWidth(floor int, rows [][]string, col int) int {
	w := floor
	for _, row := range rows {
		if col < len(row) {
			if n := lipgloss.Width(row[col]); n > w {
				w = n
			}
		}
	}
	return w
}

func fitColumnWidths(widths []int, width int) []int {
	n := len(widths)
	gaps := n - 1
	fixed := 0
	for i := 0; i < n-1; i++ {
		fixed += widths[i]
	}
	remain := width - fixed - gaps
	if remain < 1 {
		widths = shrinkFixedColumns(widths, width)
		fixed = 0
		for i := 0; i < n-1; i++ {
			fixed += widths[i]
		}
		remain = width - fixed - gaps
		if remain < 1 {
			remain = 1
		}
	}
	widths[n-1] = remain
	return widths
}

func shrinkFixedColumns(widths []int, width int) []int {
	n := len(widths)
	gaps := n - 1
	fixed := 0
	for i := 0; i < n-1; i++ {
		fixed += widths[i]
	}
	overflow := fixed + gaps + 1 - width
	for i := n - 2; i >= 0 && overflow > 0; i-- {
		spare := widths[i] - 1
		if spare <= 0 {
			continue
		}
		if spare > overflow {
			widths[i] -= overflow
			return widths
		}
		overflow -= spare
		widths[i] = 1
	}
	return widths
}

func formatAligned(headers []string, rows [][]string, widths []int) []string {
	out := make([]string, 0, 1+len(rows))
	out = append(out, formatCells(headers, widths))
	for _, row := range rows {
		cells := make([]string, len(widths))
		for i := range cells {
			if i < len(row) {
				cells[i] = row[i]
			}
		}
		out = append(out, formatCells(cells, widths))
	}
	return out
}

func formatCells(cells []string, widths []int) string {
	var b strings.Builder
	for i, width := range widths {
		if i > 0 {
			b.WriteByte(' ')
		}
		text := ""
		if i < len(cells) {
			text = cells[i]
		}
		if lipgloss.Width(text) > width {
			text = truncateVisible(text, width)
		}
		b.WriteString(text)
		if pad := width - lipgloss.Width(text); pad > 0 {
			b.WriteString(strings.Repeat(" ", pad))
		}
	}
	return b.String()
}

func statusStyle(updated, now time.Time) lipgloss.Style {
	if decide.StatusStale(updated, now) {
		return staleLine
	}
	return plainLine
}

func (m *Model) unmeasuredList() []screenLine {
	sidebar := m.sidebarLines()
	list := m.withFilterHeader(m.clipList(m.rowLines(0)))
	if m.listFocused() {
		list = boxLines(list)
	}
	return stackRegions(0, sidebar, list)
}

func (m *Model) stackedList(pane int) []screenLine {
	rowW := pane
	if m.listFocused() && pane >= 2 {
		rowW = pane - 2
	}
	list := m.withFilterHeader(m.clipList(m.rowLines(rowW)))
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
	list := m.withFilterHeader(m.clipList(m.rowLines(listInner)))
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
	detail := m.detailLines(m.detailInnerWidth(width))
	_, detailN := m.layoutBudget()
	if threadEnd(len(detail), m.scroll, detailN) {
		m.newMessages = 0
	}
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

func (m *Model) navLen() int {
	return len(Folders) + 1
}

func (m *Model) navSelected(i int) bool {
	if m.mode == modeStatus {
		return i == len(Folders)
	}
	return i == m.folder
}

func (m *Model) navName(i int) string {
	name := "Status"
	if i < len(Folders) {
		name = Folders[i].Name
	}
	if m.navSelected(i) {
		return "[" + name + "]"
	}
	return name
}

func (m *Model) navCount(i int) int {
	if i < 0 || i >= len(m.counts) {
		return 0
	}
	return m.counts[i]
}

func (m *Model) navText(i int) string {
	name := m.navName(i)
	if i >= len(Folders) {
		return name
	}
	return fmt.Sprintf("%s %d", name, m.navCount(i))
}

func (m *Model) folderTabLine(width int) screenLine {
	var b strings.Builder
	hits := make([]hit, 0, m.navLen())
	for i := 0; i < m.navLen(); i++ {
		if i > 0 {
			b.WriteByte(' ')
		}
		label := m.navText(i)
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

// folderCountMin is wide enough for 1, 12, and 123 to end in one column.
const folderCountMin = 3

func (m *Model) sidebarLines() []screenLine {
	n := m.navLen()
	names := make([]string, n)
	nameW := 0
	countW := folderCountMin
	for i := 0; i < n; i++ {
		names[i] = m.navName(i)
		if w := lipgloss.Width(names[i]); w > nameW {
			nameW = w
		}
		if i >= len(Folders) {
			continue
		}
		if w := lipgloss.Width(fmt.Sprintf("%d", m.navCount(i))); w > countW {
			countW = w
		}
	}
	lines := make([]screenLine, n)
	for i := 0; i < n; i++ {
		count := ""
		if i < len(Folders) {
			count = fmt.Sprintf("%d", m.navCount(i))
		}
		lines[i] = screenLine{
			text: padRight(names[i], nameW) + " " + padLeft(count, countW),
			hits: []hit{{kind: hitFolder, n: i}},
		}
	}
	return lines
}

func padLeft(s string, width int) string {
	if w := lipgloss.Width(s); w < width {
		return strings.Repeat(" ", width-w) + s
	}
	return s
}

func padRight(s string, width int) string {
	if w := lipgloss.Width(s); w < width {
		return s + strings.Repeat(" ", width-w)
	}
	return s
}

func (m *Model) layoutBudget() (int, int) {
	if m.height <= 1 {
		return 0, 0
	}
	folderChrome := 0
	if !m.composing && (m.mode == modeList || m.mode == modeStatus) {
		folderChrome = m.navLen()
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
	if _, ok := m.filterHeader(); ok {
		chrome++
	}
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

// copyANSI returns one CSI or OSC sequence starting at i.
func copyANSI(text string, i int) (string, int, bool) {
	if i+1 >= len(text) || text[i] != 0x1b {
		return "", i, false
	}
	switch text[i+1] {
	case ']':
		return oscSequence(text, i)
	case '[':
		return csiSequence(text, i)
	default:
		return "", i, false
	}
}

func oscSequence(text string, i int) (string, int, bool) {
	j := i + 2
	for j < len(text) {
		if text[j] == 0x07 {
			return text[i : j+1], j + 1, true
		}
		if text[j] == 0x1b && j+1 < len(text) && text[j+1] == '\\' {
			return text[i : j+2], j + 2, true
		}
		j++
	}
	return text[i:], len(text), true
}

func csiSequence(text string, i int) (string, int, bool) {
	j := i + 2
	for j < len(text) && text[j] != 'm' {
		j++
	}
	if j < len(text) {
		j++
	}
	return text[i:j], j, true
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
		if seq, next, ok := copyANSI(text, i); ok {
			b.WriteString(seq)
			i = next
			continue
		}
		_, size := utf8.DecodeRuneInString(text[i:])
		if size <= 0 {
			break
		}
		// Past the limit, keep scanning so the escape sequences after
		// the cut still close, such as the end of an OSC 8 hyperlink.
		if vis < limit {
			b.WriteString(text[i : i+size])
			vis++
		}
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
	staleLine    lipgloss.Style
	plainLine    lipgloss.Style
	focusMark    lipgloss.Style
	optionButton lipgloss.Style
	searchMatch  lipgloss.Style
)

func useStyles() {
	styleOnce.Do(func() {
		// ANSI bold is stable in tests and on a real terminal.
		lipgloss.SetColorProfile(termenv.ANSI)
		unreadRow = lipgloss.NewStyle().Bold(true)
		// Detail text is plain. Stale status rows keep their own faint style.
		detailLine = lipgloss.NewStyle()
		staleLine = lipgloss.NewStyle().Faint(true)
		plainLine = lipgloss.NewStyle()
		focusMark = lipgloss.NewStyle().Bold(true)
		optionButton = lipgloss.NewStyle().Reverse(true)
		// Black on yellow reads the same on light and dark terminals.
		searchMatch = lipgloss.NewStyle().Foreground(lipgloss.ANSIColor(0)).Background(lipgloss.ANSIColor(3))
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
		"j/k move  pgup/pgdn  home/end  gg/G",
		goToHelp(),
		"o or enter opens    u returns to the list",
		"c compose",
		"tab moves recipient, text, [Send]",
		"shift+tab moves back",
		"typing filters recipients  up/down or C-n/C-p move  enter picks",
		"enter on text inserts a newline",
		"enter on [Send] sends",
		"esc keeps the draft",
		"1-9 answer  0 reply  f forward  e/y archive  d outcome  b snooze",
		"/ filters as you type, esc restores, empty enter clears",
		"l opens a link, or picks one: j/k or a number, enter opens",
		"L copies the link   Y copies the message   A picks the owner's avatar",
		"shift+drag selects text",
		"M toggles mouse capture",
		"esc back",
		"click folder, row, option, target, or [Send]",
		"wheel scroll",
		"? or esc close",
	}
	if m.mode == modeStatus {
		text = []string{
			"shortcuts",
			"q quit",
			"j/k move  pgup/pgdn  home/end  gg/G",
			goToHelp(),
			"u returns to the list",
			"c compose",
			"tab moves recipient, text, [Send]",
			"shift+tab moves back",
			"typing filters recipients  up/down or C-n/C-p move  enter picks",
			"enter on text inserts a newline",
			"enter on [Send] sends",
			"esc keeps the draft",
			"/ filters as you type, esc restores, empty enter clears",
			"shift+drag selects text",
			"M toggles mouse capture",
			"click folder, target, or [Send]",
			"wheel scroll",
			"? or esc close",
		}
	}
	if i := slices.Index(text, "esc keeps the draft"); i >= 0 {
		text = slices.Insert(text, i+1, composerHelp...)
	}
	if m.mode == modeDetail || m.mode == modeOutcome {
		for i, line := range text {
			if strings.HasPrefix(line, "j/k move") {
				text[i] = "j/k up/down line  pgup/pgdn page  home/end/G  n/p"
			}
			if strings.HasPrefix(line, "/ filters") {
				text[i] = "/ highlights as you type, esc restores, empty enter clears"
			}
		}
		if i := slices.Index(text, "o or enter opens    u returns to the list"); i >= 0 {
			text[i] = "o jumps to the owner's herdr tab    u returns to the list"
		}
	}
	lines := make([]screenLine, len(text))
	for i, line := range text {
		lines[i] = screenLine{text: plainText(line)}
	}
	return lines
}

func (m *Model) rowLines(width int) []screenLine {
	if len(m.items) == 0 {
		return []screenLine{{text: m.emptyText("(empty)")}}
	}
	now := time.Now()
	if m.Now != nil {
		now = m.Now()
	}
	cells := make([][]string, len(m.items))
	for i, it := range m.items {
		cells[i] = inboxCells(it, now)
	}
	widths := inboxColumnWidths(cells, width)
	styles := inboxStyles()
	re := searchPattern(m.filter)
	lines := make([]screenLine, 0, len(m.items))
	for i, it := range m.items {
		marker := "  "
		if i == m.cursor {
			marker = "> "
		}
		text := marker + rowMark(it) + paintInboxRow(cells[i], widths, styles, re)
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
	re := searchPattern(m.highlight)
	title := it.ID
	if re != nil {
		title += fmt.Sprintf("  /%s (%d)", m.highlight, highlightCount(it, re))
	}
	lines := []screenLine{{text: plainText(title)}}
	lines = append(lines, headerLines(it, m.now().Location(), decide.OwnerAt(it.Owner, m.ownerLocation(it)))...)
	if m.avatarFits(width) {
		// The owner is the other party: the asker, or the recipient of
		// a thread the human started.
		if av := m.avatarLines(it.Owner); av != nil {
			lines = besideAvatar(av, lines, width-avatarColumn)
		}
	}
	lines = append(lines, screenLine{})
	lines = append(lines, m.threadLines(it, width, re)...)
	if it.Kind == decide.KindTypeIn {
		lines = append(lines,
			screenLine{text: plainText("type: " + it.TypeText)},
			screenLine{text: plainText("where: " + it.Where)},
			screenLine{text: plainText("mark done"), hits: []hit{{kind: hitMarkDone}}},
		)
		lines = append(lines, m.replyLines(width)...)
	} else {
		lines = append(lines, m.replyLines(width)...)
		lines = append(lines, m.relayLines(it, width)...)
	}
	if m.mode == modeOutcome {
		const label = "outcome: "
		wrap := width - len(label)
		body := fieldLines(m.draft, &m.outcome, m.outcome.Focused(), wrap)
		lines = append(lines, labeledLines(label, body, hitOutcome, wrap)...)
	}
	return lines
}

// threadParties is the requester and, when the thread was addressed to
// someone else, that recipient. A human compose stores the recipient as
// the owner and the requester as the first message author.
func threadParties(it *decide.Item) (from, to string) {
	from = it.Owner
	if len(it.Messages) == 0 {
		return from, ""
	}
	author := it.Messages[0].Author
	if author == "" || author == it.Owner {
		return from, ""
	}
	return author, it.Owner
}

func headerTime(t time.Time) string {
	if t.IsZero() {
		return ""
	}
	return t.UTC().Format(time.RFC3339)
}

// headerLines is the metadata block above the item body. owner is the
// owner and where it lives. Labels share one column so the colons line up.
func headerLines(it *decide.Item, loc *time.Location, owner string) []screenLine {
	from, to := threadParties(it)
	rows := []struct{ label, value string }{{"From", from}}
	if to != "" {
		rows = append(rows, struct{ label, value string }{"To", to})
	}
	rows = append(rows,
		struct{ label, value string }{"Owner", owner},
		struct{ label, value string }{"Kind", it.Kind},
		struct{ label, value string }{"State", it.State},
		struct{ label, value string }{"Created", headerTime(it.CreatedAt)},
		struct{ label, value string }{"Answered", headerTime(it.AnsweredAt)},
	)
	if it.SnoozedUntil != nil {
		rows = append(rows, struct{ label, value string }{"Snoozed", "until " + decide.SnoozeLabel(it.SnoozedUntil.In(loc))})
	}
	width := 0
	for _, row := range rows {
		if n := lipgloss.Width(row.label); n > width {
			width = n
		}
	}
	labelStyle := headerLabelStyle()
	lines := make([]screenLine, 0, len(rows))
	for _, row := range rows {
		pad := width - lipgloss.Width(row.label)
		if pad < 0 {
			pad = 0
		}
		label := strings.Repeat(" ", pad) + row.label
		text := labelStyle.Render(label+":") + " " + row.value
		lines = append(lines, screenLine{text: text})
	}
	if it.ForwardedFrom != "" {
		lines = append(lines, screenLine{text: plainText("forwarded from " + it.ForwardedFrom)})
	}
	return lines
}

func headerLabelStyle() lipgloss.Style {
	return paletteStyle(loadColors().Muted, paletteMuted)
}

// threadLines shows each message as its own block, oldest first.
// A rule separates messages. The header is the author, a relative time,
// dim option keys when that message asked a question, and latest on the newest.
// The body is indented under that header.
func (m *Model) threadLines(it *decide.Item, width int, re *regexp.Regexp) []screenLine {
	if len(it.Messages) == 0 {
		return questionLines(it.Question, width, re, !m.NoHyperlinks)
	}
	now := time.Now()
	if m.Now != nil {
		now = m.Now()
	}
	ruleWidth := width
	if ruleWidth < 1 {
		ruleWidth = 8
	}
	bodyWidth := width
	if width > 2 {
		bodyWidth = width - 2
	}
	var lines []screenLine
	last := len(it.Messages) - 1
	for i, msg := range it.Messages {
		if i > 0 {
			lines = append(lines, screenLine{text: plainText(strings.Repeat("─", ruleWidth))})
		}
		lines = append(lines, screenLine{
			text:   messageHeader(msg, now, i == last, i == m.message),
			header: i + 1,
		})
		for _, line := range questionLines(msg.Text, bodyWidth, re, !m.NoHyperlinks) {
			lines = append(lines, indentLine(line))
		}
	}
	return lines
}

// messageHeader is one line: author, relative time, option keys, and latest.
// Human and agent authors use two theme colors. The time uses the time color.
func messageHeader(msg decide.Message, now time.Time, latest, current bool) string {
	colors := loadColors()
	r := paletteRenderer()
	authorStyle := paletteStyle(colors.Agent, paletteAgent)
	if msg.Author == "human" {
		authorStyle = paletteStyle(colors.Human, paletteHuman)
	}
	var parts []string
	if msg.Author != "" {
		parts = append(parts, authorStyle.Render(msg.Sender()))
	}
	if !msg.Time.IsZero() {
		parts = append(parts, paletteStyle(colors.Time, paletteTime).Render(age(now.Sub(msg.Time))))
	}
	if keys := optionKeys(msg.Options); keys != "" {
		parts = append(parts, paletteStyle(colors.Time, paletteTime).Render(keys))
	}
	if latest {
		parts = append(parts, "latest")
	}
	text := strings.Join(parts, "  ")
	if current {
		text = r.NewStyle().Reverse(true).Render(text)
	}
	return text
}

// optionKeys lists the option keys as dim plain text. Answers use 1-9, so
// the keys must not look like shortcuts.
func optionKeys(opts []decide.Option) string {
	if len(opts) == 0 {
		return ""
	}
	keys := make([]string, len(opts))
	for i, opt := range opts {
		keys[i] = opt.Key
	}
	return strings.Join(keys, " ")
}

func indentLine(line screenLine) screenLine {
	line.text = "  " + line.text
	for i := range line.hits {
		if line.hits[i].x0 > 0 {
			line.hits[i].x0 += 2
		}
		if line.hits[i].x1 > 0 {
			line.hits[i].x1 += 2
		}
	}
	return line
}

func (m *Model) replyLines(width int) []screenLine {
	const label = "reply: "
	wrap := width - len(label)
	body := []screenLine{{text: "___"}}
	if m.reply != "" || m.replyFocus {
		body = fieldLines(m.reply, &m.replyField, m.replyFocus, wrap)
	}
	return labeledLines(label, body, hitReply, wrap)
}

// labeledLines puts label before the first composer line and lines the
// rest up under it. Every line is a composer row of hit kind.
func labeledLines(label string, body []screenLine, kind, wrap int) []screenLine {
	pad := strings.Repeat(" ", len(label))
	out := make([]screenLine, len(body))
	for i, line := range body {
		prefix := pad
		if i == 0 {
			prefix = label
		}
		out[i] = screenLine{text: plainText(prefix + line.text)}
	}
	return fieldRows(out, kind, 0, len(label), wrap)
}

func plainText(s string) string {
	useStyles()
	return detailLine.Render(s)
}

var urlPattern = regexp.MustCompile(`https?://[^[:space:]]+`)

// questionLines wraps q. A non-nil re marks its matches. With links
// set, each URL carries an OSC 8 hyperlink.
func questionLines(q string, width int, re *regexp.Regexp, links bool) []screenLine {
	parts := strings.Split(q, "\n")
	lines := make([]screenLine, 0, len(parts))
	for _, part := range parts {
		lines = append(lines, linkLines(part, width, re, links)...)
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

func linkLines(part string, width int, re *regexp.Regexp, links bool) []screenLine {
	chunks := wrapIndexed(part, width)
	spans := urlPattern.FindAllStringIndex(part, -1)
	var matches [][]int
	if re != nil {
		matches = re.FindAllStringIndex(part, -1)
	}
	lines := make([]screenLine, 0, len(chunks))
	for _, ch := range chunks {
		lines = append(lines, styleLinkChunk(part, ch, spans, matches, links))
	}
	return lines
}

// styleLinkChunk underlines the URL pieces of one wrapped chunk. With
// links set, every piece is an OSC 8 hyperlink to its whole URL, so a
// wrapped or truncated URL still opens in full.
func styleLinkChunk(original string, ch textChunk, spans, matches [][]int, links bool) screenLine {
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
			b.WriteString(paintMatches(original, pos, visStart, matches))
		}
		frag := original[visStart:visEnd]
		startCol := lipgloss.Width(original[ch.from:visStart]) + 1
		shown := "\x1b[4m" + frag + "\x1b[24m"
		b.WriteString(hyperlink(original[span[0]:span[1]], shown, links))
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
		b.WriteString(paintMatches(original, pos, ch.to, matches))
	}
	if b.Len() == 0 {
		return screenLine{text: plainText(ch.text)}
	}
	match := slices.ContainsFunc(matches, func(mt []int) bool { return mt[0] < ch.to && mt[1] > ch.from })
	return screenLine{text: b.String(), hits: hits, match: match}
}

func (m *Model) relayLines(it *decide.Item, width int) []screenLine {
	var lines []screenLine
	for i, opt := range it.Options {
		if i >= 9 {
			break
		}
		mark := ""
		if opt.Recommended {
			mark = " (recommended)"
		}
		label := fmt.Sprintf("%d  %s%s", i+1, opt.Label, mark)
		useStyles()
		for _, ch := range wrapIndexed(label, width) {
			lines = append(lines, screenLine{
				text: optionButton.Render(ch.text),
				hits: []hit{{kind: hitOption, n: i}},
			})
		}
	}
	for _, link := range it.Links {
		lines = append(lines, linkLines(link, width, nil, !m.NoHyperlinks)...)
	}
	deadline := "-"
	if it.Deadline != nil {
		deadline = it.Deadline.Format(time.RFC3339)
	}
	lines = append(lines, screenLine{text: plainText("deadline: " + deadline)})
	return lines
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

// inboxCells are the list columns: id, requester, relative time, kind or
// state, then the question.
// A snoozed item shows its wake time in place of the age.
func inboxCells(it decide.Item, now time.Time) []string {
	when := age(now.Sub(it.CreatedAt))
	if it.SnoozedUntil != nil {
		when = "until " + decide.SnoozeLabel(it.SnoozedUntil.In(now.Location()))
	}
	return []string{it.ID, it.Owner, when, rowBadge(it), rowSnippet(it)}
}

// rowBadge is the item kind, or the state when the kind is empty.
func rowBadge(it decide.Item) string {
	if it.Kind != "" {
		return it.Kind
	}
	return it.State
}

// rowMark reserves two columns. A notification with no options shows a star.
func rowMark(it decide.Item) string {
	if it.Unread && len(it.Options) == 0 {
		return "* "
	}
	return "  "
}

func rowSnippet(it decide.Item) string {
	text := it.Question
	if n := len(it.Messages); n > 0 {
		text = it.Messages[n-1].Text
	}
	return strings.Join(strings.Fields(text), " ")
}

// ANSI palette indexes follow the terminal theme, not a fixed RGB color.
// Muted body text is ANSI 7. Relative time stays ANSI 8. Neither is faint.
const (
	paletteRequester lipgloss.ANSIColor = 6
	paletteHuman     lipgloss.ANSIColor = 6
	paletteAgent     lipgloss.ANSIColor = 2
	paletteTime      lipgloss.ANSIColor = 8
	paletteBadge     lipgloss.ANSIColor = 3
	paletteMuted     lipgloss.ANSIColor = 7
)

var (
	paletteMu       sync.Mutex
	paletteCached   *lipgloss.Renderer
	paletteNoColor  string
	paletteCacheSet bool
	trueCached      *lipgloss.Renderer
)

// paletteRenderer uses the terminal ANSI palette, or no color when NO_COLOR is set.
func paletteRenderer() *lipgloss.Renderer {
	noColor := os.Getenv("NO_COLOR")
	paletteMu.Lock()
	defer paletteMu.Unlock()
	if paletteCacheSet && paletteNoColor == noColor {
		return paletteCached
	}
	r := lipgloss.NewRenderer(io.Discard)
	if noColor != "" {
		r.SetColorProfile(termenv.Ascii)
	} else {
		r.SetColorProfile(termenv.ANSI)
	}
	paletteCached = r
	paletteNoColor = noColor
	paletteCacheSet = true
	return r
}

// trueColorRenderer keeps a hex palette override at its RGB value.
func trueColorRenderer() *lipgloss.Renderer {
	paletteMu.Lock()
	defer paletteMu.Unlock()
	if trueCached != nil {
		return trueCached
	}
	r := lipgloss.NewRenderer(io.Discard)
	r.SetColorProfile(termenv.TrueColor)
	trueCached = r
	return r
}

// loadColors reads palette overrides. A missing or unreadable config keeps defaults.
func loadColors() decide.Colors {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return decide.Colors{}
	}
	return cfg.Colors
}

// paletteStyle paints one palette entry. An override is an ANSI index or a
// hex color. A hex color keeps its RGB. An empty override uses fallback.
func paletteStyle(override string, fallback lipgloss.ANSIColor) lipgloss.Style {
	color := strings.TrimSpace(override)
	r := paletteRenderer()
	if color == "" {
		return r.NewStyle().Foreground(fallback)
	}
	if strings.HasPrefix(color, "#") && os.Getenv("NO_COLOR") == "" {
		r = trueColorRenderer()
	}
	return r.NewStyle().Foreground(lipgloss.Color(color))
}

// inboxStyles colors the requester, the time, and the kind or state badge.
// A set NO_COLOR drops those colors and leaves the column text in place.
func inboxStyles() [5]lipgloss.Style {
	colors := loadColors()
	r := paletteRenderer()
	plain := r.NewStyle()
	return [5]lipgloss.Style{
		plain,
		paletteStyle(colors.Requester, paletteRequester),
		paletteStyle(colors.Time, paletteTime),
		paletteStyle(colors.Badge, paletteBadge),
		plain,
	}
}

// inboxGutter is the cursor marker plus the unread mark.
const inboxGutter = 4

// inboxColumnWidths gives every column one shared width.
// A long question keeps about half the row, and the metadata columns shrink to match.
func inboxColumnWidths(rows [][]string, rowWidth int) []int {
	widths := inboxNaturalWidths(rows)
	if rowWidth <= 0 {
		return widths
	}
	const cols = 5
	available := rowWidth - inboxGutter - (cols - 1)
	if available < cols {
		for i := range widths {
			widths[i] = 1
		}
		return widths
	}
	fitInboxWidths(widths, available)
	return widths
}

func inboxNaturalWidths(rows [][]string) []int {
	const cols = 5
	widths := make([]int, cols)
	for i := range widths {
		widths[i] = 1
	}
	for _, row := range rows {
		for i := 0; i < cols && i < len(row); i++ {
			if w := lipgloss.Width(row[i]); w > widths[i] {
				widths[i] = w
			}
		}
	}
	return widths
}

func fitInboxWidths(widths []int, available int) {
	metaEnd := len(widths) - 1
	metaSum := sumWidths(widths[:metaEnd])
	if metaSum+widths[metaEnd] <= available {
		return
	}
	qBudget := available / 2
	if qBudget < 1 {
		qBudget = 1
	}
	shrinkTo(widths[:metaEnd], available-qBudget)
	question := available - sumWidths(widths[:metaEnd])
	if question > widths[metaEnd] {
		question = widths[metaEnd]
	}
	if question < 1 {
		question = 1
	}
	widths[metaEnd] = question
}

func sumWidths(widths []int) int {
	sum := 0
	for _, w := range widths {
		sum += w
	}
	return sum
}

// shrinkTo reduces widths from the right until they sum to budget.
func shrinkTo(widths []int, budget int) {
	if len(widths) == 0 {
		return
	}
	if budget < len(widths) {
		budget = len(widths)
	}
	sum := 0
	for _, w := range widths {
		sum += w
	}
	overflow := sum - budget
	for i := len(widths) - 1; i >= 0 && overflow > 0; i-- {
		spare := widths[i] - 1
		if spare < 1 {
			continue
		}
		if spare > overflow {
			widths[i] -= overflow
			return
		}
		overflow -= spare
		widths[i] = 1
	}
}

// paintInboxRow pads each cell to its column. A non-nil re marks matches.
func paintInboxRow(cells []string, widths []int, styles [5]lipgloss.Style, re *regexp.Regexp) string {
	var b strings.Builder
	for i, width := range widths {
		if i > 0 {
			b.WriteByte(' ')
		}
		text := ""
		if i < len(cells) {
			text = cells[i]
		}
		if lipgloss.Width(text) > width {
			text = truncateVisible(text, width)
		}
		pad := width - lipgloss.Width(text)
		if pad < 0 {
			pad = 0
		}
		if text != "" && i < len(styles) {
			text = paintCell(text, styles[i], re)
		}
		b.WriteString(text)
		if pad > 0 {
			b.WriteString(strings.Repeat(" ", pad))
		}
	}
	return b.String()
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
