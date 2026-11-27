package tui

import (
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

type reloadMsg struct{}

type panelsUpdatedMsg struct {
	panels []decide.PanelEntry
}

func runeKey(r rune) tea.KeyMsg {
	switch r {
	case '\x1b':
		return tea.KeyMsg{Type: tea.KeyEsc}
	case '\r':
		return tea.KeyMsg{Type: tea.KeyEnter}
	case '\n':
		return tea.KeyMsg{Type: tea.KeyCtrlJ}
	case '\t':
		return tea.KeyMsg{Type: tea.KeyTab}
	case '\b', '\x7f':
		return tea.KeyMsg{Type: tea.KeyBackspace}
	default:
		return tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{r}}
	}
}

func keyRune(k tea.KeyMsg) (rune, bool) {
	switch k.Type {
	case tea.KeyRunes:
		if len(k.Runes) == 1 {
			return k.Runes[0], true
		}
		return 0, false
	case tea.KeySpace:
		return ' ', true
	case tea.KeyEnter:
		return '\r', true
	case tea.KeyEsc:
		return '\x1b', true
	case tea.KeyTab:
		return '\t', true
	case tea.KeyBackspace:
		return '\x7f', true
	default:
		return 0, false
	}
}

// Init starts the first ledger list in the background, or the reload
// tick once it has loaded, the first background load of the bus names,
// the first bus health check, and the delivery poll when a delivery is
// pending.
func (m *Model) Init() tea.Cmd {
	reload := m.scheduleReload()
	if m.loading {
		reload = m.listCmd()
	}
	return tea.Batch(reload, m.busLoad(), m.healthCheck(), m.startDeliveryPoll(), m.waitPanelCmd())
}

// Update drives the existing inbox behavior from bubbletea messages.
// Opening an item also starts the lookup of where its owner lives, and
// o in the item view the jump to the owner's tab. An opened link runs
// its opener the same way.
func (m *Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	next, cmd := m.update(msg)
	if locate := m.locateCmd(); locate != nil {
		cmd = tea.Batch(cmd, locate)
	}
	if focus := m.focusCmd(); focus != nil {
		cmd = tea.Batch(cmd, focus)
	}
	if open := m.openCmd(); open != nil {
		cmd = tea.Batch(cmd, open)
	}
	if bus := m.takeBusCmd(); bus != nil {
		cmd = tea.Batch(cmd, bus)
	}
	if draft := m.draftTick(); draft != nil {
		cmd = tea.Batch(cmd, draft)
	}
	if editCfg := m.editConfigCmd(); editCfg != nil {
		cmd = tea.Batch(cmd, editCfg)
	}
	if extEdit := m.externalEditCmd(); extEdit != nil {
		cmd = tea.Batch(cmd, extEdit)
	}
	return next, cmd
}

func (m *Model) update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if m.keyLog != nil {
		logInput(m.keyLog, msg)
	}
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		if msg.Height > 0 {
			m.height = msg.Height
		}
		if msg.Width > 0 {
			m.width = msg.Width
		}
		return m, nil
	case editorDoneMsg:
		m.finishExternalEdit(msg.err)
		return m, nil
	case tea.KeyMsg:
		next, cmd := m.updateKeyMsg(msg)
		if tick := m.searchTick(); tick != nil {
			cmd = tea.Batch(cmd, tick)
		}
		return next, cmd
	case searchTickMsg:
		m.finishDebounce()
		return m, nil
	case chordTimeoutMsg:
		m.expireChord(int(msg))
		return m, nil
	case tea.MouseMsg:
		return m.updateMouse(msg)
	case reloadMsg:
		next, cmd := m.updateReload()
		if bus := m.busReload(); bus != nil {
			cmd = tea.Batch(cmd, bus)
		}
		return next, cmd
	case listMsg:
		return m.finishList(msg)
	default:
		return m, m.updateResult(msg)
	}
}

// updateResult records what a background command found: an owner
// location, a tab focus, a link opener's result, the bus names, or the
// bus health. It returns the next health command.
func (m *Model) updateResult(msg tea.Msg) tea.Cmd {
	if cmd, ok := m.updateHealth(msg); ok {
		return cmd
	}
	if cmd, ok := m.updateDelivery(msg); ok {
		return cmd
	}
	switch msg := msg.(type) {
	case draftTickMsg:
		m.finishDraftDebounce(msg.kind, msg.seq)
	case locationMsg:
		m.setLocation(msg)
	case focusMsg:
		m.status = string(msg)
	case openedMsg:
		m.finishOpen(msg)
	case busListMsg:
		m.finishBusLoad(msg)
	case panelsUpdatedMsg:
		m.panels = decide.ClonePanelEntries(msg.panels)
		m.panelsLoaded = true
		if m.mode == modeStatus {
			m.clampStatusCursor()
		}
		return m.waitPanelCmd()
	case configEditorDoneMsg:
		m.finishConfigEdit(msg.err)
	}
	return nil
}

func (m *Model) waitPanelCmd() tea.Cmd {
	ch := m.panelEvents
	if ch == nil {
		return nil
	}
	return func() tea.Msg {
		panels, ok := <-ch
		if !ok {
			return nil
		}
		return panelsUpdatedMsg{panels: panels}
	}
}

// updateKeyMsg routes one key: the external-editor prefix, then composer
// editing keys, then chords, scrolling, and rune shortcuts.
func (m *Model) updateKeyMsg(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	if m.flagOverlayKeyMsg(msg) {
		return m, nil
	}
	if m.settingsEditing {
		m.handleSettingsEditKey(msg)
		return m, nil
	}
	if cmd, ok := m.updateFocusedComposerKey(msg); ok {
		return m, cmd
	}
	if m.popupArrow(msg) || m.updatePickerKey(msg) {
		return m, nil
	}
	if m.chord {
		return m.updateChordKey(msg)
	}
	if cmd, handled := m.updateComposerNavKey(msg); handled {
		return m, cmd
	}
	if m.handleScrollKey(msg) {
		return m, nil
	}
	if !m.modalOpen() && !m.inComposerState() {
		if next, cmd, handled := m.updateBrowseKey(msg); handled {
			return next, cmd
		}
	}
	return m.updateKey(msg)
}

func (m *Model) updateFocusedComposerKey(msg tea.KeyMsg) (tea.Cmd, bool) {
	if cmd, ok := m.handleEditorKey(msg); ok {
		return cmd, true
	}
	if kind, ok := m.composerKind(); ok {
		if cmd, handled := m.updateFocusedComposerAction(kind, msg); handled {
			return cmd, true
		}
	}
	if m.composerFocused() && m.isFillKey(msg) {
		m.fillComposer()
		return nil, true
	}
	if m.isEditKey(msg) && m.editComposerKey(msg) {
		return nil, true
	}
	return nil, false
}

func (m *Model) updateFocusedComposerAction(kind composerKind, msg tea.KeyMsg) (tea.Cmd, bool) {
	if m.isSendKey(msg) {
		return m.sendComposer(kind), true
	}
	if kind == composerReply && m.matchAction("editor", msg, "compose.sendAndArchive") && m.canSendAndArchive() {
		if err := m.submitReplyAndArchive(); err != nil {
			m.runErr = err
			return tea.Quit, true
		}
		return nil, true
	}
	if m.matchAction("editor", msg, "compose.discard") {
		switch kind {
		case composerReply:
			m.discardReply()
			return nil, true
		case composerNote:
			m.discardCompose()
			return nil, true
		}
	}
	return nil, false
}

func (m *Model) updatePickerKey(msg tea.KeyMsg) bool {
	if !m.composing || m.composeFocus != focusRecipient || m.modalOpen() {
		return false
	}
	switch {
	case m.matchAction("picker", msg, "compose.pickerNext"):
		m.pickerArrow(1)
	case m.matchAction("picker", msg, "compose.pickerPrev"):
		m.pickerArrow(-1)
	case m.matchAction("picker", msg, "compose.pickerStar"):
		m.starPicked()
	default:
		return false
	}
	return true
}

func (m *Model) updateChordKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	s, ok := keyStrokeFromTea(msg)
	if !ok {
		m.expireChord(m.chordSeq)
		return m, nil
	}
	quit, err := m.finishChordStroke(s)
	if err != nil {
		m.runErr = err
		return m, tea.Quit
	}
	if quit {
		return m, tea.Quit
	}
	return m, nil
}

func (m *Model) updateComposerNavKey(msg tea.KeyMsg) (tea.Cmd, bool) {
	if m.composerFocused() && (m.matchAction("editor", msg, "edit.pageUp") || m.matchAction("editor", msg, "edit.pageDown")) {
		if m.editComposerKey(msg) {
			return nil, true
		}
	}
	if !m.inComposerState() || m.modalOpen() {
		return nil, false
	}
	if m.updateComposerFieldNavKey(msg) {
		return nil, true
	}
	if m.replySend {
		if cmd, ok := m.updateReplyButtonKey(msg); ok {
			return cmd, true
		}
	}
	if m.composing && m.composeFocus >= focusSend {
		if cmd, ok := m.updateComposeButtonKey(msg); ok {
			return cmd, true
		}
	}
	if m.matchAction("compose", msg, "compose.cancel") {
		return m.applyRune('\x1b'), true
	}
	return nil, false
}

func (m *Model) updateComposerFieldNavKey(msg tea.KeyMsg) bool {
	if m.matchAction("compose", msg, "compose.prevField") {
		m.ShiftTab()
		return true
	}
	if !m.matchAction("compose", msg, "compose.nextField") {
		return false
	}
	switch {
	case m.replyFocus:
		m.focusReplySend()
	case m.replySend:
		m.advanceReplyButton()
	case m.composing:
		m.advanceCompose()
	default:
		return false
	}
	return true
}

func (m *Model) updateComposeButtonKey(msg tea.KeyMsg) (tea.Cmd, bool) {
	switch {
	case m.matchAction("compose", msg, "compose.send"):
		if err := m.sendNote(); err != nil {
			m.runErr = err
			return tea.Quit, true
		}
		return nil, true
	case m.matchAction("compose", msg, "compose.discard"):
		m.discardCompose()
		return nil, true
	default:
		return nil, false
	}
}

func (m *Model) updateReplyButtonKey(msg tea.KeyMsg) (tea.Cmd, bool) {
	switch {
	case m.matchAction("compose", msg, "compose.send"):
		if err := m.submitReply(); err != nil {
			m.runErr = err
			return tea.Quit, true
		}
		return nil, true
	case m.matchAction("compose", msg, "compose.sendAndArchive") && m.canSendAndArchive():
		if err := m.submitReplyAndArchive(); err != nil {
			m.runErr = err
			return tea.Quit, true
		}
		return nil, true
	case m.matchAction("compose", msg, "compose.discard"):
		m.discardReply()
		return nil, true
	default:
		return nil, false
	}
}

func (m *Model) updateBrowseKey(msg tea.KeyMsg) (tea.Model, tea.Cmd, bool) {
	s, ok := keyStrokeFromTea(msg)
	if !ok {
		return nil, nil, false
	}
	seq := m.chordSeq
	quit, err := m.dispatchBrowseKey(s)
	if err != nil {
		m.runErr = err
		return m, tea.Quit, true
	}
	if quit {
		return m, tea.Quit, true
	}
	if m.mouseSync {
		m.mouseSync = false
		return m, m.mouseCommand(), true
	}
	if m.chord && m.chordSeq != seq {
		return m, m.scheduleChord(), true
	}
	return m, nil, true
}

func (m *Model) modalOpen() bool {
	return m.confirmKind != confirmNone || m.chord || m.copyMenu || m.snoozing || m.flagging || m.askClosing != "" || m.searching || m.help || m.aliasPick || m.pickerOpen() || m.settingsEditing
}

func (m *Model) inComposerState() bool {
	return m.answerFocus || m.replyFocus || m.replySend || m.mode == modeOutcome || m.composing
}

// popupArrow moves the snooze popup or the link picker with up and
// down, and the avatar picker with the four arrows.
func (m *Model) popupArrow(msg tea.KeyMsg) bool {
	if m.avatarPick {
		return m.avatarArrow(msg)
	}
	if !m.snoozing && !m.linkPick {
		return false
	}
	if msg.Type != tea.KeyUp && msg.Type != tea.KeyDown {
		return false
	}
	delta := 1
	if msg.Type == tea.KeyUp {
		delta = -1
	}
	if m.snoozing {
		m.moveSnooze(delta)
	} else {
		m.moveLinkPick(delta)
	}
	return true
}

func (m *Model) updateKey(k tea.KeyMsg) (tea.Model, tea.Cmd) {
	if k.Type == tea.KeyShiftTab {
		m.ShiftTab()
		return m, nil
	}
	if len(k.Runes) > 1 && k.Type == tea.KeyRunes {
		if !m.modalOpen() && !m.composeRestorePrompt && m.composerFocused() && allPrintableRunes(k.Runes) {
			m.editComposerKey(k)
			return m, nil
		}
		for _, r := range k.Runes {
			if cmd := m.applyRune(r); cmd != nil {
				return m, cmd
			}
		}
		return m, nil
	}
	r, ok := keyRune(k)
	if !ok {
		return m, nil
	}
	if cmd := m.applyRune(r); cmd != nil {
		return m, cmd
	}
	return m, nil
}

func allPrintableRunes(runes []rune) bool {
	for _, r := range runes {
		if r < ' ' || r == '\x7f' {
			return false
		}
	}
	return true
}

type chordTimeoutMsg int

func (m *Model) scheduleChord() tea.Cmd {
	seq := m.chordSeq
	return tea.Tick(time.Second, func(time.Time) tea.Msg {
		return chordTimeoutMsg(seq)
	})
}

func (m *Model) applyRune(r rune) tea.Cmd {
	seq := m.chordSeq
	quit, err := m.Key(r)
	if err != nil {
		m.runErr = err
		return tea.Quit
	}
	if quit {
		return tea.Quit
	}
	if m.mouseSync {
		m.mouseSync = false
		return m.mouseCommand()
	}
	if m.chord && m.chordSeq != seq {
		return m.scheduleChord()
	}
	return nil
}

func (m *Model) mouseCommand() tea.Cmd {
	if m.mouseOn {
		return tea.EnableMouseCellMotion
	}
	return tea.DisableMouse
}

func (m *Model) updateMouse(msg tea.MouseMsg) (tea.Model, tea.Cmd) {
	// Shift+drag is the terminal's selection bypass. Leave it alone.
	// Capture off lets a plain drag select in the terminal. The bus-down
	// and reply route prompts take only keys.
	if !m.mouseOn || msg.Shift || m.snoozing || m.linkPick || m.confirmKind == confirmBusDown || m.confirmKind == confirmRoute {
		return m, nil
	}
	if m.avatarPick {
		return m.updateAvatarMouse(msg)
	}
	switch msg.Button {
	case tea.MouseButtonWheelDown:
		m.Wheel(true)
		return m, nil
	case tea.MouseButtonWheelUp:
		m.Wheel(false)
		return m, nil
	}
	if msg.Button != tea.MouseButtonLeft || msg.Action != tea.MouseActionPress {
		return m, nil
	}
	// Click keeps 1-based columns. Bubbletea mouse messages are 0-based.
	if msg.Ctrl {
		m.CtrlClick(msg.X+1, msg.Y+1)
		return m, nil
	}
	if err := m.Click(msg.X+1, msg.Y+1); err != nil {
		m.runErr = err
		return m, tea.Quit
	}
	return m, nil
}

// updateAvatarMouse clicks a picker cell, cancels outside one, or
// moves one row with the wheel. Ctrl+click stays a no-op.
func (m *Model) updateAvatarMouse(msg tea.MouseMsg) (tea.Model, tea.Cmd) {
	switch msg.Button {
	case tea.MouseButtonWheelDown:
		m.moveAvatarPick(m.avatarPerRow())
		return m, nil
	case tea.MouseButtonWheelUp:
		m.moveAvatarPick(-m.avatarPerRow())
		return m, nil
	}
	if msg.Ctrl || msg.Button != tea.MouseButtonLeft || msg.Action != tea.MouseActionPress {
		return m, nil
	}
	if err := m.Click(msg.X+1, msg.Y+1); err != nil {
		m.runErr = err
		return m, tea.Quit
	}
	return m, nil
}

// listMsg is a ledger read made off the update loop.
type listMsg struct {
	all       []decide.Item
	drafts    []decide.Draft
	hasDrafts bool
	err       error
}

// listCmd reads the ledger off the update loop.
func (m *Model) listCmd() tea.Cmd {
	store := m.store()
	return func() tea.Msg {
		all, err := store.List()
		msg := listMsg{all: all, err: err}
		if err == nil {
			if db, ok := store.(DraftBackend); ok {
				if drafts, derr := db.ListDrafts(); derr == nil {
					msg.drafts = drafts
					msg.hasDrafts = true
				}
			}
		}
		return msg
	}
}

// updateReload reloads the ledger. A backend that may be slow, such as
// a hub's, and the first list are read off the update loop: the view
// keeps the last good data until the read returns, and the next tick
// waits for it.
func (m *Model) updateReload() (tea.Model, tea.Cmd) {
	if m.asyncReload || m.loading {
		return m, m.listCmd()
	}
	if err := m.Reload(); err != nil {
		m.runErr = err
		return m, tea.Quit
	}
	return m.reloaded()
}

// finishList applies a background read. A failed first list shows its
// error on the status line and retries on the next tick.
func (m *Model) finishList(msg listMsg) (tea.Model, tea.Cmd) {
	if msg.err != nil {
		if err := m.reloadFailed(msg.err); err != nil {
			if !m.loading {
				m.runErr = err
				return m, tea.Quit
			}
			m.status = err.Error()
			m.loadErr = m.status
		}
		return m, m.scheduleReload()
	}
	if err := m.applyList(msg.all); err != nil {
		m.runErr = err
		return m, tea.Quit
	}
	if msg.hasDrafts {
		m.applyDrafts(msg.drafts)
	}
	m.pollDisplay()
	return m.reloaded()
}

// reloaded refreshes an open status view, schedules the next reload,
// and starts the delivery poll when a delivery became pending.
func (m *Model) reloaded() (tea.Model, tea.Cmd) {
	if m.mode == modeStatus {
		if err := m.loadStatus(); err != nil {
			m.status = err.Error()
		}
	}
	if m.mode == modeContacts {
		m.rebuildContacts()
	}
	return m, tea.Batch(m.scheduleReload(), m.startDeliveryPoll())
}

func scrollKeyType(t tea.KeyType) bool {
	switch t {
	case tea.KeyUp, tea.KeyDown, tea.KeyPgUp, tea.KeyPgDown, tea.KeyHome, tea.KeyEnd:
		return true
	default:
		return false
	}
}

// handleScrollKey scrolls an open item, or leaves the key with a focused
// field. Up and down still move a focused recipient list.
func (m *Model) handleScrollKey(k tea.KeyMsg) bool {
	if !scrollKeyType(k.Type) || m.mode == modeSettings {
		return false
	}
	if m.editComposerKey(k) {
		return true
	}
	if m.itemScroll() {
		m.scrollNav(k)
		return true
	}
	if m.listNavKey(k) {
		m.listNav(k)
		return true
	}
	if k.Type == tea.KeyUp || k.Type == tea.KeyDown {
		delta := 1
		if k.Type == tea.KeyUp {
			delta = -1
		}
		m.pickerArrow(delta)
		return true
	}
	return false
}

func (m *Model) listNavKey(k tea.KeyMsg) bool {
	if m.help || m.composing || m.linkPick || m.avatarPick || (m.mode != modeList && m.mode != modeStatus && m.mode != modeContacts) {
		return false
	}
	switch k.Type {
	case tea.KeyPgUp, tea.KeyPgDown, tea.KeyHome, tea.KeyEnd:
		return true
	default:
		return false
	}
}

func (m *Model) listNav(k tea.KeyMsg) {
	switch k.Type {
	case tea.KeyPgUp:
		m.moveList(-m.listPage())
	case tea.KeyPgDown:
		m.moveList(m.listPage())
	case tea.KeyHome:
		m.jumpList(0)
	case tea.KeyEnd:
		m.jumpList(m.listLen() - 1)
	}
}

func (m *Model) itemScroll() bool {
	return m.mode == modeDetail && !m.help && !m.composing && !m.answerFocus && !m.replyFocus && !m.replySend && !m.linkPick && !m.avatarPick
}

func (m *Model) scrollNav(k tea.KeyMsg) {
	switch k.Type {
	case tea.KeyUp:
		m.Wheel(false)
	case tea.KeyDown:
		m.Wheel(true)
	case tea.KeyPgUp:
		m.scrollPage(false)
	case tea.KeyPgDown:
		m.scrollPage(true)
	case tea.KeyHome:
		m.scrollHome()
	case tea.KeyEnd:
		m.scrollEnd()
	}
}

func (m *Model) scheduleReload() tea.Cmd {
	interval := m.interval
	if interval <= 0 {
		interval = DefaultInterval
	}
	return tea.Tick(interval, func(time.Time) tea.Msg {
		return reloadMsg{}
	})
}
