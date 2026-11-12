package tui

import (
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

type reloadMsg struct{}

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

// Init starts the ledger reload tick.
func (m *Model) Init() tea.Cmd {
	return m.scheduleReload()
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
		return m.updateReload()
	default:
		m.updateResult(msg)
		return m, nil
	}
}

// updateResult records what a background command found: an owner
// location, a tab focus, or a link opener's result.
func (m *Model) updateResult(msg tea.Msg) {
	switch msg := msg.(type) {
	case locationMsg:
		m.setLocation(msg)
	case focusMsg:
		m.status = string(msg)
	case openedMsg:
		m.finishOpen(msg)
	}
}

// updateKeyMsg routes one key: the external-editor prefix, then composer
// editing keys, then chords, scrolling, and rune shortcuts.
func (m *Model) updateKeyMsg(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	if cmd, ok := m.handleEditorKey(msg); ok {
		return m, cmd
	}
	if kind, ok := m.composerKind(); ok && isSendKey(msg) {
		return m, m.sendComposer(kind)
	}
	if isEditKey(msg) && m.editComposerKey(msg) {
		return m, nil
	}
	if m.popupArrow(msg) {
		return m, nil
	}
	switch msg.Type {
	case tea.KeyCtrlN:
		m.pickerArrow(1)
		return m, nil
	case tea.KeyCtrlP:
		m.pickerArrow(-1)
		return m, nil
	}
	if m.chord {
		if _, ok := keyRune(msg); !ok {
			m.expireChord(m.chordSeq)
			return m, nil
		}
		return m.updateKey(msg)
	}
	if m.handleScrollKey(msg) {
		return m, nil
	}
	return m.updateKey(msg)
}

// popupArrow moves the snooze popup or the link picker with up and down.
func (m *Model) popupArrow(msg tea.KeyMsg) bool {
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
	// Capture off lets a plain drag select in the terminal.
	if !m.mouseOn || msg.Shift || m.snoozing || m.linkPick {
		return m, nil
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
	if err := m.Click(msg.X+1, msg.Y+1); err != nil {
		m.runErr = err
		return m, tea.Quit
	}
	return m, nil
}

func (m *Model) updateReload() (tea.Model, tea.Cmd) {
	if err := m.Reload(); err != nil {
		m.runErr = err
		return m, tea.Quit
	}
	if m.mode == modeStatus {
		if err := m.loadStatus(); err != nil {
			m.status = err.Error()
		}
	}
	return m, m.scheduleReload()
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
	if !scrollKeyType(k.Type) {
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
	if m.help || m.composing || m.linkPick || (m.mode != modeList && m.mode != modeStatus) {
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
	return m.mode == modeDetail && !m.help && !m.composing && !m.answerFocus && !m.replyFocus && !m.linkPick
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
