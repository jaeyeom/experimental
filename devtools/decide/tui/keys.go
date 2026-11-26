package tui

import (
	"fmt"
	"os"
	"slices"
	"strings"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textarea"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func (m *Model) keyMap() *decide.KeyMap {
	if m.keys != nil {
		return m.keys
	}
	return decide.DefaultKeyMap()
}

func isUTF8Locale() bool {
	for _, name := range []string{"LC_ALL", "LC_CTYPE", "LANG"} {
		if v := os.Getenv(name); v != "" {
			lower := strings.ToLower(v)
			return strings.Contains(lower, "utf-8") || strings.Contains(lower, "utf8")
		}
	}
	return true
}

func (m *Model) useASCIIKeys() bool {
	return m.ASCIIKeys || !isUTF8Locale()
}

func (m *Model) buttonKey(action string) string {
	if action == "" {
		return ""
	}
	return m.keyMap().PrimaryButtonString(action, m.useASCIIKeys())
}

func (m *Model) shortestButtonKey(action string) string {
	if action == "" {
		return ""
	}
	return m.keyMap().ShortestButtonString(action, m.useASCIIKeys())
}

// buttonLabel formats a button "[name (hint)]" (or "> [name (hint)]" when focused)
// fitting within maxWidth when maxWidth > 0. When the full label with hint
// exceeds maxWidth, the hint is dropped first; only if "[name]" still
// exceeds maxWidth is the label truncated.
func buttonLabel(name, hint string, focused bool, maxWidth int) string {
	return formatButton("[", "]", name, hint, focused, maxWidth)
}

func formatButton(open, closeBracket, name, hint string, focused bool, maxWidth int) string {
	prefix := ""
	if focused {
		prefix = "> "
	}
	plain := prefix + open + name + closeBracket
	full := plain
	if hint != "" {
		full = prefix + open + name + " (" + hint + ")" + closeBracket
	}
	if maxWidth <= 0 || lipgloss.Width(full) <= maxWidth {
		return full
	}
	if hint != "" && lipgloss.Width(plain) <= maxWidth {
		return plain
	}
	return truncateVisible(plain, maxWidth)
}

// renderButton paints a button label reversed. When active is false, its
// "(hint)" is dimmed to show the shortcut does not work right now.
func renderButton(label string, active bool) string {
	useStyles()
	if active {
		return optionButton.Render(label)
	}
	i := strings.LastIndex(label, " (")
	if i < 0 || !strings.HasSuffix(label, ")]") {
		return optionButton.Render(label)
	}
	return optionButton.Render(label[:i]) +
		optionButton.Faint(true).Render(label[i:len(label)-1]) +
		optionButton.Render("]")
}

// renderPlainButton paints a plain-text button label. When active is false,
// its "(hint)" is dimmed to show the shortcut does not work right now.
func renderPlainButton(label string, active bool) string {
	if active {
		return plainText(label)
	}
	i := strings.LastIndex(label, " (")
	if i < 0 || !strings.HasSuffix(label, ")]") {
		return plainText(label)
	}
	useStyles()
	return plainText(label[:i]) +
		staleLine.Render(label[i:len(label)-1]) +
		plainText("]")
}

func (m *Model) actionButton(name, action string, focused bool, maxWidth int) string {
	return buttonLabel(name, m.buttonKey(action), focused, maxWidth)
}

func (m *Model) shortestActionButton(name, action string, focused bool, maxWidth int) string {
	return buttonLabel(name, m.shortestButtonKey(action), focused, maxWidth)
}

// SetKeys installs a custom keymap on m and updates all composer fields.
func (m *Model) SetKeys(km *decide.KeyMap) {
	m.keys = km
	ck := m.composerKeys()
	m.replyField.KeyMap = ck
	m.free.KeyMap = ck
	m.outcome.KeyMap = ck
	m.noteField.KeyMap = ck
}

func keyStrokeFromTea(k tea.KeyMsg) (decide.KeyStroke, bool) {
	if k.Type == tea.KeyRunes {
		if len(k.Runes) == 1 {
			if k.Runes[0] == ' ' {
				return decide.KeyStroke{Key: "space", Alt: k.Alt}, true
			}
			return decide.KeyStroke{Key: string(k.Runes[0]), Alt: k.Alt}, true
		}
		return decide.KeyStroke{}, false
	}
	if k.Type == tea.KeyShiftTab {
		return decide.KeyStroke{Key: "shift+tab"}, true
	}
	named := map[tea.KeyType]string{
		tea.KeySpace:     "space",
		tea.KeyEnter:     "enter",
		tea.KeyEsc:       "esc",
		tea.KeyTab:       "tab",
		tea.KeyUp:        "up",
		tea.KeyDown:      "down",
		tea.KeyLeft:      "left",
		tea.KeyRight:     "right",
		tea.KeyPgUp:      "pgup",
		tea.KeyPgDown:    "pgdn",
		tea.KeyHome:      "home",
		tea.KeyEnd:       "end",
		tea.KeyBackspace: "backspace",
		tea.KeyDelete:    "delete",
	}
	if name, ok := named[k.Type]; ok {
		return decide.KeyStroke{Key: name, Alt: k.Alt}, true
	}
	ctrl := map[tea.KeyType]string{
		tea.KeyCtrlA: "a", tea.KeyCtrlB: "b", tea.KeyCtrlC: "c", tea.KeyCtrlD: "d",
		tea.KeyCtrlE: "e", tea.KeyCtrlF: "f", tea.KeyCtrlG: "g", tea.KeyCtrlH: "h",
		tea.KeyCtrlJ: "j", tea.KeyCtrlK: "k", tea.KeyCtrlL: "l", tea.KeyCtrlN: "n",
		tea.KeyCtrlO: "o", tea.KeyCtrlP: "p", tea.KeyCtrlQ: "q", tea.KeyCtrlR: "r",
		tea.KeyCtrlS: "s", tea.KeyCtrlT: "t", tea.KeyCtrlU: "u", tea.KeyCtrlV: "v",
		tea.KeyCtrlW: "w", tea.KeyCtrlX: "x", tea.KeyCtrlY: "y", tea.KeyCtrlZ: "z",
	}
	if name, ok := ctrl[k.Type]; ok {
		return decide.KeyStroke{Key: name, Ctrl: true}, true
	}
	return decide.KeyStroke{}, false
}

func (m *Model) matchAction(scope string, k tea.KeyMsg, action string) bool {
	s, ok := keyStrokeFromTea(k)
	if !ok {
		return false
	}
	got, matched := m.keyMap().Match(scope, s)
	return matched && got == action
}

func (m *Model) composerKeys() textarea.KeyMap {
	km := m.keyMap()
	bind := func(action string) key.Binding {
		keys := km.ConfigKeys(action)
		if len(keys) == 0 {
			return key.NewBinding(key.WithDisabled())
		}
		return key.NewBinding(key.WithKeys(keys...))
	}
	return textarea.KeyMap{
		CharacterForward:        bind("edit.charForward"),
		CharacterBackward:       bind("edit.charBackward"),
		LineNext:                bind("edit.lineNext"),
		LinePrevious:            bind("edit.linePrev"),
		LineStart:               bind("edit.lineStart"),
		LineEnd:                 bind("edit.lineEnd"),
		DeleteCharacterForward:  bind("edit.deleteForward"),
		DeleteCharacterBackward: bind("edit.deleteBackward"),
		DeleteAfterCursor:       bind("edit.killLine"),
		InsertNewline:           bind("edit.newline"),
	}
}

func (m *Model) isEditKey(k tea.KeyMsg) bool {
	if m.matchAction("editor", k, "edit.openLine") {
		return true
	}
	km := m.composerKeys()
	return key.Matches(k,
		km.CharacterForward, km.CharacterBackward, km.LineNext, km.LinePrevious,
		km.LineStart, km.LineEnd, km.DeleteCharacterForward,
		km.DeleteCharacterBackward, km.DeleteAfterCursor, km.InsertNewline)
}

func (m *Model) editField(field *textarea.Model, k tea.KeyMsg) {
	if m.matchAction("editor", k, "edit.openLine") {
		field.InsertRune('\n')
		*field, _ = field.Update(tea.KeyMsg{Type: tea.KeyLeft})
		return
	}
	*field, _ = field.Update(k)
}

func (m *Model) isSendKey(k tea.KeyMsg) bool {
	return m.matchAction("editor", k, "compose.send")
}

func (m *Model) isFillKey(k tea.KeyMsg) bool {
	return m.matchAction("editor", k, "edit.fill")
}

func (m *Model) handleEditorKey(k tea.KeyMsg) (tea.Cmd, bool) {
	if m.editPrefix {
		m.editPrefix = false
		s, ok := keyStrokeFromTea(k)
		if _, canEdit := m.externalEditComposerKind(); ok && canEdit {
			if act, matched := m.keyMap().MatchChord("editor", m.editPrefixStroke, s); matched && act == "edit.external" {
				return m.startExternalEdit(), true
			}
		}
		return nil, true
	}
	if _, ok := m.externalEditComposerKind(); !ok {
		return nil, false
	}
	s, ok := keyStrokeFromTea(k)
	if !ok {
		return nil, false
	}
	km := m.keyMap()
	if km.IsPrefix("editor", s) {
		for _, entry := range km.ChordEntries("editor", s) {
			if entry.Action == "edit.external" {
				m.editPrefix = true
				m.editPrefixStroke = s
				return nil, true
			}
		}
	}
	if act, matched := km.Match("editor", s); matched && act == "edit.external" {
		return m.startExternalEdit(), true
	}
	return nil, false
}

func (m *Model) browseScope() string {
	switch m.mode {
	case modeDetail:
		return "detail"
	case modeStatus:
		return "status"
	case modeContacts:
		return "contacts"
	default:
		return "list"
	}
}

func (m *Model) unknownKeyStatus() string {
	h := m.actionSlash("help")
	if h == "" {
		return "unknown key"
	}
	return "unknown key, " + h + " for help"
}

func (m *Model) dispatchModeKey(s decide.KeyStroke) (bool, error) {
	switch m.mode {
	case modeOutbox:
		return m.handleOutboxKey(s)
	case modeSettings:
		return m.handleSettingsKey(s)
	case modeDetail:
		return m.handleDetailOutboxKey(s)
	default:
		return false, nil
	}
}

func (m *Model) dispatchBrowseKey(s decide.KeyStroke) (bool, error) {
	if handled, err := m.dispatchModeKey(s); handled {
		return false, err
	}
	scope := m.browseScope()
	if scope == "list" && !s.Ctrl && !s.Alt {
		if runes := []rune(s.Key); len(runes) == 1 {
			if handled, err := m.answerApprovalFromList(runes[0]); handled {
				return false, err
			}
		}
	}
	km := m.keyMap()
	if km.IsPrefix(scope, s) {
		m.beginChord(s)
		return false, nil
	}
	if action, ok := km.Match(scope, s); ok {
		return m.runAction(action)
	}
	if scope == "detail" && !s.Ctrl && !s.Alt {
		return false, m.dispatchDetailUnboundKey(s)
	}
	return false, nil
}

func (m *Model) dispatchDetailUnboundKey(s decide.KeyStroke) error {
	runes := []rune(s.Key)
	if len(runes) != 1 {
		return nil
	}
	r := runes[0]
	if r >= '1' && r <= '9' {
		return m.answerOption(int(r - '1'))
	}
	if m.hasDraftConflict(composerReply) {
		switch r {
		case 'K':
			m.resolveConflict(composerReply, conflictKeepCurrent)
			return nil
		case 'U':
			m.resolveConflict(composerReply, conflictUseOther)
			return nil
		}
	}
	if r >= ' ' {
		m.status = m.unknownKeyStatus()
	}
	return nil
}

func (m *Model) runAction(action string) (bool, error) {
	handlers := map[string]func() (bool, error){
		"quit":                 func() (bool, error) { return m.requestQuit() },
		"help":                 func() (bool, error) { m.help = true; return false, nil },
		"mouse":                func() (bool, error) { m.toggleMouse(); return false, nil },
		"search":               func() (bool, error) { m.beginSearch(); return false, nil },
		"goto.inbox":           func() (bool, error) { return false, m.showFolder(0) },
		"goto.snoozed":         func() (bool, error) { return false, m.showFolder(snoozedFolder) },
		"goto.answered":        func() (bool, error) { return false, m.showFolder(2) },
		"goto.done":            func() (bool, error) { return false, m.showFolder(3) },
		"goto.withdrawn":       func() (bool, error) { return false, m.showFolder(4) },
		"goto.status":          func() (bool, error) { return false, m.showStatus() },
		"goto.contacts":        func() (bool, error) { return false, m.showContacts() },
		"goto.outbox":          func() (bool, error) { return false, m.showOutbox() },
		"goto.settings":        func() (bool, error) { return false, m.showSettings() },
		"goto.top":             func() (bool, error) { return m.runGoToTop() },
		"list.next":            func() (bool, error) { m.moveList(1); return false, nil },
		"list.prev":            func() (bool, error) { m.moveList(-1); return false, nil },
		"list.pageDown":        func() (bool, error) { m.moveList(m.listPage()); return false, nil },
		"list.pageUp":          func() (bool, error) { m.moveList(-m.listPage()); return false, nil },
		"list.top":             func() (bool, error) { m.jumpList(0); return false, nil },
		"list.bottom":          func() (bool, error) { m.jumpList(m.listLen() - 1); return false, nil },
		"list.open":            func() (bool, error) { return false, m.openRow() },
		"list.compose":         func() (bool, error) { m.focusCompose(); return false, nil },
		"list.archive":         func() (bool, error) { return false, m.archiveSelected() },
		"list.classFilter":     func() (bool, error) { m.classFiltering = true; return false, nil },
		"list.snooze":          func() (bool, error) { m.beginSnooze(); return false, nil },
		"list.clearFilter":     func() (bool, error) { m.clearAgentFilter(); m.SetClassFilter(nil); return false, nil },
		"detail.back":          func() (bool, error) { m.backToList(); return false, nil },
		"detail.scrollDown":    func() (bool, error) { m.Wheel(true); return false, nil },
		"detail.scrollUp":      func() (bool, error) { m.Wheel(false); return false, nil },
		"detail.pageDown":      func() (bool, error) { m.scrollPage(true); return false, nil },
		"detail.pageUp":        func() (bool, error) { m.scrollPage(false); return false, nil },
		"detail.top":           func() (bool, error) { return m.runDetailTop() },
		"detail.bottom":        func() (bool, error) { m.scrollEnd(); return false, nil },
		"detail.nextMessage":   func() (bool, error) { m.moveMessage(1); return false, nil },
		"detail.prevMessage":   func() (bool, error) { m.moveMessage(-1); return false, nil },
		"detail.archive":       func() (bool, error) { return false, m.archiveSelected() },
		"detail.snooze":        func() (bool, error) { m.beginSnooze(); return false, nil },
		"detail.openLink":      func() (bool, error) { return false, m.openSelectedLink() },
		"detail.copyLink":      func() (bool, error) { m.copyCurrentLink(); return false, nil },
		"detail.copyMenu":      func() (bool, error) { m.openCopyMenu(); return false, nil },
		"detail.plain":         func() (bool, error) { m.togglePlain(); return false, nil },
		"detail.avatar":        func() (bool, error) { m.beginAvatarPick(); return false, nil },
		"detail.owner":         func() (bool, error) { m.jumpToOwner(); return false, nil },
		"detail.compose":       func() (bool, error) { m.focusCompose(); return false, nil },
		"detail.forward":       func() (bool, error) { m.forwardItem(); return false, nil },
		"detail.outcome":       func() (bool, error) { m.beginOutcome(); return false, nil },
		"detail.reply":         func() (bool, error) { m.focusReply(); return false, nil },
		"status.next":          func() (bool, error) { m.moveStatusCursor(1); return false, nil },
		"status.prev":          func() (bool, error) { m.moveStatusCursor(-1); return false, nil },
		"status.pageDown":      func() (bool, error) { m.moveList(m.listPage()); return false, nil },
		"status.pageUp":        func() (bool, error) { m.moveList(-m.listPage()); return false, nil },
		"status.top":           func() (bool, error) { m.jumpList(0); return false, nil },
		"status.bottom":        func() (bool, error) { m.jumpList(m.listLen() - 1); return false, nil },
		"status.back":          func() (bool, error) { m.backToList(); return false, nil },
		"status.compose":       func() (bool, error) { m.composeFromStatus(); return false, nil },
		"status.clearFilter":   func() (bool, error) { m.clearAgentFilter(); return false, nil },
		"status.togglePanel":   func() (bool, error) { m.toggleSelectedPanel(); return false, nil },
		"status.movePanelDown": func() (bool, error) { m.moveSelectedPanel(1); return false, nil },
		"status.movePanelUp":   func() (bool, error) { m.moveSelectedPanel(-1); return false, nil },
		"contacts.next":        func() (bool, error) { m.moveContact(1); return false, nil },
		"contacts.prev":        func() (bool, error) { m.moveContact(-1); return false, nil },
		"contacts.pageDown":    func() (bool, error) { m.moveList(m.listPage()); return false, nil },
		"contacts.pageUp":      func() (bool, error) { m.moveList(-m.listPage()); return false, nil },
		"contacts.top":         func() (bool, error) { m.jumpList(0); return false, nil },
		"contacts.bottom":      func() (bool, error) { m.jumpList(m.listLen() - 1); return false, nil },
		"contacts.back":        func() (bool, error) { m.backToList(); return false, nil },
		"contacts.filter":      func() (bool, error) { m.runContactsFilter(); return false, nil },
		"contacts.clearFilter": func() (bool, error) { m.clearAgentFilter(); return false, nil },
		"contacts.avatar":      func() (bool, error) { m.runContactsAvatar(); return false, nil },
		"contacts.compose":     func() (bool, error) { m.composeFromContact(); return false, nil },
		"contacts.star":        func() (bool, error) { m.starContact(); return false, nil },
		"contacts.hide":        func() (bool, error) { m.hideSelected(); return false, nil },
		"contacts.showHidden":  func() (bool, error) { m.toggleShowHidden(); return false, nil },
		"contacts.hideDead":    func() (bool, error) { m.askHideDead(); return false, nil },
		"contacts.release":     func() (bool, error) { m.askRelease(); return false, nil },
		"contacts.alias":       func() (bool, error) { m.beginAliasPick(); return false, nil },
		"contacts.details":     func() (bool, error) { m.contactDetail = !m.contactDetail; return false, nil },
		"contacts.orgTree":     func() (bool, error) { m.toggleOrgTree(); return false, nil },
	}
	if fn, ok := handlers[action]; ok {
		return fn()
	}
	return false, nil
}

func (m *Model) runGoToTop() (bool, error) {
	switch m.mode {
	case modeList, modeStatus, modeContacts:
		m.jumpList(0)
	case modeDetail:
		return m.runDetailTop()
	}
	return false, nil
}

// runDetailTop scrolls the thread to the top and selects its first message.
func (m *Model) runDetailTop() (bool, error) {
	m.scrollHome()
	if m.detail != nil && !m.help {
		m.message = 0
	}
	return false, nil
}

func (m *Model) runContactsFilter() {
	if c, ok := m.selectedContact(); ok {
		m.applyAgentFilter(c.Name)
	}
}

func (m *Model) runContactsAvatar() {
	if c, ok := m.selectedContact(); ok {
		m.openAvatarPick(c.Name)
	}
}

func (m *Model) toggleShowHidden() {
	m.showHidden = !m.showHidden
	if m.showHidden {
		m.status = "showing hidden contacts"
	} else {
		m.status = "hiding hidden contacts"
	}
	m.clampContactCursor()
}

func (m *Model) actionSlash(action string) string {
	seqs := m.keyMap().Bindings(action)
	if len(seqs) == 0 {
		return ""
	}
	parts := make([]string, len(seqs))
	for i, s := range seqs {
		parts[i] = s.HelpString()
	}
	return strings.Join(parts, "/")
}

func (m *Model) actionFirst(action string) string {
	seqs := m.keyMap().Bindings(action)
	if len(seqs) == 0 {
		return ""
	}
	return seqs[0].HelpString()
}

func (m *Model) actionCtrlFirst(action string) string {
	for _, s := range m.keyMap().Bindings(action) {
		if len(s) == 1 && s[0].Ctrl {
			return s.HelpString()
		}
	}
	return m.actionFirst(action)
}

func (m *Model) actionOr(action string) string {
	seqs := m.keyMap().Bindings(action)
	if len(seqs) == 0 {
		return ""
	}
	parts := make([]string, len(seqs))
	for i, s := range seqs {
		parts[i] = s.HelpString()
	}
	if len(parts) == 1 {
		return parts[0]
	}
	return strings.Join(parts[:len(parts)-1], ", ") + " or " + parts[len(parts)-1]
}

func (m *Model) sendHintLabel() string {
	seqs := m.keyMap().Bindings("compose.send")
	if len(seqs) == 0 {
		return ""
	}
	if len(seqs) == 2 && len(seqs[0]) == 1 && seqs[0][0].Key == "enter" && seqs[0][0].Alt &&
		len(seqs[1]) == 1 && seqs[1][0].Key == "s" && seqs[1][0].Ctrl {
		return "Ctrl+Enter / Alt+Enter"
	}
	return m.sendKeysHelp(" / ")
}

func (m *Model) sendKeysHelp(sep string) string {
	seqs := m.keyMap().Bindings("compose.send")
	if len(seqs) == 0 {
		return ""
	}
	parts := make([]string, 0, len(seqs))
	for _, s := range seqs {
		if len(s) == 1 && s[0].Key == "enter" && s[0].Alt {
			parts = append(parts, "Ctrl+Enter"+sep+"Alt+Enter")
			continue
		}
		parts = append(parts, s.HelpString())
	}
	if sep == ", " && len(parts) > 1 {
		return strings.Join(parts[:len(parts)-1], ", ") + " or " + parts[len(parts)-1]
	}
	return strings.Join(parts, " / ")
}

func (m *Model) composerHintLine(kind composerKind) string {
	var parts []string
	if start, end := m.actionCtrlFirst("edit.lineStart"), m.actionCtrlFirst("edit.lineEnd"); start != "" || end != "" {
		parts = append(parts, joinSlash(start, end)+" line")
	}
	if k := m.actionCtrlFirst("edit.killLine"); k != "" {
		parts = append(parts, k+" kill")
	}
	if k := m.actionCtrlFirst("edit.newline"); k != "" {
		parts = append(parts, k+" newline")
	}
	if k := m.actionCtrlFirst("edit.openLine"); k != "" {
		parts = append(parts, k+" open")
	}
	if k := m.actionSlash("edit.external"); k != "" {
		parts = append(parts, k+" editor")
	}
	if k := m.actionFirst("edit.fill"); k != "" {
		parts = append(parts, k+" fill")
	}
	if kind == composerNote || kind == composerReply {
		if s := m.sendHintLabel(); s != "" {
			parts = append(parts, s+" send")
		}
		if tab := m.actionFirst("compose.nextField"); tab != "" {
			parts = append(parts, strings.ToUpper(tab[:1])+tab[1:]+" [Send]")
		}
	} else {
		parts = append(parts, "Enter send")
	}
	if esc := m.actionFirst("compose.cancel"); esc != "" {
		verb := "save draft"
		if kind == composerOutcome {
			verb = "cancel"
		}
		parts = append(parts, strings.ToUpper(esc[:1])+esc[1:]+" "+verb)
	}
	return strings.Join(parts, "  ")
}

func (m *Model) composerHelpLines() []string {
	var lines []string
	move := joinSlash(
		m.actionCtrlFirst("edit.charForward"),
		m.actionCtrlFirst("edit.charBackward"),
		m.actionCtrlFirst("edit.lineNext"),
		m.actionCtrlFirst("edit.linePrev"),
	)
	lineEnds := joinSlash(
		m.actionCtrlFirst("edit.lineStart"),
		m.actionCtrlFirst("edit.lineEnd"),
	)
	if move != "" || lineEnds != "" {
		var p []string
		if move != "" {
			p = append(p, move+" or arrows move")
		}
		if lineEnds != "" {
			p = append(p, lineEnds+" line")
		}
		lines = append(lines, strings.Join(p, "  "))
	}
	if line := m.composerEditHelpLine(); line != "" {
		lines = append(lines, line)
	}
	var line3 []string
	if k := m.actionOr("edit.external"); k != "" {
		line3 = append(line3, k+" edits the draft")
	}
	if k := m.actionFirst("edit.fill"); k != "" {
		line3 = append(line3, k+" fill")
	}
	if len(line3) > 0 {
		lines = append(lines, strings.Join(line3, "  "))
	}
	var sendLine []string
	if s := m.sendKeysHelp(", "); s != "" {
		sendLine = append(sendLine, s+" sends the draft")
	}
	if d := m.actionFirst("compose.discard"); d != "" {
		sendLine = append(sendLine, d+" discards")
	}
	if len(sendLine) > 0 {
		lines = append(lines, strings.Join(sendLine, "  "))
	}
	return lines
}

func (m *Model) composerEditHelpLine() string {
	var parts []string
	for _, spec := range []struct {
		action string
		label  string
	}{
		{"edit.deleteForward", "delete"},
		{"edit.killLine", "kill"},
		{"edit.newline", "newline"},
		{"edit.openLine", "open"},
	} {
		if k := m.actionCtrlFirst(spec.action); k != "" {
			parts = append(parts, k+" "+spec.label)
		}
	}
	return strings.Join(parts, "  ")
}

func joinSlash(parts ...string) string {
	var nonEmpty []string
	for _, p := range parts {
		if p != "" {
			nonEmpty = append(nonEmpty, p)
		}
	}
	return strings.Join(nonEmpty, "/")
}

func (m *Model) goToChordItems() (prefix string, items []struct{ key, name string }) {
	gotoActions := []struct {
		action string
		name   string
	}{
		{"goto.inbox", "inbox"},
		{"goto.snoozed", "snoozed"},
		{"goto.answered", "answered"},
		{"goto.done", "done"},
		{"goto.withdrawn", "withdrawn"},
		{"goto.status", "status"},
		{"goto.contacts", "contacts"},
		{"goto.settings", "settings"},
		{"goto.top", "top"},
	}
	if m.hasOutbox() {
		gotoActions = append(gotoActions[:len(gotoActions)-2],
			struct {
				action string
				name   string
			}{"goto.outbox", "outbox"},
			struct {
				action string
				name   string
			}{"goto.settings", "settings"},
			struct {
				action string
				name   string
			}{"goto.top", "top"},
		)
	}
	for _, ga := range gotoActions {
		for _, seq := range m.keyMap().Bindings(ga.action) {
			if len(seq) == 2 {
				if prefix == "" {
					prefix = seq[0].HelpString()
				}
				if seq[0].HelpString() == prefix {
					items = append(items, struct{ key, name string }{key: seq[1].HelpString(), name: ga.name})
				}
			}
		}
	}
	return prefix, items
}

func (m *Model) goToMenuLine(width int) string {
	entries := m.keyMap().ChordEntries(m.browseScope(), m.chordPrefix)
	// Preserve canonical order for go-to chords when the prefix matches goto.*.
	prefix, items := m.goToChordItems()
	if prefix == m.chordPrefix.HelpString() && len(items) > 0 {
		names := make([]string, len(items))
		keys := make([]string, len(items))
		for i, c := range items {
			keys[i] = c.key
			names[i] = c.key + " " + c.name
		}
		wide := "go to: " + strings.Join(names, " · ") + " · esc cancel"
		if width <= 0 || lipgloss.Width(wide) <= width {
			return wide
		}
		return "go to: " + strings.Join(keys, " ")
	}
	names := make([]string, len(entries))
	keys := make([]string, len(entries))
	for i, e := range entries {
		keys[i] = e.Next.HelpString()
		names[i] = keys[i] + " " + e.Action
	}
	wide := m.chordPrefix.HelpString() + ": " + strings.Join(names, " · ") + " · esc cancel"
	if width <= 0 || lipgloss.Width(wide) <= width {
		return wide
	}
	return m.chordPrefix.HelpString() + ": " + strings.Join(keys, " ")
}

func (m *Model) goToHelpLine() string {
	prefix, items := m.goToChordItems()
	if prefix == "" || len(items) == 0 {
		return ""
	}
	names := make([]string, len(items))
	for i, c := range items {
		names[i] = c.key + " " + c.name
	}
	return prefix + " then " + strings.Join(names, ", ")
}

func (m *Model) goToHintLabel() string {
	prefix, _ := m.goToChordItems()
	if prefix == "" {
		return ""
	}
	return prefix + " go to"
}

func (m *Model) helpHintLabel() string {
	h := m.actionSlash("help")
	if h == "" {
		return "? help"
	}
	return h + " help"
}

func appendHintIf(parts *[]string, val string) {
	if val != "" {
		*parts = append(*parts, val)
	}
}

func (m *Model) browseHintLine(label string) string {
	switch m.mode {
	case modeStatus:
		return label + m.statusHintLine()
	case modeContacts:
		return label + m.contactsHintLine()
	case modeSettings:
		return label + m.settingsHintLine()
	case modeDetail, modeOutcome:
		return label + m.detailHintLine()
	default:
		return label + m.listHintLine()
	}
}

func (m *Model) statusHintLine() string {
	var parts []string
	appendHintIf(&parts, m.goToHintLabel())
	if u := m.actionSlash("status.back"); u != "" {
		parts = append(parts, u+" list")
	}
	appendHintIf(&parts, m.actionSlash("status.compose"))
	parts = append(parts, "tab/text/[Send]", m.helpHintLabel())
	return strings.Join(parts, "  ")
}

func (m *Model) contactsHintLine() string {
	var parts []string
	appendHintIf(&parts, m.goToHintLabel())
	if u := m.actionSlash("contacts.back"); u != "" {
		parts = append(parts, u+" list")
	}
	if f := m.actionSlash("contacts.filter"); f != "" {
		parts = append(parts, f+" filter")
	}
	if m.narrow() {
		if i := m.actionSlash("contacts.details"); i != "" {
			parts = append(parts, i+" details")
		}
	} else if a := m.actionSlash("contacts.avatar"); a != "" {
		parts = append(parts, a+" avatar")
	}
	parts = append(parts, m.helpHintLabel())
	return strings.Join(parts, "  ")
}

func (m *Model) detailHintLine() string {
	var parts []string
	if q := m.actionSlash("quit"); q != "" {
		parts = append(parts, q+" quit")
	}
	jk := joinSlash(m.actionFirst("detail.scrollDown"), m.actionFirst("detail.scrollUp"))
	pg := joinSlash(m.actionFirst("detail.pageUp"), m.actionFirst("detail.pageDown"))
	he := joinSlash(m.actionFirst("detail.top"), m.actionLast("detail.bottom"))
	appendHintIf(&parts, strings.TrimSpace(strings.Join([]string{jk, pg, he}, " ")))
	appendHintIf(&parts, joinSlash(m.actionFirst("detail.nextMessage"), m.actionFirst("detail.prevMessage")))
	parts = append(parts, m.helpHintLabel())
	return strings.Join(parts, "  ")
}

func (m *Model) listHintLine() string {
	var parts []string
	appendHintIf(&parts, m.goToHintLabel())
	if o := m.actionFirst("list.open"); o != "" {
		parts = append(parts, o+" open")
	}
	if u := m.actionFirst("detail.back"); u != "" {
		parts = append(parts, u+" list")
	}
	appendHintIf(&parts, m.actionSlash("list.compose"))
	parts = append(parts, "tab/text/[Send]", m.helpHintLabel())
	return strings.Join(parts, "  ")
}

func (m *Model) actionLast(action string) string {
	seqs := m.keyMap().Bindings(action)
	if len(seqs) == 0 {
		return ""
	}
	return seqs[len(seqs)-1].HelpString()
}

func (m *Model) optionalHintExtras() []string {
	find := m.actionFirst("search") + "find"
	if m.actionFirst("search") == "" {
		find = ""
	}
	var extras []string
	switch m.mode {
	case modeSettings:
		return nil
	case modeStatus, modeContacts:
		if find != "" {
			extras = append(extras, find)
		}
	case modeDetail, modeOutcome:
		return m.detailOptionalHints(find)
	default:
		if m.isInboxFolder() && m.unreadCount > 0 {
			extras = append(extras, fmt.Sprintf("unread: %d", m.unreadCount))
		}
		if find != "" {
			extras = append(extras, find)
		}
		if b := m.actionFirst("list.snooze"); b != "" {
			extras = append(extras, b+" snooze")
		}
	}
	return extras
}

func (m *Model) detailOptionalHints(find string) []string {
	var extras []string
	if find != "" {
		extras = append(extras, find)
	}
	if b := m.actionFirst("detail.snooze"); b != "" {
		extras = append(extras, b+" snooze")
	}
	if g := m.goToHintLabel(); g != "" {
		extras = append(extras, g)
	}
	if o := m.actionFirst("detail.owner"); o != "" {
		extras = append(extras, o+" owner")
	}
	return extras
}

func (m *Model) gotoTopLabel() string {
	s := m.actionFirst("goto.top")
	if len(s) == 3 && s[1] == ' ' {
		return s[:1] + s[2:]
	}
	return s
}

func (m *Model) helpTextLines() []string {
	text := []string{"shortcuts"}
	if q := m.actionSlash("quit"); q != "" {
		text = append(text, q+" quit")
	}
	switch m.mode {
	case modeStatus:
		text = append(text, m.statusHelpLines()...)
	case modeContacts:
		text = append(text, m.contactsHelpLines()...)
	case modeOutbox:
		text = append(text, m.outboxHelpLines()...)
	case modeSettings:
		text = append(text, m.settingsHelpLines()...)
	default:
		text = append(text, m.listOrDetailHelpLines()...)
	}
	cancel := m.actionFirst("compose.cancel")
	if i := slices.Index(text, cancel+" keeps the draft"); i >= 0 && m.mode == modeStatus {
		text = slices.Insert(text, i+1, m.composerHelpLines()...)
	}
	return text
}

func appendHelpLabeled(lines *[]string, key, suffix string) {
	if key != "" {
		*lines = append(*lines, key+suffix)
	}
}

func (m *Model) statusHelpLines() []string {
	jk := joinSlash(m.actionFirst("status.next"), m.actionFirst("status.prev"))
	pg := joinSlash(m.actionFirst("status.pageUp"), m.actionFirst("status.pageDown"))
	he := joinSlash(m.actionFirst("status.top"), m.actionLast("status.bottom"))
	gg := joinSlash(m.gotoTopLabel(), m.actionFirst("status.bottom"))
	lines := []string{strings.TrimSpace(jk + " move  " + pg + "  " + he + "  " + gg)}
	appendHelpLabeled(&lines, m.goToHelpLine(), "")
	appendHelpLabeled(&lines, m.actionSlash("status.back"), " returns to the list")
	appendHelpLabeled(&lines, m.actionSlash("status.compose"), " compose")
	appendHelpLabeled(&lines, m.actionSlash("status.togglePanel"), " toggles the selected panel")
	appendHelpLabeled(&lines, joinSlash(m.actionFirst("status.movePanelDown"), m.actionFirst("status.movePanelUp")), " moves the selected panel down/up")
	lines = append(lines, m.composeHelpFooterLines(true)...)
	appendHelpLabeled(&lines, m.actionSlash("search"), " filters as you type, esc restores, empty enter clears")
	lines = append(lines, "shift+drag selects text")
	appendHelpLabeled(&lines, m.actionSlash("mouse"), " toggles mouse capture")
	lines = append(lines, "click folder, target, or [Send]", "wheel scroll", m.actionSlash("help")+" or esc close")
	return lines
}

func (m *Model) contactsHelpLines() []string {
	jk := joinSlash(m.actionFirst("contacts.next"), m.actionFirst("contacts.prev"))
	pg := joinSlash(m.actionFirst("contacts.pageUp"), m.actionFirst("contacts.pageDown"))
	he := joinSlash(m.actionFirst("contacts.top"), m.actionLast("contacts.bottom"))
	gg := joinSlash(m.gotoTopLabel(), m.actionFirst("contacts.bottom"))
	lines := []string{strings.TrimSpace(jk + " move  " + pg + "  " + he + "  " + gg)}
	appendHelpLabeled(&lines, m.goToHelpLine(), "")
	appendHelpLabeled(&lines, m.actionSlash("contacts.filter"), " filters every folder to this agent")
	appendHelpLabeled(&lines, m.actionSlash("contacts.clearFilter"), " clears the agent filter")
	appendHelpLabeled(&lines, m.actionSlash("contacts.compose"), " compose")
	appendHelpLabeled(&lines, m.actionSlash("contacts.star"), " stars this contact; starred names lead the recipient list")
	appendHelpLabeled(&lines, m.actionSlash("contacts.avatar"), " picks this agent's avatar")
	appendHelpLabeled(&lines, m.actionSlash("contacts.details"), " shows or hides the details on a narrow screen")
	appendHelpLabeled(&lines, m.actionSlash("contacts.orgTree"), " switches between the list and the org tree (ui.orgTree)")
	appendHelpLabeled(&lines, m.actionSlash("contacts.alias"), " aliases this contact to a bus name")
	appendHelpLabeled(&lines, m.actionSlash("contacts.hide"), " hides this contact")
	appendHelpLabeled(&lines, m.actionSlash("contacts.showHidden"), " shows hidden contacts")
	appendHelpLabeled(&lines, m.actionSlash("contacts.hideDead"), " hides every dead contact")
	appendHelpLabeled(&lines, m.actionSlash("contacts.release"), " releases a dead bus name")
	appendHelpLabeled(&lines, m.actionSlash("contacts.back"), " returns to the list")
	appendHelpLabeled(&lines, m.actionSlash("search"), " filters as you type, esc restores, empty enter clears")
	appendHelpLabeled(&lines, m.actionSlash("mouse"), " toggles mouse capture")
	lines = append(lines, "click a folder", "wheel scroll", m.actionSlash("help")+" or esc close")
	return lines
}

func (m *Model) composeHelpFooterLines(isList bool) []string {
	nextField := m.actionFirst("compose.nextField")
	prevField := m.actionFirst("compose.prevField")
	cancel := m.actionFirst("compose.cancel")
	pNext := m.actionSlash("compose.pickerNext")
	pPrev := m.actionSlash("compose.pickerPrev")
	newline := "enter on text inserts a newline"
	if !isList {
		newline = "enter in the reply or note text inserts a newline, " + nextField + " reaches [Send]"
	}
	return []string{
		nextField + " moves recipient, text, [Send]",
		prevField + " moves back",
		"typing filters recipients  " + pPrev + "/" + pNext + " move  enter picks",
		newline,
		"enter on [Send] sends",
		cancel + " keeps the draft",
	}
}

func (m *Model) listOrDetailHelpLines() []string {
	isDetail := m.mode == modeDetail || m.mode == modeOutcome
	var lines []string
	if isDetail {
		jk := joinSlash(m.actionFirst("detail.scrollDown"), m.actionFirst("detail.scrollUp"))
		pg := joinSlash(m.actionFirst("detail.pageUp"), m.actionFirst("detail.pageDown"))
		he := joinSlash(m.actionFirst("detail.top"), m.gotoTopLabel(), m.actionLast("detail.bottom"), m.actionFirst("detail.bottom"))
		np := joinSlash(m.actionFirst("detail.nextMessage"), m.actionFirst("detail.prevMessage"))
		lines = append(lines, strings.TrimSpace(jk+" up/down line  "+pg+" page  "+he+"  "+np))
	} else {
		jk := joinSlash(m.actionFirst("list.next"), m.actionFirst("list.prev"))
		pg := joinSlash(m.actionFirst("list.pageUp"), m.actionFirst("list.pageDown"))
		he := joinSlash(m.actionFirst("list.top"), m.actionLast("list.bottom"))
		gg := joinSlash(m.gotoTopLabel(), m.actionFirst("list.bottom"))
		lines = append(lines, strings.TrimSpace(jk+" move  "+pg+"  "+he+"  "+gg))
	}
	appendHelpLabeled(&lines, m.goToHelpLine(), "")
	if isDetail {
		lines = append(lines, m.actionFirst("detail.owner")+" jumps to the owner's herdr tab    "+m.actionFirst("detail.back")+" returns to the list")
	} else {
		lines = append(lines, m.actionOr("list.open")+" opens    "+m.actionFirst("detail.back")+" returns to the list")
	}
	appendHelpLabeled(&lines, m.actionSlash("list.compose"), " compose")
	lines = append(lines, m.composeHelpFooterLines(!isDetail)...)
	lines = append(lines, m.composerHelpLines()...)
	itemActs := []string{"1-9 answer"}
	for _, spec := range []struct {
		action string
		label  string
	}{
		{"detail.reply", "reply"},
		{"detail.forward", "forward"},
		{"detail.archive", "archive"},
		{"detail.outcome", "archive with note"},
		{"detail.snooze", "snooze"},
	} {
		if k := m.actionSlash(spec.action); k != "" {
			itemActs = append(itemActs, k+" "+spec.label)
		}
	}
	lines = append(lines, strings.Join(itemActs, "  "))
	if isDetail {
		lines = append(lines, m.actionSlash("search")+" highlights as you type, esc restores, empty enter clears")
	} else {
		lines = append(lines, m.actionSlash("search")+" filters as you type, esc restores, empty enter clears")
	}
	lines = append(lines,
		m.actionSlash("detail.openLink")+" opens a link, or picks one: j/k or a number, enter opens",
		m.actionSlash("detail.copyLink")+" copies the link   "+m.actionSlash("detail.copyMenu")+" copy menu   "+m.actionSlash("detail.plain")+" plain view   "+m.actionSlash("detail.avatar")+" — set avatar for the selected sender",
		"shift+drag selects text",
	)
	appendHelpLabeled(&lines, m.actionSlash("mouse"), " toggles mouse capture")
	lines = append(lines, "esc back", "click folder, row, option, target, or [Send]", "wheel scroll", m.actionSlash("help")+" or esc close")
	return lines
}
