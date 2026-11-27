package tui

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/version"
)

type settingKind int

const (
	settingBool settingKind = iota
	settingEnum
	settingInt
)

type settingDef struct {
	key        string
	kind       settingKind
	choices    []string
	defaultVal string
	desc       string
}

func settingsCatalog() []settingDef {
	return []settingDef{
		{
			key:        "ui.answerAction",
			kind:       settingEnum,
			choices:    []string{decide.AnswerActionStay, decide.AnswerActionList, decide.AnswerActionEdit},
			defaultVal: decide.AnswerActionStay,
			desc:       "What happens after answering an item (stay, list, or edit).",
		},
		{
			key:        "ui.openAt",
			kind:       settingEnum,
			choices:    []string{decide.OpenAtNew, decide.OpenAtLast, decide.OpenAtTop},
			defaultVal: decide.OpenAtNew,
			desc:       "Where a thread opens (new, last, or top).",
		},
		{
			key:        "ui.utc",
			kind:       settingBool,
			defaultVal: "false",
			desc:       "Show times in UTC instead of local time.",
		},
		{
			key:        "messageAvatars",
			kind:       settingBool,
			defaultVal: "true",
			desc:       "Show sender avatar beside each thread message.",
		},
		{
			key:        "hyperlinks",
			kind:       settingBool,
			defaultVal: "true",
			desc:       "Wrap URLs in terminal OSC 8 hyperlinks.",
		},
		{
			key:        "ui.asciiKeys",
			kind:       settingBool,
			defaultVal: "false",
			desc:       "Use ASCII key labels on buttons instead of Unicode symbols.",
		},
		{
			key:        "ui.orgTree",
			kind:       settingBool,
			defaultVal: "false",
			desc:       "Allow org tree view in Contacts.",
		},
		{
			key:        "ui.clipboard",
			kind:       settingEnum,
			choices:    []string{decide.ClipboardAuto, decide.ClipboardOSC52, decide.ClipboardCommand, decide.ClipboardOff},
			defaultVal: decide.ClipboardAuto,
			desc:       "How the inbox copies text (auto, osc52, command, or off).",
		},
		{
			key:        "ui.fillColumn",
			kind:       settingInt,
			defaultVal: strconv.Itoa(defaultFillColumn),
			desc:       "Wrap width for paragraph fill in compose boxes.",
		},
	}
}

func (m *Model) showSettings() error {
	m.flushAllDrafts()
	m.composing = false
	m.answerFocus = false
	m.replyFocus = false
	m.replySend = false
	m.detail = nil
	m.settingsEditing = false
	m.settingsEditBuf = ""
	m.mode = modeSettings
	m.scroll = 0
	m.loadPanelsOnce()
	m.loadDashboardConfigOnce()
	m.clampSettingsCursor()
	m.status = ""
	return nil
}

func (m *Model) clampSettingsCursor() {
	n := len(settingsCatalog())
	if n == 0 {
		m.settingsCursor = 0
		return
	}
	if m.settingsCursor < 0 {
		m.settingsCursor = 0
	}
	if m.settingsCursor >= n {
		m.settingsCursor = n - 1
	}
}

func (m *Model) moveSettings(delta int) {
	n := len(settingsCatalog())
	if n == 0 {
		m.settingsCursor = 0
		return
	}
	m.settingsCursor += delta
	m.clampSettingsCursor()
}

func (m *Model) settingValue(def settingDef) string {
	switch def.key {
	case "ui.answerAction":
		if m.answerAction == "" {
			return decide.AnswerActionStay
		}
		return m.answerAction
	case "ui.openAt":
		if m.openAt == "" {
			return decide.OpenAtNew
		}
		return m.openAt
	case "ui.utc":
		return strconv.FormatBool(m.UTC)
	case "messageAvatars":
		return strconv.FormatBool(m.MessageAvatars)
	case "hyperlinks":
		return strconv.FormatBool(!m.NoHyperlinks)
	case "ui.asciiKeys":
		return strconv.FormatBool(m.ASCIIKeys)
	case "ui.orgTree":
		return strconv.FormatBool(m.orgTreeOn)
	case "ui.clipboard":
		if m.clipboard == "" {
			return decide.ClipboardAuto
		}
		return m.clipboard
	case "ui.fillColumn":
		if m.fillColumn <= 0 {
			return strconv.Itoa(defaultFillColumn)
		}
		return strconv.Itoa(m.fillColumn)
	default:
		return ""
	}
}

func (m *Model) settingBoolValue(key string) bool {
	switch key {
	case "ui.utc":
		return m.UTC
	case "messageAvatars":
		return m.MessageAvatars
	case "hyperlinks":
		return !m.NoHyperlinks
	case "ui.asciiKeys":
		return m.ASCIIKeys
	case "ui.orgTree":
		return m.orgTreeOn
	default:
		return false
	}
}

func (m *Model) applySetting(key string, cfg decide.Config) {
	switch key {
	case "ui.answerAction":
		m.answerAction = cfg.UI.AnswerAction
	case "ui.openAt":
		m.openAt = cfg.UI.OpenAt
	case "ui.utc":
		m.UTC = cfg.UI.UTC
	case "messageAvatars":
		m.MessageAvatars = cfg.MessageAvatarsOn()
	case "hyperlinks":
		m.NoHyperlinks = !cfg.HyperlinksOn()
	case "ui.asciiKeys":
		m.ASCIIKeys = cfg.UI.ASCIIKeys
	case "ui.orgTree":
		m.orgTreeOn = cfg.UI.OrgTree
		if !m.orgTreeOn {
			m.orgView = false
			m.orgRoot = nil
		}
	case "ui.clipboard":
		m.clipboard = cfg.UI.Clipboard
	case "ui.fillColumn":
		m.fillColumn = cfg.UI.FillColumn
	}
}

func (m *Model) applyConfig(cfg decide.Config) {
	m.answerAction = cfg.UI.AnswerAction
	m.openAt = cfg.UI.OpenAt
	m.UTC = cfg.UI.UTC
	m.MessageAvatars = cfg.MessageAvatarsOn()
	m.NoHyperlinks = !cfg.HyperlinksOn()
	m.ASCIIKeys = cfg.UI.ASCIIKeys
	m.orgTreeOn = cfg.UI.OrgTree
	m.orgSpec = cfg.UI.OrgSpec
	if !m.orgTreeOn {
		m.orgView = false
		m.orgRoot = nil
	}
	m.clipboard = cfg.UI.Clipboard
	m.fillColumn = cfg.UI.FillColumn
	m.WebPublicURL = cfg.Web.PublicURL
	m.OpenCommand = cfg.OpenCommand
	m.SendTemplate = cfg.SendTemplate
	m.LateDeliveryAfter = cfg.LateDeliveryAfter()
	m.OutboxStaleAfter = cfg.OutboxStaleAfter()
	m.archiveGrace = cfg.ArchiveGrace()
	m.healthEvery = cfg.BusHealthInterval()
	m.dashboardPanels = cfg.UI.Dashboard.Panels
	m.dashboardLoaded = true
	if km, err := decide.ResolveKeyMap(cfg.UI.Keys); err == nil {
		m.SetKeys(km)
	}
}

func (m *Model) saveSetting(def settingDef, value any) {
	cfg, err := decide.SetConfigValue(def.key, value)
	if err != nil {
		m.status = err.Error()
		return
	}
	m.applySetting(def.key, cfg)
	m.status = fmt.Sprintf("saved %s = %s", def.key, m.settingValue(def))
}

func (m *Model) handleSettingsKey(s decide.KeyStroke) (bool, error) {
	if s.Ctrl || s.Alt {
		return false, nil
	}
	defs := settingsCatalog()
	m.clampSettingsCursor()
	switch s.Key {
	case "j", "down":
		m.moveSettings(1)
		return true, nil
	case "k", "up":
		m.moveSettings(-1)
		return true, nil
	case "home":
		m.settingsCursor = 0
		return true, nil
	case "end":
		m.settingsCursor = len(defs) - 1
		return true, nil
	case "u", "esc":
		m.backToList()
		return true, nil
	case "e":
		m.editConfigWant = true
		return true, nil
	case "enter", "space", " ":
		m.activateSetting(defs[m.settingsCursor])
		return true, nil
	case "right":
		m.stepSetting(defs[m.settingsCursor], 1)
		return true, nil
	case "left":
		m.stepSetting(defs[m.settingsCursor], -1)
		return true, nil
	}
	return false, nil
}

func (m *Model) activateSetting(def settingDef) {
	switch def.kind {
	case settingBool:
		m.saveSetting(def, !m.settingBoolValue(def.key))
	case settingEnum:
		m.stepEnumSetting(def, 1)
	case settingInt:
		m.settingsEditing = true
		m.settingsEditBuf = m.settingValue(def)
		m.status = ""
	}
}

func (m *Model) stepSetting(def settingDef, delta int) {
	switch def.kind {
	case settingEnum:
		m.stepEnumSetting(def, delta)
	case settingBool:
		m.saveSetting(def, !m.settingBoolValue(def.key))
	}
}

func (m *Model) stepEnumSetting(def settingDef, delta int) {
	cur := m.settingValue(def)
	idx := slices.Index(def.choices, cur)
	if idx < 0 {
		idx = 0
	}
	n := len(def.choices)
	next := def.choices[(idx+delta+n)%n]
	m.saveSetting(def, next)
}

func (m *Model) handleSettingsEditKey(k tea.KeyMsg) {
	switch k.Type {
	case tea.KeyEsc:
		m.settingsEditing = false
		m.settingsEditBuf = ""
		m.status = ""
	case tea.KeyEnter:
		m.commitSettingsEdit()
	case tea.KeyBackspace:
		if len(m.settingsEditBuf) > 0 {
			m.settingsEditBuf = m.settingsEditBuf[:len(m.settingsEditBuf)-1]
		}
	case tea.KeyRunes:
		for _, r := range k.Runes {
			if r >= '0' && r <= '9' {
				m.settingsEditBuf += string(r)
			}
		}
	}
}

func (m *Model) settingsEditRune(r rune) {
	switch r {
	case '\x1b':
		m.settingsEditing = false
		m.settingsEditBuf = ""
		m.status = ""
	case '\r', '\n':
		m.commitSettingsEdit()
	case '\b', '\x7f':
		if len(m.settingsEditBuf) > 0 {
			m.settingsEditBuf = m.settingsEditBuf[:len(m.settingsEditBuf)-1]
		}
	default:
		if r >= '0' && r <= '9' {
			m.settingsEditBuf += string(r)
		}
	}
}

func (m *Model) commitSettingsEdit() {
	defs := settingsCatalog()
	m.clampSettingsCursor()
	def := defs[m.settingsCursor]
	raw := strings.TrimSpace(m.settingsEditBuf)
	if raw == "" {
		m.status = def.key + " must be a number"
		return
	}
	n, err := strconv.Atoi(raw)
	if err != nil || n < 0 {
		m.status = def.key + " must be 0 or more"
		return
	}
	m.settingsEditing = false
	m.settingsEditBuf = ""
	m.saveSetting(def, n)
}

func (m *Model) settingsMail(pane int) []screenLine {
	if m.width <= 0 {
		return stackRegions(0, m.sidebarLines(), m.settingsScreen(0))
	}
	if m.narrow() {
		return stackRegions(pane, []screenLine{m.folderTabLine(pane)}, m.settingsScreen(pane))
	}
	sidebar := m.sidebarLines()
	sideW, mainW := listColumnWidths(maxWidth(sidebar), pane)
	cols := [][]screenLine{
		fitColumnLines(sidebar, sideW),
		m.settingsScreen(mainW),
	}
	return zipFixed(cols, []int{sideW, mainW})
}

func (m *Model) settingsScreen(width int) []screenLine {
	defs := settingsCatalog()
	m.clampSettingsCursor()
	header := "Settings"
	if m.Hub != "" {
		header = "Settings — edits this machine's config (hub: " + m.Hub + ")"
	}
	lines := []screenLine{
		{text: boldOn + header + boldOff},
		{text: ""},
	}
	nameW := 0
	valW := 0
	vals := make([]string, len(defs))
	for i, def := range defs {
		if len(def.key) > nameW {
			nameW = len(def.key)
		}
		v := m.settingValue(def)
		switch {
		case m.settingsEditing && i == m.settingsCursor:
			vals[i] = "[" + m.settingsEditBuf + "_]"
		case v == def.defaultVal:
			vals[i] = v + " (default)"
		default:
			vals[i] = v
		}
		if len(vals[i]) > valW {
			valW = len(vals[i])
		}
	}
	for i, def := range defs {
		prefix := "  "
		if i == m.settingsCursor {
			prefix = "> "
		}
		line := fmt.Sprintf("%s%-*s  %-*s  %s", prefix, nameW, def.key, valW, vals[i], def.desc)
		lines = append(lines, screenLine{text: line})
	}
	lines = append(lines, screenLine{text: ""})
	lines = append(lines, m.settingsDashboardPanelLines()...)
	lines = append(lines, screenLine{text: ""})
	lines = append(lines, m.settingsFooterLines()...)
	_, limit := m.layoutBudget()
	lines, m.scroll = clipLines(lines, m.scroll, limit)
	if width > 0 {
		return fitColumnLines(lines, width)
	}
	return lines
}

func (m *Model) settingsFooterLines() []screenLine {
	cfgPath := "(unknown)"
	if p, ok := decide.ConfigPath(); ok {
		cfgPath = p
	}
	lines := []screenLine{
		{text: "  config:  " + cfgPath},
	}
	if m.Hub != "" {
		lines = append(lines, screenLine{text: "  hub:     " + m.Hub})
	} else {
		machine := m.Machine
		if machine == "" {
			if host, err := os.Hostname(); err == nil && host != "" {
				machine = host
			} else {
				machine = "local"
			}
		}
		lines = append(lines, screenLine{text: "  machine: " + machine})
	}
	if m.WebPublicURL != "" {
		lines = append(lines, screenLine{text: "  web:     " + m.WebPublicURL})
	}
	ver := m.Version
	if ver == "" {
		ver = version.Resolve()
	}
	lines = append(lines, screenLine{text: "  version: " + ver})
	return lines
}

func (m *Model) settingsDashboardPanelLines() []screenLine {
	m.loadPanelsOnce()
	m.loadDashboardConfigOnce()
	resolved := decide.ResolveDashboardPanels(m.dashboardPanels, m.allDashboardPanelIDs())
	titles := map[string]string{}
	for _, p := range m.panels {
		if p.ID != "" && p.Title != "" {
			titles[p.ID] = p.Title
		}
	}
	lines := []screenLine{
		{text: boldOn + "Dashboard panels" + boldOff + "  (Status: space toggle, J/K reorder)"},
	}
	for _, st := range resolved {
		state := "enabled"
		if !st.Enabled {
			state = "disabled"
		}
		title := titles[st.ID]
		if title == "" {
			title = decide.BuiltinPanelTitle(st.ID)
		}
		line := fmt.Sprintf("  %d. %-16s  %-8s  %s", st.Order+1, st.ID, state, title)
		lines = append(lines, screenLine{text: line})
	}
	return lines
}

func (m *Model) settingsHintLine() string {
	if m.settingsEditing {
		return "enter save  esc cancel"
	}
	return joinFooter("enter/space toggle  left/right cycle  e edit config.json  u/esc list", m.helpHintLabel())
}

func (m *Model) settingsHelpLines() []string {
	lines := []string{
		"j/k or up/down move",
		"enter/space toggle bool, cycle enum, or edit number",
		"left/right step through choices",
		"e opens config.json in $EDITOR and reloads",
	}
	appendHelpLabeled(&lines, m.goToHelpLine(), "")
	lines = append(lines,
		"u or esc returns to the list",
		m.actionSlash("help")+" or esc close",
	)
	return lines
}

// configEditorDoneMsg is the result of editing config.json in $EDITOR.
type configEditorDoneMsg struct {
	err error
}

// configEdit runs $EDITOR directly on config.json.
type configEdit struct {
	m      *Model
	prog   string
	args   []string
	stdin  io.Reader
	stdout io.Writer
	stderr io.Writer
}

func (e *configEdit) Run() error {
	if e.m.suspend != nil {
		e.m.suspend()
	}
	if e.m.resume != nil {
		defer e.m.resume()
	}
	// $VISUAL, $EDITOR, or nano. The operator chooses the program. Arguments are not a shell string.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(e.prog, e.args...) //nolint:gosec // G204: operator editor, arguments are not a shell string
	cmd.Stdin = e.stdin
	if !editorUsesTerminal(e.prog, e.args) {
		cmd.Stdin = nil
	}
	cmd.Stdout = e.stdout
	cmd.Stderr = e.stderr
	cmd.WaitDelay = editorWaitDelay
	if err := cmd.Run(); err != nil && !errors.Is(err, exec.ErrWaitDelay) {
		return fmt.Errorf("run editor: %w", err)
	}
	return nil
}

func (e *configEdit) SetStdin(r io.Reader) {
	if f, ok := r.(ctrlEnterFile); ok {
		r = f.File
	}
	e.stdin = r
}

func (e *configEdit) SetStdout(w io.Writer) { e.stdout = w }
func (e *configEdit) SetStderr(w io.Writer) { e.stderr = w }

func (m *Model) editConfigCmd() tea.Cmd {
	if !m.editConfigWant {
		return nil
	}
	m.editConfigWant = false
	path, ok := decide.ConfigPath()
	if !ok {
		m.status = "decide config: no home directory"
		return nil
	}
	prog, args, err := editorCommand()
	if err != nil {
		m.status = "editor not found"
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		m.status = err.Error()
		return nil
	}
	edit := &configEdit{
		m:    m,
		prog: prog,
		args: append(append([]string{}, args...), path),
	}
	m.configEditor = edit
	return tea.Exec(edit, func(err error) tea.Msg {
		return configEditorDoneMsg{err: err}
	})
}

func (m *Model) finishConfigEdit(err error) {
	m.configEditor = nil
	if err != nil {
		m.status = err.Error()
		return
	}
	cfg, loadErr := decide.LoadConfig()
	if loadErr != nil {
		m.status = loadErr.Error()
		return
	}
	m.applyConfig(cfg)
	m.status = "reloaded config.json"
}
