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
	case '\r', '\n':
		return tea.KeyMsg{Type: tea.KeyEnter}
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
func (m *Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		if msg.Height > 0 {
			m.height = msg.Height
		}
		if msg.Width > 0 {
			m.width = msg.Width
		}
		return m, nil
	case tea.KeyMsg:
		if m.chord {
			if _, ok := keyRune(msg); !ok {
				m.expireChord(m.chordSeq)
				return m, nil
			}
			return m.updateKey(msg)
		}
		if msg.Type == tea.KeyUp {
			m.pickerArrow(-1)
			return m, nil
		}
		if msg.Type == tea.KeyDown {
			m.pickerArrow(1)
			return m, nil
		}
		return m.updateKey(msg)
	case chordTimeoutMsg:
		m.expireChord(int(msg))
		return m, nil
	case tea.MouseMsg:
		return m.updateMouse(msg)
	case reloadMsg:
		return m.updateReload()
	default:
		return m, nil
	}
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
	if m.chord && m.chordSeq != seq {
		return m.scheduleChord()
	}
	return nil
}

func (m *Model) updateMouse(msg tea.MouseMsg) (tea.Model, tea.Cmd) {
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

func (m *Model) scheduleReload() tea.Cmd {
	interval := m.interval
	if interval <= 0 {
		interval = DefaultInterval
	}
	return tea.Tick(interval, func(time.Time) tea.Msg {
		return reloadMsg{}
	})
}
