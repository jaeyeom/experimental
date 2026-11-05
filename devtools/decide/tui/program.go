package tui

import (
	"errors"
	"fmt"
	"io"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

// inputMsg is one key from the existing reader. Pointer input is a bubbletea
// mouse message so Shift+Enter parsing stays in the reader.
type inputMsg struct {
	ev termEvent
}

type readDoneMsg struct {
	err error
}

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
		return m, nil
	case tea.KeyMsg:
		return m.updateKey(msg)
	case tea.MouseMsg:
		return m.updateMouse(msg)
	case inputMsg:
		return m.updateInput(msg.ev)
	case readDoneMsg:
		return m.updateReadDone(msg.err)
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

func (m *Model) applyRune(r rune) tea.Cmd {
	quit, err := m.Key(r)
	if err != nil {
		m.runErr = err
		return tea.Quit
	}
	if quit {
		return tea.Quit
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

func (m *Model) updateInput(ev termEvent) (tea.Model, tea.Cmd) {
	if ev.shiftEnter {
		m.ShiftEnter()
		return m, nil
	}
	if ev.shiftTab {
		m.ShiftTab()
		return m, nil
	}
	if cmd := m.applyRune(ev.key); cmd != nil {
		return m, cmd
	}
	return m, nil
}

func (m *Model) updateReadDone(err error) (tea.Model, tea.Cmd) {
	if err != nil && !errors.Is(err, io.EOF) {
		m.runErr = fmt.Errorf("read decide inbox key: %w", err)
	}
	return m, tea.Quit
}

func (m *Model) updateReload() (tea.Model, tea.Cmd) {
	if err := m.Reload(); err != nil {
		m.runErr = err
		return m, tea.Quit
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

func mouseMessage(ev termEvent) tea.MouseMsg {
	msg := tea.MouseMsg{
		X:      ev.x - 1,
		Y:      ev.y - 1,
		Action: tea.MouseActionPress,
		Button: tea.MouseButtonLeft,
	}
	if !ev.press {
		msg.Action = tea.MouseActionRelease
	}
	switch ev.wheel {
	case 1:
		msg.Button = tea.MouseButtonWheelDown
	case -1:
		msg.Button = tea.MouseButtonWheelUp
	}
	return msg
}

func forwardInputs(p *tea.Program, events <-chan termEvent, readErr <-chan error) {
	for {
		select {
		case ev := <-events:
			if ev.mouse {
				p.Send(mouseMessage(ev))
				continue
			}
			p.Send(inputMsg{ev: ev})
		case err := <-readErr:
			p.Send(readDoneMsg{err: err})
			return
		}
	}
}
