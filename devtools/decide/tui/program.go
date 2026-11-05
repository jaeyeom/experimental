package tui

import (
	"errors"
	"fmt"
	"io"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

// inputMsg is one event from the existing reader. Mouse parsing stays here
// until the bubbletea mouse issue replaces it. Compose keys stay here until
// the compose field issue replaces that view.
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

func (m *Model) updateInput(ev termEvent) (tea.Model, tea.Cmd) {
	if ev.mouse {
		if ev.wheel != 0 {
			m.Wheel(ev.wheel > 0)
			return m, nil
		}
		if !ev.press {
			return m, nil
		}
		if err := m.Click(ev.x, ev.y); err != nil {
			m.runErr = err
			return m, tea.Quit
		}
		return m, nil
	}
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

func forwardInputs(p *tea.Program, events <-chan termEvent, readErr <-chan error) {
	for {
		select {
		case ev := <-events:
			p.Send(inputMsg{ev: ev})
		case err := <-readErr:
			p.Send(readDoneMsg{err: err})
			return
		}
	}
}
