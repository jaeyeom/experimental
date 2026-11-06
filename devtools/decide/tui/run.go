package tui

import (
	"fmt"
	"io"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

// Options configure Run. In and Out default to the caller. Interval defaults
// to DefaultInterval. Targets and SendTemplate come from decide config.
// An empty SendTemplate is `herdr agent prompt {pane}` plus the note as its
// own argument.
type Options struct {
	Dir          string
	In           io.Reader
	Out          io.Writer
	Interval     time.Duration
	Now          func() time.Time
	Targets      []Target
	SendTemplate string
}

// Run draws the inbox until q or EOF. It reloads the ledger on Interval,
// including writes from another process. The screen is a bubbletea program.
// Bubbletea reads keys and mouse input.
func Run(opts Options) error {
	if opts.Interval <= 0 {
		opts.Interval = DefaultInterval
	}
	m, err := prepare(opts)
	if err != nil {
		return err
	}
	m.interval = opts.Interval
	if rows := termRows(opts.In); rows > 0 {
		m.height = rows
	}

	p := tea.NewProgram(m,
		tea.WithInput(opts.In),
		tea.WithOutput(opts.Out),
		tea.WithAltScreen(),
		tea.WithoutSignalHandler(),
		tea.WithMouseCellMotion(),
	)

	final, err := p.Run()
	if err != nil {
		return fmt.Errorf("decide inbox: %w", err)
	}
	done, ok := final.(*Model)
	if ok && done.runErr != nil {
		return done.runErr
	}
	return nil
}

func prepare(opts Options) (*Model, error) {
	if opts.In == nil || opts.Out == nil {
		return nil, fmt.Errorf("decide inbox input or output is nil")
	}
	m := New(opts.Dir)
	m.Targets = opts.Targets
	m.SendTemplate = opts.SendTemplate
	if opts.Now != nil {
		m.Now = opts.Now
	}
	if err := m.Reload(); err != nil {
		return nil, err
	}
	return m, nil
}
