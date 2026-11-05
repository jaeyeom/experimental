package tui

import (
	"errors"
	"fmt"
	"io"
	"time"
)

// Options configure Run. In and Out default to the caller. Interval defaults
// to DefaultInterval.
type Options struct {
	Dir      string
	In       io.Reader
	Out      io.Writer
	Interval time.Duration
	Now      func() time.Time
}

// Run draws the inbox until q or EOF. It reloads the ledger on Interval,
// including writes from another process.
func Run(opts Options) error {
	if opts.Interval <= 0 {
		opts.Interval = DefaultInterval
	}
	m, err := prepare(opts)
	if err != nil {
		return err
	}
	restore, raw, err := makeRaw(opts.In)
	if err != nil {
		return err
	}
	defer restore()
	if raw {
		if _, err := io.WriteString(opts.Out, mouseOn); err != nil {
			return fmt.Errorf("enable decide mouse: %w", err)
		}
		defer func() { _, _ = io.WriteString(opts.Out, mouseOff) }()
	}
	if rows := termRows(opts.In); rows > 0 {
		m.height = rows
	}

	events := make(chan termEvent, 8)
	readErr := make(chan error, 1)
	go readEvents(opts.In, events, readErr)
	ticker := time.NewTicker(opts.Interval)
	defer ticker.Stop()
	for {
		if err := WriteFrame(opts.Out, m.View()); err != nil {
			return err
		}
		quit, err := waitEvent(m, ticker, events, readErr)
		if err != nil || quit {
			return err
		}
	}
}

func prepare(opts Options) (*Model, error) {
	if opts.In == nil || opts.Out == nil {
		return nil, fmt.Errorf("decide inbox input or output is nil")
	}
	m := New(opts.Dir)
	if opts.Now != nil {
		m.Now = opts.Now
	}
	if err := m.Reload(); err != nil {
		return nil, err
	}
	return m, nil
}

func waitEvent(m *Model, ticker *time.Ticker, events <-chan termEvent, readErr <-chan error) (bool, error) {
	select {
	case ev := <-events:
		if ev.mouse {
			if ev.wheel != 0 {
				m.Wheel(ev.wheel > 0)
				return false, nil
			}
			if !ev.press {
				return false, nil
			}
			return false, m.Click(ev.x, ev.y)
		}
		return m.Key(ev.key)
	case <-ticker.C:
		return false, m.Reload()
	case err := <-readErr:
		if errors.Is(err, io.EOF) {
			return true, nil
		}
		return false, fmt.Errorf("read decide inbox key: %w", err)
	}
}
