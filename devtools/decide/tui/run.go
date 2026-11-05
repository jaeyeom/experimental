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
	restore, err := makeRaw(opts.In)
	if err != nil {
		return err
	}
	defer restore()

	keys := make(chan byte, 8)
	readErr := make(chan error, 1)
	go readKeys(opts.In, keys, readErr)
	ticker := time.NewTicker(opts.Interval)
	defer ticker.Stop()
	for {
		if err := WriteFrame(opts.Out, m.View()); err != nil {
			return err
		}
		quit, err := waitEvent(m, ticker, keys, readErr)
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

func waitEvent(m *Model, ticker *time.Ticker, keys <-chan byte, readErr <-chan error) (bool, error) {
	select {
	case b := <-keys:
		return m.Key(rune(b))
	case <-ticker.C:
		return false, m.Reload()
	case err := <-readErr:
		if errors.Is(err, io.EOF) {
			return true, nil
		}
		return false, fmt.Errorf("read decide inbox key: %w", err)
	}
}

func readKeys(in io.Reader, keys chan<- byte, readErr chan<- error) {
	buf := make([]byte, 1)
	for {
		n, err := in.Read(buf)
		if n == 1 {
			keys <- buf[0]
		}
		if err != nil {
			readErr <- err
			return
		}
	}
}
