package tui

import (
	"fmt"
	"io"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/muesli/termenv"
)

// Options configure Run. In and Out default to the caller. Interval defaults
// to DefaultInterval. Targets and SendTemplate come from decide config.
// An empty SendTemplate is `herdr agent prompt {pane}` plus the note as its
// own argument. Agentbus adds `agentbus list --json` names to the
// recipients. BusRunner runs agentbus and defaults to decide.ExecAgentbus.
// Checkouts map status resources to checkouts whose lease the status
// board shows.
type Options struct {
	Dir          string
	In           io.Reader
	Out          io.Writer
	Interval     time.Duration
	Now          func() time.Time
	Targets      []Target
	SendTemplate string
	Agentbus     bool
	BusRunner    BusRunner
	// Snoozed opens on the Snoozed folder.
	Snoozed bool
	// Avatars, AvatarDefault, and AvatarMono are the decide config
	// avatar settings.
	Avatars       []decide.AvatarRule
	AvatarDefault bool
	AvatarMono    string
	Checkouts     []decide.ConfigCheckout
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
	keyLog, err := openKeyLog()
	if err != nil {
		return err
	}
	if keyLog != nil {
		defer keyLog.Close()
		m.keyLog = keyLog
	}
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
	m.busOn = opts.Agentbus
	m.busRun = opts.BusRunner
	m.out = opts.Out
	m.Avatars = opts.Avatars
	m.AvatarDefault = opts.AvatarDefault
	m.AvatarMono = opts.AvatarMono
	m.Checkouts = opts.Checkouts
	m.avatarProfile = termenv.NewOutput(opts.Out).EnvColorProfile()
	if opts.Now != nil {
		m.Now = opts.Now
	}
	if opts.Snoozed {
		m.folder = snoozedFolder
	}
	if err := m.Reload(); err != nil {
		return nil, err
	}
	return m, nil
}
