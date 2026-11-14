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
	// RecipientsTimeout bounds the background `agentbus list --json`.
	// Zero is decide.DefaultBusListTimeout.
	RecipientsTimeout time.Duration
	// Snoozed opens on the Snoozed folder.
	Snoozed bool
	// Avatars, AvatarDefault, and AvatarMono are the decide config
	// avatar settings.
	Avatars       []decide.AvatarRule
	AvatarDefault bool
	AvatarMono    string
	Checkouts     []decide.ConfigCheckout
	// Machine is this machine's herdr label, so an owner pane on it is
	// looked up without --machine.
	Machine string
	// OpenCommand is the link opener argv; the URL is appended. Empty
	// is xdg-open, or open on macOS.
	OpenCommand []string
	// NoHyperlinks drops the OSC 8 hyperlinks around URLs.
	NoHyperlinks bool
	// HiddenContacts are agent names Contacts does not show.
	HiddenContacts []string
	// ContactAliases maps a ledger name to a canonical agent name.
	ContactAliases map[string]string
	// UTC shows times in UTC. False uses the clock's zone, which is
	// the machine zone when the clock is time.Now.
	UTC bool
	// Clipboard is auto, osc52, command, or off. Empty is auto.
	Clipboard string
	// Backend replaces the local ledger in Dir, such as the hub's in
	// hub mode. It is reloaded off the update loop.
	Backend Backend
	// LocalConfig is this machine's config. In hub mode a local avatar
	// rule still hides the hub's rule for that name.
	LocalConfig decide.ConfigFile
	// Display, when set, replaces the display settings above at start
	// and whenever it reports a change after a ledger read.
	Display DisplaySource
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

	var inputLog io.Writer
	if keyLog != nil {
		inputLog = keyLog
	}
	final, err := newProgram(m, opts.In, opts.Out, inputLog).Run()
	if err != nil {
		return fmt.Errorf("decide inbox: %w", err)
	}
	done, ok := final.(*Model)
	if ok && done.runErr != nil {
		return done.runErr
	}
	return nil
}

// newProgram runs m on in and out. Input goes through programInput, so
// Ctrl+Enter arrives as Alt+Enter. log gets the rewrites for key debug.
func newProgram(m *Model, in io.Reader, out io.Writer, log io.Writer) *tea.Program {
	return tea.NewProgram(m,
		tea.WithInput(programInput(in, log)),
		tea.WithOutput(out),
		tea.WithAltScreen(),
		tea.WithoutSignalHandler(),
		tea.WithMouseCellMotion(),
	)
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
	m.busTimeout = opts.RecipientsTimeout
	if m.busTimeout <= 0 {
		m.busTimeout = decide.DefaultBusListTimeout
	}
	m.out = opts.Out
	m.Avatars = opts.Avatars
	m.AvatarDefault = opts.AvatarDefault
	m.AvatarMono = opts.AvatarMono
	m.Checkouts = opts.Checkouts
	m.Machine = opts.Machine
	m.OpenCommand = opts.OpenCommand
	m.NoHyperlinks = opts.NoHyperlinks
	m.hidden = append([]string(nil), opts.HiddenContacts...)
	m.aliases = cloneAliases(opts.ContactAliases)
	m.UTC = opts.UTC
	m.clipboard = opts.Clipboard
	if opts.Backend != nil {
		m.backend = opts.Backend
		m.asyncReload = true
	}
	m.localConfig = opts.LocalConfig
	m.avatarProfile = termenv.NewOutput(opts.Out).EnvColorProfile()
	m.display = opts.Display
	m.pollDisplay()
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
