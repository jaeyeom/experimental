package tui

import (
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
)

// Display is the inbox's display settings. AvatarFiles holds avatar
// files by rule File, such as a hub's; those names are never read from
// disk. Note, when set, goes to the status line.
type Display struct {
	Targets           []Target
	Agentbus          bool
	RecipientsTimeout time.Duration
	Avatars           []decide.AvatarRule
	AvatarDefault     bool
	AvatarMono        string
	NoHyperlinks      bool
	AvatarFiles       map[string]decide.AvatarFile
	Note              string
}

// DisplaySource supplies display settings that can change while the
// inbox runs, such as a hub's after a reconnect. Display returns new
// settings and true once per change.
type DisplaySource interface {
	Display() (Display, bool)
}

// pollDisplay applies the display source's settings when they changed.
func (m *Model) pollDisplay() {
	if m.display == nil {
		return
	}
	if d, ok := m.display.Display(); ok {
		m.applyDisplay(d)
	}
}

// applyDisplay replaces the display settings. Avatars render afresh,
// and a file that failed before is tried again.
func (m *Model) applyDisplay(d Display) {
	m.Targets = d.Targets
	m.busOn = d.Agentbus
	m.busTimeout = d.RecipientsTimeout
	if m.busTimeout <= 0 {
		m.busTimeout = decide.DefaultBusListTimeout
	}
	m.Avatars = d.Avatars
	m.AvatarDefault = d.AvatarDefault
	m.AvatarMono = d.AvatarMono
	m.NoHyperlinks = d.NoHyperlinks
	files := avatar.Overlay{}
	for name, f := range d.AvatarFiles {
		files[name] = avatar.MemFile{Data: f.Data, Err: f.Error}
	}
	m.avatarRender = avatar.NewRenderer(files)
	m.avatarFailed = nil
	if d.Note != "" {
		m.status = d.Note
	}
}
