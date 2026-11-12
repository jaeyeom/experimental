package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/muesli/termenv"
)

// hubAvatarBytes is an avatar text file made of ch.
func hubAvatarBytes(ch string) []byte {
	row := "\x1b[32m" + strings.Repeat(ch, avatarCols) + "\x1b[0m\n"
	return []byte(strings.Repeat(row, avatarRows))
}

func withDisplay(d Display) func(*Model) {
	return func(m *Model) {
		m.applyDisplay(d)
		m.avatarProfile = termenv.ANSI
	}
}

func TestHubAvatarFileRendersFromItsBytes(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	m := avatarItemModel(t, 100, withDisplay(Display{
		Avatars:     []decide.AvatarRule{{Match: "a/x", File: decide.HubAvatarPrefix + "0.txt"}},
		AvatarFiles: map[string]decide.AvatarFile{decide.HubAvatarPrefix + "0.txt": {Data: hubAvatarBytes("H")}},
	}))
	if view := m.View(); !strings.Contains(view, strings.Repeat("H", avatarCols)) {
		t.Fatalf("no hub avatar:\n%s", visibleLine(view))
	}
}

func TestMissingHubAvatarFileIsHiddenAndReportedOnce(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	m := avatarItemModel(t, 100, withDisplay(Display{
		Avatars:     []decide.AvatarRule{{Match: "a/x", File: decide.HubAvatarPrefix + "0.png"}},
		AvatarFiles: map[string]decide.AvatarFile{decide.HubAvatarPrefix + "0.png": {Error: "file not found on the hub"}},
	}))
	view := m.View()
	if n := strings.Count(view, "avatar for a/x"); n != 1 || !strings.Contains(view, "not found on the hub") {
		t.Fatalf("status shows %d avatar errors:\n%s", n, visibleLine(view))
	}
	m.status = ""
	if strings.Contains(m.View(), "avatar for") {
		t.Fatal("the avatar error showed twice")
	}
}

// displaySource hands out queued display settings, one per poll.
type displaySource struct {
	queue []Display
	polls int
}

func (s *displaySource) Display() (Display, bool) {
	s.polls++
	if len(s.queue) == 0 {
		return Display{}, false
	}
	d := s.queue[0]
	s.queue = s.queue[1:]
	return d, true
}

func TestHubDisplaySettingsApplyAtStartAndAfterAReload(t *testing.T) {
	hubDir := t.TempDir()
	mustCreate(t, openLedger(t, hubDir), &decide.Item{Question: "q", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen})
	src := &displaySource{queue: []Display{{
		AvatarDefault: true,
		Targets:       []Target{{Name: "builder", Pane: "w2:p3"}},
		Note:          "display settings: hub (2 keys), local overrides: none",
	}}}
	m, err := prepare(Options{
		Dir: t.TempDir(), In: strings.NewReader(""), Out: &strings.Builder{},
		Backend: &fakeHub{hub: &LocalBackend{Dir: hubDir}}, Display: src,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !m.AvatarDefault || len(m.Targets) != 1 || m.status != "display settings: hub (2 keys), local overrides: none" {
		t.Fatalf("start: avatarDefault %v, targets %v, status %q", m.AvatarDefault, m.Targets, m.status)
	}

	src.queue = []Display{{AvatarMono: decide.AvatarMonoHide, RecipientsTimeout: 5 * time.Second}}
	m.status = ""
	m.Update(listMsg{all: nil})
	if m.AvatarDefault || m.AvatarMono != decide.AvatarMonoHide || len(m.Targets) != 0 || m.busTimeout != 5*time.Second || m.status != "" {
		t.Fatalf("after reload: avatarDefault %v, mono %q, targets %v, timeout %s, status %q", m.AvatarDefault, m.AvatarMono, m.Targets, m.busTimeout, m.status)
	}
	polls := src.polls
	m.Update(listMsg{all: nil})
	if src.polls != polls+1 || m.AvatarMono != decide.AvatarMonoHide {
		t.Fatalf("an unchanged poll changed the settings: polls %d, mono %q", src.polls, m.AvatarMono)
	}
}
