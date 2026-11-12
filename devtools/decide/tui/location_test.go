package tui

import (
	"slices"
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

func fakeOwnerHerdr(t *testing.T) *herdrtest.Fake {
	t.Helper()
	f := herdrtest.Install(t)
	for _, m := range []string{"local", "mac"} {
		f.Write(m+".panes", `{"result":{"panes":[{"pane_id":"w3:p7","tab_id":"w3:t2","workspace_id":"w3"}]}}`)
		f.Write(m+".tabs", `{"result":{"tabs":[{"tab_id":"w3:t2","label":"review"}]}}`)
		f.Write(m+".workspaces", `{"result":{"workspaces":[{"workspace_id":"w3","label":"experimental"}]}}`)
	}
	return f
}

func ownerModel(t *testing.T, item *decide.Item) *Model {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	item.Question, item.Owner, item.Kind, item.State = "ship it?", "ada", decide.KindRelay, decide.StateOpen
	mustCreate(t, led, item)
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 100
	m.height = 30
	return m
}

// update sends one key through bubbletea's Update and returns its command.
func update(t *testing.T, m *Model, r rune) tea.Cmd {
	t.Helper()
	_, cmd := m.Update(runeKey(r))
	return cmd
}

func ownerLine(t *testing.T, m *Model) string {
	t.Helper()
	for _, line := range strings.Split(visibleLine(m.View()), "\n") {
		if _, rest, ok := strings.Cut(line, "Owner: "); ok {
			return strings.TrimRight(rest, "│ \r")
		}
	}
	t.Fatalf("no owner line\n%s", m.View())
	return ""
}

func TestItemViewOwnerLocationFillsInWithoutBlocking(t *testing.T) {
	f := fakeOwnerHerdr(t)
	m := ownerModel(t, &decide.Item{NotifyPane: "w3:p7"})
	cmd := update(t, m, 'o')
	if cmd == nil {
		t.Fatal("opening the item started no lookup")
	}
	if got := ownerLine(t, m); got != "ada at (w3:p7)" {
		t.Fatalf("owner line before the lookup = %q", got)
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran before the command: %q", argv)
	}
	m.Update(cmd())
	if got := ownerLine(t, m); got != "ada at experimental / review (w3:p7)" {
		t.Fatalf("owner line after the lookup = %q", got)
	}
	if argv := f.Argv(); len(argv) != 3 {
		t.Fatalf("herdr calls = %q", argv)
	}

	// The reload tick does not ask herdr again.
	m.Update(reloadMsg{})
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("reload asked herdr: %q", argv)
	}

	// Reopening shows the cached location and refreshes it.
	update(t, m, 'u')
	f.Write("local.panes", `{"result":{"panes":[]}}`)
	cmd = update(t, m, 'o')
	if got := ownerLine(t, m); got != "ada at experimental / review (w3:p7)" {
		t.Fatalf("cached owner line = %q", got)
	}
	if cmd == nil {
		t.Fatal("reopening started no refresh")
	}
	m.Update(cmd())
	if got := ownerLine(t, m); got != "ada at (w3:p7) — tab closed" {
		t.Fatalf("refreshed owner line = %q", got)
	}
}

func TestItemViewOwnerLocationOtherMachine(t *testing.T) {
	fakeOwnerHerdr(t)
	m := ownerModel(t, &decide.Item{NotifyPane: "w3:p7", NotifyMachine: "mac"})
	m.Machine = "hub"
	cmd := update(t, m, 'o')
	if got := ownerLine(t, m); got != "ada at mac: (w3:p7)" {
		t.Fatalf("owner line before the lookup = %q", got)
	}
	m.Update(cmd())
	if got := ownerLine(t, m); got != "ada at mac: experimental / review (w3:p7)" {
		t.Fatalf("owner line = %q", got)
	}
}

func TestItemViewOwnerWithoutPaneIsUnknown(t *testing.T) {
	f := herdrtest.Install(t)
	m := ownerModel(t, &decide.Item{})
	if cmd := update(t, m, 'o'); cmd != nil {
		m.Update(cmd())
	}
	if got := ownerLine(t, m); got != "ada at unknown" {
		t.Fatalf("owner line = %q", got)
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran %q", argv)
	}
}

// openOwner opens the item and lets the owner lookup finish.
func openOwner(t *testing.T, m *Model, f *herdrtest.Fake) {
	t.Helper()
	if cmd := update(t, m, 'o'); cmd != nil {
		m.Update(cmd())
	}
	f.Argv()
}

// jumpOwner presses o in the item view and runs the command it starts.
func jumpOwner(t *testing.T, m *Model) {
	t.Helper()
	if cmd := update(t, m, 'o'); cmd != nil {
		m.Update(cmd())
	}
}

func TestItemViewJumpsToOwnerTab(t *testing.T) {
	f := fakeOwnerHerdr(t)
	m := ownerModel(t, &decide.Item{NotifyPane: "w3:p7"})
	openOwner(t, m, f)
	cursor, id := m.cursor, m.detail.ID

	cmd := update(t, m, 'o')
	if cmd == nil {
		t.Fatal("o started no focus command")
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran on the update loop: %q", argv)
	}
	m.Update(cmd())
	want := []string{"workspace focus w3", "tab focus w3:t2"}
	if argv := f.Argv(); !slices.Equal(argv, want) {
		t.Fatalf("herdr calls = %q, want %q", argv, want)
	}
	if got := footer(t, m); !strings.Contains(got, "focused experimental / review") {
		t.Fatalf("footer = %q", got)
	}
	if m.mode != modeDetail || m.cursor != cursor || m.detail == nil || m.detail.ID != id {
		t.Fatalf("jump moved the inbox: mode %v cursor %d", m.mode, m.cursor)
	}
	if m.detail.State != decide.StateOpen {
		t.Fatalf("jump changed the item state to %q", m.detail.State)
	}
}

func TestItemViewJumpToOwnerOnOtherMachine(t *testing.T) {
	f := fakeOwnerHerdr(t)
	m := ownerModel(t, &decide.Item{NotifyPane: "w3:p7", NotifyMachine: "mac"})
	m.Machine = "hub"
	openOwner(t, m, f)
	f.Write("mac.focus-refused", "")
	jumpOwner(t, m)
	if got := footer(t, m); !strings.Contains(got, "owner is on mac; not focusable from here") {
		t.Fatalf("footer = %q", got)
	}
	called := false
	for _, argv := range f.Argv() {
		if strings.HasPrefix(argv, "--machine mac workspace focus w3") {
			called = true
		}
	}
	if !called {
		t.Fatal("focus did not run with --machine mac")
	}
}

func TestItemViewJumpWithoutLiveOwnerDoesNothing(t *testing.T) {
	for _, tc := range []struct {
		name string
		item decide.Item
		want string
	}{
		{"closed", decide.Item{NotifyPane: "w3:p9"}, "owner tab closed; nothing to focus"},
		{"unknown", decide.Item{}, "owner location unknown; nothing to focus"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			f := fakeOwnerHerdr(t)
			m := ownerModel(t, &tc.item)
			openOwner(t, m, f)
			jumpOwner(t, m)
			if argv := f.Argv(); len(argv) != 0 {
				t.Fatalf("herdr ran %q", argv)
			}
			if got := footer(t, m); !strings.Contains(got, tc.want) {
				t.Fatalf("footer = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestItemViewJumpBeforeLookupWaits(t *testing.T) {
	f := fakeOwnerHerdr(t)
	m := ownerModel(t, &decide.Item{NotifyPane: "w3:p7"})
	update(t, m, 'o')
	jumpOwner(t, m)
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran %q", argv)
	}
	if got := footer(t, m); !strings.Contains(got, "owner location still loading") {
		t.Fatalf("footer = %q", got)
	}
}

func TestItemViewHelpListsOwnerJump(t *testing.T) {
	f := fakeOwnerHerdr(t)
	m := ownerModel(t, &decide.Item{NotifyPane: "w3:p7"})
	m.width = 200
	openOwner(t, m, f)
	if got := footer(t, m); !strings.Contains(got, "o owner") {
		t.Fatalf("footer = %q", got)
	}
	update(t, m, '?')
	if got := visibleLine(m.View()); !strings.Contains(got, "o jumps to the owner's herdr tab") {
		t.Fatalf("help = %q", got)
	}
}
