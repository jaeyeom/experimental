package tui

import (
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
