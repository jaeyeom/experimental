package tui

import (
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestDefaultKeyBindingsTable(t *testing.T) {
	cases := []struct {
		scope  string
		key    string
		action string
	}{
		{"list", "q", "quit"},
		{"list", "?", "help"},
		{"list", "M", "mouse"},
		{"list", "/", "search"},
		{"list", "j", "list.next"},
		{"list", "k", "list.prev"},
		{"list", "pgdn", "list.pageDown"},
		{"list", "pgup", "list.pageUp"},
		{"list", "home", "list.top"},
		{"list", "G", "list.bottom"},
		{"list", "end", "list.bottom"},
		{"list", "o", "list.open"},
		{"list", "enter", "list.open"},
		{"list", "c", "list.compose"},
		{"list", "e", "list.archive"},
		{"list", "y", "list.archive"},
		{"list", "b", "list.snooze"},
		{"list", "esc", "list.clearFilter"},
		{"detail", "u", "detail.back"},
		{"detail", "esc", "detail.back"},
		{"detail", "j", "detail.scrollDown"},
		{"detail", "down", "detail.scrollDown"},
		{"detail", "k", "detail.scrollUp"},
		{"detail", "up", "detail.scrollUp"},
		{"detail", "n", "detail.nextMessage"},
		{"detail", "p", "detail.prevMessage"},
		{"detail", "l", "detail.openLink"},
		{"detail", "L", "detail.copyLink"},
		{"detail", "Y", "detail.copyMenu"},
		{"detail", "V", "detail.plain"},
		{"detail", "A", "detail.avatar"},
		{"detail", "o", "detail.owner"},
		{"detail", "f", "detail.forward"},
		{"detail", "E", "detail.outcome"},
		{"detail", "0", "detail.reply"},
		{"status", "u", "status.back"},
		{"status", "c", "status.compose"},
		{"contacts", "enter", "contacts.filter"},
		{"contacts", "*", "contacts.star"},
		{"contacts", "x", "contacts.hide"},
		{"contacts", "H", "contacts.showHidden"},
		{"contacts", "X", "contacts.hideDead"},
		{"contacts", "R", "contacts.release"},
		{"contacts", "a", "contacts.alias"},
		{"contacts", "i", "contacts.details"},
		{"contacts", "t", "contacts.orgTree"},
		{"editor", "ctrl+f", "edit.charForward"},
		{"editor", "ctrl+b", "edit.charBackward"},
		{"editor", "ctrl+n", "edit.lineNext"},
		{"editor", "ctrl+p", "edit.linePrev"},
		{"editor", "ctrl+a", "edit.lineStart"},
		{"editor", "ctrl+e", "edit.lineEnd"},
		{"editor", "ctrl+d", "edit.deleteForward"},
		{"editor", "ctrl+h", "edit.deleteBackward"},
		{"editor", "ctrl+k", "edit.killLine"},
		{"editor", "ctrl+j", "edit.newline"},
		{"editor", "ctrl+o", "edit.openLine"},
		{"editor", "ctrl+g", "edit.external"},
		{"editor", "alt+q", "edit.fill"},
		{"editor", "alt+enter", "compose.send"},
		{"editor", "ctrl+s", "compose.send"},
	}
	km := decide.DefaultKeyMap()
	for _, tc := range cases {
		seq, err := decide.ParseKeySeq(tc.key)
		if err != nil {
			t.Fatalf("parse %q: %v", tc.key, err)
		}
		got, ok := km.Match(tc.scope, seq[0])
		if !ok || got != tc.action {
			t.Errorf("scope=%s key=%q: got (%q, %v), want (%q, true)", tc.scope, tc.key, got, ok, tc.action)
		}
	}
	for _, unbound := range []struct {
		scope string
		key   string
	}{
		{"list", "E"},
		{"detail", "d"},
	} {
		seq, err := decide.ParseKeySeq(unbound.key)
		if err != nil {
			t.Fatalf("parse %q: %v", unbound.key, err)
		}
		if got, ok := km.Match(unbound.scope, seq[0]); ok {
			t.Errorf("scope=%s key=%q: got (%q, true), want unbound", unbound.scope, unbound.key, got)
		}
	}
}

func TestRebindActionMovesToNewKeyAndOldKeyStopsWorking(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Owner:    "test",
		Kind:     decide.KindRelay,
		Question: "Continue?",
		Options: []decide.Option{
			{Key: "y", Label: "Yes"},
			{Key: "n", Label: "No"},
		},
	})
	mustCreate(t, led, &decide.Item{
		Owner:    "test2",
		Kind:     decide.KindRelay,
		Question: "Second?",
		Options: []decide.Option{
			{Key: "y", Label: "Yes"},
			{Key: "n", Label: "No"},
		},
	})

	km, err := decide.ResolveKeyMap(map[string][]string{
		"list.next":    {"n"},
		"detail.reply": {"r", "R"},
	})
	if err != nil {
		t.Fatal(err)
	}
	m := New(dir)
	m.SetKeys(km)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Old list.next key 'j' no longer moves cursor.
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'j'}})
	if m.cursor != 0 {
		t.Fatalf("old key j moved cursor to %d", m.cursor)
	}
	// New list.next key 'n' moves cursor.
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'n'}})
	if m.cursor != 1 {
		t.Fatalf("new key n cursor = %d, want 1", m.cursor)
	}

	// Open item.
	m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	if m.mode != modeDetail {
		t.Fatalf("mode = %v, want modeDetail", m.mode)
	}

	// Old detail.reply key '0' no longer focuses reply.
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'0'}})
	if m.replyFocus {
		t.Fatal("old key 0 focused reply")
	}

	// New detail.reply key 'r' focuses reply.
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'r'}})
	if !m.replyFocus {
		t.Fatal("new key r did not focus reply")
	}
}

func TestEmptyListUnbindsAction(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Owner:    "test",
		Kind:     decide.KindRelay,
		Question: "Continue?",
		Options: []decide.Option{
			{Key: "y", Label: "Yes"},
			{Key: "n", Label: "No"},
		},
	})
	km, err := decide.ResolveKeyMap(map[string][]string{
		"list.snooze": {},
	})
	if err != nil {
		t.Fatal(err)
	}
	m := New(dir)
	m.SetKeys(km)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'b'}})
	if m.snoozing {
		t.Fatal("unbound list.snooze still opened snooze")
	}
}

func TestHelpScreenShowsConfiguredKeys(t *testing.T) {
	km, err := decide.ResolveKeyMap(map[string][]string{
		"quit":         {"Q"},
		"help":         {"h"},
		"detail.reply": {"r", "R"},
	})
	if err != nil {
		t.Fatal(err)
	}
	m := New(t.TempDir())
	m.width = 160
	m.height = 60
	m.SetKeys(km)
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'h'}})
	if !m.help {
		t.Fatal("rebound help key h did not open help")
	}
	view := m.View()
	for _, want := range []string{"Q quit", "r/R reply", "h or esc close", "h help"} {
		if !strings.Contains(view, want) {
			t.Errorf("help view missing %q\n%s", want, view)
		}
	}
}
