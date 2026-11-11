package tui

import (
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

const (
	goToMenuWide = "go to: i inbox · z snoozed · a answered · d done · w withdrawn · s status · g top · esc cancel"
	goToMenuKeys = "go to: i z a d w s g"
)

// footer is the status row of a framed view: the last non-blank row inside
// the border, without borders or padding.
func footer(t *testing.T, m *Model) string {
	t.Helper()
	lines := strings.Split(m.View(), "\n")
	for i := len(lines) - 2; i > 0; i-- {
		row := visibleLine(lines[i])
		row = strings.TrimSuffix(strings.TrimPrefix(row, "│"), "│")
		if row = strings.TrimRight(row, " "); row != "" {
			return row
		}
	}
	t.Fatalf("view has no footer\n%s", m.View())
	return ""
}

func TestGoToMenuReplacesTheListHint(t *testing.T) {
	m, _, _, _ := snoozeModel(t)
	m.Update(tea.WindowSizeMsg{Width: 120, Height: 20})
	normal := footer(t, m)
	if !strings.Contains(normal, "g go to") || strings.Contains(normal, "gi/ga") {
		t.Fatalf("list hint %q", normal)
	}
	press(t, m, "g")
	if got := footer(t, m); got != goToMenuWide {
		t.Fatalf("chord footer\n got %q\nwant %q", got, goToMenuWide)
	}
	press(t, m, "\x1b")
	if got := footer(t, m); got != normal || m.chord {
		t.Fatalf("after esc %q chord %v, want %q", got, m.chord, normal)
	}
}

func TestGoToMenuInTheItemView(t *testing.T) {
	m, _, _, _ := snoozeModel(t)
	m.Update(tea.WindowSizeMsg{Width: 120, Height: 20})
	press(t, m, "o")
	if m.mode != modeDetail {
		t.Fatalf("mode %v", m.mode)
	}
	m.Update(tea.WindowSizeMsg{Width: 160, Height: 20})
	normal := footer(t, m)
	if !strings.Contains(normal, "g go to") || !strings.HasSuffix(normal, "? help") {
		t.Fatalf("item hint %q", normal)
	}
	press(t, m, "g")
	if got := footer(t, m); got != goToMenuWide {
		t.Fatalf("chord footer\n got %q\nwant %q", got, goToMenuWide)
	}
}

func TestGoToMenuStatusHint(t *testing.T) {
	m, _, _, _ := snoozeModel(t)
	m.Update(tea.WindowSizeMsg{Width: 120, Height: 20})
	press(t, m, "gs")
	if m.mode != modeStatus {
		t.Fatalf("mode %v", m.mode)
	}
	m.status = ""
	if got := footer(t, m); !strings.Contains(got, "g go to") || strings.Contains(got, "gi/ga") {
		t.Fatalf("status hint %q", got)
	}
}

func TestGoToMenuNarrowKeepsKeysOnOneLine(t *testing.T) {
	m, _, _, _ := snoozeModel(t)
	m.Update(tea.WindowSizeMsg{Width: 40, Height: 20})
	press(t, m, "g")
	view := m.View()
	if got := footer(t, m); got != goToMenuKeys {
		t.Fatalf("narrow chord footer %q\n%s", got, view)
	}
	for i, line := range strings.Split(view, "\n") {
		if lipgloss.Width(line) > 40 {
			t.Fatalf("line %d width %d\n%s", i, lipgloss.Width(line), view)
		}
	}
}

func TestGoToUnknownKeyReportsAndClears(t *testing.T) {
	m, _, _, _ := snoozeModel(t)
	press(t, m, "gx")
	if m.chord || m.status != "unknown go-to key" || m.folder != 0 {
		t.Fatalf("chord %v status %q folder %d", m.chord, m.status, m.folder)
	}
	m.status = ""
	press(t, m, "g\x1b")
	if m.chord || m.status != "" {
		t.Fatalf("esc chord %v status %q", m.chord, m.status)
	}
}

func TestGoToTableCoversMenuHelpAndChords(t *testing.T) {
	if len(goToChords) == 0 {
		t.Fatal("empty go-to table")
	}
	keys := ""
	for _, c := range goToChords {
		keys += string(c.key)
		if !strings.Contains(goToMenuWide, string(c.key)+" "+c.name) {
			t.Fatalf("menu misses %c %s", c.key, c.name)
		}
	}
	if keys != "izadwsg" {
		t.Fatalf("table keys %q", keys)
	}
	if got := goToMenu(0); got != goToMenuWide {
		t.Fatalf("menu %q", got)
	}
	for _, mode := range []string{"list", "status", "item"} {
		m, _, _, _ := snoozeModel(t)
		switch mode {
		case "status":
			press(t, m, "gs")
		case "item":
			press(t, m, "o")
		}
		press(t, m, "?")
		help := m.View()
		line := lineContaining(help, "g then")
		for _, c := range goToChords {
			if !strings.Contains(line, string(c.key)+" "+c.name) {
				t.Fatalf("%s help line %q misses %c %s", mode, line, c.key, c.name)
			}
		}
	}
	for _, c := range goToChords {
		m, _, _, _ := snoozeModel(t)
		m.status = ""
		press(t, m, "g"+string(c.key))
		if m.chord || m.status == "unknown go-to key" {
			t.Fatalf("g%c chord %v status %q", c.key, m.chord, m.status)
		}
	}
	for _, r := range "bcefhjklmnopqrtuvxy" {
		m, _, _, _ := snoozeModel(t)
		press(t, m, "g"+string(r))
		if m.status != "unknown go-to key" {
			t.Fatalf("g%c status %q", r, m.status)
		}
	}
}

func TestGoToChordsStillNavigate(t *testing.T) {
	m, _, _, _ := snoozeModel(t)
	press(t, m, "gz")
	if m.folder != snoozedFolder {
		t.Fatalf("gz folder %d", m.folder)
	}
	press(t, m, "gi")
	m.cursor = 0
	press(t, m, "G")
	press(t, m, "gg")
	if m.cursor != 0 || m.chord {
		t.Fatalf("gg cursor %d chord %v", m.cursor, m.chord)
	}
}
