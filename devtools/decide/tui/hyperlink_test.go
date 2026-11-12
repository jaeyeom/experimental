package tui

import (
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

const (
	osc8Close = "\x1b]8;;\x1b\\"
	longLink  = "https://example.com/" + "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + "/end"
)

func osc8Open(url string) string {
	return "\x1b]8;;" + url + "\x1b\\"
}

// wrappedLinkItem opens an item whose question holds longLink, wrapped
// across at least three lines at width 40, and one option button.
func wrappedLinkItem(t *testing.T) *Model {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "spec " + longLink + " ok",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Options:   []decide.Option{{Key: "1", Label: "ship it"}},
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 40
	m.height = 40
	press(t, m, "o")
	return m
}

// linkSegments are the 1-based screen rows that show part of longLink.
func linkSegments(view string) []int {
	var rows []int
	for i, line := range strings.Split(view, "\n") {
		vis := visibleLine(line)
		if strings.Contains(vis, "aaaaaaaaaa") || strings.Contains(vis, "example.com") || strings.Contains(vis, "a/end") {
			rows = append(rows, i+1)
		}
	}
	return rows
}

func ctrlPress(t *testing.T, m *Model, x, y int) *Model {
	t.Helper()
	next, cmd := m.Update(tea.MouseMsg{
		X:      x - 1,
		Y:      y - 1,
		Ctrl:   true,
		Button: tea.MouseButtonLeft,
		Action: tea.MouseActionPress,
	})
	got := next.(*Model)
	runCmd(t, got, cmd)
	return got
}

func TestEveryWrappedLinkSegmentCarriesTheFullURL(t *testing.T) {
	m := wrappedLinkItem(t)
	view := m.View()
	rows := linkSegments(view)
	if len(rows) < 3 {
		t.Fatalf("link wraps over %d lines, want at least 3\n%s", len(rows), view)
	}
	lines := strings.Split(view, "\n")
	for _, row := range rows {
		line := lines[row-1]
		if !strings.Contains(line, osc8Open(longLink)) || !strings.Contains(line, osc8Close) {
			t.Fatalf("row %d lacks OSC 8 around the full URL: %q", row, line)
		}
	}
}

func TestCtrlClickOpensAWrappedLink(t *testing.T) {
	m := wrappedLinkItem(t)
	open := &fakeOpener{}
	m.runOpen = open.run
	view := m.View()
	rows := linkSegments(view)
	if len(rows) < 3 {
		t.Fatalf("link wraps over %d lines\n%s", len(rows), view)
	}
	lines := strings.Split(view, "\n")
	for _, row := range rows[1:3] {
		col := visibleCol(lines[row-1], "aaaa")
		m = ctrlPress(t, m, col+1, row)
	}
	opened := open.urls()
	if len(opened) != 2 || opened[0] != longLink || opened[1] != longLink {
		t.Fatalf("ctrl+click opened %q", opened)
	}
	if !strings.HasPrefix(m.status, "opened ") {
		t.Fatalf("status %q", m.status)
	}
}

func TestCtrlClickOffALinkDoesNothing(t *testing.T) {
	m := wrappedLinkItem(t)
	open := &fakeOpener{}
	m.runOpen = open.run
	view := m.View()
	row := screenRow(view, "ship it")
	if row == 0 {
		t.Fatalf("option row missing\n%s", view)
	}
	col := visibleCol(strings.Split(view, "\n")[row-1], "ship it")
	m = ctrlPress(t, m, col, row)
	if len(open.calls) != 0 {
		t.Fatalf("ctrl+click on an option opened %q", open.urls())
	}
	if m.mode != modeDetail || m.detail == nil || m.detail.State != decide.StateOpen {
		t.Fatalf("ctrl+click on an option acted: mode %v item %+v", m.mode, m.detail)
	}

	m.backToList()
	view = m.View()
	row = screenRow(view, "spec")
	if row == 0 {
		t.Fatalf("list row missing\n%s", view)
	}
	m = ctrlPress(t, m, 5, row)
	if m.mode != modeList || len(open.calls) != 0 {
		t.Fatalf("ctrl+click on a list row: mode %v opened %q", m.mode, open.urls())
	}

	// A plain click on the row still opens the item.
	m = updateMouseAt(t, m, 5, row, tea.MouseButtonLeft)
	if m.mode != modeDetail {
		t.Fatalf("plain click did not open the row: mode %v", m.mode)
	}
}

func TestTruncatedLinkKeepsTheFullURLAndWidth(t *testing.T) {
	line := linkLines("see "+longLink, 200, nil, true)[0]
	cut := truncateVisible(line.text, 20)
	if !strings.Contains(cut, osc8Open(longLink)) || !strings.Contains(cut, osc8Close) {
		t.Fatalf("truncated line lost the OSC 8 link: %q", cut)
	}
	if got := lipgloss.Width(cut); got != 20 {
		t.Fatalf("truncated width %d, want 20", got)
	}
	plain := linkLines("see "+longLink, 200, nil, false)[0]
	if lipgloss.Width(line.text) != lipgloss.Width(plain.text) {
		t.Fatalf("OSC 8 changed the width: %d vs %d", lipgloss.Width(line.text), lipgloss.Width(plain.text))
	}
	if strings.Contains(plain.text, "\x1b]8;") {
		t.Fatalf("hyperlinks off still wrote OSC 8: %q", plain.text)
	}
}

func TestHyperlinksOffWritesNoOSC8(t *testing.T) {
	m := wrappedLinkItem(t)
	m.NoHyperlinks = true
	view := m.View()
	if strings.Contains(view, "\x1b]8;") {
		t.Fatalf("hyperlinks off still wrote OSC 8\n%q", view)
	}
	if len(linkSegments(view)) < 3 {
		t.Fatalf("link text missing\n%s", view)
	}
	press(t, m, "l")
	if strings.Contains(m.View(), "\x1b]8;") {
		t.Fatalf("hyperlinks off: picker wrote OSC 8")
	}
}

func TestNoColorKeepsOSC8(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	m := wrappedLinkItem(t)
	if !strings.Contains(m.View(), osc8Open(longLink)) {
		t.Fatalf("NO_COLOR dropped OSC 8\n%q", m.View())
	}
}

func TestLinkPickerRowsCarryTheFullURL(t *testing.T) {
	m := linkItem(t, []string{longLink, "https://example.test/second"})
	press(t, m, "l")
	view := m.View()
	if !m.linkPick {
		t.Fatalf("picker did not open\n%s", view)
	}
	if strings.Contains(visibleLine(view), longLink) {
		t.Fatalf("picker did not shorten the long URL\n%s", view)
	}
	for _, url := range []string{longLink, "https://example.test/second"} {
		if !strings.Contains(view, osc8Open(url)) {
			t.Fatalf("picker row lacks OSC 8 for %s\n%q", url, view)
		}
	}
}
