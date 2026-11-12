package tui

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// fakeOpener records each opener argv instead of running it.
type fakeOpener struct {
	calls [][]string
	err   error
}

func (f *fakeOpener) run(_ context.Context, argv []string) error {
	f.calls = append(f.calls, slices.Clone(argv))
	return f.err
}

// urls is the last argument of each call, the URL.
func (f *fakeOpener) urls() []string {
	var out []string
	for _, argv := range f.calls {
		out = append(out, argv[len(argv)-1])
	}
	return out
}

// finishOpen runs the opener a key asked for and feeds its result back,
// as the bubbletea program does.
func finishOpen(t *testing.T, m *Model) {
	t.Helper()
	cmd := m.openCmd()
	if cmd == nil {
		return
	}
	if _, err := m.Update(cmd()); err != nil {
		t.Fatal(err)
	}
}

// runCmd runs cmd and feeds an opener result back. Batches run each part.
func runCmd(t *testing.T, m *Model, cmd tea.Cmd) {
	t.Helper()
	if cmd == nil {
		return
	}
	switch msg := cmd().(type) {
	case tea.BatchMsg:
		for _, c := range msg {
			runCmd(t, m, c)
		}
	case openedMsg:
		m.Update(msg)
	}
}

func linkItem(t *testing.T, links []string) *Model {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "links",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Links:     links,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 100
	m.height = 40
	press(t, m, "o")
	return m
}

func twelveLinks() []string {
	var links []string
	for i := 1; i <= 12; i++ {
		links = append(links, fmt.Sprintf("https://example.test/link/%02d", i))
	}
	return links
}

func TestDefaultOpenCommandPerPlatform(t *testing.T) {
	if got := defaultOpenCommand("darwin"); !slices.Equal(got, []string{"open"}) {
		t.Fatalf("darwin opener %q", got)
	}
	if got := defaultOpenCommand("linux"); !slices.Equal(got, []string{"xdg-open"}) {
		t.Fatalf("linux opener %q", got)
	}
}

func TestLinkPickerListsEveryLinkAndOpensTwoDigits(t *testing.T) {
	links := twelveLinks()
	m := linkItem(t, links)
	open := &fakeOpener{}
	m.runOpen = open.run
	press(t, m, "l")
	if !m.linkPick {
		t.Fatalf("l with 12 links did not open the picker, status %q", m.status)
	}
	view := m.View()
	for i, link := range links {
		row := fmt.Sprintf("%2d  %s", i+1, link)
		if !strings.Contains(visibleLine(view), row) {
			t.Fatalf("picker missing row %q\n%s", row, view)
		}
	}
	press(t, m, "12")
	finishOpen(t, m)
	if len(open.calls) != 0 || !m.linkPick {
		t.Fatalf("digits opened before Enter: %v picking %v", open.calls, m.linkPick)
	}
	press(t, m, "\r")
	finishOpen(t, m)
	if !slices.Equal(open.urls(), []string{links[11]}) {
		t.Fatalf("1 2 Enter opened %v", open.urls())
	}
	if m.linkPick || m.mode != modeDetail {
		t.Fatalf("picker still open %v mode %d", m.linkPick, m.mode)
	}
	if m.status != "opened "+links[11] {
		t.Fatalf("status %q", m.status)
	}
}

func TestLinkPickerEscOpensNothing(t *testing.T) {
	m := linkItem(t, twelveLinks())
	open := &fakeOpener{}
	m.runOpen = open.run
	press(t, m, "l\x1b")
	finishOpen(t, m)
	if m.linkPick || len(open.calls) != 0 || m.mode != modeDetail {
		t.Fatalf("esc picking %v opened %v mode %d", m.linkPick, open.calls, m.mode)
	}
}

func TestLinkPickerMovesWithJKAndArrows(t *testing.T) {
	links := twelveLinks()
	m := linkItem(t, links)
	open := &fakeOpener{}
	m.runOpen = open.run
	press(t, m, "ljjjk")
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyDown})
	m = next.(*Model)
	next, _ = m.Update(tea.KeyMsg{Type: tea.KeyDown})
	m = next.(*Model)
	next, _ = m.Update(tea.KeyMsg{Type: tea.KeyUp})
	m = next.(*Model)
	if !strings.Contains(visibleLine(m.View()), ">  4  https://example.test/link/04") {
		t.Fatalf("cursor not on row 4\n%s", m.View())
	}
	next, cmd := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = next.(*Model)
	runCmd(t, m, cmd)
	if !slices.Equal(open.urls(), []string{links[3]}) {
		t.Fatalf("enter opened %v", open.urls())
	}
}

func TestLinkPickerOneDigitOpensDirectlyWithFewLinks(t *testing.T) {
	links := []string{"https://a.test/1", "https://b.test/2", "https://c.test/3"}
	m := linkItem(t, links)
	open := &fakeOpener{}
	m.runOpen = open.run
	press(t, m, "l2")
	finishOpen(t, m)
	if !slices.Equal(open.urls(), []string{links[1]}) || m.linkPick {
		t.Fatalf("2 opened %v picking %v", open.urls(), m.linkPick)
	}
}

func TestLinkPickerRejectsANumberOutOfRange(t *testing.T) {
	m := linkItem(t, twelveLinks())
	open := &fakeOpener{}
	m.runOpen = open.run
	press(t, m, "l13\r")
	finishOpen(t, m)
	if len(open.calls) != 0 || !m.linkPick || m.status != "no such link" {
		t.Fatalf("13 opened %v picking %v status %q", open.calls, m.linkPick, m.status)
	}
}

func TestLOpensASingleLinkWithTheConfiguredCommand(t *testing.T) {
	m := linkItem(t, []string{"https://example.test/only"})
	open := &fakeOpener{}
	m.runOpen = open.run
	m.OpenCommand = []string{"my-open", "--new"}
	press(t, m, "l")
	if m.linkPick {
		t.Fatal("one link opened the picker")
	}
	if len(open.calls) != 0 {
		t.Fatal("the opener ran inside Key, not in a command")
	}
	finishOpen(t, m)
	want := [][]string{{"my-open", "--new", "https://example.test/only"}}
	if !slices.EqualFunc(open.calls, want, slices.Equal) {
		t.Fatalf("argv %q", open.calls)
	}
}

func TestOpenFailureShowsOnTheStatusLine(t *testing.T) {
	m := linkItem(t, []string{"https://example.test/only"})
	open := &fakeOpener{err: errors.New("exit status 3")}
	m.runOpen = open.run
	press(t, m, "l")
	finishOpen(t, m)
	if m.status != "open failed: exit status 3" {
		t.Fatalf("status %q", m.status)
	}
}

func TestOpenedStatusShortensALongURL(t *testing.T) {
	link := "https://example.com/" + strings.Repeat("p", 120) + "/tailtoken"
	m := linkItem(t, []string{link})
	open := &fakeOpener{}
	m.runOpen = open.run
	press(t, m, "l")
	finishOpen(t, m)
	if !strings.HasPrefix(m.status, "opened example.com") || !strings.HasSuffix(m.status, "/tailtoken") ||
		len([]rune(m.status)) > len("opened ")+statusURLWidth {
		t.Fatalf("status %q", m.status)
	}
	if !slices.Equal(open.urls(), []string{link}) {
		t.Fatalf("opener got %v, not the full URL", open.urls())
	}
}

func TestShortURLKeepsHostAndPathTail(t *testing.T) {
	cases := []struct {
		url   string
		width int
		want  string
	}{
		{"https://a.test/x", 40, "https://a.test/x"},
		{"https://example.com/aaaaaaaaaa/bbbbbbbbbb/tail", 30, "example.com…aa/bbbbbbbbbb/tail"},
		{"https://example.com/aaaaaaaaaa/bbbbbbbbbb/tail", 40, "example.com/aaaaaaaaaa/bbbbbbbbbb/tail"},
		{"https://" + strings.Repeat("h", 40) + "/tail", 20, strings.Repeat("h", 10) + "…" + strings.Repeat("h", 4) + "/tail"},
	}
	for _, c := range cases {
		got := shortURL(c.url, c.width)
		if got != c.want {
			t.Errorf("shortURL(%q, %d) = %q, want %q", c.url, c.width, got, c.want)
		}
		if n := len([]rune(got)); n > c.width {
			t.Errorf("shortURL(%q, %d) is %d wide", c.url, c.width, n)
		}
	}
}

func TestExecOpenRunsWithoutAShellAndTimesOut(t *testing.T) {
	ctx := context.Background()
	if err := execOpen(ctx, []string{"true", "https://example.test/a b"}); err != nil {
		t.Fatalf("true: %v", err)
	}
	if err := execOpen(ctx, []string{"false", "x"}); err == nil {
		t.Fatal("false did not fail")
	}
	short, cancel := context.WithTimeout(ctx, 50*time.Millisecond)
	defer cancel()
	start := time.Now()
	if err := execOpen(short, []string{"sleep", "5"}); err == nil {
		t.Fatal("sleep outlived the timeout")
	}
	if time.Since(start) > 3*time.Second {
		t.Fatal("timeout did not stop the opener")
	}
}
