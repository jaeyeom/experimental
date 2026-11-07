package tui

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
	"unicode/utf8"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestMain(m *testing.M) {
	dir, err := os.MkdirTemp("", "decide-tui-config")
	if err != nil {
		fmt.Fprintf(os.Stderr, "tui config dir: %v\n", err)
		os.Exit(1)
	}
	if err := os.Setenv("XDG_CONFIG_HOME", dir); err != nil {
		fmt.Fprintf(os.Stderr, "tui config env: %v\n", err)
		os.Exit(1)
	}
	code := m.Run()
	_ = os.RemoveAll(dir)
	os.Exit(code)
}

func TestInboxShowsOpenItemsInOrderAndBoldsUnread(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	soon := time.Date(2026, 12, 1, 0, 0, 0, 0, time.UTC)
	later := time.Date(2026, 12, 2, 0, 0, 0, 0, time.UTC)
	mustCreate(t, led, &decide.Item{
		Question:  "later\ndeadline",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Deadline:  &later,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	unread := mustCreate(t, led, &decide.Item{
		Question:  "sooner deadline",
		Owner:     "bea",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Unread:    true,
		Deadline:  &soon,
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	older := mustCreate(t, led, &decide.Item{
		Question:  "older open",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 3, 0, 0, 0, 0, time.UTC),
	})
	newer := mustCreate(t, led, &decide.Item{
		Question:  "newer open",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Unread:    true,
		CreatedAt: time.Date(2026, 11, 4, 0, 0, 0, 0, time.UTC),
	})
	answered := mustCreate(t, led, &decide.Item{
		Question:  "answered",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateAnswered,
		CreatedAt: time.Date(2026, 11, 5, 0, 0, 0, 0, time.UTC),
	})

	m := New(dir)
	m.Now = func() time.Time { return time.Date(2026, 11, 5, 1, 0, 0, 0, time.UTC) }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "[Inbox]") {
		t.Fatalf("view missing inbox marker\n%s", view)
	}
	if strings.Contains(view, answered.ID) {
		t.Fatalf("inbox shows answered item\n%s", view)
	}
	order := []string{unread.ID, "later deadline", newer.ID, older.ID}
	pos := -1
	for _, needle := range order {
		at := strings.Index(view, needle)
		if at < 0 || at < pos {
			t.Fatalf("order missing %q at %d after %d\n%s", needle, at, pos, view)
		}
		pos = at
	}
	if !lineBold(view, unread.ID) || !lineBold(view, newer.ID) {
		t.Fatalf("unread rows are not bold\n%s", view)
	}
	if lineBold(view, older.ID) {
		t.Fatalf("read row is bold\n%s", view)
	}
	if !strings.Contains(view, "later deadline") || strings.Contains(view, "later\n") {
		t.Fatalf("question is not one line\n%s", view)
	}
}

func TestFolderKeysShowEachState(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	openItem := mustCreate(t, led, &decide.Item{Question: "open q", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen})
	answered := mustCreate(t, led, &decide.Item{Question: "answered q", Owner: "ada", Kind: decide.KindRelay, State: decide.StateAnswered})
	done := mustCreate(t, led, &decide.Item{Question: "done q", Owner: "ada", Kind: decide.KindTypeIn, State: decide.StateDone, TypeText: "x", Where: "y"})
	withdrawn := mustCreate(t, led, &decide.Item{Question: "withdrawn q", Owner: "ada", Kind: decide.KindRelay, State: decide.StateWithdrawn})

	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), openItem.ID) {
		t.Fatalf("inbox view:\n%s", m.View())
	}
	for _, want := range []struct {
		keys string
		id   string
		name string
	}{
		{keys: "ga", id: answered.ID, name: "[Answered]"},
		{keys: "gd", id: done.ID, name: "[Done]"},
		{keys: "gw", id: withdrawn.ID, name: "[Withdrawn]"},
	} {
		press(t, m, want.keys)
		view := m.View()
		if !strings.Contains(view, want.name) || !strings.Contains(view, want.id) {
			t.Fatalf("after %q missing %s %s\n%s", want.keys, want.name, want.id, view)
		}
	}
	press(t, m, "gd")
	if !strings.Contains(m.View(), "[Done]") {
		t.Fatalf("chord did not return to Done\n%s", m.View())
	}
}

func TestStatusFolderIsSelectable(t *testing.T) {
	dir := t.TempDir()
	if _, err := decide.Open(dir); err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 6, 12, 0, 0, 0, time.UTC)
	board, err := decide.OpenStatus(dir)
	if err != nil {
		t.Fatal(err)
	}
	board.SetClock(func() time.Time { return now })
	if _, err := board.SetAgent("alpha", "working", "fixing the inbox"); err != nil {
		t.Fatal(err)
	}
	m := New(dir)
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	for _, width := range []int{70, 120} {
		m.width = width
		m.height = 24
		m.mode = modeList
		m.folder = 0
		view := m.View()
		for _, name := range []string{"[Inbox]", "Answered", "Done", "Withdrawn", "Status"} {
			if !strings.Contains(view, name) {
				t.Fatalf("width %d missing %s\n%s", width, name, view)
			}
		}
		if strings.Contains(view, "[Status]") {
			t.Fatalf("width %d highlighted Status from the inbox\n%s", width, view)
		}
		inbox := screenRow(view, "[Inbox]")
		status := screenRow(view, "Status")
		if width < narrowWidth && status != inbox {
			t.Fatalf("width %d Status left the tab row\n%s", width, view)
		}
		if width >= narrowWidth && status == inbox {
			t.Fatalf("width %d Status shares the Inbox row\n%s", width, view)
		}
	}
	m.width = 120
	m.height = 24
	press(t, m, "gs")
	view := m.View()
	if m.mode != modeStatus || !strings.Contains(view, "[Status]") || strings.Contains(view, "[Inbox]") {
		t.Fatalf("gs mode %d\n%s", m.mode, view)
	}
	for _, needle := range []string{"alpha", "working", "fixing the inbox"} {
		if !strings.Contains(view, needle) {
			t.Fatalf("gs hid the board, missing %q\n%s", needle, view)
		}
	}
	if screenRow(view, "[Status]") == screenRow(view, "Inbox") {
		t.Fatalf("wide Status is not its own row\n%s", view)
	}
	press(t, m, "gi")
	if m.mode != modeList || m.folder != 0 || !strings.Contains(m.View(), "[Inbox]") || strings.Contains(m.View(), "[Status]") {
		t.Fatalf("gi mode %d folder %d\n%s", m.mode, m.folder, m.View())
	}
	row := screenRow(m.View(), "Status")
	line := strings.Split(m.View(), "\n")[row-1]
	if err := m.Click(visibleCol(line, "Status"), row); err != nil || m.mode != modeStatus || !strings.Contains(m.View(), "[Status]") {
		t.Fatalf("click status mode %d\n%s", m.mode, m.View())
	}
	m.width = 70
	view = m.View()
	if screenRow(view, "[Status]") != screenRow(view, "Inbox") {
		t.Fatalf("narrow Status left the tab row\n%s", view)
	}
	press(t, m, "gs")
	if m.mode != modeStatus || !strings.Contains(m.View(), "[Status]") {
		t.Fatalf("gs again mode %d\n%s", m.mode, m.View())
	}
	press(t, m, "ga")
	if m.mode != modeList || m.folder != 1 || !strings.Contains(m.View(), "[Answered]") || strings.Contains(m.View(), "[Status]") {
		t.Fatalf("ga from status mode %d folder %d\n%s", m.mode, m.folder, m.View())
	}
}

func TestFolderCountsShareAColumn(t *testing.T) {
	dir := t.TempDir()
	if _, err := decide.Open(dir); err != nil {
		t.Fatal(err)
	}
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.counts = []int{1, 12, 123, 0}
	m.width = 120
	m.height = 24

	assertFolderCountColumn(t, m)

	m.folder = 2
	assertFolderCountColumn(t, m)
	done := m.sidebarLines()[2].text
	if !strings.HasPrefix(done, "[Done]") || strings.Contains(m.sidebarLines()[0].text, "[Inbox]") {
		t.Fatalf("selection did not stay on Done\n%s", done)
	}

	m.mode = modeStatus
	assertFolderCountColumn(t, m)
	status := m.sidebarLines()[len(Folders)].text
	if !strings.HasPrefix(status, "[Status]") {
		t.Fatalf("status row %q", status)
	}

	m.mode = modeList
	m.folder = 0
	m.width = 70
	view := m.View()
	if screenRow(view, "[Inbox]") != screenRow(view, "Status") || screenRow(view, "Withdrawn") != screenRow(view, "[Inbox]") {
		t.Fatalf("narrow folders are not one row\n%s", view)
	}
	for _, name := range []string{"[Inbox]", "Answered", "Done", "Withdrawn", "Status"} {
		if !strings.Contains(view, name) {
			t.Fatalf("narrow row missing %s\n%s", name, view)
		}
	}
}

func assertFolderCountColumn(t *testing.T, m *Model) {
	t.Helper()
	lines := m.sidebarLines()
	if len(lines) != len(Folders)+1 {
		t.Fatalf("sidebar rows %d", len(lines))
	}
	ends := make([]int, len(Folders))
	width := 0
	for i, count := range []string{"1", "12", "123", "0"} {
		text := lines[i].text
		name := Folders[i].Name
		if m.navSelected(i) {
			name = "[" + name + "]"
		}
		if !strings.HasPrefix(text, name) {
			t.Fatalf("row %d %q does not start with %q", i, text, name)
		}
		at := strings.LastIndex(text, count)
		if at < 0 || at+len(count) != len(strings.TrimRight(text, " ")) {
			t.Fatalf("row %d count %s in %q", i, count, text)
		}
		ends[i] = at + len(count)
		if lipgloss.Width(text) > width {
			width = lipgloss.Width(text)
		}
	}
	for i := 1; i < len(ends); i++ {
		if ends[i] != ends[0] {
			t.Fatalf("count end %d vs %d\n%q\n%q", ends[i], ends[0], lines[i].text, lines[0].text)
		}
	}
	status := lines[len(Folders)].text
	if strings.TrimRight(status, " ") != statusName(m) {
		t.Fatalf("status count %q", status)
	}
	if lipgloss.Width(status) != width || ends[0] != width {
		t.Fatalf("status width %d row width %d count end %d %q", lipgloss.Width(status), width, ends[0], status)
	}
}

func statusName(m *Model) string {
	if m.mode == modeStatus {
		return "[Status]"
	}
	return "Status"
}

func TestEnterClearsUnread(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "read me",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Unread:   true,
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Unread {
		t.Fatal("unread still set")
	}
	if got.State != decide.StateOpen || got.Question != "read me" {
		t.Fatalf("enter changed the item: %+v", got)
	}
	if lineBold(m.View(), item.ID) {
		t.Fatalf("row stayed bold\n%s", m.View())
	}
}

func TestJAndKMove(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	first := mustCreate(t, led, &decide.Item{
		Question:  "first",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	second := mustCreate(t, led, &decide.Item{
		Question:  "second",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if !strings.HasPrefix(selectedLine(m.View()), "> "+first.ID) && !strings.Contains(selectedLine(m.View()), first.ID) {
		t.Fatalf("cursor = %q", selectedLine(m.View()))
	}
	if _, err := m.Key('j'); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(selectedLine(m.View()), second.ID) {
		t.Fatalf("after j cursor = %q", selectedLine(m.View()))
	}
	if _, err := m.Key('k'); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(selectedLine(m.View()), first.ID) {
		t.Fatalf("after k cursor = %q", selectedLine(m.View()))
	}
}

func TestWriteWhileOpenAppearsWithoutRestart(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	inR, inW := io.Pipe()
	t.Cleanup(func() { _ = inW.Close() })
	out := &lockedBuf{}
	errCh := make(chan error, 1)
	go func() {
		errCh <- Run(Options{Dir: dir, In: inR, Out: out, Interval: 15 * time.Millisecond})
	}()

	item := mustCreate(t, led, &decide.Item{
		Question: "from the cli",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
	})
	deadline := time.Now().Add(3 * time.Second)
	for !strings.Contains(out.String(), item.ID) {
		if time.Now().After(deadline) {
			t.Fatalf("inbox did not show %s\n%s", item.ID, out.String())
		}
		time.Sleep(15 * time.Millisecond)
	}
	if _, err := inW.Write([]byte{'q'}); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-errCh:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("inbox did not quit")
	}
}

func TestDetailOneAnswersFirstOption(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	deadline := time.Date(2026, 12, 1, 15, 4, 5, 0, time.UTC)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options: []decide.Option{
			{Key: "a", Label: "Alpha", Recommended: true},
			{Key: "b", Label: "Beta"},
		},
		Links:    []string{"https://example.test/spec"},
		Deadline: &deadline,
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r")
	view := m.View()
	for _, needle := range []string{
		"ship it?",
		"1 a Alpha recommended",
		"2 b Beta",
		"https://example.test/spec",
		deadline.Format(time.RFC3339),
	} {
		if !strings.Contains(view, needle) {
			t.Fatalf("detail missing %q\n%s", needle, view)
		}
	}
	press(t, m, "1")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "a" || got.Answer.AnsweredBy != "human" {
		t.Fatalf("answer = %+v state %s", got.Answer, got.State)
	}
}

func TestDetailFreeTextSubmitsOnlyWhenSet(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	openFree := mustCreate(t, led, &decide.Item{
		Question: "or else",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		FreeText: true,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	closed := mustCreate(t, led, &decide.Item{
		Question:  "pick a key",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  false,
		Options:   []decide.Option{{Key: "custom", Label: "Custom"}},
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r0custom words")
	if !strings.Contains(m.View(), "answer: custom words") {
		t.Fatalf("text box missing draft\n%s", m.View())
	}
	press(t, m, "\r")
	got, err := led.Get(openFree.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "custom words" || got.Answer.AnsweredBy != "human" {
		t.Fatalf("free text = %+v state %s", got.Answer, got.State)
	}

	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "j\r")
	if strings.Contains(m.View(), "answer:") {
		t.Fatalf("options-only item offered a free-text box\n%s", m.View())
	}
	press(t, m, "zzzz\r")
	view := m.View()
	if !strings.Contains(view, "free text is not allowed") {
		t.Fatalf("free text was not rejected\n%s", view)
	}
	denied, err := led.Get(closed.ID)
	if err != nil {
		t.Fatal(err)
	}
	if denied.State != decide.StateOpen || denied.Answer != nil {
		t.Fatalf("rejected item changed: %+v", denied.Answer)
	}
}

func TestDetailTypeInMarkDone(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "type this",
		Owner:    "ada",
		Kind:     decide.KindTypeIn,
		State:    decide.StateOpen,
		TypeText: "ship it",
		Where:    "the pane",
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r")
	view := m.View()
	if !strings.Contains(view, "ship it") || !strings.Contains(view, "the pane") {
		t.Fatalf("type-in detail missing text or place\n%s", view)
	}
	if strings.Contains(view, "\n1 ") || strings.Contains(view, "answer:") {
		t.Fatalf("type-in has an answer control\n%s", view)
	}
	press(t, m, "1")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("type-in accepted an answer: %+v", got)
	}
	press(t, m, "e")
	if !strings.Contains(m.View(), "outcome:") {
		t.Fatalf("mark done did not ask for an outcome\n%s", m.View())
	}
	press(t, m, "typed it\r")
	got, err = led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateDone || got.Outcome != "typed it" {
		t.Fatalf("done = state %s outcome %q", got.State, got.Outcome)
	}
}

func TestDetailEMarksDone(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "archive me",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\re")
	if !strings.Contains(m.View(), "outcome:") {
		t.Fatalf("e did not ask for an outcome\n%s", m.View())
	}
	press(t, m, "kept\r")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateDone || got.Outcome != "kept" {
		t.Fatalf("done = state %s outcome %q", got.State, got.Outcome)
	}
}

func TestClickOpensRowSwitchesFolderAndScrolls(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	older := mustCreate(t, led, &decide.Item{
		Question:  "older row",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Unread:    true,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	newer := mustCreate(t, led, &decide.Item{
		Question:  "newer row",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	third := mustCreate(t, led, &decide.Item{
		Question:  "third row",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 3, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.height = 10
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || strings.Contains(view, older.ID) {
		t.Fatalf("initial window = %s", view)
	}
	if strings.Contains(view, "to:") || !strings.Contains(view, "? help") {
		t.Fatalf("list chrome =\n%s", view)
	}
	m.Wheel(true)
	view = m.View()
	if strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || !strings.Contains(view, older.ID) {
		t.Fatalf("scrolled window = %s", view)
	}
	if strings.Contains(view, "to:") || !strings.Contains(view, "? help") {
		t.Fatalf("list chrome left the scrolled window\n%s", view)
	}
	m.Wheel(false)
	if !strings.Contains(m.View(), third.ID) {
		t.Fatalf("wheel up = %s", m.View())
	}
	m.scroll = 0
	m.height = 0
	if err := m.Click(1, screenRow(m.View(), "older row")); err != nil {
		t.Fatal(err)
	}
	got, err := led.Get(older.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Unread || got.State != decide.StateOpen || !strings.Contains(m.View(), "older row") {
		t.Fatalf("click open = unread %v state %s\n%s", got.Unread, got.State, m.View())
	}
	if _, err := m.Key('u'); err != nil {
		t.Fatal(err)
	}
	answeredRow := screenRow(m.View(), "Answered")
	answeredLine := strings.Split(m.View(), "\n")[answeredRow-1]
	if err := m.Click(visibleCol(answeredLine, "Answered"), answeredRow); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "[Answered]") || strings.Contains(m.View(), "older row") {
		t.Fatalf("folder click = %s", m.View())
	}
}

func TestWheelScrollsOpenItem(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "which way",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options: []decide.Option{
			{Key: "a", Label: "Alpha"},
			{Key: "b", Label: "Beta"},
			{Key: "c", Label: "Gamma"},
		},
	})
	m := New(dir)
	m.height = 4
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r")
	view := m.View()
	if top := lineAfterBorder(view); top != item.ID {
		t.Fatalf("detail top = %q\n%s", top, view)
	}
	m.Wheel(true)
	top := lineAfterBorder(m.View())
	if top == item.ID || top != "which way" {
		t.Fatalf("scrolled detail top = %q\n%s", top, m.View())
	}
}

func lineAfterBorder(view string) string {
	lines := strings.Split(view, "\n")
	for i, line := range lines {
		if strings.Contains(visibleLine(line), "┌") && i+1 < len(lines) {
			inner := strings.TrimPrefix(visibleLine(lines[i+1]), "│")
			inner = strings.TrimSuffix(inner, "│")
			return strings.TrimSpace(inner)
		}
	}
	if len(lines) > 1 {
		return visibleLine(lines[1])
	}
	return ""
}

func TestFreeTextIsRowZero(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	two := mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		CreatedAt: time.Date(2026, 11, 4, 0, 0, 0, 0, time.UTC),
		Options: []decide.Option{
			{Key: "a", Label: "Alpha"},
			{Key: "b", Label: "Beta"},
		},
	})
	nineOpts := make([]decide.Option, 9)
	for i := range nineOpts {
		nineOpts[i] = decide.Option{Key: string(rune('a' + i)), Label: "Choice"}
	}
	nine := mustCreate(t, led, &decide.Item{
		Question:  "nine choices",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		Options:   nineOpts,
		CreatedAt: time.Date(2026, 11, 3, 0, 0, 0, 0, time.UTC),
	})
	escItem := mustCreate(t, led, &decide.Item{
		Question:  "keep draft?",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}},
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	only := mustCreate(t, led, &decide.Item{
		Question:  "options only",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  false,
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "?")
	if !strings.Contains(m.View(), "esc keeps the draft") {
		t.Fatalf("help does not document esc\n%s", m.View())
	}
	if _, err := m.Key('?'); err != nil {
		t.Fatal(err)
	}

	press(t, m, "\r")
	view := m.View()
	opt := strings.Index(view, "2 b Beta")
	zero := strings.Index(view, "0  answer:")
	if opt < 0 || zero < opt || strings.Contains(view, "\n10 ") {
		t.Fatalf("free text is not last row 0\n%s", view)
	}
	if err := m.Click(1, screenRow(view, "0  answer:")); err != nil {
		t.Fatal(err)
	}
	got, err := led.Get(two.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer != nil || !strings.Contains(m.View(), "0  answer:") || !strings.Contains(m.View(), cursorOn) || !strings.Contains(m.View(), "Esc to cancel") {
		t.Fatalf("click submitted or hid the cursor\n%s", m.View())
	}
	press(t, m, "1\r")
	got, err = led.Get(two.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Value != "1" {
		t.Fatalf("focused 1 = %+v", got.Answer)
	}

	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r")
	view = m.View()
	if strings.Contains(view, "\n10 ") || !strings.Contains(view, "9 i Choice") || !strings.Contains(view, "0  answer:") {
		t.Fatalf("nine options = %s", view)
	}
	press(t, m, "0")
	got, err = led.Get(nine.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer != nil || !m.answerFocus {
		t.Fatalf("0 submitted a nine-option item: %+v", got.Answer)
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}

	press(t, m, "j\r0keep")
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	got, err = led.Get(escItem.ID)
	if err != nil {
		t.Fatal(err)
	}
	if m.answerFocus || m.draft != "keep" || got.Answer != nil {
		t.Fatalf("esc focus %v draft %q answer %+v", m.answerFocus, m.draft, got.Answer)
	}
	press(t, m, "0")
	if !strings.Contains(m.View(), "answer: keep") || !strings.Contains(m.View(), cursorOn) {
		t.Fatalf("draft was dropped\n%s", m.View())
	}
	if err := m.Click(1, screenRow(m.View(), "keep draft?")); err != nil {
		t.Fatal(err)
	}
	got, err = led.Get(escItem.ID)
	if err != nil {
		t.Fatal(err)
	}
	if m.answerFocus || m.draft != "keep" || got.Answer != nil {
		t.Fatalf("outside click focus %v draft %q answer %+v", m.answerFocus, m.draft, got.Answer)
	}
	press(t, m, "1")
	got, err = led.Get(escItem.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Value != "a" {
		t.Fatalf("digit after cancel = %+v", got.Answer)
	}

	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "j\r")
	if strings.Contains(m.View(), "0  answer:") || strings.Contains(m.View(), "answer:") {
		t.Fatalf("options-only row\n%s", m.View())
	}
	press(t, m, "0")
	got, err = led.Get(only.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer != nil {
		t.Fatalf("options-only 0 answered: %+v", got.Answer)
	}
}

func TestClickAnswersOptionAndFreeText(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	relay := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options: []decide.Option{
			{Key: "a", Label: "Alpha", Recommended: true},
			{Key: "b", Label: "Beta"},
		},
	})
	free := mustCreate(t, led, &decide.Item{
		Question:  "or else",
		Owner:     "bea",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r")
	if err := m.Click(1, screenRow(m.View(), "1 a Alpha")); err != nil {
		t.Fatal(err)
	}
	got, err := led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "a" || got.Answer.AnsweredBy != "human" {
		t.Fatalf("option click = %+v", got.Answer)
	}

	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.Click(1, screenRow(m.View(), free.ID)); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if err := m.Click(1, screenRow(view, "answer:")); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "answer") {
		t.Fatalf("free text was not focused\n%s", m.View())
	}
	press(t, m, "custom words\r")
	got, err = led.Get(free.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "custom words" || got.Answer.AnsweredBy != "human" {
		t.Fatalf("free text click = %+v state %s", got.Answer, got.State)
	}
}

func TestClickMarkDoneCompletesTypeIn(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "type this",
		Owner:    "ada",
		Kind:     decide.KindTypeIn,
		State:    decide.StateOpen,
		TypeText: "ship it",
		Where:    "the pane",
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r")
	if !strings.Contains(m.View(), "mark done") {
		t.Fatalf("missing mark done\n%s", m.View())
	}
	if err := m.Click(1, screenRow(m.View(), "mark done")); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "outcome:") {
		t.Fatalf("mark done did not ask\n%s", m.View())
	}
	press(t, m, "typed it\r")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateDone || got.Outcome != "typed it" {
		t.Fatalf("done = %s %q", got.State, got.Outcome)
	}
}

func TestComposeEnterSendsSeparateArguments(t *testing.T) {
	dir := t.TempDir()
	m := New(dir)
	m.Targets = []Target{{Name: "inbox", Pane: "pane-1"}}
	text := `say "hi" $(uname)`
	var got []string
	m.runSend = func(argv []string) error {
		got = append([]string(nil), argv...)
		return nil
	}
	m.composing = true
	m.composeFocus = focusSend
	m.note = text
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	want := []string{"herdr", "agent", "prompt", "pane-1", text}
	if len(got) != len(want) {
		t.Fatalf("argv = %#v", got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("argv = %#v", got)
		}
	}
	if got[0] == "sh" {
		t.Fatal("send used a shell")
	}
	if m.note != "" || m.status != "sent to inbox" || m.composing {
		t.Fatalf("after send note %q status %q composing %v", m.note, m.status, m.composing)
	}
	if !strings.Contains(m.View(), "sent to inbox") || !strings.Contains(m.View(), "? help") || strings.Contains(m.View(), "pane-1") {
		t.Fatalf("footer = %q", m.View())
	}
	data, err := os.ReadFile(filepath.Join(dir, sendsName))
	if err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	if len(lines) != 1 {
		t.Fatalf("history = %q", data)
	}
	var rec sendRecord
	if err := json.Unmarshal([]byte(lines[0]), &rec); err != nil {
		t.Fatal(err)
	}
	if rec.Target != "inbox" || rec.Pane != "pane-1" || rec.Text != text {
		t.Fatalf("record = %+v", rec)
	}
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 1 || len(items[0].Messages) != 1 || items[0].Messages[0].Author != "human" || items[0].Messages[0].Text != text || items[0].Owner != "inbox" || items[0].NotifyPane != "pane-1" {
		t.Fatalf("thread = %+v", items)
	}
}

func TestComposeFailureKeepsText(t *testing.T) {
	dir := t.TempDir()
	m := New(dir)
	m.Targets = []Target{{Name: "inbox", Pane: "pane-1"}}
	text := `say "hi" $(uname)`
	m.runSend = func([]string) error { return fmt.Errorf("boom") }
	m.composing = true
	m.composeFocus = focusSend
	m.note = text
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	if m.note != text || m.status != "boom" {
		t.Fatalf("note %q status %q", m.note, m.status)
	}
	view := m.View()
	if !strings.Contains(view, "boom") || !strings.Contains(view, text) || strings.Contains(view, "pane-1") || m.status == "failed" {
		t.Fatalf("view = %s", view)
	}
	_, err := os.Stat(filepath.Join(dir, sendsName))
	if !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("history err = %v", err)
	}
}

func TestComposeTargetChangesBeforeSend(t *testing.T) {
	dir := t.TempDir()
	m := New(dir)
	m.Targets = []Target{
		{Name: "inbox", Pane: "pane-1"},
		{Name: "alerts", Pane: "pane-2"},
	}
	var got [][]string
	m.runSend = func(argv []string) error {
		got = append(got, append([]string(nil), argv...))
		return nil
	}
	press(t, m, "c\t\t")
	if m.note != "" || m.target != 0 || m.composeFocus != focusSend || len(got) != 0 {
		t.Fatalf("typing note %q target %d focus %d sends %d", m.note, m.target, m.composeFocus, len(got))
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	press(t, m, "]")
	if m.target != 0 || strings.Contains(m.View(), "to:") {
		t.Fatalf("bracket changed the target %d\n%s", m.target, m.View())
	}
	if _, err := m.Key('c'); err != nil {
		t.Fatal(err)
	}
	press(t, m, "j\t note\t\r")
	if len(got) != 1 || len(got[0]) != 5 || got[0][3] != "pane-2" || got[0][4] != " note" {
		t.Fatalf("keyboard send = %#v", got)
	}

	if _, err := m.Key('c'); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	row := screenRow(view, "inbox")
	line := strings.Split(view, "\n")[row-1]
	x := strings.Index(visibleLine(line), "inbox")
	if x < 0 {
		t.Fatalf("target line %q", line)
	}
	if err := m.Click(utf8.RuneCountInString(visibleLine(line)[:x])+1, row); err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || m.target != 0 || !strings.Contains(m.View(), "[inbox]") {
		t.Fatalf("click target %d\n%s", m.target, m.View())
	}
	m.SendTemplate = "tool {pane}"
	press(t, m, "\tnote-b\t")
	sendRow := screenRow(m.View(), "[Send]")
	sendLine := strings.Split(m.View(), "\n")[sendRow-1]
	if err := m.Click(visibleCol(sendLine, "[Send]"), sendRow); err != nil {
		t.Fatal(err)
	}
	want := []string{"tool", "pane-1", "note-b"}
	if len(got) != 2 || len(got[1]) != len(want) {
		t.Fatalf("click send = %#v", got)
	}
	for i := range want {
		if got[1][i] != want[i] {
			t.Fatalf("click send = %#v", got[1])
		}
	}
}

func TestShiftEnterDoesNotSend(t *testing.T) {
	dir := t.TempDir()
	m := New(dir)
	m.Targets = []Target{{Name: "inbox", Pane: "pane-1"}}
	var calls int
	var got string
	m.runSend = func(argv []string) error {
		calls++
		got = argv[len(argv)-1]
		return nil
	}
	m.composing = true
	m.composeFocus = focusText
	m.note = `say "hi"`
	m.ShiftEnter()
	if calls != 0 {
		t.Fatal("shift+enter sent")
	}
	if m.note != "say \"hi\"\n" {
		t.Fatalf("note = %q", m.note)
	}
	if !strings.Contains(m.View(), "say \"hi\"") || strings.Contains(m.View(), "Enter send") {
		t.Fatalf("view = %s", m.View())
	}
	m.composeFocus = focusSend
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	if calls != 1 || got != "say \"hi\"\n" {
		t.Fatalf("calls %d text %q", calls, got)
	}
}

func TestComposeHintKeysAndStatus(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "open q",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 4, 0, 0, 0, 0, time.UTC),
	})
	mustCreate(t, led, &decide.Item{
		Question:  "answered q",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateAnswered,
		CreatedAt: time.Date(2026, 11, 3, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.Targets = []Target{
		{Name: "alpha", Pane: "pane-secret"},
		{Name: "beta", Pane: "pane-2"},
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if strings.Contains(view, "to:") || strings.Contains(view, "compose:") {
		t.Fatalf("list shows compose chrome\n%s", view)
	}
	if !strings.Contains(view, "[Inbox]") {
		t.Fatalf("folder =\n%s", view)
	}
	if _, err := m.Key('l'); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('h'); err != nil {
		t.Fatal(err)
	}
	if m.folder != 0 || !strings.Contains(m.View(), "[Inbox]") || strings.Contains(m.View(), "[Answered]") {
		t.Fatalf("h/l changed folder %d\n%s", m.folder, m.View())
	}
	press(t, m, "c")
	if m.composeFocus != focusRecipient || m.noteField.Focused() {
		t.Fatalf("focus %d field %v", m.composeFocus, m.noteField.Focused())
	}
	press(t, m, "\thi")
	if m.note != "hi" || m.composeFocus != focusText || m.target != 0 {
		t.Fatalf("text note %q focus %d target %d", m.note, m.composeFocus, m.target)
	}
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	if m.note != "hi\n" || m.composing != true {
		t.Fatalf("enter sent or dropped note %q composing %v", m.note, m.composing)
	}
	m.ShiftTab()
	if m.composeFocus != focusRecipient || m.target != 0 {
		t.Fatalf("shift tab focus %d target %d", m.composeFocus, m.target)
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	if m.composing || m.note != "hi\n" || strings.Contains(m.View(), "to:") {
		t.Fatalf("esc note %q composing %v\n%s", m.note, m.composing, m.View())
	}

	if _, err := m.Key('c'); err != nil {
		t.Fatal(err)
	}
	var got []string
	m.runSend = func(argv []string) error {
		got = append([]string(nil), argv...)
		return nil
	}
	press(t, m, "\t\t\r")
	if m.status != "sent to alpha" || strings.Contains(m.View(), "pane-secret") {
		t.Fatalf("status %q\n%s", m.status, m.View())
	}
	if len(got) == 0 || got[len(got)-1] != "hi\n" {
		t.Fatalf("argv = %#v", got)
	}

	m.runSend = func([]string) error { return fmt.Errorf("boom") }
	press(t, m, "c\tagain\t\r")
	if m.note != "again" || m.status != "boom" || strings.Contains(m.status, "failed") || strings.Contains(m.View(), "pane-secret") {
		t.Fatalf("failure note %q status %q\n%s", m.note, m.status, m.View())
	}
}

func TestComposeFocusDoesNotAnswer(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "inbox", Pane: "pane-1"}}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\rc\t1")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil || m.note != "1" {
		t.Fatalf("focused digit answered: state %s note %q answer %+v", got.State, m.note, got.Answer)
	}
}

func TestHelpOverlayShowsAndClosesWithoutSending(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "inbox", Pane: "pane-1"}}
	m.runSend = func([]string) error {
		t.Fatal("overlay sent a note")
		return nil
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "? help") {
		t.Fatalf("footer missing\n%s", m.View())
	}
	cursor, folder := m.cursor, m.folder
	press(t, m, "?")
	view := m.View()
	if !strings.Contains(view, "shortcuts") || !strings.Contains(view, "? help") {
		t.Fatalf("overlay = %s", view)
	}
	if strings.Contains(view, item.ID) || strings.Contains(view, "compose:") || strings.Contains(view, "compose ›") || strings.Contains(view, "[Inbox]") {
		t.Fatalf("overlay did not cover the inbox\n%s", view)
	}
	press(t, m, "1e\r")
	if err := m.Click(1, 1); err != nil {
		t.Fatal(err)
	}
	m.Wheel(true)
	if !m.help || m.cursor != cursor || m.folder != folder {
		t.Fatalf("overlay changed selection help %v cursor %d folder %d", m.help, m.cursor, m.folder)
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	if m.help {
		t.Fatal("esc left the overlay open")
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("overlay changed the item: %+v", got)
	}

	press(t, m, "??")
	if m.help {
		t.Fatal("second ? left the overlay open")
	}
	if !strings.Contains(m.View(), item.ID) || !strings.Contains(m.View(), "? help") || strings.Contains(m.View(), "shortcuts") {
		t.Fatalf("closed view = %s", m.View())
	}

	if _, err := m.Key('c'); err != nil {
		t.Fatal(err)
	}
	if !m.composing || m.composeFocus != focusRecipient || m.noteField.Focused() {
		t.Fatal("c did not focus the recipient picker")
	}
	press(t, m, "\t?")
	if m.help || m.note != "?" || !strings.Contains(m.View(), "?") {
		t.Fatalf("focused ? note %q help %v\n%s", m.note, m.help, m.View())
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	if m.composing || m.note != "?" || strings.Contains(m.View(), "to:") {
		t.Fatalf("esc note %q composing %v\n%s", m.note, m.composing, m.View())
	}

	press(t, m, "?")
	view = m.View()
	if strings.Contains(view, "compose:") || strings.Contains(view, "compose ›") || strings.Contains(view, item.ID) || !strings.Contains(view, "click folder") {
		t.Fatalf("second overlay = %s", view)
	}
	if _, err := m.Key('?'); err != nil {
		t.Fatal(err)
	}
	m.composing = false
	press(t, m, "\r?")
	view = m.View()
	if strings.Contains(view, "ship it?") || strings.Contains(view, "compose:") || strings.Contains(view, "compose ›") || strings.Contains(view, "[Inbox]") || !strings.Contains(view, "shortcuts") {
		t.Fatalf("detail overlay = %s", view)
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	got, err = led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("closing changed the item: %+v", got)
	}
}

func TestBubbleTeaMouseMessages(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	older := mustCreate(t, led, &decide.Item{
		Question:  "older row",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Unread:    true,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	newer := mustCreate(t, led, &decide.Item{
		Question:  "newer row",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	third := mustCreate(t, led, &decide.Item{
		Question:  "third row",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 3, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.height = 10
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || strings.Contains(view, older.ID) {
		t.Fatalf("initial window = %s", view)
	}
	m = updateMouseAt(t, m, 1, 1, tea.MouseButtonWheelDown)
	view = m.View()
	if strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || !strings.Contains(view, older.ID) {
		t.Fatalf("scrolled window = %s", view)
	}
	m = updateMouseAt(t, m, 1, 1, tea.MouseButtonWheelUp)
	if !strings.Contains(m.View(), third.ID) {
		t.Fatalf("wheel up = %s", m.View())
	}
	m.scroll = 0
	m.height = 0
	m = updateMouseAt(t, m, 1, screenRow(m.View(), "older row"), tea.MouseButtonLeft)
	got, err := led.Get(older.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Unread || got.State != decide.StateOpen || !strings.Contains(m.View(), "older row") {
		t.Fatalf("click open = unread %v state %s\n%s", got.Unread, got.State, m.View())
	}
	if _, err := m.Key('u'); err != nil {
		t.Fatal(err)
	}
	answeredRow := screenRow(m.View(), "Answered")
	answeredLine := strings.Split(m.View(), "\n")[answeredRow-1]
	m = updateMouseAt(t, m, visibleCol(answeredLine, "Answered"), answeredRow, tea.MouseButtonLeft)
	if !strings.Contains(m.View(), "[Answered]") || strings.Contains(m.View(), "older row") {
		t.Fatalf("folder click = %s", m.View())
	}

	relay := mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		CreatedAt: time.Date(2026, 11, 4, 0, 0, 0, 0, time.UTC),
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m.folder = 0
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.Targets = []Target{{Name: "alpha", Pane: "pane-secret"}, {Name: "beta", Pane: "pane-2"}}
	m = updateMouseAt(t, m, 1, screenRow(m.View(), "ship it?"), tea.MouseButtonLeft)
	if _, err := m.Key('c'); err != nil {
		t.Fatal(err)
	}
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if !m.composing || m.composeFocus != focusRecipient || m.noteField.Focused() || m.folder != 0 || got.Answer != nil {
		t.Fatalf("compose focus %d folder %d answer %+v", m.composeFocus, m.folder, got.Answer)
	}
	targetRow := screenRow(m.View(), "beta")
	targetLine := strings.Split(m.View(), "\n")[targetRow-1]
	m = updateMouseAt(t, m, visibleCol(targetLine, "beta"), targetRow, tea.MouseButtonLeft)
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if m.target != 1 || !strings.Contains(m.View(), "beta") || m.folder != 0 || got.Answer != nil {
		t.Fatalf("target click target %d folder %d answer %+v\n%s", m.target, m.folder, got.Answer, m.View())
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	m = updateMouseAt(t, m, 1, screenRow(m.View(), "0  answer:"), tea.MouseButtonLeft)
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if !m.answerFocus || m.folder != 0 || got.Answer != nil {
		t.Fatalf("answer focus %v folder %d answer %+v", m.answerFocus, m.folder, got.Answer)
	}
	m = updateMouseAt(t, m, 1, screenRow(m.View(), "1 a Alpha"), tea.MouseButtonLeft)
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if m.answerFocus || m.folder != 0 || got.Answer != nil {
		t.Fatalf("focused option click focus %v folder %d answer %+v", m.answerFocus, m.folder, got.Answer)
	}
	m = updateMouseAt(t, m, 1, screenRow(m.View(), "0  answer:"), tea.MouseButtonLeft)
	idRow := screenRow(m.View(), relay.ID)
	m = updateMouseAt(t, m, 1, idRow, tea.MouseButtonLeft)
	if m.folder != 0 || !strings.Contains(m.View(), "ship it?") {
		t.Fatalf("focused outside click folder %d\n%s", m.folder, m.View())
	}
	m = updateMouseAt(t, m, 1, screenRow(m.View(), "1 a Alpha"), tea.MouseButtonLeft)
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "a" || m.folder != 0 {
		t.Fatalf("option click = %+v folder %d", got.Answer, m.folder)
	}
}

func TestInboxRelayAnswerRunsNotifyOnce(t *testing.T) {
	dir := t.TempDir()
	cfgDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	logPath := filepath.Join(dir, "notify.log")
	installTUINotifier(t, logPath, false)
	writeTUINotifyConfig(t, cfgDir, "notifier", "default-pane")

	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question:   "pick one",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		State:      decide.StateOpen,
		NotifyPane: "pane-9",
		Options:    []decide.Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}},
		CreatedAt:  time.Date(2026, 11, 5, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r1")
	want := []string{item.ID, "ada", "pane-9", "a"}
	if got := notifyLog(t, logPath); !sameLines(got, want) {
		t.Fatalf("notify log = %#v", got)
	}
	saved, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if saved.State != decide.StateAnswered || saved.Answer == nil || saved.Answer.Value != "a" {
		t.Fatalf("answer = %+v state %s", saved.Answer, saved.State)
	}

	typeIn := mustCreate(t, led, &decide.Item{
		Question: "type this",
		Owner:    "ada",
		Kind:     decide.KindTypeIn,
		State:    decide.StateOpen,
		TypeText: "ship it",
		Where:    "the pane",
	})
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\retyped it\r")
	if got := notifyLog(t, logPath); !sameLines(got, want) {
		t.Fatalf("type-in ran notify: %#v", got)
	}
	done, err := led.Get(typeIn.ID)
	if err != nil {
		t.Fatal(err)
	}
	if done.State != decide.StateDone || done.Outcome != "typed it" {
		t.Fatalf("done = state %s outcome %q", done.State, done.Outcome)
	}
}

func TestInboxNotifyFailureKeepsAnswer(t *testing.T) {
	dir := t.TempDir()
	cfgDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	installTUINotifier(t, filepath.Join(dir, "notify.log"), true)
	writeTUINotifyConfig(t, cfgDir, "notifier", "pane")

	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question:  "keep me",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
		CreatedAt: time.Date(2026, 11, 5, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r1")
	if !strings.Contains(m.View(), "notify") {
		t.Fatalf("view did not show the notify failure\n%s", m.View())
	}
	saved, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if saved.State != decide.StateAnswered || saved.Answer == nil || saved.Answer.Value != "a" {
		t.Fatalf("answer was not kept: %+v state %s", saved.Answer, saved.State)
	}
}

func installTUINotifier(t *testing.T, logPath string, fail bool) {
	t.Helper()
	script := "#!/bin/sh\n" +
		"printf '%s\\n' \"$DECIDE_ID\" >> '" + logPath + "'\n" +
		"printf '%s\\n' \"$DECIDE_OWNER\" >> '" + logPath + "'\n" +
		"printf '%s\\n' \"$DECIDE_PANE\" >> '" + logPath + "'\n" +
		"printf '%s\\n' \"$DECIDE_ANSWER\" >> '" + logPath + "'\n"
	if fail {
		script += "exit 1\n"
	}
	bin := filepath.Join(t.TempDir(), "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(bin, "notifier"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func writeTUINotifyConfig(t *testing.T, cfgDir, template, pane string) {
	t.Helper()
	dir := filepath.Join(cfgDir, "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body, err := json.Marshal(map[string]string{
		"notifyTemplate": template,
		"notifyPane":     pane,
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.json"), append(body, '\n'), 0o600); err != nil {
		t.Fatal(err)
	}
}

func notifyLog(t *testing.T, path string) []string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return strings.Split(strings.TrimSpace(string(data)), "\n")
}

func sameLines(got, want []string) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range want {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}

func openLedger(t *testing.T, dir string) *decide.Ledger {
	t.Helper()
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	return led
}

func mustCreate(t *testing.T, led *decide.Ledger, item *decide.Item) decide.Item {
	t.Helper()
	got, err := led.Create(item)
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func TestFocusedTextDoesNotTriggerShortcuts(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	relay := mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		CreatedAt: time.Date(2026, 11, 5, 0, 0, 0, 0, time.UTC),
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	typeIn := mustCreate(t, led, &decide.Item{
		Question:  "type this",
		Owner:     "ada",
		Kind:      decide.KindTypeIn,
		State:     decide.StateOpen,
		TypeText:  "ship it",
		Where:     "the pane",
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	cursor := m.cursor
	m = updateRune(t, m, '?')
	if !m.help || m.cursor != cursor {
		t.Fatalf("help %v cursor %d", m.help, m.cursor)
	}
	m = updateRune(t, m, '?')
	if m.help {
		t.Fatal("overlay stayed open")
	}

	m = updateRune(t, m, '\r')
	m = updateRune(t, m, '0')
	if !m.answerFocus || !m.free.Focused() {
		t.Fatal("free text is not a focused text input")
	}
	for _, r := range "A?1" {
		m = updateRune(t, m, r)
	}
	if m.draft != "A?1" || m.free.Value() != "A?1" || m.help || m.cursor != cursor {
		t.Fatalf("free text draft %q help %v cursor %d", m.draft, m.help, m.cursor)
	}
	got, err := led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer != nil || m.mode != modeDetail {
		t.Fatalf("focused keys triggered an answer: %+v mode %d", got.Answer, m.mode)
	}
	if !strings.Contains(m.View(), "0  answer:") {
		t.Fatalf("free text is not row 0\n%s", m.View())
	}

	m = updateRune(t, m, '\x1b')
	if m.answerFocus || m.draft != "A?1" {
		t.Fatalf("esc focus %v draft %q", m.answerFocus, m.draft)
	}
	m = updateRune(t, m, '1')
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Value != "a" {
		t.Fatalf("unfocused 1 = %+v", got.Answer)
	}

	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m = updateRune(t, m, '\r')
	m = updateRune(t, m, 'e')
	if m.mode != modeOutcome || !m.outcome.Focused() {
		t.Fatal("outcome is not a focused text input")
	}
	for _, r := range "A?1" {
		m = updateRune(t, m, r)
	}
	if m.draft != "A?1" || m.outcome.Value() != "A?1" || m.help {
		t.Fatalf("outcome draft %q help %v", m.draft, m.help)
	}
	got, err = led.Get(typeIn.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Outcome != "" {
		t.Fatalf("outcome keys marked done: %+v", got)
	}
}

func TestFocusedComposeDoesNotTriggerShortcuts(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	relay := mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 5, 0, 0, 0, 0, time.UTC),
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "alpha", Pane: "pane-secret"}, {Name: "beta", Pane: "pane-2"}}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	folder := m.folder
	m = updateRune(t, m, 'c')
	if !m.composing || m.composeFocus != focusRecipient || m.noteField.Focused() {
		t.Fatal("c did not focus the recipient picker")
	}
	m = updateRune(t, m, '\t')
	for _, r := range "A?1" {
		m = updateRune(t, m, r)
	}
	if m.note != "A?1" || m.noteField.Value() != "A?1" || m.help || m.folder != folder {
		t.Fatalf("note %q field %q help %v folder %d", m.note, m.noteField.Value(), m.help, m.folder)
	}
	if strings.Contains(m.View(), "Enter send") || strings.Contains(m.View(), "to:") {
		t.Fatalf("hint or recipient chips\n%s", m.View())
	}
	got, err := led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("focused keys triggered an answer: %+v", got)
	}
	m = updateRune(t, m, ']')
	if m.note != "A?1]" || m.target != 0 {
		t.Fatalf("bracket note %q target %d\n%s", m.note, m.target, m.View())
	}
	m = updateRune(t, m, '\x7f')
	if m.note != "A?1" {
		t.Fatalf("backspace note %q", m.note)
	}
	m = updateRune(t, m, '\t')
	if m.note != "A?1" || m.target != 0 || m.composeFocus != focusSend {
		t.Fatalf("tab while focused note %q target %d focus %d", m.note, m.target, m.composeFocus)
	}
	m.ShiftTab()
	if m.note != "A?1" || m.target != 0 || m.composeFocus != focusText {
		t.Fatalf("shift tab note %q target %d focus %d", m.note, m.target, m.composeFocus)
	}
	m = updateRune(t, m, '\x1b')
	if m.composing || m.noteField.Focused() || m.note != "A?1" {
		t.Fatalf("esc composing %v focused %v note %q", m.composing, m.noteField.Focused(), m.note)
	}
	m = updateRune(t, m, 'c')
	m = updateRune(t, m, '\t')
	m = updateRune(t, m, '\t')
	var sent []string
	m.runSend = func(argv []string) error {
		sent = append([]string(nil), argv...)
		return nil
	}
	m = updateRune(t, m, '\r')
	if m.status != "sent to alpha" || m.note != "" || m.noteField.Value() != "" || m.composing {
		t.Fatalf("status %q note %q field %q composing %v", m.status, m.note, m.noteField.Value(), m.composing)
	}
	if len(sent) == 0 || sent[len(sent)-1] != "A?1" || strings.Contains(m.View(), "pane-secret") {
		t.Fatalf("argv %#v\n%s", sent, m.View())
	}
}

func TestRunIsBubbleTeaProgram(t *testing.T) {
	dir := t.TempDir()
	r, w := io.Pipe()
	var out bytes.Buffer
	done := make(chan error, 1)
	go func() {
		done <- Run(Options{Dir: dir, In: r, Out: &out, Interval: time.Hour})
	}()
	if _, err := w.Write([]byte{'q'}); err != nil {
		t.Fatal(err)
	}
	if err := w.Close(); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("inbox did not quit")
	}
	if !strings.Contains(out.String(), "[Inbox]") {
		t.Fatalf("bubbletea view = %q", out.String())
	}
	if strings.Contains(out.String(), "\x1b[>4;2m") {
		t.Fatalf("startup requested modifyOtherKeys:\n%s", out.String())
	}
}

func TestRunComposeInsertsTypedText(t *testing.T) {
	dir := t.TempDir()
	r, w := io.Pipe()
	var out bytes.Buffer
	done := make(chan error, 1)
	go func() {
		done <- Run(Options{
			Dir:      dir,
			In:       r,
			Out:      &out,
			Interval: time.Hour,
			Targets:  []Target{{Name: "alpha", Pane: "pane-1"}},
		})
	}()
	if _, err := w.Write([]byte("c\tHello? [A]")); err != nil {
		t.Fatal(err)
	}
	time.Sleep(200 * time.Millisecond)
	if _, err := w.Write([]byte{0x1b}); err != nil {
		t.Fatal(err)
	}
	time.Sleep(200 * time.Millisecond)
	if _, err := w.Write([]byte{'q'}); err != nil {
		t.Fatal(err)
	}
	if err := w.Close(); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("inbox did not quit")
	}
	view := out.String()
	if !strings.Contains(view, "Hello? [A]") {
		t.Fatalf("compose field = %q", view)
	}
	if strings.Contains(view, "\x1b[>4;2m") || strings.Contains(view, "shortcuts") {
		t.Fatalf("startup or overlay = %q", view)
	}
}

func TestRunMouseClickIsMouseMsg(t *testing.T) {
	dir := t.TempDir()
	r, w := io.Pipe()
	var out bytes.Buffer
	done := make(chan error, 1)
	go func() {
		done <- Run(Options{Dir: dir, In: r, Out: &out, Interval: time.Hour})
	}()
	if _, err := w.Write([]byte("\x1b[<0;1;1Mq")); err != nil {
		t.Fatal(err)
	}
	if err := w.Close(); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("inbox did not quit")
	}
	view := out.String()
	if strings.Contains(view, "<0;1;1M") || strings.Contains(view, "\x1b[>4;2m") {
		t.Fatalf("mouse bytes were typed:\n%s", view)
	}
}

func TestProgramShiftKeysDriveCompose(t *testing.T) {
	m := New(t.TempDir())
	m.Targets = []Target{{Name: "alpha", Pane: "p1"}, {Name: "beta", Pane: "p2"}}
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'c'}})
	m = next.(*Model)
	next, _ = m.Update(tea.KeyMsg{Type: tea.KeyTab})
	m = next.(*Model)
	if m.composeFocus != focusText || m.target != 0 {
		t.Fatalf("tab focus %d target %d", m.composeFocus, m.target)
	}
	m.ShiftEnter()
	if m.note != "\n" {
		t.Fatalf("shift enter note %q", m.note)
	}
	next, _ = m.Update(tea.KeyMsg{Type: tea.KeyShiftTab})
	m = next.(*Model)
	if m.composeFocus != focusRecipient || m.target != 0 || m.note != "\n" {
		t.Fatalf("shift tab focus %d target %d note %q", m.composeFocus, m.target, m.note)
	}
}

func TestFocusedElementHasOneBorder(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		FreeText: true,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "alpha", Pane: "pane-1"}}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if strings.Count(view, "┌") != 1 || !betweenBox(view, "ship it?") || betweenBox(view, "compose:") {
		t.Fatalf("list border =\n%s", view)
	}
	m = updateRune(t, m, 'c')
	m = updateRune(t, m, '\t')
	for _, r := range "?1jA" {
		m = updateRune(t, m, r)
	}
	view = m.View()
	if m.help || m.note != "?1jA" || !strings.Contains(view, "?1jA") || strings.Contains(view, "ship it?") {
		t.Fatalf("compose note %q help %v\n%s", m.note, m.help, view)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer != nil {
		t.Fatalf("focused keys answered %+v", got.Answer)
	}
	m = updateRune(t, m, '\x1b')
	if m.composing || m.note != "?1jA" {
		t.Fatalf("esc composing %v note %q", m.composing, m.note)
	}
	m = updateRune(t, m, '?')
	if !m.help {
		t.Fatal("overlay stayed closed")
	}
	m = updateRune(t, m, '?')
	m = updateRune(t, m, '\r')
	view = m.View()
	if strings.Count(view, "┌") != 1 || !betweenBox(view, item.ID) || betweenBox(view, "compose:") {
		t.Fatalf("detail border =\n%s", view)
	}
	m = updateRune(t, m, '0')
	for _, r := range "H1?" {
		m = updateRune(t, m, r)
	}
	view = m.View()
	if !m.answerFocus || m.draft != "H1?" || m.help || strings.Count(view, "┌") != 1 || !betweenBox(view, "answer:") || betweenBox(view, item.ID) {
		t.Fatalf("answer note %q focus %v help %v\n%s", m.draft, m.answerFocus, m.help, view)
	}
	got, err = led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer != nil {
		t.Fatalf("answer field keys answered %+v", got.Answer)
	}
}

func TestGmailLayoutNarrowAndWide(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	deadline := time.Date(2026, 12, 1, 15, 4, 5, 0, time.UTC)
	item := mustCreate(t, led, &decide.Item{
		Question:  "go https://ex.test/a",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Unread:    true,
		FreeText:  true,
		Deadline:  &deadline,
		CreatedAt: time.Date(2026, 11, 4, 0, 0, 0, 0, time.UTC),
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	mustCreate(t, led, &decide.Item{
		Question: "done already",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateDone,
	})
	m := New(dir)
	m.Now = func() time.Time { return time.Date(2026, 11, 5, 0, 0, 0, 0, time.UTC) }
	m.Targets = []Target{{Name: "alpha", Pane: "pane-1"}, {Name: "beta", Pane: "pane-2"}}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if !lineBold(m.View(), item.ID) {
		t.Fatalf("unread is not bold\n%s", m.View())
	}
	for _, width := range []int{70, 120} {
		m.width = width
		m.height = 24
		view := m.View()
		for _, needle := range []string{
			"[Inbox]", "Answered", "Done", "Withdrawn",
			item.ID, "go https://ex.test/a", "ada", "1d",
			"due " + deadline.Format(time.RFC3339),
			"? help",
		} {
			if !strings.Contains(view, needle) {
				t.Fatalf("width %d missing %q\n%s", width, needle, view)
			}
		}
		if strings.Contains(view, "to:") || strings.Contains(view, "compose:") {
			t.Fatalf("width %d list shows compose\n%s", width, view)
		}
		for _, line := range strings.Split(strings.TrimSuffix(view, "\n"), "\n") {
			if got := utf8.RuneCountInString(visibleLine(line)); got > width {
				t.Fatalf("width %d line is %d\n%s", width, got, line)
			}
		}
		if width < narrowWidth && screenRow(view, "[Inbox]") == screenRow(view, item.ID) {
			t.Fatalf("narrow sidebar overlaps the row\n%s", view)
		}
		if width >= narrowWidth && !sidebarBesideList(view, item.ID) {
			t.Fatalf("wide sidebar is not beside the row\n%s", view)
		}
	}
	press(t, m, "o")
	opened := m.View()
	if strings.Contains(opened, "[Inbox]") || strings.Contains(opened, "done already") || !strings.Contains(opened, "1 a Alpha") {
		t.Fatalf("opened view =\n%s", opened)
	}
	press(t, m, "u")

	m.width = 0
	m.height = 0
	var sent []string
	m.runSend = func(argv []string) error {
		sent = append([]string(nil), argv...)
		return nil
	}
	press(t, m, "c\thello")
	row := screenRow(m.View(), "[Send]")
	line := strings.Split(m.View(), "\n")[row-1]
	if err := m.Click(visibleCol(line, "[Send]"), row); err != nil {
		t.Fatal(err)
	}
	if m.status != "sent to alpha" || len(sent) == 0 || sent[len(sent)-1] != "hello" {
		t.Fatalf("send status %q argv %#v", m.status, sent)
	}
}

func TestWindowSizeFitsEveryPanel(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	question := strings.Repeat("Q", 180)
	owner := strings.Repeat("O", 80)
	item := mustCreate(t, led, &decide.Item{
		Question: question,
		Owner:    owner,
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "alpha", Pane: "pane-1"}}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	next, _ := m.Update(tea.WindowSizeMsg{Width: 60, Height: 24})
	resized, ok := next.(*Model)
	if !ok || resized != m {
		t.Fatalf("resize model %T", next)
	}
	view := m.View()
	inboxRow := screenRow(view, "[Inbox]")
	for _, name := range []string{"Answered", "Done", "Withdrawn"} {
		if screenRow(view, name) != inboxRow {
			t.Fatalf("folder %s left the tab row\n%s", name, view)
		}
	}
	if screenRow(view, item.ID) <= inboxRow {
		t.Fatalf("list is not under the tabs\n%s", view)
	}
	if !lineCuts(view, "QQQQ") || !lineCuts(view, "OOOO") {
		t.Fatalf("long fields were not cut\n%s", view)
	}

	m.Update(tea.WindowSizeMsg{Width: 120, Height: 24})
	view = m.View()
	if !sidebarBesideList(view, item.ID) {
		t.Fatalf("wide sidebar is not beside the list\n%s", view)
	}
	if screenRow(view, "Answered") == screenRow(view, "[Inbox]") {
		t.Fatalf("wide folders share one row\n%s", view)
	}

	for _, width := range []int{60, 80, 120, 200} {
		m.Update(tea.WindowSizeMsg{Width: width, Height: 24})
		view = m.View()
		lines := strings.Split(view, "\n")
		if strings.HasSuffix(view, "\n") || len(lines) != 24 {
			t.Fatalf("width %d height %d\n%s", width, len(lines), view)
		}
		for i, line := range lines {
			if got := lipgloss.Width(line); got > width {
				t.Fatalf("width %d line %d display width %d\n%s", width, i, got, line)
			}
		}
		if got, last := lipgloss.Width(lines[0]), lipgloss.Width(lines[len(lines)-1]); got != width || last != width {
			t.Fatalf("width %d border widths %d %d", width, got, last)
		}
		if !strings.Contains(lines[0], "┌") || !strings.Contains(lines[len(lines)-1], "└") {
			t.Fatalf("width %d missing outer border", width)
		}
		if width < narrowWidth {
			if screenRow(view, "Answered") != screenRow(view, "[Inbox]") {
				t.Fatalf("width %d folders are not one row\n%s", width, view)
			}
			continue
		}
		if !sidebarBesideList(view, item.ID) {
			t.Fatalf("width %d sidebar is not beside the list\n%s", width, view)
		}
	}
}

func TestViewHeightKeepsTopBorder(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	for _, size := range []tea.WindowSizeMsg{
		{Width: 60, Height: 20},
		{Width: 80, Height: 24},
		{Width: 120, Height: 40},
	} {
		m.Update(size)
		for _, help := range []bool{false, true} {
			m.help = help
			view := m.View()
			if strings.HasSuffix(view, "\n") {
				t.Fatalf("%dx%d help %v ends with a newline", size.Width, size.Height, help)
			}
			lines := strings.Split(view, "\n")
			if len(lines) != size.Height {
				t.Fatalf("%dx%d help %v lines %d", size.Width, size.Height, help, len(lines))
			}
			for i, line := range lines {
				if got := lipgloss.Width(line); got > size.Width {
					t.Fatalf("%dx%d line %d width %d", size.Width, size.Height, i, got)
				}
			}
			if !strings.Contains(lines[0], "┌") || !strings.Contains(lines[len(lines)-1], "└") {
				t.Fatalf("%dx%d borders\n%s", size.Width, size.Height, view)
			}
			statusAt := -1
			for i, line := range lines {
				if strings.Contains(visibleLine(line), "gi/ga") {
					statusAt = i
				}
			}
			if statusAt <= 0 || statusAt >= len(lines)-1 {
				t.Fatalf("%dx%d help %v status row %d\n%s", size.Width, size.Height, help, statusAt, view)
			}
		}
	}

	m.help = false
	m.Update(tea.WindowSizeMsg{Width: 80, Height: 1})
	short := m.View()
	if strings.HasSuffix(short, "\n") || len(strings.Split(short, "\n")) != 1 {
		t.Fatalf("height 1 view %q", short)
	}
	m.Update(tea.WindowSizeMsg{Width: 80, Height: 2})
	borders := strings.Split(m.View(), "\n")
	if strings.HasSuffix(m.View(), "\n") || len(borders) != 2 || !strings.Contains(borders[0], "┌") || !strings.Contains(borders[1], "└") {
		t.Fatalf("height 2 view\n%s", m.View())
	}
	m.width = 0
	m.height = 0
	plain := m.View()
	if strings.HasSuffix(plain, "\n") || !strings.Contains(plain, "ship it?") {
		t.Fatalf("unmeasured view\n%s", plain)
	}
}

func TestOpenItemFullScreenAndCompose(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	first := mustCreate(t, led, &decide.Item{
		Question:  "open-first",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	second := mustCreate(t, led, &decide.Item{
		Question:  "open-second",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	third := mustCreate(t, led, &decide.Item{
		Question:  "open-third",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 3, 0, 0, 0, 0, time.UTC),
	})
	answered := mustCreate(t, led, &decide.Item{
		Question: "answered-row", Owner: "ada", Kind: decide.KindRelay, State: decide.StateAnswered,
	})
	done := mustCreate(t, led, &decide.Item{
		Question: "done-row", Owner: "ada", Kind: decide.KindTypeIn, State: decide.StateDone, TypeText: "x", Where: "y",
	})
	withdrawn := mustCreate(t, led, &decide.Item{
		Question: "withdrawn-row", Owner: "ada", Kind: decide.KindRelay, State: decide.StateWithdrawn,
	})
	m := New(dir)
	m.Targets = []Target{{Name: "alpha", Pane: "pane-a"}, {Name: "beta", Pane: "pane-b"}}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(m.View(), "to:") || strings.Contains(m.View(), "alpha") {
		t.Fatalf("list shows a recipient\n%s", m.View())
	}

	next, cmd := m.Update(runeKey('g'))
	got, ok := next.(*Model)
	if cmd == nil || !ok || got != m || !m.chord || !strings.Contains(m.View(), "g…") {
		t.Fatalf("chord %v cmd %v\n%s", m.chord, cmd != nil, m.View())
	}
	quit, err := m.Key('q')
	if err != nil || quit || m.chord || m.folder != 0 || m.cursor != 0 {
		t.Fatalf("unknown key ran quit=%v chord=%v folder=%d cursor=%d", quit, m.chord, m.folder, m.cursor)
	}
	if _, err := m.Key('g'); err != nil {
		t.Fatal(err)
	}
	m.Update(chordTimeoutMsg(m.chordSeq - 1))
	if !m.chord {
		t.Fatal("stale timeout canceled the chord")
	}
	m.Update(chordTimeoutMsg(m.chordSeq))
	if m.chord || m.folder != 0 || strings.Contains(m.View(), "g…") {
		t.Fatalf("timeout folder %d chord %v\n%s", m.folder, m.chord, m.View())
	}
	if _, err := m.Key('g'); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('\x1b'); err != nil || m.chord || m.folder != 0 {
		t.Fatalf("esc chord %v folder %d", m.chord, m.folder)
	}
	if _, err := m.Key('h'); err != nil || m.folder != 0 {
		t.Fatal(err)
	}
	m.Update(runeKey('g'))
	m.Update(tea.KeyMsg{Type: tea.KeyDown})
	if m.chord || m.folder != 0 || m.cursor != 0 {
		t.Fatalf("arrow chord %v folder %d cursor %d", m.chord, m.folder, m.cursor)
	}
	m.Update(runeKey('g'))
	m.Update(tea.KeyMsg{Type: tea.KeyShiftTab})
	if m.chord || m.folder != 0 {
		t.Fatalf("shift tab chord %v folder %d", m.chord, m.folder)
	}
	if _, err := m.Key('l'); err != nil || m.folder != 0 {
		t.Fatalf("l switched to %d", m.folder)
	}

	m.width = 120
	m.height = 24
	view := m.View()
	row := screenRow(view, "Answered")
	line := strings.Split(view, "\n")[row-1]
	if err := m.Click(visibleCol(line, "Answered"), row); err != nil || m.folder != 1 || !strings.Contains(m.View(), answered.ID) {
		t.Fatalf("click folder %d\n%s", m.folder, m.View())
	}
	press(t, m, "gi")
	if m.folder != 0 || !strings.Contains(m.View(), third.ID) {
		t.Fatalf("gi folder %d\n%s", m.folder, m.View())
	}
	m.width = 70
	view = m.View()
	row = screenRow(view, "Done")
	line = strings.Split(view, "\n")[row-1]
	if err := m.Click(visibleCol(line, "Done"), row); err != nil || m.folder != 2 || !strings.Contains(m.View(), done.ID) {
		t.Fatalf("tab click folder %d\n%s", m.folder, m.View())
	}
	press(t, m, "ga")
	if m.folder != 1 || !strings.Contains(m.View(), answered.ID) {
		t.Fatalf("ga\n%s", m.View())
	}
	press(t, m, "gw")
	if m.folder != 3 || !strings.Contains(m.View(), withdrawn.ID) {
		t.Fatalf("gw\n%s", m.View())
	}
	press(t, m, "gd")
	if m.folder != 2 || !strings.Contains(m.View(), done.ID) {
		t.Fatalf("gd\n%s", m.View())
	}
	press(t, m, "gi")

	m.width = 100
	m.height = 20
	press(t, m, "o")
	opened := m.View()
	if m.mode == modeList || strings.Contains(opened, "[Inbox]") || strings.Contains(opened, "open-second") || !strings.Contains(opened, "open-third") {
		t.Fatalf("o view mode %v\n%s", m.mode, opened)
	}
	press(t, m, "u")
	if m.mode != modeList || m.cursor != 0 || !strings.Contains(m.View(), "[Inbox]") {
		t.Fatalf("u cursor %d mode %v\n%s", m.cursor, m.mode, m.View())
	}
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	if m.mode == modeList || !strings.Contains(m.View(), "open-third") || strings.Contains(m.View(), "[Inbox]") {
		t.Fatalf("enter view\n%s", m.View())
	}
	press(t, m, "j")
	if m.mode == modeList || m.cursor != 1 || !strings.Contains(m.View(), "open-second") || strings.Contains(m.View(), "open-third") {
		t.Fatalf("j cursor %d\n%s", m.cursor, m.View())
	}
	press(t, m, "j")
	if m.cursor != 2 || !strings.Contains(m.View(), "open-first") {
		t.Fatalf("j end cursor %d\n%s", m.cursor, m.View())
	}
	press(t, m, "j")
	if m.cursor != 2 || !strings.Contains(m.View(), first.ID) {
		t.Fatalf("j wrapped cursor %d\n%s", m.cursor, m.View())
	}
	press(t, m, "kk")
	if m.cursor != 0 || !strings.Contains(m.View(), "open-third") {
		t.Fatalf("k cursor %d\n%s", m.cursor, m.View())
	}
	press(t, m, "k")
	if m.cursor != 0 {
		t.Fatalf("k wrapped to %d", m.cursor)
	}
	press(t, m, "j")
	press(t, m, "u")
	if m.mode != modeList || m.cursor != 1 || !rowSelected(m.View(), second.ID) {
		t.Fatalf("u restored cursor %d\n%s", m.cursor, m.View())
	}

	if _, err := m.Key('?'); err != nil {
		t.Fatal(err)
	}
	help := m.View()
	for _, needle := range []string{
		"g then i inbox",
		"o or enter opens",
		"u returns to the list",
		"tab moves recipient, text, [Send]",
		"enter on text inserts a newline",
		"enter on [Send] sends",
	} {
		if !strings.Contains(help, needle) {
			t.Fatalf("help missing %q\n%s", needle, help)
		}
	}
	for _, banned := range []string{"h/l", "Enter send", "]/[", "to:"} {
		if strings.Contains(help, banned) {
			t.Fatalf("help contains %q\n%s", banned, help)
		}
	}
	if _, err := m.Key('\x1b'); err != nil || m.help {
		t.Fatal("help stayed open")
	}

	var sent [][]string
	m.runSend = func(argv []string) error {
		sent = append(sent, append([]string(nil), argv...))
		return nil
	}
	press(t, m, "c")
	if m.composeFocus != focusRecipient || m.noteField.Focused() {
		t.Fatalf("compose focus %d field %v", m.composeFocus, m.noteField.Focused())
	}
	view = m.View()
	start := screenRow(view, "> [alpha]")
	end := screenRow(view, "[Send]")
	if start == 0 || end-start+1 < m.height/2 {
		t.Fatalf("compose rows %d-%d\n%s", start, end, view)
	}
	lines := strings.Split(strings.TrimSuffix(view, "\n"), "\n")
	if len(lines) != m.height {
		t.Fatalf("frame height %d", len(lines))
	}
	vis := visibleLine(lines[start-1])
	if lipgloss.Width(vis) != m.width || !strings.HasPrefix(vis, "│") || !strings.HasSuffix(vis, "│") {
		t.Fatalf("compose width %d\n%s", lipgloss.Width(vis), vis)
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(vis, "│"), "│")
	if lipgloss.Width(inner) != m.width-2 || !strings.Contains(inner, "> [alpha]") {
		t.Fatalf("inner width %d\n%s", lipgloss.Width(inner), inner)
	}
	m.Update(tea.KeyMsg{Type: tea.KeyDown})
	m.Update(tea.KeyMsg{Type: tea.KeyDown})
	if m.target != 1 {
		t.Fatalf("down wrapped to %d", m.target)
	}
	m.Update(tea.KeyMsg{Type: tea.KeyUp})
	if m.target != 0 {
		t.Fatalf("up target %d", m.target)
	}
	m.Update(tea.KeyMsg{Type: tea.KeyUp})
	if m.target != 0 {
		t.Fatalf("up wrapped to %d", m.target)
	}
	press(t, m, "][")
	if m.target != 0 || m.note != "" {
		t.Fatalf("brackets target %d note %q", m.target, m.note)
	}
	if _, err := m.Key('\r'); err != nil || len(sent) != 0 || !m.composing {
		t.Fatalf("picker enter sent %d composing %v", len(sent), m.composing)
	}
	press(t, m, "j\tone")
	if m.composeFocus != focusText || m.target != 1 || m.note != "one" {
		t.Fatalf("text focus %d target %d note %q", m.composeFocus, m.target, m.note)
	}
	if _, err := m.Key('\r'); err != nil || len(sent) != 0 || m.note != "one\n" || !m.composing {
		t.Fatalf("newline note %q sent %d", m.note, len(sent))
	}
	press(t, m, "two")
	if _, err := m.Key('\x1b'); err != nil || m.composing || m.note != "one\ntwo" || m.target != 1 {
		t.Fatalf("esc note %q target %d composing %v", m.note, m.target, m.composing)
	}
	press(t, m, "c")
	if m.composeFocus != focusRecipient || m.target != 1 || m.note != "one\ntwo" || !strings.Contains(m.View(), "> [beta]") || !strings.Contains(m.View(), "one") {
		t.Fatalf("restored focus %d target %d note %q\n%s", m.composeFocus, m.target, m.note, m.View())
	}
	press(t, m, "\t\t\r")
	want := sendArgv("", "pane-b", "one\ntwo")
	if m.status != "sent to beta" || len(sent) != 1 || strings.Join(sent[0], "\x00") != strings.Join(want, "\x00") {
		t.Fatalf("sent %#v want %#v status %q", sent, want, m.status)
	}

	press(t, m, "ck\tkeep")
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	press(t, m, "me")
	if m.note != "keep\nme" || m.target != 0 {
		t.Fatalf("click draft %q target %d", m.note, m.target)
	}
	row = screenRow(m.View(), "[Send]")
	line = strings.Split(m.View(), "\n")[row-1]
	if err := m.Click(visibleCol(line, "[Send]"), row); err != nil {
		t.Fatal(err)
	}
	want = sendArgv("", "pane-a", "keep\nme")
	if len(sent) != 2 || strings.Join(sent[1], "\x00") != strings.Join(want, "\x00") {
		t.Fatalf("click sent %#v want %#v", sent, want)
	}
}

func rowSelected(view, id string) bool {
	for _, line := range strings.Split(view, "\n") {
		if strings.Contains(visibleLine(line), "> "+id) {
			return true
		}
	}
	return false
}

func sidebarBesideList(view, id string) bool {
	inbox := screenRow(view, "[Inbox]")
	item := screenRow(view, id)
	return inbox > 0 && item >= inbox && item <= inbox+2
}

func lineCuts(view, prefix string) bool {
	for _, line := range strings.Split(view, "\n") {
		vis := visibleLine(line)
		if strings.Contains(vis, prefix) && strings.Contains(vis, "…") {
			return true
		}
	}
	return false
}

func betweenBox(view, needle string) bool {
	start := strings.Index(view, "┌")
	end := strings.Index(view, "└")
	if start < 0 || end < start {
		return false
	}
	return strings.Contains(view[start:end], needle)
}

func updateMouseAt(t *testing.T, m *Model, x, y int, button tea.MouseButton) *Model {
	t.Helper()
	next, _ := m.Update(tea.MouseMsg{
		X:      x - 1,
		Y:      y - 1,
		Button: button,
		Action: tea.MouseActionPress,
	})
	got, ok := next.(*Model)
	if !ok {
		t.Fatalf("update type %T", next)
	}
	return got
}

func visibleCol(line, needle string) int {
	vis := visibleLine(line)
	i := strings.Index(vis, needle)
	if i < 0 {
		return 0
	}
	return utf8.RuneCountInString(vis[:i]) + 1
}

func updateRune(t *testing.T, m *Model, r rune) *Model {
	t.Helper()
	next, _ := m.Update(runeKey(r))
	got, ok := next.(*Model)
	if !ok {
		t.Fatalf("update type %T", next)
	}
	return got
}

func screenRow(view, needle string) int {
	for i, line := range strings.Split(view, "\n") {
		if strings.Contains(line, needle) {
			return i + 1
		}
	}
	return 0
}

func press(t *testing.T, m *Model, keys string) {
	t.Helper()
	for _, r := range keys {
		if _, err := m.Key(r); err != nil {
			t.Fatalf("key %q: %v", r, err)
		}
	}
}

func visibleLine(s string) string {
	var b strings.Builder
	for i := 0; i < len(s); {
		if s[i] == 0x1b && i+1 < len(s) && s[i+1] == '[' {
			i += 2
			for i < len(s) && s[i] != 'm' {
				i++
			}
			if i < len(s) {
				i++
			}
			continue
		}
		b.WriteByte(s[i])
		i++
	}
	return b.String()
}

func lineBold(view, id string) bool {
	for _, line := range strings.Split(view, "\n") {
		if strings.Contains(line, id) {
			return strings.Contains(line, boldOn) && strings.Contains(line, boldOff)
		}
	}
	return false
}

func selectedLine(view string) string {
	for _, line := range strings.Split(view, "\n") {
		plain := strings.TrimPrefix(visibleLine(line), "│")
		if strings.HasPrefix(plain, "> ") || strings.Contains(line, boldOn+"> ") {
			return plain
		}
	}
	return ""
}

type lockedBuf struct {
	mu sync.Mutex
	b  bytes.Buffer
}

func (l *lockedBuf) Write(p []byte) (int, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	n, err := l.b.Write(p)
	if err != nil {
		return n, fmt.Errorf("buffer decide inbox output: %w", err)
	}
	return n, nil
}

func (l *lockedBuf) String() string {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.b.String()
}

func TestThreadViewShowsTheDraftAndReplies(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question:   "what should I send?",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		State:      decide.StateOpen,
		NotifyPane: "pane-9",
		Options: []decide.Option{
			{Key: "draft", Label: "draft a message for me", Recommended: true},
			{Key: "later", Label: "Later"},
		},
	})
	var words strings.Builder
	for i := 0; i < 40; i++ {
		fmt.Fprintf(&words, "draftword%d ", i)
	}
	draft := words.String() + "draftend"
	replied, err := led.Reply(item.ID, draft, []decide.Option{
		{Key: "send", Label: "send"},
		{Key: "edit", Label: "edit"},
		{Key: "don't send", Label: "don't send"},
	}, false)
	if err != nil {
		t.Fatal(err)
	}
	note, err := led.Create(&decide.Item{
		Question: "progress note",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Unread:   true,
	})
	if err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.width = 36
	m.height = 12
	var sent []string
	m.runSend = func(argv []string) error {
		sent = append([]string(nil), argv...)
		return nil
	}
	m.SendTemplate = "sentinel {pane}"
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	list := m.View()
	if !strings.Contains(list, "draftword0") || strings.Contains(list, "what should I send?") {
		t.Fatalf("list row is not the latest message\n%s", list)
	}
	if !strings.Contains(list, "*") || !strings.Contains(list, note.ID) {
		t.Fatalf("notification has no unread marker\n%s", list)
	}
	if strings.Contains(lineContaining(list, replied.ID), "*") {
		t.Fatalf("a thread that needs an answer was marked as a notification\n%s", list)
	}
	if _, err := led.Done(note.ID, "read"); err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	press(t, m, "o")
	opened := m.View()
	if !strings.Contains(opened, "draftword0") || strings.Contains(opened, "draftend") {
		t.Fatalf("draft did not start wrapped above the fold\n%s", opened)
	}
	if strings.Contains(visibleLine(lineContaining(opened, "draftword0")), "…") {
		t.Fatalf("draft was clipped\n%s", opened)
	}
	for m.scroll < 80 && (!strings.Contains(m.View(), "draftend") || !strings.Contains(m.View(), "3 don't send")) {
		m.Wheel(true)
	}
	bottom := m.View()
	if !strings.Contains(bottom, "draftend") || !strings.Contains(bottom, "1 send") || !strings.Contains(bottom, "3 don't send") {
		t.Fatalf("options are not at the bottom\n%s", bottom)
	}
	if screenRow(bottom, "1 send") < screenRow(bottom, "draftend") {
		t.Fatalf("options are above the draft\n%s", bottom)
	}
	if strings.Contains(bottom, "draftword0") {
		t.Fatalf("scroll left the draft top in view\n%s", bottom)
	}

	press(t, m, "1")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "send" {
		t.Fatalf("answer = %+v state %s", got.Answer, got.State)
	}

	// The answered thread left the inbox. Reopen a fresh draft to reply with edits.
	again, err := led.Create(&decide.Item{
		Question:   "what should I send?",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		State:      decide.StateOpen,
		NotifyPane: "pane-9",
		Options:    []decide.Option{{Key: "draft", Label: "draft a message for me"}, {Key: "later", Label: "Later"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Reply(again.ID, draft, []decide.Option{
		{Key: "send", Label: "send"},
		{Key: "edit", Label: "edit"},
		{Key: "don't send", Label: "don't send"},
	}, false); err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "oredits\r")
	edited, err := led.Get(again.ID)
	if err != nil {
		t.Fatal(err)
	}
	last := edited.Messages[len(edited.Messages)-1]
	if edited.State != decide.StateOpen || last.Author != "human" || last.Text != "edits" {
		t.Fatalf("reply = state %s message %+v", edited.State, last)
	}
	if len(edited.Options) != 3 || edited.Options[0].Key != "send" {
		t.Fatalf("reply cleared the options: %+v", edited.Options)
	}
	if len(sent) != 3 || sent[len(sent)-1] != again.ID+"\n"+"edits" {
		t.Fatalf("notify argv = %#v", sent)
	}
	if !strings.Contains(m.View(), "edits") {
		t.Fatalf("reply is not in the thread view\n%s", m.View())
	}
}

func TestYArchivesANotificationNotAQuestion(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "what should I send?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "draft", Label: "draft a message for me"}},
	})
	if _, err := led.Reply(item.ID, "full draft", []decide.Option{
		{Key: "send", Label: "send"},
		{Key: "edit", Label: "edit"},
		{Key: "don't send", Label: "don't send"},
	}, false); err != nil {
		t.Fatal(err)
	}
	note, err := led.Create(&decide.Item{
		Question: "progress note",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Unread:   true,
	})
	if err != nil {
		t.Fatal(err)
	}
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if len(m.items) < 2 || m.items[0].ID != note.ID || m.items[1].ID != item.ID {
		t.Fatalf("inbox order = %+v", m.items)
	}
	press(t, m, "jy")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || len(got.Options) != 3 || got.Options[0].Key != "send" {
		t.Fatalf("y archived a question: state %s options %+v", got.State, got.Options)
	}
	press(t, m, "oy")
	got, err = led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen {
		t.Fatalf("y archived an open question: %s", got.State)
	}
	press(t, m, "uky")
	got, err = led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen {
		t.Fatalf("question changed after leaving it: %s", got.State)
	}
	archived, err := led.Get(note.ID)
	if err != nil {
		t.Fatal(err)
	}
	if archived.State != decide.StateDone || archived.Outcome != "archived" {
		t.Fatalf("notification = state %s outcome %q", archived.State, archived.Outcome)
	}
}

func TestOpenItemWrapsQuestionOptionsAndLinks(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	var words strings.Builder
	for i := 0; i < 40; i++ {
		fmt.Fprintf(&words, "word%d ", i)
	}
	question := words.String()
	label := "this option label is long enough to wrap inside the frame"
	link := "https://example.com/" + strings.Repeat("p", 40) + "/tailtoken"
	mustCreate(t, led, &decide.Item{
		Question:  question,
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		Options:   []decide.Option{{Key: "a", Label: label}},
		Links:     []string{link},
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 60
	m.height = 20
	list := m.View()
	if strings.Count(list, "\n")+1 != 20 {
		t.Fatalf("list lines %d", strings.Count(list, "\n")+1)
	}
	if !strings.Contains(list, "…") {
		t.Fatalf("list did not ellipsize the question\n%s", list)
	}

	m.height = 14
	press(t, m, "o")
	view := m.View()
	assertFrame(t, view, 60, 14)
	if !strings.Contains(view, "word0") || strings.Contains(view, "tailtoken") {
		t.Fatalf("open item did not start at the question\n%s", view)
	}
	if strings.Contains(visibleLine(lineContaining(view, "word0")), "…") {
		t.Fatalf("question was clipped\n%s", view)
	}
	word0 := screenRow(view, "word0")
	word12 := screenRow(view, "word12")
	if word12 == 0 || word12 <= word0 {
		t.Fatalf("word12 row %d word0 row %d\n%s", word12, word0, view)
	}
	if screenRow(view, "this option") == 0 || screenRow(view, "frame") == 0 {
		t.Fatalf("option label did not wrap\n%s", view)
	}
	if screenRow(view, "this option") == screenRow(view, "frame") {
		t.Fatalf("option label stayed on one line\n%s", view)
	}

	for m.scroll < 40 && strings.Contains(m.View(), "word0") {
		m.Wheel(true)
	}
	scrolled := m.View()
	assertFrame(t, scrolled, 60, 14)
	if !strings.Contains(scrolled, "tailtoken") {
		t.Fatalf("scroll did not reveal the link\n%s", scrolled)
	}
	if strings.Contains(scrolled, "word0") {
		t.Fatalf("scroll left the question top in view\n%s", scrolled)
	}
	row := screenRow(scrolled, "tailtoken")
	line := strings.Split(scrolled, "\n")[row-1]
	if err := m.Click(visibleCol(line, "tailtoken"), row); err != nil {
		t.Fatal(err)
	}
	if m.status != link {
		t.Fatalf("link status %q", m.status)
	}
}

func TestFocusedTextShowsACursor(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "cursor item",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		FreeText:  true,
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.Targets = []Target{{Name: "alpha", Pane: "p"}}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 80
	m.height = 24
	press(t, m, "c")
	if strings.Contains(draftArea(m.View()), cursorOn) {
		t.Fatalf("recipient focus drew a text cursor\n%s", m.View())
	}
	press(t, m, "\t")
	if !strings.Contains(draftArea(m.View()), cursorOn) {
		t.Fatalf("empty message has no cursor\n%s", m.View())
	}
	press(t, m, "Hi")
	draft := draftArea(m.View())
	if !strings.Contains(visibleLine(draft), "Hi") || !strings.Contains(draft, cursorOn) {
		t.Fatalf("cursor not after Hi\n%s", m.View())
	}
	assertFrame(t, m.View(), 80, 24)

	press(t, m, "\t")
	if strings.Contains(draftArea(m.View()), cursorOn) {
		t.Fatalf("send focus kept the text cursor\n%s", m.View())
	}

	press(t, m, "\x1b")
	press(t, m, "o0")
	if !strings.Contains(m.View(), cursorOn) || strings.Contains(m.View(), "answer: _") {
		t.Fatalf("free text cursor\n%s", m.View())
	}
	press(t, m, "Z")
	if !strings.Contains(visibleLine(lineContaining(m.View(), "answer:")), "Z") {
		t.Fatalf("free text\n%s", m.View())
	}

	press(t, m, "\x1be")
	if m.mode != modeOutcome || !strings.Contains(m.View(), cursorOn) {
		t.Fatalf("outcome cursor mode %v\n%s", m.mode, m.View())
	}
	press(t, m, "Ok")
	if !strings.Contains(visibleLine(lineContaining(m.View(), "outcome:")), "Ok") {
		t.Fatalf("outcome text\n%s", m.View())
	}
}

func TestComposeBoxesFollowFocusAndFit(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "box item",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.Targets = []Target{{Name: "alpha", Pane: "p"}, {Name: "beta", Pane: "q"}}
	m.runSend = func([]string) error { return nil }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	for _, size := range [][2]int{{60, 20}, {80, 24}, {120, 40}} {
		m.width = size[0]
		m.height = size[1]
		press(t, m, "c")
		view := m.View()
		assertFrame(t, view, size[0], size[1])
		tops := borderRows(view, "┌")
		if len(tops) < 4 {
			t.Fatalf("size %v borders %d\n%s", size, len(tops), view)
		}
		if !borderHighlighted(strings.Split(view, "\n")[tops[1]]) {
			t.Fatalf("size %v recipient box not highlighted\n%s", size, view)
		}
		press(t, m, "\t")
		view = m.View()
		tops = borderRows(view, "┌")
		if len(tops) < 4 || !borderHighlighted(strings.Split(view, "\n")[tops[2]]) {
			t.Fatalf("size %v text box not highlighted\n%s", size, view)
		}
		press(t, m, "Hi\r")
		if !strings.Contains(m.note, "\n") {
			t.Fatalf("enter did not insert a newline %q", m.note)
		}
		press(t, m, "\t")
		view = m.View()
		tops = borderRows(view, "┌")
		if len(tops) < 4 || !borderHighlighted(strings.Split(view, "\n")[tops[3]]) {
			t.Fatalf("size %v send box not highlighted\n%s", size, view)
		}
		if borderHighlighted(strings.Split(view, "\n")[tops[1]]) {
			t.Fatalf("size %v recipient stayed highlighted\n%s", size, view)
		}
		next, _ := m.Update(tea.KeyMsg{Type: tea.KeyShiftTab})
		m = next.(*Model)
		if m.composeFocus != focusText {
			t.Fatalf("shift tab focus %d", m.composeFocus)
		}
		press(t, m, "\t\r")
		if m.status != "sent to alpha" {
			t.Fatalf("send status %q", m.status)
		}
	}
}

func assertFrame(t *testing.T, view string, width, height int) {
	t.Helper()
	if strings.HasSuffix(view, "\n") {
		t.Fatalf("trailing newline")
	}
	lines := strings.Split(view, "\n")
	if len(lines) != height {
		t.Fatalf("lines %d want %d\n%s", len(lines), height, view)
	}
	for i, line := range lines {
		if got := lipgloss.Width(line); got > width {
			t.Fatalf("line %d width %d\n%s", i, got, line)
		}
	}
	if !strings.Contains(lines[0], "┌") || !strings.Contains(lines[len(lines)-1], "└") {
		t.Fatalf("outer border\n%s", view)
	}
}

func lineContaining(view, needle string) string {
	for _, line := range strings.Split(view, "\n") {
		if strings.Contains(line, needle) {
			return line
		}
	}
	return ""
}

func TestStatusBoardShowsAgentsResourcesAndDimsStale(t *testing.T) {
	dir := t.TempDir()
	if _, err := decide.Open(dir); err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 6, 12, 0, 0, 0, time.UTC)
	board, err := decide.OpenStatus(dir)
	if err != nil {
		t.Fatal(err)
	}
	board.SetClock(func() time.Time { return now })
	longText := strings.Repeat("x", 200)
	if _, err := board.SetAgent("alpha", "working", longText); err != nil {
		t.Fatal(err)
	}
	board.SetClock(func() time.Time { return now.Add(-10 * time.Minute) })
	if _, err := board.SetAgent("beta", "paused", "old task"); err != nil {
		t.Fatal(err)
	}
	board.SetClock(func() time.Time { return now })
	if _, err := board.SetResource("workspace", decide.ResourceFree, "paused"); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "gs")
	if m.mode != modeStatus {
		t.Fatalf("mode %d", m.mode)
	}
	for _, width := range []int{60, 80, 120, 200} {
		m.width = width
		m.height = 24
		view := m.View()
		assertFrame(t, view, width, 24)
		agentHeader := lineContaining(view, "text")
		alpha := lineContaining(view, "alpha")
		beta := lineContaining(view, "beta")
		resHeader := lineContaining(view, "free/busy")
		workspace := lineContaining(view, "workspace")
		if agentHeader == "" || alpha == "" || beta == "" || resHeader == "" || workspace == "" {
			t.Fatalf("width %d missing a status row\n%s", width, view)
		}
		stateCol := strings.Index(visibleLine(agentHeader), "state")
		textCol := strings.Index(visibleLine(agentHeader), "text")
		ageCol := strings.Index(visibleLine(agentHeader), " age ") + 1
		if stateCol < 1 || textCol <= stateCol || ageCol <= stateCol {
			t.Fatalf("width %d header columns state %d age %d text %d\n%s", width, stateCol, ageCol, textCol, visibleLine(agentHeader))
		}
		if strings.Index(visibleLine(alpha), "working") != stateCol || strings.Index(visibleLine(beta), "paused") != stateCol {
			t.Fatalf("width %d state column drifted\nalpha %s\nbeta %s", width, visibleLine(alpha), visibleLine(beta))
		}
		if strings.Index(visibleLine(alpha), "now") != ageCol || strings.Index(visibleLine(beta), "10m") != ageCol {
			t.Fatalf("width %d age column drifted\nalpha %s\nbeta %s", width, visibleLine(alpha), visibleLine(beta))
		}
		if strings.Index(visibleLine(alpha), "xxxx") != textCol {
			t.Fatalf("width %d text column drifted\n%s", width, visibleLine(alpha))
		}
		if !strings.Contains(visibleLine(alpha), "…") || strings.Contains(visibleLine(alpha), longText) {
			t.Fatalf("width %d long text left its column\n%s", width, visibleLine(alpha))
		}
		busyCol := strings.Index(visibleLine(resHeader), "free/busy")
		noteCol := strings.Index(visibleLine(resHeader), "note")
		if busyCol < 1 || noteCol <= busyCol {
			t.Fatalf("width %d resource header\n%s", width, visibleLine(resHeader))
		}
		if strings.Index(visibleLine(workspace), "free") != busyCol || strings.Index(visibleLine(workspace), "paused") != noteCol {
			t.Fatalf("width %d resource columns drifted\n%s", width, visibleLine(workspace))
		}
		if !strings.Contains(beta, "\x1b[2m") {
			t.Fatalf("width %d stale row was not dimmed\n%s", width, beta)
		}
		if strings.Contains(alpha, "\x1b[2m") || strings.Contains(workspace, "\x1b[2m") {
			t.Fatalf("width %d fresh row was dimmed\nalpha %s\nworkspace %s", width, alpha, workspace)
		}
	}

	if _, err := board.SetAgent("gamma", "idle", "just arrived"); err != nil {
		t.Fatal(err)
	}
	next, _ := m.Update(reloadMsg{})
	m = next.(*Model)
	if !strings.Contains(m.View(), "just arrived") {
		t.Fatalf("refresh missed the new row\n%s", m.View())
	}
}

func TestStatusComposeFromHighlightedAgent(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question: "stay listed", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen,
	})
	now := time.Date(2026, 11, 6, 12, 0, 0, 0, time.UTC)
	board, err := decide.OpenStatus(dir)
	if err != nil {
		t.Fatal(err)
	}
	board.SetClock(func() time.Time { return now })
	if _, err := board.SetAgent("alpha", "working", "fixing"); err != nil {
		t.Fatal(err)
	}
	if _, err := board.SetAgent("gamma", "idle", "other"); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return now }
	m.Targets = []Target{{Name: "alpha", Pane: "pane-a"}, {Name: "beta", Pane: "pane-b"}}
	m.target = 1
	m.width = 100
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "o open") {
		t.Fatalf("list footer dropped open\n%s", m.View())
	}
	press(t, m, "gs")
	view := m.View()
	if m.mode != modeStatus || strings.Contains(view, "o open") || !strings.Contains(view, "c  tab") || !strings.Contains(view, "? help") {
		t.Fatalf("status footer\n%s", view)
	}
	alpha := lineContaining(view, "alpha")
	gamma := lineContaining(view, "gamma")
	if alpha == "" || gamma == "" || !strings.Contains(alpha, "\x1b[7m") || strings.Contains(gamma, "\x1b[7m") {
		t.Fatalf("first agent is not highlighted\nalpha %s\ngamma %s", alpha, gamma)
	}
	press(t, m, "j")
	view = m.View()
	alpha = lineContaining(view, "alpha")
	gamma = lineContaining(view, "gamma")
	if strings.Contains(alpha, "\x1b[7m") || !strings.Contains(gamma, "\x1b[7m") {
		t.Fatalf("j did not move the highlight\nalpha %s\ngamma %s", alpha, gamma)
	}
	press(t, m, "j")
	gamma = lineContaining(m.View(), "gamma")
	if !strings.Contains(gamma, "\x1b[7m") {
		t.Fatalf("j wrapped past the last agent\n%s", gamma)
	}
	press(t, m, "k")
	alpha = lineContaining(m.View(), "alpha")
	if !strings.Contains(alpha, "\x1b[7m") {
		t.Fatalf("k did not move back\n%s", alpha)
	}
	press(t, m, "k")
	alpha = lineContaining(m.View(), "alpha")
	if !strings.Contains(alpha, "\x1b[7m") {
		t.Fatalf("k wrapped past the first agent\n%s", alpha)
	}
	press(t, m, "o")
	if m.mode != modeStatus || m.composing {
		t.Fatalf("o on status mode %d composing %v", m.mode, m.composing)
	}
	if _, err := m.Key('?'); err != nil {
		t.Fatal(err)
	}
	help := m.View()
	for _, needle := range []string{"c compose", "j/k move", "u returns to the list", "q quit"} {
		if !strings.Contains(help, needle) {
			t.Fatalf("status help missing %q\n%s", needle, help)
		}
	}
	for _, banned := range []string{"o or enter opens", "1-9 answer", "e done", "click folder, row, option"} {
		if strings.Contains(help, banned) {
			t.Fatalf("status help contains %q\n%s", banned, help)
		}
	}
	if _, err := m.Key('\x1b'); err != nil || m.help {
		t.Fatal("help stayed open")
	}
	press(t, m, "c")
	if !m.composing || m.composeFocus != focusRecipient || m.target != 0 || !strings.Contains(m.View(), "> [alpha]") || !strings.Contains(m.View(), "[Send]") {
		t.Fatalf("compose target %d focus %d\n%s", m.target, m.composeFocus, m.View())
	}
	if _, err := m.Key('\x1b'); err != nil || m.composing {
		t.Fatal("compose stayed open")
	}
	press(t, m, "j")
	m.target = 1
	press(t, m, "c")
	if !m.composing || m.target != 1 || !strings.Contains(m.View(), "> [beta]") || strings.Contains(m.View(), "> [alpha]") {
		t.Fatalf("unmatched agent changed recipient to %d\n%s", m.target, m.View())
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	press(t, m, "gi")
	if m.mode != modeList || !strings.Contains(m.View(), "o open") {
		t.Fatalf("list footer was not restored\n%s", m.View())
	}
	if _, err := m.Key('?'); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "o or enter opens") || !strings.Contains(m.View(), "1-9 answer") {
		t.Fatalf("list help changed\n%s", m.View())
	}
}

func draftArea(view string) string {
	lines := strings.Split(view, "\n")
	tops := borderRows(view, "┌")
	if len(tops) < 4 {
		return view
	}
	return strings.Join(lines[tops[2]:tops[3]], "\n")
}

func borderRows(view, mark string) []int {
	var rows []int
	for i, line := range strings.Split(view, "\n") {
		if strings.Contains(visibleLine(line), mark) {
			rows = append(rows, i)
		}
	}
	return rows
}

func borderHighlighted(line string) bool {
	return strings.Contains(line, boldOn) && strings.Contains(visibleLine(line), "┌")
}
