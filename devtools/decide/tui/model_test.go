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
		key  rune
		id   string
		name string
	}{
		{key: 'l', id: answered.ID, name: "[Answered]"},
		{key: 'l', id: done.ID, name: "[Done]"},
		{key: 'l', id: withdrawn.ID, name: "[Withdrawn]"},
	} {
		if _, err := m.Key(want.key); err != nil {
			t.Fatal(err)
		}
		view := m.View()
		if !strings.Contains(view, want.name) || !strings.Contains(view, want.id) {
			t.Fatalf("after %q missing %s %s\n%s", want.key, want.name, want.id, view)
		}
	}
	if _, err := m.Key('h'); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "[Done]") {
		t.Fatalf("h did not return to Done\n%s", m.View())
	}
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
	press(t, m, "custom\r")
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
	m.height = 14
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || strings.Contains(view, older.ID) {
		t.Fatalf("initial window = %s", view)
	}
	if !strings.Contains(view, "compose:") || !strings.Contains(view, "to: ▾ (none)") || !strings.Contains(view, "? help") {
		t.Fatalf("compose left the initial window\n%s", view)
	}
	m.Wheel(true)
	view = m.View()
	if strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || !strings.Contains(view, older.ID) {
		t.Fatalf("scrolled window = %s", view)
	}
	if !strings.Contains(view, "compose:") || !strings.Contains(view, "to: ▾ (none)") || !strings.Contains(view, "? help") {
		t.Fatalf("compose left the scrolled window\n%s", view)
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
	if got.Answer != nil || !strings.Contains(m.View(), "0  answer: _ Esc to cancel") {
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
	if !strings.Contains(m.View(), "answer: keep_") {
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
	if !strings.Contains(m.View(), "sent to inbox  ? help") || strings.Contains(m.View(), "pane-1") {
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
}

func TestComposeFailureKeepsText(t *testing.T) {
	dir := t.TempDir()
	m := New(dir)
	m.Targets = []Target{{Name: "inbox", Pane: "pane-1"}}
	text := `say "hi" $(uname)`
	m.runSend = func([]string) error { return fmt.Errorf("boom") }
	m.composing = true
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
	if m.note != "" || m.target != 0 || len(got) != 0 {
		t.Fatalf("typing note %q target %d sends %d", m.note, m.target, len(got))
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	press(t, m, "]")
	if !strings.Contains(m.View(), "to: ▾ alerts") {
		t.Fatalf("keyboard target\n%s", m.View())
	}
	if _, err := m.Key('c'); err != nil {
		t.Fatal(err)
	}
	press(t, m, " note\r")
	if len(got) != 1 || len(got[0]) != 5 || got[0][3] != "pane-2" || got[0][4] != " note" {
		t.Fatalf("keyboard send = %#v", got)
	}

	view := m.View()
	row := screenRow(view, "inbox")
	line := strings.Split(view, "\n")[row-1]
	x := strings.Index(line, "inbox")
	if x < 0 {
		t.Fatalf("target line %q", line)
	}
	if err := m.Click(utf8.RuneCountInString(line[:x])+1, row); err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || !strings.Contains(m.View(), "to: ▾ inbox") {
		t.Fatalf("click target\n%s", m.View())
	}
	if err := m.Click(1, screenRow(m.View(), "compose:")); err != nil {
		t.Fatal(err)
	}
	m.SendTemplate = "tool {pane}"
	press(t, m, "note-b\r")
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
	m.note = `say "hi"`
	m.ShiftEnter()
	if calls != 0 {
		t.Fatal("shift+enter sent")
	}
	if m.note != "say \"hi\"\n" {
		t.Fatalf("note = %q", m.note)
	}
	if !strings.Contains(m.View(), "compose › say \"hi\"\\n_") {
		t.Fatalf("view = %s", m.View())
	}
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
	if !strings.Contains(view, "compose:") || !strings.Contains(view, "to: ▾ alpha") {
		t.Fatalf("unfocused =\n%s", view)
	}
	if !strings.Contains(view, "[Inbox]") {
		t.Fatalf("folder =\n%s", view)
	}
	press(t, m, "xyz\t")
	if m.note != "" || m.target != 1 || !strings.Contains(m.View(), "[Inbox]") || strings.Contains(m.View(), "[Answered]") {
		t.Fatalf("tab typed or changed folder note %q target %d\n%s", m.note, m.target, m.View())
	}
	press(t, m, "[")
	if m.note != "" || m.target != 0 {
		t.Fatalf("previous target note %q target %d", m.note, m.target)
	}
	if _, err := m.Key('l'); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(m.View(), "[Answered]") {
		t.Fatalf("l did not change folder\n%s", m.View())
	}
	if _, err := m.Key('h'); err != nil {
		t.Fatal(err)
	}

	if err := m.Click(1, screenRow(m.View(), "compose:")); err != nil {
		t.Fatal(err)
	}
	focused := m.View()
	if !strings.Contains(focused, "compose › …_  Enter send · Esc cancel") {
		t.Fatalf("focused empty =\n%s", focused)
	}
	press(t, m, "hi")
	if _, err := m.Key('\t'); err != nil {
		t.Fatal(err)
	}
	if m.note != "hi" || m.target != 0 || !strings.Contains(m.View(), "compose › hi_") || strings.Contains(m.View(), "to: ▾ beta") {
		t.Fatalf("focused tab note %q target %d\n%s", m.note, m.target, m.View())
	}
	m.ShiftTab()
	if m.note != "hi" || m.target != 1 {
		t.Fatalf("focused cycle note %q target %d", m.note, m.target)
	}
	m.ShiftTab()
	if m.note != "hi" || m.target != 0 {
		t.Fatalf("focused cycle back note %q target %d", m.note, m.target)
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	if m.composing || m.note != "hi" || !strings.Contains(m.View(), "compose:") {
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
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	if m.status != "sent to alpha" || strings.Contains(m.View(), "pane-secret") {
		t.Fatalf("status %q\n%s", m.status, m.View())
	}
	if len(got) == 0 || got[len(got)-1] != "hi" {
		t.Fatalf("argv = %#v", got)
	}

	m.runSend = func([]string) error { return fmt.Errorf("boom") }
	press(t, m, "cagain\r")
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
	press(t, m, "\r")
	if err := m.Click(1, screenRow(m.View(), "compose:")); err != nil {
		t.Fatal(err)
	}
	press(t, m, "1")
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
	if !m.composing || !m.noteField.Focused() {
		t.Fatal("c did not focus the compose textarea")
	}
	press(t, m, "?")
	if m.help || m.note != "?" || !strings.Contains(m.View(), "compose › ?_") {
		t.Fatalf("focused ? note %q help %v\n%s", m.note, m.help, m.View())
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	if m.composing || m.note != "?" || !strings.Contains(m.View(), "compose:") {
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
	m.height = 14
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
	m = updateMouseAt(t, m, 1, screenRow(m.View(), "compose:"), tea.MouseButtonLeft)
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if !m.composing || !m.noteField.Focused() || m.folder != 0 || got.Answer != nil {
		t.Fatalf("compose click composing %v folder %d answer %+v", m.composing, m.folder, got.Answer)
	}
	targetRow := screenRow(m.View(), "beta")
	targetLine := strings.Split(m.View(), "\n")[targetRow-1]
	m = updateMouseAt(t, m, visibleCol(targetLine, "beta"), targetRow, tea.MouseButtonLeft)
	got, err = led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if m.target != 1 || !strings.Contains(m.View(), "to: ▾ beta") || m.folder != 0 || got.Answer != nil {
		t.Fatalf("target click target %d folder %d answer %+v\n%s", m.target, m.folder, got.Answer, m.View())
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
	answeredRow = screenRow(m.View(), "Answered")
	answeredLine = strings.Split(m.View(), "\n")[answeredRow-1]
	m = updateMouseAt(t, m, visibleCol(answeredLine, "Answered"), answeredRow, tea.MouseButtonLeft)
	if m.folder != 0 || !strings.Contains(m.View(), "ship it?") {
		t.Fatalf("focused folder click folder %d\n%s", m.folder, m.View())
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
	if !m.composing || !m.noteField.Focused() {
		t.Fatal("c did not focus the compose textarea")
	}
	m = updateRune(t, m, '\x1b')
	m = updateRune(t, m, '\r')
	row := screenRow(m.View(), "compose:")
	if err := m.Click(1, row); err != nil {
		t.Fatal(err)
	}
	if !m.composing || !m.noteField.Focused() {
		t.Fatal("click did not focus the compose textarea")
	}
	for _, r := range "A?1" {
		m = updateRune(t, m, r)
	}
	if m.note != "A?1" || m.noteField.Value() != "A?1" || m.help || m.folder != folder {
		t.Fatalf("note %q field %q help %v folder %d", m.note, m.noteField.Value(), m.help, m.folder)
	}
	if !strings.Contains(m.View(), "compose › A?1_  Enter send · Esc cancel") {
		t.Fatalf("focused line\n%s", m.View())
	}
	got, err := led.Get(relay.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("focused keys triggered an answer: %+v", got)
	}
	m = updateRune(t, m, ']')
	if m.note != "A?1]" || m.target != 0 || strings.Contains(m.View(), "to: ▾ beta") {
		t.Fatalf("bracket note %q target %d\n%s", m.note, m.target, m.View())
	}
	m = updateRune(t, m, '\x7f')
	if m.note != "A?1" {
		t.Fatalf("backspace note %q", m.note)
	}
	m = updateRune(t, m, '\t')
	if m.note != "A?1" || m.target != 0 || strings.Contains(m.View(), "to: ▾ beta") {
		t.Fatalf("tab while focused note %q target %d\n%s", m.note, m.target, m.View())
	}
	m.ShiftTab()
	if m.note != "A?1" || m.target != 1 || !strings.Contains(m.View(), "to: ▾ beta") {
		t.Fatalf("shift tab note %q target %d\n%s", m.note, m.target, m.View())
	}
	m.ShiftTab()
	if m.note != "A?1" || m.target != 0 || !strings.Contains(m.View(), "to: ▾ alpha") {
		t.Fatalf("shift tab back note %q target %d\n%s", m.note, m.target, m.View())
	}
	m = updateRune(t, m, '\x1b')
	if m.composing || m.noteField.Focused() || m.note != "A?1" {
		t.Fatalf("esc composing %v focused %v note %q", m.composing, m.noteField.Focused(), m.note)
	}
	if err := m.Click(1, screenRow(m.View(), "compose:")); err != nil {
		t.Fatal(err)
	}
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
	if _, err := w.Write([]byte("cHello? [A]")); err != nil {
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
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyShiftTab})
	m = next.(*Model)
	if m.target != 1 || m.note != "" {
		t.Fatalf("shift tab target %d note %q", m.target, m.note)
	}
	m.composing = true
	m.ShiftEnter()
	if m.note != "\n" {
		t.Fatalf("shift enter note %q", m.note)
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
	for _, r := range "?1jA" {
		m = updateRune(t, m, r)
	}
	view = m.View()
	if m.help || m.note != "?1jA" || strings.Count(view, "┌") != 1 || !betweenBox(view, "compose › ?1jA") || betweenBox(view, "ship it?") {
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
	press(t, m, "\r")
	for _, width := range []int{70, 120} {
		m.width = width
		m.height = 24
		view := m.View()
		for _, needle := range []string{
			"[Inbox] 1", "Answered 0", "Done 1", "Withdrawn 0",
			item.ID, "go https://ex.test/a", "ada", "1d",
			"due " + deadline.Format(time.RFC3339),
			"1 a Alpha", "0  answer:", "https://ex.test/a",
			"to: ▾ alpha", "beta", "compose:", "Send", "? help",
		} {
			if !strings.Contains(view, needle) {
				t.Fatalf("width %d missing %q\n%s", width, needle, view)
			}
		}
		for _, line := range strings.Split(strings.TrimSuffix(view, "\n"), "\n") {
			if got := utf8.RuneCountInString(visibleLine(line)); got > width {
				t.Fatalf("width %d line is %d\n%s", width, got, line)
			}
		}
		if width < 72 && screenRow(view, "[Inbox]") == screenRow(view, item.ID) {
			t.Fatalf("narrow sidebar overlaps the row\n%s", view)
		}
		if width >= 72 && screenRow(view, "[Inbox]") != screenRow(view, item.ID) {
			t.Fatalf("wide sidebar is not beside the row\n%s", view)
		}
	}

	m.width = 0
	m.height = 0
	var sent []string
	m.runSend = func(argv []string) error {
		sent = append([]string(nil), argv...)
		return nil
	}
	press(t, m, "\x1bc")
	press(t, m, "hello")
	row := screenRow(m.View(), "Send")
	line := strings.Split(m.View(), "\n")[row-1]
	if err := m.Click(visibleCol(line, "Send"), row); err != nil {
		t.Fatal(err)
	}
	if m.status != "sent to alpha" || len(sent) == 0 || sent[len(sent)-1] != "hello" {
		t.Fatalf("send status %q argv %#v", m.status, sent)
	}
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
