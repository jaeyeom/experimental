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
	m.height = 6
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || strings.Contains(view, older.ID) {
		t.Fatalf("initial window = %s", view)
	}
	if !strings.Contains(view, "compose (c to type") || !strings.Contains(view, "to (none)") || !strings.Contains(view, "? help") {
		t.Fatalf("compose left the initial window\n%s", view)
	}
	m.Wheel(true)
	view = m.View()
	if strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || !strings.Contains(view, older.ID) {
		t.Fatalf("scrolled window = %s", view)
	}
	if !strings.Contains(view, "compose (c to type") || !strings.Contains(view, "to (none)") || !strings.Contains(view, "? help") {
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
	bar := strings.Split(m.View(), "\n")[0]
	if err := m.Click(strings.Index(bar, "Answered")+1, 1); err != nil {
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
	lines := strings.Split(view, "\n")
	if len(lines) < 2 || lines[1] != item.ID {
		t.Fatalf("detail top = %q\n%s", lines[1], view)
	}
	m.Wheel(true)
	lines = strings.Split(m.View(), "\n")
	if lines[1] == item.ID || lines[1] != "which way" {
		t.Fatalf("scrolled detail top = %q", lines[1])
	}
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
	if !strings.Contains(m.View(), "\nsent to inbox\n") || strings.Contains(m.View(), "pane-1") {
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
	press(t, m, "c]\t")
	if m.note != "" || m.target != 0 || len(got) != 0 {
		t.Fatalf("typing note %q target %d sends %d", m.note, m.target, len(got))
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	press(t, m, "]")
	if !strings.Contains(m.View(), "to: alerts ▸") {
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
	row := screenRow(view, "to: ")
	line := strings.Split(view, "\n")[row-1]
	x := strings.Index(line, "inbox")
	if x < 0 {
		t.Fatalf("target line %q", line)
	}
	if err := m.Click(utf8.RuneCountInString(line[:x])+1, row); err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || !strings.Contains(m.View(), "to: inbox ▸") {
		t.Fatalf("click target\n%s", m.View())
	}
	if err := m.Click(1, screenRow(m.View(), "compose (")); err != nil {
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
	if !strings.Contains(view, "compose (c to type, ] or click to pick target):") || !strings.Contains(view, "to: alpha ▸") {
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

	if err := m.Click(1, screenRow(m.View(), "compose (")); err != nil {
		t.Fatal(err)
	}
	focused := m.View()
	if !strings.Contains(focused, "compose › …_  Enter send · Esc cancel · Tab target") {
		t.Fatalf("focused empty =\n%s", focused)
	}
	press(t, m, "hi]")
	if m.note != "hi" || m.target != 1 || !strings.Contains(m.View(), "compose › hi_") || !strings.Contains(m.View(), "to: beta ▸") {
		t.Fatalf("focused keys note %q target %d\n%s", m.note, m.target, m.View())
	}
	if _, err := m.Key('['); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('\t'); err != nil {
		t.Fatal(err)
	}
	m.ShiftTab()
	if m.note != "hi" || m.target != 0 {
		t.Fatalf("focused cycle note %q target %d", m.note, m.target)
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	if m.composing || m.note != "hi" || !strings.Contains(m.View(), "compose (c to type, ] or click to pick target):") {
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

func TestShiftTabSequence(t *testing.T) {
	r, w := io.Pipe()
	t.Cleanup(func() { _ = w.Close() })
	events := make(chan termEvent, 4)
	errCh := make(chan error, 1)
	go readEvents(r, events, errCh)
	if _, err := w.Write([]byte("\x1b[Z\x1b[9;2u")); err != nil {
		t.Fatal(err)
	}
	first := <-events
	second := <-events
	if !first.shiftTab || !second.shiftTab || first.key == '[' || second.key == '[' {
		t.Fatalf("sequences = %+v %+v", first, second)
	}
}

func TestShiftEnterSequence(t *testing.T) {
	r, w := io.Pipe()
	t.Cleanup(func() { _ = w.Close() })
	events := make(chan termEvent, 2)
	errCh := make(chan error, 1)
	go readEvents(r, events, errCh)
	if _, err := w.Write([]byte("\x1b[13;2u\x1b[27;2;13~")); err != nil {
		t.Fatal(err)
	}
	first := <-events
	second := <-events
	if !first.shiftEnter || !second.shiftEnter {
		t.Fatalf("sequences = %+v %+v", first, second)
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
	if err := m.Click(1, screenRow(m.View(), "compose (")); err != nil {
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
	if strings.Contains(view, item.ID) || strings.Contains(view, "compose (") || strings.Contains(view, "compose ›") || strings.Contains(view, "[Inbox]") {
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

	m.composing = true
	m.note = "keep"
	press(t, m, "?\r")
	if m.note != "keep" || !m.help {
		t.Fatalf("enter while open note %q help %v", m.note, m.help)
	}
	press(t, m, "\r?")
	if m.help || m.note != "keep" {
		t.Fatalf("after close note %q help %v", m.note, m.help)
	}
	if !strings.Contains(m.View(), "compose ›") {
		t.Fatalf("compose missing after close\n%s", m.View())
	}

	press(t, m, "?")
	view = m.View()
	if strings.Contains(view, "compose (") || strings.Contains(view, "compose ›") || strings.Contains(view, item.ID) || !strings.Contains(view, "click folder") {
		t.Fatalf("second overlay = %s", view)
	}
	if _, err := m.Key('?'); err != nil {
		t.Fatal(err)
	}
	m.composing = false
	press(t, m, "\r?")
	view = m.View()
	if strings.Contains(view, "ship it?") || strings.Contains(view, "compose (") || strings.Contains(view, "compose ›") || strings.Contains(view, "[Inbox]") || !strings.Contains(view, "shortcuts") {
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

func TestSGRClickAndWheel(t *testing.T) {
	r, w := io.Pipe()
	t.Cleanup(func() { _ = w.Close() })
	events := make(chan termEvent, 2)
	errCh := make(chan error, 1)
	go readEvents(r, events, errCh)
	if _, err := w.Write([]byte("\x1b[<0;4;5M\x1b[<65;1;1M")); err != nil {
		t.Fatal(err)
	}
	click := <-events
	wheel := <-events
	if !click.mouse || !click.press || click.x != 4 || click.y != 5 || click.wheel != 0 {
		t.Fatalf("click = %+v", click)
	}
	if !wheel.mouse || wheel.wheel != 1 {
		t.Fatalf("wheel = %+v", wheel)
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
		if strings.HasPrefix(line, "> ") || strings.Contains(line, boldOn+"> ") {
			return line
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
