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

	"github.com/jaeyeom/experimental/devtools/decide"
)

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
	if !strings.Contains(view, "compose:") || !strings.Contains(view, "to (none)") || !strings.Contains(view, "? help") {
		t.Fatalf("compose left the initial window\n%s", view)
	}
	m.Wheel(true)
	view = m.View()
	if strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || !strings.Contains(view, older.ID) {
		t.Fatalf("scrolled window = %s", view)
	}
	if !strings.Contains(view, "compose:") || !strings.Contains(view, "to (none)") || !strings.Contains(view, "? help") {
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
	if m.note != "" || m.status != "sent" || m.composing {
		t.Fatalf("after send note %q status %q composing %v", m.note, m.status, m.composing)
	}
	if !strings.Contains(m.View(), "\nsent\n") {
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
	if m.note != text || m.status != "failed" {
		t.Fatalf("note %q status %q", m.note, m.status)
	}
	view := m.View()
	if !strings.Contains(view, "failed") || !strings.Contains(view, text) {
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
	if m.note != "]" || m.target != 1 || len(got) != 0 {
		t.Fatalf("typing note %q target %d sends %d", m.note, m.target, len(got))
	}
	if _, err := m.Key('\x1b'); err != nil {
		t.Fatal(err)
	}
	press(t, m, "]]")
	if !strings.Contains(m.View(), "inbox [alerts]") {
		t.Fatalf("keyboard target\n%s", m.View())
	}
	if _, err := m.Key('c'); err != nil {
		t.Fatal(err)
	}
	press(t, m, " note\r")
	if len(got) != 1 || len(got[0]) != 5 || got[0][3] != "pane-2" || got[0][4] != "] note" {
		t.Fatalf("keyboard send = %#v", got)
	}

	view := m.View()
	row := screenRow(view, "to ")
	line := strings.Split(view, "\n")[row-1]
	x := strings.Index(line, "inbox")
	if x < 0 {
		t.Fatalf("target line %q", line)
	}
	if err := m.Click(x+1, row); err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || !strings.Contains(m.View(), "[inbox] alerts") {
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
	if !strings.Contains(m.View(), "compose: say \"hi\"\\n_") {
		t.Fatalf("view = %s", m.View())
	}
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	if calls != 1 || got != "say \"hi\"\n" {
		t.Fatalf("calls %d text %q", calls, got)
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
	if strings.Contains(view, item.ID) || strings.Contains(view, "compose:") || strings.Contains(view, "[Inbox]") {
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
	if !strings.Contains(m.View(), "compose:") {
		t.Fatalf("compose missing after close\n%s", m.View())
	}

	press(t, m, "?")
	view = m.View()
	if strings.Contains(view, "compose:") || strings.Contains(view, item.ID) || !strings.Contains(view, "click folder") {
		t.Fatalf("second overlay = %s", view)
	}
	if _, err := m.Key('?'); err != nil {
		t.Fatal(err)
	}
	m.composing = false
	press(t, m, "\r?")
	view = m.View()
	if strings.Contains(view, "ship it?") || strings.Contains(view, "compose:") || strings.Contains(view, "[Inbox]") || !strings.Contains(view, "shortcuts") {
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
