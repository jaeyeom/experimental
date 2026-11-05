package tui

import (
	"bytes"
	"fmt"
	"io"
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
	press(t, m, "\r")
	press(t, m, "custom words")
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
	m.height = 3
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || strings.Contains(view, older.ID) {
		t.Fatalf("initial window = %s", view)
	}
	m.Wheel(true)
	view = m.View()
	if strings.Contains(view, third.ID) || !strings.Contains(view, newer.ID) || !strings.Contains(view, older.ID) {
		t.Fatalf("scrolled window = %s", view)
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
	press(t, m, "custom words")
	if err := m.Click(1, screenRow(m.View(), "submit")); err != nil {
		t.Fatal(err)
	}
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
