package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestTUIAppendsOnAThreeMessageThread(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	t.Run("answer", func(t *testing.T) {
		dir := t.TempDir()
		id, before := seedTUIThread(t, dir)
		m := New(dir)
		m.runSend = func([]string) error { return nil }
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		press(t, m, "o1")
		assertTUIAppended(t, dir, id, before, "human", "keep")
	})
	t.Run("reply", func(t *testing.T) {
		dir := t.TempDir()
		id, before := seedTUIThread(t, dir)
		m := openThread(t, dir)
		press(t, m, "0more\r")
		assertTUIAppended(t, dir, id, before, "human", "more")
	})
	t.Run("compose", func(t *testing.T) {
		dir := t.TempDir()
		id, before := seedTUIThread(t, dir)
		m := openThread(t, dir)
		press(t, m, "0note\r")
		assertTUIAppended(t, dir, id, before, "human", "note")
	})
}

func TestZeroTypesOneHumanMessage(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	t.Run("open question", func(t *testing.T) {
		for _, key := range []string{"0"} {
			t.Run(key, func(t *testing.T) {
				dir := t.TempDir()
				led := openLedger(t, dir)
				item := mustCreate(t, led, &decide.Item{
					Question:   "root ask",
					Owner:      "ada",
					Kind:       decide.KindRelay,
					FreeText:   false,
					NotifyPane: "pane-9",
					Messages: []decide.Message{{
						Author: "ada",
						Text:   "root ask",
					}},
					Options: []decide.Option{
						{Key: "a", Label: "Alpha", Recommended: true},
						{Key: "b", Label: "Beta"},
					},
				})
				m := openThread(t, dir)
				press(t, m, "?")
				help := m.View()
				if strings.Contains(help, "free text") || !strings.Contains(help, "0 reply") || strings.Contains(help, "r reply") {
					t.Fatalf("help lists two typing actions\n%s", help)
				}
				press(t, m, "?")
				if strings.Contains(m.View(), "free text is not allowed") || strings.Contains(m.View(), "0  answer:") {
					t.Fatalf("composer is a second row\n%s", m.View())
				}
				press(t, m, key+`say "hi"`+"\r")
				if strings.Contains(m.View(), "free text is not allowed") {
					t.Fatalf("status rejected the composer\n%s", m.View())
				}
				got, err := led.Get(item.ID)
				if err != nil {
					t.Fatal(err)
				}
				const want = `say "hi"`
				if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != want || got.Answer.AnsweredBy != "human" {
					t.Fatalf("answer = %+v state %s", got.Answer, got.State)
				}
				if len(got.Messages) != 2 || got.Messages[0].Text != "root ask" || got.Messages[1].Author != "human" || got.Messages[1].Text != want {
					t.Fatalf("messages = %+v", got.Messages)
				}
			})
		}
	})

	t.Run("not an open question", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question:   "root ask",
			Owner:      "ada",
			Kind:       decide.KindRelay,
			State:      decide.StateAnswered,
			NotifyPane: "pane-9",
			Answer:     &decide.Answer{Value: "kept", AnsweredBy: "human"},
			Messages: []decide.Message{
				{Author: "ada", Text: "root ask"},
				{Author: "human", Text: "kept"},
			},
		})
		m := New(dir)
		m.runSend = func([]string) error { return nil }
		m.SendTemplate = "herdr agent prompt {pane}"
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		press(t, m, "gao0later\r")
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "kept" {
			t.Fatalf("answer changed: %+v state %s", got.Answer, got.State)
		}
		if len(got.Messages) != 3 || got.Messages[0].Text != "root ask" || got.Messages[1].Text != "kept" || got.Messages[2].Text != "later" || got.Messages[2].Author != "human" {
			t.Fatalf("messages = %+v", got.Messages)
		}
	})

	t.Run("empty and cancel", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question: "root ask",
			Owner:    "ada",
			Kind:     decide.KindRelay,
			Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
		})
		m := openThread(t, dir)
		press(t, m, "0\r")
		press(t, m, "0nope\x1b")
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateOpen || got.Answer != nil || len(got.Messages) != 0 && len(got.Messages) != 1 {
			t.Fatalf("cancel changed the thread: state %s answer %+v messages %+v", got.State, got.Answer, got.Messages)
		}
		if len(got.Messages) == 1 && got.Messages[0].Text != "root ask" {
			t.Fatalf("messages = %+v", got.Messages)
		}
		press(t, m, "1")
		got, err = led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.Answer == nil || got.Answer.Value != "a" {
			t.Fatalf("option key = %+v", got.Answer)
		}
	})
}

func openThread(t *testing.T, dir string) *Model {
	t.Helper()
	m := New(dir)
	m.runSend = func([]string) error { return nil }
	m.SendTemplate = "herdr agent prompt {pane}"
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	return m
}

func seedTUIThread(t *testing.T, dir string) (string, []decide.Message) {
	t.Helper()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question:   "root ask",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		FreeText:   true,
		NotifyPane: "pane-9",
		Options: []decide.Option{
			{Key: "y", Label: "Yes", Recommended: true},
			{Key: "n", Label: "No"},
		},
	})
	if _, err := led.Answer(item.ID, "human note"); err != nil {
		t.Fatal(err)
	}
	got, err := led.Reply(item.ID, "agent draft", []decide.Option{
		{Key: "keep", Label: "Keep", Recommended: true},
		{Key: "drop", Label: "Drop"},
	}, false)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Messages) != 3 {
		t.Fatalf("seed = %+v", got.Messages)
	}
	return got.ID, append([]decide.Message(nil), got.Messages...)
}

func TestItemViewFooterNamesArchive(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	dir := t.TempDir()
	led := openLedger(t, dir)
	note := mustCreate(t, led, &decide.Item{
		Question:  "shipped",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC),
	})
	question := mustCreate(t, led, &decide.Item{
		Question:  "which way",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Options:   []decide.Option{{Key: "a", Label: "Alpha"}},
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	view := m.View()
	if !strings.Contains(view, "y archive") || !strings.Contains(view, "? help") {
		t.Fatalf("item footer\n%s", view)
	}
	press(t, m, "?")
	help := m.View()
	if !strings.Contains(help, "y archive") {
		t.Fatalf("item help\n%s", help)
	}
	press(t, m, "?")
	press(t, m, "y")
	got, err := led.Get(note.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateDone || got.Outcome != "archived" {
		t.Fatalf("notification = state %s outcome %q", got.State, got.Outcome)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "oy")
	got, err = led.Get(question.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || m.status != "needs an answer" {
		t.Fatalf("question state %s status %q", got.State, m.status)
	}
}

func TestThreadMessagesAreBlocks(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "")

	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 8, 12, 0, 0, 0, time.UTC)
	long := strings.Repeat("word ", 30)
	mustCreate(t, led, &decide.Item{
		Question: "root ask",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Messages: []decide.Message{
			{Author: "ada", Time: now.Add(-10 * 24 * time.Hour), Text: "root ask"},
			{
				Author: "human",
				Time:   now.Add(-2 * time.Hour),
				Text:   long,
				Options: []decide.Option{
					{Key: "keep", Label: "Keep"},
					{Key: "drop", Label: "Drop"},
				},
			},
			{Author: "ada", Time: now.Add(-3 * time.Minute), Text: "newest note"},
		},
	})
	m := New(dir)
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 40
	m.height = 24
	press(t, m, "o")
	view := m.View()
	assertFrame(t, view, 40, 24)
	vis := visibleLine(view)
	if strings.Count(vis, "────") < 2 {
		t.Fatalf("rules = %d\n%s", strings.Count(vis, "────"), view)
	}
	for _, needle := range []string{"ada  10d", "human  2h  keep drop", "ada  3m  latest", "  root ask", "  newest note"} {
		if !strings.Contains(vis, needle) {
			t.Fatalf("missing %q\n%s", needle, vis)
		}
	}
	if strings.Count(vis, "latest") != 1 {
		t.Fatalf("latest count\n%s", vis)
	}
	middle := visibleLine(lineContaining(view, "human"))
	if !strings.Contains(middle, "keep") || !strings.Contains(middle, "2h") {
		t.Fatalf("middle header %q", middle)
	}
	if strings.Contains(visibleLine(lineContaining(view, "10d")), "latest") {
		t.Fatalf("oldest marked latest\n%s", vis)
	}
	humanColor := authorSGR(view, "2h", "human")
	agentColor := authorSGR(view, "10d", "ada")
	if humanColor == "" || agentColor == "" || humanColor == agentColor {
		t.Fatalf("author colors human %q agent %q", humanColor, agentColor)
	}

	t.Setenv("NO_COLOR", "1")
	plain := m.View()
	assertFrame(t, plain, 40, 24)
	if hasColorSequence(plain) {
		t.Fatalf("NO_COLOR view has color\n%s", plain)
	}
	plainVis := visibleLine(plain)
	for _, needle := range []string{"ada  10d", "human  2h  keep drop", "latest", "────", "  root ask"} {
		if !strings.Contains(plainVis, needle) {
			t.Fatalf("NO_COLOR missing %q\n%s", needle, plainVis)
		}
	}
}

// authorSGR is the color sequence applied to author on the line that shows marker.
func authorSGR(view, marker, author string) string {
	for _, line := range strings.Split(view, "\n") {
		if !strings.Contains(visibleLine(line), marker) {
			continue
		}
		i := strings.Index(line, author)
		if i < 0 {
			return ""
		}
		start := strings.LastIndex(line[:i], "\x1b[")
		if start < 0 {
			return ""
		}
		end := strings.Index(line[start:], "m")
		if end < 0 {
			return ""
		}
		return line[start : start+end+1]
	}
	return ""
}

func assertTUIAppended(t *testing.T, dir, id string, before []decide.Message, author, text string) {
	t.Helper()
	led := openLedger(t, dir)
	got, err := led.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Messages) != len(before)+1 {
		t.Fatalf("messages = %+v", got.Messages)
	}
	for i := range before {
		if got.Messages[i].Author != before[i].Author || got.Messages[i].Text != before[i].Text {
			t.Fatalf("message %d = %+v, want %+v", i, got.Messages[i], before[i])
		}
		if len(got.Messages[i].Options) != len(before[i].Options) {
			t.Fatalf("message %d options = %+v, want %+v", i, got.Messages[i].Options, before[i].Options)
		}
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Author != author || last.Text != text {
		t.Fatalf("new message = %+v", last)
	}
}

func TestOnlyZeroFocusesReplyInItemView(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question: "root ask",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}},
		Messages: []decide.Message{{Author: "ada", Text: "root ask"}},
	})
	m := openThread(t, dir)
	m.width = 160
	for _, key := range []string{"r", "f"} {
		press(t, m, key)
		if m.replyFocus {
			t.Fatalf("%q focused the reply box", key)
		}
		if m.composing {
			press(t, m, "\x1b")
		}
	}
	m.status = ""
	footer := lineContaining(m.View(), "? help")
	if !strings.Contains(footer, "0 reply") || strings.Contains(footer, "r reply") || strings.Contains(footer, "f reply") {
		t.Fatalf("footer %q", footer)
	}
	press(t, m, "?")
	help := m.View()
	if !strings.Contains(help, "0 reply") || strings.Contains(help, "f r") || strings.Contains(help, "r reply") {
		t.Fatalf("help\n%s", help)
	}
	press(t, m, "?")
	press(t, m, "0")
	if !m.replyFocus {
		t.Fatal("0 did not focus the reply box")
	}
}

func TestOptionsRenderNumberedWithoutKeyBadges(t *testing.T) {
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "")
	writeColorConfig(t, cfg, "{}\n")
	dir := t.TempDir()
	led := openLedger(t, dir)
	opts := []decide.Option{
		{Key: "y", Label: "Archive it"},
		{Key: "k", Label: "Keep it", Recommended: true},
	}
	mustCreate(t, led, &decide.Item{
		Question: "root ask",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		Options:  opts,
		Messages: []decide.Message{{Author: "ada", Text: "root ask", Options: opts}},
	})
	m := openThread(t, dir)
	m.width = 120
	m.height = 40
	view := m.View()
	vis := visibleLine(view)
	for _, needle := range []string{"1  Archive it", "2  Keep it (recommended)"} {
		if !strings.Contains(vis, needle) {
			t.Fatalf("missing %q\n%s", needle, vis)
		}
	}
	for _, bad := range []string{"1 y ", "2 k ", " recommended "} {
		if strings.Contains(vis, bad) {
			t.Fatalf("view has %q\n%s", bad, vis)
		}
	}
	badge, _, _ := strings.Cut(paletteStyle("", paletteBadge).Render("y"), "y")
	for _, key := range []string{"y", "k"} {
		if strings.Contains(view, badge+key) {
			t.Fatalf("option key %q uses the badge style\n%q", key, view)
		}
	}
}

func seedForward(t *testing.T, dir string, msgs []decide.Message, links []string) decide.Item {
	t.Helper()
	led := openLedger(t, dir)
	return mustCreate(t, led, &decide.Item{
		Question: "ship the release?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		Links:    links,
		Messages: msgs,
	})
}

func forwardModel(t *testing.T, dir string) (*Model, *[]string) {
	t.Helper()
	m := openThread(t, dir)
	m.Targets = []Target{{Name: "alpha", Pane: "pane-1"}, {Name: "beta", Pane: "pane-2"}}
	var sent []string
	m.runSend = func(argv []string) error {
		sent = append([]string(nil), argv...)
		return nil
	}
	m.width = 120
	m.height = 30
	return m, &sent
}

func TestForwardPrefillsTheSelectedMessage(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	at := func(h int) time.Time { return time.Date(2026, 11, 5, h, 0, 0, 0, time.UTC) }
	item := seedForward(t, dir, []decide.Message{
		{Author: "ada", Time: at(1), Text: "ship the release?"},
		{Author: "human", Time: at(2), Text: "second line\nmore"},
		{Author: "ada", Time: at(3), Text: "third"},
	}, []string{"https://ex.test/pr/1"})
	m, _ := forwardModel(t, dir)
	press(t, m, "n")
	press(t, m, "f")
	want := "Forwarded from " + item.ID + " (owner: ada)\n" +
		"> ship the release?\n" +
		"> human 2026-11-05T02:00:00Z\n" +
		"> second line\n" +
		"> more\n" +
		"Link: https://ex.test/pr/1\n"
	if !m.composing || m.composeFocus != focusText || m.note != want {
		t.Fatalf("composing %v focus %d note\n%q\nwant\n%q", m.composing, m.composeFocus, m.note, want)
	}
	press(t, m, "hi")
	if m.note != want+"hi" {
		t.Fatalf("cursor is not after the prefill: %q", m.note)
	}
}

func TestForwardWithoutMessagesOrLinks(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "plain ask",
		Owner:    "ada",
		Kind:     decide.KindRelay,
	})
	m, _ := forwardModel(t, dir)
	// Create threads the question as the first message, so drop it.
	m.detail.Messages = nil
	press(t, m, "f")
	want := "Forwarded from " + item.ID + " (owner: ada)\n> plain ask\n"
	if m.note != want {
		t.Fatalf("note %q want %q", m.note, want)
	}
}

func TestForwardTruncatesALongMessage(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	long := strings.Repeat("x", 5000)
	seedForward(t, dir, []decide.Message{{Author: "ada", Text: long}}, nil)
	m, _ := forwardModel(t, dir)
	press(t, m, "f")
	if len(m.note) > 2500 || !strings.Contains(m.note, "…") {
		t.Fatalf("long note is %d bytes\n%.200s", len(m.note), m.note)
	}
}

func TestForwardSendRecordsTheSource(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	item := seedForward(t, dir, []decide.Message{{Author: "ada", Text: "ship the release?"}}, nil)
	m, sent := forwardModel(t, dir)
	press(t, m, "f")
	press(t, m, "please look")
	m.ShiftTab()
	press(t, m, "j\t\t\r")
	if m.composing || m.status != "sent to beta" {
		t.Fatalf("composing %v status %q", m.composing, m.status)
	}
	items, err := openLedger(t, dir).List()
	if err != nil {
		t.Fatal(err)
	}
	var fwd *decide.Item
	for i := range items {
		if items[i].ID != item.ID {
			fwd = &items[i]
		}
	}
	if fwd == nil || fwd.ForwardedFrom != item.ID || fwd.Owner != "beta" || !strings.HasSuffix(fwd.Question, "please look") {
		t.Fatalf("forwarded thread = %+v", fwd)
	}
	note := (*sent)[len(*sent)-1]
	if !strings.Contains(note, "Fwd "+item.ID) || note != decide.ForwardNote(fwd.ID, item.ID, "human", fwd.Question) {
		t.Fatalf("note = %q", note)
	}
	press(t, m, "c")
	if m.note != "" {
		t.Fatalf("compose after a forward kept %q", m.note)
	}
	press(t, m, "\x1b")

	m.detail = fwd
	m.mode = modeDetail
	if !strings.Contains(m.View(), "forwarded from "+item.ID) {
		t.Fatalf("header misses the source\n%s", m.View())
	}
}

func TestForwardEscDiscardsThePrefill(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	seedForward(t, dir, []decide.Message{{Author: "ada", Text: "ship the release?"}}, nil)
	m, sent := forwardModel(t, dir)
	press(t, m, "f\x1b")
	if m.composing || m.note != "" || m.noteField.Value() != "" {
		t.Fatalf("composing %v note %q", m.composing, m.note)
	}
	press(t, m, "c")
	if m.note != "" || strings.Contains(m.View(), "Forwarded from") {
		t.Fatalf("draft left behind\n%s", m.View())
	}
	press(t, m, "\x1b")
	if len(*sent) != 0 {
		t.Fatalf("sent %q", *sent)
	}
}

func TestForwardHints(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	seedForward(t, dir, []decide.Message{{Author: "ada", Text: "ship the release?"}}, nil)
	m, _ := forwardModel(t, dir)
	footer := lineContaining(m.View(), "? help")
	if !strings.Contains(footer, "f forward") {
		t.Fatalf("footer %q", footer)
	}
	press(t, m, "?")
	if !strings.Contains(m.View(), "f forward") {
		t.Fatalf("help\n%s", m.View())
	}
}
