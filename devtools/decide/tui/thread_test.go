package tui

import (
	"strings"
	"testing"

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
		press(t, m, "rmore\r")
		assertTUIAppended(t, dir, id, before, "human", "more")
	})
	t.Run("compose", func(t *testing.T) {
		dir := t.TempDir()
		id, before := seedTUIThread(t, dir)
		m := openThread(t, dir)
		press(t, m, "rnote\r")
		assertTUIAppended(t, dir, id, before, "human", "note")
	})
}

func TestZeroAndRTypeOneHumanMessage(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	t.Run("open question", func(t *testing.T) {
		for _, key := range []string{"0", "r", "f"} {
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
				if strings.Contains(help, "free text") || !strings.Contains(help, "0 f r reply") || !strings.Contains(help, "r reply") {
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
