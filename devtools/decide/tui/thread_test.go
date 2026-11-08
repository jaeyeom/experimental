package tui

import (
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
