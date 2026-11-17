package tui

import (
	"slices"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestNotifyThreadSendsToTheItemMachine(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	b := &LocalBackend{Dir: t.TempDir()}
	var got []string
	b.RunSend = func(argv []string) error {
		got = append([]string(nil), argv...)
		return nil
	}
	item := &decide.Item{ID: "D-1", NotifyPane: "p3", NotifyMachine: "m2"}
	if _, err := b.notifyThread(item, decide.Config{}, decide.ReplyTarget{Pane: "p3", Machine: "m2"}, "hi"); err != nil {
		t.Fatal(err)
	}
	want := []string{"herdr", "--machine", "m2", "agent", "prompt", "p3", decide.DeliveryNote("D-1", "human", "hi")}
	if !slices.Equal(got, want) {
		t.Fatalf("argv = %q, want %q", got, want)
	}
	if _, err := b.notifyThread(item, decide.Config{}, decide.ReplyTarget{Pane: "p3"}, "hi"); err != nil {
		t.Fatal(err)
	}
	if got[1] != "agent" {
		t.Fatalf("local argv = %q", got)
	}
}
