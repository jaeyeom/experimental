package tui

import (
	"slices"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestNotifyThreadSendsToTheItemMachine(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	m := New(t.TempDir())
	var got []string
	m.runSend = func(argv []string) error {
		got = append([]string(nil), argv...)
		return nil
	}
	item := &decide.Item{ID: "D-1", NotifyPane: "p3", NotifyMachine: "m2"}
	if err := m.notifyThread(item, "hi"); err != nil {
		t.Fatal(err)
	}
	want := []string{"herdr", "--machine", "m2", "agent", "prompt", "p3", decide.DeliveryNote("D-1", "human", "hi")}
	if !slices.Equal(got, want) {
		t.Fatalf("argv = %q, want %q", got, want)
	}
	item.NotifyMachine = ""
	if err := m.notifyThread(item, "hi"); err != nil {
		t.Fatal(err)
	}
	if got[1] != "agent" {
		t.Fatalf("local argv = %q", got)
	}
}
