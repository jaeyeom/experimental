package decide

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
)

func TestSamePane(t *testing.T) {
	if !SamePane("box/pane-a", "pane-a") || !SamePane("pane-a", "pane-a") || !SamePane("box/pane-a", "box/pane-a") {
		t.Fatal("matching handles were rejected")
	}
	if SamePane("", "pane-a") || SamePane("box/pane-a", "") || SamePane("box/pane-ab", "pane-a") || SamePane("box/pane-a", "pane") {
		t.Fatal("non-matching handles were accepted")
	}
}

func TestResolveSendUsesTheInboxList(t *testing.T) {
	targets := []ConfigTarget{{Name: "alpha", Pane: "pane-a"}, {Name: "beta", Pane: "pane-b"}}
	bus := []BusRecipient{{Name: "coord/primary", Handle: "box/pane-a"}, {Name: "beta", Handle: "box/other"}}

	got, ok := ResolveSend(targets, bus, false, "alpha")
	if !ok || got.Bus || got.Pane != "pane-a" {
		t.Fatalf("gate off alpha = %+v %v", got, ok)
	}
	if _, ok := ResolveSend(targets, bus, false, "coord/primary"); ok {
		t.Fatal("gate off accepted a bus name")
	}

	if _, ok := ResolveSend(targets, bus, true, "alpha"); ok {
		t.Fatal("a target whose pane a bus handle holds stayed selectable")
	}
	got, ok = ResolveSend(targets, bus, true, "coord/primary")
	if !ok || !got.Bus || got.Pane != "agentbus:coord/primary" {
		t.Fatalf("bus name = %+v %v", got, ok)
	}
	got, ok = ResolveSend(targets, bus, true, "beta")
	if !ok || got.Bus || got.Pane != "pane-b" {
		t.Fatalf("config name should win over a bus name = %+v %v", got, ok)
	}
	if _, ok := ResolveSend(targets, bus, true, "nope"); ok {
		t.Fatal("unknown name was accepted")
	}
}

func TestResolveSendAgentbusPaneUsesTheBus(t *testing.T) {
	targets := []ConfigTarget{
		{Name: "team/desk", Pane: "agentbus:team/desk"},
		{Name: "alpha", Pane: "pane-a"},
	}
	bus := []BusRecipient{{Name: "team/desk", Handle: "box/other"}}

	got, ok := ResolveSend(targets, bus, true, "team/desk")
	if !ok || !got.Bus || got.Name != "team/desk" || got.Pane != "agentbus:team/desk" {
		t.Fatalf("agentbus pane = %+v %v", got, ok)
	}
	got, ok = ResolveSend(targets, bus, true, "alpha")
	if !ok || got.Bus || got.Pane != "pane-a" || got.Name != "alpha" {
		t.Fatalf("herdr pane = %+v %v", got, ok)
	}
	got, ok = ResolveSend(targets, nil, false, "team/desk")
	if !ok || !got.Bus || got.Name != "team/desk" || got.Pane != "agentbus:team/desk" {
		t.Fatalf("gate off = %+v %v", got, ok)
	}
}

func TestOpenBusThreadDeliversTheForwardNote(t *testing.T) {
	dir := t.TempDir()
	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	var got []string
	run := func(_ context.Context, args []string) ([]byte, error) {
		got = append([]string(nil), args...)
		return []byte("m-7f3a queued\n"), nil
	}
	item, err := OpenBusThread(led, dir, "D-9", "coord/primary", "hello", ViaUI, run)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 3 || got[0] != "send" || got[1] != "coord/primary" || got[2] != ForwardNote(item.ID, "D-9", "human", "hello") {
		t.Fatalf("argv = %q", got)
	}
	if item.BusMessage != "m-7f3a" || item.NotifyPane != "agentbus:coord/primary" || item.ForwardedFrom != "D-9" || item.Messages[0].Via != ViaUI {
		t.Fatalf("thread = %+v", item)
	}
}

func TestOpenBusThreadFailureDeletesTheThread(t *testing.T) {
	dir := t.TempDir()
	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	run := func(context.Context, []string) ([]byte, error) {
		return nil, errors.New("agentbus: no such name")
	}
	_, err = OpenBusThread(led, dir, "", "coord/primary", "hello", ViaCLI, run)
	if err == nil || err.Error() != "agentbus: no such name" {
		t.Fatalf("err = %v", err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 0 {
		t.Fatalf("items = %+v", items)
	}
	if _, statErr := os.Stat(filepath.Join(dir, SendsName)); !errors.Is(statErr, fs.ErrNotExist) {
		t.Fatalf("history stat = %v", statErr)
	}
}
