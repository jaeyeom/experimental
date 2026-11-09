package adapter_test

import (
	"context"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func TestBrokenSelfTestHoldsAndLogs(t *testing.T) {
	dir := t.TempDir()
	log := eventlog.Open(dir)
	broken := &fake.Adapter{
		AdapterName: "herdr",
		SelfTestErr: errString("herdr agent list failed"),
		Reach:       map[string]adapter.State{"herdr:local/default/w2:p3": adapter.StateIdle},
		Result:      map[string]adapter.Result{"herdr:local/default/w2:p3": adapter.ResultAccepted},
	}
	set := &adapter.Set{Adapters: []adapter.Adapter{broken}, Log: log}
	got, err := set.Deliver(context.Background(), "herdr:local/default/w2:p3", "hello")
	if err != nil {
		t.Fatal(err)
	}
	if got != adapter.ResultHeld {
		t.Fatalf("result = %q, want held", got)
	}
	if broken.Calls != 0 {
		t.Fatalf("broken adapter was called %d times", broken.Calls)
	}
	lines, err := log.Read(eventlog.Filter{Event: eventlog.EventAdapterError})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 1 || !strings.Contains(lines[0], "herdr agent list failed") {
		t.Fatalf("adapter_error lines = %v", lines)
	}
	held, err := log.Read(eventlog.Filter{Event: eventlog.EventHeld})
	if err != nil {
		t.Fatal(err)
	}
	if len(held) != 1 || !strings.Contains(held[0], `"target_state":"held"`) {
		t.Fatalf("held lines = %v", held)
	}
}

func TestDeliveryFallsBackToTheNextAdapter(t *testing.T) {
	broken := &fake.Adapter{AdapterName: "broken", SelfTestErr: errString("down")}
	next := &fake.Adapter{
		AdapterName: "fake",
		Reach:       map[string]adapter.State{"herdr:local/default/w2:p3": adapter.StateIdle},
		Result:      map[string]adapter.Result{"herdr:local/default/w2:p3": adapter.ResultAccepted},
	}
	log := eventlog.Open(t.TempDir())
	set := &adapter.Set{Adapters: []adapter.Adapter{broken, next}, Log: log}
	got, err := set.Deliver(context.Background(), "herdr:local/default/w2:p3", "hello")
	if err != nil {
		t.Fatal(err)
	}
	if got != adapter.ResultAccepted {
		t.Fatalf("result = %q, want accepted", got)
	}
	if next.Calls != 1 || next.LastText != "hello" {
		t.Fatalf("fallback calls = %d text %q", next.Calls, next.LastText)
	}
	lines, err := log.Read(eventlog.Filter{Event: eventlog.EventHeld})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 0 {
		t.Fatalf("held lines = %v", lines)
	}
}

type errString string

func (e errString) Error() string { return string(e) }
