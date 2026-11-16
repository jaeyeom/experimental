package tui

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// markBus answers agentbus send with send and status --json with status,
// and records every call.
type markBus struct {
	send   string
	status string
	calls  []string
}

func (b *markBus) run(_ context.Context, args []string) ([]byte, error) {
	b.calls = append(b.calls, strings.Join(args, " "))
	switch args[0] {
	case "send":
		return []byte(b.send + "\n"), nil
	case "status":
		return []byte(b.status + "\n"), nil
	}
	return nil, nil
}

func (b *markBus) statusCalls() []string {
	var out []string
	for _, c := range b.calls {
		if strings.HasPrefix(c, "status") {
			out = append(out, c)
		}
	}
	return out
}

// noRealAgentbus puts an agentbus first on PATH that always fails, so
// a send that skips the injected runner never reaches a real bus.
func noRealAgentbus(t *testing.T) {
	t.Helper()
	bin := t.TempDir()
	if err := os.WriteFile(filepath.Join(bin, "agentbus"), []byte("#!/bin/sh\necho 'agentbus: test fake' >&2\nexit 1\n"), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func seedBusThread(t *testing.T, dir string) decide.Item {
	t.Helper()
	noRealAgentbus(t)
	return mustCreate(t, openLedger(t, dir), &decide.Item{
		Question:   "root ask",
		Owner:      "ops/x",
		Kind:       decide.KindRelay,
		FreeText:   true,
		NotifyPane: "agentbus:ops/x",
	})
}

func markModel(t *testing.T, dir string, bus *markBus) *Model {
	t.Helper()
	m := New(dir)
	m.busRun = bus.run
	m.UTC = true
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	return m
}

func TestInboxReplyGetsAWaitingMarkWithTheBusID(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	item := seedBusThread(t, dir)
	bus := &markBus{send: "m-5 held: herdr unreachable"}
	m := markModel(t, dir, bus)
	press(t, m, "o")
	press(t, m, "0hello\r")

	got, err := openLedger(t, dir).Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Delivery) != 1 || got.Delivery[0].BusID != "m-5" || !got.Delivery[0].Compose || got.Delivery[0].State() != decide.OutcomeWaiting {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if view := m.View(); !strings.Contains(view, "waiting: bus down") {
		t.Fatalf("thread has no waiting mark\n%s", view)
	}
}

func TestThreadMarksDeliveredLateAndFailed(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	item := seedBusThread(t, dir)
	led := openLedger(t, dir)
	sent := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return sent })
	item, err := led.Compose(item.ID, "first", decide.ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	first := item.Messages[len(item.Messages)-1].Time
	if _, err := led.RecordCompose(item.ID, first, decide.Delivery{Target: "agentbus:ops/x", Outcome: decide.OutcomeWaiting, BusID: "m-1"}); err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return sent.Add(time.Minute) })
	item, err = led.Compose(item.ID, "second", decide.ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	second := item.Messages[len(item.Messages)-1].Time
	if _, err := led.RecordCompose(item.ID, second, decide.Delivery{Target: "agentbus:ops/x", Outcome: decide.OutcomeFailed, Error: "agentbus: no such name"}); err != nil {
		t.Fatal(err)
	}
	bus := &markBus{status: `{"id":"m-1","state":"delivered","delivered_at":"2026-11-14T12:05:00.000Z"}`}
	if _, err := led.CheckDeliveries(context.Background(), bus.run); err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return sent.Add(2 * time.Minute) })
	if _, err := led.Reply(item.ID, "", "ack", nil, false); err != nil {
		t.Fatal(err)
	}

	m := markModel(t, dir, bus)
	m.width, m.height = 120, 40
	press(t, m, "o")
	view := m.View()
	for _, want := range []string{"delivered 12:05, 5 min late", "failed: agentbus: no such name"} {
		if !strings.Contains(view, want) {
			t.Fatalf("thread lacks %q\n%s", want, view)
		}
	}
	if strings.Contains(view, "delivered (queued)") {
		t.Fatalf("thread shows a queued message as delivered\n%s", view)
	}
}

func TestDeliveryPollRunsOnlyWhileAMessageWaits(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	item := seedBusThread(t, dir)
	bus := &markBus{status: `{"id":"m-1","state":"held","held_reason":"composer busy"}`}
	m := markModel(t, dir, bus)

	if cmd := m.startDeliveryPoll(); cmd != nil || m.deliveryPolling {
		t.Fatal("a poll started with nothing waiting")
	}
	if cmd, _ := m.updateDelivery(deliveryTickMsg{}); cmd != nil {
		t.Fatal("a tick checked with nothing waiting")
	}
	if len(bus.statusCalls()) != 0 {
		t.Fatalf("agentbus status ran with nothing waiting: %q", bus.calls)
	}

	led := openLedger(t, dir)
	item, err := led.Compose(item.ID, "hello", decide.ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Reply(item.ID, "", "ack", nil, false); err != nil {
		t.Fatal(err)
	}
	if _, err := led.RecordCompose(item.ID, item.Messages[len(item.Messages)-1].Time, decide.Delivery{Target: "agentbus:ops/x", Outcome: decide.OutcomeWaiting, BusID: "m-1"}); err != nil {
		t.Fatal(err)
	}
	if _, cmd := m.updateReload(); cmd == nil || !m.deliveryPolling {
		t.Fatal("a reload with a waiting message started no poll")
	}
	check, _ := m.updateDelivery(deliveryTickMsg{})
	if check == nil {
		t.Fatal("the tick started no check")
	}
	if next, _ := m.updateDelivery(check()); next == nil {
		t.Fatal("a finished check scheduled no next tick")
	}
	if calls := bus.statusCalls(); len(calls) != 1 || calls[0] != "status --json m-1" {
		t.Fatalf("agentbus calls = %q", bus.calls)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	if view := m.View(); !strings.Contains(view, "waiting: target composer busy") {
		t.Fatalf("thread lacks the held reason\n%s", view)
	}

	bus.status = `{"id":"m-1","state":"delivered","delivered_at":"2026-11-14T12:05:00.000Z"}`
	check, _ = m.updateDelivery(deliveryTickMsg{})
	if next, _ := m.updateDelivery(check()); next == nil {
		t.Fatal("a finished check scheduled no next tick")
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if cmd, _ := m.updateDelivery(deliveryTickMsg{}); cmd != nil || m.deliveryPolling {
		t.Fatal("the poll kept going once nothing waited")
	}
	if calls := bus.statusCalls(); len(calls) != 2 {
		t.Fatalf("agentbus status calls = %q", calls)
	}
}
