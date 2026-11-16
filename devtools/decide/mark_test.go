package decide

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// statusBus is an agentbus runner that prints reports, one per line,
// and records every call.
type statusBus struct {
	reports []string
	calls   [][]string
}

func (b *statusBus) run(_ context.Context, args []string) ([]byte, error) {
	b.calls = append(b.calls, args)
	return []byte(strings.Join(b.reports, "\n") + "\n"), nil
}

func answerMessage(t *testing.T, it *Item) Message {
	t.Helper()
	for _, msg := range it.Messages {
		if msg.Author == "human" && msg.Time.Equal(it.AnsweredAt) {
			return msg
		}
	}
	t.Fatalf("no answer message in %+v", it.Messages)
	return Message{}
}

func TestAnswerDeliveryKeepsTheBusID(t *testing.T) {
	writeConfig(t, `{}`)
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' 'm-1 held: pane focused'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "agentbus:ops/x", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 1 {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
	d := got.Delivery[0]
	if d.BusID != "m-1" || d.Outcome != OutcomeWaiting || !d.Message.Equal(got.AnsweredAt) || d.Reason != HeldComposerBusy {
		t.Fatalf("delivery = %+v", d)
	}
	mark, ok := MessageMark(&got, answerMessage(t, &got))
	if !ok || mark.String(time.UTC) != "waiting: target composer busy" {
		t.Fatalf("mark = %+v, %v", mark, ok)
	}
}

func TestNewBusThreadRecordsItsFirstMessage(t *testing.T) {
	bus := &statusBus{reports: []string{"m-2 queued for delivery"}}
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	dir := t.TempDir()
	led := openDeliveryLedger(t, dir, &clock)
	item, err := OpenBusThread(led, dir, "", "ops/x", "hello", ViaUI, bus.run)
	if err != nil {
		t.Fatal(err)
	}
	if item.BusMessage != "m-2" || len(item.Delivery) != 1 {
		t.Fatalf("item = %+v", item)
	}
	d := item.Delivery[0]
	if d.BusID != "m-2" || !d.Compose || d.State() != OutcomeWaiting || !d.Message.Equal(item.Messages[0].Time) {
		t.Fatalf("delivery = %+v", d)
	}
	if mark, ok := MessageMark(&item, item.Messages[0]); !ok || mark.State != OutcomeWaiting {
		t.Fatalf("mark = %+v, %v", mark, ok)
	}
}

func TestNewBusThreadWhileTheBusIsDownWaits(t *testing.T) {
	run := func(context.Context, []string) ([]byte, error) {
		return []byte("m-4 held: herdr unreachable\n"), &exitErr{code: 3}
	}
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	dir := t.TempDir()
	led := openDeliveryLedger(t, dir, &clock)
	item, err := OpenBusThread(led, dir, "", "ops/x", "hello", ViaUI, run)
	if err != nil {
		t.Fatal(err)
	}
	mark, ok := MessageMark(&item, item.Messages[0])
	if !ok || mark.String(time.UTC) != "waiting: bus down" {
		t.Fatalf("mark = %+v, %v", mark, ok)
	}
}

type exitErr struct{ code int }

func (e *exitErr) Error() string { return "exit status 3" }

func (e *exitErr) ExitCode() int { return e.code }

func TestReplyDeliveryRecordsTheBusID(t *testing.T) {
	writeConfig(t, `{}`)
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' 'm-3 delivered'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "agentbus:ops/x", "")
	clock = clock.Add(time.Minute)
	item, err := led.Compose(item.ID, "one more thing", ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	reply := item.Messages[len(item.Messages)-1]
	d, err := NotifyOwner(item.NotifyPane, "", item.ID, reply.Text)
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.RecordCompose(item.ID, reply.Time, d)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Delivery) != 1 || got.Delivery[0].BusID != "m-3" || !got.Delivery[0].Compose {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
	if mark, ok := MessageMark(&got, reply); !ok || mark.String(time.UTC) != "delivered 12:01" {
		t.Fatalf("mark = %+v, %v", mark, ok)
	}
	if _, ok := MessageMark(&got, answerMessage(t, &got)); ok {
		t.Fatal("the reply record marked the answer")
	}
	if attempts := attemptsSinceAnswer(&got); len(attempts) != 0 {
		t.Fatalf("a reply counted as an answer attempt: %+v", attempts)
	}
}

func TestHeldThenDeliveredIsLate(t *testing.T) {
	writeConfig(t, `{}`)
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' 'm-1 queued for delivery'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "agentbus:ops/x", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}

	bus := &statusBus{reports: []string{`{"id":"m-1","state":"held","held_reason":"transport unreachable","stored_at":"2026-11-14T12:00:00.000Z"}`}}
	if changed, err := led.CheckDeliveries(context.Background(), bus.run); err != nil || !changed {
		t.Fatalf("check = %v, %v", changed, err)
	}
	if len(bus.calls) != 1 || strings.Join(bus.calls[0], " ") != "status --json m-1" {
		t.Fatalf("agentbus calls = %q", bus.calls)
	}
	got := mustGet(t, led, item.ID)
	if mark, _ := MessageMark(&got, answerMessage(t, &got)); mark.String(time.UTC) != "waiting: bus down" {
		t.Fatalf("held mark = %q", mark.String(time.UTC))
	}

	bus.reports = []string{`{"id":"m-1","state":"delivered","stored_at":"2026-11-14T12:00:00.000Z","delivered_at":"2026-11-14T12:07:00.000Z"}`}
	if _, err := led.CheckDeliveries(context.Background(), bus.run); err != nil {
		t.Fatal(err)
	}
	got = mustGet(t, led, item.ID)
	mark, _ := MessageMark(&got, answerMessage(t, &got))
	if mark.State != OutcomeDelivered || mark.String(time.UTC) != "delivered 12:07, 7 min late" {
		t.Fatalf("delivered mark = %q", mark.String(time.UTC))
	}
	if HasPending(&got) {
		t.Fatal("a delivered record is still pending")
	}
}

func TestBusFailureShowsTheError(t *testing.T) {
	writeConfig(t, `{}`)
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' 'm-1 queued for delivery'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "agentbus:ops/x", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	bus := &statusBus{reports: []string{`{"id":"m-1","state":"failed","error":"wake failed: pane gone"}`}}
	if _, err := led.CheckDeliveries(context.Background(), bus.run); err != nil {
		t.Fatal(err)
	}
	got := mustGet(t, led, item.ID)
	if mark, _ := MessageMark(&got, answerMessage(t, &got)); mark.String(time.UTC) != "failed: wake failed: pane gone" {
		t.Fatalf("mark = %q", mark.String(time.UTC))
	}
	if !HasPending(&got) {
		t.Fatal("a failure agentbus retries is not pending")
	}
	if err := led.RetryDue(clock.Add(time.Hour), nil); err != nil {
		t.Fatal(err)
	}
	if again := mustGet(t, led, item.ID); len(again.Delivery) != 1 {
		t.Fatalf("re-sent a message agentbus is retrying: %+v", again.Delivery)
	}

	installProgram(t, "agentbus", "#!/bin/sh\necho 'agentbus: no such name' >&2\nexit 1\n")
	clock = clock.Add(time.Minute)
	item, err := led.Compose(item.ID, "again", ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	reply := item.Messages[len(item.Messages)-1]
	d, serr := NotifyOwner(item.NotifyPane, "", item.ID, reply.Text)
	if serr == nil {
		t.Fatal("failing send returned nil")
	}
	got, err = led.RecordCompose(item.ID, reply.Time, d)
	if err != nil {
		t.Fatal(err)
	}
	if mark, _ := MessageMark(&got, reply); mark.State != OutcomeFailed || !strings.Contains(mark.String(time.UTC), "no such name") {
		t.Fatalf("send failure mark = %q", mark.String(time.UTC))
	}
}

func TestLegacyQueuedRecordReadsAsWaitingUntilResolved(t *testing.T) {
	dir := t.TempDir()
	legacy := `{"next":2,"items":[{"id":"D-001","question":"ship","owner":"ada","kind":"relay","state":"answered",
"options":[{"key":"a","label":"Alpha","recommended":false}],"freeText":false,"unread":false,
"createdAt":"2026-11-14T11:00:00Z","answeredAt":"2026-11-14T12:00:00Z",
"answer":{"value":"a","key":"a","label":"Alpha","answeredBy":"human","via":"cli"},
"notifyPane":"agentbus:ops/x",
"messages":[{"author":"ada","time":"2026-11-14T11:00:00Z","text":"ship"},{"author":"human","time":"2026-11-14T12:00:00Z","text":"a: Alpha","via":"cli"}],
"delivery":[{"at":"2026-11-14T12:00:01Z","target":"agentbus:ops/x","outcome":"delivered (queued)"}]}]}`
	if err := os.WriteFile(filepath.Join(dir, ledgerName), []byte(legacy), 0o600); err != nil {
		t.Fatal(err)
	}
	clock := time.Date(2026, 11, 14, 13, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	got := mustGet(t, led, "D-001")
	mark, ok := MessageMark(&got, got.Messages[1])
	if !ok || mark.State != OutcomeWaiting || mark.String(time.UTC) != "waiting" {
		t.Fatalf("legacy mark = %+v, %v", mark, ok)
	}
	if _, ok := Undelivered(&got); ok {
		t.Fatal("a legacy queued record is undelivered")
	}
	key := DeliveryKey(&got)
	bus := &statusBus{reports: []string{`{"id":"` + key + `","state":"delivered","delivered_at":"2026-11-14T12:00:01.000Z"}`}}
	if _, err := led.CheckDeliveries(context.Background(), bus.run); err != nil {
		t.Fatal(err)
	}
	if len(bus.calls) != 1 || strings.Join(bus.calls[0], " ") != "status --json "+key {
		t.Fatalf("agentbus calls = %q", bus.calls)
	}
	got = mustGet(t, led, "D-001")
	if mark, _ := MessageMark(&got, got.Messages[1]); mark.String(time.UTC) != "delivered 12:00" {
		t.Fatalf("resolved mark = %q", mark.String(time.UTC))
	}
}

func TestCheckDeliveriesRunsNothingWhenNothingWaits(t *testing.T) {
	writeConfig(t, `{}`)
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' 'm-1 delivered'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "agentbus:ops/x", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	bus := &statusBus{}
	if changed, err := led.CheckDeliveries(context.Background(), bus.run); err != nil || changed {
		t.Fatalf("check = %v, %v", changed, err)
	}
	if len(bus.calls) != 0 {
		t.Fatalf("agentbus ran with nothing waiting: %q", bus.calls)
	}
}
