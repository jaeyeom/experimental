package decide

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestDeliverySendErrorRecordsFailureAndRetries(t *testing.T) {
	dir := t.TempDir()
	logPath := filepath.Join(dir, "keys")
	writeConfig(t, `{"notifyTemplate":"notifier"}`)
	installProgram(t, "notifier", "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\nexit 1\n")

	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "pane-9", "")

	if _, err := led.DeliverAnswer(&item, nil); err == nil {
		t.Fatal("failing notify returned nil")
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeFailed || got.Delivery[0].Error == "" {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
	if got.Delivery[0].Target != "pane-9" {
		t.Fatalf("target = %q", got.Delivery[0].Target)
	}
	raw, err := json.Marshal(got)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(raw), `"delivery"`) || !strings.Contains(string(raw), `"outcome":"failed"`) {
		t.Fatalf("json = %s", raw)
	}
	key := DeliveryKey(&got)
	if keys := readLines(t, logPath); len(keys) != 1 || keys[0] != key {
		t.Fatalf("keys = %#v, want %q", keys, key)
	}

	if err := led.RetryDue(clock, nil); err != nil {
		t.Fatal(err)
	}
	if keys := readLines(t, logPath); len(keys) != 1 {
		t.Fatalf("early retry keys = %#v", keys)
	}

	clock = clock.Add(DeliveryBackoff(1))
	if err := led.RetryDue(clock, nil); err == nil {
		t.Fatal("due retry returned nil")
	}
	keys := readLines(t, logPath)
	if len(keys) != 2 || keys[0] != keys[1] || keys[0] != key {
		t.Fatalf("retry keys = %#v, want %q twice", keys, key)
	}
	got = mustGet(t, led, item.ID)
	if len(got.Delivery) != 2 || got.Delivery[1].Outcome != OutcomeFailed {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
}

func TestDeliveryErrorJSONIsFailed(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"printf '%s' '{\"error\":{\"code\":\"agent_not_found\"}}'"}`)
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "w1:p1", "")
	if _, err := led.DeliverAnswer(&item, nil); err == nil || !strings.Contains(err.Error(), "agent_not_found") {
		t.Fatalf("err = %v", err)
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeFailed || !strings.Contains(got.Delivery[0].Error, "agent_not_found") {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
}

func TestDeliveryPlainMissIsFailed(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"printf '%s' 'pane not found'"}`)
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "w1:p1", "")
	if _, err := led.DeliverAnswer(&item, nil); err == nil || !strings.Contains(err.Error(), "pane not found") {
		t.Fatalf("err = %v", err)
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeFailed {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
}

func TestDeliveryHeldIsQueuedAndDoesNotRetry(t *testing.T) {
	dir := t.TempDir()
	logPath := filepath.Join(dir, "args")
	writeConfig(t, `{"notifyTemplate":"true"}`)
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' \"$@\" >> '"+logPath+"'\nprintf '%s\\n' 'm-1 held: busy'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "agentbus:ops/x", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeQueued || got.NotifyError != "" {
		t.Fatalf("delivery = %+v notifyError = %q", got.Delivery, got.NotifyError)
	}
	if err := led.RetryDue(clock.Add(time.Hour), nil); err != nil {
		t.Fatal(err)
	}
	if again := mustGet(t, led, item.ID); len(again.Delivery) != 1 {
		t.Fatalf("retried a queued delivery: %+v", again.Delivery)
	}
	text := readText(t, logPath)
	key := DeliveryKey(&got)
	if !strings.Contains(text, "--client-id\n"+key+"\n") {
		t.Fatalf("agentbus args = %q, want client id %q", text, key)
	}
}

func TestDeliveryRetryCap(t *testing.T) {
	dir := t.TempDir()
	logPath := filepath.Join(dir, "keys")
	writeConfig(t, `{"notifyTemplate":"notifier"}`)
	installProgram(t, "notifier", "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\nexit 1\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "pane-9", "")
	if _, err := led.DeliverAnswer(&item, nil); err == nil {
		t.Fatal("first attempt succeeded")
	}
	clock = clock.Add(DeliveryBackoff(1))
	if err := led.RetryDue(clock, nil); err == nil {
		t.Fatal("second attempt succeeded")
	}
	clock = clock.Add(DeliveryBackoff(2))
	if err := led.RetryDue(clock, nil); err == nil {
		t.Fatal("third attempt succeeded")
	}
	if keys := readLines(t, logPath); len(keys) != MaxDeliveryAttempts {
		t.Fatalf("attempts = %d, want %d", len(keys), MaxDeliveryAttempts)
	}
	clock = clock.Add(time.Hour)
	if err := led.RetryDue(clock, nil); err != nil {
		t.Fatal(err)
	}
	if keys := readLines(t, logPath); len(keys) != MaxDeliveryAttempts {
		t.Fatalf("attempts after cap = %d", len(keys))
	}
}

func TestDeliveryKeyChangesForANewAnswer(t *testing.T) {
	dir := t.TempDir()
	logPath := filepath.Join(dir, "keys")
	writeConfig(t, `{"notifyTemplate":"notifier"}`)
	installProgram(t, "notifier", "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\nexit 1\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "pane-9", "")
	if _, err := led.DeliverAnswer(&item, nil); err == nil {
		t.Fatal(err)
	}
	first := DeliveryKey(&item)
	clock = clock.Add(time.Minute)
	again, err := led.Answer(item.ID, "b", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.DeliverAnswer(&again, nil); err == nil {
		t.Fatal(err)
	}
	second := DeliveryKey(&again)
	if first == "" || second == "" || first == second {
		t.Fatalf("keys = %q and %q", first, second)
	}
	keys := readLines(t, logPath)
	if len(keys) != 2 || keys[0] != first || keys[1] != second {
		t.Fatalf("keys = %#v, want %q then %q", keys, first, second)
	}
}

func TestDeliveryRemotePaneCarriesTheKey(t *testing.T) {
	dir := t.TempDir()
	logPath := filepath.Join(dir, "herdr")
	writeConfig(t, `{"notifyTemplate":"true","sendTemplate":"herdr agent prompt {pane}"}`)
	installProgram(t, "herdr", "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\nprintf '%s\\n' \"$@\" >> '"+logPath+"'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "p3", "m2")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeDelivered || got.Delivery[0].Target != "p3@m2" {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
	text := readText(t, logPath)
	key := DeliveryKey(&got)
	if !strings.Contains(text, key) || !strings.Contains(text, "--machine\nm2\n") {
		t.Fatalf("herdr log = %q", text)
	}
}

func openDeliveryLedger(t *testing.T, dir string, clock *time.Time) *Ledger {
	t.Helper()
	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return *clock })
	return led
}

func answeredRelay(t *testing.T, led *Ledger, pane, machine string) Item {
	t.Helper()
	item, err := led.Create(&Item{
		Question:      "ship",
		Owner:         "ada",
		Kind:          KindRelay,
		NotifyPane:    pane,
		NotifyMachine: machine,
		Options:       []Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	item, err = led.Answer(item.ID, "a", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	return item
}

func mustGet(t *testing.T, led *Ledger, id string) Item {
	t.Helper()
	got, err := led.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func readLines(t *testing.T, path string) []string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return strings.Split(strings.TrimSpace(string(data)), "\n")
}
