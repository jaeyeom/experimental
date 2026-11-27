package decide

import (
	"strings"
	"testing"
	"time"
)

func TestAsksDisabledCreatesEventLogButNoAskRecords(t *testing.T) {
	writeConfig(t, `{"asks":{"enabled":false}}`)
	led, _ := openTemp(t)
	now := time.Date(2026, 11, 26, 14, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	it, err := led.Create(&Item{
		Question: "Proceed with Alice?",
		Kind:     KindRelay,
		Owner:    "ada",
		Options: []Option{
			{Key: "a", Label: "Ask Alice to merge", Recommended: true, Ask: "Alice: merge release PR", AskLinks: []string{"https://github.com/org/repo/pull/1"}},
			{Key: "b", Label: "Wait"},
		},
	})
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	if _, err := led.Answer(it.ID, "a", ViaUI); err != nil {
		t.Fatalf("Answer: %v", err)
	}
	raw, err := led.ListRawEvents()
	if err != nil {
		t.Fatal(err)
	}
	if len(raw) != 1 || raw[0].Kind != EventChoice {
		t.Fatalf("events = %+v, want single choice event", raw)
	}
	asks, err := led.ListAsks()
	if err != nil {
		t.Fatal(err)
	}
	if len(asks) != 0 {
		t.Fatalf("asks = %+v, want 0 when disabled", asks)
	}
	if _, err := led.AddAsk(it.ID, "Bob: check CI", nil, "", ViaCLI); err == nil {
		t.Fatal("expected AddAsk to fail when asks disabled")
	}
}

func TestAsksEnabledLifecycleSortingAndThreadNote(t *testing.T) {
	writeConfig(t, `{"asks":{"enabled":true}}`)
	led, _ := openTemp(t)
	t0 := time.Date(2026, 11, 26, 15, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return t0 })

	it1, err := led.Create(&Item{
		Question: "Ping someone?",
		Kind:     KindRelay,
		Owner:    "ada",
		Options: []Option{
			{Key: "a", Label: "Ask Alice", Recommended: true, Ask: "Alice: deploy staging", AskLinks: []string{"OPS-12"}},
			{Key: "b", Label: "Skip"},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(it1.ID, "a", ViaUI); err != nil {
		t.Fatal(err)
	}

	led.SetClock(func() time.Time { return t0.Add(2 * time.Minute) })
	it2, err := led.Create(&Item{
		Question: "Another check?",
		Kind:     KindRelay,
		Owner:    "bea",
		Options: []Option{
			{Key: "a", Label: "Yes", Recommended: true},
			{Key: "b", Label: "No"},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Compose(it2.ID, "Ask Carol instead", ViaUI, ""); err != nil {
		t.Fatal(err)
	}
	ask2, err := led.AddAsk(it2.ID, "Carol: review migration", []string{"https://chat.example/123"}, "", ViaCLI)
	if err != nil {
		t.Fatalf("AddAsk: %v", err)
	}
	if ask2.EventID == "" || ask2.Person != "Carol" || ask2.What != "review migration" {
		t.Fatalf("ask2 = %+v", ask2)
	}

	// Open asks sorted oldest first.
	asks, err := led.ListAsks()
	if err != nil {
		t.Fatal(err)
	}
	if len(asks) != 2 || asks[0].Person != "Alice" || asks[1].Person != "Carol" {
		t.Fatalf("sorted asks = %+v", asks)
	}

	// Non-owner agent cannot close Alice's ask.
	if _, err := led.CloseAsk(asks[0].ID, AskStatusDone, "nope", "eve", ViaCLI); err == nil {
		t.Fatal("expected non-owner close to fail")
	}

	// Owning agent marks first ask done; open ask remains first!
	led.SetClock(func() time.Time { return t0.Add(5 * time.Minute) })
	closed1, err := led.CloseAsk(asks[0].ID, AskStatusDone, "Alice finished staging deploy", "ada", ViaCLI)
	if err != nil {
		t.Fatalf("CloseAsk done: %v", err)
	}
	if closed1.Status != AskStatusDone || closed1.ClosedBy != "ada" {
		t.Fatalf("closed1 = %+v", closed1)
	}

	// Verify original thread received system note without marking unread.
	thread1, err := led.Get(it1.ID)
	if err != nil {
		t.Fatal(err)
	}
	lastMsg := thread1.Messages[len(thread1.Messages)-1]
	if lastMsg.Author != "system" || !strings.Contains(lastMsg.Text, asks[0].ID) || !strings.Contains(lastMsg.Text, "done") || !strings.Contains(lastMsg.Text, "Alice finished staging deploy") {
		t.Fatalf("thread1 system note = %+v", lastMsg)
	}

	// Human drops second ask from UI -> asks list still works and shows closed asks.
	led.SetClock(func() time.Time { return t0.Add(6 * time.Minute) })
	if _, err := led.CloseAsk(ask2.ID, AskStatusDropped, "no longer needed", "", ViaUI); err != nil {
		t.Fatalf("CloseAsk dropped: %v", err)
	}

	asks, err = led.ListAsks()
	if err != nil {
		t.Fatal(err)
	}
	if len(asks) != 2 || OpenAskCount(asks) != 0 {
		t.Fatalf("asks = %+v, open count = %d", asks, OpenAskCount(asks))
	}

	// Re-open check: add a third open ask and ensure it sorts BEFORE the two closed asks.
	led.SetClock(func() time.Time { return t0.Add(10 * time.Minute) })
	it3, err := led.Create(&Item{
		Question: "Third ask?",
		Kind:     KindRelay,
		Owner:    "ada",
		Options: []Option{
			{Key: "a", Label: "Ask Dan", Recommended: true, Ask: "Dan: sign off"},
			{Key: "b", Label: "Skip"},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(it3.ID, "a", ViaCLI); err != nil {
		t.Fatal(err)
	}
	asks, err = led.ListAsks()
	if err != nil {
		t.Fatal(err)
	}
	if len(asks) != 3 || asks[0].Person != "Dan" || asks[0].Status != AskStatusOpen {
		t.Fatalf("open-first order wrong: %+v", asks)
	}
}
