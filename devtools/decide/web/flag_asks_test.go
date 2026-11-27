package web

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestWebReasonsAndFlagEndpoint(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{
		Owner:    "worker/alice",
		Question: "Pick it cleanly.",
		Options: []decide.Option{
			{Key: "a", Label: "Do it"},
			{Key: "b", Label: "Skip it"},
		},
	})

	// GET /api/v1/reasons loads default catalog including escalation.
	res := a.wantOK(a.do("GET", "/api/v1/reasons", ""), 200)
	reasons, _ := res["reasons"].([]any)
	if len(reasons) != len(decide.DefaultReasons()) {
		t.Fatalf("reasons count = %d, want %d", len(reasons), len(decide.DefaultReasons()))
	}
	var hasEscalation bool
	for _, raw := range reasons {
		m, _ := raw.(map[string]any)
		if m["id"] == decide.ReasonAlreadyDecided && m["category"] == "escalation" && m["allowEarlierItem"] == true {
			hasEscalation = true
		}
	}
	if !hasEscalation {
		t.Fatalf("reasons missing escalation reason: %v", reasons)
	}

	// Custom reasons in config override defaults.
	xdg := os.Getenv("XDG_CONFIG_HOME")
	cfgPath := filepath.Join(xdg, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, []byte(`{"reasons":[{"id":"custom-only","category":"format","label":"custom label"}]}`), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Remove(cfgPath) })

	customRes := a.wantOK(a.do("GET", "/api/v1/reasons", ""), 200)
	customReasons, _ := customRes["reasons"].([]any)
	if len(customReasons) != 1 || customReasons[0].(map[string]any)["id"] != "custom-only" {
		t.Fatalf("custom reasons = %v", customRes["reasons"])
	}

	// Restore default reasons for flagging check with "other" + custom rule test.
	_ = os.Remove(cfgPath)

	// Missing reasons fails 400.
	a.wantErr(a.post("/api/v1/items/"+it.ID+"/flag", `{}`), 400)
	// "other" without note fails 400.
	a.wantErr(a.post("/api/v1/items/"+it.ID+"/flag", `{"reasons":["other"]}`), 400)
	// Unknown item fails 404.
	a.wantErr(a.post("/api/v1/items/D-999/flag", `{"reasons":["vague-pronoun"]}`), 404)

	// Valid flag with "other" + note succeeds 200 and appends event.
	flagRes := a.wantOK(a.post("/api/v1/items/"+it.ID+"/flag", `{"messageIndex":0,"reasons":["vague-pronoun","other"],"note":"what does it mean?"}`), 200)
	evMap, _ := flagRes["event"].(map[string]any)
	if evMap["kind"] != decide.EventFlag || evMap["sendingAgent"] != "worker/alice" || evMap["note"] != "what does it mean?" {
		t.Fatalf("flag event = %v", evMap)
	}

	events, err := a.open().ListRawEvents()
	if err != nil || len(events) != 1 || events[0].Kind != decide.EventFlag {
		t.Fatalf("stored events = %+v, err=%v", events, err)
	}

	// Reject earlierItemId on non-escalation reason.
	a.wantErr(a.post("/api/v1/items/"+it.ID+"/flag", `{"reasons":["vague-pronoun"],"earlierItemId":"D-001"}`), 400)

	// Escalation flag stores optional earlierItemId.
	escRes := a.wantOK(a.post("/api/v1/items/"+it.ID+"/flag", `{"reasons":["already-decided"],"earlierItemId":"D-001"}`), 200)
	escMap, _ := escRes["event"].(map[string]any)
	if escMap["earlierItemId"] != it.ID {
		t.Fatalf("escalation flag event = %v", escMap)
	}

	// Thumbs-up endpoint writes good event with or without note; 404 on unknown item.
	a.wantErr(a.post("/api/v1/items/D-999/good", `{}`), 404)
	goodRes := a.wantOK(a.post("/api/v1/items/"+it.ID+"/good", `{"messageIndex":0,"note":"helpful options"}`), 200)
	goodMap, _ := goodRes["event"].(map[string]any)
	if goodMap["kind"] != decide.EventGood || goodMap["sendingAgent"] != "worker/alice" || goodMap["note"] != "helpful options" {
		t.Fatalf("good event = %v", goodMap)
	}
	goodEmpty := a.wantOK(a.post("/api/v1/items/"+it.ID+"/good", `{}`), 200)
	if goodEmpty["event"].(map[string]any)["kind"] != decide.EventGood {
		t.Fatalf("good event without note = %v", goodEmpty)
	}

	events, err = a.open().ListRawEvents()
	if err != nil || len(events) != 4 || events[2].Kind != decide.EventGood || events[3].Kind != decide.EventGood {
		t.Fatalf("stored events after good = %+v, err=%v", events, err)
	}
}

func TestWebAsksDisabledAndEnabledAndSSE(t *testing.T) {
	a, srv := newSSE(t)
	it := a.create(&decide.Item{
		Owner:    "worker/alice",
		Question: "Check PR?",
		Options: []decide.Option{
			{Key: "a", Label: "Ask Bob", Ask: "Bob: review PR #42", AskLinks: []string{"https://example.com/pr/42"}},
			{Key: "b", Label: "Wait"},
		},
	})

	// When asks disabled: choice event logged, no asks created, /api/v1/asks reports disabled, done refuses.
	a.wantOK(a.post("/api/v1/items/"+it.ID+"/answer", `{"value":"a"}`), 200)
	disabledBody := a.wantOK(a.do("GET", "/api/v1/asks", ""), 200)
	if disabledBody["enabled"] != false {
		t.Fatalf("enabled = %v, want false", disabledBody["enabled"])
	}
	if asks, _ := disabledBody["asks"].([]any); len(asks) != 0 {
		t.Fatalf("asks when disabled = %v", asks)
	}
	a.wantErr(a.post("/api/v1/asks/ASK-001/done", `{"reason":"done"}`), 400)

	// Enable asks in config and create a second item -> creates ask and streams over SSE!
	xdg := os.Getenv("XDG_CONFIG_HOME")
	cfgPath := filepath.Join(xdg, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, []byte(`{"asks":{"enabled":true}}`), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Remove(cfgPath) })

	c := openSSE(t, srv, "/api/v1/events", nil)
	resync := c.next(true)
	if resync.event != "resync" || resync.data["asksEnabled"] != true {
		t.Fatalf("resync = %+v", resync)
	}

	it2 := a.create(&decide.Item{
		Owner:    "worker/bob",
		Question: "Notify Carol?",
		Options: []decide.Option{
			{Key: "a", Label: "Ask Carol", Ask: "Carol: sign off"},
		},
	})
	a.wantOK(a.post("/api/v1/items/"+it2.ID+"/answer", `{"value":"a"}`), 200)

	// Expect delta carrying updated asks.
	var sawAsk bool
	deadline := time.Now().Add(3 * time.Second)
	for time.Now().Before(deadline) {
		f := c.next(true)
		if asks, ok := f.data["asks"].([]any); ok && len(asks) == 1 {
			sawAsk = true
			break
		}
	}
	if !sawAsk {
		t.Fatal("did not receive asks over SSE after ask-marked choice answer")
	}

	enabledBody := a.wantOK(a.do("GET", "/api/v1/asks", ""), 200)
	if enabledBody["enabled"] != true {
		t.Fatalf("enabled = %v, want true", enabledBody["enabled"])
	}
	asks, _ := enabledBody["asks"].([]any)
	if len(asks) != 1 || asks[0].(map[string]any)["id"] != "ASK-001" {
		t.Fatalf("asks = %v", enabledBody["asks"])
	}

	// Empty reason refused 400; missing ask 404.
	a.wantErr(a.post("/api/v1/asks/ASK-001/done", `{"reason":"  "}`), 400)
	a.wantErr(a.post("/api/v1/asks/ASK-999/done", `{"reason":"ok"}`), 404)

	// Mark done succeeds 200 and appends system message on original thread.
	doneRes := a.wantOK(a.post("/api/v1/asks/ASK-001/done", `{"reason":"Carol signed off"}`), 200)
	askMap, _ := doneRes["ask"].(map[string]any)
	if askMap["status"] != decide.AskStatusDone || askMap["closeReason"] != "Carol signed off" {
		t.Fatalf("closed ask = %v", askMap)
	}
	// Closing again returns 409 conflict.
	a.wantErr(a.post("/api/v1/asks/ASK-001/drop", `{"reason":"dup"}`), 409)

	thread := a.get(it2)
	if len(thread.Messages) == 0 || !strings.Contains(thread.Messages[len(thread.Messages)-1].Text, "ASK-001") {
		t.Fatalf("expected thread system note for ASK-001, got %+v", thread.Messages)
	}
}
