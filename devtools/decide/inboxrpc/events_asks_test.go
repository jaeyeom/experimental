package inboxrpc

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestFlagReasonsAndAsksRoundTrip(t *testing.T) {
	cfgDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	cfgPath := filepath.Join(cfgDir, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, []byte(`{"asks":{"enabled":true},"reasons":[{"id":"vague-pronoun","category":"main-text","label":"Vague pronoun"},{"id":"other","category":"other","label":"Other"}]}`), 0o600); err != nil {
		t.Fatal(err)
	}

	h := newHub(t)
	h.server.Display = func() (decide.DisplaySettings, error) {
		f, err := decide.LoadConfigFile()
		if err != nil {
			return decide.DisplaySettings{}, fmt.Errorf("load config: %w", err)
		}
		return f.Display(), nil
	}
	it := h.create(t, &decide.Item{
		Question: "Pick reviewer?",
		Owner:    "ada",
		Options: []decide.Option{
			{Key: "a", Label: "Ask Alice", Recommended: true, Ask: "Alice: review PR", AskLinks: []string{"PR-9"}},
			{Key: "b", Label: "Skip"},
		},
	})
	c := connect(t, h)

	reasons, err := c.Reasons()
	if err != nil {
		t.Fatalf("Reasons: %v", err)
	}
	if len(reasons) != 2 || reasons[0].ID != "vague-pronoun" {
		t.Fatalf("reasons = %+v", reasons)
	}

	msgIdx := 0
	ev, err := c.Flag(it.ID, &msgIdx, []string{"vague-pronoun"}, "Unclear wording")
	if err != nil {
		t.Fatalf("Flag: %v", err)
	}
	if ev.Kind != decide.EventFlag || ev.SendingAgent != "ada" {
		t.Fatalf("flag event = %+v", ev)
	}

	enabled, err := c.AsksEnabled()
	if err != nil || !enabled {
		t.Fatalf("AsksEnabled = %v, %v", enabled, err)
	}
	if _, err := c.Answer(it.ID, "a"); err != nil {
		t.Fatalf("Answer: %v", err)
	}
	eventually(t, func() bool {
		asks, lerr := c.ListAsks()
		return lerr == nil && len(asks) == 1 && asks[0].Person == "Alice"
	}, "ask did not replicate via watch")

	asks, err := c.ListAsks()
	if err != nil {
		t.Fatal(err)
	}
	closed, err := c.CloseAsk(asks[0].ID, decide.AskStatusDone, "Alice reviewed PR")
	if err != nil {
		t.Fatalf("CloseAsk: %v", err)
	}
	if closed.Status != decide.AskStatusDone || closed.CloseReason != "Alice reviewed PR" {
		t.Fatalf("closed ask = %+v", closed)
	}
	updatedItem, err := c.Get(it.ID)
	if err != nil {
		t.Fatal(err)
	}
	if len(updatedItem.Messages) == 0 || updatedItem.Messages[len(updatedItem.Messages)-1].Author != "system" {
		t.Fatalf("item thread did not get system note: %+v", updatedItem.Messages)
	}
	_ = time.Second
}
