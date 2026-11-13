package diskwatch

import (
	"strings"
	"testing"
	"time"
)

func TestStatusTextAndJSON(t *testing.T) {
	t.Parallel()
	cfg := percentMount()
	now := time.Date(2026, 11, 13, 2, 0, 0, 0, time.UTC)
	state := State{}
	state.put("/", MountState{
		Armed:       false,
		TriggeredAt: now.Add(-time.Hour),
		LastAction:  "ok",
	})
	rows := Status(cfg, state, now, scriptedStat{100}.stat)
	text, err := FormatStatus(rows, false)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(text, "free 10.00% (100/1000)") || !strings.Contains(text, "disarmed") || !strings.Contains(text, "cooldown") {
		t.Fatalf("text = %s", text)
	}
	encoded, err := FormatStatus(rows, true)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(encoded, `"freeBytes": 100`) || !strings.Contains(encoded, `"cooldown": true`) {
		t.Fatalf("json = %s", encoded)
	}
}
