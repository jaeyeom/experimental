package memstore

import (
	"encoding/xml"
	"strings"
	"testing"
)

func TestRenderLaunchdPlist(t *testing.T) {
	raw, err := RenderLaunchdPlist(LaunchdPlistOptions{
		IntervalSeconds: 300,
		BinaryPath:      "/usr/local/bin/memstore",
	})
	if err != nil {
		t.Fatalf("RenderLaunchdPlist: %v", err)
	}
	text := string(raw)
	for _, want := range []string{
		"com.jaeyeom.memstore.tick",
		"<integer>300</integer>",
		"<string>/usr/local/bin/memstore</string>",
		"<string>routine</string>",
		"<string>tick</string>",
	} {
		if !strings.Contains(text, want) {
			t.Errorf("plist missing %q:\n%s", want, text)
		}
	}

	var doc any
	if err := xml.Unmarshal(raw, &doc); err != nil {
		t.Fatalf("invalid XML: %v\n%s", err, text)
	}

	if _, err := RenderLaunchdPlist(LaunchdPlistOptions{IntervalSeconds: 0}); err == nil {
		t.Error("expected error for non-positive interval")
	}
}
