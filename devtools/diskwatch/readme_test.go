package diskwatch

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestDiskwatchReadmeOrder(t *testing.T) {
	doc := readDiskwatchReadme(t)
	lines := strings.Split(doc, "\n")
	if len(lines) < 12 {
		t.Fatalf("README has %d lines", len(lines))
	}
	head := strings.Join(lines[:12], "\n")
	for _, want := range []string{"warns you", "timer", "never deletes", "once"} {
		if !strings.Contains(head, want) {
			t.Errorf("first screen lacks %q", want)
		}
	}
	for _, banned := range []string{"statfs", "inotify", "FSEvents", "Hysteresis"} {
		if strings.Contains(head, banned) {
			t.Errorf("first screen still discusses %q", banned)
		}
	}

	alerts := strings.Index(doc, "\n* When it alerts\n")
	if alerts < 0 {
		t.Fatal("missing When it alerts")
	}
	before := doc[:alerts]
	orderIn(t, before,
		"go install github.com/jaeyeom/experimental/devtools/diskwatch/cmd/diskwatch@latest",
		"config.toml",
		"diskwatch check --dry-run",
		"dry-run / low free",
	)
	if strings.Contains(before, "Hysteresis") {
		t.Error("alert rules still use the heading Hysteresis before they are explained")
	}
	if !strings.Contains(doc, "armed") || !strings.Contains(doc, "14%") || !strings.Contains(doc, "26%") {
		t.Error("armed is not explained with the low/high timeline")
	}
	for _, want := range []string{
		"~/.local/state/diskwatch/state.json",
		"Library/Application Support/diskwatch/state.json",
		"Library/Application Support/diskwatch/config.toml",
		"~/.config/diskwatch/config.toml",
	} {
		if !strings.Contains(doc, want) {
			t.Errorf("README lacks %q", want)
		}
	}
	file := strings.Index(doc, "DISKWATCH_SUMMARY_FILE")
	if file < 0 || !strings.Contains(doc[file:min(len(doc), file+160)], "Path to a JSON file") {
		t.Error("DISKWATCH_SUMMARY_FILE is not documented as a file path")
	}
	if !strings.Contains(doc, "./ensure.sh diskwatch") || !strings.Contains(doc, "not enabled unless a config file exists") {
		t.Error("README does not say how the periodic runner is enabled")
	}
	if !strings.Contains(doc, "armed ready last-trigger") {
		t.Error("README has no status sample")
	}
	if !strings.Contains(doc, "diskwatch: hook skipped") || !strings.Contains(doc, "hook does not take --dry-run") {
		t.Error("hook skip and dry-run rejection are missing")
	}
	for _, code := range []string{"| 0 ", "| 1 ", "| 2 "} {
		if !strings.Contains(doc, code) {
			t.Errorf("exit-code table lacks %q", code)
		}
	}
	if !strings.Contains(doc, "\n* Disk usage report\n") {
		t.Error("usage summary was not renamed")
	}
	if strings.Contains(doc, "* Usage summary") {
		t.Error("old Usage summary heading remains")
	}
}

func orderIn(t *testing.T, section string, parts ...string) {
	t.Helper()
	prev := -1
	for _, part := range parts {
		idx := strings.Index(section, part)
		if idx < 0 || idx < prev {
			t.Errorf("quick start missing or out of order: %q", part)
		}
		prev = idx
	}
}

func readDiskwatchReadme(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "README.org"))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "diskwatch", "README.org"))
	}
	for _, path := range candidates {
		body, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		return string(body)
	}
	t.Fatal("README.org not found")
	return ""
}
