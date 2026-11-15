package docsync_test

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestDocsyncReadmeOrder(t *testing.T) {
	doc := readDocsyncReadme(t)
	lines := strings.Split(doc, "\n")
	if len(lines) < 12 {
		t.Fatalf("README has %d lines", len(lines))
	}
	head := strings.Join(lines[:12], "\n")
	for _, want := range []string{"out of date", "docsync.yml", "docsync check"} {
		if !strings.Contains(head, want) {
			t.Errorf("first screen lacks %q", want)
		}
	}
	for _, banned := range []string{"shadow", "matcher", "change set"} {
		if strings.Contains(head, banned) {
			t.Errorf("first screen still says %q", banned)
		}
	}

	commands := strings.Index(doc, "\n* Commands\n")
	quick := strings.Index(doc, "\n* Quick start\n")
	discovery := strings.Index(doc, "\n* Config discovery\n")
	if quick < 0 || commands < 0 || discovery < quick || discovery > commands {
		t.Fatal("config discovery must sit after the quick start and before the commands")
	}
	before := doc[quick:commands]
	orderIn(t, before,
		"go install github.com/jaeyeom/experimental/devtools/docsync/cmd/docsync@latest",
		"version: 1",
		"docsync check --base main",
		"Affected docs",
	)
	if strings.Contains(before, "| Flag |") {
		t.Error("flag table appears inside the quick start")
	}
	if strings.Contains(doc, "2026-08-28-docsync-design.md") {
		t.Error("README still links the missing design spec")
	}
	if strings.Contains(doc, "* Two-list maps") {
		t.Error("two-list migration section remains")
	}
	if !strings.Contains(doc, "An exact path is just a glob with no wildcards.") {
		t.Error("exact-path note is missing")
	}
	defined := strings.Index(doc, "The mapping root is the directory that contains")
	if defined < 0 || strings.Index(doc, "mapping root") != defined+len("The ") {
		t.Error("mapping root is used before it is defined")
	}
	nonGoals := strings.Index(doc, "\n* Non-goals\n")
	dev := strings.Index(doc, "\n* Development\n")
	if nonGoals < 0 || dev < 0 || nonGoals < dev {
		t.Fatal("non-goals must follow Development")
	}
	for _, phrase := range []string{"last_reviewed", "does not talk to the network", "does not draft"} {
		if strings.Index(doc, phrase) < nonGoals {
			t.Errorf("%q appears before Non-goals", phrase)
		}
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

func readDocsyncReadme(t *testing.T) string {
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
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "docsync", "README.org"))
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
