package claudeusage_test

import (
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

func TestClaudeUsageReadmeOrder(t *testing.T) {
	doc := readClaudeUsageReadme(t)
	lines := strings.Split(doc, "\n")
	if len(lines) < 12 {
		t.Fatalf("README has %d lines", len(lines))
	}
	head := strings.Join(lines[:12], "\n")
	for _, want := range []string{"5-hour", "7-day", "subscription", "API key", "--json"} {
		if !strings.Contains(head, want) {
			t.Errorf("first screen lacks %q", want)
		}
	}
	for _, banned := range []string{"claude-hud", ".local/bin", "port"} {
		if strings.Contains(head, banned) {
			t.Errorf("first screen still says %q", banned)
		}
	}

	usage := strings.Index(doc, "\n* Usage\n")
	prereq := strings.Index(doc, "\n* Prerequisites\n")
	install := strings.Index(doc, "\n* Installation\n")
	if prereq < 0 || usage < 0 || install < 0 || prereq > usage || install > usage {
		t.Fatal("prerequisites and installation must appear before usage")
	}
	if strings.Contains(strings.ToLower(doc), "legacy") {
		t.Error("README still calls a supported path legacy")
	}
	if !strings.Contains(doc, "Linux: reads =~/.claude/.credentials.json=") {
		t.Error("Linux credential path is not described")
	}
	if regexp.MustCompile(`#\d+`).MatchString(doc) {
		t.Error("README still has an unlinked issue number")
	}
	if strings.Contains(doc, "\n* Failure modes\n") {
		t.Error("failure modes are still a separate section")
	}
	exits := section(t, doc, "* Exit codes")
	for _, want := range []string{"Success", "429", "API-key", "401"} {
		if !strings.Contains(exits, want) {
			t.Errorf("exit table lacks %q", want)
		}
	}
	if strings.Index(doc, "\n* Security\n") < strings.Index(doc, "\n* Exit codes\n") {
		t.Error("security still precedes the exit codes")
	}
}

func section(t *testing.T, doc, heading string) string {
	t.Helper()
	start := strings.Index(doc, "\n"+heading+"\n")
	if start < 0 {
		t.Fatalf("missing heading %s", heading)
	}
	start += len("\n" + heading + "\n")
	rest := doc[start:]
	if next := strings.Index(rest, "\n* "); next >= 0 {
		return rest[:next]
	}
	return rest
}

func readClaudeUsageReadme(t *testing.T) string {
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
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "claude-usage", "README.org"))
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
