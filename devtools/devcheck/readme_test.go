package devcheck_test

import (
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

func TestDevcheckReadmeOrder(t *testing.T) {
	doc := readDevcheckReadme(t)
	overview := section(t, doc, "* Overview")
	for _, want := range []string{"--format=summary", "human-readable", "coding agent"} {
		if !strings.Contains(overview, want) {
			t.Errorf("overview lacks %q", want)
		}
	}
	if strings.Contains(doc, "PRD") || regexp.MustCompile(`#\d+`).MatchString(doc) {
		t.Error("README still has a PRD note or an unlinked issue number")
	}
	if strings.Contains(doc, "BUILD_WORKING_DIRECTORY") {
		t.Error("README still names BUILD_WORKING_DIRECTORY")
	}
	prompt := strings.Index(doc, "Next Steps for AI Agent:")
	summary := strings.Index(doc, "DevCheck: 3 passed, 0 failed")
	intro := strings.Index(doc, "written for an AI coding agent")
	if prompt < 0 || summary < 0 || intro < 0 || summary > prompt || intro > prompt {
		t.Fatal("the prompt example must follow the summary example and an agent introduction")
	}
	const link = "[[file:../linters/README.org][unnecessary-interface-assertion-linter]]"
	if !strings.Contains(doc, link) {
		t.Error("Go lint fallback does not link the linters README")
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

func readDevcheckReadme(t *testing.T) string {
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
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "devcheck", "README.org"))
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
