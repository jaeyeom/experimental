package reviewpush_test

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestReviewpushReadmeOrder(t *testing.T) {
	doc := readReviewpushReadme(t)
	lines := strings.Split(doc, "\n")
	if len(lines) < 5 {
		t.Fatalf("README has %d lines", len(lines))
	}
	head := strings.Join(lines[:5], "\n")
	for _, want := range []string{"review-and-push-loop", "push", "Codex CLI"} {
		if !strings.Contains(head, want) {
			t.Errorf("first five lines lack %q:\n%s", want, head)
		}
	}
	if strings.Contains(head, "super power") {
		t.Error("quote still leads the README")
	}

	prereq := section(t, doc, "## Prerequisites")
	for _, want := range []string{"codex", "authenticated", "origin", "push", "CODEX_BIN", "--reviewer-cmd", "REVIEWPUSH_REVIEWER_CMD"} {
		if !strings.Contains(prereq, want) {
			t.Errorf("prerequisites lack %q", want)
		}
	}

	what := strings.Index(doc, "\n## What it does\n")
	install := strings.Index(doc, "go install github.com/jaeyeom/experimental/devtools/reviewpush/cmd/review-and-push-loop@latest")
	firstRun := strings.Index(doc, "review-and-push-loop --max-iterations 1")
	if what < 0 || install < 0 || firstRun < 0 || install > what || firstRun > what {
		t.Fatal("install and the first command must appear before the workflow diagrams")
	}
	if strings.Contains(doc[:what], "Claude Code writes commits locally") {
		t.Error("workflow diagram appears before the first command")
	}

	if strings.Contains(doc, "\n## Flags\n") || strings.Contains(doc, "\n## Environment variables\n") {
		t.Error("flags and environment variables are still separate sections")
	}
	options := section(t, doc, "## Options")
	for _, want := range []string{
		"--repo", "--base", "--max-iterations", "--output", "--json", "--reviewer-cmd",
		"GIT_BIN", "REVIEWPUSH_REVIEWER_CMD", "REVIEWPUSH_TIMEOUT_SECONDS", "CODEX_BIN", "CODEX_TIMEOUT_SECONDS",
		"`100`", "`human`", "`git`", "`codex`", "`300`",
	} {
		if !strings.Contains(options, want) {
			t.Errorf("options table lacks %q", want)
		}
	}

	for _, want := range []string{"TARGET COMMIT", "Pushed through", "status=PUSH", "========================================", `"pushed_commits"`} {
		if !strings.Contains(doc, want) {
			t.Errorf("sample output lacks %q", want)
		}
	}
	if !strings.Contains(doc, "overview.jpg") {
		t.Error("overview image is no longer referenced")
	}
	if !strings.Contains(doc, "//devtools/reviewpush/cmd/review-and-push-loop") {
		t.Error("Bazel target is missing")
	}

	arch := section(t, doc, "## Architecture & Factory Integration")
	for _, want := range []string{"factory", `[output] push_cmd`, "verify", "settle"} {
		if !strings.Contains(arch, want) {
			t.Errorf("architecture section lacks %q", want)
		}
	}

	internals := strings.Index(doc, "\n## Internals\n")
	safety := strings.Index(doc, "\n## Safety properties\n")
	if internals < 0 || safety < 0 || safety > internals {
		t.Fatal("safety properties must stay ahead of internals")
	}
	if strings.Contains(doc[safety:internals], "push_target") || strings.Contains(doc[safety:internals], "multi_commits") {
		t.Error("safety properties use the JSON field names before they are defined")
	}
	contract := strings.Index(doc, "\n### Reviewer response contract\n")
	if contract < internals {
		t.Fatal("Reviewer JSON contract must sit under internals")
	}
	if strings.Index(doc, "multi_commits") < internals {
		t.Error("multi_commits is used before internals")
	}
	if !strings.Contains(doc[contract:], `"status": "PUSH"`) || !strings.Contains(doc[contract:], `"blocked_reason"`) {
		t.Error("internals are missing the JSON contract")
	}
	if !strings.Contains(doc[internals:], "codex exec --help") {
		t.Error("internals are missing the Codex flag probe")
	}
	if !strings.Contains(doc[internals:], "REVIEWPUSH_SCHEMA_FILE") {
		t.Error("internals are missing custom reviewer env contract")
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
	if next := strings.Index(rest, "\n## "); next >= 0 {
		return rest[:next]
	}
	return rest
}

func readReviewpushReadme(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "README.md"))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "reviewpush", "README.md"))
	}
	for _, path := range candidates {
		body, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		return string(body)
	}
	t.Fatal("README.md not found")
	return ""
}
