package ghask

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestGhAskReadmeOrder(t *testing.T) {
	doc := readGhAskReadme(t)
	lines := strings.Split(doc, "\n")
	if len(lines) < 10 {
		t.Fatalf("README has %d lines", len(lines))
	}
	head := strings.Join(lines[:10], "\n")
	for _, want := range []string{
		"Ask agent",
		"pull request",
		"question",
		"coding agent",
		"panel",
		"locally",
		"127.0.0.1",
	} {
		if !strings.Contains(head, want) {
			t.Errorf("first 10 lines lack %q", want)
		}
	}

	var prev int
	for _, heading := range []string{
		"* Setup",
		"* Try it without an agent",
		"* Using it on GitHub",
		"* Writing your own commands",
		"* Configuration",
		"* HTTP API",
		"* Security",
		"* Limitations",
		"* Development",
	} {
		idx := strings.Index(doc, "\n"+heading+"\n")
		if idx < 0 || idx < prev {
			t.Errorf("heading %q missing or out of order", heading)
		}
		prev = idx
	}

	setup := between(doc, "\n* Setup\n", "\n* Try it without an agent\n")
	orderIn(t, setup,
		"go install github.com/jaeyeom/experimental/devtools/gh-ask/cmd/gh-ask@latest",
		"gh-ask-inbox",
		"id_pattern",
		`reuse = "none"`,
		"gh-ask token",
		"gh-ask serve",
		"Violentmonkey",
	)

	tryIt := between(doc, "\n* Try it without an agent\n", "\n* Using it on GitHub\n")
	if !strings.Contains(tryIt, "curl") || !strings.Contains(tryIt, `{"id":`) {
		t.Error("try-it section needs a curl example and the id response")
	}
	if !strings.Contains(tryIt, "X-Gh-Ask-Token") {
		t.Error("try-it curl omits the token header")
	}

	agent := strings.Index(doc, "ask-agent")
	if agent < 0 || !strings.Contains(doc[agent:min(len(doc), agent+80)], "placeholder") {
		t.Error("ask-agent is not marked as a placeholder at its first mention")
	}

	threadDef := strings.Index(doc, "A thread is")
	deepDef := strings.Index(doc, "~deep~ is")
	followDef := strings.Index(doc, "A follow-up is")
	if threadDef < 0 || threadDef > strings.Index(doc, "threads.json") {
		t.Error("thread is not defined before threads.json")
	}
	if deepDef < 0 || deepDef > strings.Index(doc, "deep=") {
		t.Error("deep is not defined before deep=")
	}
	if followDef < 0 || followDef > strings.Index(doc, "followup_command") {
		t.Error("follow-up is not defined before followup_command")
	}

	http := strings.Index(doc, "\n* HTTP API\n")
	security := strings.Index(doc, "\n* Security\n")
	dev := strings.Index(doc, "\n* Development\n")
	if http < 0 || strings.Index(doc, "| 400") < http {
		t.Error("status-code table comes before the HTTP API section")
	}
	if security < 0 || strings.Index(doc, "constant time") < security {
		t.Error("security reference comes before the Security section")
	}
	for _, want := range []string{
		"node --test",
		"bazel test //devtools/gh-ask/userscript:userscript_test",
		"biome lint",
		"oxlint",
		"auth failed",
		"update the fixture",
	} {
		if strings.Index(doc, want) < dev {
			t.Errorf("%q is not under Development", want)
		}
	}
}

func orderIn(t *testing.T, section string, parts ...string) {
	t.Helper()
	prev := -1
	for _, part := range parts {
		idx := strings.Index(section, part)
		if idx < 0 || idx < prev {
			t.Errorf("setup order missing or early: %q", part)
		}
		prev = idx
	}
}

func between(doc, start, end string) string {
	i := strings.Index(doc, start)
	j := strings.Index(doc, end)
	if i < 0 || j < i {
		return ""
	}
	return doc[i:j]
}

func readGhAskReadme(t *testing.T) string {
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
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "gh-ask", "README.org"))
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
