package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func writeAskTestConfig(t *testing.T, content string) {
	t.Helper()
	cfgDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	path := filepath.Join(cfgDir, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
}

func TestAskWithAskOptionAndSubcommandsCLI(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("HERDR_PANE_ID", "")
	writeAskTestConfig(t, `{"asks":{"enabled":true}}`)

	// File choice question with --ask and --ask-link.
	out, stderr, code := runCLI(
		"ask", "--class", "decision", "--owner", "ada", "--kind", "relay",
		"--option", "a:Ask Alice to merge", "--option", "b:Hold",
		"--rec", "a",
		"--ask", "a=Alice: merge PR #42",
		"--ask-link", "a=https://github.com/org/repo/pull/42",
		"Should Alice merge PR #42?",
	)
	if code != 0 {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	itemID := strings.TrimSpace(out)

	// Answer option a -> opens ASK-001.
	if _, stderr, code := runCLI("answer", itemID, "a"); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
	out, stderr, code = runCLI("ask", "ls", "--json")
	if code != 0 {
		t.Fatalf("ask ls = %d\n%s", code, stderr)
	}
	var asks []decide.AskRecord
	if err := json.Unmarshal([]byte(out), &asks); err != nil || len(asks) != 1 {
		t.Fatalf("asks = %+v err=%v raw=%s", asks, err, out)
	}
	if asks[0].Person != "Alice" || asks[0].What != "merge PR #42" || asks[0].Status != decide.AskStatusOpen {
		t.Fatalf("ask[0] = %+v", asks[0])
	}
	if len(asks[0].Links) != 1 || asks[0].Links[0] != "https://github.com/org/repo/pull/42" {
		t.Fatalf("links = %+v", asks[0].Links)
	}

	// Show subcommand.
	out, stderr, code = runCLI("ask", "show", asks[0].ID)
	if code != 0 || !strings.Contains(out, "Alice") || !strings.Contains(out, "merge PR #42") {
		t.Fatalf("ask show = %d\n%s\n%s", code, out, stderr)
	}

	// Add ask on free-text answer item.
	out, stderr, code = runCLI("ask", "--class", "decision", "--owner", "bea", "--kind", "relay",
		"--option", "x:Yes", "--option", "y:No", "--rec", "x", "Ask someone else?")
	if code != 0 {
		t.Fatalf("ask 2 = %d\n%s", code, stderr)
	}
	item2 := strings.TrimSpace(out)
	if _, stderr, code := runCLI("send", "--id", item2, "--via", "ui", "Ask Bob instead"); code != 0 {
		t.Fatalf("send --id = %d\n%s", code, stderr)
	}
	out, stderr, code = runCLI("ask", "add", item2, "Bob: check CI", "--link", "CI-99")
	if code != 0 {
		t.Fatalf("ask add = %d\n%s", code, stderr)
	}
	ask2ID := strings.TrimSpace(out)

	// Close first ask as done by owning agent, second as dropped by human.
	if _, stderr, code := runCLI("ask", "done", asks[0].ID, "--by", "ada", "--reason", "Alice merged it"); code != 0 {
		t.Fatalf("ask done = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("ask", "drop", ask2ID, "--human", "--reason", "Resolved on call"); code != 0 {
		t.Fatalf("ask drop = %d\n%s", code, stderr)
	}

	// --open shows 0; plain shows both closed.
	out, stderr, code = runCLI("ask", "ls", "--open", "--json")
	if code != 0 {
		t.Fatalf("ask ls --open = %d\n%s", code, stderr)
	}
	if strings.TrimSpace(out) != "[]" {
		t.Fatalf("open asks = %s, want []", out)
	}

	// Passing -- "ls" still files an item with question "ls" (backwards-compatible dispatch).
	out, stderr, code = runCLI("ask", "--class", "note", "--owner", "ada", "--", "ls")
	if code != 0 || !strings.HasPrefix(strings.TrimSpace(out), "N-") {
		t.Fatalf("ask -- ls = %d out=%q err=%s", code, out, stderr)
	}
}
