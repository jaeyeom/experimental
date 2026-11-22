package main

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestLogSearchAndSupersedeCLI(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("HERDR_PANE_ID", "")
	for _, q := range []string{"Use postgres for billing?", "Use redis for cache?"} {
		_, stderr, code := runCLI("ask", "--class", "decision", "--owner", "ada", "--kind", "relay",
			"--option", "yes:Yes", "--option", "no:No", "--rec", "yes", q)
		if code != 0 {
			t.Fatalf("ask = %d\n%s", code, stderr)
		}
	}
	for _, id := range []string{"1", "2"} {
		if _, stderr, code := runCLI("answer", id, "yes"); code != 0 {
			t.Fatalf("answer = %d\n%s", code, stderr)
		}
	}
	out, stderr, code := runCLI("log", "search", "postgres")
	if code != 0 || !strings.Contains(out, "D-001") || strings.Contains(out, "D-002") || !strings.Contains(out, "yes") {
		t.Fatalf("search = %d\n%s\n%s", code, out, stderr)
	}
	if _, stderr, code := runCLI("supersede", "1", "--by", "2"); code != 0 {
		t.Fatalf("supersede = %d\n%s", code, stderr)
	}
	out, _, _ = runCLI("log", "search", "use")
	if strings.Contains(out, "D-001\t") || !strings.Contains(out, "D-002") || !strings.Contains(out, "supersedes D-001") {
		t.Fatalf("default search:\n%s", out)
	}
	out, _, _ = runCLI("log", "search", "--all", "--json", "use")
	var items []struct {
		ID           string `json:"id"`
		Supersedes   string `json:"supersedes"`
		SupersededBy string `json:"supersededBy"`
	}
	if err := json.Unmarshal([]byte(out), &items); err != nil {
		t.Fatalf("json: %v\n%s", err, out)
	}
	if len(items) != 2 || items[0].ID != "D-002" || items[0].Supersedes != "D-001" || items[1].SupersededBy != "D-002" {
		t.Fatalf("items = %+v", items)
	}
	if _, _, code := runCLI("log", "search", "--class", "bogus"); code != 2 {
		t.Fatalf("bogus class code = %d", code)
	}
	if _, _, code := runCLI("supersede", "1"); code != 2 {
		t.Fatalf("supersede without --by code = %d", code)
	}
}
