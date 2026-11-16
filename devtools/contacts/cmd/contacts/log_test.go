package main

import (
	"strings"
	"testing"
)

func TestEveryCommandWritesOneLogRow(t *testing.T) {
	ta := newTestApp(t)
	ta.env["CONTACTS_CALLER"] = "env-agent"
	ta.mustRun("add", "Ada", "--handle", "chat:ada", "--by", "ops")
	ta.mustRun("add", "Bob")
	ta.mustRun("edit", "ada", "--team", "core", "--by", "ops")
	ta.mustRun("resolve", "@ada", "--reason", "who is this", "--by", "inbox")
	ta.mustRun("merge", "ada", "bob", "--by", "ops")
	ta.mustRun("delete", "ada", "--yes", "--by", "ops")
	ta.mustRun("log")
	ta.mustRun("log", "--json")

	out := ta.mustRun("log")
	want := []string{
		"ops\tdelete\tada\t",
		"ops\tmerge\tada\t",
		"inbox\tresolve\tada\twho is this",
		"ops\tedit\tada\t",
		"env-agent\tadd\tbob\t",
		"ops\tadd\tada\t",
	}
	lines := strings.Split(strings.TrimSuffix(out, "\n"), "\n")
	if len(lines) != len(want) {
		t.Fatalf("log has %d rows, want %d (log must not log itself):\n%s", len(lines), len(want), out)
	}
	for i, w := range want {
		if !strings.HasSuffix(lines[i], w) {
			t.Errorf("row %d = %q, want suffix %q", i, lines[i], w)
		}
	}
}

func TestLogFiltersGolden(t *testing.T) {
	ta := newTestApp(t)
	ta.mustRun("add", "Ada", "--by", "ops")
	ta.mustRun("add", "Bob", "--by", "inbox")
	ta.mustRun("resolve", "bob", "--reason", "prep", "--by", "ops")
	// A deleted person's id still filters.
	ta.mustRun("delete", "bob", "--yes", "--by", "ops")
	ta.golden("log_person_bob.json", ta.mustRun("log", "--person", "bob", "--json"))
	if out := ta.mustRun("log", "--caller", "inbox"); strings.Count(out, "\n") != 1 {
		t.Errorf("--caller inbox:\n%s", out)
	}
	if out := ta.mustRun("log", "--since", "2026-03-11"); out != "" {
		t.Errorf("--since after the fixed clock:\n%s", out)
	}
	if code, _, _ := ta.run("log", "--since", "yesterday"); code != exitUsage {
		t.Errorf("bad --since: exit %d", code)
	}
}
