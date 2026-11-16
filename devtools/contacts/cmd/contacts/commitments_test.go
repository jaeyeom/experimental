package main

import (
	"strings"
	"testing"
)

func TestCommitmentsCLI(t *testing.T) {
	ta := newTestApp(t) // today is 2026-03-10
	ta.mustRun("add", "Ada")
	ta.mustRun("add", "Bob")
	if id := ta.mustRun("commit", "ada", "--direction", "mine", "--text", "send the draft", "--due", "2026-03-09",
		"--link", "https://chat.example/x/2", "--by", "ops"); id != "1\n" {
		t.Fatalf("commit printed %q", id)
	}
	ta.mustRun("commit", "ada", "--direction", "theirs", "--text", "review the API", "--due", "2026-03-12", "--by", "ops")
	ta.mustRun("commit", "bob", "--direction", "theirs", "--text", "share the logs", "--by", "ops")
	if code, _, _ := ta.run("commit", "bob", "--direction", "both", "--text", "x", "--by", "ops"); code != exitUsage {
		t.Errorf("bad direction: exit %d", code)
	}

	if out := ta.mustRun("close", "3", "--status", "done", "--note", "got them"); out != "3 done\n" {
		t.Errorf("close printed %q", out)
	}
	code, _, errOut := ta.run("close", "3", "--status", "dropped")
	if code != exitError || !strings.Contains(errOut, "already done") {
		t.Errorf("re-close: exit %d, %s", code, errOut)
	}

	ta.golden("commitments_all.json", ta.mustRun("commitments", "--json"))
	ta.golden("commitments_all.txt", ta.mustRun("commitments"))
	for args, want := range map[string]int{
		"commitments ada":        2,
		"commitments bob":        1,
		"commitments --open":     2,
		"commitments --overdue":  1,
		"commitments bob --open": 0,
	} {
		out := ta.mustRun(strings.Fields(args)...)
		if got := strings.Count(out, "\n"); got != want {
			t.Errorf("%s: %d lines, want %d:\n%s", args, got, want, out)
		}
	}
	if out := ta.mustRun("commitments", "--overdue"); !strings.Contains(out, "OVERDUE") {
		t.Errorf("overdue not flagged:\n%s", out)
	}
}
