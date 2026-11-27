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

func TestLogEventsMisfitCauseAndFlagsCLI(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("HERDR_PANE_ID", "")

	// Choice answer -> recorded as non-misfit choice event.
	out, stderr, code := runCLI("ask", "--class", "decision", "--owner", "ada", "--kind", "relay",
		"--option", "a:Postgres", "--option", "b:SQLite", "--rec", "a", "Which DB?")
	if code != 0 {
		t.Fatalf("ask 1 = %d %s", code, stderr)
	}
	id1 := strings.TrimSpace(out)
	if _, stderr, code := runCLI("answer", id1, "a"); code != 0 {
		t.Fatalf("answer 1 = %d %s", code, stderr)
	}

	// Free-text Reply (0) via send --id on open choice item -> recorded as misfit.
	out, stderr, code = runCLI("ask", "--class", "decision", "--owner", "bea", "--kind", "relay",
		"--option", "a:I push it", "--option", "b:You push it", "--rec", "a", "Who pushes?")
	if code != 0 {
		t.Fatalf("ask 2 = %d %s", code, stderr)
	}
	id2 := strings.TrimSpace(out)
	if _, stderr, code := runCLI("send", "--id", id2, "--via", "ui", "Hold off until review"); code != 0 {
		t.Fatalf("send --id = %d %s", code, stderr)
	}

	// List misfits.
	out, stderr, code = runCLI("log", "--misfit")
	if code != 0 || !strings.Contains(out, id2) || !strings.Contains(out, "options did not fit") || strings.Contains(out, id1) {
		t.Fatalf("log --misfit = %d\nout=%s\nerr=%s", code, out, stderr)
	}
	out, stderr, code = runCLI("log", "--misfit", "--group-by-agent")
	if code != 0 || !strings.Contains(out, "agent: bea (1)") || !strings.Contains(out, "I push it") {
		t.Fatalf("log --misfit --group-by-agent = %d\nout=%s\nerr=%s", code, out, stderr)
	}

	// Annotate cause onto EV-002.
	out, stderr, code = runCLI("log", "--misfit", "--json")
	if code != 0 {
		t.Fatalf("log --misfit --json = %d %s", code, stderr)
	}
	var misfits []struct {
		ID string `json:"id"`
	}
	if err := json.Unmarshal([]byte(out), &misfits); err != nil || len(misfits) != 1 {
		t.Fatalf("misfits json = %v %s", err, out)
	}
	if _, stderr, code := runCLI("log", "cause", misfits[0].ID, "actor-unclear", "--note", "Pronoun I/you ambiguous"); code != 0 {
		t.Fatalf("log cause = %d %s", code, stderr)
	}
	out, stderr, code = runCLI("log", "--misfit", "--cause", "actor-unclear")
	if code != 0 || !strings.Contains(out, "actor-unclear") || !strings.Contains(out, "Pronoun I/you ambiguous") {
		t.Fatalf("log --misfit --cause = %d\nout=%s\nerr=%s", code, out, stderr)
	}

	// Reject other without --note.
	if _, _, code := runCLI("log", "cause", misfits[0].ID, "other"); code != 2 {
		t.Fatalf("log cause other without note code = %d, want 2", code)
	}

	// Flag from ledger then list via decide log --flags.
	led, err := openLedger()
	if err != nil {
		t.Fatal(err)
	}
	msgIdx := 0
	if _, err := led.FlagItem(id1, &msgIdx, []string{"vague-pronoun"}, "Uses unclear it", "ui"); err != nil {
		t.Fatalf("FlagItem: %v", err)
	}
	out, stderr, code = runCLI("log", "--flags")
	if code != 0 || !strings.Contains(out, "agent: ada (1)") || !strings.Contains(out, "vague-pronoun") {
		t.Fatalf("log --flags = %d\nout=%s\nerr=%s", code, out, stderr)
	}
}
