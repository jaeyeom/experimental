package main

import (
	"strings"
	"testing"
)

func TestBackfillCLIProposesThenAppliesOnApproval(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("HERDR_PANE_ID", "")
	if _, stderr, code := runCLI("ask", "--class", "decision", "--owner", "ada", "--kind", "relay",
		"--option", "yes:Yes", "--option", "no:No", "--rec", "yes", "Post as written?"); code != 0 {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("ask", "--class", "decision", "--owner", "ada", "--kind", "relay",
		"--option", "a:A", "--option", "b:B", "--rec", "a", "Which db?"); code != 0 {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	t.Cleanup(func() { backfillIn = nil })

	// Declined: prints the batch and writes nothing.
	backfillIn = strings.NewReader("n\n")
	out, stderr, code := runCLI("backfill")
	if code != 0 || !strings.Contains(out, "D-001\tdecision -> approval") || strings.Contains(out, "D-002") {
		t.Fatalf("declined = %d\n%s\n%s", code, out, stderr)
	}
	if out, _, _ := runCLI("show", "1"); strings.Contains(out, "retagged") || !strings.Contains(out, "D-001") {
		t.Fatalf("declined backfill changed item:\n%s", out)
	}

	// Approved: retags.
	backfillIn = strings.NewReader("y\n")
	out, stderr, code = runCLI("backfill")
	if code != 0 || !strings.Contains(out, "retagged 1") {
		t.Fatalf("approved = %d\n%s\n%s", code, out, stderr)
	}
	if out, _, _ := runCLI("show", "1"); !strings.Contains(out, "A-001") {
		t.Fatalf("not retagged:\n%s", out)
	}
	// --yes skips the prompt; nothing left to do now.
	out, _, code = runCLI("backfill", "--yes")
	if code != 0 || !strings.Contains(out, "nothing to retag") {
		t.Fatalf("again = %d\n%s", code, out)
	}
}
