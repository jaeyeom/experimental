package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func historyFixture(t *testing.T) *testApp {
	t.Helper()
	ta := newTestApp(t)
	ta.mustRun("add", "Ada", "--handle", "chat:ada.l")
	for _, e := range [][]string{
		{"2026-01-05", "https://chat.example/x/1", "kickoff for the API change"},
		{"2026-02-10", "https://chat.example/x/2", "agreed to split the API change"},
		{"2026-03-01", "https://mail.example/m/3", "sent the review notes"},
	} {
		ta.mustRun("report", "ada", "--channel", "chat", "--date", e[0], "--link", e[1], "--summary", e[2], "--by", "ops/inbox")
	}
	ta.mustRun("commit", "ada", "--direction", "theirs", "--text", "review the API", "--due", "2026-03-12", "--by", "ops")
	if err := os.WriteFile(filepath.Join(ta.dataDir, "people", "ada.md"), []byte("PRIVATE NOTES\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	return ta
}

func TestHistory(t *testing.T) {
	ta := historyFixture(t)
	out := ta.mustRun("history", "@ada.l", "--reason", "prep for review")
	ta.golden("history.txt", out)
	if strings.Contains(out, "PRIVATE NOTES") {
		t.Error("history printed the notes file contents")
	}
	ta.golden("history.json", ta.mustRun("history", "ada", "--reason", "prep", "--json"))

	for args, want := range map[string]int{
		"--topic api":                           2,
		"--since 2026-02-01 --until 2026-02-28": 1,
		"--limit 1":                             1,
	} {
		full := append([]string{"history", "ada", "--reason", "r", "--no-commitments"}, strings.Fields(args)...)
		out := ta.mustRun(full...)
		lines := strings.Split(strings.TrimSpace(out), "\n")
		events := 0
		for _, l := range lines {
			if strings.HasPrefix(l, "2026-") {
				events++
			}
		}
		if events != want || strings.Contains(out, "open commitments") {
			t.Errorf("%s: %d events, want %d:\n%s", args, events, want, out)
		}
	}
	if code, _, errOut := ta.run("history", "ada"); code != exitUsage || !strings.Contains(errOut, "--reason") {
		t.Errorf("missing --reason: exit %d, %s", code, errOut)
	}
}

func TestReadCommandsWriteOneLogRow(t *testing.T) {
	ta := historyFixture(t)
	before := strings.Count(ta.mustRun("log"), "\n")
	ta.mustRun("history", "ada", "--reason", "prep", "--by", "inbox")
	ta.run("history", "ada") // usage error: nothing read, nothing logged
	ta.mustRun("commitments", "ada", "--reason", "follow up", "--by", "inbox")
	out := ta.mustRun("log")
	if got := strings.Count(out, "\n") - before; got != 2 {
		t.Fatalf("new log rows = %d, want 2:\n%s", got, out)
	}
	lines := strings.Split(out, "\n")
	if !strings.HasSuffix(lines[0], "inbox\tcommitments\tada\tfollow up") ||
		!strings.HasSuffix(lines[1], "inbox\thistory\tada\tprep") {
		t.Errorf("log rows:\n%s", out)
	}
}
