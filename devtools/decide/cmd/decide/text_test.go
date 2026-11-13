package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeBody(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "msg.txt")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func withStdin(t *testing.T, body string) {
	t.Helper()
	prev := cliStdin
	cliStdin = strings.NewReader(body)
	t.Cleanup(func() { cliStdin = prev })
}

func useState(t *testing.T) {
	t.Helper()
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
}

func assertNoItems(t *testing.T) {
	t.Helper()
	stdout, stderr, code := runCLI("list")
	if code != 0 || strings.TrimSpace(stdout) != "" {
		t.Fatalf("list = %d %q\n%s", code, stdout, stderr)
	}
}

func TestAskTextFileKeepsInternalNewlines(t *testing.T) {
	useState(t)
	path := writeBody(t, "summary\n\n1. one\n2. two\n")
	args := relayArgs("placeholder")
	args[len(args)-1] = "--text-file"
	args = append(args, path)
	stdout, stderr, code := runCLI(args...)
	if code != 0 {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	if got := showJSON(t, strings.TrimSpace(stdout)).Question; got != "summary\n\n1. one\n2. two" {
		t.Fatalf("question = %q", got)
	}
}

func TestReplyStdinReadsTheBody(t *testing.T) {
	useState(t)
	id := askRelay(t, nil, "root")
	withStdin(t, "line1\nline2\n\n")
	if _, stderr, code := runCLI("reply", id, "-"); code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
	msgs := showJSON(t, id).Messages
	if len(msgs) == 0 || msgs[len(msgs)-1].Text != "line1\nline2" {
		t.Fatalf("messages = %+v", msgs)
	}
}

func TestTextFileWithPositionalIsUsage(t *testing.T) {
	useState(t)
	path := writeBody(t, "from file\n")
	args := append(relayArgs("also positional"), "--text-file", path)
	_, stderr, code := runCLI(args...)
	if code != 2 || !strings.Contains(stderr, "not more than one") {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	assertNoItems(t)
}

func TestEmptyTextFileIsAnError(t *testing.T) {
	useState(t)
	path := writeBody(t, "")
	args := relayArgs("")
	args[len(args)-1] = "--text-file"
	args = append(args, path)
	_, stderr, code := runCLI(args...)
	if code == 0 || !strings.Contains(stderr, "empty") {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	assertNoItems(t)
}

func TestDoneAndWithdrawReadTextFiles(t *testing.T) {
	useState(t)
	id := askRelay(t, nil, "root")
	outcome := writeBody(t, "shipped\nit\n")
	if _, stderr, code := runCLI("done", id, "--text-file", outcome); code != 0 {
		t.Fatalf("done = %d\n%s", code, stderr)
	}
	if got := showJSON(t, id).Outcome; got != "shipped\nit" {
		t.Fatalf("outcome = %q", got)
	}

	id = askRelay(t, nil, "other")
	reason := writeBody(t, "no longer needed\n")
	if _, stderr, code := runCLI("withdraw", "--text-file", reason, id); code != 0 {
		t.Fatalf("withdraw = %d\n%s", code, stderr)
	}
	if got := showJSON(t, id).Reason; got != "no longer needed" {
		t.Fatalf("reason = %q", got)
	}
}

func TestSendTextFileStartsTheThread(t *testing.T) {
	useState(t)
	dir := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := []byte(`{"targets":[{"name":"alpha","pane":"pane-a"}],"sendTemplate":"true {pane}"}` + "\n")
	if err := os.WriteFile(filepath.Join(dir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}
	path := writeBody(t, "hello\nthere\n")
	if _, stderr, code := runCLI("send", "--to", "alpha", "--text-file", path); code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	items := showThreads(t)
	if len(items) != 1 || items[0].Question != "hello\nthere" || items[0].Messages[0].Text != "hello\nthere" {
		t.Fatalf("threads = %+v", items)
	}
}

func TestStatusTextFileAndOptionalResourceNote(t *testing.T) {
	useState(t)
	path := writeBody(t, "fixing the inbox\n")
	stdout, stderr, code := runCLI("status", "set", "--agent", "ada", "--text-file", path)
	if code != 0 || stdout != "ada working fixing the inbox\n" {
		t.Fatalf("agent = %d %q\n%s", code, stdout, stderr)
	}
	withStdin(t, "still fixing\n")
	stdout, stderr, code = runCLI("status", "set", "--agent", "ada", "-")
	if code != 0 || stdout != "ada working still fixing\n" {
		t.Fatalf("stdin = %d %q\n%s", code, stdout, stderr)
	}

	stdout, stderr, code = runCLI("status", "set", "--resource", "workspace", "free")
	if code != 0 || stdout != "workspace: free\n" {
		t.Fatalf("omit note = %d %q\n%s", code, stdout, stderr)
	}
	note := writeBody(t, "paused\n")
	stdout, stderr, code = runCLI("status", "set", "--resource", "workspace", "busy", "--text-file", note)
	if code != 0 || stdout != "workspace: busy (paused)\n" {
		t.Fatalf("note = %d %q\n%s", code, stdout, stderr)
	}
	empty := writeBody(t, "")
	_, stderr, code = runCLI("status", "set", "--resource", "workspace", "free", "--text-file", empty)
	if code == 0 || !strings.Contains(stderr, "empty") {
		t.Fatalf("empty note = %d\n%s", code, stderr)
	}
}

func TestSkillShowsInboxWriting(t *testing.T) {
	stdout, stderr, code := runCLI("skill")
	if code != 0 {
		t.Fatalf("skill = %d\n%s", code, stderr)
	}
	for _, phrase := range []string{
		"Write for the inbox",
		"decide reply D-123 --text-file",
		"Your action:",
		"$(cat file)",
		"Ship the inbox fix",
	} {
		if !strings.Contains(stdout, phrase) {
			t.Fatalf("skill missing %q", phrase)
		}
	}
}

func TestUsageListsTextSources(t *testing.T) {
	_, stderr, code := runCLI()
	if code != 2 || !strings.Contains(stderr, "--text-file") || !strings.Contains(stderr, "stdin") {
		t.Fatalf("usage = %d\n%s", code, stderr)
	}
}

func TestHubTextFileIsReadLocally(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub"}`)
	path := writeBody(t, "hello\nworld\n")
	var stderr strings.Builder
	code := run([]string{"reply", "D-1", "--text-file", path}, &strings.Builder{}, &stderr)
	if code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr.String())
	}
	if got := fake.Read("decide.args"); got != "reply\nD-1\nhello\nworld\n" {
		t.Fatalf("hub args = %q", got)
	}
	if fake.Read("decide.stdin") != "" {
		t.Fatalf("stdin = %q", fake.Read("decide.stdin"))
	}
	assertNoLocalState(t, state)
}

func TestHubStdinIsReadLocally(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub"}`)
	withStdin(t, "line1\nline2\n\n")
	code := run([]string{"reply", "D-1", "-"}, &strings.Builder{}, &strings.Builder{})
	if code != 0 {
		t.Fatalf("reply = %d", code)
	}
	if got := fake.Read("decide.args"); got != "reply\nD-1\nline1\nline2\n" {
		t.Fatalf("hub args = %q", got)
	}
	assertNoLocalState(t, state)
}

func TestHubTextErrorDoesNotCallSSH(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub"}`)
	path := writeBody(t, "hello\n")
	_, stderr, code := runCLI("reply", "D-1", "--text-file", path, "also")
	if code != 2 || !strings.Contains(stderr, "not more than one") {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
	if fake.Read("decide.args") != "" || fake.Read("argv") != "" {
		t.Fatalf("ssh ran: args %q argv %q", fake.Read("decide.args"), fake.Read("argv"))
	}
	assertNoLocalState(t, state)
}
