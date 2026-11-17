package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func useLineConfig(t *testing.T, body string) {
	t.Helper()
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	dir := filepath.Join(cfg, "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}

func TestAskRejectsALongLineAndStoresNothing(t *testing.T) {
	useState(t)
	long := "I marked it done too early: " + strings.Repeat("x", 200)
	_, stderr, code := runCLI(relayArgs("Short essence\n" + long)...)
	if code == 0 {
		t.Fatalf("ask with a long line exited 0")
	}
	assertNoItems(t)
	for _, want := range []string{
		"not sent: line 2 is 228 characters (limit 160)",
		`"I marked it done too early: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx…"`,
		"A person reads this in an inbox, often at a glance",
		"First line: the essence",
		"short bullet lines",
		"file or stdin",
		"--long-lines",
	} {
		if !strings.Contains(stderr, want) {
			t.Errorf("stderr missing %q:\n%s", want, stderr)
		}
	}
	if strings.Index(stderr, "--long-lines") < strings.Index(stderr, "file or stdin") {
		t.Errorf("--long-lines is not mentioned last:\n%s", stderr)
	}
}

func TestReplyRejectsALongLineAndStoresNothing(t *testing.T) {
	useState(t)
	id := askRelay(t, nil, "root")
	before := len(showJSON(t, id).Messages)
	_, stderr, code := runCLI("reply", id, strings.Repeat("y", 161))
	if code == 0 || !strings.Contains(stderr, "line 1 is 161 characters (limit 160)") {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
	if got := len(showJSON(t, id).Messages); got != before {
		t.Fatalf("messages = %d, want %d", got, before)
	}
}

func TestLongLineExemptionsPass(t *testing.T) {
	useState(t)
	long := strings.Repeat("z", 200)
	text := strings.Join([]string{
		"Essence first",
		"https://example.com/" + long,
		"  https://example.com/" + long + "  ",
		"> " + long,
		"```",
		long,
		"```",
		"- short bullet",
	}, "\n")
	id := askRelay(t, nil, text)
	if _, stderr, code := runCLI("reply", id, text); code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
}

func TestLongLineInsideAnUnclosedFenceStillPasses(t *testing.T) {
	useState(t)
	askRelay(t, nil, "Essence\n```\n"+strings.Repeat("z", 200))
}

func TestLongLinesFlagSendsTheTextUnchanged(t *testing.T) {
	useState(t)
	long := strings.Repeat("w", 300)
	id := askRelay(t, []string{"--long-lines"}, long)
	if got := showJSON(t, id).Question; got != long {
		t.Fatalf("question = %q", got)
	}
	if _, stderr, code := runCLI("reply", id, long, "--long-lines"); code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
	msgs := showJSON(t, id).Messages
	if msgs[len(msgs)-1].Text != long {
		t.Fatalf("reply text = %q", msgs[len(msgs)-1].Text)
	}
}

func TestMaxLineLengthConfigIsHonored(t *testing.T) {
	useLineConfig(t, `{"ui":{"maxLineLength":20}}`)
	_, stderr, code := runCLI(relayArgs(strings.Repeat("a", 21))...)
	if code == 0 || !strings.Contains(stderr, "line 1 is 21 characters (limit 20)") {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	askRelay(t, nil, strings.Repeat("a", 20))
}

func TestMaxLineLengthZeroDisablesTheCheck(t *testing.T) {
	useLineConfig(t, `{"ui":{"maxLineLength":0}}`)
	id := askRelay(t, nil, strings.Repeat("a", 500))
	if _, stderr, code := runCLI("reply", id, strings.Repeat("b", 500)); code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
}

func TestMaxLineLengthRejectsANegativeValue(t *testing.T) {
	useLineConfig(t, `{"ui":{"maxLineLength":-1}}`)
	_, stderr, code := runCLI(relayArgs("short")...)
	if code == 0 || !strings.Contains(stderr, "maxLineLength") {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
}

func TestHumanUIAnswerIsNotChecked(t *testing.T) {
	useState(t)
	long := strings.Repeat("h", 400)
	id := askRelay(t, []string{"--free-text"}, "root")
	if _, stderr, code := runCLI("answer", "--via", "ui", id, long); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
}
