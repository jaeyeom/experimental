package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestClientsAppendOnAThreeMessageThread(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	installHerdr(t)

	t.Run("answer", func(t *testing.T) {
		id, before := seedCLIThread(t)
		if code := answer(t, id, "keep"); code != 0 {
			t.Fatal(code)
		}
		assertAppended(t, id, before, "human", "keep")
	})
	t.Run("reply", func(t *testing.T) {
		id, before := seedCLIThread(t)
		stdout, stderr, code := runCLI(
			"reply", id, "agent follow-up",
			"--option", "a:One", "--option", "b:Two", "--option", "c:Three",
			"--rec", "a",
		)
		if code != 0 {
			t.Fatalf("reply code = %d\n%s\n%s", code, stderr, stdout)
		}
		got := assertAppended(t, id, before, "ada", "agent follow-up")
		last := got.Messages[len(got.Messages)-1]
		if !optionKeys(last.Options, "a", "b", "c") || !recommendedKey(last.Options, "a") || got.FreeText {
			t.Fatalf("reply message = %+v free %v", last, got.FreeText)
		}
		if !optionKeys(got.Messages[0].Options, "a", "b") || got.Messages[0].Options[0].Label != "Alpha" {
			t.Fatalf("root gained reply options: %+v", got.Messages[0].Options)
		}
	})
	t.Run("compose", func(t *testing.T) {
		id, before := seedCLIThread(t)
		stdout, stderr, code := runCLI("send", "--id", id, "please edit")
		if code != 0 {
			t.Fatalf("send --id code = %d\n%s\n%s", code, stderr, stdout)
		}
		assertAppended(t, id, before, "human", "please edit")
	})
}

func TestReplyArgumentOrdersAppendTheBody(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	body := "line one\n---\nline two"
	opts := []string{"--option", "a:One", "--option", "b:Two", "--option", "c:Three"}
	cases := []struct {
		name string
		text string
		free bool
		rec  bool
		args func(id, text string) []string
	}{
		{
			name: "text before flags",
			text: "before flags",
			args: func(id, text string) []string {
				return append([]string{"reply", id, text}, opts...)
			},
		},
		{
			name: "flags before text with rec",
			text: "after flags",
			rec:  true,
			args: func(id, text string) []string {
				return append(append([]string{"reply", id}, opts...), "--rec", "a", text)
			},
		},
		{
			name: "text before flags with rec and free text",
			text: "free before",
			rec:  true,
			free: true,
			args: func(id, text string) []string {
				return append(append([]string{"reply", id, text}, opts...), "--rec", "a", "--free-text")
			},
		},
		{
			name: "flags before text with rec and free text",
			text: "free after",
			rec:  true,
			free: true,
			args: func(id, text string) []string {
				return append(append([]string{"reply", id}, opts...), "--rec", "a", "--free-text", text)
			},
		},
		{
			name: "text before flags with a separator line",
			text: body,
			free: true,
			args: func(id, text string) []string {
				return append(append([]string{"reply", id, text}, opts...), "--free-text")
			},
		},
		{
			name: "flags before text with a separator line",
			text: body,
			free: true,
			args: func(id, text string) []string {
				return append(append([]string{"reply", id}, opts...), "--free-text", text)
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			id, before := seedCLIThread(t)
			stdout, stderr, code := runCLI(tc.args(id, tc.text)...)
			if code != 0 {
				t.Fatalf("code = %d\n%s\n%s", code, stderr, stdout)
			}
			got := assertAppended(t, id, before, "ada", tc.text)
			last := got.Messages[len(got.Messages)-1]
			if !optionKeys(last.Options, "a", "b", "c") {
				t.Fatalf("options = %+v", last.Options)
			}
			if tc.rec && !recommendedKey(last.Options, "a") {
				t.Fatalf("recommended = %+v", last.Options)
			}
			if !tc.rec && recommendedKey(last.Options, "a") {
				t.Fatalf("unexpected recommendation = %+v", last.Options)
			}
			if got.FreeText != tc.free {
				t.Fatalf("freeText = %v, want %v", got.FreeText, tc.free)
			}
			if !optionKeys(got.Messages[0].Options, "a", "b") || got.Messages[0].Options[0].Label != "Alpha" {
				t.Fatalf("root options = %+v", got.Messages[0].Options)
			}
			if !optionKeys(got.Messages[2].Options, "keep", "drop") {
				t.Fatalf("agent options changed: %+v", got.Messages[2].Options)
			}
		})
	}
}

func TestReplyUnknownFlagAfterTextDoesNotChangeTheThread(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	id, before := seedCLIThread(t)
	stdout, stderr, code := runCLI("reply", id, "kept", "--made-up")
	if code == 0 {
		t.Fatalf("unknown flag exited 0\n%s", stdout)
	}
	if !strings.Contains(stderr, "made-up") {
		t.Fatalf("stderr = %q", stderr)
	}
	got := showJSON(t, id)
	if signature(got.Messages) != signature(before) {
		t.Fatalf("messages changed\nbefore %+v\nafter %+v", before, got.Messages)
	}
}

func TestSendToDoesNotReplaceAnotherThread(t *testing.T) {
	state := t.TempDir()
	cfg := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", cfg)
	installHerdr(t)
	dir := filepath.Join(cfg, "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := []byte("{\"targets\":[{\"name\":\"alpha\",\"pane\":\"pane-a\"}]}\n")
	if err := os.WriteFile(filepath.Join(dir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}
	id, before := seedCLIThread(t)
	stdout, stderr, code := runCLI("send", "--to", "alpha", "a new note")
	if code != 0 {
		t.Fatalf("send code = %d\n%s\n%s", code, stderr, stdout)
	}
	got := showJSON(t, id)
	if signature(got.Messages) != signature(before) {
		t.Fatalf("existing thread changed\n%+v", got.Messages)
	}
	items := showThreads(t)
	if len(items) != 2 {
		t.Fatalf("items = %d", len(items))
	}
	var created bool
	for _, it := range items {
		if it.ID == id {
			continue
		}
		if len(it.Messages) == 1 && it.Messages[0].Text == "a new note" && it.Owner == "alpha" {
			created = true
		}
	}
	if !created {
		t.Fatalf("new thread missing: %+v", items)
	}
}

func TestEmacsAppendsAndReadsTheStoredThread(t *testing.T) {
	emacs, err := exec.LookPath("emacs")
	if err != nil {
		t.Skip("emacs is not installed")
	}
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	installHerdr(t)
	t.Setenv("DECIDE_PROGRAM", os.Args[0])
	cmd := exec.Command(emacs, "--batch", "-Q",
		"-l", emacsLisp(t, "decide.el"),
		"-l", emacsLisp(t, "decide-test.el"),
		"--eval", "(ert-run-tests-batch-and-exit '(tag decide-stored))",
	)
	cmd.Env = append(os.Environ(), "DECIDE_TEST_CHILD=1")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("emacs: %v\n%s", err, out)
	}
	for _, name := range []string{
		"decide-stored-answer-appends",
		"decide-stored-reply-appends",
		"decide-stored-compose-appends",
		"decide-stored-zero-opens-composer",
		"decide-stored-composer-keeps-a-finished-answer",
		"decide-stored-thread-blocks",
		"decide-stored-message-next-and-previous",
		"decide-stored-item-key-line-names-archive",
	} {
		if !strings.Contains(string(out), name) {
			t.Fatalf("emacs output did not run %s\n%s", name, out)
		}
	}
}

func seedCLIThread(t *testing.T) (string, []decide.Message) {
	t.Helper()
	id := askRelay(t, []string{"--free-text", "--notify-pane", "pane-9"}, "root ask")
	if code := answer(t, id, "human note"); code != 0 {
		t.Fatalf("seed answer code = %d", code)
	}
	stdout, stderr, code := runCLI(
		"reply", id, "agent draft",
		"--option", "keep:Keep", "--option", "drop:Drop", "--rec", "keep",
	)
	if code != 0 {
		t.Fatalf("seed reply code = %d\n%s\n%s", code, stderr, stdout)
	}
	got := showJSON(t, id)
	if len(got.Messages) != 3 {
		t.Fatalf("seed messages = %+v", got.Messages)
	}
	return id, got.Messages
}

func assertAppended(t *testing.T, id string, before []decide.Message, author, text string) decide.Item {
	t.Helper()
	got := showJSON(t, id)
	if len(got.Messages) != len(before)+1 {
		t.Fatalf("messages = %+v", got.Messages)
	}
	if signature(got.Messages[:len(before)]) != signature(before) {
		t.Fatalf("earlier messages = %+v\nwant %+v", got.Messages[:len(before)], before)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Author != author || last.Text != text {
		t.Fatalf("new message = %+v", last)
	}
	return got
}

func signature(msgs []decide.Message) string {
	var b strings.Builder
	for _, msg := range msgs {
		b.WriteString(msg.Author)
		b.WriteByte('\t')
		b.WriteString(msg.Text)
		b.WriteByte('\n')
		for _, opt := range msg.Options {
			b.WriteString(opt.Key)
			b.WriteByte(':')
			b.WriteString(opt.Label)
			if opt.Recommended {
				b.WriteString("*")
			}
			b.WriteByte(',')
		}
		b.WriteByte('\n')
	}
	return b.String()
}

func installHerdr(t *testing.T) {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	script := "#!/bin/sh\nexit 0\n"
	if err := os.WriteFile(filepath.Join(bin, "herdr"), []byte(script), 0o755); err != nil { //nolint:gosec // test stand-in for herdr
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func emacsLisp(t *testing.T, name string) string {
	t.Helper()
	var candidates []string
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools/decide/emacs", name))
	}
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "..", "..", "emacs", name))
	}
	for _, path := range candidates {
		if _, err := os.Stat(path); err == nil {
			return path
		}
	}
	t.Fatalf("emacs lisp %s not found in %v", name, candidates)
	return ""
}
