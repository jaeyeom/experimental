package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
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

func TestLongLinesFlagIsRemovedUsageError(t *testing.T) {
	useState(t)
	id := askRelay(t, nil, "Short verdict")
	for _, args := range [][]string{
		append(relayArgs("Short verdict"), "--long-lines"),
		{"reply", id, "--long-lines", "Short verdict"},
	} {
		_, stderr, code := runCLI(args...)
		if code != 2 {
			t.Errorf("%v exit = %d, want 2\n%s", args, code, stderr)
		}
		for _, want := range []string{"--long-lines was removed", "First line: the verdict", "--verbatim-file"} {
			if !strings.Contains(stderr, want) {
				t.Errorf("%v stderr missing %q:\n%s", args, want, stderr)
			}
		}
	}
}

func TestShapeChecksRefuseEachViolation(t *testing.T) {
	cases := []struct {
		name string
		text string
		want string
	}{
		{
			name: "long line",
			text: "Verdict\n\n- ok\nI marked it done too early: " + strings.Repeat("x", 200),
			want: "line 4 is 228 characters (limit 160)",
		},
		{
			name: "long first line",
			text: strings.Repeat("a", 101),
			want: "first line is 101 characters (limit 100)",
		},
		{
			name: "first line runs into second prose line",
			text: "Short verdict\nThis continues on line 2",
			want: "first line runs into line 2 without a blank line or bullet",
		},
		{
			name: "four line paragraph",
			text: "Verdict\n\nLine one.\nLine two.\nLine three.\nLine four.",
			want: "paragraph at line 3 is 4 lines (limit 3)",
		},
		{
			name: "over-size message",
			text: "Verdict\n\n" + strings.Repeat("- "+strings.Repeat("b", 100)+"\n", 12),
			want: "limit 1200",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			useState(t)
			_, stderr, code := runCLI(relayArgs(tc.text)...)
			if code != 2 {
				t.Fatalf("ask exit = %d, want 2\n%s", code, stderr)
			}
			assertNoItems(t)
			if !strings.Contains(stderr, tc.want) {
				t.Errorf("stderr missing %q:\n%s", tc.want, stderr)
			}
			for _, guide := range []string{"First line: the verdict", "short bullet lines", "--text-file or stdin"} {
				if !strings.Contains(stderr, guide) {
					t.Errorf("stderr missing guidance %q:\n%s", guide, stderr)
				}
			}
			if strings.Contains(stderr, "--long-lines") {
				t.Errorf("refusal mentions --long-lines:\n%s", stderr)
			}
		})
	}
}

func TestVerdictBlankAndBulletsPasses(t *testing.T) {
	useState(t)
	text := "Ship the fix now?\n\n- Tests pass\n- Risk is low\n- Rollback is trivial"
	id := askRelay(t, nil, text)
	if _, stderr, code := runCLI("reply", id, text); code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
}

func TestShapeChecksEnforcedAcrossDoneOptionsStatusAndHub(t *testing.T) {
	t.Run("done outcome", func(t *testing.T) {
		useState(t)
		id := askRelay(t, nil, "Root")
		_, stderr, code := runCLI("done", id, strings.Repeat("d", 101))
		if code != 2 || !strings.Contains(stderr, "first line is 101 characters") {
			t.Fatalf("done = %d\n%s", code, stderr)
		}
	})
	t.Run("option label", func(t *testing.T) {
		useState(t)
		_, stderr, code := runCLI("ask", "--owner", "ada", "--kind", "relay",
			"--option", "a:"+strings.Repeat("o", 101),
			"--option", "b:Beta", "--rec", "a", "Pick one")
		if code != 2 || !strings.Contains(stderr, "option \"a\" label") {
			t.Fatalf("ask = %d\n%s", code, stderr)
		}
		assertNoItems(t)
	})
	t.Run("status text", func(t *testing.T) {
		useState(t)
		_, stderr, code := runCLI("status", "set", "--agent", "ada", strings.Repeat("s", 101))
		if code != 2 || !strings.Contains(stderr, "first line is 101 characters") {
			t.Fatalf("status set = %d\n%s", code, stderr)
		}
	})
	t.Run("hub path", func(t *testing.T) {
		hubState, _, _ := hubDecide(t, "{}\n")
		bin := t.TempDir()
		sshPath := filepath.Join(bin, "ssh")
		sshScript := "#!/bin/sh\nwhile [ \"$#\" -gt 0 ] && [ \"$1\" != \"me@hub\" ]; do shift; done\nshift\nexec sh -c \"$*\"\n"
		if err := os.WriteFile(sshPath, []byte(sshScript), 0o755); err != nil { //nolint:gosec // test wrapper
			t.Fatal(err)
		}
		t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
		t.Setenv("DECIDE_HUB", "me@hub")
		localState := t.TempDir()
		t.Setenv("XDG_STATE_HOME", localState)
		t.Setenv("XDG_CONFIG_HOME", t.TempDir())
		_, stderr, code := runCLI(relayArgs(strings.Repeat("h", 101))...)
		if code != 2 || !strings.Contains(stderr, "first line is 101 characters") {
			t.Fatalf("hub ask = %d\n%s", code, stderr)
		}
		if entries, _ := os.ReadDir(hubState); len(entries) != 0 {
			t.Fatalf("hub stored rejected item: %v", entries)
		}
	})
}

func TestLongLineExemptionsPass(t *testing.T) {
	useState(t)
	long := strings.Repeat("z", 200)
	text := strings.Join([]string{
		"Essence first",
		"",
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

func TestVerbatimFileStoredByteForByteAndAuthorTextStillChecked(t *testing.T) {
	useState(t)
	draft := "  " + strings.Repeat("v", 220) + "\nLine 2\nLine 3\nLine 4\nLine 5\n\n"
	draftPath := filepath.Join(t.TempDir(), "draft.txt")
	if err := os.WriteFile(draftPath, []byte(draft), 0o600); err != nil {
		t.Fatal(err)
	}

	// Bad author question still fails even with --verbatim-file.
	badArgs := append([]string{"ask", "--owner", "ada", "--kind", "relay", "--option", "a:Alpha", "--option", "b:Beta", "--rec", "a", "--verbatim-file", draftPath}, strings.Repeat("q", 101))
	if _, stderr, code := runCLI(badArgs...); code != 2 || !strings.Contains(stderr, "first line is 101 characters") {
		t.Fatalf("bad ask with verbatim = %d\n%s", code, stderr)
	}
	assertNoItems(t)

	// Good author question + --verbatim-file succeeds and stores exact bytes.
	goodArgs := []string{"ask", "--owner", "ada", "--kind", "relay", "--option", "a:Alpha", "--option", "b:Beta", "--rec", "a", "--verbatim-file", draftPath, "Post this draft?"}
	stdout, stderr, code := runCLI(goodArgs...)
	if code != 0 {
		t.Fatalf("ask with verbatim = %d\n%s", code, stderr)
	}
	askID := strings.TrimSpace(stdout)
	got := showJSON(t, askID)
	if got.Verbatim != draft || len(got.Messages) == 0 || got.Messages[0].Verbatim != draft {
		t.Fatalf("stored verbatim mismatch:\nitem=%q\nmsg=%+v", got.Verbatim, got.Messages)
	}

	// Reply with --verbatim-file also stores exact bytes.
	if _, stderr, code := runCLI("reply", askID, "--verbatim-file", draftPath, "Updated draft"); code != 0 {
		t.Fatalf("reply with verbatim = %d\n%s", code, stderr)
	}
	msgs := showJSON(t, askID).Messages
	if msgs[len(msgs)-1].Verbatim != draft {
		t.Fatalf("reply verbatim = %q, want %q", msgs[len(msgs)-1].Verbatim, draft)
	}
}

func TestConfigZeroDisablesOnlyThatCheck(t *testing.T) {
	t.Run("maxLineLength 0", func(t *testing.T) {
		useLineConfig(t, `{"ui":{"maxLineLength":0}}`)
		// Long line on line 3 passes, but >100 first line still fails.
		askRelay(t, nil, "Short\n\n"+strings.Repeat("a", 200))
		if _, stderr, code := runCLI(relayArgs(strings.Repeat("a", 101))...); code != 2 || !strings.Contains(stderr, "first line") {
			t.Fatalf("expected first line failure, got %d\n%s", code, stderr)
		}
	})
	t.Run("maxFirstLineLength 0", func(t *testing.T) {
		useLineConfig(t, `{"ui":{"maxFirstLineLength":0}}`)
		askRelay(t, nil, strings.Repeat("a", 120)+"\nnext line")
		if _, stderr, code := runCLI(relayArgs("a\nb\nc\nd")...); code != 2 || !strings.Contains(stderr, "paragraph") {
			t.Fatalf("expected paragraph failure, got %d\n%s", code, stderr)
		}
	})
	t.Run("maxParagraphLines 0", func(t *testing.T) {
		useLineConfig(t, `{"ui":{"maxParagraphLines":0}}`)
		askRelay(t, nil, "Header\n\nline1\nline2\nline3\nline4")
		if _, stderr, code := runCLI(relayArgs(strings.Repeat("a", 101))...); code != 2 || !strings.Contains(stderr, "first line") {
			t.Fatalf("expected first line failure, got %d\n%s", code, stderr)
		}
	})
	t.Run("maxMessageChars 0", func(t *testing.T) {
		useLineConfig(t, `{"ui":{"maxMessageChars":0}}`)
		big := "Header\n\n" + strings.Repeat("- "+strings.Repeat("b", 80)+"\n", 20)
		askRelay(t, nil, big)
		if _, stderr, code := runCLI(relayArgs(strings.Repeat("a", 101))...); code != 2 || !strings.Contains(stderr, "first line") {
			t.Fatalf("expected first line failure, got %d\n%s", code, stderr)
		}
	})
}

func TestNegativeShapeConfigRejected(t *testing.T) {
	for _, key := range []string{"maxLineLength", "maxFirstLineLength", "maxParagraphLines", "maxMessageChars"} {
		useLineConfig(t, `{"ui":{"`+key+`":-1}}`)
		_, stderr, code := runCLI(relayArgs("short")...)
		if code == 0 || !strings.Contains(stderr, key) {
			t.Fatalf("%s: ask = %d\n%s", key, code, stderr)
		}
	}
}

func TestHumanUIMessagesAreNotChecked(t *testing.T) {
	useLineConfig(t, `{"sendTemplate":"true {pane}"}`)
	long := strings.Repeat("h", 400)
	id := askRelay(t, []string{"--free-text", "--notify-pane", "pane-1"}, "root")
	if _, stderr, code := runCLI("answer", "--via", "ui", id, long); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("send", "--id", id, "--via", "ui", long); code != 0 {
		t.Fatalf("send --via ui = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("done", "--via", "ui", id, long); code != 0 {
		t.Fatalf("done --via ui = %d\n%s", code, stderr)
	}
	led, err := decide.Open(filepath.Join(os.Getenv("XDG_STATE_HOME"), "decide"))
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.StartThread("ada", "pane-1", long, decide.ViaUI); err != nil {
		t.Fatalf("StartThread ViaUI = %v", err)
	}
}
