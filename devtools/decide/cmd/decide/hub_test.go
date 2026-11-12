package main

import (
	"bytes"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall/sshtest"
)

// setupHub puts a fake ssh and a fake hub decide on PATH, with an empty
// local state directory that hub mode must leave alone.
func setupHub(t *testing.T, config string) (*sshtest.Fake, string) {
	t.Helper()
	fake := sshtest.Install(t)
	fake.InstallTool("decide")
	cfgDir := t.TempDir()
	if config != "" {
		if err := os.MkdirAll(filepath.Join(cfgDir, "decide"), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(cfgDir, "decide", "config.json"), []byte(config), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	return fake, state
}

func assertNoLocalState(t *testing.T, state string) {
	t.Helper()
	if entries, _ := os.ReadDir(state); len(entries) != 0 {
		t.Fatalf("hub mode created local state: %v", entries)
	}
}

func TestHubReplyRunsOnTheHub(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub"}`)
	fake.Write("decide.stdout", "D-1 open\n")
	var stdout, stderr bytes.Buffer
	code := run([]string{"reply", "D-1", "x; y"}, &stdout, &stderr)
	if code != 0 || stdout.String() != "D-1 open\n" {
		t.Fatalf("reply = %d, stdout %q, stderr %q", code, stdout.String(), stderr.String())
	}
	if got := fake.Lines("decide.args"); !slices.Equal(got, []string{"reply", "D-1", "x; y"}) {
		t.Fatalf("hub args = %q", got)
	}
	argv := fake.Lines("argv")
	if !slices.Contains(argv, "BatchMode=yes") || !slices.Contains(argv, "ConnectTimeout=5") || !slices.Contains(argv, "me@hub") {
		t.Fatalf("ssh argv = %q", argv)
	}
	assertNoLocalState(t, state)
}

func TestHubEnvOverridesConfigAndPassesExitCode(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@other", "hubTimeout": "2s"}`)
	t.Setenv("DECIDE_HUB", "me@hub")
	fake.Write("decide.stderr", "decide: item D-9 not found\n")
	fake.Write("decide.code", "1")
	var stderr bytes.Buffer
	code := run([]string{"show", "D-9"}, &bytes.Buffer{}, &stderr)
	if code != 1 || stderr.String() != "decide: item D-9 not found\n" {
		t.Fatalf("show = %d, stderr %q", code, stderr.String())
	}
	argv := fake.Lines("argv")
	if !slices.Contains(argv, "me@hub") || !slices.Contains(argv, "ConnectTimeout=2") {
		t.Fatalf("ssh argv = %q", argv)
	}
}

func TestHubUnreachableNeverFallsBack(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub", "hubTimeout": "1s"}`)
	fake.Write("down", "")
	fake.Unreachable()
	var stderr bytes.Buffer
	code := run([]string{"list"}, &bytes.Buffer{}, &stderr)
	if code == 0 || !strings.Contains(stderr.String(), "decide: hub me@hub unreachable: host unreachable") {
		t.Fatalf("list = %d, stderr %q", code, stderr.String())
	}
	assertNoLocalState(t, state)
}

func TestHubInboxRefuses(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub"}`)
	var stderr bytes.Buffer
	code := run([]string{"inbox"}, &bytes.Buffer{}, &stderr)
	if code == 0 || !strings.Contains(stderr.String(), "run decide inbox there") {
		t.Fatalf("inbox = %d, stderr %q", code, stderr.String())
	}
	if fake.Read("argv") != "" {
		t.Fatal("inbox ran ssh")
	}
}

// TestHubAnswerKeepsVia forwards a client's answer to the fake hub, then
// replays the forwarded arguments on a real ledger as the hub would.
func TestHubAnswerKeepsVia(t *testing.T) {
	fake, _ := setupHub(t, "")
	for _, tc := range []struct {
		flags []string
		want  string
	}{
		{[]string{"--via", "ui"}, decide.ViaUI},
		{nil, decide.ViaCLI},
	} {
		t.Setenv("DECIDE_HUB", "")
		id := askRelay(t, nil, "on the hub")
		args := append(append([]string{"answer"}, tc.flags...), id, "a")

		t.Setenv("DECIDE_HUB", "me@hub")
		var stderr bytes.Buffer
		if code := run(args, &bytes.Buffer{}, &stderr); code != 0 {
			t.Fatalf("client %q = %d, stderr %q", args, code, stderr.String())
		}
		forwarded := fake.Lines("decide.args")
		if !slices.Equal(forwarded, args) {
			t.Fatalf("hub args = %q, want %q", forwarded, args)
		}

		t.Setenv("DECIDE_HUB", "")
		if _, stderr, code := runCLI(forwarded...); code != 0 {
			t.Fatalf("hub %q = %d\n%s", forwarded, code, stderr)
		}
		if got := showJSON(t, id); got.Answer == nil || got.Answer.Via != tc.want {
			t.Fatalf("hub answer for %q = %+v, want via %s", args, got.Answer, tc.want)
		}
	}
}
