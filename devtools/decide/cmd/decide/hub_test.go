package main

import (
	"bytes"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
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

// hubDecide puts a decide on PATH for the fake ssh that is this test
// binary with its own hub state and config, so `decide serve --stdio`
// runs for real on a separate ledger. It returns the hub's ledger
// directory and the directory of the script, which records its args.
func hubDecide(t *testing.T, hubConfig string) (string, string, string) {
	t.Helper()
	bin := t.TempDir()
	hubState, hubCfg := t.TempDir(), t.TempDir()
	if err := os.MkdirAll(filepath.Join(hubCfg, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(hubCfg, "decide", "config.json"), []byte(hubConfig), 0o600); err != nil {
		t.Fatal(err)
	}
	self, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	script := "#!/bin/sh\n" +
		"unset DECIDE_HUB\n" +
		"export DECIDE_TEST_CHILD=1 XDG_STATE_HOME=" + hubcall.Quote(hubState) + " XDG_CONFIG_HOME=" + hubcall.Quote(hubCfg) + "\n" +
		`printf '%s\n' "$@" > ` + hubcall.Quote(filepath.Join(bin, "args")) + "\n" +
		"exec " + hubcall.Quote(self) + ` "$@"` + "\n"
	if err := os.WriteFile(filepath.Join(bin, "decide"), []byte(script), 0o755); err != nil { //nolint:gosec // an executable test script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
	return filepath.Join(hubState, "decide"), bin, hubCfg
}

// stubInbox replaces the TUI and returns the options it was run with.
func stubInbox(t *testing.T, check func(tui.Options)) *tui.Options {
	t.Helper()
	var got tui.Options
	prev := runInbox
	runInbox = func(opts tui.Options) error {
		got = opts
		if check != nil {
			check(opts)
		}
		return nil
	}
	t.Cleanup(func() { runInbox = prev })
	return &got
}

func TestHubInboxRunsLocallyAgainstTheHub(t *testing.T) {
	_, state := setupHub(t, `{"hub": "me@hub", "machine": "mac", "hubMachine": "hub"}`)
	hubDir, bin, _ := hubDecide(t, `{"targets": [{"name": "builder", "pane": "w2:p3"}]}`)
	led, err := decide.Open(hubDir)
	if err != nil {
		t.Fatal(err)
	}
	item, err := led.Create(&decide.Item{Question: "ship?", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, Options: []decide.Option{{Key: "y", Label: "Yes"}}})
	if err != nil {
		t.Fatal(err)
	}
	var display tui.Display
	opts := stubInbox(t, func(opts tui.Options) {
		display, _ = opts.Display.Display()
		items, err := opts.Backend.List()
		if err != nil || len(items) != 1 || items[0].ID != item.ID {
			t.Errorf("hub list = %+v, %v", items, err)
		}
		if _, err := opts.Backend.Answer(item.ID, "y"); err != nil {
			t.Errorf("hub answer: %v", err)
		}
	})
	var stderr bytes.Buffer
	if code := run([]string{"inbox"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("inbox = %d, stderr %q", code, stderr.String())
	}
	if opts.Backend == nil || len(display.Targets) != 1 || display.Targets[0].Name != "builder" {
		t.Fatalf("options = %+v, display %+v", opts, display)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Via != decide.ViaUI {
		t.Fatalf("hub answer = %+v", got.Answer)
	}
	if args, _ := os.ReadFile(filepath.Join(bin, "args")); string(args) != "serve\n--stdio\n" {
		t.Fatalf("hub ran decide %q", args)
	}
	assertNoLocalState(t, state)
}

func TestHubInboxUnreachableFailsAtStart(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub", "hubTimeout": "1s"}`)
	fake.Write("down", "")
	fake.Unreachable()
	stubInbox(t, func(tui.Options) { t.Error("the inbox started without the hub") })
	var stderr bytes.Buffer
	code := run([]string{"inbox"}, &bytes.Buffer{}, &stderr)
	if code == 0 || !strings.Contains(stderr.String(), "hub me@hub unreachable") {
		t.Fatalf("inbox = %d, stderr %q", code, stderr.String())
	}
	assertNoLocalState(t, state)
}

func TestInboxUTCComesFromTheFlagOrLocalConfig(t *testing.T) {
	t.Setenv("DECIDE_HUB", "")
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	if err := os.MkdirAll(filepath.Join(cfg, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	opts := stubInbox(t, nil)
	var stderr bytes.Buffer
	if code := run([]string{"inbox", "--utc"}, &bytes.Buffer{}, &stderr); code != 0 || !opts.UTC {
		t.Fatalf("flag utc %v code %d %s", opts.UTC, code, stderr.String())
	}
	if err := os.WriteFile(filepath.Join(cfg, "decide", "config.json"), []byte(`{"ui":{"utc":true}}`), 0o600); err != nil {
		t.Fatal(err)
	}
	opts = stubInbox(t, nil)
	stderr.Reset()
	if code := run([]string{"inbox"}, &bytes.Buffer{}, &stderr); code != 0 || !opts.UTC {
		t.Fatalf("config utc %v code %d %s", opts.UTC, code, stderr.String())
	}
	if err := os.WriteFile(filepath.Join(cfg, "decide", "config.json"), []byte(`{}`), 0o600); err != nil {
		t.Fatal(err)
	}
	opts = stubInbox(t, nil)
	stderr.Reset()
	if code := run([]string{"inbox"}, &bytes.Buffer{}, &stderr); code != 0 || opts.UTC {
		t.Fatalf("local zone utc %v code %d %s", opts.UTC, code, stderr.String())
	}
}

func TestInboxWithoutAHubUsesTheLocalLedger(t *testing.T) {
	t.Setenv("DECIDE_HUB", "")
	opts := stubInbox(t, nil)
	var stderr bytes.Buffer
	if code := run([]string{"inbox", "--snoozed"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("inbox = %d, stderr %q", code, stderr.String())
	}
	dir, _ := decide.DefaultDir()
	if opts.Backend != nil || opts.Dir != dir || !opts.Snoozed {
		t.Fatalf("options = %+v", opts)
	}
}

func TestRemoteOverridesTheHubForOneInvocation(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@config"}`)
	t.Setenv("DECIDE_HUB", "me@env")
	var stderr bytes.Buffer
	if code := run([]string{"--remote", "me@hub", "show", "D-1"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("show = %d, stderr %q", code, stderr.String())
	}
	if argv := fake.Lines("argv"); !slices.Contains(argv, "me@hub") || slices.Contains(argv, "me@env") {
		t.Fatalf("ssh argv = %q", argv)
	}
	if got := fake.Lines("decide.args"); !slices.Equal(got, []string{"show", "D-1"}) {
		t.Fatalf("hub args = %q", got)
	}
	if code := run([]string{"--remote=me@other", "list"}, &bytes.Buffer{}, &stderr); code != 0 || !slices.Contains(fake.Lines("argv"), "me@other") {
		t.Fatalf("--remote= = %d, argv %q", code, fake.Lines("argv"))
	}
	if code := run([]string{"--remote"}, &bytes.Buffer{}, &stderr); code != 2 {
		t.Fatalf("--remote without a target = %d", code)
	}
}

// TestHubSendIDKeepsVia forwards a client's send --id to the fake hub,
// then replays the forwarded arguments on a real ledger as the hub would.
func TestHubSendIDKeepsVia(t *testing.T) {
	fake, _ := setupHub(t, "")
	writeSendConfig(t, os.Getenv("XDG_CONFIG_HOME"))
	for _, tc := range []struct {
		flags []string
		want  string
	}{
		{[]string{"--via", "ui"}, decide.ViaUI},
		{nil, decide.ViaCLI},
	} {
		t.Setenv("DECIDE_HUB", "")
		id := askRelay(t, []string{"--notify-pane", "p1"}, "on the hub")
		args := append(append([]string{"send", "--id", id}, tc.flags...), "edits")

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
			t.Fatalf("hub send --id for %q = %+v, want via %s", args, got.Answer, tc.want)
		}
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
