package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/decide/tui"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
	"github.com/muesli/termenv"
)

func TestAvatarsListShowsEveryID(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("COLUMNS", "80")
	stdout, stderr, code := runCLI("avatars", "list")
	if code != 0 {
		t.Fatalf("code = %d, stderr = %s", code, stderr)
	}
	for _, id := range append(avatar.BuiltinIDs(), "queen", "beer-mug") {
		if !strings.Contains(stdout, " "+id) {
			t.Errorf("output has no id %q", id)
		}
	}
	owl, err := avatar.Builtin("owl", 10, 6, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	for _, row := range owl {
		if !strings.Contains(stdout, strings.TrimRight(row, " ")) {
			t.Fatalf("output has no owl row %q\n%s", row, stdout)
		}
	}
	if strings.Contains(stdout, "\x1b") {
		t.Errorf("output to a non-terminal has escapes")
	}
}

func TestAvatarsListFitsWidth(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	tests := []struct {
		cols         int
		minRow, most int
	}{
		{20, 1, 1},
		{80, 2, 3},
		{200, 5, 8},
	}
	for _, tt := range tests {
		t.Setenv("COLUMNS", strconv.Itoa(tt.cols))
		stdout, _, code := runCLI("avatars", "list")
		if code != 0 {
			t.Fatalf("COLUMNS=%d: code %d", tt.cols, code)
		}
		for _, line := range strings.Split(stdout, "\n") {
			if n := utf8.RuneCountInString(line); n > tt.cols && tt.cols >= 30 {
				t.Errorf("COLUMNS=%d: line is %d wide: %q", tt.cols, n, line)
			}
		}
		first := strings.SplitN(stdout, "\n\n", 2)[0]
		perRow := 0
		for _, id := range avatar.BuiltinIDs() {
			if strings.Contains(first, " "+id+" ") || strings.HasSuffix(first, " "+id) || strings.Contains(first, " "+id+"\n") {
				perRow++
			}
		}
		if perRow < tt.minRow || perRow > tt.most {
			t.Errorf("COLUMNS=%d: %d figures in the first row, want %d to %d", tt.cols, perRow, tt.minRow, tt.most)
		}
	}
}

func TestAvatarsListName(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("COLUMNS", "80")
	stdout, stderr, code := runCLI("avatars", "list", "--name", "coord/primary")
	if code != 0 {
		t.Fatalf("code = %d, stderr = %s", code, stderr)
	}
	want := "coord/primary: " + avatar.PickBuiltin("coord/primary")
	if !strings.HasPrefix(stdout, want+"\n") {
		t.Fatalf("output does not start with %q\n%s", want, stdout)
	}
	for _, id := range avatar.BuiltinIDs() {
		if !strings.Contains(stdout, " "+id) {
			t.Errorf("output has no id %q", id)
		}
	}
}

func TestAvatarsUsage(t *testing.T) {
	t.Setenv("DECIDE_HUB", "")
	for _, args := range [][]string{{"avatars"}, {"avatars", "show"}, {"avatars", "list", "extra"}} {
		var stderr bytes.Buffer
		if code := run(args, &bytes.Buffer{}, &stderr); code != 2 {
			t.Errorf("%q: code = %d, want 2", args, code)
		}
	}
}

func TestAvatarsListRunsLocallyInHubMode(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub"}`)
	t.Setenv("COLUMNS", "80")
	stdout, stderr, code := runCLI("avatars", "list")
	if code != 0 || !strings.Contains(stdout, " owl") {
		t.Fatalf("code = %d, stderr = %s", code, stderr)
	}
	if fake.Read("argv") != "" {
		t.Fatal("avatars list ran ssh")
	}
}

// avatarConfig writes config to a temp config dir and returns its path.
func avatarConfig(t *testing.T, config string) string {
	t.Helper()
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", home)
	t.Setenv("DECIDE_HUB", "")
	path := filepath.Join(home, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(config), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestAvatarsSetMatchesPicker(t *testing.T) {
	const config = `{"zeta": true, "avatars": [{"match": "ops/*", "builtin": "cat"}, {"match": "ops/x", "file": "x.png"}]}`
	for _, choice := range []string{"owl", "default", "none"} {
		path := avatarConfig(t, config)
		if _, err := tui.SaveAvatar("ops/x", choice); err != nil {
			t.Fatal(err)
		}
		want, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}

		path = avatarConfig(t, config)
		stdout, stderr, code := runCLI("avatars", "set", "ops/x", choice)
		if code != 0 {
			t.Fatalf("%s: code = %d, stderr = %s", choice, code, stderr)
		}
		if stdout != "avatar for ops/x: "+choice+" (saved to config)\n" {
			t.Fatalf("%s: stdout = %q", choice, stdout)
		}
		got, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		if string(got) != string(want) {
			t.Fatalf("%s: CLI wrote\n%s\npicker wrote\n%s", choice, got, want)
		}
	}
}

func TestAvatarsSetErrors(t *testing.T) {
	const broken = `{"avatars": [`
	path := avatarConfig(t, broken)
	if _, stderr, code := runCLI("avatars", "set", "ops/x", "owl"); code != 1 || !strings.Contains(stderr, "parse decide config") {
		t.Fatalf("broken config: code %d stderr %q", code, stderr)
	}
	if data, _ := os.ReadFile(path); string(data) != broken {
		t.Fatalf("broken config changed to %q", data)
	}
	avatarConfig(t, `{}`)
	if _, stderr, code := runCLI("avatars", "set", "ops/x", "nope"); code != 1 || !strings.Contains(stderr, `unknown built-in avatar "nope"`) {
		t.Fatalf("unknown id: code %d stderr %q", code, stderr)
	}
	for _, args := range [][]string{{"avatars", "set"}, {"avatars", "set", "ops/x"}, {"avatars", "set", "a", "owl", "x"}} {
		if _, _, code := runCLI(args...); code != 2 {
			t.Errorf("%q: code = %d, want 2", args, code)
		}
	}
}

func TestAvatarsSetLocalFlagWritesThisMachine(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub"}`)
	stdout, stderr, code := runCLI("avatars", "set", "--local", "ops/x", "owl")
	if code != 0 {
		t.Fatalf("code = %d, stderr = %s", code, stderr)
	}
	if stdout != "avatar for ops/x: owl (saved to config)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if fake.Read("argv") != "" {
		t.Fatal("--local ran ssh")
	}
	data, err := os.ReadFile(filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide", "config.json"))
	if err != nil || !strings.Contains(string(data), `"hub": "me@hub"`) || !strings.Contains(string(data), `"builtin": "owl"`) {
		t.Fatalf("local config = %s, %v", data, err)
	}
}

func TestAvatarsSetInHubModeWritesTheHub(t *testing.T) {
	_, _ = setupHub(t, `{"hub": "me@hub"}`)
	_, _, hubCfg := hubDecide(t, `{}`)
	client := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide", "config.json")
	before, err := os.ReadFile(client)
	if err != nil {
		t.Fatal(err)
	}
	stdout, stderr, code := runCLI("avatars", "set", "ops/x", "owl")
	if code != 0 {
		t.Fatalf("code = %d, stderr = %s", code, stderr)
	}
	if stdout != "avatar for ops/x: owl (saved on hub)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if got, err := os.ReadFile(client); err != nil || string(got) != string(before) {
		t.Fatalf("client config = %s, %v", got, err)
	}
	hub, err := os.ReadFile(filepath.Join(hubCfg, "decide", "config.json"))
	if err != nil || !strings.Contains(string(hub), `"builtin": "owl"`) || !strings.Contains(string(hub), `"match": "ops/x"`) {
		t.Fatalf("hub config = %s, %v", hub, err)
	}
}

func TestAvatarsSetNotesALocalOverride(t *testing.T) {
	_, _ = setupHub(t, `{"hub": "me@hub", "avatars": [{"match": "ops/x", "builtin": "cat"}]}`)
	_, _, hubCfg := hubDecide(t, `{}`)
	stdout, stderr, code := runCLI("avatars", "set", "ops/x", "owl")
	if code != 0 {
		t.Fatalf("code = %d, stderr = %s", code, stderr)
	}
	want := "avatar for ops/x: owl (saved on hub; a local rule for ops/x overrides it here)\n"
	if stdout != want {
		t.Fatalf("stdout = %q", stdout)
	}
	client, err := os.ReadFile(filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide", "config.json"))
	if err != nil || !strings.Contains(string(client), `"builtin": "cat"`) || strings.Contains(string(client), "owl") {
		t.Fatalf("client config = %s, %v", client, err)
	}
	hub, err := os.ReadFile(filepath.Join(hubCfg, "decide", "config.json"))
	if err != nil || !strings.Contains(string(hub), `"builtin": "owl"`) {
		t.Fatalf("hub config = %s, %v", hub, err)
	}
}

func TestAvatarsSetHubRejectsABadChoiceAndAnUnreachableHub(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub", "hubTimeout": "1s"}`)
	_, _, hubCfg := hubDecide(t, `{}`)
	if _, stderr, code := runCLI("avatars", "set", "ops/x", "nope"); code != 1 || !strings.Contains(stderr, `unknown built-in avatar "nope"`) {
		t.Fatalf("bad choice: code %d stderr %q", code, stderr)
	}
	hub, err := os.ReadFile(filepath.Join(hubCfg, "decide", "config.json"))
	if err != nil || strings.Contains(string(hub), "ops/x") {
		t.Fatalf("hub config = %s, %v", hub, err)
	}
	fake.Write("down", "")
	fake.Unreachable()
	if _, stderr, code := runCLI("avatars", "set", "ops/x", "owl"); code != 1 || !strings.Contains(stderr, "unreachable") {
		t.Fatalf("unreachable: code %d stderr %q", code, stderr)
	}
	client, err := os.ReadFile(filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide", "config.json"))
	if err != nil || strings.Contains(string(client), "owl") {
		t.Fatalf("client config = %s, %v", client, err)
	}
}

func TestAvatarsListIDs(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	stdout, stderr, code := runCLI("avatars", "list", "--ids")
	if code != 0 {
		t.Fatalf("code = %d, stderr = %s", code, stderr)
	}
	if want := strings.Join(avatar.BuiltinIDs(), "\n") + "\n"; stdout != want {
		t.Fatalf("stdout = %q", stdout)
	}
}
