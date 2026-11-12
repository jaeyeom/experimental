package main

import (
	"bytes"
	"strconv"
	"strings"
	"testing"
	"unicode/utf8"

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
