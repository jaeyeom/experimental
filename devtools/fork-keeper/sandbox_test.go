package forkkeeper

import (
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestResolveSandboxMode(t *testing.T) {
	found := func(want string) func(string) (string, error) {
		return func(name string) (string, error) {
			if name == want {
				return "/usr/bin/" + name, nil
			}
			return "", errors.New("not found")
		}
	}
	noneFound := func(string) (string, error) {
		return "", errors.New("not found")
	}

	tests := []struct {
		name     string
		mode     string
		goos     string
		lookPath func(string) (string, error)
		want     string
		wantErr  bool
	}{
		{name: "auto darwin seatbelt", mode: "auto", goos: "darwin", lookPath: found("sandbox-exec"), want: "seatbelt"},
		{name: "empty mode uses auto", mode: "", goos: "darwin", lookPath: found("sandbox-exec"), want: "seatbelt"},
		{name: "auto darwin missing", mode: "auto", goos: "darwin", lookPath: noneFound, wantErr: true},
		{name: "auto linux bwrap", mode: "auto", goos: "linux", lookPath: found("bwrap"), want: "bwrap"},
		{name: "auto linux missing", mode: "auto", goos: "linux", lookPath: noneFound, wantErr: true},
		{name: "auto unsupported os", mode: "auto", goos: "windows", lookPath: noneFound, wantErr: true},
		{name: "explicit none", mode: "none", goos: "linux", lookPath: noneFound, want: "none"},
		{name: "explicit seatbelt", mode: "seatbelt", goos: "darwin", lookPath: found("sandbox-exec"), want: "seatbelt"},
		{name: "explicit seatbelt missing", mode: "seatbelt", goos: "darwin", lookPath: noneFound, wantErr: true},
		{name: "explicit bwrap", mode: "bwrap", goos: "linux", lookPath: found("bwrap"), want: "bwrap"},
		{name: "explicit bwrap missing", mode: "bwrap", goos: "linux", lookPath: noneFound, wantErr: true},
		{name: "unknown mode", mode: "docker", goos: "linux", lookPath: noneFound, wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := ResolveSandboxMode(tc.mode, tc.goos, tc.lookPath)
			if (err != nil) != tc.wantErr {
				t.Fatalf("ResolveSandboxMode(%q, %q) err = %v, wantErr %v", tc.mode, tc.goos, err, tc.wantErr)
			}
			if got != tc.want {
				t.Errorf("ResolveSandboxMode(%q, %q) = %q, want %q", tc.mode, tc.goos, got, tc.want)
			}
		})
	}
}

func TestWrapAgentSandboxWith(t *testing.T) {
	found := func(want string) func(string) (string, error) {
		return func(name string) (string, error) {
			if name == want {
				return "/usr/bin/" + name, nil
			}
			return "", errors.New("not found")
		}
	}

	args := []string{"my-reviewer", "--flag"}
	noneWrapped, err := wrapAgentSandboxWith("none", "linux", found("bwrap"), "/home/alice", "/tmp/git", args)
	if err != nil || !slices.Equal(noneWrapped, args) {
		t.Fatalf("none mode: got=%v err=%v", noneWrapped, err)
	}

	seatbeltWrapped, err := wrapAgentSandboxWith("auto", "darwin", found("sandbox-exec"), "/home/alice", "/tmp/git", args)
	if err != nil {
		t.Fatalf("seatbelt wrap: %v", err)
	}
	if len(seatbeltWrapped) != 6 || seatbeltWrapped[0] != "sandbox-exec" || seatbeltWrapped[1] != "-p" || seatbeltWrapped[3] != "--" {
		t.Fatalf("unexpected seatbelt args: %v", seatbeltWrapped)
	}

	bwrapWrapped, err := wrapAgentSandboxWith("auto", "linux", found("bwrap"), "/home/alice", "/tmp/git", args)
	if err != nil {
		t.Fatalf("bwrap wrap: %v", err)
	}
	if bwrapWrapped[0] != "bwrap" || !containsSubslice(bwrapWrapped, []string{"--", "my-reviewer", "--flag"}) {
		t.Fatalf("unexpected bwrap args: %v", bwrapWrapped)
	}
}

func TestBuildSeatbeltProfile(t *testing.T) {
	home := "/Users/alice"
	gitDir := "/Users/alice/.cache/fork-keeper/demo.git"
	profile := buildSeatbeltProfileWithEnv(home, gitDir, []string{
		"XDG_CONFIG_HOME=/Users/alice/.xdg",
		"GH_CONFIG_DIR=/Users/alice/.gh-custom",
	})

	for _, want := range []string{
		`(version 1)`,
		`(allow default)`,
		`(deny file-read* file-write*`,
		`(subpath "/Users/alice/.ssh")`,
		`(subpath "/Users/alice/.config/gh")`,
		`(subpath "/Users/alice/.xdg/gh")`,
		`(subpath "/Users/alice/.gh-custom")`,
		`(subpath "/Users/alice/.gnupg")`,
		`(subpath "/Users/alice/.aws")`,
		`(subpath "/Users/alice/.cache/fork-keeper/demo.git")`,
		`(literal "/Users/alice/.netrc")`,
		`(literal "/Users/alice/.git-credentials")`,
		`(literal "/Users/alice/.config/git/credentials")`,
		`(literal "/Users/alice/.xdg/git/credentials")`,
	} {
		if !strings.Contains(profile, want) {
			t.Errorf("seatbelt profile missing %q:\n%s", want, profile)
		}
	}

	escaped := quoteSBPL(`/tmp/a"b\c`)
	if escaped != `"/tmp/a\"b\\c"` {
		t.Errorf("quoteSBPL = %s", escaped)
	}
}

func TestBuildBwrapArgs(t *testing.T) {
	home := t.TempDir()
	gitDir := filepath.Join(t.TempDir(), "demo.git")
	if err := os.MkdirAll(gitDir, 0o700); err != nil {
		t.Fatal(err)
	}
	sshDir := filepath.Join(home, ".ssh")
	ghDir := filepath.Join(home, ".config", "gh")
	if err := os.MkdirAll(sshDir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(ghDir, 0o700); err != nil {
		t.Fatal(err)
	}
	netrc := filepath.Join(home, ".netrc")
	if err := os.WriteFile(netrc, []byte("machine github.com\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	got := buildBwrapArgsWithEnv(home, gitDir, nil, []string{"claude", "-p", "sync"})
	wantContains := [][]string{
		{"--tmpfs", sshDir},
		{"--tmpfs", ghDir},
		{"--tmpfs", gitDir},
		{"--ro-bind", "/dev/null", netrc},
		{"--", "claude", "-p", "sync"},
	}
	for _, seq := range wantContains {
		if !containsSubslice(got, seq) {
			t.Errorf("bwrap args missing %v:\n%v", seq, got)
		}
	}
	// Non-existent dirs/files should not be passed to bwrap.
	if slices.Contains(got, filepath.Join(home, ".aws")) {
		t.Errorf("bwrap args unexpectedly included missing .aws dir: %v", got)
	}
}

func containsSubslice(haystack, needle []string) bool {
	if len(needle) == 0 || len(needle) > len(haystack) {
		return false
	}
	for i := 0; i <= len(haystack)-len(needle); i++ {
		if slices.Equal(haystack[i:i+len(needle)], needle) {
			return true
		}
	}
	return false
}
