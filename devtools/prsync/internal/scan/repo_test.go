package scan

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func TestParseGitHubRemote(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		raw  string
		want string
		ok   bool
	}{
		{name: "ssh scp", raw: "git@github.com:acme/widgets.git", want: "acme/widgets", ok: true},
		{name: "https with git suffix", raw: "https://github.com/acme/widgets.git", want: "acme/widgets", ok: true},
		{name: "https", raw: "https://github.com/acme/widgets", want: "acme/widgets", ok: true},
		{name: "https with userinfo", raw: "https://git" + ":" + "x@github.com/acme/widgets.git", want: "acme/widgets", ok: true},
		{name: "ssh url", raw: "ssh://git@github.com/acme/widgets.git", want: "acme/widgets", ok: true},
		{name: "not github", raw: "https://gitlab.com/acme/widgets.git", ok: false},
		{name: "trailing newline", raw: "git@github.com:acme/widgets.git\n", want: "acme/widgets", ok: true},
		{name: "empty", raw: "", ok: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, ok := parseGitHubRemote(tc.raw)
			if ok != tc.ok || got != tc.want {
				t.Fatalf("parseGitHubRemote(%q) = %q, %v, want %q, %v", tc.raw, got, ok, tc.want, tc.ok)
			}
		})
	}
}

func TestGitHubRepoFromCWD(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not installed")
	}
	dir := t.TempDir()
	cmd := exec.Command("git", "init")
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git init: %v\n%s", err, out)
	}
	remote := exec.Command("git", "remote", "add", "origin", "git@github.com:acme/widgets.git")
	remote.Dir = dir
	if out, err := remote.CombinedOutput(); err != nil {
		t.Fatalf("git remote add: %v\n%s", err, out)
	}
	got, err := GitHubRepoFromCWD(dir)
	if err != nil {
		t.Fatalf("GitHubRepoFromCWD() unexpected error: %v", err)
	}
	if got != "acme/widgets" {
		t.Fatalf("GitHubRepoFromCWD() = %q, want acme/widgets", got)
	}

	sub := filepath.Join(dir, "sub")
	if err := os.Mkdir(sub, 0o755); err != nil {
		t.Fatal(err)
	}
	got, err = GitHubRepoFromCWD(sub)
	if err != nil {
		t.Fatalf("GitHubRepoFromCWD(sub) unexpected error: %v", err)
	}
	if got != "acme/widgets" {
		t.Fatalf("GitHubRepoFromCWD(sub) = %q, want acme/widgets", got)
	}
}
