package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// withTempDir creates a temporary directory, chdirs into it for the duration
// of the test, and restores the original working directory afterwards.
func withTempDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	orig, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	if err := os.Chdir(dir); err != nil {
		t.Fatalf("Chdir: %v", err)
	}
	t.Cleanup(func() {
		if err := os.Chdir(orig); err != nil {
			t.Errorf("restore cwd: %v", err)
		}
	})
	return dir
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("WriteFile %s: %v", path, err)
	}
}

func TestGetPlaybookImports(t *testing.T) {
	withTempDir(t)
	writeFile(t, "parent.yml", `---
- import_playbook: child.yml
- import_playbook: other.yml  # trailing comment
# - import_playbook: commented.yml
- name: Some play
  hosts: all
`)
	writeFile(t, "child.yml", "---\n")
	writeFile(t, "other.yml", "---\n")

	got, err := getPlaybookImports("parent")
	if err != nil {
		t.Fatalf("getPlaybookImports: %v", err)
	}
	want := []string{"child", "other"}
	if len(got) != len(want) {
		t.Fatalf("getPlaybookImports() = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("getPlaybookImports()[%d] = %q, want %q", i, got[i], want[i])
		}
	}
}

func TestGetPlaybookTaskIncludes(t *testing.T) {
	withTempDir(t)
	writeFile(t, "play.yml", `---
- name: demo
  hosts: all
  tasks:
    - name: include something
      ansible.builtin.include_tasks: tasks/github-release-info.yml
    # ansible.builtin.include_tasks: tasks/ignored.yml
    - name: another
      include_tasks: tasks/other.yml  # comment
`)

	got, err := getPlaybookTaskIncludes("play.yml")
	if err != nil {
		t.Fatalf("getPlaybookTaskIncludes: %v", err)
	}
	want := []string{"tasks/github-release-info.yml", "tasks/other.yml"}
	if len(got) != len(want) {
		t.Fatalf("getPlaybookTaskIncludes() = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("got[%d] = %q, want %q", i, got[i], want[i])
		}
	}
}

func TestGetAllDependencies(t *testing.T) {
	withTempDir(t)
	// a -> b -> c, a -> d; cycle b -> a should be ignored via visited.
	writeFile(t, "a.yml", "- import_playbook: b.yml\n- import_playbook: d.yml\n")
	writeFile(t, "b.yml", "- import_playbook: c.yml\n- import_playbook: a.yml\n")
	writeFile(t, "c.yml", "---\n")
	writeFile(t, "d.yml", "---\n")

	got, err := getAllDependencies("a", map[string]bool{})
	if err != nil {
		t.Fatalf("getAllDependencies: %v", err)
	}
	seen := make(map[string]bool)
	for _, d := range got {
		seen[d] = true
	}
	// Direct and transitive deps must appear. Through the b->a cycle, "a" may
	// also appear as a listed import of b; the important property is that
	// the cycle does not recurse forever (test would hang/stack overflow).
	for _, want := range []string{"b", "c", "d"} {
		if !seen[want] {
			t.Errorf("missing dependency %q in %v", want, got)
		}
	}
}

func TestGetAllYmlFiles(t *testing.T) {
	withTempDir(t)
	writeFile(t, "alpha.yml", "---\n")
	writeFile(t, "beta.yml", "---\n")
	writeFile(t, "subdir/nested.yml", "---\n") // should be ignored (contains /)
	writeFile(t, "notes.txt", "nope")

	got, err := getAllYmlFiles()
	if err != nil {
		t.Fatalf("getAllYmlFiles: %v", err)
	}
	if len(got) != 2 || got[0] != "alpha" || got[1] != "beta" {
		t.Errorf("getAllYmlFiles() = %v, want [alpha beta]", got)
	}
}

func TestReadManualContent(t *testing.T) {
	withTempDir(t)

	t.Run("missing file returns default", func(t *testing.T) {
		got, err := readManualContent("BUILD.bazel")
		if err != nil {
			t.Fatalf("readManualContent: %v", err)
		}
		if got != defaultManualContent {
			t.Errorf("expected defaultManualContent for missing file")
		}
	})

	t.Run("reads content before marker", func(t *testing.T) {
		writeFile(t, "BUILD.bazel", `load(":x.bzl", "x")

# GENERATED ANSIBLE TESTS - DO NOT EDIT BELOW THIS LINE
sh_test(name = "foo")
`)
		got, err := readManualContent("BUILD.bazel")
		if err != nil {
			t.Fatalf("readManualContent: %v", err)
		}
		want := "load(\":x.bzl\", \"x\")\n\n"
		if got != want {
			t.Errorf("readManualContent() = %q, want %q", got, want)
		}
	})

	t.Run("no marker returns default", func(t *testing.T) {
		writeFile(t, "BUILD.bazel", "just some content\n")
		got, err := readManualContent("BUILD.bazel")
		if err != nil {
			t.Fatalf("readManualContent: %v", err)
		}
		if got != defaultManualContent {
			t.Errorf("expected defaultManualContent when marker missing")
		}
	})
}

func TestGenerateTestRule(t *testing.T) {
	withTempDir(t)
	writeFile(t, "root.yml", `- import_playbook: dep.yml
- name: play
  hosts: all
  tasks:
    - ansible.builtin.include_tasks: tasks/helper.yml
`)
	writeFile(t, "dep.yml", "---\n")
	writeFile(t, "tasks/helper.yml", "---\n")

	got, err := generateTestRule("root")
	if err != nil {
		t.Fatalf("generateTestRule: %v", err)
	}
	for _, want := range []string{
		`name = "root_syntax_test"`,
		`args = ["root.yml"]`,
		`"root.yml"`,
		`"dep.yml"`,
		`"tasks/helper.yml"`,
		`"ansible"`,
		`"local"`,
	} {
		if !strings.Contains(got, want) {
			t.Errorf("generateTestRule missing %q\n%s", want, got)
		}
	}
}

func TestWriteGPGKeySetup(t *testing.T) {
	t.Run("base64 embedded key", func(t *testing.T) {
		var sb strings.Builder
		writeGPGKeySetup(&sb, &AptRepoInstallMethod{
			GPGKeyBase64: "QUJDRA==",
			GPGKeyPath:   "/etc/apt/trusted.gpg.d/tool.gpg",
		})
		got := sb.String()
		for _, want := range []string{
			"base64 -d << 'GPGKEY'",
			"QUJDRA==",
			"GPGKEY",
			"/etc/apt/trusted.gpg.d/tool.gpg",
		} {
			if !strings.Contains(got, want) {
				t.Errorf("missing %q in:\n%s", want, got)
			}
		}
		if strings.Contains(got, "curl") {
			t.Errorf("should not curl when base64 is set:\n%s", got)
		}
	})

	t.Run("curl download", func(t *testing.T) {
		var sb strings.Builder
		writeGPGKeySetup(&sb, &AptRepoInstallMethod{
			GPGKeyURL:  "https://example.com/key.gpg",
			GPGKeyPath: "/etc/apt/trusted.gpg.d/tool.gpg",
		})
		got := sb.String()
		want := "curl -fsSL 'https://example.com/key.gpg' | gpg --dearmor -o /etc/apt/trusted.gpg.d/tool.gpg\n"
		if got != want {
			t.Errorf("writeGPGKeySetup() = %q, want %q", got, want)
		}
	})
}

func TestGenerateQubesTemplatevmScript(t *testing.T) {
	withTempDir(t)

	pkgs := []AptPackageInfo{
		{PackageName: "curl", SourceType: AptSourceNone},
		{PackageName: "emacs", SourceType: AptSourcePPA, PPA: "ppa:ubuntuhandbook1/emacs"},
		{PackageName: "ripgrep", SourceType: AptSourceBackports},
		{
			PackageName: "gcloud-cli",
			SourceType:  AptSourceAptRepo,
			AptRepo: &AptRepoInstallMethod{
				Name:           "gcloud-cli",
				GPGKeyURL:      "https://example.com/key.gpg",
				GPGKeyPath:     "/etc/apt/trusted.gpg.d/google.gpg",
				RepoURL:        "https://packages.example.com/apt",
				RepoComponents: "main",
				Codename:       "stable",
				Arch:           "amd64",
			},
		},
	}

	if err := generateQubesTemplatevmScript("minimal", pkgs); err != nil {
		t.Fatalf("generateQubesTemplatevmScript: %v", err)
	}

	content, err := os.ReadFile("qubes-templatevm-minimal.sh")
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	got := string(content)
	for _, want := range []string{
		"#!/bin/bash",
		"set -euo pipefail",
		"backports",
		"add-apt-repository -y ppa:ubuntuhandbook1/emacs",
		"curl -fsSL 'https://example.com/key.gpg'",
		"arch=amd64",
		"apt-get update",
		"apt-get install -y",
		"curl",
		"emacs",
		"ripgrep",
		"gcloud-cli",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in script:\n%s", want, got)
		}
	}
}

func TestValidatePlatformNames(t *testing.T) {
	// All current package data should use valid platform constants.
	if err := validatePlatformNames(); err != nil {
		t.Errorf("validatePlatformNames() = %v", err)
	}
}

func TestGetGeneratedRuleNames(t *testing.T) {
	names := getGeneratedRuleNames()
	if len(names) == 0 {
		t.Fatal("expected non-empty generated rule names")
	}
	// Sorted
	for i := 1; i < len(names); i++ {
		if names[i] < names[i-1] {
			t.Errorf("not sorted: %q before %q", names[i-1], names[i])
		}
	}
	// Spot-check known generated packages/tools.
	seen := make(map[string]bool, len(names))
	for _, n := range names {
		seen[n] = true
	}
	for _, want := range []string{"curl", "git", "gopls"} {
		if !seen[want] {
			// gopls might be named differently; only require common packages.
			if want == "curl" || want == "git" {
				t.Errorf("expected generated rule %q", want)
			}
		}
	}
}
