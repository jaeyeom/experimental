package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestSkillSetupPointer(t *testing.T) {
	if !strings.Contains(skillText, "## Setup") {
		t.Fatalf("skill.md missing ## Setup section:\n%s", skillText)
	}
	if !strings.Contains(skillText, "references/setup.md") {
		t.Fatalf("skill.md missing references/setup.md pointer:\n%s", skillText)
	}
}

func TestSkillEmbeddedReferences(t *testing.T) {
	tool := newDecideTool()
	if len(tool.SkillFiles) == 0 {
		t.Fatal("newDecideTool().SkillFiles is empty")
	}

	byPath := make(map[string]string, len(tool.SkillFiles))
	for _, f := range tool.SkillFiles {
		if strings.TrimSpace(f.Content) == "" {
			t.Fatalf("embedded skill file %q is empty", f.Path)
		}
		byPath[f.Path] = f.Content
	}

	wantFiles := []string{
		"references/setup.md",
		"references/setup-hub.md",
		"references/setup-tui-emacs.md",
		"references/setup-web-mobile.md",
		"references/setup-delivery.md",
		"references/setup-companions.md",
	}
	for _, want := range wantFiles {
		if _, ok := byPath[want]; !ok {
			t.Fatalf("missing embedded skill reference %q", want)
		}
	}

	// Verify every references/*.md link mentioned in skill.md and setup.md exists,
	// and every embedded setup-*.md branch guide is linked from setup.md.
	refRe := regexp.MustCompile(`references/[a-z0-9-]+\.md`)
	setupMD := byPath["references/setup.md"]
	for _, src := range []struct {
		name string
		body string
	}{
		{"skill.md", skillText},
		{"references/setup.md", setupMD},
	} {
		for _, match := range refRe.FindAllString(src.body, -1) {
			if _, ok := byPath[match]; !ok {
				t.Fatalf("%s references missing file %q", src.name, match)
			}
		}
	}
	linkedFromSetup := refRe.FindAllString(setupMD, -1)
	for path := range byPath {
		if path == "references/setup.md" {
			continue
		}
		if !slices.Contains(linkedFromSetup, path) {
			t.Fatalf("references/setup.md does not link branch guide %q", path)
		}
	}

	// Verify critical setup content against documented commands and config keys.
	checks := map[string][]string{
		"references/setup.md": {
			"decide version",
			"decide doctor --json",
			"go install github.com/jaeyeom/experimental/devtools/decide/cmd/decide@latest",
			"devtools/setup-dev/ansible/setup-decide.yml",
			"decide skill install --agent all",
			"decide ask --class note",
		},
		"references/setup-hub.md": {
			"decide serve --stdio",
			`"hub"`,
			`"hubTimeout"`,
			`"machine"`,
			`"hubMachine"`,
			"DECIDE_HUB",
			"decide config show --effective",
		},
		"references/setup-tui-emacs.md": {
			"decide inbox",
			"ui.keys",
			"ui.answerAction",
			"ui.fillColumn",
			"avatarDefault",
			"devtools/decide/emacs/decide.el",
			"decide-program",
		},
		"references/setup-web-mobile.md": {
			"decide web setup",
			"web.publicUrl",
			"web.auth.mode",
			"web.push.enabled",
			"web.push.subject",
			"decide doctor",
		},
		"references/setup-delivery.md": {
			"notifyTemplate",
			"sendTemplate",
			"decide follow",
			"recipients",
			"checkouts",
			"targets",
			"transports",
			"--notify-pane pull",
		},
		"references/setup-companions.md": {
			"ui.orgTree",
			"ui.orgSpec",
			"orgctl tree --json",
			"owners",
			"slackwatch",
			"gh-outbox",
			"prsync",
			"decide skill install --agent all",
		},
	}
	for path, wants := range checks {
		body := byPath[path]
		for _, want := range wants {
			if !strings.Contains(body, want) {
				t.Fatalf("%s missing %q", path, want)
			}
		}
	}
}

func TestSkillInstallStatusUninstallCLI(t *testing.T) {
	homeDir := t.TempDir()
	workDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
	t.Setenv("DECIDE_HUB", "")

	for _, root := range []string{".claude", ".agents", ".gemini", ".grok"} {
		if err := os.MkdirAll(filepath.Join(homeDir, root), 0o750); err != nil {
			t.Fatal(err)
		}
	}

	exe := filepath.Join(t.TempDir(), "decide")
	if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
		t.Fatal(err)
	}
	tool := newDecideTool()
	tool.HomeDir = func() (string, error) { return homeDir, nil }
	tool.WorkDir = func() (string, error) { return workDir, nil }
	tool.Executable = func() (string, error) { return exe, nil }
	tool.LookPath = func(file string) (string, error) {
		if file == "decide" {
			return exe, nil
		}
		return "/bin/" + file, nil
	}

	var stdout, stderr bytes.Buffer
	code, handled := tool.Handle(context.Background(), []string{"skill", "install", "--agent", "all"}, &stdout, &stderr)
	if !handled || code != 0 || stderr.Len() != 0 {
		t.Fatalf("skill install = (%d, %v), stderr=%s, stdout=%s", code, handled, stderr.String(), stdout.String())
	}

	claudeDir := filepath.Join(homeDir, ".claude", "skills", "decide")
	claudeSkill := filepath.Join(claudeDir, "SKILL.md")
	if _, err := os.Stat(claudeSkill); err != nil {
		t.Fatalf("installed SKILL.md missing: %v", err)
	}
	for _, f := range tool.SkillFiles {
		dest := filepath.Join(claudeDir, filepath.FromSlash(f.Path))
		got, err := os.ReadFile(dest)
		if err != nil {
			t.Fatalf("installed companion file %s missing: %v", dest, err)
		}
		if string(got) != f.Content {
			t.Fatalf("installed companion file %s content mismatch", dest)
		}
	}

	stdout.Reset()
	stderr.Reset()
	code, handled = tool.Handle(context.Background(), []string{"skill", "status", "--json"}, &stdout, &stderr)
	if !handled || code != 0 {
		t.Fatalf("skill status = (%d, %v), stderr=%s", code, handled, stderr.String())
	}
	var stOut toolcli.SkillStatusOutput
	if err := json.Unmarshal(stdout.Bytes(), &stOut); err != nil {
		t.Fatalf("unmarshal skill status: %v\n%s", err, stdout.String())
	}
	for _, e := range stOut.Entries {
		if e.Scope == toolcli.ScopeUser && e.Status != "current" {
			t.Fatalf("status for %s = %q, want current", e.Path, e.Status)
		}
	}

	stdout.Reset()
	stderr.Reset()
	code, handled = tool.Handle(context.Background(), []string{"skill", "uninstall", "--agent", "all"}, &stdout, &stderr)
	if !handled || code != 0 {
		t.Fatalf("skill uninstall = (%d, %v), stderr=%s", code, handled, stderr.String())
	}
	if _, err := os.Stat(claudeDir); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("expected %s to be removed after uninstall, stat err=%v", claudeDir, err)
	}
}
