package main

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestSetupDevReadmeEntry(t *testing.T) {
	top := readSetupFile(t, filepath.Join("..", "README.org"))
	lines := strings.Split(top, "\n")
	if len(lines) < 10 {
		t.Fatalf("top README has %d lines", len(lines))
	}
	head := strings.Join(lines[:10], "\n")
	for _, want := range []string{"macOS", "Debian", "Termux", "Qubes", "ensure.sh"} {
		if !strings.Contains(head, want) {
			t.Errorf("first 10 lines lack %q", want)
		}
	}
	if strings.Contains(head, "reboot.org") {
		t.Error("reboot runbook is in the first 10 lines")
	}
	const curl = "curl -fsSL https://raw.githubusercontent.com/jaeyeom/experimental/main/devtools/setup-dev/ansible/bootstrap.sh"
	if strings.Count(top, curl) != 1 {
		t.Errorf("bootstrap curl appears %d times in the top README", strings.Count(top, curl))
	}
	for _, want := range []string{
		"./ensure.sh profile-minimal",
		"./ensure.sh rg",
		"./ensure.sh all",
		"sudo",
		"~/.cache/setup-dev/ensure_sh.log",
		"qubes/",
		"trim-debian.sh",
		"qubes-templatevm",
		"file:ansible/docs/hub-setup.org",
		"file:../agentbus/reboot.org",
	} {
		if !strings.Contains(top, want) {
			t.Errorf("top README lacks %q", want)
		}
	}
	quick := strings.Index(top, "* Quick start")
	reboot := strings.Index(top, "reboot.org")
	if quick < 0 || reboot < quick {
		t.Fatal("reboot runbook must follow Quick start")
	}
}

func TestAnsibleReadmeOrder(t *testing.T) {
	doc := readSetupFile(t, "README.org")
	hub := readSetupFile(t, filepath.Join("docs", "hub-setup.org"))
	if strings.Contains(doc, "bootstrap.sh") {
		t.Error("ansible README repeats the bootstrap instructions")
	}
	if strings.Contains(doc, "variuos") || strings.Contains(doc, "* Manual Playbooks") {
		t.Error("ansible README still has the old typo or the filename list heading")
	}
	simple := strings.Index(doc, "./ensure.sh rg")
	from := strings.Index(doc, "--from")
	sudors := strings.Index(doc, "sudo-rs")
	general := strings.Index(doc, "community.general")
	if simple < 0 || from < simple || sudors < simple || general < simple {
		t.Fatalf("simple usage must come before flags, --from, and troubleshooting (rg=%d from=%d sudo-rs=%d community.general=%d)", simple, from, sudors, general)
	}
	for _, want := range []string{
		"packages_data.go",
		"make generate-ansible",
		"verified-run",
		"profile-minimal.yml",
		"profile-emacs-daily.yml",
		"profile-antigravity.yml",
		"profile-cloudflare-dev.yml",
		"file:docs/hub-setup.org",
		"AUTO-GENERATED",
	} {
		if !strings.Contains(doc, want) {
			t.Errorf("ansible README lacks %q", want)
		}
	}
	for _, want := range []string{"agentbusd", "decide", "herdr", "client_machine_label", "hub_socket"} {
		if !strings.Contains(hub, want) {
			t.Errorf("hub doc lacks %q", want)
		}
	}
}

func readSetupFile(t *testing.T, rel string) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), rel))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "setup-dev", "ansible", rel))
	}
	for _, path := range candidates {
		body, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		return string(body)
	}
	t.Fatalf("file %s not found", rel)
	return ""
}
