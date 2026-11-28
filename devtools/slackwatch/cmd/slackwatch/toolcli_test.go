package main

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

func TestSlackwatchToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
			t.Setenv("SLACKWATCH_CONFIG", "")
			exe := filepath.Join(t.TempDir(), "slackwatch")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newSlackwatchTool("")
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "slackwatch" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			if tool.ModulePath != "github.com/jaeyeom/experimental" {
				t.Fatalf("ModulePath = %q, want %q", tool.ModulePath, "github.com/jaeyeom/experimental")
			}
			return tool
		},
	})
}

func TestDoctorWarnsOnStaleDaemonBinary(t *testing.T) {
	dir := t.TempDir()
	tokenPath := filepath.Join(dir, "tokens.json")
	if err := os.WriteFile(tokenPath, []byte(`{"appToken":"xapp-1","userToken":"xoxp-1"}`), 0o600); err != nil {
		t.Fatal(err)
	}
	dbPath := filepath.Join(dir, "slackwatch.db")
	cfgPath := filepath.Join(dir, "config.json")
	cfgBytes, err := json.Marshal(map[string]any{
		"tokenFile": tokenPath,
		"dbPath":    dbPath,
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, cfgBytes, 0o600); err != nil {
		t.Fatal(err)
	}
	store, err := slackwatch.OpenStore(dbPath)
	if err != nil {
		t.Fatal(err)
	}
	now := time.Now()
	if err := store.WriteStatus(slackwatch.Status{
		PID:               os.Getpid(),
		StartedAt:         now,
		HeartbeatAt:       now,
		HeartbeatInterval: slackwatch.Duration{Duration: time.Minute},
		Connection:        slackwatch.ConnConnected,
		BinaryInfo:        "old-build-info-from-previous-version",
	}); err != nil {
		t.Fatal(err)
	}
	store.Close()

	tool := newSlackwatchTool(cfgPath)
	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(context.Background(), []string{"doctor", "--offline", "--json"}, &out, &errBuf)
	if !ok {
		t.Fatalf("doctor not handled: %s", errBuf.String())
	}
	var doc toolcli.DoctorOutput
	if err := json.Unmarshal(out.Bytes(), &doc); err != nil {
		t.Fatalf("decode doctor json: %v (code=%d out=%s)", err, code, out.String())
	}
	var found bool
	for _, c := range doc.Checks {
		if c.Name == "daemon-binary" {
			found = true
			if c.Status != toolcli.StatusWarn {
				t.Errorf("daemon-binary status = %s, want warn (detail=%q)", c.Status, c.Detail)
			}
			if !strings.Contains(c.Detail, "slackwatch binary updated; restart needed") {
				t.Errorf("daemon-binary detail = %q", c.Detail)
			}
		}
	}
	if !found {
		t.Fatalf("doctor output missing daemon-binary check: %+v", doc.Checks)
	}
}

func TestSlackwatchSkillInstallWithAndWithoutInbound(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(home, ".config"))
	t.Setenv("SLACKWATCH_CONFIG", "")
	if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o750); err != nil {
		t.Fatal(err)
	}

	cfgPath := filepath.Join(home, "config.json")
	if err := os.WriteFile(cfgPath, []byte(`{"outbox": "ops/outbox"}`), 0o600); err != nil {
		t.Fatal(err)
	}

	mainSkill := filepath.Join(home, ".claude", "skills", "slackwatch", "SKILL.md")
	triageSkill := filepath.Join(home, ".claude", "skills", "slackwatch-triage", "SKILL.md")

	// Without inbound: only slackwatch is installed.
	var out, errBuf bytes.Buffer
	if code := run([]string{"--config", cfgPath, "skill", "install", "--agent", "all", "--force", "--dry-run"}, &out, &errBuf); code != 0 {
		t.Fatalf("dry-run without inbound: code=%d stderr=%s", code, errBuf.String())
	}
	if !strings.Contains(out.String(), "would install claude (user): "+mainSkill) {
		t.Fatalf("dry-run missing main skill:\n%s", out.String())
	}
	if strings.Contains(out.String(), "slackwatch-triage") {
		t.Fatalf("dry-run unexpectedly included triage without inbound:\n%s", out.String())
	}

	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "install", "--agent", "all", "--force"}, &out, &errBuf); code != 0 {
		t.Fatalf("install without inbound: code=%d stderr=%s", code, errBuf.String())
	}
	if _, err := os.Stat(triageSkill); err == nil {
		t.Fatalf("triage skill should not exist when inbound is unconfigured")
	}

	// Enable inbound: both slackwatch and slackwatch-triage are handled.
	if err := os.WriteFile(cfgPath, []byte(`{"outbox": "ops/outbox", "inbound": {"target": "inbox-triage"}}`), 0o600); err != nil {
		t.Fatal(err)
	}

	// `slackwatch skill triage` remains backwards-compatible.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "triage"}, &out, &errBuf); code != 0 {
		t.Fatalf("skill triage: code=%d stderr=%s", code, errBuf.String())
	}
	if !strings.Contains(out.String(), "You are `inbox-triage`.") {
		t.Fatalf("unexpected triage output:\n%s", out.String())
	}

	// Foreign triage file blocks non-forced install.
	if err := os.MkdirAll(filepath.Dir(triageSkill), 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(triageSkill, []byte("---\nname: slackwatch-triage\n---\nlegacy\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "install", "--agent", "all"}, &out, &errBuf); code != 2 {
		t.Fatalf("expected refusal on foreign triage file, got %d (stderr=%s)", code, errBuf.String())
	}

	// Dry-run with inbound reports both skills.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "install", "--agent", "all", "--force", "--dry-run"}, &out, &errBuf); code != 0 {
		t.Fatalf("dry-run with inbound: code=%d stderr=%s", code, errBuf.String())
	}
	if !strings.Contains(out.String(), "would install claude (user): "+mainSkill) ||
		!strings.Contains(out.String(), "would install claude (user): "+triageSkill) {
		t.Fatalf("dry-run missing expected paths:\n%s", out.String())
	}

	// --force installs both skills cleanly.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "install", "--agent", "all", "--force"}, &out, &errBuf); code != 0 {
		t.Fatalf("force install with inbound: code=%d stderr=%s", code, errBuf.String())
	}
	triageBytes, err := os.ReadFile(triageSkill)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(triageBytes), "toolcli-tool: slackwatch-triage") ||
		!strings.Contains(string(triageBytes), "You are `inbox-triage`.") {
		t.Fatalf("unexpected installed triage file:\n%s", string(triageBytes))
	}

	// `skill status --json` reports both skills as current.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "status", "--json"}, &out, &errBuf); code != 0 {
		t.Fatalf("status --json: code=%d stderr=%s", code, errBuf.String())
	}
	var st toolcli.SkillStatusOutput
	if err := json.Unmarshal(out.Bytes(), &st); err != nil {
		t.Fatal(err)
	}
	byPath := map[string]toolcli.SkillStatusEntry{}
	for _, e := range st.Entries {
		byPath[e.Path] = e
	}
	if byPath[mainSkill].Status != "current" || byPath[mainSkill].Skill != "slackwatch" {
		t.Fatalf("unexpected main skill status: %+v", byPath[mainSkill])
	}
	if byPath[triageSkill].Status != "current" || byPath[triageSkill].Skill != "slackwatch-triage" {
		t.Fatalf("unexpected triage skill status: %+v", byPath[triageSkill])
	}

	// Changing inbound target marks both skills stale.
	if err := os.WriteFile(cfgPath, []byte(`{"outbox": "ops/outbox", "inbound": {"target": "other-triage"}}`), 0o600); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "status", "--json"}, &out, &errBuf); code != 0 {
		t.Fatalf("status --json after change: code=%d stderr=%s", code, errBuf.String())
	}
	if err := json.Unmarshal(out.Bytes(), &st); err != nil {
		t.Fatal(err)
	}
	byPath = map[string]toolcli.SkillStatusEntry{}
	for _, e := range st.Entries {
		byPath[e.Path] = e
	}
	if byPath[mainSkill].Status != "stale" || byPath[triageSkill].Status != "stale" {
		t.Fatalf("expected both skills stale, got main=%+v triage=%+v", byPath[mainSkill], byPath[triageSkill])
	}

	// Uninstall removes both skills.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"--config", cfgPath, "skill", "uninstall", "--agent", "all"}, &out, &errBuf); code != 0 {
		t.Fatalf("uninstall: code=%d stderr=%s", code, errBuf.String())
	}
	for _, p := range []string{mainSkill, triageSkill} {
		if _, err := os.Stat(p); err == nil {
			t.Fatalf("expected %s to be removed", p)
		}
	}
}
