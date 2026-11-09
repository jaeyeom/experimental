package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestMain(m *testing.M) {
	if os.Getenv("AGENTBUS_TEST_HOOK") == "1" {
		os.Exit(run(context.Background(), os.Args[1:], os.Environ(), os.Stdin, os.Stdout, os.Stderr))
	}
	os.Exit(m.Run())
}

func TestInstallHooksLeavesProjectSettings(t *testing.T) {
	home := t.TempDir()
	project := t.TempDir()
	projSettings := []byte("{\"theme\":\"project\"}\n")
	localSettings := []byte("{\"local\":true}\n")
	writeFile(t, filepath.Join(project, ".claude", "settings.json"), projSettings)
	writeFile(t, filepath.Join(project, ".claude", "settings.local.json"), localSettings)
	writeFile(t, filepath.Join(home, ".claude", "settings.json"), []byte(`{
  "theme": "dim",
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [{"type": "command", "command": "/tmp/other"}]
      }
    ]
  }
}
`))
	t.Chdir(project)

	env := []string{"HOME=" + home}
	code, stdout, stderr := runCmd([]string{"install-hooks"}, env)
	if code != 0 {
		t.Fatalf("install-hooks = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	script := filepath.Join(home, ".claude", "hooks", hookScriptName)
	if !strings.Contains(stdout, "installed hook "+script) {
		t.Fatalf("stdout = %q", stdout)
	}
	assertMode(t, script, 0o755)
	scriptBody, err := os.ReadFile(script)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(scriptBody), "command -v agentbus") || !strings.HasPrefix(string(scriptBody), "#!/bin/sh\n") {
		t.Fatalf("script = %q", scriptBody)
	}

	code, stdout, stderr = runCmd([]string{"install-hooks"}, env)
	if code != 0 {
		t.Fatalf("second install-hooks = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	assertBytes(t, filepath.Join(project, ".claude", "settings.json"), projSettings)
	assertBytes(t, filepath.Join(project, ".claude", "settings.local.json"), localSettings)
	if _, err := os.Stat(filepath.Join(home, ".claude", "settings.local.json")); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("user settings.local.json err = %v", err)
	}
	doc := readJSON(t, filepath.Join(home, ".claude", "settings.json"))
	if doc["theme"] != "dim" {
		t.Fatalf("theme = %#v", doc["theme"])
	}
	if sendMessageCommands(t, doc) != 1 || !hasBashHook(t, doc) {
		t.Fatalf("settings = %#v", doc)
	}
}

func TestHookLogsBypassAndExitsZero(t *testing.T) {
	dirs, env := hookEnv(t)
	project := t.TempDir()
	projSettings := []byte("{\"keep\":true}\n")
	localSettings := []byte("{\"local\":true}\n")
	writeFile(t, filepath.Join(project, ".claude", "settings.json"), projSettings)
	writeFile(t, filepath.Join(project, ".claude", "settings.local.json"), localSettings)
	t.Chdir(project)
	code, stdout, stderr := runCmd([]string{"install-hooks"}, env)
	if code != 0 {
		t.Fatalf("install-hooks = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	script := filepath.Join(homeOf(t, env), ".claude", "hooks", hookScriptName)

	stop := startDaemon(t, dirs, daemon.Config{PollInterval: time.Hour})
	defer stop()

	wrapper := t.TempDir()
	writeAgentbusWrapper(t, wrapper)
	childEnv := commandEnv(env, wrapper)
	shortIn := `{"session_id":"sess-1","tool_name":"SendMessage","tool_input":{"to":"ops/inbox","message":"hello from claude"}}`
	code, stdout, stderr = runScript(t, script, shortIn, childEnv)
	if code != 0 || stdout != "" {
		t.Fatalf("hook = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}

	longBody := strings.Repeat("y", 250) + "TAIL"
	longIn := `{"session_id":"sess-2","tool_input":{"recipient":"eng/reviewer","content":"` + longBody + `"}}`
	code, stdout, stderr = runScript(t, script, longIn, childEnv)
	if code != 0 || stdout != "" {
		t.Fatalf("long hook = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}

	events := bypassEvents(t, dirs.StateDir)
	if len(events) != 2 {
		t.Fatalf("bypass events = %#v", events)
	}
	assertBypass(t, events[0], "sess-1", "ops/inbox", "hello from claude")
	detail, _ := events[1]["detail"].(string)
	if events[1]["actor"] != "sess-2" || events[1]["name"] != "eng/reviewer" {
		t.Fatalf("long event = %#v", events[1])
	}
	if !strings.HasPrefix(detail, strings.Repeat("y", 200)) || !strings.HasSuffix(detail, "…") || strings.Contains(detail, "TAIL") {
		t.Fatalf("preview = %q", detail)
	}

	code, stdout, stderr = runScript(t, script, shortIn, commandEnv(env, t.TempDir()))
	if code != 0 || stdout != "" {
		t.Fatalf("missing agentbus = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
	if got := bypassEvents(t, dirs.StateDir); len(got) != 2 {
		t.Fatalf("bypass events after missing agentbus = %d", len(got))
	}
	assertBytes(t, filepath.Join(project, ".claude", "settings.json"), projSettings)
	assertBytes(t, filepath.Join(project, ".claude", "settings.local.json"), localSettings)
}

func TestHookExitsZeroWhenBusIsDown(t *testing.T) {
	_, env := hookEnv(t)
	code, _, stderr := runCmd([]string{"install-hooks"}, env)
	if code != 0 {
		t.Fatalf("install-hooks = %d, stderr %s", code, stderr)
	}
	script := filepath.Join(homeOf(t, env), ".claude", "hooks", hookScriptName)
	wrapper := t.TempDir()
	writeAgentbusWrapper(t, wrapper)
	stdin := `{"session_id":"sess-down","tool_input":{"to":"ops/inbox","message":"pending"}}`
	code, stdout, stderr := runScript(t, script, stdin, commandEnv(env, wrapper))
	if code != 0 || stdout != "" {
		t.Fatalf("hook = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
}

func TestInstallWritesSupervisorWithoutLoading(t *testing.T) {
	stubDaemon(t)
	home := t.TempDir()
	env := []string{
		"HOME=" + home,
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"AGENTBUS_INSTALL_NO_LOAD=1",
	}
	code, stdout, stderr := runCmd([]string{"install"}, env)
	if code != 0 {
		t.Fatalf("install = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	path := installedPath(home)
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	bin := filepath.Join(filepath.Dir(exe), "agentbusd")
	if !strings.Contains(string(data), bin) && !strings.Contains(string(data), "agentbusd") {
		t.Fatalf("unit = %s", data)
	}
	if !strings.Contains(stdout, "wrote "+path) {
		t.Fatalf("stdout = %q", stdout)
	}
	if runtime.GOOS == "darwin" {
		if !strings.Contains(string(data), "<string>"+launchdLabel+"</string>") || !strings.Contains(string(data), "<key>KeepAlive</key>") {
			t.Fatalf("plist = %s", data)
		}
	}
	if runtime.GOOS == "linux" && !strings.Contains(string(data), "WantedBy=default.target") {
		t.Fatalf("unit = %s", data)
	}
}

func TestInstallFailsWithoutRuntimeDir(t *testing.T) {
	home := t.TempDir()
	code, _, stderr := runCmd([]string{"install"}, []string{"HOME=" + home, "AGENTBUS_INSTALL_NO_LOAD=1"})
	if code == 0 || !strings.Contains(stderr, "XDG_RUNTIME_DIR") {
		t.Fatalf("install = %d, stderr %q", code, stderr)
	}
	if _, err := os.Stat(installedPath(home)); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("unit err = %v", err)
	}
}

func TestSupervisorTexts(t *testing.T) {
	vars := [][2]string{{"HOME", "/Users/me"}, {"XDG_RUNTIME_DIR", "/tmp/ab"}}
	plist, err := supervisorBody("darwin", "/bin/agentbusd", vars)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"<string>" + launchdLabel + "</string>",
		"<string>/bin/agentbusd</string>",
		"<key>RunAtLoad</key>",
		"<key>KeepAlive</key>",
		"<string>/tmp/ab</string>",
		"<true/>",
	} {
		if !strings.Contains(plist, want) {
			t.Fatalf("plist missing %s\n%s", want, plist)
		}
	}
	escaped, err := supervisorBody("darwin", "/bin/agentbusd&", nil)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(escaped, "/bin/agentbusd&amp;") {
		t.Fatalf("escaped plist = %s", escaped)
	}

	unit, err := supervisorBody("linux", "/bin/agentbusd", vars)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"Description=agentbus daemon\n",
		"ExecStart=/bin/agentbusd\n",
		"Restart=on-failure\n",
		"Environment=HOME=/Users/me\n",
		"Environment=XDG_RUNTIME_DIR=/tmp/ab\n",
		"WantedBy=default.target\n",
	} {
		if !strings.Contains(unit, want) {
			t.Fatalf("unit missing %s\n%s", want, unit)
		}
	}
	quoted, err := supervisorBody("linux", "/opt/a&b/agentbusd", nil)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(quoted, `ExecStart="/opt/a&b/agentbusd"`) {
		t.Fatalf("quoted unit = %s", quoted)
	}
	path, err := supervisorPath("linux", "/home/me", []string{"XDG_CONFIG_HOME=/cfg"})
	if err != nil {
		t.Fatal(err)
	}
	if path != filepath.Join("/cfg", "systemd", "user", systemdUnitName) {
		t.Fatalf("systemd path = %s", path)
	}
	_, err = supervisorBody("plan9", "/bin/agentbusd", nil)
	if err == nil {
		t.Fatal("plan9 supervisor body succeeded")
	}
}

func hookEnv(t *testing.T) (paths.Dirs, []string) {
	t.Helper()
	home := t.TempDir()
	env := []string{
		"HOME=" + home,
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	return dirs, env
}

func homeOf(t *testing.T, env []string) string {
	t.Helper()
	home := envGet(env, "HOME")
	if home == "" {
		t.Fatal("HOME is empty")
	}
	return home
}

func writeAgentbusWrapper(t *testing.T, dir string) {
	t.Helper()
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	body := "#!/bin/sh\nexport AGENTBUS_TEST_HOOK=1\nexec " + shellQuote(exe) + " \"$@\"\n"
	writeFile(t, filepath.Join(dir, "agentbus"), []byte(body))
	if err := os.Chmod(filepath.Join(dir, "agentbus"), 0o755); err != nil {
		t.Fatal(err)
	}
}

func commandEnv(base []string, path string) []string {
	out := make([]string, 0, len(base)+1)
	for _, entry := range base {
		key, _, ok := strings.Cut(entry, "=")
		if ok && key == "PATH" {
			continue
		}
		out = append(out, entry)
	}
	return append(out, "PATH="+path)
}

func runScript(t *testing.T, script, stdin string, env []string) (int, string, string) {
	t.Helper()
	cmd := exec.Command(script)
	cmd.Env = env
	cmd.Stdin = strings.NewReader(stdin)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	if err == nil {
		return 0, stdout.String(), stderr.String()
	}
	var exit *exec.ExitError
	if errors.As(err, &exit) {
		return exit.ExitCode(), stdout.String(), stderr.String()
	}
	t.Fatal(err)
	return 1, stdout.String(), stderr.String()
}

func bypassEvents(t *testing.T, dir string) []map[string]any {
	t.Helper()
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Event: eventlog.EventBypass})
	if err != nil {
		t.Fatal(err)
	}
	out := make([]map[string]any, 0, len(lines))
	for _, line := range lines {
		var rec map[string]any
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("decode %s: %v", line, err)
		}
		out = append(out, rec)
	}
	return out
}

func assertBypass(t *testing.T, rec map[string]any, actor, name, detail string) {
	t.Helper()
	if rec["event"] != eventlog.EventBypass || rec["actor"] != actor || rec["name"] != name || rec["detail"] != detail {
		t.Fatalf("bypass = %#v", rec)
	}
	ts, _ := rec["ts"].(string)
	if _, err := time.Parse(time.RFC3339Nano, ts); err != nil {
		t.Fatalf("ts %q: %v", ts, err)
	}
}

func sendMessageCommands(t *testing.T, doc map[string]any) int {
	t.Helper()
	return countMatchers(t, doc, "SendMessage")
}

func hasBashHook(t *testing.T, doc map[string]any) bool {
	t.Helper()
	return countMatchers(t, doc, "Bash") == 1
}

func countMatchers(t *testing.T, doc map[string]any, matcher string) int {
	t.Helper()
	hooks, _ := doc["hooks"].(map[string]any)
	pre, _ := hooks["PreToolUse"].([]any)
	n := 0
	for _, item := range pre {
		entry, _ := item.(map[string]any)
		if entry["matcher"] == matcher {
			n++
		}
	}
	return n
}

func installedPath(home string) string {
	if runtime.GOOS == "linux" {
		return filepath.Join(home, ".config", "systemd", "user", systemdUnitName)
	}
	return filepath.Join(home, "Library", "LaunchAgents", launchdLabel+".plist")
}

func stubDaemon(t *testing.T) {
	t.Helper()
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(filepath.Dir(exe), "agentbusd")
	if err := os.WriteFile(path, []byte("#!/bin/sh\nexit 0\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Remove(path) })
}

func writeFile(t *testing.T, path string, data []byte) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, data, 0o600); err != nil {
		t.Fatal(err)
	}
}

func assertBytes(t *testing.T, path string, want []byte) {
	t.Helper()
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("%s = %q, want %q", path, got, want)
	}
}

func assertMode(t *testing.T, path string, mode os.FileMode) {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm() != mode {
		t.Fatalf("%s mode = %o, want %o", path, info.Mode().Perm(), mode)
	}
}

func readJSON(t *testing.T, path string) map[string]any {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	var doc map[string]any
	if err := json.Unmarshal(data, &doc); err != nil {
		t.Fatal(err)
	}
	return doc
}

func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}
