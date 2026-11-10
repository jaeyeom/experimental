package main

import (
	"bytes"
	"context"
	"encoding/json"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

const (
	launchdLabel    = "com.agentbus.agentbusd"
	systemdUnitName = "agentbus.service"
	hookScriptName  = "agentbus-sendmessage.sh"
	// promptHookScriptName is the UserPromptSubmit guard against a bus
	// message merged into a user draft.
	promptHookScriptName = "agentbus-userpromptsubmit.sh"
)

// hookScript is the PreToolUse command. It stays a thin POSIX wrapper so a
// missing agentbus exits 0 before the tool call is affected. JSON stays in Go.
const hookScript = `#!/bin/sh
# Observe a Claude Code SendMessage call. Fail open: never block the tool.
set -eu
if ! command -v agentbus >/dev/null 2>&1; then
  exit 0
fi
agentbus hook || exit 0
exit 0
`

// promptHookScript is the UserPromptSubmit command. Without agentbus the
// prompt goes through. agentbus prompt-hook prints the block decision.
const promptHookScript = `#!/bin/sh
# Block a Claude Code prompt where a bus message was pasted after a user draft.
set -eu
if ! command -v agentbus >/dev/null 2>&1; then
  exit 0
fi
agentbus prompt-hook || exit 0
exit 0
`

func cmdInstall(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintf(stderr, "agentbus install: unexpected arguments\n")
		return 2
	}
	if _, err := paths.FromEnv(env); err != nil {
		fmt.Fprintf(stderr, "agentbus install: %s\n", err)
		return 1
	}
	home, err := homeFrom(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install: %s\n", err)
		return 1
	}
	exe, err := os.Executable()
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install: cannot find this executable: %s\n", err)
		return 1
	}
	bin, err := client.DaemonBinBeside(exe)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install: %s\n", err)
		return 1
	}
	path, err := installSupervisor(runtime.GOOS, home, bin, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install: %s\n", err)
		return 1
	}
	if envFlag(env, "AGENTBUS_INSTALL_NO_LOAD") {
		fmt.Fprintf(stdout, "wrote %s\n", path)
		return 0
	}
	if err := loadSupervisor(ctx, runtime.GOOS, path); err != nil {
		fmt.Fprintf(stderr, "agentbus install: wrote %s\n%s\n", path, err)
		return 1
	}
	fmt.Fprintf(stdout, "installed %s\n", path)
	return 0
}

func cmdInstallHooks(_ context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintf(stderr, "agentbus install-hooks: unexpected arguments\n")
		return 2
	}
	home, err := homeFrom(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install-hooks: %s\n", err)
		return 1
	}
	scriptPath, err := writeHookScript(home, hookScriptName, hookScript)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install-hooks: %s\n", err)
		return 1
	}
	promptPath, err := writeHookScript(home, promptHookScriptName, promptHookScript)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install-hooks: %s\n", err)
		return 1
	}
	settingsPath := filepath.Join(home, ".claude", "settings.json")
	if err := mergeUserSettings(settingsPath, scriptPath, promptPath); err != nil {
		fmt.Fprintf(stderr, "agentbus install-hooks: %s\n", err)
		return 1
	}
	fmt.Fprintf(stdout, "installed hook %s\ninstalled hook %s\n", scriptPath, promptPath)
	return 0
}

func installSupervisor(goos, home, bin string, env []string) (string, error) {
	path, err := supervisorPath(goos, home, env)
	if err != nil {
		return "", err
	}
	body, err := supervisorBody(goos, bin, supervisorEnv(env))
	if err != nil {
		return "", err
	}
	if err := writeAtomic(path, []byte(body), 0o644); err != nil {
		return "", err
	}
	return path, nil
}

func supervisorPath(goos, home string, env []string) (string, error) {
	switch goos {
	case "darwin":
		if home == "" {
			return "", errors.New("HOME is not set")
		}
		return filepath.Join(home, "Library", "LaunchAgents", launchdLabel+".plist"), nil
	case "linux":
		return filepath.Join(systemdUserDir(home, env), systemdUnitName), nil
	default:
		return "", fmt.Errorf("agentbus install supports systemd and launchd, not %s", goos)
	}
}

func systemdUserDir(home string, env []string) string {
	if dir := envGet(env, "XDG_CONFIG_HOME"); dir != "" {
		return filepath.Join(dir, "systemd", "user")
	}
	return filepath.Join(home, ".config", "systemd", "user")
}

func supervisorBody(goos, bin string, envVars [][2]string) (string, error) {
	switch goos {
	case "darwin":
		return launchdPlist(bin, envVars)
	case "linux":
		return systemdUnit(bin, envVars), nil
	default:
		return "", fmt.Errorf("agentbus install supports systemd and launchd, not %s", goos)
	}
}

func launchdPlist(bin string, envVars [][2]string) (string, error) {
	escBin, err := xmlEscape(bin)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString(`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>` + launchdLabel + `</string>
  <key>ProgramArguments</key>
  <array>
    <string>` + escBin + `</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <true/>
`)
	if len(envVars) > 0 {
		b.WriteString("  <key>EnvironmentVariables</key>\n  <dict>\n")
		for _, kv := range envVars {
			key, err := xmlEscape(kv[0])
			if err != nil {
				return "", err
			}
			val, err := xmlEscape(kv[1])
			if err != nil {
				return "", err
			}
			fmt.Fprintf(&b, "    <key>%s</key>\n    <string>%s</string>\n", key, val)
		}
		b.WriteString("  </dict>\n")
	}
	b.WriteString("</dict>\n</plist>\n")
	return b.String(), nil
}

func systemdUnit(bin string, envVars [][2]string) string {
	var b strings.Builder
	b.WriteString("[Unit]\nDescription=agentbus daemon\n\n[Service]\nExecStart=")
	b.WriteString(systemdValue(bin))
	b.WriteString("\nRestart=on-failure\n")
	for _, kv := range envVars {
		b.WriteString("Environment=")
		b.WriteString(kv[0])
		b.WriteString("=")
		b.WriteString(systemdValue(kv[1]))
		b.WriteString("\n")
	}
	b.WriteString("\n[Install]\nWantedBy=default.target\n")
	return b.String()
}

func systemdValue(s string) string {
	if s == "" {
		return `""`
	}
	for _, r := range s {
		if isSystemdPlain(r) {
			continue
		}
		return `"` + strings.NewReplacer(`\`, `\\`, `"`, `\"`).Replace(s) + `"`
	}
	return s
}

func isSystemdPlain(r rune) bool {
	switch {
	case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9':
		return true
	default:
		return strings.ContainsRune("/._:@+-", r)
	}
}

func supervisorEnv(env []string) [][2]string {
	var out [][2]string
	for _, key := range []string{"HOME", "XDG_RUNTIME_DIR", "XDG_STATE_HOME", "XDG_CONFIG_HOME"} {
		if val := envGet(env, key); val != "" {
			out = append(out, [2]string{key, val})
		}
	}
	return out
}

func loadSupervisor(ctx context.Context, goos, path string) error {
	switch goos {
	case "darwin":
		return launchctlBootstrap(ctx, path)
	case "linux":
		return systemctlEnable(ctx, path)
	default:
		return fmt.Errorf("agentbus install supports systemd and launchd, not %s", goos)
	}
}

func launchctlBootstrap(ctx context.Context, plistPath string) error {
	target := "gui/" + strconv.Itoa(os.Getuid())
	// A previous job with this plist keeps bootstrap from replacing it.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	bootout := exec.CommandContext(ctx, "launchctl", "bootout", target, plistPath) //nolint:gosec // G204: launchctl targets the plist this command just wrote
	_ = bootout.Run()
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, "launchctl", "bootstrap", target, plistPath) //nolint:gosec // G204: launchctl loads the plist this command just wrote
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("launchctl bootstrap: %w (%s)", err, strings.TrimSpace(string(out)))
	}
	return nil
}

func systemctlEnable(ctx context.Context, unitPath string) error {
	if _, err := os.Stat(unitPath); err != nil {
		return fmt.Errorf("stat unit: %w", err)
	}
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	reload := exec.CommandContext(ctx, "systemctl", "--user", "daemon-reload") //nolint:gosec // G204: fixed systemctl args, no shell
	out, err := reload.CombinedOutput()
	if err != nil {
		return fmt.Errorf("systemctl daemon-reload: %w (%s)", err, strings.TrimSpace(string(out)))
	}
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	enable := exec.CommandContext(ctx, "systemctl", "--user", "enable", "--now", systemdUnitName) //nolint:gosec // G204: fixed systemctl args, no shell
	out, err = enable.CombinedOutput()
	if err != nil {
		return fmt.Errorf("systemctl enable --now %s: %w (%s)", systemdUnitName, err, strings.TrimSpace(string(out)))
	}
	return nil
}

func writeHookScript(home, name, body string) (string, error) {
	path := filepath.Join(home, ".claude", "hooks", name)
	if err := writeAtomic(path, []byte(body), 0o755); err != nil {
		return "", err
	}
	return path, nil
}

func mergeUserSettings(path, sendCommand, promptCommand string) error {
	doc, err := readSettings(path)
	if err != nil {
		return err
	}
	if err := addSendMessageHook(doc, sendCommand); err != nil {
		return err
	}
	if err := addPromptHook(doc, promptCommand); err != nil {
		return err
	}
	data, err := encodeSettings(doc)
	if err != nil {
		return err
	}
	return writeAtomic(path, data, 0o600)
}

func encodeSettings(doc map[string]any) ([]byte, error) {
	var b bytes.Buffer
	enc := json.NewEncoder(&b)
	enc.SetEscapeHTML(false)
	enc.SetIndent("", "  ")
	if err := enc.Encode(doc); err != nil {
		return nil, fmt.Errorf("encode settings: %w", err)
	}
	return b.Bytes(), nil
}

func readSettings(path string) (map[string]any, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return map[string]any{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read settings: %w", err)
	}
	if len(bytes.TrimSpace(data)) == 0 {
		return map[string]any{}, nil
	}
	var doc map[string]any
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("decode settings: %w", err)
	}
	if doc == nil {
		doc = map[string]any{}
	}
	return doc, nil
}

func addSendMessageHook(doc map[string]any, command string) error {
	hooks, err := ensureObject(doc, "hooks")
	if err != nil {
		return err
	}
	pre, err := ensureList(hooks, "PreToolUse")
	if err != nil {
		return err
	}
	for i, item := range pre {
		entry, ok := item.(map[string]any)
		if !ok {
			continue
		}
		matcher, _ := entry["matcher"].(string)
		if matcher != "SendMessage" {
			continue
		}
		upsertCommand(entry, command, hookScriptName)
		pre[i] = entry
		hooks["PreToolUse"] = pre
		return nil
	}
	hooks["PreToolUse"] = append(pre, map[string]any{
		"matcher": "SendMessage",
		"hooks": []any{
			map[string]any{"type": "command", "command": command},
		},
	})
	return nil
}

// addPromptHook adds the UserPromptSubmit guard once. UserPromptSubmit
// has no matcher, so an entry that already runs the script is updated
// and other entries are left alone.
func addPromptHook(doc map[string]any, command string) error {
	hooks, err := ensureObject(doc, "hooks")
	if err != nil {
		return err
	}
	entries, err := ensureList(hooks, "UserPromptSubmit")
	if err != nil {
		return err
	}
	for _, item := range entries {
		entry, ok := item.(map[string]any)
		if ok && updateCommand(entry, command, promptHookScriptName) {
			return nil
		}
	}
	hooks["UserPromptSubmit"] = append(entries, map[string]any{
		"hooks": []any{
			map[string]any{"type": "command", "command": command},
		},
	})
	return nil
}

func upsertCommand(entry map[string]any, command, script string) {
	if updateCommand(entry, command, script) {
		return
	}
	hooks, _ := entry["hooks"].([]any)
	entry["hooks"] = append(hooks, map[string]any{
		"type":    "command",
		"command": command,
	})
}

// updateCommand points a hook that runs script at command.
func updateCommand(entry map[string]any, command, script string) bool {
	hooks, _ := entry["hooks"].([]any)
	for _, item := range hooks {
		hook, ok := item.(map[string]any)
		if !ok {
			continue
		}
		current, _ := hook["command"].(string)
		if current == command || strings.HasSuffix(current, "/"+script) {
			hook["type"] = "command"
			hook["command"] = command
			return true
		}
	}
	return false
}

func ensureObject(parent map[string]any, key string) (map[string]any, error) {
	raw, ok := parent[key]
	if !ok || raw == nil {
		child := map[string]any{}
		parent[key] = child
		return child, nil
	}
	child, ok := raw.(map[string]any)
	if !ok {
		return nil, fmt.Errorf("settings %s is not an object", key)
	}
	return child, nil
}

func ensureList(parent map[string]any, key string) ([]any, error) {
	raw, ok := parent[key]
	if !ok || raw == nil {
		list := []any{}
		parent[key] = list
		return list, nil
	}
	list, ok := raw.([]any)
	if !ok {
		return nil, fmt.Errorf("settings %s is not a list", key)
	}
	return list, nil
}

func writeAtomic(path string, data []byte, mode os.FileMode) error {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("create %s: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, ".agentbus-tmp-*")
	if err != nil {
		return fmt.Errorf("create temp file: %w", err)
	}
	tmpName := tmp.Name()
	done := false
	defer func() {
		if !done {
			_ = tmp.Close()
			_ = os.Remove(tmpName)
		}
	}()
	if _, err := tmp.Write(data); err != nil {
		return fmt.Errorf("write %s: %w", path, err)
	}
	if err := tmp.Chmod(mode); err != nil {
		return fmt.Errorf("chmod %s: %w", path, err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close %s: %w", path, err)
	}
	if err := os.Rename(tmpName, path); err != nil {
		return fmt.Errorf("replace %s: %w", path, err)
	}
	done = true
	return nil
}

func xmlEscape(s string) (string, error) {
	var b bytes.Buffer
	if err := xml.EscapeText(&b, []byte(s)); err != nil {
		return "", fmt.Errorf("escape xml: %w", err)
	}
	return b.String(), nil
}

func homeFrom(env []string) (string, error) {
	if home := envGet(env, "HOME"); home != "" {
		return home, nil
	}
	return "", errors.New("HOME is not set")
}

func envGet(env []string, key string) string {
	for _, entry := range env {
		k, val, ok := strings.Cut(entry, "=")
		if ok && k == key {
			return val
		}
	}
	return ""
}

func envFlag(env []string, key string) bool {
	return envGet(env, key) == "1"
}
