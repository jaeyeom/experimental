package main

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

// countChecks counts the lease-check calls that reach the daemon.
func countChecks(t *testing.T) *atomic.Int32 {
	t.Helper()
	old := dialDaemon
	t.Cleanup(func() { dialDaemon = old })
	var n atomic.Int32
	dialDaemon = func(ctx context.Context, socket string, req daemon.Request) (daemon.Response, error) {
		if req.Op == "lease-check" {
			n.Add(1)
		}
		return old(ctx, socket, req)
	}
	return &n
}

// hook runs agentbus lease hook as pane for one tool call made in cwd.
// It returns the permission decision ("" when the hook printed none),
// its reason, and stderr.
func (w *leaseWorld) hook(t *testing.T, env []string, tool string, input map[string]any, cwd string) (string, string, string) {
	t.Helper()
	raw, err := json.Marshal(map[string]any{
		"session_id":      "sess-1",
		"hook_event_name": "PreToolUse",
		"cwd":             cwd,
		"tool_name":       tool,
		"tool_input":      input,
	})
	if err != nil {
		t.Fatal(err)
	}
	code, stdout, stderr := runCmdIn([]string{"lease", "hook"}, env, strings.NewReader(string(raw)))
	if code != 0 {
		t.Fatalf("lease hook = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	if stdout == "" {
		return "", "", stderr
	}
	var out struct {
		HookSpecificOutput struct {
			HookEventName            string `json:"hookEventName"`
			PermissionDecision       string `json:"permissionDecision"`
			PermissionDecisionReason string `json:"permissionDecisionReason"`
		} `json:"hookSpecificOutput"`
	}
	if err := json.Unmarshal([]byte(stdout), &out); err != nil {
		t.Fatalf("hook stdout %q: %v", stdout, err)
	}
	if out.HookSpecificOutput.HookEventName != "PreToolUse" {
		t.Fatalf("hook stdout = %s", stdout)
	}
	return out.HookSpecificOutput.PermissionDecision, out.HookSpecificOutput.PermissionDecisionReason, stderr
}

func (w *leaseWorld) edit(t *testing.T, pane, path string) (string, string, string) {
	t.Helper()
	return w.hook(t, w.as(pane), "Edit", map[string]any{"file_path": path, "old_string": "a", "new_string": "b"}, w.repo)
}

func (w *leaseWorld) bash(t *testing.T, pane, command, cwd string) (string, string, string) {
	t.Helper()
	return w.hook(t, w.as(pane), "Bash", map[string]any{"command": command}, cwd)
}

func assertDenied(t *testing.T, what, decision, reason string) {
	t.Helper()
	if decision != "deny" || !strings.Contains(reason, "agentbus lease claim --for") {
		t.Fatalf("%s = %q %q, want deny with the claim hint", what, decision, reason)
	}
}

func assertAllowed(t *testing.T, what, decision, reason string) {
	t.Helper()
	if decision != "" {
		t.Fatalf("%s = %q %q, want allowed", what, decision, reason)
	}
}

func TestLeaseHookBlocksANonHolderEditAndAllowsTheHolder(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	file := filepath.Join(w.repo, "a.txt")

	decision, reason, _ := w.edit(t, "p2", file)
	assertDenied(t, "edit of a free checkout", decision, reason)

	w.mustLease(t, "p1", "claim", "--for", "40m")
	decision, reason, _ = w.edit(t, "p2", file)
	assertDenied(t, "non-holder edit", decision, reason)
	if !strings.Contains(reason, "held by "+leaseAlice) {
		t.Fatalf("reason = %q", reason)
	}
	for _, tool := range []struct {
		name  string
		input map[string]any
	}{
		{"Write", map[string]any{"file_path": filepath.Join(w.repo, "new.txt"), "content": "x"}},
		{"MultiEdit", map[string]any{"file_path": "a.txt", "edits": []any{}}},
		{"NotebookEdit", map[string]any{"notebook_path": filepath.Join(w.repo, "n.ipynb"), "new_source": "x"}},
	} {
		decision, reason, _ := w.hook(t, w.as("p2"), tool.name, tool.input, w.repo)
		assertDenied(t, "non-holder "+tool.name, decision, reason)
	}

	decision, reason, stderr := w.edit(t, "p1", file)
	assertAllowed(t, "holder edit", decision, reason)
	if stderr != "" {
		t.Fatalf("holder edit stderr = %q", stderr)
	}
}

func TestLeaseHookBlocksTheAdopterUntilAdopt(t *testing.T) {
	noProcs(t)
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.dirty(t)
	w.mustLease(t, "p1", "claim", "--for", "40m")
	decision, reason, _ := w.edit(t, "p1", filepath.Join(w.repo, "a.txt"))
	if decision != "deny" || !strings.Contains(reason, "agentbus lease adopt") {
		t.Fatalf("adopter edit before adopt = %q %q", decision, reason)
	}
	w.mustLease(t, "p1", "adopt")
	decision, reason, _ = w.edit(t, "p1", filepath.Join(w.repo, "a.txt"))
	assertAllowed(t, "adopter edit after adopt", decision, reason)
}

func TestLeaseHookEditOutsideGuardedCheckoutsMakesNoDaemonCall(t *testing.T) {
	w := newLeaseWorld(t)
	calls := countChecks(t)
	outside := t.TempDir()
	decision, reason, stderr := w.hook(t, w.as("p2"), "Edit", map[string]any{"file_path": filepath.Join(outside, "f.go")}, outside)
	assertAllowed(t, "unguarded edit", decision, reason)
	decision, reason, _ = w.hook(t, w.as("p2"), "Write", map[string]any{"file_path": "rel.go"}, outside)
	assertAllowed(t, "unguarded relative write", decision, reason)
	decision, reason, _ = w.hook(t, w.as("p2"), "Read", map[string]any{"file_path": filepath.Join(w.repo, "a.txt")}, w.repo)
	assertAllowed(t, "read in the checkout", decision, reason)
	if n := calls.Load(); n != 0 || stderr != "" {
		t.Fatalf("daemon calls = %d, stderr %q", n, stderr)
	}
}

func TestLeaseHookBash(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	calls := countChecks(t)
	elsewhere := t.TempDir()

	for _, command := range []string{"git status", "git log --oneline", "ls && git diff", "make test"} {
		decision, reason, _ := w.bash(t, "p2", command, w.repo)
		assertAllowed(t, command, decision, reason)
	}
	if n := calls.Load(); n != 0 {
		t.Fatalf("read-only commands made %d daemon calls", n)
	}
	for _, tc := range []struct{ command, cwd string }{
		{"git commit -m x", w.repo},
		{"cd " + w.repo + " && git rebase main", elsewhere},
		{"git -C " + w.repo + " reset --hard", elsewhere},
	} {
		decision, reason, _ := w.bash(t, "p2", tc.command, tc.cwd)
		assertDenied(t, tc.command, decision, reason)
	}
	decision, reason, _ := w.bash(t, "p1", "git commit -m x", w.repo)
	assertAllowed(t, "holder commit", decision, reason)
	decision, reason, _ = w.bash(t, "p2", "git commit -m x", elsewhere)
	assertAllowed(t, "commit outside the checkout", decision, reason)
}

func TestLeaseHookFailsOpenWhenTheDaemonIsDown(t *testing.T) {
	w := newLeaseWorld(t)
	decision, reason, stderr := w.edit(t, "p2", filepath.Join(w.repo, "a.txt"))
	assertAllowed(t, "edit with the daemon down", decision, reason)
	if !strings.Contains(stderr, "agentbus lease hook:") || !strings.Contains(stderr, "allowing") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestLeaseHookFailsOpenAtTheTimeout(t *testing.T) {
	w := newLeaseWorld(t)
	old := leaseHookTimeout
	leaseHookTimeout = 50 * time.Millisecond
	t.Cleanup(func() { leaseHookTimeout = old })
	fakeHookDial(t, func(ctx context.Context, _ daemon.Request) (daemon.Response, error) {
		<-ctx.Done()
		return daemon.Response{}, errors.New("read response: i/o timeout")
	})
	start := time.Now()
	decision, reason, stderr := w.edit(t, "p2", filepath.Join(w.repo, "a.txt"))
	assertAllowed(t, "edit with a slow daemon", decision, reason)
	if time.Since(start) > 2*time.Second || !strings.Contains(stderr, "allowing") {
		t.Fatalf("took %s, stderr %q", time.Since(start), stderr)
	}
}

func TestLeaseHookFailsOpenInHubMode(t *testing.T) {
	w := newLeaseWorld(t)
	calls := countChecks(t)
	env := append(w.as("p2"), hubEnv+"=me@hub")
	decision, reason, stderr := w.hook(t, env, "Edit", map[string]any{"file_path": filepath.Join(w.repo, "a.txt")}, w.repo)
	assertAllowed(t, "hub-mode edit", decision, reason)
	if !strings.Contains(stderr, "hub mode") || calls.Load() != 0 {
		t.Fatalf("stderr = %q, daemon calls %d", stderr, calls.Load())
	}
}

func TestLeaseHookCachesTheHolderUntilRelease(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	calls := countChecks(t)
	file := filepath.Join(w.repo, "a.txt")

	for i := range 3 {
		decision, reason, _ := w.edit(t, "p1", file)
		assertAllowed(t, "holder edit", decision, reason)
		if n := calls.Load(); n != 1 {
			t.Fatalf("after edit %d: daemon calls = %d, want 1", i+1, n)
		}
	}
	// Denials are never cached.
	for range 2 {
		decision, reason, _ := w.edit(t, "p2", file)
		assertDenied(t, "non-holder edit", decision, reason)
	}
	if n := calls.Load(); n != 3 {
		t.Fatalf("after denials: daemon calls = %d, want 3", n)
	}

	w.mustLease(t, "p1", "release")
	decision, reason, _ := w.edit(t, "p1", file)
	assertDenied(t, "edit after release", decision, reason)
	if n := calls.Load(); n != 4 {
		t.Fatalf("after release: daemon calls = %d, want 4", n)
	}
}

func TestLeaseInstallHookWithoutWritePrintsOnly(t *testing.T) {
	noDaemon(t)
	home := t.TempDir()
	env := []string{"HOME=" + home}
	code, stdout, stderr := runCmd([]string{"lease", "install-hook"}, env)
	if code != 0 {
		t.Fatalf("install-hook = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	for _, want := range []string{leaseHookScriptName, "agentbus lease hook", `"matcher": "` + leaseHookMatcher + `"`, "--write"} {
		if !strings.Contains(stdout, want) {
			t.Errorf("stdout is missing %q:\n%s", want, stdout)
		}
	}
	entries, err := os.ReadDir(home)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 0 {
		t.Fatalf("install-hook wrote %v under HOME", entries)
	}
}

func TestLeaseInstallHookWriteMergesUserSettings(t *testing.T) {
	noDaemon(t)
	home := t.TempDir()
	project := t.TempDir()
	projSettings := []byte("{\"theme\":\"project\"}\n")
	writeFile(t, filepath.Join(project, ".claude", "settings.json"), projSettings)
	writeFile(t, filepath.Join(home, ".claude", "settings.json"), []byte(`{
  "theme": "dim",
  "hooks": {"PreToolUse": [{"matcher": "Bash", "hooks": [{"type": "command", "command": "/tmp/other"}]}]}
}
`))
	t.Chdir(project)
	env := []string{"HOME=" + home}
	for range 2 {
		code, stdout, stderr := runCmd([]string{"lease", "install-hook", "--write"}, env)
		if code != 0 {
			t.Fatalf("install-hook --write = %d\nstdout %s\nstderr %s", code, stdout, stderr)
		}
	}
	script := filepath.Join(home, ".claude", "hooks", leaseHookScriptName)
	assertMode(t, script, 0o755)
	body, err := os.ReadFile(script)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(body), "command -v agentbus") || !strings.Contains(string(body), "agentbus lease hook") {
		t.Fatalf("script = %q", body)
	}
	assertBytes(t, filepath.Join(project, ".claude", "settings.json"), projSettings)
	doc := readJSON(t, filepath.Join(home, ".claude", "settings.json"))
	if doc["theme"] != "dim" || !hasBashHook(t, doc) || countMatchers(t, doc, leaseHookMatcher) != 1 {
		t.Fatalf("settings = %#v", doc)
	}
}
